#include "nashR.h"
#include <stdlib.h>
#include <math.h>

/*
 * Lemke-Howson complementary pivoting algorithm for 2-player bimatrix games.
 *
 * Given an m x n bimatrix game (A, B) where A is player 1's payoff matrix
 * and B is player 2's payoff matrix, find a Nash equilibrium.
 *
 * Labels 0..m-1 correspond to player 1's pure strategies (rows).
 * Labels m..m+n-1 correspond to player 2's pure strategies (columns).
 *
 * Tableau 1 (player 1's best-response polytope):
 *   Rows indexed by player 2's strategies j=0..n-1.
 *   Variables: q_0..q_{m-1} (player 1 mixed strategy components) and
 *              s1_0..s1_{n-1} (slack variables).
 *   Constraint: B^T q + s1 = 1  (n constraints).
 *   Column labels for q_i: label i (0..m-1).
 *   Column labels for s1_j: label m+j (m..m+n-1).
 *   Initially, q variables are non-basic, s1 variables are basic.
 *   So basis1[j] = m+j for j=0..n-1.
 *
 * Tableau 2 (player 2's best-response polytope):
 *   Rows indexed by player 1's strategies i=0..m-1.
 *   Variables: p_0..p_{n-1} (player 2 mixed strategy components) and
 *              s2_0..s2_{m-1} (slack variables).
 *   Constraint: A p + s2 = 1  (m constraints).
 *   Column labels for p_j: label m+j (m..m+n-1).
 *   Column labels for s2_i: label i (0..m-1).
 *   Initially, p variables are non-basic, s2 variables are basic.
 *   So basis2[i] = i for i=0..m-1.
 *
 * Tableau layout (for tableau 1, n rows, m+n+1 columns):
 *   Columns 0..m-1: coefficients for q_0..q_{m-1}
 *   Columns m..m+n-1: coefficients for s1_0..s1_{n-1}
 *   Column m+n: RHS
 *
 * Tableau layout (for tableau 2, m rows, m+n+1 columns):
 *   Columns 0..n-1: coefficients for p_0..p_{n-1}
 *   Columns m..m+n-1: coefficients for s2_0..s2_{m-1}
 *   (We keep the label-indexed layout: column with label k is at position k
 *    if k < m for tab1 q-vars, or k for tab1 slack-vars, etc.)
 *
 * Actually, for cleaner indexing, each tableau has columns indexed by labels
 * 0..m+n-1, plus one RHS column. The "entering" variable is identified by
 * its label.
 */

#define TOL 1e-12

/* Pivot on tableau: pivot_row, pivot_col in a tableau with n_rows rows
 * and n_total_cols columns (including RHS). */
static void pivot(double *tab, int n_rows, int n_total_cols,
                  int pivot_row, int pivot_col) {
    double pivot_elem = tab[pivot_row * n_total_cols + pivot_col];
    if (fabs(pivot_elem) < TOL) {
        error("Lemke-Howson: zero pivot element at row %d, col %d", pivot_row, pivot_col);
    }

    /* Scale pivot row */
    for (int j = 0; j < n_total_cols; j++) {
        tab[pivot_row * n_total_cols + j] /= pivot_elem;
    }

    /* Eliminate from all other rows */
    for (int i = 0; i < n_rows; i++) {
        if (i == pivot_row) continue;
        double factor = tab[i * n_total_cols + pivot_col];
        if (fabs(factor) < TOL) continue;
        for (int j = 0; j < n_total_cols; j++) {
            tab[i * n_total_cols + j] -= factor * tab[pivot_row * n_total_cols + j];
        }
    }
}

/* Minimum ratio test: find the row with smallest positive ratio RHS / coeff
 * for the entering column. Returns -1 if no valid pivot row. */
static int min_ratio_test(double *tab, int n_rows, int n_total_cols,
                          int enter_col) {
    int best_row = -1;
    double best_ratio = 0.0;

    int rhs_col = n_total_cols - 1;

    for (int i = 0; i < n_rows; i++) {
        double coeff = tab[i * n_total_cols + enter_col];
        if (coeff <= TOL) continue;  /* must be strictly positive */
        double ratio = tab[i * n_total_cols + rhs_col] / coeff;
        if (best_row == -1 || ratio < best_ratio) {
            best_ratio = ratio;
            best_row = i;
        }
    }

    return best_row;
}

SEXP c_lemke_howson(SEXP payoff_A, SEXP payoff_B, SEXP n_rows, SEXP n_cols,
                    SEXP init_label) {
    int m = INTEGER(n_rows)[0];
    int n = INTEGER(n_cols)[0];
    int k0 = INTEGER(init_label)[0] - 1;  /* convert 1-indexed to 0-indexed */

    double *A = REAL(payoff_A);  /* m x n, column-major from R: A[i,j] = A[i + j*m] */
    double *B = REAL(payoff_B);  /* m x n, column-major from R: B[i,j] = B[i + j*m] */

    int L = m + n;  /* total number of labels: 0..L-1 */
    int ncols1 = L + 1;  /* tableau 1 columns: L variable columns + 1 RHS */
    int ncols2 = L + 1;  /* tableau 2 columns: L variable columns + 1 RHS */

    /* Allocate tableaux using R_alloc (auto-freed by R) */
    double *tab1 = (double *) R_alloc((size_t)n * ncols1, sizeof(double));
    double *tab2 = (double *) R_alloc((size_t)m * ncols2, sizeof(double));

    /* Basis arrays: basis1[j] is the label of the basic variable in row j of tab1
     *               basis2[i] is the label of the basic variable in row i of tab2 */
    int *basis1 = (int *) R_alloc(n, sizeof(int));
    int *basis2 = (int *) R_alloc(m, sizeof(int));

    /*
     * Initialize Tableau 1 (n rows for player 1's polytope).
     * Constraint row j (j=0..n-1): sum_i B[i,j] * q_i + s1_j = 1
     * Note: B[i,j] = B[i + j*m] in R's column-major storage.
     *
     * Columns 0..m-1 are for q_0..q_{m-1} (labels 0..m-1).
     * Columns m..m+n-1 are for s1_0..s1_{n-1} (labels m..m+n-1).
     * Column L = m+n is RHS.
     */
    for (int j = 0; j < n; j++) {
        for (int col = 0; col < ncols1; col++) {
            tab1[j * ncols1 + col] = 0.0;
        }
        /* q_i coefficients: B^T[j,i] = B[i,j] */
        for (int i = 0; i < m; i++) {
            tab1[j * ncols1 + i] = B[i + j * m];
        }
        /* s1_j coefficient (identity for slack) */
        tab1[j * ncols1 + (m + j)] = 1.0;
        /* RHS */
        tab1[j * ncols1 + L] = 1.0;

        basis1[j] = m + j;  /* slack s1_j has label m+j */
    }

    /*
     * Initialize Tableau 2 (m rows for player 2's polytope).
     * Constraint row i (i=0..m-1): sum_j A[i,j] * p_j + s2_i = 1
     *
     * Columns 0..m-1 are for labels 0..m-1. In tableau 2, label i corresponds
     * to s2_i (slack), so column i is for s2_i.
     * Columns m..m+n-1 are for labels m..m+n-1. Label m+j corresponds to p_j.
     * Column L is RHS.
     */
    for (int i = 0; i < m; i++) {
        for (int col = 0; col < ncols2; col++) {
            tab2[i * ncols2 + col] = 0.0;
        }
        /* p_j coefficients: A[i,j], stored at column m+j */
        for (int j = 0; j < n; j++) {
            tab2[i * ncols2 + (m + j)] = A[i + j * m];
        }
        /* s2_i coefficient (identity for slack), stored at column i */
        tab2[i * ncols2 + i] = 1.0;
        /* RHS */
        tab2[i * ncols2 + L] = 1.0;

        basis2[i] = i;  /* slack s2_i has label i */
    }

    /*
     * Complementary pivoting.
     *
     * A label k is "owned" by tableau 1 if it can be a non-basic column there
     * and by tableau 2 likewise. Both tableaux share the same label space 0..L-1.
     *
     * Label k "enters" a tableau means we pivot column k into the basis of
     * that tableau. The question is: which tableau does label k enter?
     *
     * Labels 0..m-1 are "player 1 labels" (row strategies).
     *   - In tableau 1: label i is a non-basic q_i variable (column i).
     *   - In tableau 2: label i is a basic s2_i slack variable (column i).
     *   If label i needs to enter: it enters tableau 1 (making q_i basic)
     *   or leaves tableau 2 (we pivot on column i in tab2 to move s2_i out,
     *   actually: we look for the label in the basis, then the dropped label
     *   is complementary).
     *
     * Labels m..m+n-1 are "player 2 labels" (column strategies).
     *   - In tableau 1: label m+j is a basic s1_j slack (column m+j).
     *   - In tableau 2: label m+j is a non-basic p_j variable (column m+j).
     *
     * The rule: when label k enters, it should enter the tableau where k is
     * currently NOT in the basis (i.e., k is non-basic there, so we can
     * pivot it in). We then determine which basic variable leaves, and that
     * leaving variable's label is the "dropped" label.
     *
     * Helper to determine which tableau: label k enters tableau where
     * it is currently non-basic.
     */

    int entering_label = k0;
    int max_iter = 2 * (m + n) * (m + n) + 100;  /* safety bound */

    for (int iter = 0; iter < max_iter; iter++) {
        int k = entering_label;

        /*
         * Determine which tableau label k is non-basic in.
         * Check if k is in basis1: if so, it's basic in tab1, must enter tab2.
         * Check if k is in basis2: if so, it's basic in tab2, must enter tab1.
         * If k is in neither basis, we need the initial assignment:
         *   - labels 0..m-1 are initially non-basic in tab1 (q variables)
         *   - labels m..m+n-1 are initially non-basic in tab2 (p variables)
         */
        int in_basis1 = 0, in_basis2 = 0;
        for (int j = 0; j < n; j++) {
            if (basis1[j] == k) { in_basis1 = 1; break; }
        }
        for (int i = 0; i < m; i++) {
            if (basis2[i] == k) { in_basis2 = 1; break; }
        }

        int use_tab;  /* 1 = pivot in tab1, 2 = pivot in tab2 */
        if (in_basis1 && !in_basis2) {
            use_tab = 2;  /* k is basic in tab1, so enter tab2 */
        } else if (in_basis2 && !in_basis1) {
            use_tab = 1;  /* k is basic in tab2, so enter tab1 */
        } else if (!in_basis1 && !in_basis2) {
            /* k is non-basic in both; use the "home" tableau */
            if (k < m) {
                use_tab = 1;  /* label 0..m-1: enter tab1 (q variable) */
            } else {
                use_tab = 2;  /* label m..m+n-1: enter tab2 (p variable) */
            }
        } else {
            /* k is basic in both -- should not happen */
            error("Lemke-Howson: label %d is basic in both tableaux", k);
            return R_NilValue;
        }

        int pivot_row, leaving_label;

        if (use_tab == 1) {
            /* Pivot label k into tableau 1 */
            pivot_row = min_ratio_test(tab1, n, ncols1, k);
            if (pivot_row < 0) {
                error("Lemke-Howson: unbounded pivot in tableau 1 for label %d", k);
                return R_NilValue;
            }
            leaving_label = basis1[pivot_row];
            pivot(tab1, n, ncols1, pivot_row, k);
            basis1[pivot_row] = k;
        } else {
            /* Pivot label k into tableau 2 */
            pivot_row = min_ratio_test(tab2, m, ncols2, k);
            if (pivot_row < 0) {
                error("Lemke-Howson: unbounded pivot in tableau 2 for label %d", k);
                return R_NilValue;
            }
            leaving_label = basis2[pivot_row];
            pivot(tab2, m, ncols2, pivot_row, k);
            basis2[pivot_row] = k;
        }

        /* Check termination: if the leaving label equals the initial label */
        if (leaving_label == k0) {
            break;
        }

        /* Complementary pivot: the leaving label enters the other tableau */
        entering_label = leaving_label;
    }

    /*
     * Extract the solution.
     *
     * From tableau 1: q_i (label i, i=0..m-1) is in the basis if basis1[j]==i
     * for some j. Its value is tab1[j * ncols1 + L] (the RHS of that row).
     *
     * From tableau 2: p_j (label m+j, j=0..n-1) is in the basis if basis2[i]==m+j
     * for some i. Its value is tab2[i * ncols2 + L].
     */
    double *q_raw = (double *) R_alloc(m, sizeof(double));
    double *p_raw = (double *) R_alloc(n, sizeof(double));

    for (int i = 0; i < m; i++) q_raw[i] = 0.0;
    for (int j = 0; j < n; j++) p_raw[j] = 0.0;

    /* Read q values from tableau 1 basis */
    for (int row = 0; row < n; row++) {
        int lbl = basis1[row];
        if (lbl >= 0 && lbl < m) {
            q_raw[lbl] = tab1[row * ncols1 + L];
        }
    }

    /* Read p values from tableau 2 basis */
    for (int row = 0; row < m; row++) {
        int lbl = basis2[row];
        if (lbl >= m && lbl < L) {
            p_raw[lbl - m] = tab2[row * ncols2 + L];
        }
    }

    /* Normalize to probability distributions */
    double sum_q = 0.0, sum_p = 0.0;
    for (int i = 0; i < m; i++) sum_q += q_raw[i];
    for (int j = 0; j < n; j++) sum_p += p_raw[j];

    /* Handle degenerate case: if sums are zero, return uniform */
    if (sum_q < TOL) {
        for (int i = 0; i < m; i++) q_raw[i] = 1.0 / m;
        sum_q = 1.0;
    }
    if (sum_p < TOL) {
        for (int j = 0; j < n; j++) p_raw[j] = 1.0 / n;
        sum_p = 1.0;
    }

    /* Build R result */
    SEXP result = PROTECT(allocVector(VECSXP, 4));
    SEXP names  = PROTECT(allocVector(STRSXP, 4));
    SEXP r_p    = PROTECT(allocVector(REALSXP, n));
    SEXP r_q    = PROTECT(allocVector(REALSXP, m));
    SEXP r_pay1 = PROTECT(allocVector(REALSXP, 1));
    SEXP r_pay2 = PROTECT(allocVector(REALSXP, 1));

    /* Normalized strategies */
    for (int i = 0; i < m; i++) REAL(r_q)[i] = q_raw[i] / sum_q;
    for (int j = 0; j < n; j++) REAL(r_p)[j] = p_raw[j] / sum_p;

    /* Compute expected payoffs: payoff1 = q^T A p, payoff2 = q^T B p */
    double pay1 = 0.0, pay2 = 0.0;
    for (int i = 0; i < m; i++) {
        for (int j = 0; j < n; j++) {
            double qp = REAL(r_q)[i] * REAL(r_p)[j];
            pay1 += qp * A[i + j * m];
            pay2 += qp * B[i + j * m];
        }
    }
    REAL(r_pay1)[0] = pay1;
    REAL(r_pay2)[0] = pay2;

    SET_STRING_ELT(names, 0, mkChar("p"));
    SET_STRING_ELT(names, 1, mkChar("q"));
    SET_STRING_ELT(names, 2, mkChar("payoff1"));
    SET_STRING_ELT(names, 3, mkChar("payoff2"));

    SET_VECTOR_ELT(result, 0, r_p);
    SET_VECTOR_ELT(result, 1, r_q);
    SET_VECTOR_ELT(result, 2, r_pay1);
    SET_VECTOR_ELT(result, 3, r_pay2);
    setAttrib(result, R_NamesSymbol, names);

    UNPROTECT(6);
    return result;
}
