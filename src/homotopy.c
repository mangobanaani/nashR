/*
 * Logit-response homotopy for N-player mixed strategy Nash equilibrium.
 *
 * Uses a logit (softmax) best-response smoothing:
 *   x_p[a] = exp(beta * v_p[a]) / sum_a' exp(beta * v_p[a'])
 * where v_p[a] = (1-t)*prior_ep[a] + t*game_ep[a]
 *
 * Traces from t=0 (prior) to t=1 with adaptive step size, then
 * anneals beta upward to sharpen the response toward an approximate NE.
 */

#include "nashR.h"
#include <stdlib.h>
#include <math.h>
#include <string.h>

#define HTOL 1e-10
#define MAX_ITER 50000
#define MIN_STEP 1e-14
#define MAX_STEP 0.1
#define INIT_STEP 0.01
#define NEWTON_TOL 1e-12
#define NEWTON_MAX 30

/*
 * Compute expected payoff for player p playing pure strategy action,
 * given other players' mixed strategies x.
 */
static double expected_payoff(const double *payoffs, int n_players,
                              const int *n_strats, const int *strides,
                              const int *x_offsets, const double *x,
                              int player, int action) {
    int total_opp_profiles = 1;
    for (int p = 0; p < n_players; p++) {
        if (p != player) total_opp_profiles *= n_strats[p];
    }

    double ep = 0.0;

    for (int idx = 0; idx < total_opp_profiles; idx++) {
        int rem = idx;
        int payoff_index = action * strides[player] + player * strides[n_players];
        double prob = 1.0;

        for (int p = 0; p < n_players; p++) {
            if (p == player) continue;
            int a_p = rem % n_strats[p];
            rem /= n_strats[p];
            payoff_index += a_p * strides[p];
            prob *= x[x_offsets[p] + a_p];
        }

        ep += prob * payoffs[payoff_index];
    }

    return ep;
}

/* Softmax with temperature, numerically stable */
static void softmax(double *out, const double *v, int n, double beta) {
    double max_v = v[0];
    for (int i = 1; i < n; i++) {
        if (v[i] > max_v) max_v = v[i];
    }
    double sum = 0.0;
    for (int i = 0; i < n; i++) {
        out[i] = exp(beta * (v[i] - max_v));
        sum += out[i];
    }
    for (int i = 0; i < n; i++) {
        out[i] /= sum;
        if (out[i] < 1e-15) out[i] = 1e-15;
    }
    sum = 0.0;
    for (int i = 0; i < n; i++) sum += out[i];
    for (int i = 0; i < n; i++) out[i] /= sum;
}

/*
 * Compute fixed-point residual F(x,t) = softmax_response(x,t) - x.
 * v and sm are pre-allocated workspace of size max_strats.
 * Returns L2 norm.
 */
static double compute_residual(double *residual, const double *x, double t,
                               const double *payoffs, const double *prior,
                               int n_players, const int *n_strats,
                               const int *strides, const int *x_offsets,
                               int dim, double beta,
                               double *v, double *sm) {
    double norm = 0.0;

    for (int p = 0; p < n_players; p++) {
        int ns = n_strats[p];

        for (int a = 0; a < ns; a++) {
            double game_ep = expected_payoff(payoffs, n_players, n_strats,
                                             strides, x_offsets, x, p, a);
            double prior_ep = expected_payoff(payoffs, n_players, n_strats,
                                              strides, x_offsets, prior, p, a);
            v[a] = (1.0 - t) * prior_ep + t * game_ep;
        }

        softmax(sm, v, ns, beta);

        for (int a = 0; a < ns; a++) {
            residual[x_offsets[p] + a] = sm[a] - x[x_offsets[p] + a];
            norm += residual[x_offsets[p] + a] * residual[x_offsets[p] + a];
        }
    }

    return sqrt(norm);
}

/*
 * Damped fixed-point iteration: x <- x + damping * (softmax_response - x).
 * workspace: v and sm arrays of size max_strats, residual of size dim.
 */
static double iterate_fixed_point(double *x, double t,
                                  const double *payoffs, const double *prior,
                                  int n_players, const int *n_strats,
                                  const int *strides, const int *x_offsets,
                                  int dim, double beta, double damping,
                                  double *residual, double *v, double *sm) {
    double norm = 0.0;

    for (int iter = 0; iter < NEWTON_MAX; iter++) {
        norm = compute_residual(residual, x, t, payoffs, prior,
                                n_players, n_strats, strides, x_offsets,
                                dim, beta, v, sm);

        if (norm < NEWTON_TOL) break;

        for (int i = 0; i < dim; i++) {
            x[i] += damping * residual[i];
            if (x[i] < 1e-15) x[i] = 1e-15;
        }

        for (int p = 0; p < n_players; p++) {
            double sum = 0.0;
            int ns = n_strats[p];
            for (int a = 0; a < ns; a++) {
                sum += x[x_offsets[p] + a];
            }
            for (int a = 0; a < ns; a++) {
                x[x_offsets[p] + a] /= sum;
            }
        }
    }

    return norm;
}

SEXP c_homotopy_nash(SEXP payoffs_r, SEXP n_players_r, SEXP n_strats_r,
                     SEXP prior_r) {
    int n_players = INTEGER(n_players_r)[0];
    int *n_strats = INTEGER(n_strats_r);
    double *payoffs = REAL(payoffs_r);
    double *prior_start = REAL(prior_r);

    /* Compute dimensions and offsets */
    int dim = 0;
    int max_strats = 0;
    int *x_offsets = (int *)R_alloc(n_players + 1, sizeof(int));
    for (int p = 0; p < n_players; p++) {
        x_offsets[p] = dim;
        dim += n_strats[p];
        if (n_strats[p] > max_strats) max_strats = n_strats[p];
    }
    x_offsets[n_players] = dim;

    /* Strides for column-major payoff array */
    int *strides = (int *)R_alloc(n_players + 1, sizeof(int));
    strides[0] = 1;
    for (int p = 1; p <= n_players; p++) {
        strides[p] = strides[p - 1] * n_strats[p - 1];
    }

    /* Working arrays (all R_alloc, auto-freed) */
    double *x = (double *)R_alloc(dim, sizeof(double));
    double *prior = (double *)R_alloc(dim, sizeof(double));
    double *x_prev = (double *)R_alloc(dim, sizeof(double));
    double *residual = (double *)R_alloc(dim, sizeof(double));
    double *v = (double *)R_alloc(max_strats, sizeof(double));
    double *sm = (double *)R_alloc(max_strats, sizeof(double));

    memcpy(prior, prior_start, dim * sizeof(double));
    memcpy(x, prior, dim * sizeof(double));

    double beta = 20.0;
    double beta_max = 200.0;
    double t = 0.0;
    double dt = INIT_STEP;
    int converged = 0;

    /* Phase 1: trace path from t=0 to t=1 */
    for (int iter = 0; iter < MAX_ITER && t < 1.0; iter++) {
        double t_next = t + dt;
        if (t_next > 1.0) t_next = 1.0;

        memcpy(x_prev, x, dim * sizeof(double));

        double norm = iterate_fixed_point(x, t_next, payoffs, prior,
                                          n_players, n_strats, strides,
                                          x_offsets, dim, beta, 0.8,
                                          residual, v, sm);

        if (norm < HTOL) {
            t = t_next;
            dt = fmin(dt * 1.5, MAX_STEP);
        } else {
            memcpy(x, x_prev, dim * sizeof(double));
            dt *= 0.5;
            if (dt < MIN_STEP) {
                /* Step size too small; jump t forward and re-solve */
                t = t_next;
                dt = INIT_STEP;
                iterate_fixed_point(x, t, payoffs, prior,
                                    n_players, n_strats, strides,
                                    x_offsets, dim, beta, 0.8,
                                    residual, v, sm);
            }
        }
    }

    /* Phase 2: anneal beta upward at t=1 to sharpen response */
    if (t >= 1.0 - HTOL) {
        t = 1.0;
        for (double b = beta; b <= beta_max; b *= 1.5) {
            iterate_fixed_point(x, 1.0, payoffs, prior,
                                n_players, n_strats, strides,
                                x_offsets, dim, b, 0.5,
                                residual, v, sm);
        }

        /* Polish at max beta */
        for (int polish = 0; polish < 5; polish++) {
            iterate_fixed_point(x, 1.0, payoffs, prior,
                                n_players, n_strats, strides,
                                x_offsets, dim, beta_max, 0.5,
                                residual, v, sm);
        }

        /* Check convergence after polishing */
        double final_norm = compute_residual(residual, x, 1.0, payoffs, prior,
                                              n_players, n_strats, strides,
                                              x_offsets, dim, beta_max, v, sm);
        converged = (final_norm < 1e-6) ? 1 : 0;
    }

    /* Snap near-zero probabilities and renormalize */
    for (int p = 0; p < n_players; p++) {
        int ns = n_strats[p];
        double sum = 0.0;
        for (int a = 0; a < ns; a++) {
            double val = x[x_offsets[p] + a];
            if (val < 1e-8) val = 0.0;
            x[x_offsets[p] + a] = val;
            sum += val;
        }
        if (sum > 0) {
            for (int a = 0; a < ns; a++) {
                x[x_offsets[p] + a] /= sum;
            }
        }
    }

    /* Compute expected payoffs */
    double *exp_payoffs = (double *)R_alloc(n_players, sizeof(double));
    for (int p = 0; p < n_players; p++) {
        exp_payoffs[p] = 0.0;
        int ns = n_strats[p];
        for (int a = 0; a < ns; a++) {
            if (x[x_offsets[p] + a] > 0) {
                exp_payoffs[p] += x[x_offsets[p] + a] *
                    expected_payoff(payoffs, n_players, n_strats,
                                    strides, x_offsets, x, p, a);
            }
        }
    }

    /* Build R result -- constant PROTECT count */
    SEXP result = PROTECT(allocVector(VECSXP, 3));
    SEXP names = PROTECT(allocVector(STRSXP, 3));

    SEXP strat_list = PROTECT(allocVector(VECSXP, n_players));
    for (int p = 0; p < n_players; p++) {
        SEXP sv = PROTECT(allocVector(REALSXP, n_strats[p]));
        for (int a = 0; a < n_strats[p]; a++) {
            REAL(sv)[a] = x[x_offsets[p] + a];
        }
        SET_VECTOR_ELT(strat_list, p, sv);
        UNPROTECT(1); /* sv held by strat_list */
    }

    SEXP pay_vec = PROTECT(allocVector(REALSXP, n_players));
    for (int p = 0; p < n_players; p++) {
        REAL(pay_vec)[p] = exp_payoffs[p];
    }

    SEXP conv = PROTECT(allocVector(LGLSXP, 1));
    LOGICAL(conv)[0] = converged;

    SET_STRING_ELT(names, 0, mkChar("strategies"));
    SET_STRING_ELT(names, 1, mkChar("payoffs"));
    SET_STRING_ELT(names, 2, mkChar("converged"));

    SET_VECTOR_ELT(result, 0, strat_list);
    SET_VECTOR_ELT(result, 1, pay_vec);
    SET_VECTOR_ELT(result, 2, conv);
    setAttrib(result, R_NamesSymbol, names);

    UNPROTECT(5);
    return result;
}
