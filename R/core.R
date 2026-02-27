#' Check if an allocation is in the core
#'
#' An allocation is in the core if it is efficient (sums to v(N)) and
#' satisfies coalition rationality: for every non-empty coalition S,
#' the sum of payoffs to members of S is at least v(S).
#'
#' @param game A \code{\link{CooperativeGame}} object.
#' @param allocation Named numeric vector of payoffs, names matching players.
#' @param tol Numerical tolerance for comparisons.
#' @return Logical, \code{TRUE} if the allocation is in the core.
#' @export
in_core <- function(game, allocation, tol = 1e-8) {
  stopifnot(inherits(game, "CooperativeGame"))
  players <- game$players
  n <- length(players)

  # allocation must be named and match players
  stopifnot(all(players %in% names(allocation)))

  # Efficiency: allocation must sum to v(N)
  v_N <- game$value(players)
  if (abs(sum(allocation[players]) - v_N) > tol) {
    return(FALSE)
  }

  # Coalition rationality: for every non-empty proper subset S of N,
  # sum(allocation[S]) >= v(S)
  for (mask in 1:(2^n - 2)) {
    S <- players[which(bitwAnd(mask, 2L^(seq_len(n) - 1L)) > 0L)]
    if (sum(allocation[S]) < game$value(S) - tol) {
      return(FALSE)
    }
  }

  TRUE
}


#' Generate core constraint matrix
#'
#' Returns the constraint system defining the core. The core is
#' \eqn{\{x : Ax \ge b, \sum x_i = v(N)\}}{x : Ax >= b, sum(x) = v(N)}
#' where rows of A correspond to non-empty proper subsets of N.
#'
#' @param game A \code{\link{CooperativeGame}} object.
#' @return A list with components:
#' \describe{
#'   \item{A}{Matrix with one row per coalition (non-empty proper subset of N)
#'     and one column per player. Entry is 1 if the player is in the coalition.}
#'   \item{b}{Numeric vector of coalition values v(S).}
#'   \item{v_N}{Value of the grand coalition.}
#' }
#' @export
core_constraints <- function(game) {
  stopifnot(inherits(game, "CooperativeGame"))
  players <- game$players
  n <- length(players)

  n_coalitions <- 2^n - 2  # non-empty proper subsets
  A <- matrix(0, nrow = n_coalitions, ncol = n)
  colnames(A) <- players
  b <- numeric(n_coalitions)

  row <- 0
  for (mask in 1:(2^n - 2)) {
    row <- row + 1
    members <- which(bitwAnd(mask, 2L^(seq_len(n) - 1L)) > 0L)
    A[row, members] <- 1
    S <- players[members]
    b[row] <- game$value(S)
  }

  list(A = A, b = b, v_N = game$value(players))
}


#' Compute the nucleolus of a cooperative game
#'
#' The nucleolus is the unique allocation that lexicographically minimises
#' the sorted vector of coalition excesses. It always exists and, when the
#' core is non-empty, lies inside the core.
#'
#' The implementation uses an iterative linear-programming approach
#' (Maschler, Peleg & Shapley 1979) solved with a built-in simplex
#' method so that no external package dependencies are required.
#'
#' @param game A \code{\link{CooperativeGame}} object.
#' @param tol Numerical tolerance used in the simplex solver.
#' @return Named numeric vector giving each player's nucleolus payoff.
#' @export
nucleolus <- function(game, tol = 1e-10) {
  stopifnot(inherits(game, "CooperativeGame"))
  players <- game$players
  n <- length(players)
  v_N <- game$value(players)

  # Build the coalition table (non-empty proper subsets)
  constr <- core_constraints(game)
  A_coal <- constr$A          # m x n  (m = 2^n - 2)
  b_coal <- constr$b          # m-vector
  m <- nrow(A_coal)

  # Active coalition indices (those not yet fixed)
  active <- seq_len(m)

  # Equality constraints accumulated across iterations.
  # Always includes the efficiency constraint: 1'x = v_N.
  eq_A <- matrix(1, nrow = 1, ncol = n)
  eq_b <- v_N

  x <- rep(v_N / n, n)  # initial guess (will be overwritten)

  for (iter in seq_len(m + n)) {
    n_active <- length(active)
    if (n_active == 0) break

    # Solve: max eps
    #   s.t. A_coal[active,] %*% x - eps >= b_coal[active]
    #        eq_A %*% x = eq_b
    #
    # Decision variables: x (n vars, unrestricted), eps (1 var, unrestricted)
    # Total: n + 1 variables
    #
    # Rewrite as standard form for the simplex:
    #   Inequality constraints (>=):
    #     A_coal[active,] %*% x - eps >= b_coal[active]
    #   Equality constraints:
    #     eq_A %*% x = eq_b

    sol <- .solve_lp_max(
      obj = c(rep(0, n), 1),    # maximise eps
      A_ge = cbind(A_coal[active, , drop = FALSE], -1),
      b_ge = b_coal[active],
      A_eq = cbind(eq_A, 0),
      b_eq = eq_b,
      tol = tol
    )

    if (is.null(sol)) break

    x <- sol[seq_len(n)]
    eps_star <- sol[n + 1]

    # Determine which active coalitions are tight (excess == eps_star)
    excesses <- as.numeric(A_coal[active, , drop = FALSE] %*% x) - b_coal[active]
    tight <- which(abs(excesses - eps_star) < max(sqrt(tol), abs(eps_star) * 1e-6, 1e-7))

    if (length(tight) == 0) break

    # Add tight coalitions as new equality constraints
    new_eq_A <- A_coal[active[tight], , drop = FALSE]
    new_eq_b <- b_coal[active[tight]] + eps_star

    eq_A <- rbind(eq_A, new_eq_A)
    eq_b <- c(eq_b, new_eq_b)

    # Remove tight coalitions from active set
    active <- active[-tight]

    # Check if x is fully determined (rank of eq_A == n)
    if (qr(eq_A)$rank >= n) break
  }

  names(x) <- players
  x
}


# -----------------------------------------------------------------------
# Internal: LP solver using big-M simplex method
#
# Solves:
#   max  obj'z
#   s.t. A_ge %*% z >= b_ge   (>= constraints)
#        A_eq %*% z  = b_eq   (equality constraints)
#        z unrestricted
#
# Approach:
#   - Split each unrestricted z_j into z_j+ - z_j- (both >= 0)
#   - Convert >= to <= by multiplying by -1
#   - Add slacks for <= constraints
#   - Use big-M method for Phase I (add artificials for equality rows
#     and for rows where b was originally negative after slack addition)
# -----------------------------------------------------------------------
.solve_lp_max <- function(obj, A_ge = NULL, b_ge = NULL,
                           A_eq = NULL, b_eq = NULL, tol = 1e-10) {
  n_orig <- length(obj)
  m_ge <- if (is.null(A_ge)) 0L else nrow(A_ge)
  m_eq <- if (is.null(A_eq)) 0L else nrow(A_eq)
  m <- m_ge + m_eq
  if (m == 0L) return(rep(0, n_orig))

  # Convert >= constraints to <= by negation:
  #   A_ge %*% z >= b_ge  =>  -A_ge %*% z <= -b_ge
  if (m_ge > 0) {
    A_le <- -A_ge
    b_le <- -b_ge
  } else {
    A_le <- matrix(0, 0, n_orig)
    b_le <- numeric(0)
  }

  # After splitting z = z+ - z-:
  # For <= rows: A_le * z+ - A_le * z- + slack = b_le
  # For eq rows: A_eq * z+ - A_eq * z- = b_eq
  # All new vars >= 0.

  n_split <- 2 * n_orig
  n_slack <- m_ge
  # We'll need artificials for certain rows (explained below)

  # Build constraint matrix without artificials first
  # Rows: [le_rows; eq_rows]
  # Cols: [z+; z-; slacks]
  n_vars <- n_split + n_slack
  A_full <- matrix(0, nrow = m, ncol = n_vars)
  b_full <- numeric(m)

  if (m_ge > 0) {
    A_full[1:m_ge, 1:n_orig] <- A_le                            # z+
    A_full[1:m_ge, (n_orig + 1):n_split] <- -A_le               # z-
    A_full[1:m_ge, (n_split + 1):(n_split + n_slack)] <- diag(m_ge)  # slack
    b_full[1:m_ge] <- b_le
  }

  if (m_eq > 0) {
    rows_eq <- (m_ge + 1):m
    A_full[rows_eq, 1:n_orig] <- A_eq
    A_full[rows_eq, (n_orig + 1):n_split] <- -A_eq
    b_full[rows_eq] <- b_eq
  }

  # Ensure b >= 0 by multiplying negative rows by -1
  # For <= rows this flips to >=, so the slack becomes a surplus (negative slack).
  # For eq rows flipping is fine (equality is symmetric).
  # After flipping, rows that were <= with negative b become >= rows.
  # We need artificials for: (a) eq rows, and (b) flipped <= rows (now >= rows).
  need_art <- rep(FALSE, m)

  for (i in seq_len(m)) {
    if (b_full[i] < -tol) {
      A_full[i, ] <- -A_full[i, ]
      b_full[i] <- -b_full[i]
      need_art[i] <- TRUE  # was <=, now >=, slack is negative => need artificial
    } else if (i > m_ge) {
      # Equality row with non-negative b: needs artificial
      need_art[i] <- TRUE
    }
    # <= row with b >= 0: slack provides feasible basis element, no artificial needed
  }

  # Add artificial columns
  art_indices <- which(need_art)
  n_art <- length(art_indices)

  if (n_art > 0) {
    A_art <- matrix(0, nrow = m, ncol = n_art)
    for (k in seq_along(art_indices)) {
      A_art[art_indices[k], k] <- 1
    }
    A_all <- cbind(A_full, A_art)
  } else {
    A_all <- A_full
  }

  n_total <- ncol(A_all)
  art_cols <- if (n_art > 0) (n_vars + 1):n_total else integer(0)

  # Set up initial basis
  basis <- integer(m)
  # For rows without artificials, the slack is the basis variable
  # For rows with artificials, the artificial is the basis variable
  slack_used <- 0
  art_used <- 0
  for (i in seq_len(m)) {
    if (need_art[i]) {
      art_used <- art_used + 1
      basis[i] <- n_vars + art_used
    } else {
      # Must be a <= row (i <= m_ge) with b >= 0; slack variable is basis
      # Slack for row i is column n_split + i
      basis[i] <- n_split + i
    }
  }

  # Big-M objective: original objective minus M * sum(artificials)
  bigM <- 1e6
  c_obj <- rep(0, n_total)
  c_obj[1:n_orig] <- obj              # z+
  c_obj[(n_orig + 1):n_split] <- -obj # z-
  if (n_art > 0) {
    c_obj[art_cols] <- -bigM
  }

  # Solve using simplex
  res <- .simplex_core(A_all, b_full, c_obj, basis, tol)
  if (is.null(res)) return(NULL)

  x_full <- res$x

  # Check that artificials are zero (feasibility)
  if (n_art > 0 && any(x_full[art_cols] > tol)) {
    return(NULL)  # infeasible
  }

  # Recover original variables
  z <- x_full[1:n_orig] - x_full[(n_orig + 1):n_split]
  z
}


# Core simplex iteration (maximisation)
# Solves: max c'x s.t. Ax = b, x >= 0
# Given an initial basic feasible solution.
.simplex_core <- function(A, b, cc, basis, tol, max_iter = 20000) {
  m <- nrow(A)
  n <- ncol(A)

  # Ensure the tableau is in canonical form for the given basis
  for (i in seq_len(m)) {
    piv_col <- basis[i]
    piv_val <- A[i, piv_col]
    if (abs(piv_val) < tol) next
    if (abs(piv_val - 1) > tol) {
      A[i, ] <- A[i, ] / piv_val
      b[i] <- b[i] / piv_val
    }
    for (k in seq_len(m)) {
      if (k != i) {
        factor <- A[k, piv_col]
        if (abs(factor) > tol) {
          A[k, ] <- A[k, ] - factor * A[i, ]
          b[k] <- b[k] - factor * b[i]
        }
      }
    }
  }

  for (it in seq_len(max_iter)) {
    # Compute reduced costs: rc_j = c_j - c_B' * A[,j]
    c_B <- cc[basis]
    rc <- cc - as.numeric(crossprod(c_B, A))

    # Bland's rule: smallest index with positive reduced cost
    candidates <- which(rc > tol)
    if (length(candidates) == 0) break  # optimal
    entering <- candidates[1]

    # Minimum ratio test
    col_enter <- A[, entering]
    valid <- col_enter > tol
    if (!any(valid)) return(NULL)  # unbounded

    ratios <- rep(Inf, m)
    ratios[valid] <- b[valid] / col_enter[valid]
    min_rat <- min(ratios)
    # Bland's rule tie-breaking: pick the row whose basis variable has smallest index
    ties <- which(abs(ratios - min_rat) < tol * (1 + abs(min_rat)))
    if (length(ties) == 1) {
      leaving <- ties[1]
    } else {
      leaving <- ties[which.min(basis[ties])]
    }

    # Pivot
    piv <- A[leaving, entering]
    A[leaving, ] <- A[leaving, ] / piv
    b[leaving] <- b[leaving] / piv
    for (k in seq_len(m)) {
      if (k != leaving) {
        factor <- A[k, entering]
        if (abs(factor) > tol) {
          A[k, ] <- A[k, ] - factor * A[leaving, ]
          b[k] <- b[k] - factor * b[leaving]
        }
      }
    }
    basis[leaving] <- entering
  }

  x <- rep(0, n)
  x[basis] <- pmax(b, 0)

  list(x = x, basis = basis)
}
