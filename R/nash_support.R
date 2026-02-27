#' Generate All Non-Empty Subsets
#'
#' @param n Integer, generate subsets of \code{1:n}.
#' @return List of integer vectors, each a non-empty subset.
#' @keywords internal
.all_supports <- function(n) {
  supports <- list()
  for (k in seq_len(n)) {
    combos <- utils::combn(n, k, simplify = FALSE)
    supports <- c(supports, combos)
  }
  supports
}


#' Solve the Indifference Conditions for One Player
#'
#' Given a payoff submatrix, solve for the opponent's mixed strategy that makes
#' the player indifferent across all strategies in their support.
#'
#' The system is: M_sub %*% q = v * 1, sum(q) = 1, where M_sub is the
#' submatrix of the payoff matrix restricted to the support rows and columns.
#' This gives a (k+1) x (k+1) system when both supports have size k.
#'
#' @param M Payoff matrix for the player (full, m x n).
#' @param s_own Integer vector, the player's support indices.
#' @param s_opp Integer vector, the opponent's support indices.
#' @return A list with \code{prob} (probability vector over opponent's full
#'   strategy set) and \code{value} (the equilibrium expected payoff), or
#'   \code{NULL} if the system is singular or yields invalid probabilities.
#' @keywords internal
.solve_indifference <- function(M, s_own, s_opp, n_opp) {
  k_own <- length(s_own)
  k_opp <- length(s_opp)

  # Extract the submatrix: rows from s_own, columns from s_opp
  M_sub <- M[s_own, s_opp, drop = FALSE]

  # Build the linear system:
  # For k_own indifference conditions + 1 probability sum = 1 constraint,
  # we need k_own + 1 equations for k_opp + 1 unknowns (q values + v).
  # Only well-determined when k_own == k_opp.
  if (k_own != k_opp) {
    return(NULL)
  }

  k <- k_own  # = k_opp

  # System: M_sub %*% q - v * 1 = 0  (k equations)
  #         sum(q) = 1                 (1 equation)
  # Unknowns: q (k values) and v (1 scalar) => k+1 unknowns, k+1 equations

  # Build coefficient matrix A_sys and RHS b
  A_sys <- matrix(0, nrow = k + 1, ncol = k + 1)
  b <- numeric(k + 1)

  # Indifference conditions: M_sub[i,] %*% q - v = 0 for each i in 1..k
  A_sys[seq_len(k), seq_len(k)] <- M_sub
  A_sys[seq_len(k), k + 1] <- -1  # coefficient of v

  # Sum constraint: sum(q) = 1
  A_sys[k + 1, seq_len(k)] <- 1
  b[k + 1] <- 1

  # Solve
  sol <- tryCatch(solve(A_sys, b), error = function(e) NULL)
  if (is.null(sol)) return(NULL)

  q_support <- sol[seq_len(k)]
  v <- sol[k + 1]

  # Check all probabilities are non-negative
  if (any(q_support < -1e-12)) return(NULL)

  # Clamp small negative values to zero
  q_support <- pmax(q_support, 0)

  # Build the full probability vector
  q_full <- numeric(n_opp)
  q_full[s_opp] <- q_support

  list(prob = q_full, value = v)
}


#' Solve and Validate One Support Pair
#'
#' For a given pair of support sets, solve the indifference conditions for
#' both players and check that the resulting strategies form a Nash equilibrium.
#'
#' @param A Payoff matrix for player 1 (m x n).
#' @param B Payoff matrix for player 2 (m x n).
#' @param s1 Integer vector, player 1's support.
#' @param s2 Integer vector, player 2's support.
#' @param m Number of strategies for player 1.
#' @param n Number of strategies for player 2.
#' @param tol Numeric tolerance.
#' @return A list with \code{strategies} and \code{payoffs} if valid, or
#'   \code{NULL}.
#' @keywords internal
.solve_support_pair <- function(A, B, s1, s2, m, n, tol = 1e-10) {
  # Player 2's strategy q must make player 1 indifferent over s1
  # Player 1's payoff matrix is A; the indifference is over rows in s1,
  # columns in s2
  res_q <- .solve_indifference(A, s1, s2, n)
  if (is.null(res_q)) return(NULL)

  # Player 1's strategy p must make player 2 indifferent over s2
  # Player 2's payoff matrix is B; the indifference is over columns in s2,
  # rows in s1. We transpose: t(B) has rows = original columns, cols = original rows.
  res_p <- .solve_indifference(t(B), s2, s1, m)
  if (is.null(res_p)) return(NULL)

  q <- res_q$prob  # Player 2's mixed strategy
  p <- res_p$prob  # Player 1's mixed strategy
  v1 <- res_q$value  # Player 1's expected payoff
  v2 <- res_p$value  # Player 2's expected payoff

  # Verify: no profitable deviation outside support
  # For player 1: every strategy i NOT in s1 must yield expected payoff <= v1
  for (i in seq_len(m)) {
    if (i %in% s1) next
    ep <- sum(A[i, ] * q)
    if (ep > v1 + tol) return(NULL)
  }

  # For player 2: every strategy j NOT in s2 must yield expected payoff <= v2
  for (j in seq_len(n)) {
    if (j %in% s2) next
    ep <- sum(B[, j] * p)
    if (ep > v2 + tol) return(NULL)
  }

  list(
    strategies = list(p, q),
    payoffs = c(v1, v2)
  )
}


#' Support Enumeration for Nash Equilibria
#'
#' Find all Nash equilibria of a 2-player normal form game using the support
#' enumeration method. This algorithm enumerates all pairs of equal-sized
#' support sets for the two players, solves the resulting linear system of
#' indifference conditions, and validates each candidate equilibrium.
#'
#' @param game A \code{\link{NormalFormGame}} object (must be a 2-player game).
#' @param tol Numeric tolerance for validity checks (default \code{1e-10}).
#' @return A list of equilibria. Each equilibrium is a list with:
#'   \describe{
#'     \item{strategies}{A list of two numeric vectors, one per player, giving
#'       the mixed strategy (probability distribution over strategies).}
#'     \item{payoffs}{A numeric vector of length 2 giving expected payoffs.}
#'   }
#' @export
support_enumeration <- function(game, tol = 1e-10) {
  stopifnot(inherits(game, "NormalFormGame"))
  stopifnot(game$n_players == 2L)

  ns <- game$n_strategies
  m <- ns[1]
  n <- ns[2]

  # Extract payoff matrices A (player 1) and B (player 2)
  A <- matrix(0, nrow = m, ncol = n)
  B <- matrix(0, nrow = m, ncol = n)
  for (i in seq_len(m)) {
    for (j in seq_len(n)) {
      pf <- game$payoff(c(i, j))
      A[i, j] <- pf[1]
      B[i, j] <- pf[2]
    }
  }

  # Generate all non-empty supports for each player
  supports1 <- .all_supports(m)
  supports2 <- .all_supports(n)

  equilibria <- list()

  for (s1 in supports1) {
    for (s2 in supports2) {
      # Only consider equal-size supports (non-degenerate games)
      if (length(s1) != length(s2)) next

      result <- .solve_support_pair(A, B, s1, s2, m, n, tol)
      if (!is.null(result)) {
        equilibria[[length(equilibria) + 1]] <- result
      }
    }
  }

  equilibria
}
