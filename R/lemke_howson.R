#' Lemke-Howson Algorithm for Nash Equilibrium
#'
#' Finds one Nash equilibrium of a 2-player bimatrix game using
#' complementary pivoting. Different starting labels may yield different
#' equilibria.
#'
#' @param game A 2-player \code{\link{NormalFormGame}}.
#' @param init_label Integer (1 to m+n): starting label for the pivoting
#'   algorithm. Default 1. Different labels may lead to different equilibria.
#' @return A list with:
#'   \describe{
#'     \item{strategies}{A list of two numeric vectors, one per player, giving
#'       the mixed strategy (probability distribution over strategies).}
#'     \item{payoffs}{A numeric vector of length 2 giving expected payoffs.}
#'   }
#' @export
lemke_howson <- function(game, init_label = 1L) {
  stopifnot(inherits(game, "NormalFormGame"))
  stopifnot(game$n_players == 2)

  m <- game$n_strategies[1]
  n <- game$n_strategies[2]
  init_label <- as.integer(init_label)
  stopifnot(init_label >= 1L && init_label <= m + n)

  # Extract payoff matrices
  A <- matrix(0, nrow = m, ncol = n)
  B <- matrix(0, nrow = m, ncol = n)
  for (i in seq_len(m)) {
    for (j in seq_len(n)) {
      p <- game$payoff(c(i, j))
      A[i, j] <- p[1]
      B[i, j] <- p[2]
    }
  }

  # Lemke-Howson requires strictly positive payoff matrices.
  # Shift both matrices so all entries are > 0, then adjust payoffs back.
  shift_A <- 0
  shift_B <- 0
  min_A <- min(A)
  min_B <- min(B)
  if (min_A <= 0) {
    shift_A <- abs(min_A) + 1
    A <- A + shift_A
  }
  if (min_B <= 0) {
    shift_B <- abs(min_B) + 1
    B <- B + shift_B
  }

  result <- .Call("c_lemke_howson", A, B, as.integer(m), as.integer(n),
                  init_label, PACKAGE = "econtk")

  # Adjust payoffs back by removing the shift
  payoff1 <- result$payoff1 - shift_A
  payoff2 <- result$payoff2 - shift_B

  list(
    strategies = list(result$q, result$p),  # q = player 1, p = player 2
    payoffs = c(payoff1, payoff2)
  )
}
