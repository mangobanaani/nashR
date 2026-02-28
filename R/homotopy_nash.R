#' Homotopy Continuation for Nash Equilibrium
#'
#' Finds a mixed strategy Nash equilibrium of an N-player normal form game
#' using a logit-response homotopy method implemented in C. Traces a smooth
#' path from a prior belief toward an equilibrium using adaptive continuation
#' with softmax best-response smoothing and beta-annealing.
#'
#' @param game A \code{\link{NormalFormGame}} object.
#' @param start An optional starting profile: a list of numeric vectors
#'   (one per player) specifying prior beliefs. Default is uniform over
#'   each player's strategies.
#' @return A list with:
#'   \describe{
#'     \item{strategies}{A list of numeric vectors, one per player.}
#'     \item{payoffs}{A numeric vector of expected payoffs.}
#'     \item{converged}{Logical indicating whether the algorithm converged.}
#'   }
#' @export
homotopy_nash <- function(game, start = NULL) {
  stopifnot(inherits(game, "NormalFormGame"))

  n <- game$n_players
  ns <- game$n_strategies

  # Build the prior (starting point)
  if (is.null(start)) {
    prior <- unlist(lapply(ns, function(k) rep(1 / k, k)))
  } else {
    stopifnot(is.list(start), length(start) == n)
    for (p in seq_len(n)) {
      stopifnot(is.numeric(start[[p]]), length(start[[p]]) == ns[p])
      stopifnot(abs(sum(start[[p]]) - 1) < 1e-8)
    }
    prior <- unlist(start)
  }

  # Flatten payoff array (already stored in R's column-major order)
  payoffs <- as.numeric(game$payoff_array)

  result <- .Call("c_homotopy_nash",
                  payoffs,
                  as.integer(n),
                  as.integer(ns),
                  prior,
                  PACKAGE = "nashR")

  list(
    strategies = result$strategies,
    payoffs = result$payoffs,
    converged = result$converged
  )
}
