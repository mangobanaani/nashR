#' Shapley Value
#'
#' Compute the Shapley value for a cooperative game with transferable utility.
#'
#' The Shapley value assigns to each player a fair share of the grand
#' coalition's worth, based on their average marginal contribution across
#' all possible orderings of players.
#'
#' For player \eqn{i}, the Shapley value is:
#' \deqn{\phi_i = \sum_{S \subseteq N \setminus \{i\}}
#'   \frac{|S|!(n-|S|-1)!}{n!} [v(S \cup \{i\}) - v(S)]}
#'
#' @param game A \code{\link{CooperativeGame}} object.
#' @param n_samples Integer or NULL. If NULL and the number of players is
#'   at most 12, the exact Shapley value is computed by enumerating all
#'   \eqn{2^n} coalitions. If non-NULL (or if n > 12 and NULL), Monte Carlo
#'   sampling over random permutations is used. For Monte Carlo, the default
#'   is 10000 samples.
#' @return A named numeric vector of Shapley values, one per player.
#'   Names correspond to the player names in the game.
#'   The values sum to \eqn{v(N)}, the value of the grand coalition.
#' @export
shapley_value <- function(game, n_samples = NULL) {
  stopifnot(inherits(game, "CooperativeGame"))

  players <- game$players
  n <- game$n_players

  use_exact <- is.null(n_samples) && n <= 12L

  if (use_exact) {
    shapley_exact(game, players, n)
  } else {
    if (is.null(n_samples)) n_samples <- 10000L
    shapley_monte_carlo(game, players, n, n_samples)
  }
}


#' Exact Shapley value computation
#'
#' Enumerates all \eqn{2^n} coalitions and computes the exact Shapley value
#' using the combinatorial formula.
#'
#' @param game A \code{\link{CooperativeGame}} object.
#' @param players Character vector of player names.
#' @param n Integer, number of players.
#' @return Named numeric vector of Shapley values.
#' @noRd
shapley_exact <- function(game, players, n) {
  phi <- numeric(n)
  names(phi) <- players

  n_fact <- factorial(n)

  # Enumerate all subsets using bitmasks 0..(2^n - 1)
  n_subsets <- 2L^n

  for (i in seq_len(n)) {
    i_bit <- 2L^(i - 1L)

    for (mask in 0:(n_subsets - 1L)) {
      # Skip if player i is in this subset
      if (bitwAnd(mask, i_bit) > 0L) next

      # Build coalition S from mask
      s_size <- 0L
      S <- character(0)
      if (mask > 0L) {
        members <- which(bitwAnd(mask, 2L^(seq_len(n) - 1L)) > 0L)
        S <- players[members]
        s_size <- length(members)
      }

      # Shapley weight: |S|! * (n - |S| - 1)! / n!
      weight <- factorial(s_size) * factorial(n - s_size - 1L) / n_fact

      # Marginal contribution: v(S union {i}) - v(S)
      S_with_i <- c(S, players[i])
      marginal <- game$value(S_with_i) - game$value(S)

      phi[i] <- phi[i] + weight * marginal
    }
  }

  phi
}


#' Monte Carlo Shapley value estimation
#'
#' Estimates the Shapley value by sampling random permutations of players
#' and averaging marginal contributions.
#'
#' @param game A \code{\link{CooperativeGame}} object.
#' @param players Character vector of player names.
#' @param n Integer, number of players.
#' @param n_samples Integer, number of random permutations to sample.
#' @return Named numeric vector of estimated Shapley values.
#' @noRd
shapley_monte_carlo <- function(game, players, n, n_samples) {
  phi <- numeric(n)
  names(phi) <- players

  for (s in seq_len(n_samples)) {
    perm <- sample(players)
    prev_value <- 0

    for (j in seq_len(n)) {
      coalition <- perm[seq_len(j)]
      curr_value <- game$value(coalition)
      marginal <- curr_value - prev_value
      player <- perm[j]
      phi[player] <- phi[player] + marginal
      prev_value <- curr_value
    }
  }

  phi / n_samples
}
