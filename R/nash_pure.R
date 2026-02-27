#' Find Pure Strategy Nash Equilibria
#'
#' Enumerates all strategy profiles and identifies pure strategy Nash equilibria.
#' A profile is a Nash equilibrium if no player can unilaterally deviate to
#' improve their payoff.
#'
#' @param game A \code{\link{NormalFormGame}} object.
#' @return A list of equilibria. Each element is a list with components:
#' \describe{
#'   \item{profile}{Integer vector of strategy indices, one per player.}
#'   \item{payoffs}{Numeric vector of payoffs at the equilibrium profile.}
#'   \item{strategy_names}{Character vector of strategy names at the equilibrium.}
#' }
#' Returns an empty list if no pure strategy Nash equilibrium exists.
#' @export
pure_nash <- function(game) {
  stopifnot(inherits(game, "NormalFormGame"))

  n <- game$n_players
  ns <- game$n_strategies

  # Generate all strategy profiles
  all_profiles <- as.matrix(expand.grid(lapply(ns, seq_len)))

  equilibria <- list()

  for (row in seq_len(nrow(all_profiles))) {
    profile <- as.integer(all_profiles[row, ])
    payoffs <- game$payoff(profile)
    is_eq <- TRUE

    # Check each player for a profitable unilateral deviation
    for (p in seq_len(n)) {
      current_payoff <- payoffs[p]
      for (s in seq_len(ns[p])) {
        if (s == profile[p]) next
        # Build deviated profile
        deviated <- profile
        deviated[p] <- s
        deviated_payoff <- game$payoff(deviated)[p]
        if (deviated_payoff > current_payoff) {
          is_eq <- FALSE
          break
        }
      }
      if (!is_eq) break
    }

    if (is_eq) {
      strategy_names <- character(n)
      for (p in seq_len(n)) {
        strategy_names[p] <- game$strategies[[p]][profile[p]]
      }
      equilibria[[length(equilibria) + 1L]] <- list(
        profile = profile,
        payoffs = payoffs,
        strategy_names = strategy_names
      )
    }
  }

  equilibria
}
