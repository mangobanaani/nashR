#' Check if a Game is Zero-Sum
#'
#' For a 2-player normal form game, check whether all payoff sums across
#' strategy profiles are zero (within floating-point tolerance).
#'
#' @param game A \code{\link{NormalFormGame}} object.
#' @param tol Numeric tolerance for comparing sums to zero.
#' @return Logical scalar.
#' @export
is_zero_sum <- function(game, tol = 1e-10) {
  stopifnot(inherits(game, "NormalFormGame"))
  stopifnot(game$n_players == 2L)

  ns <- game$n_strategies
  for (i in seq_len(ns[1])) {
    for (j in seq_len(ns[2])) {
      payoffs <- game$payoff(c(i, j))
      if (abs(sum(payoffs)) > tol) {
        return(FALSE)
      }
    }
  }
  TRUE
}


#' Check if a Game is Symmetric
#'
#' For a 2-player game with the same number of strategies for both players,
#' check the symmetry condition: \eqn{u_1(s_i, s_j) = u_2(s_j, s_i)} for all
#' strategy pairs \eqn{(i, j)}.
#'
#' @param game A \code{\link{NormalFormGame}} object.
#' @param tol Numeric tolerance for payoff comparisons.
#' @return Logical scalar.
#' @export
is_symmetric <- function(game, tol = 1e-10) {
  stopifnot(inherits(game, "NormalFormGame"))
  stopifnot(game$n_players == 2L)

  ns <- game$n_strategies
  if (ns[1] != ns[2]) {
    return(FALSE)
  }

  for (i in seq_len(ns[1])) {
    for (j in seq_len(ns[2])) {
      u1_ij <- game$payoff(c(i, j))[1]  # player 1's payoff at (i, j)
      u2_ji <- game$payoff(c(j, i))[2]  # player 2's payoff at (j, i)
      if (abs(u1_ij - u2_ji) > tol) {
        return(FALSE)
      }
    }
  }
  TRUE
}


#' Find Dominant Strategies
#'
#' For each player, check if any strategy strictly dominates all others
#' across every opponent strategy profile. Returns a list with one element
#' per player: the name of the dominant strategy, or \code{NULL} if none exists.
#'
#' @param game A \code{\link{NormalFormGame}} object.
#' @return A list of length \code{game$n_players}. Each element is either a
#'   character string (the dominant strategy name) or \code{NULL}.
#' @export
dominant_strategy <- function(game) {
  stopifnot(inherits(game, "NormalFormGame"))

  n <- game$n_players
  ns <- game$n_strategies
  result <- vector("list", n)

  # Generate all opponent profiles for each player
  # For player p, the opponents are all players except p
  for (p in seq_len(n)) {
    # Build opponent strategy index grid
    opp_dims <- ns[-p]
    if (length(opp_dims) == 0L) {
      # Single player game -- every strategy trivially dominates
      result[[p]] <- game$strategies[[p]][1]
      next
    }
    opp_profiles <- as.matrix(expand.grid(lapply(opp_dims, seq_len)))

    dominant <- NULL
    for (s in seq_len(ns[p])) {
      is_dominant <- TRUE
      for (s_alt in seq_len(ns[p])) {
        if (s_alt == s) next
        # s must strictly dominate s_alt for all opponent profiles
        for (row in seq_len(nrow(opp_profiles))) {
          opp <- opp_profiles[row, ]
          # Build full profiles
          profile_s <- integer(n)
          profile_alt <- integer(n)
          profile_s[p] <- s
          profile_alt[p] <- s_alt
          opp_idx <- seq_len(n)[-p]
          profile_s[opp_idx] <- opp
          profile_alt[opp_idx] <- opp

          payoff_s <- game$payoff(profile_s)[p]
          payoff_alt <- game$payoff(profile_alt)[p]

          if (payoff_s <= payoff_alt) {
            is_dominant <- FALSE
            break
          }
        }
        if (!is_dominant) break
      }
      if (is_dominant) {
        dominant <- game$strategies[[p]][s]
        break
      }
    }
    result[p] <- list(dominant)
  }
  result
}


#' Compute Best Response
#'
#' For a 2-player game, compute the expected payoff for each of a player's
#' strategies against a given mixed strategy of the opponent, and return the
#' indices of all strategies that achieve the maximum expected payoff.
#'
#' @param game A \code{\link{NormalFormGame}} object.
#' @param player Integer, the player (1 or 2).
#' @param opponent_strategy Numeric vector giving the opponent's mixed strategy
#'   (probability distribution over the opponent's strategies).
#' @param tol Numeric tolerance for comparing expected payoffs.
#' @return Integer vector of strategy indices that are best responses.
#' @export
best_response <- function(game, player, opponent_strategy, tol = 1e-10) {
  stopifnot(inherits(game, "NormalFormGame"))
  stopifnot(game$n_players == 2L)
  stopifnot(player %in% c(1L, 2L))

  opponent <- 3L - player
  ns <- game$n_strategies

  stopifnot(length(opponent_strategy) == ns[opponent])

  expected_payoffs <- numeric(ns[player])
  for (s in seq_len(ns[player])) {
    ep <- 0
    for (o in seq_len(ns[opponent])) {
      if (player == 1L) {
        profile <- c(s, o)
      } else {
        profile <- c(o, s)
      }
      ep <- ep + opponent_strategy[o] * game$payoff(profile)[player]
    }
    expected_payoffs[s] <- ep
  }

  max_ep <- max(expected_payoffs)
  which(expected_payoffs >= max_ep - tol)
}


#' Check if a Strategy Profile is Pareto Optimal
#'
#' Check whether a given strategy profile (specified as an integer vector of
#' strategy indices) is Pareto optimal, i.e., there is no other profile that
#' makes all players weakly better off and at least one player strictly better
#' off.
#'
#' @param game A \code{\link{NormalFormGame}} object.
#' @param profile Integer vector of strategy indices, one per player.
#' @return Logical scalar.
#' @export
is_pareto_optimal <- function(game, profile) {
  stopifnot(inherits(game, "NormalFormGame"))

  n <- game$n_players
  ns <- game$n_strategies
  stopifnot(length(profile) == n)

  payoffs_at_profile <- game$payoff(profile)

  # Generate all strategy profiles
  all_profiles <- as.matrix(expand.grid(lapply(ns, seq_len)))

  for (row in seq_len(nrow(all_profiles))) {
    alt <- all_profiles[row, ]
    # Skip the profile itself
    if (identical(as.integer(alt), as.integer(profile))) next

    alt_payoffs <- game$payoff(alt)

    # Check for Pareto domination: weakly better for all, strictly better for some
    weakly_better <- all(alt_payoffs >= payoffs_at_profile)
    strictly_better <- any(alt_payoffs > payoffs_at_profile)

    if (weakly_better && strictly_better) {
      return(FALSE)
    }
  }
  TRUE
}
