#' Bayesian Game
#'
#' R6 class representing a Bayesian (incomplete information) game. Each player
#' has a set of possible types, a prior distribution over type profiles, and
#' a payoff function that depends on the type profile and action profile.
#'
#' @field players Character vector of player names.
#' @field type_sets List of character vectors, one per player, giving the
#'   possible types for that player.
#' @field priors Prior distribution over type profiles. Either a list of numeric
#'   vectors (independent priors, one per player) or an array (joint prior over
#'   all type profiles).
#' @field strategies List of character vectors, one per player, giving the
#'   available actions.
#' @field payoff_fn Function with signature
#'   \code{function(player, type_profile, action_profile)} returning a numeric
#'   scalar payoff.
#'
#' @export
BayesianGame <- R6::R6Class("BayesianGame",
  public = list(
    players = NULL,
    type_sets = NULL,
    priors = NULL,
    strategies = NULL,
    payoff_fn = NULL,

    #' @description Create a new Bayesian game.
    #' @param players Character vector of player names.
    #' @param type_sets List of character vectors of types per player.
    #' @param priors List of numeric vectors (independent) or array (joint).
    #' @param strategies List of character vectors of actions per player.
    #' @param payoff_fn Function(player, type_profile, action_profile) -> numeric.
    initialize = function(players, type_sets, priors, strategies, payoff_fn) {
      stopifnot(is.character(players), length(players) >= 2)
      n <- length(players)

      stopifnot(is.list(type_sets), length(type_sets) == n)
      for (i in seq_len(n)) {
        stopifnot(is.character(type_sets[[i]]), length(type_sets[[i]]) >= 1)
      }

      stopifnot(is.list(strategies), length(strategies) == n)
      for (i in seq_len(n)) {
        stopifnot(is.character(strategies[[i]]), length(strategies[[i]]) >= 1)
      }

      stopifnot(is.function(payoff_fn))

      # Validate priors
      if (is.list(priors) && !is.array(priors)) {
        stopifnot(length(priors) == n)
        for (i in seq_len(n)) {
          stopifnot(is.numeric(priors[[i]]))
          stopifnot(length(priors[[i]]) == length(type_sets[[i]]))
          stopifnot(abs(sum(priors[[i]]) - 1) < 1e-10)
          stopifnot(all(priors[[i]] >= 0))
        }
      } else if (is.array(priors) || is.numeric(priors)) {
        expected_dim <- vapply(type_sets, length, integer(1))
        if (is.array(priors)) {
          stopifnot(identical(as.integer(dim(priors)), as.integer(expected_dim)))
        } else {
          stopifnot(length(priors) == prod(expected_dim))
          dim(priors) <- expected_dim
        }
        stopifnot(abs(sum(priors) - 1) < 1e-10)
        stopifnot(all(priors >= 0))
      } else {
        stop("priors must be a list of numeric vectors or an array")
      }

      self$players <- players
      self$type_sets <- type_sets
      self$priors <- priors
      self$strategies <- strategies
      self$payoff_fn <- payoff_fn
    },

    #' @description Get payoff for a player given type and action profiles.
    #' @param player Integer player index.
    #' @param type_profile Integer vector of type indices, one per player.
    #' @param action_profile Integer vector of action indices, one per player.
    #' @return Numeric scalar payoff.
    payoff = function(player, type_profile, action_profile) {
      self$payoff_fn(player, type_profile, action_profile)
    },

    #' @description Convert to a normal form game by enumerating all
    #'   type-contingent strategies (complete plans mapping types to actions).
    #' @return A \code{\link{NormalFormGame}} object.
    to_normal_form = function() {
      n <- self$n_players
      n_types <- self$n_types
      n_actions <- self$n_strategies

      # Enumerate composite strategies for each player:
      # a composite strategy maps each type to an action
      composite_strats <- vector("list", n)
      composite_labels <- vector("list", n)
      for (p in seq_len(n)) {
        nt <- n_types[p]
        na <- n_actions[p]
        # Total composite strategies = na^nt
        n_composite <- as.integer(na^nt)
        strats <- matrix(0L, nrow = n_composite, ncol = nt)
        labels <- character(n_composite)
        for (s in seq_len(n_composite)) {
          idx <- s - 1L
          action_map <- integer(nt)
          for (t in seq_len(nt)) {
            action_map[t] <- (idx %% na) + 1L
            idx <- idx %/% na
          }
          strats[s, ] <- action_map
          parts <- character(nt)
          for (t in seq_len(nt)) {
            parts[t] <- paste0(self$type_sets[[p]][t], "->",
                               self$strategies[[p]][action_map[t]])
          }
          labels[s] <- paste(parts, collapse = ",")
        }
        composite_strats[[p]] <- strats
        composite_labels[[p]] <- labels
      }

      # Enumerate all type profiles
      type_dims <- n_types
      n_type_profiles <- prod(type_dims)
      type_profiles <- matrix(0L, nrow = n_type_profiles, ncol = n)
      for (tp in seq_len(n_type_profiles)) {
        idx <- tp - 1L
        for (p in seq_len(n)) {
          type_profiles[tp, p] <- (idx %% type_dims[p]) + 1L
          idx <- idx %/% type_dims[p]
        }
      }

      # Compute prior probability for each type profile
      type_probs <- numeric(n_type_profiles)
      is_independent <- is.list(self$priors) && !is.array(self$priors)
      for (tp in seq_len(n_type_profiles)) {
        t_prof <- type_profiles[tp, ]
        if (is_independent) {
          prob <- 1
          for (p in seq_len(n)) {
            prob <- prob * self$priors[[p]][t_prof[p]]
          }
          type_probs[tp] <- prob
        } else {
          type_probs[tp] <- self$priors[matrix(t_prof, nrow = 1)]
        }
      }

      # Build payoff array
      n_comp <- vapply(composite_labels, length, integer(1))
      payoff_dims <- c(n_comp, n)
      payoff_array <- array(0, dim = payoff_dims)

      # Iterate over all composite strategy profiles
      n_comp_profiles <- prod(n_comp)
      for (cp in seq_len(n_comp_profiles)) {
        idx <- cp - 1L
        comp_profile <- integer(n)
        for (p in seq_len(n)) {
          comp_profile[p] <- (idx %% n_comp[p]) + 1L
          idx <- idx %/% n_comp[p]
        }

        # Expected payoff over type profiles
        expected_payoffs <- numeric(n)
        for (tp in seq_len(n_type_profiles)) {
          t_prof <- type_profiles[tp, ]
          prob <- type_probs[tp]
          if (prob < 1e-15) next

          # Determine action profile from composite strategies
          action_profile <- integer(n)
          for (p in seq_len(n)) {
            action_profile[p] <- composite_strats[[p]][comp_profile[p], t_prof[p]]
          }

          for (p in seq_len(n)) {
            expected_payoffs[p] <- expected_payoffs[p] +
              prob * self$payoff_fn(p, t_prof, action_profile)
          }
        }

        # Store in payoff array
        idx_list <- as.list(comp_profile)
        for (p in seq_len(n)) {
          idx_full <- c(idx_list, p)
          payoff_array[matrix(unlist(idx_full), nrow = 1)] <- expected_payoffs[p]
        }
      }

      NormalFormGame$new(
        players = self$players,
        strategies = composite_labels,
        payoffs = payoff_array
      )
    },

    #' @description Print a summary of the Bayesian game.
    print = function(...) {
      cat("Bayesian Game\n")
      cat("Players:", paste(self$players, collapse = ", "), "\n")
      for (i in seq_along(self$players)) {
        cat("  ", self$players[i], ": types = {",
            paste(self$type_sets[[i]], collapse = ", "), "}, actions = {",
            paste(self$strategies[[i]], collapse = ", "), "}\n", sep = "")
      }
      is_independent <- is.list(self$priors) && !is.array(self$priors)
      if (is_independent) {
        cat("Priors: independent\n")
      } else {
        cat("Priors: joint distribution\n")
      }
      invisible(self)
    }
  ),

  active = list(
    #' @field n_players Number of players.
    n_players = function() length(self$players),
    #' @field n_types Integer vector of type counts per player.
    n_types = function() vapply(self$type_sets, length, integer(1)),
    #' @field n_strategies Integer vector of action counts per player.
    n_strategies = function() vapply(self$strategies, length, integer(1))
  )
)


#' Create a Bayesian Game
#'
#' Constructor function for \code{\link{BayesianGame}}.
#'
#' @param players Character vector of player names.
#' @param type_sets List of character vectors of types per player.
#' @param priors Prior distribution. Either a list of numeric vectors
#'   (independent priors) or an array (joint prior).
#' @param strategies List of character vectors of actions per player.
#' @param payoff_fn Function with signature
#'   \code{function(player, type_profile, action_profile)} -> numeric.
#' @return A \code{\link{BayesianGame}} object.
#' @export
bayesian_game <- function(players, type_sets, priors, strategies, payoff_fn) {
  BayesianGame$new(players, type_sets, priors, strategies, payoff_fn)
}
