#' Bayesian Nash Equilibrium
#'
#' Finds Bayesian Nash equilibria of a Bayesian game by converting to
#' normal form and solving the resulting game, then mapping composite
#' strategy probabilities back to behavioral strategies (type-contingent
#' action probabilities).
#'
#' @param game A \code{\link{BayesianGame}} object.
#' @param method Character string specifying the method for the normal form
#'   solver. Default \code{"auto"}.
#' @return A list of equilibria. Each equilibrium is a list with:
#'   \describe{
#'     \item{strategies}{A list of lists, one per player. Each inner list
#'       maps type names to numeric vectors of action probabilities.}
#'     \item{payoffs}{Numeric vector of expected payoffs, one per player.}
#'   }
#' @export
bayesian_nash <- function(game, method = "auto") {
  stopifnot(inherits(game, "BayesianGame"))

  nf <- game$to_normal_form()
  nf_eqs <- nash_equilibria(nf, method = method)

  n <- game$n_players
  n_types <- game$n_types
  n_actions <- game$n_strategies

  # Map each normal-form equilibrium back to behavioral strategies
  lapply(nf_eqs, function(eq) {
    behavioral <- vector("list", n)
    for (p in seq_len(n)) {
      nt <- n_types[p]
      na <- n_actions[p]
      n_composite <- as.integer(na^nt)
      mixed <- eq$strategies[[p]]

      # For each type, accumulate action probabilities
      type_action_probs <- vector("list", nt)
      for (t in seq_len(nt)) {
        type_action_probs[[t]] <- numeric(na)
      }

      for (s in seq_len(n_composite)) {
        prob <- mixed[s]
        if (prob < 1e-15) next
        idx <- s - 1L
        for (t in seq_len(nt)) {
          action <- (idx %% na) + 1L
          idx <- idx %/% na
          type_action_probs[[t]][action] <- type_action_probs[[t]][action] + prob
        }
      }

      # Normalize (each type's action probs should sum to 1 if total > 0)
      for (t in seq_len(nt)) {
        s <- sum(type_action_probs[[t]])
        if (s > 1e-15) {
          type_action_probs[[t]] <- type_action_probs[[t]] / s
        }
      }

      names(type_action_probs) <- game$type_sets[[p]]
      behavioral[[p]] <- type_action_probs
    }
    names(behavioral) <- game$players

    list(
      strategies = behavioral,
      payoffs = eq$payoffs
    )
  })
}
