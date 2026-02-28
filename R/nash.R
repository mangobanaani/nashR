#' Find Nash Equilibria
#'
#' Unified interface for finding Nash equilibria of a normal form game.
#' Dispatches to the appropriate algorithm based on the game structure and
#' the requested method.
#'
#' @param game A \code{\link{NormalFormGame}} object.
#' @param method Character string specifying the algorithm:
#'   \describe{
#'     \item{\code{"auto"}}{For 2-player games, uses support enumeration.
#'       For N-player games (N > 2), uses homotopy continuation.}
#'     \item{\code{"pure"}}{Pure strategy enumeration via \code{\link{pure_nash}},
#'       with results converted to mixed strategy format.}
#'     \item{\code{"support"}}{Support enumeration via
#'       \code{\link{support_enumeration}} (2-player games only).}
#'     \item{\code{"homotopy"}}{Homotopy continuation via
#'       \code{\link{homotopy_nash}} (any number of players). Returns a single
#'       equilibrium.}
#'   }
#' @return A list of equilibria. Each equilibrium is a list with:
#'   \describe{
#'     \item{strategies}{A list of numeric vectors, one per player, giving the
#'       mixed strategy (probability distribution over strategies).}
#'     \item{payoffs}{A numeric vector of payoffs, one per player.}
#'   }
#' @export
nash_equilibria <- function(game, method = "auto") {
  stopifnot(inherits(game, "NormalFormGame"))

  method <- match.arg(method, c("auto", "pure", "support", "homotopy"))

  if (method == "auto") {
    if (game$n_players == 2L) {
      method <- "support"
    } else {
      method <- "homotopy"
    }
  }

  if (method == "support") {
    if (game$n_players != 2L) {
      stop("Support enumeration requires a 2-player game")
    }
    return(support_enumeration(game))
  }

  if (method == "homotopy") {
    result <- homotopy_nash(game)
    return(list(result))
  }

  if (method == "pure") {
    pure_results <- pure_nash(game)
    # Convert pure NE results to the mixed strategy format
    equilibria <- lapply(pure_results, function(pe) {
      n <- length(pe$profile)
      ns <- game$n_strategies
      strategies <- vector("list", n)
      for (p in seq_len(n)) {
        prob <- numeric(ns[p])
        prob[pe$profile[p]] <- 1
        strategies[[p]] <- prob
      }
      list(
        strategies = strategies,
        payoffs = pe$payoffs
      )
    })
    return(equilibria)
  }
}


#' Print Nash Equilibria
#'
#' Pretty-print a list of Nash equilibria with player names, mixed strategies,
#' and expected payoffs.
#'
#' @param equilibria A list of equilibria as returned by
#'   \code{\link{nash_equilibria}}.
#' @param game A \code{\link{NormalFormGame}} object (used for player and
#'   strategy names).
#' @return Invisibly returns \code{equilibria}.
#' @export
print_equilibria <- function(equilibria, game) {
  stopifnot(inherits(game, "NormalFormGame"))

  n_eq <- length(equilibria)
  cat("Nash Equilibria:", n_eq, "found\n")

  if (n_eq == 0L) {
    return(invisible(equilibria))
  }

  n <- game$n_players

  for (i in seq_len(n_eq)) {
    eq <- equilibria[[i]]
    cat("\n--- Equilibrium", i, "---\n")

    for (p in seq_len(n)) {
      strategy_names <- game$strategies[[p]]
      probs <- eq$strategies[[p]]

      # Format as strategy_name=probability pairs
      parts <- character(length(probs))
      for (s in seq_along(probs)) {
        parts[s] <- paste0(strategy_names[s], "=", round(probs[s], 4))
      }
      cat("  ", game$players[p], ": ", paste(parts, collapse = ", "), "\n", sep = "")
    }

    cat("  Payoffs: (", paste(round(eq$payoffs, 4), collapse = ", "), ")\n", sep = "")
  }

  invisible(equilibria)
}
