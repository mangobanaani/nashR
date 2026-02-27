#' Normal (Strategic) Form Game
#'
#' R6 class representing an N-player normal form game. A normal form game
#' is defined by a set of players, a strategy set for each player, and a
#' payoff function mapping strategy profiles to payoff vectors.
#'
#' @section Fields:
#' \describe{
#'   \item{players}{Character vector of player names.}
#'   \item{strategies}{List of character vectors, one per player, giving the
#'     strategy labels.}
#'   \item{payoff_array}{Numeric array of dimension
#'     \eqn{s_1 \times s_2 \times \ldots \times s_N \times N}{s1 x s2 x ... x sN x N}
#'     where \eqn{s_i}{si} is the number of strategies for player \eqn{i}.
#'     The last dimension indexes players.}
#' }
#'
#' @section Active Bindings:
#' \describe{
#'   \item{n_players}{Integer, number of players.}
#'   \item{n_strategies}{Integer vector giving the number of strategies per player.}
#' }
#'
#' @section Methods:
#' \describe{
#'   \item{initialize(players, strategies, payoffs)}{Create a new game.}
#'   \item{payoff(profile)}{Return the payoff vector for a strategy profile
#'     given as an integer vector of strategy indices.}
#'   \item{print()}{Print a summary of the game.}
#' }
#'
#' @export
NormalFormGame <- R6::R6Class(
  "NormalFormGame",

  public = list(

    #' @field players Character vector of player names.
    players = NULL,

    #' @field strategies List of character vectors of strategy labels.
    strategies = NULL,

    #' @field payoff_array Numeric array of payoffs.
    payoff_array = NULL,

    #' @description
    #' Create a new NormalFormGame.
    #'
    #' @param players Character vector of player names.
    #' @param strategies List of character vectors, one per player.
    #' @param payoffs Numeric array with dimensions matching the number of
    #'   strategies per player, plus a final dimension equal to the number of
    #'   players.
    #' @return A new `NormalFormGame` object.
    initialize = function(players, strategies, payoffs) {
      # Validate: number of players must match number of strategy sets
      if (length(players) != length(strategies)) {
        stop(
          "Number of players (", length(players),
          ") must match number of strategy sets in strategies (",
          length(strategies), ")"
        )
      }

      n <- length(players)
      expected_dim <- c(vapply(strategies, length, integer(1)), n)

      # Validate: payoff array dimensions
      if (!is.array(payoffs) || !identical(as.integer(dim(payoffs)), as.integer(expected_dim))) {
        stop(
          "Payoff array dimensions must be (",
          paste(expected_dim, collapse = " x "),
          "), got (",
          paste(dim(payoffs), collapse = " x "), ")"
        )
      }

      self$players <- players
      self$strategies <- strategies
      self$payoff_array <- payoffs
    },

    #' @description
    #' Get payoffs for a strategy profile.
    #'
    #' @param profile Integer vector of strategy indices, one per player.
    #' @return Numeric vector of payoffs, one per player.
    payoff = function(profile) {
      n <- length(self$players)
      # Build the index list: one index per strategy dimension, then all players
      idx <- c(as.list(profile), list(seq_len(n)))
      do.call(`[`, c(list(self$payoff_array), idx))
    },

    #' @description
    #' Print a summary of the game.
    print = function() {
      n <- length(self$players)
      cat("Normal Form Game\n")
      cat("Players:", paste(self$players, collapse = ", "), "\n")
      for (i in seq_len(n)) {
        cat(
          "  ", self$players[i], ": ",
          paste(self$strategies[[i]], collapse = ", "), "\n",
          sep = ""
        )
      }

      # For 2-player games, print a payoff matrix
      if (n == 2L) {
        cat("\nPayoff Matrix:\n")
        s1 <- self$strategies[[1]]
        s2 <- self$strategies[[2]]

        # Column header
        header <- formatC("", width = 12)
        for (j in seq_along(s2)) {
          header <- paste0(header, formatC(s2[j], width = 12))
        }
        cat(header, "\n")

        # Rows
        for (i in seq_along(s1)) {
          row_str <- formatC(s1[i], width = 12)
          for (j in seq_along(s2)) {
            p <- self$payoff(c(i, j))
            cell <- paste0("(", paste(p, collapse = ","), ")")
            row_str <- paste0(row_str, formatC(cell, width = 12))
          }
          cat(row_str, "\n")
        }
      }

      invisible(self)
    }
  ),

  active = list(

    #' @field n_players Number of players.
    n_players = function() {
      length(self$players)
    },

    #' @field n_strategies Integer vector of strategy counts per player.
    n_strategies = function() {
      vapply(self$strategies, length, integer(1))
    }
  )
)
