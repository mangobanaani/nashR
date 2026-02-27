#' Cooperative (Transferable Utility) Game
#'
#' R6 class representing a cooperative game with transferable utility.
#' A cooperative game is defined by a set of players and a characteristic
#' (value) function that assigns a real number to each coalition (subset
#' of players).
#'
#' @section Fields:
#' \describe{
#'   \item{players}{Character vector of player names.}
#'   \item{value_fn}{Function mapping a character vector (coalition) to a
#'     numeric scalar.}
#' }
#'
#' @section Active Bindings:
#' \describe{
#'   \item{n_players}{Integer, number of players.}
#' }
#'
#' @section Methods:
#' \describe{
#'   \item{initialize(players, value)}{Create a new game. Validates that
#'     v(empty set) = 0.}
#'   \item{value(S)}{Return the value of coalition S (character vector).}
#'   \item{print()}{Print a summary of the game.}
#' }
#'
#' @export
CooperativeGame <- R6::R6Class(
  "CooperativeGame",

  public = list(

    #' @field players Character vector of player names.
    players = NULL,

    #' @field value_fn Characteristic function mapping coalitions to values.
    value_fn = NULL,

    #' @description
    #' Create a new CooperativeGame.
    #'
    #' @param players Character vector of player names.
    #' @param value Function taking a character vector (coalition) and
    #'   returning a numeric scalar.
    #' @return A new `CooperativeGame` object.
    initialize = function(players, value) {
      stopifnot(is.character(players), length(players) >= 1L)
      stopifnot(is.function(value))

      # Validate v(empty set) = 0
      v_empty <- value(character(0))
      if (!isTRUE(all.equal(v_empty, 0))) {
        stop("Value of the empty coalition must be 0, got ", v_empty)
      }

      self$players <- players
      self$value_fn <- value
    },

    #' @description
    #' Evaluate the characteristic function for a coalition.
    #'
    #' @param S Character vector of player names in the coalition.
    #' @return Numeric scalar, the value of coalition S.
    value = function(S) {
      self$value_fn(S)
    },

    #' @description
    #' Print a summary of the game.
    print = function() {
      cat("Cooperative Game\n")
      cat("Players:", paste(self$players, collapse = ", "), "\n")
      cat("Number of players:", self$n_players, "\n")

      # Show singleton and grand coalition values
      cat("\nSingleton values:\n")
      for (p in self$players) {
        cat("  v({", p, "}) = ", self$value(p), "\n", sep = "")
      }
      cat("v(", paste(self$players, collapse = ","), ") = ",
          self$value(self$players), "\n", sep = "")

      invisible(self)
    }
  ),

  active = list(

    #' @field n_players Number of players.
    n_players = function() {
      length(self$players)
    }
  )
)


#' Create a cooperative game
#'
#' Constructor function for \code{\link{CooperativeGame}}.
#'
#' @param players Character vector of player names.
#' @param value Function mapping a character vector (coalition) to a numeric
#'   scalar. Must satisfy v(empty set) = 0.
#' @return A \code{\link{CooperativeGame}} object.
#' @export
cooperative_game <- function(players, value) {
  CooperativeGame$new(players = players, value = value)
}


#' Check superadditivity of a cooperative game
#'
#' A cooperative game is superadditive if for all pairs of disjoint
#' coalitions S and T, v(S union T) >= v(S) + v(T).
#'
#' @param game A \code{\link{CooperativeGame}} object.
#' @return Logical, \code{TRUE} if the game is superadditive.
#' @export
is_superadditive <- function(game) {
  stopifnot(inherits(game, "CooperativeGame"))

  players <- game$players
  n <- length(players)

  # Generate all subsets as integer masks
  # For each pair of disjoint subsets, check superadditivity
  n_subsets <- 2L^n

  for (s_mask in 0:(n_subsets - 1L)) {
    # All subsets of the complement of S
    complement_mask <- bitwAnd(bitwNot(s_mask), n_subsets - 1L)

    # Iterate over subsets of the complement
    t_mask <- complement_mask
    while (t_mask > 0L) {
      # Only check each unordered pair once: require s_mask < t_mask
      if (s_mask < t_mask) {
        S <- players[which(bitwAnd(s_mask, 2L^(seq_len(n) - 1L)) > 0L)]
        T_coal <- players[which(bitwAnd(t_mask, 2L^(seq_len(n) - 1L)) > 0L)]
        union_mask <- bitwOr(s_mask, t_mask)
        ST <- players[which(bitwAnd(union_mask, 2L^(seq_len(n) - 1L)) > 0L)]

        if (game$value(ST) < game$value(S) + game$value(T_coal)) {
          return(FALSE)
        }
      }

      # Next subset of complement (Gosper's hack for subset enumeration)
      t_mask <- bitwAnd(t_mask - 1L, complement_mask)
    }
  }

  TRUE
}
