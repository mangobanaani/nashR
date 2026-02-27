#' Create a Normal Form Game
#'
#' Convenience constructor for creating a \code{\link{NormalFormGame}} from
#' payoff matrices (one per player for 2-player games) or a pre-built payoff
#' array.
#'
#' @param players Character vector of player names.
#' @param strategies List of character vectors, one per player.
#' @param payoffs Either a list of matrices (one per player, for 2-player
#'   games) or a numeric array with dimensions matching the number of
#'   strategies per player plus a final dimension equal to the number of
#'   players.
#' @return A \code{\link{NormalFormGame}} object.
#' @export
normal_form <- function(players, strategies, payoffs) {
  n <- length(players)

  if (is.list(payoffs) && !is.array(payoffs)) {
    # List of matrices — convert to the array format NormalFormGame expects
    if (length(payoffs) != n) {
      stop("Length of payoffs list (", length(payoffs),
           ") must equal the number of players (", n, ")")
    }

    dims <- vapply(strategies, length, integer(1))
    arr <- array(dim = c(dims, n))

    for (p in seq_len(n)) {
      mat <- payoffs[[p]]
      if (!is.matrix(mat)) {
        stop("payoffs[[", p, "]] must be a matrix")
      }
      if (!identical(dim(mat), as.integer(dims[1:2]))) {
        stop("payoffs[[", p, "]] dimensions (", paste(dim(mat), collapse = "x"),
             ") do not match strategy counts (", paste(dims[1:2], collapse = "x"), ")")
      }
      # Fill the array: for each strategy profile, set the p-th player payoff
      for (i in seq_len(dims[1])) {
        for (j in seq_len(dims[2])) {
          arr[i, j, p] <- mat[i, j]
        }
      }
    }

    payoffs <- arr
  }

  NormalFormGame$new(players = players, strategies = strategies, payoffs = payoffs)
}


#' Prisoner's Dilemma
#'
#' Create a Prisoner's Dilemma game with standard or custom payoffs.
#' Strategies are Cooperate (C) and Defect (D). The payoff parameters must
#' satisfy T > R > P > S.
#'
#' @param R Reward for mutual cooperation (default 3).
#' @param T Temptation to defect (default 5).
#' @param S Sucker's payoff (default 0).
#' @param P Punishment for mutual defection (default 1).
#' @return A \code{\link{NormalFormGame}} object.
#' @export
prisoners_dilemma <- function(R = 3, T = 5, S = 0, P = 1) {
  normal_form(
    players = c("Row", "Col"),
    strategies = list(c("C", "D"), c("C", "D")),
    payoffs = list(
      matrix(c(R, S, T, P), nrow = 2, byrow = TRUE),   # Row's payoffs
      matrix(c(R, T, S, P), nrow = 2, byrow = TRUE)     # Col's payoffs
    )
  )
}


#' Battle of the Sexes
#'
#' Create a Battle of the Sexes game. Player 1 prefers outcome A and Player 2
#' prefers outcome B, but both prefer coordination over miscoordination.
#'
#' @return A \code{\link{NormalFormGame}} object.
#' @export
battle_of_sexes <- function() {
  normal_form(
    players = c("Player 1", "Player 2"),
    strategies = list(c("A", "B"), c("A", "B")),
    payoffs = list(
      matrix(c(3, 0, 0, 2), nrow = 2, byrow = TRUE),   # Player 1's payoffs
      matrix(c(2, 0, 0, 3), nrow = 2, byrow = TRUE)     # Player 2's payoffs
    )
  )
}


#' Matching Pennies
#'
#' Create a Matching Pennies game — a zero-sum game where Player 1 wins if
#' both choose the same side and Player 2 wins otherwise.
#'
#' @return A \code{\link{NormalFormGame}} object.
#' @export
matching_pennies <- function() {
  normal_form(
    players = c("Player 1", "Player 2"),
    strategies = list(c("H", "T"), c("H", "T")),
    payoffs = list(
      matrix(c(1, -1, -1, 1), nrow = 2, byrow = TRUE),   # Player 1's payoffs
      matrix(c(-1, 1, 1, -1), nrow = 2, byrow = TRUE)     # Player 2's payoffs
    )
  )
}


#' Coordination Game
#'
#' Create a pure coordination game with two Nash equilibria on the diagonal.
#' Payoffs are (2,2) for coordinating on A, (1,1) for coordinating on B,
#' and (0,0) for miscoordination.
#'
#' @return A \code{\link{NormalFormGame}} object.
#' @export
coordination_game <- function() {
  normal_form(
    players = c("Player 1", "Player 2"),
    strategies = list(c("A", "B"), c("A", "B")),
    payoffs = list(
      matrix(c(2, 0, 0, 1), nrow = 2, byrow = TRUE),   # Player 1's payoffs
      matrix(c(2, 0, 0, 1), nrow = 2, byrow = TRUE)     # Player 2's payoffs
    )
  )
}


#' Hawk-Dove Game
#'
#' Create a Hawk-Dove (Chicken) game with parameterized resource value V and
#' cost of fighting C. Standard payoffs:
#' \itemize{
#'   \item Hawk vs Hawk: ((V-C)/2, (V-C)/2)
#'   \item Hawk vs Dove: (V, 0)
#'   \item Dove vs Hawk: (0, V)
#'   \item Dove vs Dove: (V/2, V/2)
#' }
#'
#' @param V Value of the resource (default 4).
#' @param C Cost of fighting (default 6).
#' @return A \code{\link{NormalFormGame}} object.
#' @export
hawk_dove <- function(V = 4, C = 6) {
  normal_form(
    players = c("Player 1", "Player 2"),
    strategies = list(c("Hawk", "Dove"), c("Hawk", "Dove")),
    payoffs = list(
      matrix(c((V - C) / 2, V, 0, V / 2), nrow = 2, byrow = TRUE),   # Player 1
      matrix(c((V - C) / 2, 0, V, V / 2), nrow = 2, byrow = TRUE)    # Player 2
    )
  )
}
