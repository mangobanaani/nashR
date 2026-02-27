#' Cournot Oligopoly Game
#'
#' Create an N-player Cournot oligopoly game where firms simultaneously choose
#' production quantities. Market price is determined by a demand function of
#' total output, and each firm's profit is quantity times the price-cost margin.
#'
#' @param n Integer, number of firms.
#' @param quantities Numeric vector of possible quantity choices (discrete).
#' @param demand Function mapping total quantity Q to market price.
#' @param costs Numeric vector of marginal costs (one per firm), or a single
#'   value for symmetric firms.
#' @return A \code{\link{NormalFormGame}} object.
#' @export
cournot <- function(n, quantities, demand, costs) {
  if (length(costs) == 1L) {
    costs <- rep(costs, n)
  }
  if (length(costs) != n) {
    stop("costs must have length 1 or n (", n, "), got ", length(costs))
  }

  n_q <- length(quantities)
  dims <- rep(n_q, n)

  # All strategy profiles as a grid of indices
  profiles <- as.matrix(expand.grid(lapply(dims, seq_len)))

  # Build the payoff array: dims x n
  payoff_arr <- array(NA_real_, dim = c(dims, n))

  for (row in seq_len(nrow(profiles))) {
    idx <- as.integer(profiles[row, ])
    q_chosen <- quantities[idx]
    total_q <- sum(q_chosen)
    price <- demand(total_q)
    profits <- q_chosen * (price - costs)

    # Assign into the array
    arr_idx <- c(as.list(idx), list(seq_len(n)))
    payoff_arr <- do.call(`[<-`, c(list(payoff_arr), arr_idx, list(profits)))
  }

  players <- paste0("Firm", seq_len(n))
  strat_labels <- as.character(quantities)
  strategies <- rep(list(strat_labels), n)

  NormalFormGame$new(
    players = players,
    strategies = strategies,
    payoffs = payoff_arr
  )
}


#' Bertrand Competition Game
#'
#' Create an N-player Bertrand competition game where firms simultaneously
#' choose prices. The firm with the lowest price captures the entire market
#' demand; ties split demand equally among lowest-price firms.
#'
#' @param n Integer, number of firms.
#' @param prices Numeric vector of possible price choices (discrete).
#' @param demand Function mapping price to market demand at that price.
#' @param costs Numeric vector of marginal costs (one per firm), or a single
#'   value for symmetric firms.
#' @return A \code{\link{NormalFormGame}} object.
#' @export
bertrand <- function(n, prices, demand, costs) {
  if (length(costs) == 1L) {
    costs <- rep(costs, n)
  }
  if (length(costs) != n) {
    stop("costs must have length 1 or n (", n, "), got ", length(costs))
  }

  n_p <- length(prices)
  dims <- rep(n_p, n)

  profiles <- as.matrix(expand.grid(lapply(dims, seq_len)))
  payoff_arr <- array(NA_real_, dim = c(dims, n))

  for (row in seq_len(nrow(profiles))) {
    idx <- as.integer(profiles[row, ])
    p_chosen <- prices[idx]
    min_price <- min(p_chosen)
    profits <- numeric(n)

    # Firms at the minimum price split demand; others get zero
    at_min <- which(p_chosen == min_price)
    n_at_min <- length(at_min)
    total_demand <- demand(min_price)

    for (f in at_min) {
      profits[f] <- (p_chosen[f] - costs[f]) * total_demand / n_at_min
    }
    # Firms above minimum price get zero profit (already initialized to 0)

    arr_idx <- c(as.list(idx), list(seq_len(n)))
    payoff_arr <- do.call(`[<-`, c(list(payoff_arr), arr_idx, list(profits)))
  }

  players <- paste0("Firm", seq_len(n))
  strat_labels <- as.character(prices)
  strategies <- rep(list(strat_labels), n)

  NormalFormGame$new(
    players = players,
    strategies = strategies,
    payoffs = payoff_arr
  )
}


#' Public Goods Game
#'
#' Create an N-player public goods game. Each player has an endowment and
#' chooses a contribution level. All contributions are pooled, multiplied by
#' a factor, and divided equally among all players.
#'
#' @param n Integer, number of players.
#' @param endowment Numeric, each player's endowment.
#' @param multiplier Numeric, the public goods multiplier (typically > 1 and < n).
#' @param contribution_levels Numeric vector of possible contribution amounts.
#' @return A \code{\link{NormalFormGame}} object.
#' @export
public_goods_game <- function(n, endowment, multiplier, contribution_levels) {
  n_c <- length(contribution_levels)
  dims <- rep(n_c, n)

  profiles <- as.matrix(expand.grid(lapply(dims, seq_len)))
  payoff_arr <- array(NA_real_, dim = c(dims, n))

  for (row in seq_len(nrow(profiles))) {
    idx <- as.integer(profiles[row, ])
    contribs <- contribution_levels[idx]
    total_contrib <- sum(contribs)
    public_share <- multiplier * total_contrib / n
    payoffs <- endowment - contribs + public_share

    arr_idx <- c(as.list(idx), list(seq_len(n)))
    payoff_arr <- do.call(`[<-`, c(list(payoff_arr), arr_idx, list(payoffs)))
  }

  players <- paste0("Player", seq_len(n))
  strat_labels <- as.character(contribution_levels)
  strategies <- rep(list(strat_labels), n)

  NormalFormGame$new(
    players = players,
    strategies = strategies,
    payoffs = payoff_arr
  )
}


#' Stag Hunt Game
#'
#' Create a classic 2-player Stag Hunt game. Both players choose between hunting
#' Stag (cooperative, high-reward but risky) or Hare (safe, low-reward).
#' The game has two pure Nash equilibria: (Stag, Stag) and (Hare, Hare).
#'
#' Standard payoffs:
#' \itemize{
#'   \item (Stag, Stag): (4, 4) -- both cooperate
#'   \item (Stag, Hare): (0, 3) -- stag hunter fails alone
#'   \item (Hare, Stag): (3, 0) -- hare hunter safe
#'   \item (Hare, Hare): (3, 3) -- both play it safe
#' }
#'
#' @return A \code{\link{NormalFormGame}} object.
#' @export
stag_hunt <- function() {
  normal_form(
    players = c("Player 1", "Player 2"),
    strategies = list(c("Stag", "Hare"), c("Stag", "Hare")),
    payoffs = list(
      matrix(c(4, 0, 3, 3), nrow = 2, byrow = TRUE),  # Player 1
      matrix(c(4, 3, 0, 3), nrow = 2, byrow = TRUE)    # Player 2
    )
  )
}
