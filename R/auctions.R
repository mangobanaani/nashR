#' First-Price Sealed-Bid Auction
#'
#' Create a discrete N-bidder first-price sealed-bid auction as a
#' \code{\link{NormalFormGame}}. Bidders simultaneously submit bids from a
#' discrete set. The highest bidder wins and pays their own bid. Ties are split
#' equally among the highest bidders.
#'
#' @param n_bidders Integer, number of bidders.
#' @param values Numeric vector of private values, one per bidder.
#' @param bid_levels Numeric vector of possible bid levels (same for all bidders).
#' @return A \code{\link{NormalFormGame}} object.
#' @export
first_price_auction <- function(n_bidders, values, bid_levels) {
  if (length(values) != n_bidders) {
    stop("values must have length equal to n_bidders (", n_bidders, ")")
  }

  n_bids <- length(bid_levels)
  dims <- rep(n_bids, n_bidders)

  profiles <- as.matrix(expand.grid(lapply(dims, seq_len)))
  payoff_arr <- array(NA_real_, dim = c(dims, n_bidders))

  for (row in seq_len(nrow(profiles))) {
    idx <- as.integer(profiles[row, ])
    bids <- bid_levels[idx]
    max_bid <- max(bids)
    winners <- which(bids == max_bid)
    n_winners <- length(winners)

    payoffs <- numeric(n_bidders)
    for (w in winners) {
      payoffs[w] <- (values[w] - bids[w]) / n_winners
    }
    # Losers get 0 (already initialized)

    arr_idx <- c(as.list(idx), list(seq_len(n_bidders)))
    payoff_arr <- do.call(`[<-`, c(list(payoff_arr), arr_idx, list(payoffs)))
  }

  players <- paste0("Bidder", seq_len(n_bidders))
  strat_labels <- as.character(bid_levels)
  strategies <- rep(list(strat_labels), n_bidders)

  NormalFormGame$new(
    players = players,
    strategies = strategies,
    payoffs = payoff_arr
  )
}


#' Second-Price Sealed-Bid (Vickrey) Auction
#'
#' Create a discrete N-bidder second-price sealed-bid auction as a
#' \code{\link{NormalFormGame}}. Bidders simultaneously submit bids from a
#' discrete set. The highest bidder wins and pays the second-highest bid.
#' Ties are split equally among the highest bidders.
#'
#' @param n_bidders Integer, number of bidders.
#' @param values Numeric vector of private values, one per bidder.
#' @param bid_levels Numeric vector of possible bid levels (same for all bidders).
#' @return A \code{\link{NormalFormGame}} object.
#' @export
second_price_auction <- function(n_bidders, values, bid_levels) {
  if (length(values) != n_bidders) {
    stop("values must have length equal to n_bidders (", n_bidders, ")")
  }

  n_bids <- length(bid_levels)
  dims <- rep(n_bids, n_bidders)

  profiles <- as.matrix(expand.grid(lapply(dims, seq_len)))
  payoff_arr <- array(NA_real_, dim = c(dims, n_bidders))

  for (row in seq_len(nrow(profiles))) {
    idx <- as.integer(profiles[row, ])
    bids <- bid_levels[idx]
    max_bid <- max(bids)
    winners <- which(bids == max_bid)
    n_winners <- length(winners)

    # Second-highest bid: highest bid among non-winners, or max_bid if tie
    if (n_winners >= 2) {
      second_price <- max_bid
    } else {
      non_winner_bids <- bids[-winners]
      if (length(non_winner_bids) == 0) {
        second_price <- 0
      } else {
        second_price <- max(non_winner_bids)
      }
    }

    payoffs <- numeric(n_bidders)
    for (w in winners) {
      payoffs[w] <- (values[w] - second_price) / n_winners
    }
    # Losers get 0 (already initialized)

    arr_idx <- c(as.list(idx), list(seq_len(n_bidders)))
    payoff_arr <- do.call(`[<-`, c(list(payoff_arr), arr_idx, list(payoffs)))
  }

  players <- paste0("Bidder", seq_len(n_bidders))
  strat_labels <- as.character(bid_levels)
  strategies <- rep(list(strat_labels), n_bidders)

  NormalFormGame$new(
    players = players,
    strategies = strategies,
    payoffs = payoff_arr
  )
}


#' All-Pay Auction
#'
#' Create a discrete N-bidder all-pay auction as a
#' \code{\link{NormalFormGame}}. All bidders pay their bids regardless of the
#' outcome. The highest bidder wins the item. Ties are split equally.
#'
#' @param n_bidders Integer, number of bidders.
#' @param values Numeric vector of private values, one per bidder.
#' @param bid_levels Numeric vector of possible bid levels (same for all bidders).
#' @return A \code{\link{NormalFormGame}} object.
#' @export
all_pay_auction <- function(n_bidders, values, bid_levels) {
  if (length(values) != n_bidders) {
    stop("values must have length equal to n_bidders (", n_bidders, ")")
  }

  n_bids <- length(bid_levels)
  dims <- rep(n_bids, n_bidders)

  profiles <- as.matrix(expand.grid(lapply(dims, seq_len)))
  payoff_arr <- array(NA_real_, dim = c(dims, n_bidders))

  for (row in seq_len(nrow(profiles))) {
    idx <- as.integer(profiles[row, ])
    bids <- bid_levels[idx]
    max_bid <- max(bids)
    winners <- which(bids == max_bid)
    n_winners <- length(winners)

    # All bidders pay their bid
    payoffs <- -bids
    # Winners additionally receive their value (split if tie)
    for (w in winners) {
      payoffs[w] <- values[w] / n_winners - bids[w]
    }

    arr_idx <- c(as.list(idx), list(seq_len(n_bidders)))
    payoff_arr <- do.call(`[<-`, c(list(payoff_arr), arr_idx, list(payoffs)))
  }

  players <- paste0("Bidder", seq_len(n_bidders))
  strat_labels <- as.character(bid_levels)
  strategies <- rep(list(strat_labels), n_bidders)

  NormalFormGame$new(
    players = players,
    strategies = strategies,
    payoffs = payoff_arr
  )
}


#' Optimal Bid in First-Price Auction
#'
#' Compute the Bayesian Nash equilibrium bid for a first-price sealed-bid
#' auction with independent private values drawn from a common distribution.
#'
#' For uniform[0,1] values with \eqn{n} bidders, the optimal bid is
#' \eqn{(n-1)/n \times v}{(n-1)/n * v}. For uniform[a,b] values, the optimal
#' bid is \eqn{a/n + (n-1)/n \times v}{a/n + (n-1)/n * v}.
#'
#' @param value Numeric, the bidder's private value.
#' @param n_bidders Integer, number of bidders.
#' @param distribution Character, the value distribution. Currently only
#'   \code{"uniform"} is supported.
#' @param lower Numeric, lower bound of the uniform distribution (default 0).
#' @param upper Numeric, upper bound of the uniform distribution (default 1).
#' @return Numeric, the optimal bid.
#' @export
optimal_bid_first_price <- function(value, n_bidders, distribution = "uniform",
                                    lower = 0, upper = 1) {
  if (distribution != "uniform") {
    stop("Only 'uniform' distribution is currently supported")
  }

  n <- n_bidders
  # For uniform[a,b] values:
  # Optimal bid = a/n + (n-1)/n * value
  # This reduces to (n-1)/n * value when a = 0
  lower / n + (n - 1) / n * value
}


#' Expected Revenue of Standard Auctions
#'
#' Compute the expected revenue of standard auction formats with independent
#' private values drawn from a uniform distribution, using analytical formulas
#' based on order statistics.
#'
#' By the Revenue Equivalence Theorem, the expected revenue is the same for
#' first-price, second-price, and all-pay auctions when values are i.i.d. from
#' a regular distribution. For \eqn{n} bidders with values uniform on
#' \eqn{[0, u]}, the expected revenue is \eqn{(n-1)/(n+1) \times u}{(n-1)/(n+1) * u}.
#'
#' @param auction_type Character, one of \code{"first_price"},
#'   \code{"second_price"}, or \code{"all_pay"}.
#' @param n_bidders Integer, number of bidders.
#' @param distribution Character, the value distribution. Currently only
#'   \code{"uniform"} is supported.
#' @param lower Numeric, lower bound of the uniform distribution (default 0).
#' @param upper Numeric, upper bound of the uniform distribution (default 1).
#' @return Numeric, the expected revenue.
#' @export
expected_revenue <- function(auction_type, n_bidders, distribution = "uniform",
                             lower = 0, upper = 1) {
  if (distribution != "uniform") {
    stop("Only 'uniform' distribution is currently supported")
  }
  if (!auction_type %in% c("first_price", "second_price", "all_pay")) {
    stop("auction_type must be one of 'first_price', 'second_price', 'all_pay'")
  }

  n <- n_bidders

  # For uniform[0, upper] with n bidders:
  # Expected revenue = (n-1)/(n+1) * upper
  # (Revenue equivalence: same for first-price, second-price, and all-pay)
  #
  # For uniform[lower, upper], the formula generalizes. With the standard

  # independent private values model and lower = 0, this simplifies.
  # General formula for uniform[a, b]:
  # E[revenue] = a + (n-1)/(n+1) * (upper - lower)
  # because the expected second-order statistic from n draws of Uniform[a,b]
  # is a + (n-1)/(n+1) * (b-a)
  lower + (n - 1) / (n + 1) * (upper - lower)
}
