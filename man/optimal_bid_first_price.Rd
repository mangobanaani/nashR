\name{optimal_bid_first_price}
\alias{optimal_bid_first_price}
\title{Optimal Bid in First-Price Auction}
\description{
Compute the BNE bid for a first-price auction with i.i.d. uniform values.
}
\usage{
optimal_bid_first_price(value, n_bidders, distribution = "uniform", lower = 0, upper = 1)
}
\arguments{
\item{value}{Numeric, the bidder's private value.}
\item{n_bidders}{Integer, number of bidders.}
\item{distribution}{Character, value distribution (only 'uniform').}
\item{lower}{Lower bound of uniform distribution.}
\item{upper}{Upper bound of uniform distribution.}
}
\value{
Numeric, the optimal bid.
}
