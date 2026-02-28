\name{second_price_auction}
\alias{second_price_auction}
\title{Second-Price Sealed-Bid Auction}
\description{
Create a discrete N-bidder second-price (Vickrey) auction.
}
\usage{
second_price_auction(n_bidders, values, bid_levels)
}
\arguments{
\item{n_bidders}{Integer, number of bidders.}
\item{values}{Numeric vector of private values.}
\item{bid_levels}{Numeric vector of possible bid levels.}
}
\value{
A NormalFormGame object.
}
