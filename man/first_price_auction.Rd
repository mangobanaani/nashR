\name{first_price_auction}
\alias{first_price_auction}
\title{First-Price Sealed-Bid Auction}
\description{
Create a discrete N-bidder first-price sealed-bid auction.
}
\usage{
first_price_auction(n_bidders, values, bid_levels)
}
\arguments{
\item{n_bidders}{Integer, number of bidders.}
\item{values}{Numeric vector of private values.}
\item{bid_levels}{Numeric vector of possible bid levels.}
}
\value{
A NormalFormGame object.
}
