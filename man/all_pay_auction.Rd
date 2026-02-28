\name{all_pay_auction}
\alias{all_pay_auction}
\title{All-Pay Auction}
\description{
Create a discrete N-bidder all-pay auction.
}
\usage{
all_pay_auction(n_bidders, values, bid_levels)
}
\arguments{
\item{n_bidders}{Integer, number of bidders.}
\item{values}{Numeric vector of private values.}
\item{bid_levels}{Numeric vector of possible bid levels.}
}
\value{
A NormalFormGame object.
}
