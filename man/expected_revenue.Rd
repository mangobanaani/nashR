\name{expected_revenue}
\alias{expected_revenue}
\title{Expected Revenue of Standard Auctions}
\description{
Compute expected revenue using analytical formulas.
}
\usage{
expected_revenue(auction_type, n_bidders, distribution = "uniform", lower = 0, upper = 1)
}
\arguments{
\item{auction_type}{Character: 'first_price', 'second_price', or 'all_pay'.}
\item{n_bidders}{Integer, number of bidders.}
\item{distribution}{Character, value distribution.}
\item{lower}{Lower bound of uniform distribution.}
\item{upper}{Upper bound of uniform distribution.}
}
\value{
Numeric, the expected revenue.
}
