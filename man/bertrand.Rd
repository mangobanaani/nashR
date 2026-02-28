\name{bertrand}
\alias{bertrand}
\title{Bertrand Competition Game}
\description{
Create an N-player Bertrand competition game.
}
\usage{
bertrand(n, prices, demand, costs)
}
\arguments{
\item{n}{Integer, number of firms.}
\item{prices}{Numeric vector of possible prices.}
\item{demand}{Function mapping price to demand.}
\item{costs}{Marginal costs (vector or scalar).}
}
\value{
A NormalFormGame object.
}
