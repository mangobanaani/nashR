\name{cournot}
\alias{cournot}
\title{Cournot Oligopoly Game}
\description{
Create an N-player Cournot oligopoly game.
}
\usage{
cournot(n, quantities, demand, costs)
}
\arguments{
\item{n}{Integer, number of firms.}
\item{quantities}{Numeric vector of possible quantities.}
\item{demand}{Function mapping total quantity to price.}
\item{costs}{Marginal costs (vector or scalar).}
}
\value{
A NormalFormGame object.
}
