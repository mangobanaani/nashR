\name{in_core}
\alias{in_core}
\title{Check Core Membership}
\description{
Check if an allocation is in the core of a cooperative game.
}
\usage{
in_core(game, allocation, tol = 1e-08)
}
\arguments{
\item{game}{A CooperativeGame object.}
\item{allocation}{Named numeric vector of payoffs.}
\item{tol}{Numerical tolerance.}
}
\value{
Logical scalar.
}
