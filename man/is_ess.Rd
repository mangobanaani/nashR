\name{is_ess}
\alias{is_ess}
\title{Check Evolutionarily Stable Strategy}
\description{
Test whether a mixed strategy is an ESS.
}
\usage{
is_ess(game, strategy, tol = 1e-10)
}
\arguments{
\item{game}{A symmetric 2-player NormalFormGame.}
\item{strategy}{Numeric vector (mixed strategy, sums to 1).}
\item{tol}{Numeric tolerance.}
}
\value{
Logical scalar.
}
