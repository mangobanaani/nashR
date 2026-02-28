\name{nash_equilibria}
\alias{nash_equilibria}
\title{Find Nash Equilibria}
\description{
Unified interface for finding Nash equilibria.
}
\usage{
nash_equilibria(game, method = "auto")
}
\arguments{
\item{game}{A NormalFormGame object.}
\item{method}{Algorithm: 'auto', 'pure', or 'support'.}
}
\value{
A list of equilibria.
}
