\name{homotopy_nash}
\alias{homotopy_nash}
\title{Homotopy Continuation for Nash Equilibrium}
\description{
Finds a mixed strategy Nash equilibrium of an N-player normal form game
using a logit-response homotopy method implemented in C.
}
\usage{
homotopy_nash(game, start = NULL)
}
\arguments{
\item{game}{A \code{\link{NormalFormGame}} object.}
\item{start}{An optional starting profile: a list of numeric vectors
  (one per player) specifying prior beliefs. Default is uniform.}
}
\value{
A list with:
\describe{
  \item{strategies}{A list of numeric vectors, one per player.}
  \item{payoffs}{A numeric vector of expected payoffs.}
  \item{converged}{Logical indicating convergence.}
}
}
\seealso{
\code{\link{nash_equilibria}}, \code{\link{NormalFormGame}}
}
