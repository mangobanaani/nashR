\name{bayesian_nash}
\alias{bayesian_nash}
\title{Bayesian Nash Equilibrium}
\description{
Finds Bayesian Nash equilibria of a Bayesian game by converting to
normal form and solving, then mapping composite strategy probabilities
back to behavioral strategies (type-contingent action probabilities).
}
\usage{
bayesian_nash(game, method = "auto")
}
\arguments{
\item{game}{A \code{\link{BayesianGame}} object.}
\item{method}{Character string specifying the method for the normal form
  solver. Default \code{"auto"}.}
}
\value{
A list of equilibria. Each equilibrium is a list with:
\describe{
  \item{strategies}{A list of lists, one per player. Each inner list
    maps type names to numeric vectors of action probabilities.}
  \item{payoffs}{Numeric vector of expected payoffs, one per player.}
}
}
\seealso{
\code{\link{BayesianGame}}, \code{\link{bayesian_game}}, \code{\link{nash_equilibria}}
}
