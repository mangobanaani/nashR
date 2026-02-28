\name{bayesian_game}
\alias{bayesian_game}
\title{Create a Bayesian Game}
\description{
Constructor function for \code{\link{BayesianGame}}.
}
\usage{
bayesian_game(players, type_sets, priors, strategies, payoff_fn)
}
\arguments{
\item{players}{Character vector of player names.}
\item{type_sets}{List of character vectors of types per player.}
\item{priors}{Prior distribution. Either a list of numeric vectors
  (independent priors) or an array (joint prior).}
\item{strategies}{List of character vectors of actions per player.}
\item{payoff_fn}{Function with signature
  \code{function(player, type_profile, action_profile)} returning a numeric scalar.}
}
\value{
A \code{\link{BayesianGame}} object.
}
\seealso{
\code{\link{BayesianGame}}, \code{\link{bayesian_nash}}
}
