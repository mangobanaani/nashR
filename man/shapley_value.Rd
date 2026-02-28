\name{shapley_value}
\alias{shapley_value}
\title{Shapley Value}
\description{
Compute the Shapley value for a cooperative game.
}
\usage{
shapley_value(game, n_samples = NULL)
}
\arguments{
\item{game}{A CooperativeGame object.}
\item{n_samples}{Number of Monte Carlo samples, or NULL for exact.}
}
\value{
Named numeric vector of Shapley values.
}
