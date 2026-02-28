\name{best_response_dynamics}
\alias{best_response_dynamics}
\title{Best Response Dynamics}
\description{
Simulate continuous best response dynamics for a symmetric 2-player game.
}
\usage{
best_response_dynamics(game, x0, n_rounds, alpha = 0.5)
}
\arguments{
\item{game}{A symmetric 2-player NormalFormGame.}
\item{x0}{Initial population state (sums to 1).}
\item{n_rounds}{Number of update steps.}
\item{alpha}{Adjustment speed in (0, 1].}
}
\value{
A list with trajectory matrix.
}
