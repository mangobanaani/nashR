\name{replicator_dynamics}
\alias{replicator_dynamics}
\title{Replicator Dynamics}
\description{
Simulate replicator dynamics for a symmetric 2-player evolutionary game.
}
\usage{
replicator_dynamics(game, x0, t_max, dt = 0.01)
}
\arguments{
\item{game}{A symmetric 2-player NormalFormGame.}
\item{x0}{Initial population state (sums to 1).}
\item{t_max}{Time horizon.}
\item{dt}{Time step.}
}
\value{
A list with trajectory matrix and time vector.
}
