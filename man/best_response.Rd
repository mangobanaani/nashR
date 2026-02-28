\name{best_response}
\alias{best_response}
\title{Compute Best Response}
\description{
Compute the best response strategies for a player given an opponent mixed strategy.
}
\usage{
best_response(game, player, opponent_strategy, tol = 1e-10)
}
\arguments{
\item{game}{A NormalFormGame object.}
\item{player}{Integer, the player (1 or 2).}
\item{opponent_strategy}{Numeric vector of opponent's mixed strategy.}
\item{tol}{Numeric tolerance.}
}
\value{
Integer vector of best response strategy indices.
}
