\name{fictitious_play}
\alias{fictitious_play}
\title{Fictitious Play}
\description{
Simulate fictitious play for a 2-player game.
}
\usage{
fictitious_play(game, n_rounds)
}
\arguments{
\item{game}{A 2-player NormalFormGame.}
\item{n_rounds}{Integer, number of rounds.}
}
\value{
A list with actions, beliefs, and belief_history.
}
