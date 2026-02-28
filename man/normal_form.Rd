\name{normal_form}
\alias{normal_form}
\title{Create a Normal Form Game}
\description{
Convenience constructor for creating a NormalFormGame from payoff matrices or array.
}
\usage{
normal_form(players, strategies, payoffs)
}
\arguments{
\item{players}{Character vector of player names.}
\item{strategies}{List of character vectors, one per player.}
\item{payoffs}{List of matrices or a numeric array.}
}
\value{
A NormalFormGame object.
}
