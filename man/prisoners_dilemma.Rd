\name{prisoners_dilemma}
\alias{prisoners_dilemma}
\title{Prisoner's Dilemma}
\description{
Create a Prisoner's Dilemma game.
}
\usage{
prisoners_dilemma(R = 3, T = 5, S = 0, P = 1)
}
\arguments{
\item{R}{Reward for mutual cooperation.}
\item{T}{Temptation to defect.}
\item{S}{Sucker's payoff.}
\item{P}{Punishment for mutual defection.}
}
\value{
A NormalFormGame object.
}
