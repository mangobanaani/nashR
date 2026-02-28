\name{public_goods_game}
\alias{public_goods_game}
\title{Public Goods Game}
\description{
Create an N-player public goods game.
}
\usage{
public_goods_game(n, endowment, multiplier, contribution_levels)
}
\arguments{
\item{n}{Integer, number of players.}
\item{endowment}{Numeric, each player's endowment.}
\item{multiplier}{Numeric, the public goods multiplier.}
\item{contribution_levels}{Numeric vector of possible contributions.}
}
\value{
A NormalFormGame object.
}
