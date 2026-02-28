\name{lemke_howson}
\alias{lemke_howson}
\title{Lemke-Howson Algorithm}
\description{
Find one Nash equilibrium using the Lemke-Howson complementary pivoting algorithm.
}
\usage{
lemke_howson(game, init_label = 1L)
}
\arguments{
\item{game}{A 2-player NormalFormGame.}
\item{init_label}{Starting label for pivoting (1 to m+n).}
}
\value{
A list with strategies and payoffs.
}
