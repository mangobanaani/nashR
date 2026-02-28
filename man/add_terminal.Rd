\name{add_terminal}
\alias{add_terminal}
\title{Add a Terminal Node}
\description{
Adds a terminal (leaf) node with payoffs to the game tree.
}
\usage{
add_terminal(game, node_id, payoffs)
}
\arguments{
\item{game}{An ExtensiveFormGame object.}
\item{node_id}{Character, the path-based id for this terminal.}
\item{payoffs}{Numeric vector of payoffs.}
}
\value{
The modified game object.
}
