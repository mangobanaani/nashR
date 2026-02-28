\name{add_node}
\alias{add_node}
\title{Add a Decision Node}
\description{
Adds a decision node to the game tree.
}
\usage{
add_node(game, parent_id, player, actions, info_set = NULL)
}
\arguments{
\item{game}{An ExtensiveFormGame object.}
\item{parent_id}{Character, the node id to attach to.}
\item{player}{Integer, which player decides.}
\item{actions}{Character vector of available actions.}
\item{info_set}{Optional character, information set label.}
}
\value{
The modified game object.
}
