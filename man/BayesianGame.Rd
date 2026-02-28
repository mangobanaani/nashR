\name{BayesianGame}
\alias{BayesianGame}
\title{Bayesian Game R6 Class}
\description{
R6 class representing a Bayesian (incomplete information) game. Each player
has a set of possible types, a prior distribution over type profiles, and
a payoff function that depends on the type profile and action profile.
}
\section{Active Bindings}{
\describe{
  \item{n_players}{Number of players.}
  \item{n_types}{Integer vector of type counts per player.}
  \item{n_strategies}{Integer vector of action counts per player.}
}
}
\section{Methods}{
\describe{
  \item{\code{initialize(players, type_sets, priors, strategies, payoff_fn)}}{Create a new Bayesian game.}
  \item{\code{payoff(player, type_profile, action_profile)}}{Get payoff for a player.}
  \item{\code{to_normal_form()}}{Convert to a NormalFormGame.}
  \item{\code{print()}}{Print a summary.}
}
}
\seealso{
\code{\link{bayesian_game}}, \code{\link{bayesian_nash}}
}
