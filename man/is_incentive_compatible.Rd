\name{is_incentive_compatible}
\alias{is_incentive_compatible}
\title{Check Incentive Compatibility}
\description{
Check whether truthful reporting is a dominant strategy.
}
\usage{
is_incentive_compatible(mechanism, n_agents, type_space, tol = 1e-08)
}
\arguments{
\item{mechanism}{List with allocation and payment functions.}
\item{n_agents}{Number of agents.}
\item{type_space}{Numeric vector of possible types.}
\item{tol}{Numeric tolerance.}
}
\value{
Logical scalar.
}
