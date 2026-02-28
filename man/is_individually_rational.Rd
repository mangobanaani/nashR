\name{is_individually_rational}
\alias{is_individually_rational}
\title{Check Individual Rationality}
\description{
Check that no agent gets negative utility from truthful participation.
}
\usage{
is_individually_rational(mechanism, n_agents, type_space, tol = 1e-08)
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
