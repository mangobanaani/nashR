\name{vcg_mechanism}
\alias{vcg_mechanism}
\title{VCG Mechanism}
\description{
Compute VCG mechanism allocation and payments.
}
\usage{
vcg_mechanism(valuations, allocation_rule)
}
\arguments{
\item{valuations}{Named list of agent valuations.}
\item{allocation_rule}{Function computing the efficient allocation.}
}
\value{
List with allocation, payments, and utilities.
}
