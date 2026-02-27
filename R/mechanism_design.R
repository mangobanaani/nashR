#' Compute VCG mechanism allocation and payments
#'
#' Implements the Vickrey-Clarke-Groves mechanism. Given agent valuations and
#' an efficient allocation rule, computes the socially efficient allocation
#' and the corresponding VCG payments (Clarke pivot payments).
#'
#' @param valuations Named list, one entry per agent. Each entry is a named
#'   numeric vector of values for each item/outcome.
#' @param allocation_rule Function that takes a list of valuations and returns
#'   an allocation (vector for single item, matrix for multi-item).
#' @return List with components:
#'   \describe{
#'     \item{allocation}{The efficient allocation (vector or matrix).}
#'     \item{payments}{Numeric vector of VCG payments per agent.}
#'     \item{utilities}{Numeric vector of utilities per agent (value - payment).}
#'   }
#' @export
vcg_mechanism <- function(valuations, allocation_rule) {
  n_agents <- length(valuations)

  # Step 1: Run allocation rule with all agents
  allocation <- allocation_rule(valuations)

  # Determine if single-item (vector) or multi-item (matrix)
  is_matrix <- is.matrix(allocation)

  # Helper: compute total value an agent gets from an allocation
  agent_value <- function(agent_idx, alloc, vals) {
    if (is.matrix(alloc)) {
      sum(vals[[agent_idx]] * alloc[agent_idx, ])
    } else {
      sum(vals[[agent_idx]] * alloc[agent_idx])
    }
  }

  # Step 2: Compute payments for each agent
  payments <- numeric(n_agents)

  for (i in seq_len(n_agents)) {
    # Value of others under allocation WITH agent i
    others_value_with_i <- 0
    for (j in seq_len(n_agents)) {
      if (j != i) {
        others_value_with_i <- others_value_with_i + agent_value(j, allocation, valuations)
      }
    }

    # Re-run allocation without agent i
    vals_without_i <- valuations[-i]
    alloc_without_i <- allocation_rule(vals_without_i)

    # Value of others under allocation WITHOUT agent i
    others_value_without_i <- 0
    for (k in seq_along(vals_without_i)) {
      if (is.matrix(alloc_without_i)) {
        others_value_without_i <- others_value_without_i +
          sum(vals_without_i[[k]] * alloc_without_i[k, ])
      } else {
        others_value_without_i <- others_value_without_i +
          sum(vals_without_i[[k]] * alloc_without_i[k])
      }
    }

    # VCG payment = externality imposed on others
    payments[i] <- others_value_without_i - others_value_with_i
  }

  # Step 3: Compute utilities
  utilities <- numeric(n_agents)
  for (i in seq_len(n_agents)) {
    utilities[i] <- agent_value(i, allocation, valuations) - payments[i]
  }

  list(
    allocation = allocation,
    payments = payments,
    utilities = utilities
  )
}

#' Check incentive compatibility of a mechanism
#'
#' Performs a brute-force check over a discrete type space to verify whether
#' truthful reporting is a dominant strategy for every agent.
#'
#' @param mechanism List with \code{allocation(reports)} and
#'   \code{payment(reports)} functions.
#' @param n_agents Number of agents.
#' @param type_space Numeric vector of possible types (values) for each agent.
#' @param tol Numeric tolerance for comparisons (default 1e-8).
#' @return TRUE if the mechanism is incentive compatible, FALSE otherwise.
#' @export
is_incentive_compatible <- function(mechanism, n_agents, type_space, tol = 1e-8) {
  alloc_fn <- mechanism$allocation
  pay_fn <- mechanism$payment

  # Generate all possible opponent type profiles
  # For n_agents - 1 opponents, each drawing from type_space
  opponent_profiles <- function(n_opponents) {
    if (n_opponents == 0) return(matrix(nrow = 1, ncol = 0))
    as.matrix(expand.grid(rep(list(type_space), n_opponents)))
  }

  for (i in seq_len(n_agents)) {
    n_opponents <- n_agents - 1
    opp_profs <- opponent_profiles(n_opponents)

    for (true_type in type_space) {
      for (reported_type in type_space) {
        # Check over all opponent profiles
        for (row in seq_len(nrow(opp_profs))) {
          opp <- opp_profs[row, ]

          # Build full report vectors: truthful and deviated
          truthful_reports <- numeric(n_agents)
          deviated_reports <- numeric(n_agents)

          # Place opponents
          opp_idx <- 1
          for (j in seq_len(n_agents)) {
            if (j == i) {
              truthful_reports[j] <- true_type
              deviated_reports[j] <- reported_type
            } else {
              truthful_reports[j] <- opp[opp_idx]
              deviated_reports[j] <- opp[opp_idx]
              opp_idx <- opp_idx + 1
            }
          }

          # Utility from truthful reporting
          alloc_truth <- alloc_fn(truthful_reports)
          pay_truth <- pay_fn(truthful_reports)
          utility_truth <- true_type * alloc_truth[i] - pay_truth[i]

          # Utility from deviating
          alloc_dev <- alloc_fn(deviated_reports)
          pay_dev <- pay_fn(deviated_reports)
          utility_dev <- true_type * alloc_dev[i] - pay_dev[i]

          # IC requires truthful >= deviated
          if (utility_dev > utility_truth + tol) {
            return(FALSE)
          }
        }
      }
    }
  }

  TRUE
}

#' Check individual rationality of a mechanism
#'
#' Verifies that no agent gets negative utility from participating truthfully,
#' for all possible type profiles.
#'
#' @param mechanism List with \code{allocation(reports)} and
#'   \code{payment(reports)} functions.
#' @param n_agents Number of agents.
#' @param type_space Numeric vector of possible types (values) for each agent.
#' @param tol Numeric tolerance for comparisons (default 1e-8).
#' @return TRUE if the mechanism is individually rational, FALSE otherwise.
#' @export
is_individually_rational <- function(mechanism, n_agents, type_space, tol = 1e-8) {
  alloc_fn <- mechanism$allocation
  pay_fn <- mechanism$payment

  # Generate all possible full type profiles
  all_profiles <- as.matrix(expand.grid(rep(list(type_space), n_agents)))

  for (row in seq_len(nrow(all_profiles))) {
    reports <- all_profiles[row, ]

    alloc <- alloc_fn(reports)
    pays <- pay_fn(reports)

    for (i in seq_len(n_agents)) {
      utility_i <- reports[i] * alloc[i] - pays[i]
      if (utility_i < -tol) {
        return(FALSE)
      }
    }
  }

  TRUE
}
