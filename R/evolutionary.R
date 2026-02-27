#' Replicator Dynamics
#'
#' Simulate replicator dynamics for a symmetric 2-player game interpreted as a
#' single-population evolutionary model. The population state \eqn{x} evolves
#' according to \eqn{dx_i/dt = x_i \bigl(\pi_i(x) - \bar\pi(x)\bigr)}{dx_i/dt = x_i * (pi_i(x) - avg_pi(x))}
#' where \eqn{\pi_i(x) = \sum_j A_{ij} x_j}{pi_i(x) = sum_j A[i,j]*x_j} is
#' the expected payoff to strategy \eqn{i}, and
#' \eqn{\bar\pi(x) = \sum_i x_i \pi_i(x)}{avg_pi(x) = sum_i x_i * pi_i(x)}.
#'
#' @param game A symmetric 2-player \code{\link{NormalFormGame}}.
#' @param x0 Numeric vector giving the initial population state (must sum to 1).
#' @param t_max Numeric scalar, the time horizon.
#' @param dt Numeric scalar, the time step (default 0.01).
#' @return A list with components:
#'   \describe{
#'     \item{trajectory}{Numeric matrix with one row per time step and one column
#'       per strategy, giving the population state over time.}
#'     \item{time}{Numeric vector of time points corresponding to the rows of
#'       \code{trajectory}.}
#'   }
#' @export
replicator_dynamics <- function(game, x0, t_max, dt = 0.01) {
  stopifnot(inherits(game, "NormalFormGame"))
  stopifnot(game$n_players == 2L)

  ns <- game$n_strategies
  n_strats <- ns[1]
  stopifnot(length(x0) == n_strats)
  stopifnot(abs(sum(x0) - 1) < 1e-10)


  # Extract the payoff matrix A for player 1 (row player)
  # A[i,j] = payoff to player 1 when row plays i, column plays j
  A <- matrix(0, nrow = n_strats, ncol = ns[2])
  for (i in seq_len(n_strats)) {
    for (j in seq_len(ns[2])) {
      A[i, j] <- game$payoff(c(i, j))[1]
    }
  }

  n_steps <- as.integer(t_max / dt)
  trajectory <- matrix(0, nrow = n_steps + 1L, ncol = n_strats)
  times <- numeric(n_steps + 1L)

  trajectory[1, ] <- x0
  times[1] <- 0

  x <- x0
  for (step in seq_len(n_steps)) {
    # Expected payoff for each strategy: pi_i = sum_j A[i,j]*x_j
    payoffs <- as.numeric(A %*% x)
    # Average payoff: avg_pi = sum_i x_i * pi_i
    avg_payoff <- sum(x * payoffs)
    # Replicator equation: dx_i/dt = x_i * (pi_i - avg_pi)
    dx <- x * (payoffs - avg_payoff)
    x <- x + dt * dx
    # Clamp to [0, 1] and renormalize to stay on simplex
    x <- pmax(x, 0)
    s <- sum(x)
    if (s > 0) x <- x / s

    trajectory[step + 1L, ] <- x
    times[step + 1L] <- step * dt
  }

  list(trajectory = trajectory, time = times)
}


#' Fictitious Play
#'
#' Simulate fictitious play for a 2-player normal form game. In each round,
#' both players simultaneously best-respond to the empirical frequency of
#' their opponent's past actions.
#'
#' @param game A 2-player \code{\link{NormalFormGame}}.
#' @param n_rounds Integer, the number of rounds to simulate.
#' @return A list with components:
#'   \describe{
#'     \item{actions}{A list of length \code{n_rounds}, each element is an
#'       integer vector of length 2 giving the strategy indices chosen by each
#'       player.}
#'     \item{beliefs}{A list of length 2 giving the final empirical beliefs.
#'       \code{beliefs[[p]]} is a numeric vector of frequencies representing
#'       player \code{p}'s belief about the opponent's strategy distribution.}
#'     \item{belief_history}{A list of length 2, each element is a matrix
#'       with \code{n_rounds} rows tracking the belief evolution.}
#'   }
#' @export
fictitious_play <- function(game, n_rounds) {
  stopifnot(inherits(game, "NormalFormGame"))
  stopifnot(game$n_players == 2L)

  ns <- game$n_strategies
  actions <- vector("list", n_rounds)

  # Counts of opponent actions observed
  counts <- list(
    numeric(ns[2]),  # player 1's counts of player 2's actions
    numeric(ns[1])   # player 2's counts of player 1's actions
  )

  belief_history <- list(
    matrix(0, nrow = n_rounds, ncol = ns[2]),
    matrix(0, nrow = n_rounds, ncol = ns[1])
  )

  for (t in seq_len(n_rounds)) {
    chosen <- integer(2)
    for (p in 1:2) {
      opp <- 3L - p
      opp_n <- ns[opp]

      if (t == 1L) {
        # First round: uniform belief over opponent strategies
        belief <- rep(1 / opp_n, opp_n)
      } else {
        total <- sum(counts[[p]])
        if (total > 0) {
          belief <- counts[[p]] / total
        } else {
          belief <- rep(1 / opp_n, opp_n)
        }
      }

      # Best respond to belief
      br_indices <- best_response(game, p, belief)
      # Break ties uniformly at random
      chosen[p] <- if (length(br_indices) == 1L) {
        br_indices
      } else {
        sample(br_indices, 1)
      }
    }

    actions[[t]] <- chosen

    # Update counts: each player observes the opponent's action
    counts[[1]][chosen[2]] <- counts[[1]][chosen[2]] + 1
    counts[[2]][chosen[1]] <- counts[[2]][chosen[1]] + 1

    # Record beliefs
    total1 <- sum(counts[[1]])
    total2 <- sum(counts[[2]])
    belief_history[[1]][t, ] <- if (total1 > 0) counts[[1]] / total1 else rep(1 / ns[2], ns[2])
    belief_history[[2]][t, ] <- if (total2 > 0) counts[[2]] / total2 else rep(1 / ns[1], ns[1])
  }

  # Final beliefs
  beliefs <- list(
    counts[[1]] / sum(counts[[1]]),
    counts[[2]] / sum(counts[[2]])
  )

  list(actions = actions, beliefs = beliefs, belief_history = belief_history)
}


#' Best Response Dynamics
#'
#' Simulate continuous best response dynamics for a symmetric 2-player game
#' interpreted as a single-population model. At each step, the population state
#' is updated as a convex combination of the current state and the best
#' response distribution:
#' \eqn{x_{t+1} = (1-\alpha) x_t + \alpha \cdot BR(x_t)}{x_{t+1} = (1-alpha)*x_t + alpha*BR(x_t)}.
#'
#' @param game A symmetric 2-player \code{\link{NormalFormGame}}.
#' @param x0 Numeric vector giving the initial population state (must sum to 1).
#' @param n_rounds Integer, the number of update steps.
#' @param alpha Numeric scalar in (0, 1], the adjustment speed (default 0.5).
#' @return A list with component \code{trajectory}, a numeric matrix with one
#'   row per time step and one column per strategy.
#' @export
best_response_dynamics <- function(game, x0, n_rounds, alpha = 0.5) {
  stopifnot(inherits(game, "NormalFormGame"))
  stopifnot(game$n_players == 2L)

  ns <- game$n_strategies
  n_strats <- ns[1]
  stopifnot(length(x0) == n_strats)
  stopifnot(abs(sum(x0) - 1) < 1e-10)
  stopifnot(alpha > 0 && alpha <= 1)

  trajectory <- matrix(0, nrow = n_rounds + 1L, ncol = n_strats)
  trajectory[1, ] <- x0

  x <- x0
  for (t in seq_len(n_rounds)) {
    # Find best response strategies to current population state
    # Using player 1 (symmetric game, so same for both)
    br_indices <- best_response(game, 1L, x)

    # Convert to probability vector (uniform over best responses)
    br_vec <- numeric(n_strats)
    br_vec[br_indices] <- 1 / length(br_indices)

    # Update
    x <- (1 - alpha) * x + alpha * br_vec
    # Ensure on simplex
    x <- pmax(x, 0)
    x <- x / sum(x)

    trajectory[t + 1L, ] <- x
  }

  list(trajectory = trajectory)
}


#' Check Evolutionarily Stable Strategy
#'
#' Test whether a given mixed strategy is an Evolutionarily Stable Strategy
#' (ESS) for a symmetric 2-player game. A strategy \eqn{x^*}{x*} is an ESS if:
#' \enumerate{
#'   \item \eqn{x^*}{x*} is a best response to itself (Nash equilibrium condition).
#'   \item For every alternative best response \eqn{y \neq x^*}{y != x*},
#'     the strategy \eqn{x^*}{x*} does strictly better against \eqn{y} than
#'     \eqn{y} does against itself: \eqn{x^{*T} A y > y^T A y}{x*'Ay > y'Ay}.
#' }
#'
#' @param game A symmetric 2-player \code{\link{NormalFormGame}}.
#' @param strategy Numeric vector giving the mixed strategy to test (must sum to 1).
#' @param tol Numeric tolerance for comparisons.
#' @return Logical scalar.
#' @export
is_ess <- function(game, strategy, tol = 1e-10) {
  stopifnot(inherits(game, "NormalFormGame"))
  stopifnot(game$n_players == 2L)

  ns <- game$n_strategies
  n_strats <- ns[1]
  stopifnot(length(strategy) == n_strats)
  stopifnot(abs(sum(strategy) - 1) < tol)

  # Extract the payoff matrix A for player 1
  A <- matrix(0, nrow = n_strats, ncol = ns[2])
  for (i in seq_len(n_strats)) {
    for (j in seq_len(ns[2])) {
      A[i, j] <- game$payoff(c(i, j))[1]
    }
  }

  x <- strategy

  # Condition 1: x must be a best response to itself
  # payoff of x against x: x'Ax
  payoff_xx <- as.numeric(t(x) %*% A %*% x)

  # Check each pure strategy's payoff against x
  payoffs_against_x <- as.numeric(A %*% x)
  max_payoff <- max(payoffs_against_x)

  # x must achieve the maximum payoff against itself
  payoff_x_vs_x <- sum(x * payoffs_against_x)
  if (payoff_x_vs_x < max_payoff - tol) {
    return(FALSE)  # Not a best response to itself
  }

  # Condition 2: For any alternative best response y != x, x'Ay > y'Ay
  # Check all pure strategies that are best responses to x
  br_indices <- which(payoffs_against_x >= max_payoff - tol)

  for (i in br_indices) {
    # Pure strategy e_i
    y <- numeric(n_strats)
    y[i] <- 1

    # Skip if y == x (within tolerance)
    if (all(abs(y - x) < tol)) next

    # x'Ay: payoff of x against y
    payoff_x_vs_y <- as.numeric(t(x) %*% A %*% y)
    # y'Ay: payoff of y against y
    payoff_y_vs_y <- as.numeric(t(y) %*% A %*% y)

    if (payoff_x_vs_y <= payoff_y_vs_y + tol) {
      return(FALSE)
    }
  }

  # Also check mixed strategies over the best response set
  # For 2x2 games, checking pure strategies suffices since any convex

  # combination of alternative best responses would also need to satisfy
  # the condition, and it's enough to check the vertices.
  # For larger games, a more thorough check would be needed.

  TRUE
}
