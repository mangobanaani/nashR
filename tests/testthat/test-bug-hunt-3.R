# Tests for bug fixes found in bug hunt 3

# Bug 1: evolutionary functions should reject asymmetric games
test_that("replicator_dynamics rejects asymmetric games", {
  # 2x3 game: different strategy counts
  g <- NormalFormGame$new(
    players = c("P1", "P2"),
    strategies = list(c("A", "B"), c("X", "Y", "Z")),
    payoffs = array(c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12), dim = c(2, 3, 2))
  )
  expect_error(
    replicator_dynamics(g, c(0.5, 0.5), t_max = 1),
    "symmetric"
  )
})

test_that("best_response_dynamics rejects asymmetric games", {
  g <- NormalFormGame$new(
    players = c("P1", "P2"),
    strategies = list(c("A", "B"), c("X", "Y", "Z")),
    payoffs = array(c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12), dim = c(2, 3, 2))
  )
  expect_error(
    best_response_dynamics(g, c(0.5, 0.5), n_rounds = 10),
    "symmetric"
  )
})

test_that("is_ess rejects asymmetric games", {
  g <- NormalFormGame$new(
    players = c("P1", "P2"),
    strategies = list(c("A", "B"), c("X", "Y", "Z")),
    payoffs = array(c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12), dim = c(2, 3, 2))
  )
  expect_error(
    is_ess(g, c(0.5, 0.5)),
    "symmetric"
  )
})

# Bug 2: is_ess must check mixed deviations via negative definiteness
# on the tangent space, not just pure strategy deviations

test_that("is_ess correctly identifies Rock-Paper-Scissors mixed NE as not ESS", {
  # RPS: the unique fully mixed NE (1/3, 1/3, 1/3) is NOT an ESS
  # (it is only neutrally stable)
  A_rps <- matrix(c(
    0, -1,  1,
    1,  0, -1,
   -1,  1,  0
  ), nrow = 3, byrow = TRUE)

  g <- normal_form(
    players = c("P1", "P2"),
    strategies = list(c("R", "P", "S"), c("R", "P", "S")),
    payoffs = list(A_rps, t(A_rps))
  )

  # The mixed NE (1/3, 1/3, 1/3) should NOT be ESS
  expect_false(is_ess(g, c(1/3, 1/3, 1/3)))
})

test_that("is_ess correctly identifies ESS in 3-strategy game", {
  # Payoff matrix where (1/3,1/3,1/3) is NE and the tangent-space
  # matrix is negative definite, making it a true ESS
  A_mat <- matrix(c(
   -2,  1,  1,
    1, -2,  1,
    1,  1, -2
  ), nrow = 3, byrow = TRUE)

  g <- normal_form(
    players = c("P1", "P2"),
    strategies = list(c("A", "B", "C"), c("A", "B", "C")),
    payoffs = list(A_mat, t(A_mat))
  )

  # All payoffs against (1/3,1/3,1/3) are 0, so it's a NE.
  # Tangent-space eigenvalues are -3 and -9 (negative definite) -> ESS.
  expect_true(is_ess(g, c(1/3, 1/3, 1/3)))
})

test_that("is_ess works correctly for 2-strategy Hawk-Dove mixed NE", {
  # Hawk-Dove with V=2, C=4: mixed NE at (V/C, 1-V/C) = (0.5, 0.5)
  g <- hawk_dove(V = 2, C = 4)
  expect_true(is_ess(g, c(0.5, 0.5)))
})

test_that("is_ess returns FALSE for dominated strategy", {
  # In Prisoner's Dilemma, pure Cooperate is not ESS
  g <- prisoners_dilemma()
  expect_false(is_ess(g, c(1, 0)))  # pure C is not even NE
})

test_that("is_ess identifies pure ESS correctly", {
  # In PD, pure Defect is ESS
  g <- prisoners_dilemma()
  expect_true(is_ess(g, c(0, 1)))  # pure D is the unique NE and ESS
})

# Verify symmetric game functions still work on valid inputs
test_that("replicator_dynamics works on symmetric games", {
  g <- hawk_dove(V = 2, C = 4)
  result <- replicator_dynamics(g, c(0.9, 0.1), t_max = 5, dt = 0.01)
  expect_equal(ncol(result$trajectory), 2)
  # Should converge toward the mixed ESS (0.5, 0.5)
  final <- result$trajectory[nrow(result$trajectory), ]
  expect_true(abs(final[1] - 0.5) < 0.1)
})

test_that("best_response_dynamics works on symmetric games", {
  g <- coordination_game()
  result <- best_response_dynamics(g, c(0.9, 0.1), n_rounds = 50)
  expect_equal(ncol(result$trajectory), 2)
})
