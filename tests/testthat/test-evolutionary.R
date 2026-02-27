test_that("replicator_dynamics converges for PD", {
  g <- prisoners_dilemma()
  # Start with mixed population: 50% cooperators, 50% defectors
  result <- replicator_dynamics(g, x0 = c(0.5, 0.5), t_max = 100, dt = 0.01)

  # In PD, defection dominates -> should converge to all defectors
  final <- result$trajectory[nrow(result$trajectory), ]
  expect_equal(as.numeric(final[1]), 0, tolerance = 0.01)  # cooperators -> 0
  expect_equal(as.numeric(final[2]), 1, tolerance = 0.01)  # defectors -> 1
})

test_that("replicator_dynamics converges to mixed ESS in hawk-dove", {
  g <- hawk_dove(V = 4, C = 6)
  result <- replicator_dynamics(g, x0 = c(0.5, 0.5), t_max = 100, dt = 0.01)

  final <- result$trajectory[nrow(result$trajectory), ]
  # ESS for hawk-dove with V=4, C=6: p_hawk = V/C = 2/3
  expect_equal(as.numeric(final[1]), 2/3, tolerance = 0.05)
})

test_that("replicator_dynamics returns trajectory", {
  g <- matching_pennies()
  result <- replicator_dynamics(g, x0 = c(0.6, 0.4), t_max = 10, dt = 0.1)

  expect_true(is.matrix(result$trajectory))
  expect_equal(ncol(result$trajectory), 2)
  expect_true(nrow(result$trajectory) > 1)
  # All rows should sum to 1 (probability simplex)
  row_sums <- rowSums(result$trajectory)
  expect_true(all(abs(row_sums - 1) < 1e-10))
})

test_that("fictitious_play converges for PD", {
  g <- prisoners_dilemma()
  result <- fictitious_play(g, n_rounds = 1000)

  # Should converge to both playing Defect
  final_beliefs <- result$beliefs
  # Player 1's belief about player 2 should concentrate on Defect
  expect_true(final_beliefs[[1]][2] > 0.9)  # belief that P2 plays D
  # Player 2's belief about player 1 should concentrate on Defect
  expect_true(final_beliefs[[2]][2] > 0.9)
})

test_that("fictitious_play returns action history", {
  g <- matching_pennies()
  result <- fictitious_play(g, n_rounds = 100)

  expect_equal(length(result$actions), 100)
  expect_true(is.list(result$beliefs))
  expect_equal(length(result$beliefs), 2)
})

test_that("best_response_dynamics converges for coordination game", {
  g <- coordination_game()
  # Start near one equilibrium
  result <- best_response_dynamics(g, x0 = c(0.8, 0.2), n_rounds = 50)

  final <- result$trajectory[nrow(result$trajectory), ]
  # Should converge to (A, A) = (1, 0)
  expect_equal(as.numeric(final[1]), 1, tolerance = 0.1)
})

test_that("is_ess checks evolutionary stability", {
  g <- hawk_dove(V = 4, C = 6)

  # Mixed strategy (2/3 Hawk, 1/3 Dove) is ESS
  expect_true(is_ess(g, c(2/3, 1/3)))

  # Pure Hawk is NOT ESS (since V < C, mutual hawk is bad)
  expect_false(is_ess(g, c(1, 0)))
})
