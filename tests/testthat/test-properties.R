test_that("is_zero_sum detects zero-sum games", {
  expect_true(is_zero_sum(matching_pennies()))
  expect_false(is_zero_sum(prisoners_dilemma()))
})

test_that("is_symmetric detects symmetric games", {
  expect_true(is_symmetric(prisoners_dilemma()))
  expect_true(is_symmetric(coordination_game()))
  expect_false(is_symmetric(battle_of_sexes()))
  expect_false(is_symmetric(matching_pennies()))
})

test_that("dominant_strategy finds dominant strategies", {
  g <- prisoners_dilemma()
  ds <- dominant_strategy(g)
  expect_equal(ds[[1]], "D")
  expect_equal(ds[[2]], "D")
})

test_that("dominant_strategy returns NULL when none exists", {
  g <- battle_of_sexes()
  ds <- dominant_strategy(g)
  expect_null(ds[[1]])
  expect_null(ds[[2]])
})

test_that("best_response computes best response to mixed strategy", {
  g <- matching_pennies()
  br <- best_response(g, player = 1, opponent_strategy = c(0.5, 0.5))
  expect_equal(length(br), 2)  # both strategies equally good

  br <- best_response(g, player = 1, opponent_strategy = c(1, 0))
  expect_equal(br, 1)  # H is best against pure H
})

test_that("is_pareto_optimal identifies Pareto optimal outcomes", {
  g <- prisoners_dilemma()
  expect_true(is_pareto_optimal(g, c(1, 1)))   # (C,C)=(3,3) is Pareto optimal
  expect_false(is_pareto_optimal(g, c(2, 2)))   # (D,D)=(1,1) is NOT (dominated by C,C)
})
