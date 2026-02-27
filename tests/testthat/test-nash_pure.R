test_that("pure_nash finds PD equilibrium", {
  g <- prisoners_dilemma()
  eq <- pure_nash(g)
  expect_length(eq, 1)
  expect_equal(eq[[1]]$profile, c(2, 2))  # (D, D)
  expect_equal(eq[[1]]$payoffs, c(1, 1))
})

test_that("pure_nash finds BoS equilibria", {
  g <- battle_of_sexes()
  eq <- pure_nash(g)
  expect_length(eq, 2)
  profiles <- lapply(eq, function(e) e$profile)
  expect_true(list(c(1, 1)) %in% profiles)
  expect_true(list(c(2, 2)) %in% profiles)
})

test_that("pure_nash returns empty for matching pennies", {
  g <- matching_pennies()
  eq <- pure_nash(g)
  expect_length(eq, 0)
})

test_that("pure_nash finds coordination equilibria", {
  g <- coordination_game()
  eq <- pure_nash(g)
  expect_length(eq, 2)
})

test_that("pure_nash includes strategy names", {
  g <- prisoners_dilemma()
  eq <- pure_nash(g)
  expect_equal(eq[[1]]$strategy_names, c("D", "D"))
})

test_that("pure_nash validates input class", {
  expect_error(pure_nash(list()), "NormalFormGame")
})
