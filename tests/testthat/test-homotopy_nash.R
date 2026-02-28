# Tests for homotopy Nash equilibrium solver

test_that("homotopy finds matching pennies mixed NE", {
  game <- matching_pennies()
  result <- homotopy_nash(game)

  expect_true(is.list(result))
  expect_equal(length(result$strategies), 2)

  expect_equal(sum(result$strategies[[1]]), 1, tolerance = 1e-6)
  expect_equal(sum(result$strategies[[2]]), 1, tolerance = 1e-6)
  expect_equal(result$strategies[[1]], c(0.5, 0.5), tolerance = 0.05)
  expect_equal(result$strategies[[2]], c(0.5, 0.5), tolerance = 0.05)
})

test_that("homotopy finds pure NE for prisoner's dilemma", {
  game <- prisoners_dilemma()
  result <- homotopy_nash(game)

  expect_equal(sum(result$strategies[[1]]), 1, tolerance = 1e-6)
  expect_equal(sum(result$strategies[[2]]), 1, tolerance = 1e-6)
  expect_true(result$strategies[[1]][2] > 0.9)
  expect_true(result$strategies[[2]][2] > 0.9)
})

test_that("homotopy works for 3-player game", {
  game <- public_goods_game(n = 3, endowment = 10, multiplier = 1.5,
                            contribution_levels = c(0, 10))
  result <- homotopy_nash(game)

  expect_equal(length(result$strategies), 3)
  for (p in seq_len(3)) {
    expect_equal(sum(result$strategies[[p]]), 1, tolerance = 1e-6)
    expect_true(all(result$strategies[[p]] >= -1e-6))
  }
})

test_that("homotopy validates input", {
  expect_error(homotopy_nash("not a game"))
})

test_that("homotopy with custom start point", {
  game <- matching_pennies()
  result <- homotopy_nash(game, start = list(c(0.3, 0.7), c(0.6, 0.4)))

  expect_equal(sum(result$strategies[[1]]), 1, tolerance = 1e-6)
  expect_equal(sum(result$strategies[[2]]), 1, tolerance = 1e-6)
})

test_that("homotopy NE has valid probabilities for 3-player game", {
  players <- c("P1", "P2", "P3")
  strategies <- list(c("A", "B"), c("A", "B"), c("A", "B"))

  payoffs <- array(0, dim = c(2, 2, 2, 3))
  payoffs[1, 1, 1, ] <- c(3, 3, 3)
  payoffs[2, 1, 1, ] <- c(4, 1, 1)
  payoffs[1, 2, 1, ] <- c(1, 4, 1)
  payoffs[1, 1, 2, ] <- c(1, 1, 4)
  payoffs[2, 2, 1, ] <- c(2, 2, 0)
  payoffs[2, 1, 2, ] <- c(2, 0, 2)
  payoffs[1, 2, 2, ] <- c(0, 2, 2)
  payoffs[2, 2, 2, ] <- c(1, 1, 1)

  game <- NormalFormGame$new(players, strategies, payoffs)
  result <- homotopy_nash(game)

  for (p in 1:3) {
    expect_equal(sum(result$strategies[[p]]), 1, tolerance = 1e-6)
    expect_true(all(result$strategies[[p]] >= -1e-6))
  }
  expect_equal(length(result$payoffs), 3)
})
