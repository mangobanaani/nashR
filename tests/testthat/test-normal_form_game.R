test_that("NormalFormGame stores players and strategies", {
  g <- NormalFormGame$new(
    players = c("Row", "Col"),
    strategies = list(c("U", "D"), c("L", "R")),
    payoffs = array(c(3, 1, 0, 2, 2, 0, 1, 3), dim = c(2, 2, 2))
  )

  expect_equal(g$n_players, 2)
  expect_equal(g$players, c("Row", "Col"))
  expect_equal(g$strategies, list(c("U", "D"), c("L", "R")))
  expect_equal(g$n_strategies, c(2, 2))
})

test_that("NormalFormGame retrieves payoffs correctly", {
  # Prisoner's Dilemma payoffs:
  #          Col: C    Col: D
  # Row: C   (3,3)    (0,5)
  # Row: D   (5,0)    (1,1)
  payoffs <- array(dim = c(2, 2, 2))
  payoffs[1, 1, ] <- c(3, 3)  # (C, C)
  payoffs[1, 2, ] <- c(0, 5)  # (C, D)
  payoffs[2, 1, ] <- c(5, 0)  # (D, C)
  payoffs[2, 2, ] <- c(1, 1)  # (D, D)

  g <- NormalFormGame$new(
    players = c("Row", "Col"),
    strategies = list(c("C", "D"), c("C", "D")),
    payoffs = payoffs
  )

  expect_equal(g$payoff(c(1, 1)), c(3, 3))
  expect_equal(g$payoff(c(1, 2)), c(0, 5))
  expect_equal(g$payoff(c(2, 1)), c(5, 0))
  expect_equal(g$payoff(c(2, 2)), c(1, 1))
})

test_that("NormalFormGame validates inputs", {
  expect_error(
    NormalFormGame$new(
      players = c("Row"),
      strategies = list(c("U", "D"), c("L", "R")),
      payoffs = array(0, dim = c(2, 2, 2))
    ),
    "players.*strategies|Number of players"
  )
})

test_that("NormalFormGame print method works", {
  payoffs <- array(dim = c(2, 2, 2))
  payoffs[1, 1, ] <- c(3, 3)
  payoffs[1, 2, ] <- c(0, 5)
  payoffs[2, 1, ] <- c(5, 0)
  payoffs[2, 2, ] <- c(1, 1)

  g <- NormalFormGame$new(
    players = c("Row", "Col"),
    strategies = list(c("C", "D"), c("C", "D")),
    payoffs = payoffs
  )

  expect_output(print(g), "Normal Form Game")
})
