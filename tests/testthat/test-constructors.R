test_that("normal_form creates a game from payoff matrices", {
  g <- normal_form(
    players = c("Row", "Col"),
    strategies = list(c("U", "D"), c("L", "R")),
    payoffs = list(
      matrix(c(3, 0, 5, 1), nrow = 2, byrow = TRUE),  # Row's payoffs
      matrix(c(3, 5, 0, 1), nrow = 2, byrow = TRUE)    # Col's payoffs
    )
  )

  expect_s3_class(g, "NormalFormGame")
  expect_equal(g$n_players, 2)
  expect_equal(g$payoff(c(1, 1)), c(3, 3))
})

test_that("normal_form accepts a pre-built payoff array", {
  payoffs <- array(dim = c(2, 2, 2))
  payoffs[1, 1, ] <- c(3, 3)
  payoffs[1, 2, ] <- c(0, 5)
  payoffs[2, 1, ] <- c(5, 0)
  payoffs[2, 2, ] <- c(1, 1)

  g <- normal_form(
    players = c("Row", "Col"),
    strategies = list(c("C", "D"), c("C", "D")),
    payoffs = payoffs
  )

  expect_s3_class(g, "NormalFormGame")
  expect_equal(g$payoff(c(1, 1)), c(3, 3))
  expect_equal(g$payoff(c(2, 2)), c(1, 1))
})

test_that("prisoners_dilemma creates correct game", {
  g <- prisoners_dilemma()
  expect_s3_class(g, "NormalFormGame")
  expect_equal(g$n_players, 2)
  expect_equal(g$payoff(c(1, 1)), c(3, 3))  # Cooperate, Cooperate
  expect_equal(g$payoff(c(2, 2)), c(1, 1))  # Defect, Defect
  # Temptation > Reward > Punishment > Sucker
  pd_payoffs <- c(
    g$payoff(c(2, 1))[1],  # temptation (5)
    g$payoff(c(1, 1))[1],  # reward (3)
    g$payoff(c(2, 2))[1],  # punishment (1)
    g$payoff(c(1, 2))[1]   # sucker (0)
  )
  expect_true(all(diff(pd_payoffs) < 0))
})

test_that("prisoners_dilemma accepts custom payoffs", {
  g <- prisoners_dilemma(R = 4, T = 6, S = -1, P = 0)
  expect_equal(g$payoff(c(1, 1)), c(4, 4))
  expect_equal(g$payoff(c(2, 1))[1], 6)
  expect_equal(g$payoff(c(1, 2))[1], -1)
  expect_equal(g$payoff(c(2, 2)), c(0, 0))
})

test_that("battle_of_sexes creates correct game", {
  g <- battle_of_sexes()
  expect_s3_class(g, "NormalFormGame")
  expect_true(g$payoff(c(1, 1))[1] > 0)  # coordination payoff
  expect_true(g$payoff(c(2, 2))[1] > 0)  # coordination payoff
  expect_equal(g$payoff(c(1, 2)), c(0, 0))  # miscoordination
})

test_that("matching_pennies creates correct zero-sum game", {
  g <- matching_pennies()
  expect_s3_class(g, "NormalFormGame")
  for (i in 1:2) {
    for (j in 1:2) {
      expect_equal(sum(g$payoff(c(i, j))), 0)
    }
  }
})

test_that("coordination_game creates correct game", {
  g <- coordination_game()
  expect_s3_class(g, "NormalFormGame")
  expect_true(g$payoff(c(1, 1))[1] > 0)
  expect_true(g$payoff(c(2, 2))[1] > 0)
  expect_equal(g$payoff(c(1, 2)), c(0, 0))
})

test_that("hawk_dove creates correct game", {
  g <- hawk_dove()
  expect_s3_class(g, "NormalFormGame")
  expect_equal(g$n_players, 2)
})
