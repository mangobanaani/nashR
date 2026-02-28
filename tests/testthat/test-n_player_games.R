test_that("cournot creates N-player Cournot game", {
  # 3-firm Cournot with linear demand P = 100 - Q, costs c_i = 10
  g <- cournot(
    n = 3,
    quantities = c(10, 20, 30, 40),  # discrete quantity choices
    demand = function(Q) max(100 - Q, 0),
    costs = c(10, 10, 10)
  )

  expect_s3_class(g, "NormalFormGame")
  expect_equal(g$n_players, 3)
  expect_equal(g$n_strategies, c(4, 4, 4))
})

test_that("cournot payoffs are correct", {
  g <- cournot(
    n = 2,
    quantities = c(0, 30, 45),
    demand = function(Q) max(100 - Q, 0),
    costs = c(10, 10)
  )

  # If both produce 30: Q=60, P=40, profit = 30*(40-10) = 900 each
  # Find index of quantity 30 (should be 2)
  p <- g$payoff(c(2, 2))
  expect_equal(p[1], 30 * (100 - 60 - 10))  # 900
  expect_equal(p[2], 30 * (100 - 60 - 10))  # 900
})

test_that("cournot accepts scalar cost for symmetric firms", {
  g <- cournot(
    n = 3,
    quantities = c(10, 20),
    demand = function(Q) max(100 - Q, 0),
    costs = 10
  )

  expect_s3_class(g, "NormalFormGame")
  expect_equal(g$n_players, 3)
})

test_that("bertrand creates 2-player Bertrand game", {
  g <- bertrand(
    n = 2,
    prices = c(0, 5, 10, 15, 20),
    demand = function(p) max(20 - p, 0),
    costs = c(5, 5)
  )

  expect_s3_class(g, "NormalFormGame")
  expect_equal(g$n_players, 2)
})

test_that("pure_nash works for 3-player game", {
  g <- cournot(
    n = 3,
    quantities = c(20, 22, 23),
    demand = function(Q) max(100 - Q, 0),
    costs = c(10, 10, 10)
  )

  eq <- pure_nash(g)
  # Should find at least one equilibrium (the game has few strategies)
  expect_true(length(eq) >= 1)  # should find at least one equilibrium
  for (e in eq) {
    expect_length(e$profile, 3)
    expect_length(e$payoffs, 3)
  }
})

test_that("public_goods_game creates N-player game", {
  g <- public_goods_game(
    n = 4,
    endowment = 10,
    multiplier = 2,
    contribution_levels = c(0, 5, 10)
  )

  expect_s3_class(g, "NormalFormGame")
  expect_equal(g$n_players, 4)
  expect_equal(g$n_strategies, c(3, 3, 3, 3))

  # If everyone contributes 10: pool = 40, multiplied = 80, each gets 80/4 = 20
  # Payoff = endowment - contribution + share = 10 - 10 + 20 = 20
  all_contribute <- rep(3, 4)  # index 3 = contribute 10
  p <- g$payoff(all_contribute)
  expect_equal(p[1], 20)
})

test_that("stag_hunt creates correct game", {
  g <- stag_hunt()

  expect_s3_class(g, "NormalFormGame")
  expect_equal(g$n_players, 2)
  # Two pure NE: (Stag, Stag) and (Hare, Hare)
  eq <- pure_nash(g)
  expect_length(eq, 2)
})
