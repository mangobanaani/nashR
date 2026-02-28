# Tests for BayesianGame class and BNE solver

test_that("BayesianGame construction works", {
  bg <- bayesian_game(
    players = c("P1", "P2"),
    type_sets = list(c("low", "high"), c("low", "high")),
    priors = list(c(0.5, 0.5), c(0.5, 0.5)),
    strategies = list(c("bid0", "bid1"), c("bid0", "bid1")),
    payoff_fn = function(player, type_profile, action_profile) {
      values <- c(1, 2)
      v <- values[type_profile[player]]
      bid <- action_profile[player] - 1
      opp <- 3 - player
      opp_bid <- action_profile[opp] - 1
      if (bid > opp_bid) return(v - bid)
      if (bid < opp_bid) return(0)
      return((v - bid) / 2)
    }
  )

  expect_s3_class(bg, "BayesianGame")
  expect_equal(bg$n_players, 2)
  expect_equal(bg$n_types, c(2, 2))
  expect_equal(bg$n_strategies, c(2, 2))
})

test_that("BayesianGame validation works", {
  expect_error(
    bayesian_game(
      players = c("P1"),
      type_sets = list(c("a")),
      priors = list(c(1)),
      strategies = list(c("x")),
      payoff_fn = function(p, t, a) 0
    )
  )

  expect_error(
    bayesian_game(
      players = c("P1", "P2"),
      type_sets = list(c("a"), c("b")),
      priors = list(c(0.3, 0.7), c(1)),
      strategies = list(c("x"), c("y")),
      payoff_fn = function(p, t, a) 0
    )
  )
})

test_that("to_normal_form produces correct dimensions", {
  bg <- bayesian_game(
    players = c("P1", "P2"),
    type_sets = list(c("L", "H"), c("L", "H")),
    priors = list(c(0.5, 0.5), c(0.5, 0.5)),
    strategies = list(c("A", "B"), c("A", "B")),
    payoff_fn = function(player, type_profile, action_profile) {
      type_profile[player] + action_profile[player]
    }
  )

  nf <- bg$to_normal_form()
  expect_s3_class(nf, "NormalFormGame")
  # 2 types, 2 actions each -> 2^2 = 4 composite strategies per player
  expect_equal(nf$n_strategies, c(4, 4))
  expect_equal(nf$n_players, 2)
})

test_that("BNE solver finds equilibrium for simple game", {
  # Matching pennies with complete information (trivial Bayesian game)
  bg <- bayesian_game(
    players = c("Matcher", "Mismatcher"),
    type_sets = list(c("t1"), c("t1")),
    priors = list(c(1), c(1)),
    strategies = list(c("H", "T"), c("H", "T")),
    payoff_fn = function(player, type_profile, action_profile) {
      match <- action_profile[1] == action_profile[2]
      if (player == 1) return(if (match) 1 else -1)
      return(if (match) -1 else 1)
    }
  )

  eqs <- bayesian_nash(bg)
  expect_true(length(eqs) >= 1)

  # With one type, behavioral strategy should be the mixed NE
  eq <- eqs[[1]]
  expect_equal(length(eq$strategies), 2)
  # Each player has one type "t1"
  expect_true("t1" %in% names(eq$strategies[[1]]))
  # Mixed NE: each plays 50/50
  expect_equal(eq$strategies[[1]][["t1"]], c(0.5, 0.5), tolerance = 0.05)
  expect_equal(eq$strategies[[2]][["t1"]], c(0.5, 0.5), tolerance = 0.05)
})

test_that("print method works", {
  bg <- bayesian_game(
    players = c("P1", "P2"),
    type_sets = list(c("L", "H"), c("L")),
    priors = list(c(0.5, 0.5), c(1)),
    strategies = list(c("A", "B"), c("A", "B")),
    payoff_fn = function(player, type_profile, action_profile) 0
  )

  expect_output(print(bg), "Bayesian Game")
  expect_output(print(bg), "P1")
  expect_output(print(bg), "independent")
})

test_that("joint prior works", {
  prior <- array(c(0.3, 0.2, 0.1, 0.4), dim = c(2, 2))
  bg <- bayesian_game(
    players = c("P1", "P2"),
    type_sets = list(c("L", "H"), c("L", "H")),
    priors = prior,
    strategies = list(c("A", "B"), c("A", "B")),
    payoff_fn = function(player, type_profile, action_profile) {
      type_profile[player]
    }
  )

  expect_output(print(bg), "joint distribution")
  nf <- bg$to_normal_form()
  expect_s3_class(nf, "NormalFormGame")
})
