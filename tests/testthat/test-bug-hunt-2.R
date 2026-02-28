# Bug-hunting test suite for nashR
# These tests probe for incorrect behavior in cooperative_game, shapley, core,
# extensive_form_game, backward_induction, auctions, mechanism_design,
# evolutionary, and n_player_games.

library(testthat)
library(nashR)

# =========================================================================
# 1. SHAPLEY VALUE
# =========================================================================

test_that("shapley_value: weighted voting game [51; 50, 49, 1]", {
  # Weighted voting game [51; 50, 49, 1]:
  # v({A})=0 (50<51), v({B})=0 (49<51), v({C})=0 (1<51)
  # v({A,B})=1 (99>=51), v({A,C})=1 (51>=51), v({B,C})=0 (50<51)
  # v({A,B,C})=1
  #
  # Shapley values computed by permutations:
  # ABC: A pivotal (A brings 50, need 51, not enough) -> wait...
  # Permutation analysis: who makes the coalition winning?
  # ABC: after A: {A}=50<51, after AB: {A,B}=99>=51 -> B pivotal
  # ACB: after A: 50<51, after AC: 51>=51 -> C pivotal
  # BAC: after B: 49<51, after BA: 99>=51 -> A pivotal
  # BCA: after B: 49<51, after BC: 50<51, after BCA: 100>=51 -> A pivotal
  # CAB: after C: 1<51, after CA: 51>=51 -> A pivotal
  # CBA: after C: 1<51, after CB: 50<51, after CBA: 100>=51 -> A pivotal
  #
  # A pivotal in: BAC, BCA, CAB, CBA = 4/6 = 2/3
  # B pivotal in: ABC = 1/6
  # C pivotal in: ACB = 1/6
  game <- cooperative_game(
    players = c("A", "B", "C"),
    value = function(S) {
      weights <- c(A = 50, B = 49, C = 1)
      if (length(S) == 0) return(0)
      if (sum(weights[S]) >= 51) 1 else 0
    }
  )
  sv <- shapley_value(game)
  # Efficiency: sum = v(N) = 1
  expect_equal(sum(sv), 1, tolerance = 1e-10)
  # Correct Shapley values
  expect_equal(unname(sv["A"]), 2/3, tolerance = 1e-10)
  expect_equal(unname(sv["B"]), 1/6, tolerance = 1e-10)
  expect_equal(unname(sv["C"]), 1/6, tolerance = 1e-10)
})

test_that("shapley_value: efficiency (sum = v(N)) for glove game", {
  # Glove game: 2 left-hand players, 1 right-hand player
  # v(S) = min(# left in S, # right in S)
  game <- cooperative_game(
    players = c("L1", "L2", "R"),
    value = function(S) {
      if (length(S) == 0) return(0)
      n_left <- sum(S %in% c("L1", "L2"))
      n_right <- sum(S == "R")
      min(n_left, n_right)
    }
  )
  sv <- shapley_value(game)
  # Efficiency: must sum to v(N) = 1
  expect_equal(sum(sv), 1, tolerance = 1e-10)
  # Known values: L1=L2=1/6, R=2/3
  expect_equal(unname(sv["L1"]), 1/6, tolerance = 1e-10)
  expect_equal(unname(sv["L2"]), 1/6, tolerance = 1e-10)
  expect_equal(unname(sv["R"]),  2/3, tolerance = 1e-10)
})

test_that("shapley_value: unanimity game", {
  # Unanimity game u_N: v(S) = 1 iff S = N
  game <- cooperative_game(
    players = c("1", "2", "3"),
    value = function(S) {
      if (length(S) == 3) 1 else 0
    }
  )
  sv <- shapley_value(game)
  expect_equal(sum(sv), 1, tolerance = 1e-10)
  # Each player gets 1/3
  expect_equal(unname(sv), rep(1/3, 3), tolerance = 1e-10)
})

test_that("shapley_value: 2-player additive game", {
  # v(S) = sum of individual values -> shapley = individual values
  game <- cooperative_game(
    players = c("A", "B"),
    value = function(S) {
      vals <- c(A = 3, B = 7)
      if (length(S) == 0) return(0)
      sum(vals[S])
    }
  )
  sv <- shapley_value(game)
  expect_equal(unname(sv["A"]), 3, tolerance = 1e-10)
  expect_equal(unname(sv["B"]), 7, tolerance = 1e-10)
})

# =========================================================================
# 2. NUCLEOLUS
# =========================================================================

test_that("nucleolus: 3-player majority game", {
  # Majority game: v(S) = 1 iff |S| >= 2
  # The nucleolus is (1/3, 1/3, 1/3) by symmetry
  game <- cooperative_game(
    players = c("A", "B", "C"),
    value = function(S) {
      if (length(S) >= 2) 1 else 0
    }
  )
  nuc <- nucleolus(game)
  expect_equal(sum(nuc), 1, tolerance = 1e-6)
  expect_equal(unname(nuc["A"]), 1/3, tolerance = 1e-6)
  expect_equal(unname(nuc["B"]), 1/3, tolerance = 1e-6)
  expect_equal(unname(nuc["C"]), 1/3, tolerance = 1e-6)
})

test_that("nucleolus: asymmetric 3-player game", {
  # v({1}) = 0, v({2}) = 0, v({3}) = 0
  # v({1,2}) = 6, v({1,3}) = 8, v({2,3}) = 4
  # v({1,2,3}) = 10
  # The nucleolus should be efficient (sum = 10)
  game <- cooperative_game(
    players = c("1", "2", "3"),
    value = function(S) {
      if (length(S) == 0) return(0)
      key <- paste(sort(S), collapse = ",")
      vals <- c("1" = 0, "2" = 0, "3" = 0,
                "1,2" = 6, "1,3" = 8, "2,3" = 4,
                "1,2,3" = 10)
      unname(vals[key])
    }
  )
  nuc <- nucleolus(game)
  # Must be efficient
  expect_equal(sum(nuc), 10, tolerance = 1e-6)
  # Must be in the core (if core is non-empty)
  expect_true(in_core(game, nuc, tol = 1e-5))
})

test_that("in_core: allocation not in core is correctly rejected", {
  # Simple majority game
  game <- cooperative_game(
    players = c("A", "B", "C"),
    value = function(S) {
      if (length(S) >= 2) 1 else 0
    }
  )
  # Allocation (1, 0, 0) is NOT in core because v({B,C}) = 1 but B+C get 0
  alloc <- c(A = 1, B = 0, C = 0)
  expect_false(in_core(game, alloc))
})

# =========================================================================
# 3. EXTENSIVE FORM GAMES
# =========================================================================

test_that("extensive_form: 3-player game n_players detection", {
  # BUG: n_players is computed as max(player index) across decision nodes.
  # If player 3 never has a decision node but only appears in terminal payoffs,
  # the game reports n_players = 2 instead of 3.
  # This is a genuine design bug: 3-player games where only 2 players act
  # cannot be properly represented.
  efg <- extensive_form() |>
    add_node("root", player = 1, actions = c("L", "R")) |>
    add_node("L", player = 2, actions = c("a", "b")) |>
    add_node("R", player = 2, actions = c("a", "b")) |>
    add_terminal("La", payoffs = c(3, 2, 1)) |>
    add_terminal("Lb", payoffs = c(1, 3, 2)) |>
    add_terminal("Ra", payoffs = c(2, 1, 3)) |>
    add_terminal("Rb", payoffs = c(0, 0, 0))

  # BUG DETECTED: n_players returns 2 (max player index in decision nodes)
  # but the game has 3-element payoff vectors, so it's a 3-player game.
  # This causes to_normal_form to fail with dimension mismatch.
  expect_equal(efg$n_players, 3L)
})

test_that("extensive_form: to_normal_form with 3 players who all decide", {
  # All 3 players have decision nodes so n_players should be correct
  efg <- extensive_form() |>
    add_node("root", player = 1, actions = c("L", "R")) |>
    add_node("L", player = 2, actions = c("a", "b")) |>
    add_node("R", player = 3, actions = c("x", "y")) |>
    add_terminal("La", payoffs = c(3, 2, 1)) |>
    add_terminal("Lb", payoffs = c(1, 3, 2)) |>
    add_terminal("Rx", payoffs = c(2, 1, 3)) |>
    add_terminal("Ry", payoffs = c(0, 0, 0))

  expect_equal(efg$n_players, 3L)

  # Convert to normal form should work for a true 3-player game
  nfg <- to_normal_form(efg)
  expect_equal(nfg$n_players, 3L)
})

test_that("extensive_form: to_normal_form with multiple decision nodes for same player", {
  # Player 1 acts at root, Player 2 acts after each P1 action
  # Then Player 1 acts again (has 2 decision nodes)
  efg <- extensive_form() |>
    add_node("root", player = 1, actions = c("L", "R")) |>
    add_node("L", player = 2, actions = c("a", "b")) |>
    add_node("R", player = 2, actions = c("a", "b")) |>
    add_node("La", player = 1, actions = c("x", "y")) |>
    add_terminal("Lax", payoffs = c(4, 1)) |>
    add_terminal("Lay", payoffs = c(2, 3)) |>
    add_terminal("Lb", payoffs = c(1, 2)) |>
    add_terminal("Ra", payoffs = c(3, 3)) |>
    add_terminal("Rb", payoffs = c(0, 0))

  nfg <- to_normal_form(efg)
  # Player 1 has 2 decision nodes (root: L/R, and La: x/y), so 2*2=4 strategies
  # Player 2 has 2 decision nodes (L: a/b, R: a/b), so 2*2=4 strategies
  expect_equal(nfg$n_strategies[1], 4L)
  expect_equal(nfg$n_strategies[2], 4L)
})

test_that("backward_induction: handles ties (equal payoffs)", {
  # Game where player has equal payoffs for two actions
  efg <- extensive_form() |>
    add_node("root", player = 1, actions = c("L", "R")) |>
    add_terminal("L", payoffs = c(5, 5)) |>
    add_terminal("R", payoffs = c(5, 3))

  # Both L and R give player 1 a payoff of 5, so backward induction has a tie
  # It should still return a valid result without error
  result <- backward_induction(efg)
  expect_true(result$outcome[1] == 5)
  # The action chosen should be one of L or R
  expect_true(result$actions[["root"]] %in% c("L", "R"))
})

test_that("backward_induction: strict preferences", {
  # Simple 2-player game where backward induction has clear answer
  efg <- extensive_form() |>
    add_node("root", player = 1, actions = c("L", "R")) |>
    add_node("L", player = 2, actions = c("a", "b")) |>
    add_terminal("La", payoffs = c(3, 1)) |>
    add_terminal("Lb", payoffs = c(1, 3)) |>
    add_terminal("R", payoffs = c(2, 2))

  result <- backward_induction(efg)
  # Player 2 at node L: chooses b (payoff 3 > 1)
  # Player 1 at root: L gives (1, 3) via b, R gives (2, 2)
  # Player 1 prefers R (2 > 1)
  expect_equal(result$actions[["root"]], "R")
  expect_equal(result$outcome, c(2, 2))
})

# =========================================================================
# 4. AUCTIONS
# =========================================================================

test_that("second_price_auction: 3 bidders correct payoffs", {
  # Values: 10, 8, 6. Bid levels: 0, 5, 8, 10
  auction <- second_price_auction(3, values = c(10, 8, 6), bid_levels = c(0, 5, 8, 10))

  # If bidders bid 10, 8, 5: winner is bidder 1 (bid 10), pays 2nd price = 8
  # Payoff: 10 - 8 = 2 for bidder 1, 0 for others
  p <- auction$payoff(c(4, 3, 2))  # bids: 10, 8, 5
  expect_equal(p[1], 2)
  expect_equal(p[2], 0)
  expect_equal(p[3], 0)
})

test_that("second_price_auction: all bidders bid 0", {
  auction <- second_price_auction(3, values = c(10, 8, 6), bid_levels = c(0, 5, 10))

  # All bid 0: tie among all 3, second price = 0
  # Payoff split: (10 - 0)/3, (8 - 0)/3, (6 - 0)/3
  p <- auction$payoff(c(1, 1, 1))  # all bid 0
  expect_equal(p[1], 10/3, tolerance = 1e-10)
  expect_equal(p[2], 8/3, tolerance = 1e-10)
  expect_equal(p[3], 6/3, tolerance = 1e-10)
})

test_that("first_price_auction: tie-breaking with N>2", {
  auction <- first_price_auction(3, values = c(10, 10, 10), bid_levels = c(0, 5, 10))

  # All bid 5: 3-way tie, each wins with prob 1/3
  # Payoff: (10 - 5)/3 = 5/3 each
  p <- auction$payoff(c(2, 2, 2))  # all bid 5
  expect_equal(p[1], 5/3, tolerance = 1e-10)
  expect_equal(p[2], 5/3, tolerance = 1e-10)
  expect_equal(p[3], 5/3, tolerance = 1e-10)
})

test_that("first_price_auction: 2-way tie with 3 bidders", {
  auction <- first_price_auction(3, values = c(10, 8, 6), bid_levels = c(0, 5, 10))

  # Bidders 1 and 2 bid 5, bidder 3 bids 0: 2-way tie between 1 and 2
  # Payoff: (10-5)/2=2.5 for bidder 1, (8-5)/2=1.5 for bidder 2, 0 for bidder 3
  p <- auction$payoff(c(2, 2, 1))  # bids: 5, 5, 0
  expect_equal(p[1], 2.5, tolerance = 1e-10)
  expect_equal(p[2], 1.5, tolerance = 1e-10)
  expect_equal(p[3], 0, tolerance = 1e-10)
})

test_that("second_price_auction: truthful bidding is dominant strategy", {
  # In a 2nd-price auction, bidding true value is weakly dominant
  auction <- second_price_auction(2, values = c(10, 7), bid_levels = 0:10)

  eqs <- pure_nash(auction)
  # Truthful bidding (10, 7) should be a NE
  truthful_found <- FALSE
  for (eq in eqs) {
    bids <- sapply(seq_len(2), function(p)
      as.numeric(auction$strategies[[p]][eq$profile[p]]))
    if (all(bids == c(10, 7))) {
      truthful_found <- TRUE
      break
    }
  }
  expect_true(truthful_found, info = "Truthful bidding should be a Nash equilibrium")
})

# =========================================================================
# 5. MECHANISM DESIGN
# =========================================================================

test_that("vcg_mechanism: agent removal doesn't change allocation", {
  # Single-item auction with 3 agents
  # Removing agent 2 or 3 doesn't change the winner (agent 1 still wins)
  valuations <- list(
    agent1 = c(item = 10),
    agent2 = c(item = 5),
    agent3 = c(item = 3)
  )

  alloc_rule <- function(vals) {
    n <- length(vals)
    alloc <- numeric(n)
    best <- which.max(sapply(vals, function(v) v["item"]))
    alloc[best] <- 1
    alloc
  }

  result <- vcg_mechanism(valuations, alloc_rule)

  # Agent 1 wins, pays externality = value others lose
  # With agent 1: others get 0. Without agent 1: agent 2 wins, gets 5.
  # Payment for agent 1 = 5 - 0 = 5
  expect_equal(result$payments[1], 5)
  # Agent 2 and 3 pay 0 (removing them doesn't change outcome)
  expect_equal(result$payments[2], 0)
  expect_equal(result$payments[3], 0)
  # Utilities: agent1 = 10 - 5 = 5, agent2 = 0, agent3 = 0
  expect_equal(result$utilities[1], 5)
  expect_equal(result$utilities[2], 0)
  expect_equal(result$utilities[3], 0)
})

test_that("vcg_mechanism: single-item allocation indexing", {
  # Test that agent_value indexing works correctly for vector allocations.
  # The agent_value helper uses alloc[agent_idx] for vector allocations,
  # which should correctly index into a flat vector.
  valuations <- list(
    a1 = c(item = 10),
    a2 = c(item = 8),
    a3 = c(item = 5)
  )

  alloc_rule <- function(vals) {
    n <- length(vals)
    alloc <- numeric(n)
    best <- which.max(sapply(vals, function(v) v["item"]))
    alloc[best] <- 1
    alloc
  }

  # This should not error
  result <- vcg_mechanism(valuations, alloc_rule)
  expect_equal(result$allocation[1], 1)
  expect_equal(result$allocation[2], 0)
  expect_equal(result$allocation[3], 0)

  # VCG payment for winner should be 8 (second highest value)
  expect_equal(result$payments[1], 8)
})

test_that("vcg_mechanism: payments are non-negative (no subsidies)", {
  # With VCG, payments should be >= 0 in a single-item auction
  valuations <- list(
    agent1 = c(item = 10),
    agent2 = c(item = 10)  # same value as agent 1
  )

  alloc_rule <- function(vals) {
    n <- length(vals)
    alloc <- numeric(n)
    best <- which.max(sapply(vals, function(v) v["item"]))
    alloc[best] <- 1
    alloc
  }

  result <- vcg_mechanism(valuations, alloc_rule)
  expect_true(all(result$payments >= -1e-10))
})

test_that("is_incentive_compatible: second-price auction is IC", {
  # A simple second-price auction mechanism
  mechanism <- list(
    allocation = function(reports) {
      n <- length(reports)
      alloc <- numeric(n)
      winner <- which.max(reports)
      if (length(winner) > 1) winner <- winner[1]
      alloc[winner] <- 1
      alloc
    },
    payment = function(reports) {
      n <- length(reports)
      pays <- numeric(n)
      winner <- which.max(reports)
      if (length(winner) > 1) winner <- winner[1]
      # Second price = max of others
      pays[winner] <- max(reports[-winner])
      pays
    }
  )

  result <- is_incentive_compatible(mechanism, n_agents = 2,
                                     type_space = c(0, 1, 2, 3))
  expect_true(result)
})

# =========================================================================
# 6. EVOLUTIONARY DYNAMICS
# =========================================================================

test_that("replicator_dynamics: simplex preservation (probabilities sum to 1)", {
  # Prisoner's dilemma
  pd <- normal_form(
    players = c("Row", "Col"),
    strategies = list(c("C", "D"), c("C", "D")),
    payoffs = list(
      matrix(c(3, 0, 5, 1), nrow = 2, byrow = TRUE),
      matrix(c(3, 5, 0, 1), nrow = 2, byrow = TRUE)
    )
  )

  result <- replicator_dynamics(pd, x0 = c(0.6, 0.4), t_max = 10, dt = 0.01)

  # Check that ALL rows of the trajectory sum to 1
  row_sums <- rowSums(result$trajectory)
  expect_true(all(abs(row_sums - 1) < 1e-6),
              info = paste("Max deviation from 1:", max(abs(row_sums - 1))))

  # Check all values are non-negative
  expect_true(all(result$trajectory >= -1e-10))
})

test_that("replicator_dynamics: D converges to 1 in prisoner's dilemma", {
  pd <- normal_form(
    players = c("Row", "Col"),
    strategies = list(c("C", "D"), c("C", "D")),
    payoffs = list(
      matrix(c(3, 0, 5, 1), nrow = 2, byrow = TRUE),
      matrix(c(3, 5, 0, 1), nrow = 2, byrow = TRUE)
    )
  )

  result <- replicator_dynamics(pd, x0 = c(0.5, 0.5), t_max = 50, dt = 0.01)
  final <- result$trajectory[nrow(result$trajectory), ]
  # Defect should dominate: x_D -> 1
  expect_true(final[2] > 0.99, info = paste("D frequency:", final[2]))
})

test_that("is_ess: pure strategy in prisoner's dilemma", {
  # In PD, Defect is the unique ESS
  pd <- normal_form(
    players = c("Row", "Col"),
    strategies = list(c("C", "D"), c("C", "D")),
    payoffs = list(
      matrix(c(3, 0, 5, 1), nrow = 2, byrow = TRUE),
      matrix(c(3, 5, 0, 1), nrow = 2, byrow = TRUE)
    )
  )

  # Defect (pure strategy [0, 1]) should be ESS
  expect_true(is_ess(pd, c(0, 1)))
  # Cooperate (pure strategy [1, 0]) should NOT be ESS
  expect_false(is_ess(pd, c(1, 0)))
})

test_that("is_ess: checks second condition (invasion barrier) in RPS", {
  # Rock-Paper-Scissors: the mixed NE (1/3, 1/3, 1/3) is NOT an ESS
  # because every pure strategy is also a best response to the mixed NE
  # and the invasion barrier condition fails
  rps <- normal_form(
    players = c("Row", "Col"),
    strategies = list(c("R", "P", "S"), c("R", "P", "S")),
    payoffs = list(
      matrix(c(0, -1, 1,
               1, 0, -1,
               -1, 1, 0), nrow = 3, byrow = TRUE),
      matrix(c(0, 1, -1,
               -1, 0, 1,
               1, -1, 0), nrow = 3, byrow = TRUE)
    )
  )

  # The mixed NE (1/3, 1/3, 1/3) is NOT an ESS
  expect_false(is_ess(rps, c(1/3, 1/3, 1/3)))
})

test_that("fictitious_play: matching pennies oscillates (no convergence to pure)", {
  mp <- normal_form(
    players = c("Row", "Col"),
    strategies = list(c("H", "T"), c("H", "T")),
    payoffs = list(
      matrix(c(1, -1, -1, 1), nrow = 2, byrow = TRUE),
      matrix(c(-1, 1, 1, -1), nrow = 2, byrow = TRUE)
    )
  )

  set.seed(42)
  result <- fictitious_play(mp, n_rounds = 1000)

  # Beliefs should converge toward (0.5, 0.5) -- the mixed NE
  # They should NOT converge to a pure strategy (0 or 1)
  final_belief_1 <- result$beliefs[[1]]
  final_belief_2 <- result$beliefs[[2]]

  # Neither belief component should be very close to 0 or 1
  expect_true(all(final_belief_1 > 0.1 & final_belief_1 < 0.9),
              info = paste("Belief 1:", paste(round(final_belief_1, 3), collapse = ", ")))
  expect_true(all(final_belief_2 > 0.1 & final_belief_2 < 0.9),
              info = paste("Belief 2:", paste(round(final_belief_2, 3), collapse = ", ")))
})

# =========================================================================
# 7. N-PLAYER GAMES
# =========================================================================

test_that("cournot: asymmetric costs produce correct NE", {
  # 2 firms, P = 100 - Q, costs c1=10, c2=20
  # Cournot NE: q1 = (100 - 2*10 + 20)/3 = 100/3 ~ 33.33
  #             q2 = (100 - 2*20 + 10)/3 = 70/3 ~ 23.33
  game <- cournot(
    n = 2,
    quantities = seq(0, 50, by = 1),
    demand = function(Q) max(100 - Q, 0),
    costs = c(10, 20)
  )

  eqs <- pure_nash(game)
  expect_true(length(eqs) >= 1, info = "Should find at least one NE")

  # Check that at least one equilibrium has quantities close to theoretical values
  found_close <- FALSE
  for (eq in eqs) {
    q1 <- as.numeric(game$strategies[[1]][eq$profile[1]])
    q2 <- as.numeric(game$strategies[[2]][eq$profile[2]])
    if (abs(q1 - 100/3) <= 1 && abs(q2 - 70/3) <= 1) {
      found_close <- TRUE
      break
    }
  }
  expect_true(found_close, info = "Should find NE near theoretical values")
})

test_that("bertrand: lowest bidder pricing below cost gets negative profit", {
  # Firm 1 prices at 3 (below cost of 5), Firm 2 prices at 10
  game <- bertrand(
    n = 2,
    prices = c(3, 5, 10),
    demand = function(p) 100,
    costs = c(5, 5)
  )

  # Firm 1 bids 3, Firm 2 bids 10 -> Firm 1 wins at price 3, cost 5
  # Profit = (3 - 5) * 100 = -200
  p <- game$payoff(c(1, 3))  # price indices: 3 and 10
  expect_equal(p[1], -200)
  expect_equal(p[2], 0)
})

test_that("public_goods_game: free-rider equilibrium", {
  # With multiplier < n, contributing 0 is the dominant strategy
  game <- public_goods_game(
    n = 3,
    endowment = 10,
    multiplier = 1.5,  # < 3, so free-riding dominates
    contribution_levels = c(0, 5, 10)
  )

  eqs <- pure_nash(game)
  expect_true(length(eqs) >= 1)

  # The free-rider equilibrium: all contribute 0
  free_rider_found <- FALSE
  for (eq in eqs) {
    contribs <- sapply(seq_len(3), function(p)
      as.numeric(game$strategies[[p]][eq$profile[p]]))
    if (all(contribs == 0)) {
      free_rider_found <- TRUE
      break
    }
  }
  expect_true(free_rider_found, info = "Free-rider equilibrium (all contribute 0) should exist")
})

# =========================================================================
# 8. COURNOT EQUILIBRIUM: specific known NE
# =========================================================================

test_that("cournot: 2 firms, P=100-Q, c=10, NE includes q1=q2=30", {
  # With symmetric costs c=10, demand P=100-Q:
  # NE: q* = (100 - c) / 3 = 90 / 3 = 30
  # With integer grid, (30, 30) should be among the NE found.
  game <- cournot(
    n = 2,
    quantities = seq(0, 60, by = 1),
    demand = function(Q) max(100 - Q, 0),
    costs = c(10, 10)
  )

  eqs <- pure_nash(game)
  expect_true(length(eqs) >= 1, info = "Should find at least one NE")

  # Check that (30, 30) is among the Nash equilibria
  found_30_30 <- FALSE
  for (eq in eqs) {
    q1 <- as.numeric(game$strategies[[1]][eq$profile[1]])
    q2 <- as.numeric(game$strategies[[2]][eq$profile[2]])
    if (q1 == 30 && q2 == 30) {
      found_30_30 <- TRUE
      break
    }
  }
  expect_true(found_30_30, info = "NE at (30, 30) should be found")
})

test_that("cournot: NE is unique at exact best response", {
  # With continuous best response q_i = (90 - q_j)/2, the unique NE is (30, 30).
  # But with discrete grid, there may be multiple NE due to flat best responses.
  # If the code computes integer payoffs, (30, 30) gives profit 30*(100-60-10)=900
  # while (31, 29) gives firm 1: 31*(100-60-10)=930 and firm 2: 29*(100-60-10)=870
  # Wait -- total Q is still 60 in both cases! So price is 40 for both.
  # At (31, 29): profit1 = 31*(40-10) = 930, profit2 = 29*(40-10) = 870
  # Can firm 1 deviate from 31 to 30? At (30, 29): Q=59, P=41, profit1=30*31=930
  # Can firm 1 deviate from 31 to 32? At (32, 29): Q=61, P=39, profit1=32*29=928
  # So 31 is indeed best response to 29, and 29 is best response to 31!
  # This means (31, 29) and (29, 31) are also NE in the discretized game.
  # This is NOT a bug -- it's a feature of discretization.
  game <- cournot(
    n = 2,
    quantities = seq(0, 60, by = 1),
    demand = function(Q) max(100 - Q, 0),
    costs = c(10, 10)
  )

  eqs <- pure_nash(game)
  # With discretization, we expect 3 NE: (29,31), (30,30), (31,29)
  expect_true(length(eqs) >= 1)

  # All NE should have total quantity close to 60
  for (eq in eqs) {
    q1 <- as.numeric(game$strategies[[1]][eq$profile[1]])
    q2 <- as.numeric(game$strategies[[2]][eq$profile[2]])
    expect_true(abs(q1 + q2 - 60) <= 2,
                info = paste("Total Q =", q1 + q2, "expected ~60"))
  }
})

test_that("cournot: 3-firm symmetric Cournot NE", {
  # 3 firms, P = 100 - Q, c = 10
  # NE: q* = (100 - 10) / (3 + 1) = 22.5
  game <- cournot(
    n = 3,
    quantities = seq(0, 50, by = 1),
    demand = function(Q) max(100 - Q, 0),
    costs = c(10, 10, 10)
  )

  eqs <- pure_nash(game)
  expect_true(length(eqs) >= 1, info = "Should find at least one NE for 3-firm Cournot")

  # With grid step 1, NE should be near q=22 or q=23
  eq <- eqs[[1]]
  qs <- sapply(seq_len(3), function(p)
    as.numeric(game$strategies[[p]][eq$profile[p]]))
  for (i in 1:3) {
    expect_true(abs(qs[i] - 22.5) <= 1,
                info = paste("Firm", i, "quantity:", qs[i], "expected ~22.5"))
  }
})

test_that("bertrand: symmetric firms, NE at marginal cost", {
  # Standard Bertrand result: NE is p = c (marginal cost pricing)
  game <- bertrand(
    n = 2,
    prices = c(5, 6, 7, 8, 9, 10),
    demand = function(p) 100,
    costs = c(5, 5)
  )

  eqs <- pure_nash(game)
  expect_true(length(eqs) >= 1)

  # The NE should have both firms pricing at marginal cost = 5
  mc_eq_found <- FALSE
  for (eq in eqs) {
    prices <- sapply(seq_len(2), function(p)
      as.numeric(game$strategies[[p]][eq$profile[p]]))
    if (all(prices == 5)) {
      mc_eq_found <- TRUE
      break
    }
  }
  expect_true(mc_eq_found, info = "Bertrand NE at marginal cost should exist")
})

# =========================================================================
# 9. ANALYTICAL FORMULAS
# =========================================================================

test_that("optimal_bid_first_price: formula correctness", {
  # For uniform[0,1] with n=2 bidders and value=0.8:
  # Optimal bid = (n-1)/n * v = 1/2 * 0.8 = 0.4
  bid <- optimal_bid_first_price(0.8, n_bidders = 2)
  expect_equal(bid, 0.4)

  # For uniform[0,1] with n=3 bidders and value=0.9:
  # Optimal bid = (2/3) * 0.9 = 0.6
  bid <- optimal_bid_first_price(0.9, n_bidders = 3)
  expect_equal(bid, 0.6)
})

test_that("expected_revenue: revenue equivalence", {
  # For n=3 bidders with uniform[0,1]:
  # Expected revenue = (n-1)/(n+1) = 2/4 = 0.5
  for (atype in c("first_price", "second_price", "all_pay")) {
    rev <- expected_revenue(atype, n_bidders = 3)
    expect_equal(rev, 0.5, tolerance = 1e-10,
                 info = paste("Revenue for", atype))
  }
})
