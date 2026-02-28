# ===========================================================================
# Bug-hunting test suite for nashR
# Each test probes a specific edge case or potential correctness issue.
# Tests that FAIL indicate bugs in the package.
# ===========================================================================

# ---------------------------------------------------------------------------
# 1. Payoff matrix construction in normal_form()
# ---------------------------------------------------------------------------

test_that("normal_form: asymmetric 3x2 game payoffs are correct", {
  # Player 1 has 3 strategies, Player 2 has 2 strategies
  # Row payoffs:       Col payoffs:
  #      L   R              L   R
  # T  (1, 5) (2, 4)
  # M  (3, 3) (4, 2)
  # B  (5, 1) (6, 0)
  row_pay <- matrix(c(1, 2,
                       3, 4,
                       5, 6), nrow = 3, byrow = TRUE)
  col_pay <- matrix(c(5, 4,
                       3, 2,
                       1, 0), nrow = 3, byrow = TRUE)

  g <- normal_form(
    players = c("Row", "Col"),
    strategies = list(c("T", "M", "B"), c("L", "R")),
    payoffs = list(row_pay, col_pay)
  )

  # Check every cell
  expect_equal(g$payoff(c(1, 1)), c(1, 5))
  expect_equal(g$payoff(c(1, 2)), c(2, 4))
  expect_equal(g$payoff(c(2, 1)), c(3, 3))
  expect_equal(g$payoff(c(2, 2)), c(4, 2))
  expect_equal(g$payoff(c(3, 1)), c(5, 1))
  expect_equal(g$payoff(c(3, 2)), c(6, 0))
})

test_that("normal_form: 2x3 asymmetric game payoffs are correct", {
  # Player 1 has 2 strategies, Player 2 has 3 strategies
  row_pay <- matrix(c(1, 2, 3,
                       4, 5, 6), nrow = 2, byrow = TRUE)
  col_pay <- matrix(c(6, 5, 4,
                       3, 2, 1), nrow = 2, byrow = TRUE)

  g <- normal_form(
    players = c("Row", "Col"),
    strategies = list(c("U", "D"), c("L", "M", "R")),
    payoffs = list(row_pay, col_pay)
  )

  expect_equal(g$payoff(c(1, 1)), c(1, 6))
  expect_equal(g$payoff(c(1, 2)), c(2, 5))
  expect_equal(g$payoff(c(1, 3)), c(3, 4))
  expect_equal(g$payoff(c(2, 1)), c(4, 3))
  expect_equal(g$payoff(c(2, 2)), c(5, 2))
  expect_equal(g$payoff(c(2, 3)), c(6, 1))
})


# ---------------------------------------------------------------------------
# 2. Support enumeration correctness
# ---------------------------------------------------------------------------

test_that("support_enum: Rock-Paper-Scissors unique NE at (1/3, 1/3, 1/3)", {
  # Standard RPS: win=1, lose=-1, draw=0
  rps_1 <- matrix(c( 0, -1,  1,
                      1,  0, -1,
                     -1,  1,  0), nrow = 3, byrow = TRUE)
  rps_2 <- matrix(c( 0,  1, -1,
                     -1,  0,  1,
                      1, -1,  0), nrow = 3, byrow = TRUE)

  g <- normal_form(
    players = c("P1", "P2"),
    strategies = list(c("R", "P", "S"), c("R", "P", "S")),
    payoffs = list(rps_1, rps_2)
  )

  eq <- support_enumeration(g)
  expect_length(eq, 1)  # Exactly one NE

  p1_strat <- eq[[1]]$strategies[[1]]
  p2_strat <- eq[[1]]$strategies[[2]]

  expect_equal(p1_strat, c(1/3, 1/3, 1/3), tolerance = 1e-8)
  expect_equal(p2_strat, c(1/3, 1/3, 1/3), tolerance = 1e-8)
  expect_equal(eq[[1]]$payoffs, c(0, 0), tolerance = 1e-8)
})

test_that("support_enum: 3x3 game with known mixed NE", {
  # Game with a known completely mixed NE.
  # A =  [2  0  1]    B =  [1  0  2]
  #      [0  2  1]         [0  2  1]
  #      [1  1  0]         [2  1  0]
  # Player 1 indifference: A*q = v*1
  # Player 2 indifference: B^T*p = v*1
  A <- matrix(c(2, 0, 1,
                0, 2, 1,
                1, 1, 0), nrow = 3, byrow = TRUE)
  B <- matrix(c(1, 0, 2,
                0, 2, 1,
                2, 1, 0), nrow = 3, byrow = TRUE)

  g <- normal_form(
    players = c("P1", "P2"),
    strategies = list(c("A", "B", "C"), c("A", "B", "C")),
    payoffs = list(A, B)
  )

  eq <- support_enumeration(g)

  # There should be at least one equilibrium found

  expect_true(length(eq) >= 1)

  # Each equilibrium should have valid probability distributions
  for (e in eq) {
    expect_equal(sum(e$strategies[[1]]), 1, tolerance = 1e-8)
    expect_equal(sum(e$strategies[[2]]), 1, tolerance = 1e-8)
    expect_true(all(e$strategies[[1]] >= -1e-10))
    expect_true(all(e$strategies[[2]] >= -1e-10))
  }
})

test_that("support_enum: Battle of Sexes finds all 3 NE", {
  g <- battle_of_sexes()
  eq <- support_enumeration(g)

  # BoS has 3 NE: two pure (A,A) and (B,B), one mixed
  expect_equal(length(eq), 3)

  # Verify the mixed NE probabilities
  # Player 1: prob(A) = 3/5, prob(B) = 2/5
  # Player 2: prob(A) = 2/5, prob(B) = 3/5
  mixed_eq <- NULL
  for (e in eq) {
    if (all(e$strategies[[1]] > 0.01) && all(e$strategies[[1]] < 0.99)) {
      mixed_eq <- e
      break
    }
  }
  expect_false(is.null(mixed_eq))
  expect_equal(mixed_eq$strategies[[1]][1], 3/5, tolerance = 1e-8)
  expect_equal(mixed_eq$strategies[[2]][1], 2/5, tolerance = 1e-8)
})

test_that("support_enum: Matching Pennies unique mixed NE at (1/2, 1/2)", {
  g <- matching_pennies()
  eq <- support_enumeration(g)

  expect_length(eq, 1)
  expect_equal(eq[[1]]$strategies[[1]], c(0.5, 0.5), tolerance = 1e-8)
  expect_equal(eq[[1]]$strategies[[2]], c(0.5, 0.5), tolerance = 1e-8)
  expect_equal(eq[[1]]$payoffs, c(0, 0), tolerance = 1e-8)
})

test_that("support_enum: asymmetric 3x2 game", {
  # A game where player 1 has 3 strategies and player 2 has 2.
  # Support enumeration only considers equal-size supports, so it will
  # find equilibria with support sizes (1,1) or (2,2), but NOT (3,2) etc.
  # This tests whether the algorithm handles non-square payoff matrices.
  A <- matrix(c(3, 0,
                0, 3,
                1, 1), nrow = 3, byrow = TRUE)
  B <- matrix(c(3, 0,
                0, 3,
                1, 1), nrow = 3, byrow = TRUE)

  g <- normal_form(
    players = c("P1", "P2"),
    strategies = list(c("T", "M", "B"), c("L", "R")),
    payoffs = list(A, B)
  )

  eq <- support_enumeration(g)
  # Should find at least the two pure NE: (T,L) and (M,R)
  expect_true(length(eq) >= 2)
})

test_that("support_enum: degenerate game with multiple equilibria on same support", {
  # A game where indifference conditions hold for a continuum of equilibria.
  # This is a degenerate game: e.g., both players get the same payoff everywhere.
  A <- matrix(c(1, 1,
                1, 1), nrow = 2, byrow = TRUE)
  B <- matrix(c(1, 1,
                1, 1), nrow = 2, byrow = TRUE)

  g <- normal_form(
    players = c("P1", "P2"),
    strategies = list(c("U", "D"), c("L", "R")),
    payoffs = list(A, B)
  )

  # Every strategy profile is a NE. The support enumeration should
  # find equilibria (at least the pure ones).
  eq <- support_enumeration(g)
  expect_true(length(eq) >= 1)
})


# ---------------------------------------------------------------------------
# 3. Lemke-Howson correctness
# ---------------------------------------------------------------------------

test_that("lemke_howson: Prisoners Dilemma finds (D,D)", {
  g <- prisoners_dilemma()
  result <- lemke_howson(g)

  # PD has unique NE: (D,D) with payoffs (1,1)
  expect_equal(result$strategies[[1]], c(0, 1), tolerance = 1e-8)
  expect_equal(result$strategies[[2]], c(0, 1), tolerance = 1e-8)
  expect_equal(result$payoffs, c(1, 1), tolerance = 1e-8)
})

test_that("lemke_howson: Matching Pennies finds (1/2, 1/2)", {
  g <- matching_pennies()
  result <- lemke_howson(g)

  expect_equal(result$strategies[[1]], c(0.5, 0.5), tolerance = 1e-6)
  expect_equal(result$strategies[[2]], c(0.5, 0.5), tolerance = 1e-6)
  expect_equal(result$payoffs, c(0, 0), tolerance = 1e-6)
})

test_that("lemke_howson: 3x3 RPS finds (1/3, 1/3, 1/3)", {
  rps_1 <- matrix(c( 0, -1,  1,
                      1,  0, -1,
                     -1,  1,  0), nrow = 3, byrow = TRUE)
  rps_2 <- matrix(c( 0,  1, -1,
                     -1,  0,  1,
                      1, -1,  0), nrow = 3, byrow = TRUE)

  g <- normal_form(
    players = c("P1", "P2"),
    strategies = list(c("R", "P", "S"), c("R", "P", "S")),
    payoffs = list(rps_1, rps_2)
  )

  result <- lemke_howson(g)
  expect_equal(result$strategies[[1]], c(1/3, 1/3, 1/3), tolerance = 1e-6)
  expect_equal(result$strategies[[2]], c(1/3, 1/3, 1/3), tolerance = 1e-6)
  expect_equal(result$payoffs, c(0, 0), tolerance = 1e-6)
})

test_that("lemke_howson: game with negative payoffs returns correct shifted payoffs", {
  # A simple game with all-negative payoffs
  A <- matrix(c(-3, -1,
                -1, -3), nrow = 2, byrow = TRUE)
  B <- matrix(c(-3, -1,
                -1, -3), nrow = 2, byrow = TRUE)

  g <- normal_form(
    players = c("P1", "P2"),
    strategies = list(c("U", "D"), c("L", "R")),
    payoffs = list(A, B)
  )

  result <- lemke_howson(g)

  # This is a symmetric coordination game (in negative):
  # Pure NE: (U,L) with (-3,-3) and (D,R) with (-3,-3), plus mixed NE at (1/2,1/2) with payoff -2
  # Lemke-Howson should find one of these.
  # Verify the result is a valid NE by checking:
  # 1) Strategies are valid probability distributions
  expect_equal(sum(result$strategies[[1]]), 1, tolerance = 1e-8)
  expect_equal(sum(result$strategies[[2]]), 1, tolerance = 1e-8)
  expect_true(all(result$strategies[[1]] >= -1e-8))
  expect_true(all(result$strategies[[2]] >= -1e-8))

  # 2) Expected payoffs are correct given the strategies
  p <- result$strategies[[1]]
  q <- result$strategies[[2]]
  expected_pay1 <- t(p) %*% A %*% q
  expected_pay2 <- t(p) %*% B %*% q
  expect_equal(result$payoffs[1], as.numeric(expected_pay1), tolerance = 1e-6)
  expect_equal(result$payoffs[2], as.numeric(expected_pay2), tolerance = 1e-6)
})

test_that("lemke_howson: different init_labels can find different equilibria", {
  g <- battle_of_sexes()
  m <- g$n_strategies[1]
  n <- g$n_strategies[2]

  results <- list()
  for (lbl in seq_len(m + n)) {
    results[[lbl]] <- lemke_howson(g, init_label = lbl)
  }

  # BoS has 3 NE. Verify at least the results are all valid NE.
  for (r in results) {
    expect_equal(sum(r$strategies[[1]]), 1, tolerance = 1e-8)
    expect_equal(sum(r$strategies[[2]]), 1, tolerance = 1e-8)
  }
})

test_that("lemke_howson: strategy vectors have correct lengths", {
  # 3x2 game
  A <- matrix(c(3, 0,
                0, 3,
                1, 1), nrow = 3, byrow = TRUE)
  B <- matrix(c(3, 0,
                0, 3,
                1, 1), nrow = 3, byrow = TRUE)

  g <- normal_form(
    players = c("P1", "P2"),
    strategies = list(c("T", "M", "B"), c("L", "R")),
    payoffs = list(A, B)
  )

  result <- lemke_howson(g)

  # Player 1 should have 3 probabilities, Player 2 should have 2
  expect_length(result$strategies[[1]], 3)
  expect_length(result$strategies[[2]], 2)
})


# ---------------------------------------------------------------------------
# 4. Property functions
# ---------------------------------------------------------------------------

test_that("is_symmetric: returns FALSE for non-square 3x2 game", {
  A <- matrix(c(1, 2, 3, 4, 5, 6), nrow = 3, byrow = TRUE)
  B <- matrix(c(6, 5, 4, 3, 2, 1), nrow = 3, byrow = TRUE)

  g <- normal_form(
    players = c("P1", "P2"),
    strategies = list(c("A", "B", "C"), c("X", "Y")),
    payoffs = list(A, B)
  )

  expect_false(is_symmetric(g))
})

test_that("is_symmetric: coordination game is symmetric", {
  g <- coordination_game()
  expect_true(is_symmetric(g))
})

test_that("is_symmetric: BoS is NOT symmetric", {
  g <- battle_of_sexes()
  # Player 1 prefers (A,A)=3, Player 2 prefers (B,B)=3
  # u1(A,B)=0 but u2(B,A)=0; u1(A,A)=3 but u2(A,A)=2
  # So u1(1,1)=3 != u2(1,1)=2, NOT symmetric
  expect_false(is_symmetric(g))
})

test_that("is_zero_sum: RPS is zero-sum", {
  rps_1 <- matrix(c( 0, -1,  1,
                      1,  0, -1,
                     -1,  1,  0), nrow = 3, byrow = TRUE)
  rps_2 <- matrix(c( 0,  1, -1,
                     -1,  0,  1,
                      1, -1,  0), nrow = 3, byrow = TRUE)

  g <- normal_form(
    players = c("P1", "P2"),
    strategies = list(c("R", "P", "S"), c("R", "P", "S")),
    payoffs = list(rps_1, rps_2)
  )

  expect_true(is_zero_sum(g))
})

test_that("is_zero_sum: PD is not zero-sum", {
  g <- prisoners_dilemma()
  expect_false(is_zero_sum(g))
})

test_that("dominant_strategy: PD correctly identifies D as dominant", {
  g <- prisoners_dilemma()
  dom <- dominant_strategy(g)
  expect_equal(dom[[1]], "D")
  expect_equal(dom[[2]], "D")
})

test_that("dominant_strategy: no dominant strategy in BoS", {
  g <- battle_of_sexes()
  dom <- dominant_strategy(g)
  expect_null(dom[[1]])
  expect_null(dom[[2]])
})

test_that("dominant_strategy: weak dominance is NOT detected (strict only)", {
  # Game where U weakly dominates D:
  #        L     R
  # U   (2,0)  (1,0)
  # D   (2,0)  (0,0)
  # U weakly dominates D (equal at L, strictly better at R)
  # But dominant_strategy uses strict dominance, so no dominant strategy.
  A <- matrix(c(2, 1, 2, 0), nrow = 2, byrow = TRUE)
  B <- matrix(c(0, 0, 0, 0), nrow = 2, byrow = TRUE)

  g <- normal_form(
    players = c("P1", "P2"),
    strategies = list(c("U", "D"), c("L", "R")),
    payoffs = list(A, B)
  )

  dom <- dominant_strategy(g)
  # Under strict dominance, U does NOT strictly dominate D
  # because at profile (U,L) vs (D,L), payoffs are equal (2 vs 2).
  expect_null(dom[[1]])
})

test_that("best_response: pure strategy opponent", {
  g <- prisoners_dilemma()

  # If Col plays L (C) with certainty, Row's BR should be D (index 2)
  br <- best_response(g, player = 1L, opponent_strategy = c(1, 0))
  expect_equal(br, 2L)

  # If Col plays R (D) with certainty, Row's BR should still be D
  br <- best_response(g, player = 1L, opponent_strategy = c(0, 1))
  expect_equal(br, 2L)
})

test_that("best_response: mixed strategy opponent in BoS", {
  g <- battle_of_sexes()

  # If Player 2 plays uniform (0.5, 0.5):
  # EP(A) for P1 = 0.5 * 3 + 0.5 * 0 = 1.5
  # EP(B) for P1 = 0.5 * 0 + 0.5 * 2 = 1.0
  # BR = {A} = {1}
  br <- best_response(g, player = 1L, opponent_strategy = c(0.5, 0.5))
  expect_equal(br, 1L)
})

test_that("best_response: multiple best responses (tie)", {
  # Matching pennies: against (0.5, 0.5), both strategies are equally good
  g <- matching_pennies()
  br <- best_response(g, player = 1L, opponent_strategy = c(0.5, 0.5))
  expect_equal(sort(br), c(1L, 2L))
})

test_that("best_response: player 2 perspective", {
  g <- prisoners_dilemma()
  # Row plays C with certainty. Col's BR should be D.
  br <- best_response(g, player = 2L, opponent_strategy = c(1, 0))
  expect_equal(br, 2L)
})

test_that("is_pareto_optimal: mutual cooperation in PD is Pareto optimal", {
  g <- prisoners_dilemma()
  # (C,C) = (3,3) is Pareto optimal (no profile makes both better)
  expect_true(is_pareto_optimal(g, c(1, 1)))
})

test_that("is_pareto_optimal: mutual defection in PD is NOT Pareto optimal", {
  g <- prisoners_dilemma()
  # (D,D) = (1,1) is Pareto dominated by (C,C) = (3,3)
  expect_false(is_pareto_optimal(g, c(2, 2)))
})


# ---------------------------------------------------------------------------
# 5. Nash equilibria dispatch
# ---------------------------------------------------------------------------

test_that("nash_equilibria method='pure' converts to mixed format correctly", {
  g <- prisoners_dilemma()
  eq <- nash_equilibria(g, method = "pure")

  # PD has one pure NE: (D, D)
  expect_length(eq, 1)
  expect_equal(eq[[1]]$strategies[[1]], c(0, 1))  # prob 1 on D
  expect_equal(eq[[1]]$strategies[[2]], c(0, 1))  # prob 1 on D
  expect_equal(eq[[1]]$payoffs, c(1, 1))
})

test_that("nash_equilibria method='pure' finds all pure NE in BoS", {
  g <- battle_of_sexes()
  eq <- nash_equilibria(g, method = "pure")

  # BoS has 2 pure NE: (A,A) and (B,B)
  expect_length(eq, 2)

  # Verify both are present in mixed strategy format
  profiles <- lapply(eq, function(e) {
    c(which.max(e$strategies[[1]]), which.max(e$strategies[[2]]))
  })
  expect_true(list(c(1, 1)) %in% profiles || any(sapply(profiles, function(p) identical(p, c(1L, 1L)))))
  expect_true(any(sapply(profiles, function(p) identical(p, c(2L, 2L)))))
})

test_that("nash_equilibria method='pure' strategy vectors have correct length", {
  # 3x2 game
  A <- matrix(c(3, 0,
                0, 3,
                1, 1), nrow = 3, byrow = TRUE)
  B <- matrix(c(3, 0,
                0, 3,
                1, 1), nrow = 3, byrow = TRUE)

  g <- normal_form(
    players = c("P1", "P2"),
    strategies = list(c("T", "M", "B"), c("L", "R")),
    payoffs = list(A, B)
  )

  eq <- nash_equilibria(g, method = "pure")
  for (e in eq) {
    expect_length(e$strategies[[1]], 3)  # Player 1 has 3 strategies
    expect_length(e$strategies[[2]], 2)  # Player 2 has 2 strategies
    expect_equal(sum(e$strategies[[1]]), 1)
    expect_equal(sum(e$strategies[[2]]), 1)
  }
})

test_that("nash_equilibria auto method uses support for 2-player", {
  g <- matching_pennies()
  eq <- nash_equilibria(g, method = "auto")

  # Should find the mixed NE via support enumeration
  expect_length(eq, 1)
  expect_equal(eq[[1]]$strategies[[1]], c(0.5, 0.5), tolerance = 1e-8)
})


# ---------------------------------------------------------------------------
# 6. N-player pure Nash
# ---------------------------------------------------------------------------

test_that("pure_nash: 3-player game with known equilibria", {
  # 3-player game. Each player has 2 strategies.
  # Payoff array dimensions: 2 x 2 x 2 x 3
  # We design it so that (1,1,1) is a NE: no player can deviate profitably.
  payoffs <- array(0, dim = c(2, 2, 2, 3))

  # Set payoffs so that (1,1,1) => (3,3,3) is a NE
  # At (1,1,1): all get 3
  payoffs[1, 1, 1, ] <- c(3, 3, 3)
  # If player 1 deviates to 2: gets 2
  payoffs[2, 1, 1, ] <- c(2, 4, 4)
  # If player 2 deviates to 2: gets 2
  payoffs[1, 2, 1, ] <- c(4, 2, 4)
  # If player 3 deviates to 2: gets 2
  payoffs[1, 1, 2, ] <- c(4, 4, 2)
  # Other profiles
  payoffs[2, 2, 1, ] <- c(1, 1, 5)
  payoffs[2, 1, 2, ] <- c(1, 5, 1)
  payoffs[1, 2, 2, ] <- c(5, 1, 1)
  payoffs[2, 2, 2, ] <- c(0, 0, 0)

  g <- NormalFormGame$new(
    players = c("P1", "P2", "P3"),
    strategies = list(c("A", "B"), c("A", "B"), c("A", "B")),
    payoffs = payoffs
  )

  eq <- pure_nash(g)

  # (1,1,1) should be a NE
  profiles <- lapply(eq, function(e) e$profile)
  found <- any(sapply(profiles, function(p) identical(p, c(1L, 1L, 1L))))
  expect_true(found)
})

test_that("pure_nash: 3-player game pure NE payoffs are correct", {
  # Simple 3-player game where (2,2,2) is the unique NE
  payoffs <- array(0, dim = c(2, 2, 2, 3))
  # Make (2,2,2) a strict NE with payoff (5,5,5)
  payoffs[2, 2, 2, ] <- c(5, 5, 5)
  # Any deviation from (2,2,2) gives deviator payoff 0
  payoffs[1, 2, 2, ] <- c(0, 6, 6)
  payoffs[2, 1, 2, ] <- c(6, 0, 6)
  payoffs[2, 2, 1, ] <- c(6, 6, 0)
  # All other profiles give 1 to everyone (making deviations from them tempting)
  payoffs[1, 1, 1, ] <- c(1, 1, 1)
  payoffs[2, 1, 1, ] <- c(3, 0, 0)  # P1 deviates from (1,1,1) and gains
  payoffs[1, 2, 1, ] <- c(0, 3, 0)
  payoffs[1, 1, 2, ] <- c(0, 0, 3)

  g <- NormalFormGame$new(
    players = c("P1", "P2", "P3"),
    strategies = list(c("A", "B"), c("A", "B"), c("A", "B")),
    payoffs = payoffs
  )

  eq <- pure_nash(g)
  profiles <- lapply(eq, function(e) e$profile)

  # (2,2,2) should be a NE
  found_222 <- any(sapply(profiles, function(p) identical(p, c(2L, 2L, 2L))))
  expect_true(found_222)

  # (1,1,1) should NOT be a NE (each player can deviate and gain)
  found_111 <- any(sapply(profiles, function(p) identical(p, c(1L, 1L, 1L))))
  expect_false(found_111)
})

test_that("nash_equilibria method='pure' works for 3-player games", {
  payoffs <- array(0, dim = c(2, 2, 2, 3))
  payoffs[1, 1, 1, ] <- c(3, 3, 3)
  payoffs[2, 1, 1, ] <- c(2, 4, 4)
  payoffs[1, 2, 1, ] <- c(4, 2, 4)
  payoffs[1, 1, 2, ] <- c(4, 4, 2)
  payoffs[2, 2, 1, ] <- c(1, 1, 5)
  payoffs[2, 1, 2, ] <- c(1, 5, 1)
  payoffs[1, 2, 2, ] <- c(5, 1, 1)
  payoffs[2, 2, 2, ] <- c(0, 0, 0)

  g <- NormalFormGame$new(
    players = c("P1", "P2", "P3"),
    strategies = list(c("A", "B"), c("A", "B"), c("A", "B")),
    payoffs = payoffs
  )

  # method='auto' should fall back to 'pure' for 3-player games
  eq <- nash_equilibria(g, method = "auto")

  # Should return results in mixed strategy format with 3 players
  for (e in eq) {
    expect_length(e$strategies, 3)
    for (p in 1:3) {
      expect_length(e$strategies[[p]], 2)
      expect_equal(sum(e$strategies[[p]]), 1)
    }
  }
})


# ---------------------------------------------------------------------------
# 7. Prisoners Dilemma constructor payoff verification
# ---------------------------------------------------------------------------

test_that("prisoners_dilemma: all four outcomes have correct payoffs", {
  g <- prisoners_dilemma(R = 3, T = 5, S = 0, P = 1)

  # (C,C) -> (R,R) = (3,3)
  expect_equal(g$payoff(c(1, 1)), c(3, 3))

  # (C,D) -> (S,T) = (0,5): Row cooperates, Col defects
  # Row gets Sucker=0, Col gets Temptation=5
  expect_equal(g$payoff(c(1, 2)), c(0, 5))

  # (D,C) -> (T,S) = (5,0): Row defects, Col cooperates
  # Row gets Temptation=5, Col gets Sucker=0
  expect_equal(g$payoff(c(2, 1)), c(5, 0))

  # (D,D) -> (P,P) = (1,1)
  expect_equal(g$payoff(c(2, 2)), c(1, 1))
})


# ---------------------------------------------------------------------------
# 8. Edge cases and additional probes
# ---------------------------------------------------------------------------

test_that("support_enum: game with no pure NE still finds mixed NE", {
  # Matching pennies: no pure NE, but (0.5, 0.5) is mixed NE
  g <- matching_pennies()
  pure_eq <- pure_nash(g)
  expect_length(pure_eq, 0)

  mixed_eq <- support_enumeration(g)
  expect_length(mixed_eq, 1)
})

test_that("support_enum: coordination game finds both pure and mixed NE", {
  g <- coordination_game()
  eq <- support_enumeration(g)

  # Coordination game: 2 pure NE + 1 mixed NE = 3 total
  expect_equal(length(eq), 3)
})

test_that("hawk_dove: payoffs match theoretical values", {
  g <- hawk_dove(V = 4, C = 6)

  # Hawk vs Hawk: ((V-C)/2, (V-C)/2) = (-1, -1)
  expect_equal(g$payoff(c(1, 1)), c(-1, -1))

  # Hawk vs Dove: (V, 0) = (4, 0)
  expect_equal(g$payoff(c(1, 2)), c(4, 0))

  # Dove vs Hawk: (0, V) = (0, 4)
  expect_equal(g$payoff(c(2, 1)), c(0, 4))

  # Dove vs Dove: (V/2, V/2) = (2, 2)
  expect_equal(g$payoff(c(2, 2)), c(2, 2))
})

test_that("lemke_howson: result is verified as NE via best_response", {
  g <- battle_of_sexes()
  result <- lemke_howson(g)

  p1 <- result$strategies[[1]]
  p2 <- result$strategies[[2]]

  # Check: every strategy in the support of player 1 must be a best response to p2
  support_1 <- which(p1 > 1e-8)
  br_1 <- best_response(g, player = 1L, opponent_strategy = p2)
  expect_true(all(support_1 %in% br_1))

  # Check: every strategy in the support of player 2 must be a best response to p1
  support_2 <- which(p2 > 1e-8)
  br_2 <- best_response(g, player = 2L, opponent_strategy = p1)
  expect_true(all(support_2 %in% br_2))
})

test_that("support_enum results are verified as NE via best_response", {
  g <- battle_of_sexes()
  eqs <- support_enumeration(g)

  for (eq in eqs) {
    p1 <- eq$strategies[[1]]
    p2 <- eq$strategies[[2]]

    # Every strategy in support of player 1 must be BR to p2
    support_1 <- which(p1 > 1e-8)
    br_1 <- best_response(g, player = 1L, opponent_strategy = p2)
    expect_true(all(support_1 %in% br_1))

    # Every strategy in support of player 2 must be BR to p1
    support_2 <- which(p2 > 1e-8)
    br_2 <- best_response(g, player = 2L, opponent_strategy = p1)
    expect_true(all(support_2 %in% br_2))
  }
})

test_that("support_enum payoffs match computed expected payoffs", {
  g <- battle_of_sexes()
  eqs <- support_enumeration(g)

  ns <- g$n_strategies
  for (eq in eqs) {
    p1 <- eq$strategies[[1]]
    p2 <- eq$strategies[[2]]

    # Compute expected payoffs from scratch
    A <- matrix(0, nrow = ns[1], ncol = ns[2])
    B <- matrix(0, nrow = ns[1], ncol = ns[2])
    for (i in seq_len(ns[1])) {
      for (j in seq_len(ns[2])) {
        pf <- g$payoff(c(i, j))
        A[i, j] <- pf[1]
        B[i, j] <- pf[2]
      }
    }

    expected_pay1 <- as.numeric(t(p1) %*% A %*% p2)
    expected_pay2 <- as.numeric(t(p1) %*% B %*% p2)

    expect_equal(eq$payoffs[1], expected_pay1, tolerance = 1e-8)
    expect_equal(eq$payoffs[2], expected_pay2, tolerance = 1e-8)
  }
})
