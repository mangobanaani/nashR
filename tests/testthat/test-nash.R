test_that("nash_equilibria auto-selects method for 2-player game", {
  g <- matching_pennies()
  eq <- nash_equilibria(g)
  expect_length(eq, 1)
  expect_equal(eq[[1]]$strategies[[1]], c(0.5, 0.5), tolerance = 1e-10)
})

test_that("nash_equilibria with method='pure' returns only pure NE", {
  g <- battle_of_sexes()
  eq <- nash_equilibria(g, method = "pure")
  expect_length(eq, 2)
  for (e in eq) {
    for (s in e$strategies) {
      expect_true(all(s %in% c(0, 1)))
    }
  }
})

test_that("nash_equilibria with method='support' works", {
  g <- prisoners_dilemma()
  eq <- nash_equilibria(g, method = "support")
  expect_length(eq, 1)
})

test_that("print_equilibria outputs formatted text", {
  g <- battle_of_sexes()
  eq <- nash_equilibria(g)
  expect_output(print_equilibria(eq, g), "Nash Equilibri")
})

test_that("nash_equilibria errors on unknown method", {
  g <- matching_pennies()
  expect_error(nash_equilibria(g, method = "unknown"))
})

test_that("nash_equilibria method='pure' converts profiles to mixed strategy format", {
  g <- prisoners_dilemma()
  eq <- nash_equilibria(g, method = "pure")
  expect_length(eq, 1)
  # PD has unique pure NE: (D, D) -> strategies should be c(0, 1) for both
  expect_equal(eq[[1]]$strategies[[1]], c(0, 1))
  expect_equal(eq[[1]]$strategies[[2]], c(0, 1))
  # Payoffs should be present
  expect_length(eq[[1]]$payoffs, 2)
})

test_that("nash_equilibria method='support' errors for non-2-player game", {
  # Create a 3-player game
  payoffs <- array(1, dim = c(2, 2, 2, 3))
  g <- NormalFormGame$new(
    players = c("A", "B", "C"),
    strategies = list(c("X", "Y"), c("X", "Y"), c("X", "Y")),
    payoffs = payoffs
  )
  expect_error(nash_equilibria(g, method = "support"))
})
