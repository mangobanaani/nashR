test_that("support_enumeration finds matching pennies mixed NE", {
  g <- matching_pennies()
  eq <- support_enumeration(g)
  expect_length(eq, 1)
  expect_equal(eq[[1]]$strategies[[1]], c(0.5, 0.5), tolerance = 1e-10)
  expect_equal(eq[[1]]$strategies[[2]], c(0.5, 0.5), tolerance = 1e-10)
})

test_that("support_enumeration finds BoS equilibria (pure + mixed)", {
  g <- battle_of_sexes()
  eq <- support_enumeration(g)
  expect_length(eq, 3)  # 2 pure + 1 mixed

  pure_profiles <- list()
  mixed_profiles <- list()
  for (e in eq) {
    if (all(e$strategies[[1]] %in% c(0, 1))) {
      pure_profiles[[length(pure_profiles) + 1]] <- e
    } else {
      mixed_profiles[[length(mixed_profiles) + 1]] <- e
    }
  }
  expect_length(pure_profiles, 2)
  expect_length(mixed_profiles, 1)

  # Mixed NE of BoS: p1 plays A with prob 3/5, p2 plays A with prob 2/5
  m <- mixed_profiles[[1]]
  expect_equal(m$strategies[[1]][1], 3 / 5, tolerance = 1e-10)
  expect_equal(m$strategies[[2]][1], 2 / 5, tolerance = 1e-10)
})

test_that("support_enumeration handles PD (single pure NE)", {
  g <- prisoners_dilemma()
  eq <- support_enumeration(g)
  expect_length(eq, 1)
  expect_equal(eq[[1]]$strategies[[1]], c(0, 1))  # (0, 1) = pure D
  expect_equal(eq[[1]]$strategies[[2]], c(0, 1))
})
