test_that("lemke_howson finds matching pennies NE", {
  g <- matching_pennies()
  eq <- lemke_howson(g)
  expect_equal(eq$strategies[[1]], c(0.5, 0.5), tolerance = 1e-8)
  expect_equal(eq$strategies[[2]], c(0.5, 0.5), tolerance = 1e-8)
})

test_that("lemke_howson finds a BoS equilibrium", {
  g <- battle_of_sexes()
  eq <- lemke_howson(g)
  expect_length(eq$strategies, 2)
  expect_equal(sum(eq$strategies[[1]]), 1, tolerance = 1e-10)
  expect_equal(sum(eq$strategies[[2]]), 1, tolerance = 1e-10)
  expect_true(all(eq$strategies[[1]] >= -1e-10))
  expect_true(all(eq$strategies[[2]] >= -1e-10))
})

test_that("lemke_howson finds PD equilibrium", {
  g <- prisoners_dilemma()
  eq <- lemke_howson(g)
  # (D, D) = second strategy for both
  expect_equal(eq$strategies[[1]][2], 1, tolerance = 1e-10)
  expect_equal(eq$strategies[[2]][2], 1, tolerance = 1e-10)
})

test_that("lemke_howson with different starting labels finds different NE", {
  g <- battle_of_sexes()
  results <- list()
  for (label in 1:4) {
    eq <- lemke_howson(g, init_label = label)
    results[[label]] <- eq
  }
  distinct <- unique(lapply(results, function(e) round(e$strategies[[1]], 6)))
  expect_true(length(distinct) >= 2)
})

test_that("lemke_howson returns payoffs consistent with strategies", {
  g <- matching_pennies()
  eq <- lemke_howson(g)
  # In matching pennies, expected payoff at mixed NE is 0 for both
  expect_equal(eq$payoffs[1], 0, tolerance = 1e-8)
  expect_equal(eq$payoffs[2], 0, tolerance = 1e-8)
})

test_that("lemke_howson rejects non-2-player games", {
  expect_error(lemke_howson(list()), "NormalFormGame")
})

test_that("lemke_howson rejects invalid init_label", {
  g <- matching_pennies()
  expect_error(lemke_howson(g, init_label = 0L))
  expect_error(lemke_howson(g, init_label = 5L))
})
