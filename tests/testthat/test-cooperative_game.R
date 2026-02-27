test_that("CooperativeGame stores players and value function", {
  g <- cooperative_game(
    players = c("A", "B", "C"),
    value = function(S) {
      if (length(S) == 0) return(0)
      if (setequal(S, c("A", "B", "C"))) return(10)
      if (setequal(S, c("A", "B"))) return(7)
      if (setequal(S, c("A", "C"))) return(5)
      if (setequal(S, c("B", "C"))) return(4)
      if ("A" %in% S) return(3)
      if ("B" %in% S) return(2)
      if ("C" %in% S) return(1)
      0
    }
  )

  expect_equal(g$n_players, 3)
  expect_equal(g$players, c("A", "B", "C"))
  expect_equal(g$value(c("A", "B", "C")), 10)
  expect_equal(g$value(c("A", "B")), 7)
  expect_equal(g$value("A"), 3)
  expect_equal(g$value(character(0)), 0)
})

test_that("CooperativeGame validates v(empty) = 0", {
  expect_error(
    cooperative_game(
      players = c("A", "B"),
      value = function(S) 1  # always returns 1, even for empty set
    ),
    "empty"
  )
})

test_that("is_superadditive checks superadditivity", {
  # Superadditive: v(S union T) >= v(S) + v(T) for disjoint S, T
  g <- cooperative_game(
    players = c("A", "B", "C"),
    value = function(S) {
      if (length(S) == 0) return(0)
      if (setequal(S, c("A", "B", "C"))) return(10)
      if (setequal(S, c("A", "B"))) return(7)
      if (setequal(S, c("A", "C"))) return(5)
      if (setequal(S, c("B", "C"))) return(4)
      if ("A" %in% S) return(3)
      if ("B" %in% S) return(2)
      if ("C" %in% S) return(1)
      0
    }
  )
  expect_true(is_superadditive(g))
})

test_that("is_superadditive detects non-superadditive games", {
  g <- cooperative_game(
    players = c("A", "B"),
    value = function(S) {
      if (length(S) == 0) return(0)
      if (setequal(S, c("A", "B"))) return(3)  # v(AB) < v(A) + v(B)
      if ("A" %in% S) return(5)
      if ("B" %in% S) return(5)
      0
    }
  )
  expect_false(is_superadditive(g))
})

test_that("CooperativeGame print method works", {
  g <- cooperative_game(
    players = c("A", "B"),
    value = function(S) length(S)
  )
  expect_output(print(g), "Cooperative Game")
})
