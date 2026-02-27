test_that("in_core checks core membership correctly", {
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

  # (4, 3, 3) sums to 10 = v(N)
  # Check coalitions: v(AB)=7 <= 4+3=7 OK, v(AC)=5 <= 4+3=7 OK,
  # v(BC)=4 <= 3+3=6 OK, v(A)=3 <= 4 OK, v(B)=2 <= 3 OK, v(C)=1 <= 3 OK
  alloc1 <- c(A = 4, B = 3, C = 3)
  expect_true(in_core(g, alloc1))

  # (8, 1, 1) sums to 10 = v(N)
  # v(BC) = 4 > 1+1 = 2 -- NOT in core
  alloc2 <- c(A = 8, B = 1, C = 1)
  expect_false(in_core(g, alloc2))
})

test_that("in_core rejects allocations not summing to v(N)", {
  g <- cooperative_game(
    players = c("A", "B"),
    value = function(S) {
      if (setequal(S, c("A", "B"))) return(10)
      0
    }
  )

  # Doesn't sum to 10
  expect_false(in_core(g, c(A = 3, B = 3)))
})

test_that("core_constraints returns coalition constraints", {
  g <- cooperative_game(
    players = c("A", "B", "C"),
    value = function(S) {
      if (length(S) == 0) return(0)
      if (setequal(S, c("A", "B", "C"))) return(6)
      if (length(S) == 2) return(4)
      if (length(S) == 1) return(0)
      0
    }
  )

  constraints <- core_constraints(g)
  # Should have 2^n - 2 = 6 constraints (all non-empty proper subsets)
  expect_equal(nrow(constraints$A), 6)
  expect_equal(length(constraints$b), 6)
})

test_that("nucleolus computes correctly for simple game", {
  # Simple 2-player game: v(A)=0, v(B)=0, v(AB)=10
  g <- cooperative_game(
    players = c("A", "B"),
    value = function(S) {
      if (setequal(S, c("A", "B"))) return(10)
      0
    }
  )

  nuc <- nucleolus(g)
  # Nucleolus should be (5, 5) by symmetry
  expect_equal(as.numeric(nuc["A"]), 5, tolerance = 1e-6)
  expect_equal(as.numeric(nuc["B"]), 5, tolerance = 1e-6)
  expect_equal(sum(nuc), 10, tolerance = 1e-6)
})

test_that("nucleolus is in the core when core is non-empty", {
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

  nuc <- nucleolus(g)
  expect_true(in_core(g, nuc))
  expect_equal(sum(nuc), 10, tolerance = 1e-6)
})
