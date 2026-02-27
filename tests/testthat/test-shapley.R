test_that("shapley_value computes correctly for 3-player game", {
  # Classic example: v(A)=0, v(B)=0, v(C)=0, v(AB)=6, v(AC)=6, v(BC)=0, v(ABC)=12
  g <- cooperative_game(
    players = c("A", "B", "C"),
    value = function(S) {
      if (length(S) == 0) return(0)
      if (setequal(S, c("A", "B", "C"))) return(12)
      if (setequal(S, c("A", "B"))) return(6)
      if (setequal(S, c("A", "C"))) return(6)
      if (setequal(S, c("B", "C"))) return(0)
      0  # singletons
    }
  )

  phi <- shapley_value(g)
  # Shapley values should sum to v(N) = 12
  expect_equal(sum(phi), 12, tolerance = 1e-10)
  # By symmetry of B and C: phi_B = phi_C
  expect_equal(unname(phi["B"]), unname(phi["C"]), tolerance = 1e-10)
  # A contributes more: phi_A > phi_B
  expect_true(phi["A"] > phi["B"])
})

test_that("shapley_value for glove game", {
  # Left-right glove game: L has 1 left glove, R1 and R2 have right gloves
  # v(S) = min(#left in S, #right in S)
  g <- cooperative_game(
    players = c("L", "R1", "R2"),
    value = function(S) {
      if (length(S) == 0) return(0)
      left <- sum(S == "L")
      right <- sum(S %in% c("R1", "R2"))
      min(left, right)
    }
  )

  phi <- shapley_value(g)
  expect_equal(sum(phi), 1, tolerance = 1e-10)  # v(N) = 1
  # L has more power (scarce resource)
  expect_true(phi["L"] > phi["R1"])
  # R1 and R2 are symmetric
  expect_equal(unname(phi["R1"]), unname(phi["R2"]), tolerance = 1e-10)
  # Known values: phi_L = 2/3, phi_R1 = phi_R2 = 1/6
  expect_equal(as.numeric(phi["L"]), 2/3, tolerance = 1e-10)
  expect_equal(as.numeric(phi["R1"]), 1/6, tolerance = 1e-10)
})

test_that("shapley_value for unanimous game", {
  # Unanimous game: v(S) = 1 if all players in S, 0 otherwise
  g <- cooperative_game(
    players = c("A", "B"),
    value = function(S) {
      if (setequal(S, c("A", "B"))) return(1)
      0
    }
  )

  phi <- shapley_value(g)
  expect_equal(as.numeric(phi["A"]), 0.5, tolerance = 1e-10)
  expect_equal(as.numeric(phi["B"]), 0.5, tolerance = 1e-10)
})

test_that("shapley_value sums to v(N)", {
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

  phi <- shapley_value(g)
  expect_equal(sum(phi), 10, tolerance = 1e-10)
  # Named vector
  expect_equal(names(phi), c("A", "B", "C"))
})
