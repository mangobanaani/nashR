test_that("vcg_mechanism computes correct payments for single item", {
  # Single item auction with 3 bidders
  # Values: bidder 1 = 10, bidder 2 = 8, bidder 3 = 5
  valuations <- list(
    bidder1 = c(item = 10),
    bidder2 = c(item = 8),
    bidder3 = c(item = 5)
  )

  result <- vcg_mechanism(
    valuations = valuations,
    allocation_rule = function(vals) {
      # Efficient allocation: give to highest value bidder
      winner <- which.max(sapply(vals, function(v) v["item"]))
      alloc <- rep(0, length(vals))
      alloc[winner] <- 1
      alloc
    }
  )

  # VCG allocation: item goes to bidder 1
  expect_equal(result$allocation, c(1, 0, 0))
  # VCG payment for bidder 1: externality they impose
  # Without bidder 1, item goes to bidder 2 (value 8)
  # With bidder 1, bidder 2 gets 0
  # Payment = 8 (the second-highest value)
  expect_equal(result$payments[1], 8)
  # Non-winners pay 0
  expect_equal(result$payments[2], 0)
  expect_equal(result$payments[3], 0)
})

test_that("vcg_mechanism computes correct payments for multi-item", {
  # 2 items, 3 bidders
  valuations <- list(
    bidder1 = c(item_a = 10, item_b = 5),
    bidder2 = c(item_a = 8, item_b = 12),
    bidder3 = c(item_a = 3, item_b = 7)
  )

  result <- vcg_mechanism(
    valuations = valuations,
    allocation_rule = function(vals) {
      # Simple: assign each item to highest-value bidder
      n_items <- length(vals[[1]])
      alloc <- matrix(0, nrow = length(vals), ncol = n_items)
      for (j in seq_len(n_items)) {
        item_vals <- sapply(vals, function(v) v[j])
        winner <- which.max(item_vals)
        alloc[winner, j] <- 1
      }
      alloc
    }
  )

  # Item A -> bidder 1 (value 10), Item B -> bidder 2 (value 12)
  expect_equal(result$allocation[1, ], c(1, 0))
  expect_equal(result$allocation[2, ], c(0, 1))
  expect_equal(result$allocation[3, ], c(0, 0))
})

test_that("is_incentive_compatible checks truthfulness", {
  # A mechanism that charges the reported value (first-price) is NOT IC
  mechanism <- list(
    allocation = function(reports) {
      winner <- which.max(reports)
      alloc <- rep(0, length(reports))
      alloc[winner] <- 1
      alloc
    },
    payment = function(reports) {
      # First-price: pay your bid
      alloc <- rep(0, length(reports))
      winner <- which.max(reports)
      alloc[winner] <- reports[winner]
      alloc
    }
  )

  # Not IC because bidders have incentive to shade bids
  expect_false(is_incentive_compatible(
    mechanism,
    n_agents = 2,
    type_space = seq(0, 10, by = 1)
  ))
})

test_that("is_incentive_compatible confirms Vickrey is IC", {
  mechanism <- list(
    allocation = function(reports) {
      winner <- which.max(reports)
      alloc <- rep(0, length(reports))
      alloc[winner] <- 1
      alloc
    },
    payment = function(reports) {
      n <- length(reports)
      pays <- rep(0, n)
      winner <- which.max(reports)
      # Pay second-highest bid
      pays[winner] <- sort(reports, decreasing = TRUE)[2]
      pays
    }
  )

  expect_true(is_incentive_compatible(
    mechanism,
    n_agents = 2,
    type_space = seq(0, 10, by = 2)
  ))
})

test_that("is_individually_rational checks participation", {
  # A mechanism where everyone pays 5 regardless
  mechanism <- list(
    allocation = function(reports) rep(0, length(reports)),
    payment = function(reports) rep(5, length(reports))
  )

  # Not IR: agents with value < 5 get negative utility
  expect_false(is_individually_rational(
    mechanism,
    n_agents = 2,
    type_space = seq(0, 10, by = 2)
  ))
})

test_that("is_individually_rational confirms VCG is IR", {
  mechanism <- list(
    allocation = function(reports) {
      winner <- which.max(reports)
      alloc <- rep(0, length(reports))
      alloc[winner] <- 1
      alloc
    },
    payment = function(reports) {
      n <- length(reports)
      pays <- rep(0, n)
      winner <- which.max(reports)
      pays[winner] <- sort(reports, decreasing = TRUE)[2]
      pays
    }
  )

  expect_true(is_individually_rational(
    mechanism,
    n_agents = 2,
    type_space = seq(0, 10, by = 2)
  ))
})
