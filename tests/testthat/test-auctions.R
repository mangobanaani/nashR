test_that("first_price_auction creates game with correct structure", {
  g <- first_price_auction(
    n_bidders = 2,
    values = c(50, 80),
    bid_levels = seq(0, 100, by = 10)
  )

  expect_s3_class(g, "NormalFormGame")
  expect_equal(g$n_players, 2)
  expect_equal(g$n_strategies, c(11, 11))
})

test_that("first_price_auction payoffs are correct", {
  g <- first_price_auction(
    n_bidders = 2,
    values = c(100, 80),
    bid_levels = c(0, 40, 60, 80, 100)
  )

  # If bidder 1 bids 60 (idx 3), bidder 2 bids 40 (idx 2):
  # Bidder 1 wins, pays 60, profit = 100 - 60 = 40
  # Bidder 2 loses, profit = 0
  p <- g$payoff(c(3, 2))
  expect_equal(p[1], 40)
  expect_equal(p[2], 0)

  # If both bid 60 (tie): split equally
  p <- g$payoff(c(3, 3))
  expect_equal(p[1], (100 - 60) / 2)  # 20
  expect_equal(p[2], (80 - 60) / 2)   # 10
})

test_that("second_price_auction payoffs follow Vickrey rule", {
  g <- second_price_auction(
    n_bidders = 2,
    values = c(100, 80),
    bid_levels = c(0, 40, 60, 80, 100)
  )

  # If bidder 1 bids 80 (idx 4), bidder 2 bids 60 (idx 3):
  # Bidder 1 wins, pays second-highest bid = 60
  # Profit = 100 - 60 = 40
  p <- g$payoff(c(4, 3))
  expect_equal(p[1], 40)
  expect_equal(p[2], 0)
})

test_that("truthful bidding is dominant in second_price_auction", {
  g <- second_price_auction(
    n_bidders = 2,
    values = c(100, 80),
    bid_levels = c(0, 20, 40, 60, 80, 100)
  )

  ds <- dominant_strategy(g)
  # In second-price auction, bidding true value is weakly dominant
  # Value 100 is at bid level index 6 (100), value 80 is at index 5 (80)
  # dominant_strategy finds strictly dominant, so this may return NULL
  # But let's check that truthful bidding gives highest expected payoff
  # against any opponent strategy
  value_idx_1 <- which(c(0, 20, 40, 60, 80, 100) == 100)
  value_idx_2 <- which(c(0, 20, 40, 60, 80, 100) == 80)

  # For bidder 1: check that bidding 100 is at least as good as any other bid
  # against each possible opponent bid
  for (opp_bid in 1:6) {
    truthful <- g$payoff(c(value_idx_1, opp_bid))[1]
    for (my_bid in 1:6) {
      alternative <- g$payoff(c(my_bid, opp_bid))[1]
      expect_true(truthful >= alternative - 1e-10)
    }
  }
})

test_that("all_pay_auction has correct payoffs", {
  g <- all_pay_auction(
    n_bidders = 2,
    values = c(100, 80),
    bid_levels = c(0, 20, 40, 60)
  )

  # Both pay their bids regardless of outcome
  # If bidder 1 bids 40 (idx 3), bidder 2 bids 20 (idx 2):
  # Bidder 1 wins: profit = 100 - 40 = 60
  # Bidder 2 loses: profit = 0 - 20 = -20
  p <- g$payoff(c(3, 2))
  expect_equal(p[1], 60)
  expect_equal(p[2], -20)
})

test_that("optimal_bid_first_price computes optimal bid for uniform values", {
  # With n bidders and values uniform on [0,1]:
  # Optimal bid = (n-1)/n * value
  bid <- optimal_bid_first_price(value = 0.8, n_bidders = 2, distribution = "uniform")
  expect_equal(bid, 0.4, tolerance = 1e-6)

  bid <- optimal_bid_first_price(value = 0.9, n_bidders = 3, distribution = "uniform")
  expect_equal(bid, 0.6, tolerance = 1e-6)
})

test_that("expected_revenue computes for standard auctions", {
  # For 2 bidders with values uniform on [0,1]:
  # Expected revenue of first-price and second-price auctions = 1/3
  rev_fp <- expected_revenue(
    auction_type = "first_price",
    n_bidders = 2,
    distribution = "uniform",
    lower = 0, upper = 1
  )
  rev_sp <- expected_revenue(
    auction_type = "second_price",
    n_bidders = 2,
    distribution = "uniform",
    lower = 0, upper = 1
  )

  # Revenue equivalence: should be equal
  expect_equal(rev_fp, rev_sp, tolerance = 0.01)
  # Both should be 1/3
  expect_equal(rev_fp, 1/3, tolerance = 0.01)
})
