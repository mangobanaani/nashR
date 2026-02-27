test_that("extensive_form builds a simple game tree", {
  # Simple 2-player sequential game:
  #   Player 1 chooses L or R
  #   If L: Player 2 chooses l or r
  #     (L,l) -> (2,1), (L,r) -> (0,0)
  #   If R: terminal with payoffs (1,3)
  g <- extensive_form() |>
    add_node("root", player = 1, actions = c("L", "R")) |>
    add_node("L", player = 2, actions = c("l", "r")) |>
    add_terminal("Ll", payoffs = c(2, 1)) |>
    add_terminal("Lr", payoffs = c(0, 0)) |>
    add_terminal("R", payoffs = c(1, 3))

  expect_s3_class(g, "ExtensiveFormGame")
  expect_equal(g$n_players, 2)
  expect_equal(length(g$terminals), 3)
  expect_equal(length(g$decision_nodes), 2)  # root and L
})

test_that("extensive_form retrieves terminal payoffs", {
  g <- extensive_form() |>
    add_node("root", player = 1, actions = c("L", "R")) |>
    add_node("L", player = 2, actions = c("l", "r")) |>
    add_terminal("Ll", payoffs = c(2, 1)) |>
    add_terminal("Lr", payoffs = c(0, 0)) |>
    add_terminal("R", payoffs = c(1, 3))

  # Terminal at path "Ll" has payoffs (2,1)
  expect_equal(g$terminal_payoff("Ll"), c(2, 1))
  expect_equal(g$terminal_payoff("R"), c(1, 3))
})

test_that("extensive_form with info sets (imperfect information)", {
  # Player 2 doesn't know whether Player 1 chose L or R
  g <- extensive_form() |>
    add_node("root", player = 1, actions = c("L", "R")) |>
    add_node("L", player = 2, actions = c("l", "r"), info_set = "h2") |>
    add_node("R", player = 2, actions = c("l", "r"), info_set = "h2") |>
    add_terminal("Ll", payoffs = c(2, 1)) |>
    add_terminal("Lr", payoffs = c(0, 0)) |>
    add_terminal("Rl", payoffs = c(1, 2)) |>
    add_terminal("Rr", payoffs = c(3, 0))

  expect_equal(g$n_players, 2)
  # Two nodes in info set "h2"
  expect_equal(length(g$info_sets[["h2"]]), 2)
})

test_that("extensive_form validates tree structure", {
  # Adding a node to a non-existent parent should error
  expect_error(
    extensive_form() |>
      add_node("nonexistent", player = 1, actions = c("L", "R")),
    "parent|not found"
  )
})

test_that("to_normal_form converts extensive to normal form", {
  g <- extensive_form() |>
    add_node("root", player = 1, actions = c("L", "R")) |>
    add_node("L", player = 2, actions = c("l", "r")) |>
    add_terminal("Ll", payoffs = c(2, 1)) |>
    add_terminal("Lr", payoffs = c(0, 0)) |>
    add_terminal("R", payoffs = c(1, 3))

  nf <- to_normal_form(g)
  expect_s3_class(nf, "NormalFormGame")
  # Player 1 has 2 strategies: L, R
  # Player 2 has 2 strategies: l, r (what to do at node L)
  expect_equal(nf$n_strategies[1], 2)
  expect_equal(nf$n_strategies[2], 2)

  # Strategy (L, l) -> payoffs (2, 1)
  expect_equal(nf$payoff(c(1, 1)), c(2, 1))
  # Strategy (R, l) -> payoffs (1, 3) (player 2's choice doesn't matter)
  expect_equal(nf$payoff(c(2, 1)), c(1, 3))
  expect_equal(nf$payoff(c(2, 2)), c(1, 3))
})

test_that("print method works for ExtensiveFormGame", {
  g <- extensive_form() |>
    add_node("root", player = 1, actions = c("L", "R")) |>
    add_terminal("L", payoffs = c(1, 0)) |>
    add_terminal("R", payoffs = c(0, 1))

  expect_output(print(g), "Extensive Form Game")
})
