test_that("backward_induction solves simple sequential game", {
  # Player 1: L or R
  # If L: Player 2: l or r -> (L,l)=(2,1), (L,r)=(0,0)
  # If R: terminal (1,3)
  g <- extensive_form() |>
    add_node("root", player = 1, actions = c("L", "R")) |>
    add_node("L", player = 2, actions = c("l", "r")) |>
    add_terminal("Ll", payoffs = c(2, 1)) |>
    add_terminal("Lr", payoffs = c(0, 0)) |>
    add_terminal("R", payoffs = c(1, 3))

  result <- backward_induction(g)

  # Player 2 at node L: l gives (2,1), r gives (0,0) -> chooses l
  # Player 1 at root: L leads to (2,1) via player 2 choosing l, R leads to (1,3)
  # Player 1 prefers (2,1) over (1,3)? 2 > 1, so chooses L
  expect_equal(result$outcome, c(2, 1))
  expect_equal(result$actions[["root"]], "L")
  expect_equal(result$actions[["L"]], "l")
})

test_that("backward_induction solves centipede-like game", {
  # Player 1: Stop(2,0) or Continue
  # Player 2: Stop(1,3) or Continue
  # Player 1: Stop(4,2) or Continue -> (3,5)
  g <- extensive_form() |>
    add_node("root", player = 1, actions = c("Stop", "Continue")) |>
    add_terminal("Stop", payoffs = c(2, 0)) |>
    add_node("Continue", player = 2, actions = c("Stop", "Continue")) |>
    add_terminal("ContinueStop", payoffs = c(1, 3)) |>
    add_node("ContinueContinue", player = 1, actions = c("Stop", "Continue")) |>
    add_terminal("ContinueContinueStop", payoffs = c(4, 2)) |>
    add_terminal("ContinueContinueContinue", payoffs = c(3, 5))

  result <- backward_induction(g)
  # Working backwards:
  # Node ContinueContinue (P1): Stop(4,2) vs Continue(3,5) -> P1 chooses Stop
  # Node Continue (P2): Stop(1,3) vs Continue->Stop(4,2) -> P2 chooses Stop (3 > 2)
  # Node root (P1): Stop(2,0) vs Continue->Stop(1,3) -> P1 chooses Stop (2 > 1)
  expect_equal(result$outcome, c(2, 0))
  expect_equal(result$actions[["root"]], "Stop")
})

test_that("backward_induction solves ultimatum-like game", {
  # Proposer offers Fair or Unfair
  # Responder Accepts or Rejects
  g <- extensive_form() |>
    add_node("root", player = 1, actions = c("Fair", "Unfair")) |>
    add_node("Fair", player = 2, actions = c("Accept", "Reject")) |>
    add_terminal("FairAccept", payoffs = c(5, 5)) |>
    add_terminal("FairReject", payoffs = c(0, 0)) |>
    add_node("Unfair", player = 2, actions = c("Accept", "Reject")) |>
    add_terminal("UnfairAccept", payoffs = c(8, 2)) |>
    add_terminal("UnfairReject", payoffs = c(0, 0))

  result <- backward_induction(g)
  # P2 at Fair: Accept(5) > Reject(0) -> Accept
  # P2 at Unfair: Accept(2) > Reject(0) -> Accept
  # P1 at root: Fair->Accept(5) vs Unfair->Accept(8) -> Unfair
  expect_equal(result$outcome, c(8, 2))
  expect_equal(result$actions[["root"]], "Unfair")
  expect_equal(result$actions[["Unfair"]], "Accept")
})

test_that("backward_induction returns strategy profile", {
  g <- extensive_form() |>
    add_node("root", player = 1, actions = c("L", "R")) |>
    add_node("L", player = 2, actions = c("l", "r")) |>
    add_terminal("Ll", payoffs = c(2, 1)) |>
    add_terminal("Lr", payoffs = c(0, 0)) |>
    add_terminal("R", payoffs = c(1, 3))

  result <- backward_induction(g)

  # Should return the full strategy profile (what each player does at each node)
  expect_true("actions" %in% names(result))
  expect_true("outcome" %in% names(result))
  expect_true("path" %in% names(result))
  # Path should be the sequence of actions taken in equilibrium
  expect_equal(result$path, c("L", "l"))
})

test_that("backward_induction rejects imperfect information games", {
  g <- extensive_form() |>
    add_node("root", player = 1, actions = c("L", "R")) |>
    add_node("L", player = 2, actions = c("l", "r"), info_set = "h") |>
    add_node("R", player = 2, actions = c("l", "r"), info_set = "h") |>
    add_terminal("Ll", payoffs = c(1, 0)) |>
    add_terminal("Lr", payoffs = c(0, 1)) |>
    add_terminal("Rl", payoffs = c(0, 1)) |>
    add_terminal("Rr", payoffs = c(1, 0))

  expect_error(backward_induction(g), "perfect information|info.set|imperfect")
})
