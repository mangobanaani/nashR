#' Backward Induction for Extensive Form Games
#'
#' Finds the subgame perfect equilibrium of a perfect information extensive
#' form game using backward induction. The algorithm works from the terminal
#' nodes upward: at each decision node the acting player selects the action
#' that maximises their own payoff given the optimal play in all subsequent
#' subgames.
#'
#' @param game An \code{\link{ExtensiveFormGame}} object with perfect
#'   information (all information sets must be singletons or absent).
#' @return A list with components:
#'   \describe{
#'     \item{actions}{Named list mapping each decision node id to the optimal
#'       action chosen at that node.}
#'     \item{outcome}{Numeric vector of payoffs at the equilibrium terminal
#'       node.}
#'     \item{path}{Character vector of actions taken along the equilibrium
#'       path from root to terminal.}
#'   }
#' @export
backward_induction <- function(game) {
  # Validate perfect information: every info set must be a singleton
  info_sets <- game$info_sets
  if (length(info_sets) > 0) {
    for (iset_name in names(info_sets)) {
      if (length(info_sets[[iset_name]]) > 1) {
        stop(
          "backward_induction requires perfect information but found ",
          "imperfect info set '", iset_name, "' with ",
          length(info_sets[[iset_name]]), " nodes"
        )
      }
    }
  }

  # Environment to accumulate optimal actions at every decision node
  actions <- list()

  solve_node <- function(node_id) {
    node <- game$nodes[[node_id]]

    if (node$type == "terminal") {
      return(list(payoffs = node$payoffs, path = character(0)))
    }

    best_payoff <- NULL
    best_action <- NULL
    best_path <- NULL

    for (action in node$actions) {
      child_id <- node$children[[action]]
      child_result <- solve_node(child_id)
      if (is.null(best_payoff) ||
          child_result$payoffs[node$player] > best_payoff[node$player]) {
        best_payoff <- child_result$payoffs
        best_action <- action
        best_path <- c(action, child_result$path)
      }
    }

    actions[[node_id]] <<- best_action
    list(payoffs = best_payoff, path = best_path)
  }

  root_result <- solve_node(game$root_id)

  list(
    actions = actions,
    outcome = root_result$payoffs,
    path    = root_result$path
  )
}
