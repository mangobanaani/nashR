#' Extensive (Sequential) Form Game
#'
#' R6 class representing an extensive form (game tree) with support for
#' imperfect information via information sets. The tree is built using a
#' pipe-friendly builder pattern with \code{\link{add_node}} and
#' \code{\link{add_terminal}}.
#'
#' @section Active Bindings:
#' \describe{
#'   \item{n_players}{Integer, number of players (max player index).}
#'   \item{terminals}{Character vector of terminal node ids.}
#'   \item{decision_nodes}{Character vector of decision node ids.}
#'   \item{info_sets}{Named list mapping info set name to vector of node ids.}
#' }
#'
#' @section Methods:
#' \describe{
#'   \item{terminal_payoff(node_id)}{Return payoff vector at a terminal node.}
#'   \item{print()}{Print a summary of the game.}
#' }
#'
#' @export
ExtensiveFormGame <- R6::R6Class(
  "ExtensiveFormGame",

  public = list(

    #' @field nodes Named list of node data. Each entry is a list with fields
    #'   depending on node type.
    nodes = NULL,

    #' @field root_id Character, id of the root node.
    root_id = NULL,

    #' @description
    #' Create a new ExtensiveFormGame (normally use \code{\link{extensive_form}}
    #' instead).
    #' @return A new \code{ExtensiveFormGame} object.
    initialize = function() {
      self$nodes <- list()
      self$root_id <- NULL
    },

    #' @description
    #' Get the payoff vector at a terminal node.
    #' @param node_id Character, id of the terminal node.
    #' @return Numeric vector of payoffs, one per player.
    terminal_payoff = function(node_id) {
      node <- self$nodes[[node_id]]
      if (is.null(node)) {
        stop("Node '", node_id, "' not found")
      }
      if (node$type != "terminal") {
        stop("Node '", node_id, "' is not a terminal node")
      }
      node$payoffs
    },

    #' @description
    #' Print a summary of the game.
    print = function() {
      cat("Extensive Form Game\n")
      cat("Players:", self$n_players, "\n")
      cat("Decision nodes:", length(self$decision_nodes), "\n")
      cat("Terminal nodes:", length(self$terminals), "\n")
      isets <- self$info_sets
      if (length(isets) > 0) {
        cat("Information sets:", length(isets), "\n")
      }
      invisible(self)
    }
  ),

  active = list(

    #' @field n_players Number of players.
    n_players = function() {
      decision <- self$decision_nodes
      terminal <- self$terminals
      n_from_decisions <- 0L
      if (length(decision) > 0) {
        n_from_decisions <- max(vapply(
          self$nodes[decision], function(n) n$player, integer(1)
        ))
      }
      n_from_terminals <- 0L
      if (length(terminal) > 0) {
        n_from_terminals <- max(vapply(
          self$nodes[terminal], function(n) length(n$payoffs), integer(1)
        ))
      }
      as.integer(max(n_from_decisions, n_from_terminals))
    },

    #' @field terminals Character vector of terminal node ids.
    terminals = function() {
      ids <- names(self$nodes)
      ids[vapply(self$nodes, function(n) n$type == "terminal", logical(1))]
    },

    #' @field decision_nodes Character vector of decision node ids.
    decision_nodes = function() {
      ids <- names(self$nodes)
      ids[vapply(self$nodes, function(n) n$type == "decision", logical(1))]
    },

    #' @field info_sets Named list mapping info set name -> vector of node ids.
    info_sets = function() {
      result <- list()
      for (id in names(self$nodes)) {
        node <- self$nodes[[id]]
        if (!is.null(node$info_set)) {
          iset <- node$info_set
          result[[iset]] <- c(result[[iset]], id)
        }
      }
      result
    }
  )
)


#' Create an Extensive Form Game
#'
#' Initializes an empty extensive form game tree. Build the tree by piping
#' into \code{\link{add_node}} and \code{\link{add_terminal}}.
#'
#' @return An \code{\link{ExtensiveFormGame}} object.
#' @export
extensive_form <- function() {
  ExtensiveFormGame$new()
}


#' Add a Decision Node to an Extensive Form Game
#'
#' Adds a decision node to the game tree. On the first call, use
#' \code{parent_id = "root"} to create the root node. Subsequent calls
#' use the path-based id of the parent's action to specify placement.
#'
#' @param game An \code{\link{ExtensiveFormGame}} object.
#' @param parent_id Character, the node id to attach to. Use \code{"root"}
#'   for the root node.
#' @param player Integer, which player decides at this node.
#' @param actions Character vector of available actions.
#' @param info_set Optional character, information set label for imperfect
#'   information.
#' @return The modified game object (for pipe chaining).
#' @export
add_node <- function(game, parent_id, player, actions, info_set = NULL) {
  # If this is the root node
  if (parent_id == "root") {
    if (!is.null(game$root_id)) {
      stop("Root node already exists")
    }
    # Build children ids from action labels
    children <- as.list(actions)
    names(children) <- actions

    game$nodes[["root"]] <- list(
      type = "decision",
      player = as.integer(player),
      actions = actions,
      children = children,
      info_set = info_set,
      parent = NULL
    )
    game$root_id <- "root"
    return(game)
  }

  # For non-root nodes, parent_id is the path id (e.g., "L", "Ll")

  # Find the parent: the parent is the node whose children list contains
  # parent_id as a value
  parent_found <- FALSE
  for (nid in names(game$nodes)) {
    node <- game$nodes[[nid]]
    if (node$type == "decision" && parent_id %in% unlist(node$children)) {
      parent_found <- TRUE
      break
    }
  }

  if (!parent_found) {
    stop("Parent not found: no existing node has child id '", parent_id, "'")
  }

  # Build children ids: concatenate parent_id with each action label
  children <- as.list(paste0(parent_id, actions))
  names(children) <- actions

  game$nodes[[parent_id]] <- list(
    type = "decision",
    player = as.integer(player),
    actions = actions,
    children = children,
    info_set = info_set,
    parent = nid
  )

  return(game)
}


#' Add a Terminal Node to an Extensive Form Game
#'
#' Adds a terminal (leaf) node with payoffs to the game tree.
#'
#' @param game An \code{\link{ExtensiveFormGame}} object.
#' @param node_id Character, the path-based id for this terminal node.
#' @param payoffs Numeric vector of payoffs, one per player.
#' @return The modified game object (for pipe chaining).
#' @export
add_terminal <- function(game, node_id, payoffs) {
  # Verify this node_id is a valid child of some existing decision node
  parent_found <- FALSE
  for (nid in names(game$nodes)) {
    node <- game$nodes[[nid]]
    if (node$type == "decision" && node_id %in% unlist(node$children)) {
      parent_found <- TRUE
      break
    }
  }

  if (!parent_found) {
    stop("Parent not found: no existing node has child id '", node_id, "'")
  }

  game$nodes[[node_id]] <- list(
    type = "terminal",
    payoffs = payoffs,
    parent = nid
  )

  return(game)
}


#' Convert Extensive Form Game to Normal Form
#'
#' Converts an extensive form game to its normal (strategic) form
#' representation by enumerating all pure strategies for each player and
#' computing the resulting payoffs.
#'
#' A pure strategy for a player is a complete contingency plan: one action
#' choice at every information set (or decision node, for perfect information
#' games) where that player acts.
#'
#' @param game An \code{\link{ExtensiveFormGame}} object.
#' @return A \code{\link{NormalFormGame}} object.
#' @export
to_normal_form <- function(game) {
  n <- game$n_players

  # Identify decision points per player
  # For each player, collect information sets (or individual nodes if no
  # info set is specified)
  player_decision_points <- vector("list", n)
  for (p in seq_len(n)) {
    player_decision_points[[p]] <- list()
  }

  info_sets_seen <- character(0)

  for (nid in game$decision_nodes) {
    node <- game$nodes[[nid]]
    p <- node$player
    iset <- node$info_set

    if (!is.null(iset)) {
      # Group by information set - only add once
      if (!(iset %in% info_sets_seen)) {
        info_sets_seen <- c(info_sets_seen, iset)
        player_decision_points[[p]] <- c(
          player_decision_points[[p]],
          list(list(
            id = iset,
            actions = node$actions,
            type = "info_set"
          ))
        )
      }
    } else {
      # Individual decision node
      player_decision_points[[p]] <- c(
        player_decision_points[[p]],
        list(list(
          id = nid,
          actions = node$actions,
          type = "node"
        ))
      )
    }
  }

  # Enumerate pure strategies for each player
  # A pure strategy is one action per decision point
  player_strategies <- vector("list", n)
  player_strategy_labels <- vector("list", n)

  for (p in seq_len(n)) {
    dps <- player_decision_points[[p]]
    if (length(dps) == 0) {
      # Player has no decision nodes (shouldn't happen in well-formed game)
      player_strategies[[p]] <- list(list())
      player_strategy_labels[[p]] <- "none"
      next
    }

    # Enumerate all combinations of actions across decision points
    action_lists <- lapply(dps, function(dp) dp$actions)
    combos <- expand.grid(action_lists, stringsAsFactors = FALSE)

    strategies <- list()
    labels <- character(nrow(combos))
    for (i in seq_len(nrow(combos))) {
      strat <- list()
      label_parts <- character(0)
      for (j in seq_along(dps)) {
        action <- combos[i, j]
        strat[[dps[[j]]$id]] <- action
        label_parts <- c(label_parts, action)
      }
      strategies[[i]] <- strat
      labels[i] <- paste(label_parts, collapse = "")
    }
    player_strategies[[p]] <- strategies
    player_strategy_labels[[p]] <- labels
  }

  # Build payoff array
  n_strats <- vapply(player_strategies, length, integer(1))
  payoff_arr <- array(dim = c(n_strats, n))

  # Iterate over all strategy profiles
  profile_indices <- expand.grid(lapply(n_strats, seq_len))

  for (row in seq_len(nrow(profile_indices))) {
    profile <- as.integer(profile_indices[row, ])

    # Build a combined action map: decision_point_id -> action
    action_map <- list()
    for (p in seq_len(n)) {
      strat <- player_strategies[[p]][[profile[p]]]
      for (dp_id in names(strat)) {
        action_map[[dp_id]] <- strat[[dp_id]]
      }
    }

    # Trace through tree to find terminal
    payoffs <- trace_tree(game, game$root_id, action_map)

    # Store in array
    idx <- c(as.list(profile), list(seq_len(n)))
    payoff_arr <- do.call(`[<-`, c(list(payoff_arr), idx, list(payoffs)))
  }

  # Create player names
  players <- paste0("Player ", seq_len(n))

  NormalFormGame$new(
    players = players,
    strategies = player_strategy_labels,
    payoffs = payoff_arr
  )
}


#' Trace through game tree following a strategy profile
#'
#' @param game ExtensiveFormGame object.
#' @param node_id Current node id.
#' @param action_map Named list mapping decision point id -> action choice.
#' @return Numeric vector of payoffs at the reached terminal node.
#' @keywords internal
trace_tree <- function(game, node_id, action_map) {
  node <- game$nodes[[node_id]]

  if (node$type == "terminal") {
    return(node$payoffs)
  }

  # Determine which action to take at this node
  # Check info_set first, then node id
  dp_id <- if (!is.null(node$info_set)) node$info_set else node_id
  action <- action_map[[dp_id]]

  if (is.null(action)) {
    stop("No action specified for decision point '", dp_id, "'")
  }

  # Follow the action to the child
  child_id <- node$children[[action]]
  trace_tree(game, child_id, action_map)
}
