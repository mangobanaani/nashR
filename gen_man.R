# Generate minimal .Rd man pages from roxygen-documented R files
# This is a workaround since roxygen2 cannot be installed

man_dir <- "man"

# Helper to write an .Rd file
write_rd <- function(name, title, description, usage, params, value, keywords_internal = FALSE) {
  lines <- character(0)
  lines <- c(lines, paste0("\\name{", name, "}"))
  lines <- c(lines, paste0("\\alias{", name, "}"))
  lines <- c(lines, paste0("\\title{", title, "}"))
  lines <- c(lines, "\\description{")
  lines <- c(lines, description)
  lines <- c(lines, "}")
  if (!is.null(usage)) {
    lines <- c(lines, "\\usage{")
    lines <- c(lines, usage)
    lines <- c(lines, "}")
  }
  if (length(params) > 0) {
    lines <- c(lines, "\\arguments{")
    for (p in names(params)) {
      lines <- c(lines, paste0("\\item{", p, "}{", params[[p]], "}"))
    }
    lines <- c(lines, "}")
  }
  if (!is.null(value)) {
    lines <- c(lines, "\\value{")
    lines <- c(lines, value)
    lines <- c(lines, "}")
  }
  if (keywords_internal) {
    lines <- c(lines, "\\keyword{internal}")
  }
  writeLines(lines, file.path(man_dir, paste0(name, ".Rd")))
}

# NormalFormGame
write_rd("NormalFormGame", "Normal (Strategic) Form Game",
  "R6 class representing an N-player normal form game.",
  NULL,
  list(),
  "An R6 class object.")

# normal_form
write_rd("normal_form", "Create a Normal Form Game",
  "Convenience constructor for creating a NormalFormGame from payoff matrices or array.",
  "normal_form(players, strategies, payoffs)",
  list(players = "Character vector of player names.",
       strategies = "List of character vectors, one per player.",
       payoffs = "List of matrices or a numeric array."),
  "A NormalFormGame object.")

# prisoners_dilemma
write_rd("prisoners_dilemma", "Prisoner's Dilemma",
  "Create a Prisoner's Dilemma game.",
  "prisoners_dilemma(R = 3, T = 5, S = 0, P = 1)",
  list(R = "Reward for mutual cooperation.",
       T = "Temptation to defect.",
       S = "Sucker's payoff.",
       P = "Punishment for mutual defection."),
  "A NormalFormGame object.")

# battle_of_sexes
write_rd("battle_of_sexes", "Battle of the Sexes",
  "Create a Battle of the Sexes game.",
  "battle_of_sexes()",
  list(),
  "A NormalFormGame object.")

# matching_pennies
write_rd("matching_pennies", "Matching Pennies",
  "Create a Matching Pennies game.",
  "matching_pennies()",
  list(),
  "A NormalFormGame object.")

# coordination_game
write_rd("coordination_game", "Coordination Game",
  "Create a pure coordination game.",
  "coordination_game()",
  list(),
  "A NormalFormGame object.")

# hawk_dove
write_rd("hawk_dove", "Hawk-Dove Game",
  "Create a Hawk-Dove (Chicken) game.",
  "hawk_dove(V = 4, C = 6)",
  list(V = "Value of the resource.",
       C = "Cost of fighting."),
  "A NormalFormGame object.")

# stag_hunt
write_rd("stag_hunt", "Stag Hunt Game",
  "Create a Stag Hunt game.",
  "stag_hunt()",
  list(),
  "A NormalFormGame object.")

# pure_nash
write_rd("pure_nash", "Find Pure Strategy Nash Equilibria",
  "Enumerates all strategy profiles and identifies pure strategy Nash equilibria.",
  "pure_nash(game)",
  list(game = "A NormalFormGame object."),
  "A list of equilibria.")

# support_enumeration
write_rd("support_enumeration", "Support Enumeration for Nash Equilibria",
  "Find all Nash equilibria of a 2-player game using support enumeration.",
  "support_enumeration(game, tol = 1e-10)",
  list(game = "A NormalFormGame object (2-player).",
       tol = "Numeric tolerance."),
  "A list of equilibria.")

# nash_equilibria
write_rd("nash_equilibria", "Find Nash Equilibria",
  "Unified interface for finding Nash equilibria.",
  "nash_equilibria(game, method = \"auto\")",
  list(game = "A NormalFormGame object.",
       method = "Algorithm: 'auto', 'pure', or 'support'."),
  "A list of equilibria.")

# print_equilibria
write_rd("print_equilibria", "Print Nash Equilibria",
  "Pretty-print a list of Nash equilibria.",
  "print_equilibria(equilibria, game)",
  list(equilibria = "A list of equilibria from nash_equilibria.",
       game = "A NormalFormGame object."),
  "Invisibly returns equilibria.")

# lemke_howson
write_rd("lemke_howson", "Lemke-Howson Algorithm",
  "Find one Nash equilibrium using the Lemke-Howson complementary pivoting algorithm.",
  "lemke_howson(game, init_label = 1L)",
  list(game = "A 2-player NormalFormGame.",
       init_label = "Starting label for pivoting (1 to m+n)."),
  "A list with strategies and payoffs.")

# is_zero_sum
write_rd("is_zero_sum", "Check if a Game is Zero-Sum",
  "For a 2-player game, check whether all payoff sums are zero.",
  "is_zero_sum(game, tol = 1e-10)",
  list(game = "A NormalFormGame object.",
       tol = "Numeric tolerance."),
  "Logical scalar.")

# is_symmetric
write_rd("is_symmetric", "Check if a Game is Symmetric",
  "For a 2-player game, check the symmetry condition.",
  "is_symmetric(game, tol = 1e-10)",
  list(game = "A NormalFormGame object.",
       tol = "Numeric tolerance."),
  "Logical scalar.")

# dominant_strategy
write_rd("dominant_strategy", "Find Dominant Strategies",
  "Check if any strategy strictly dominates all others for each player.",
  "dominant_strategy(game)",
  list(game = "A NormalFormGame object."),
  "A list with one element per player.")

# best_response
write_rd("best_response", "Compute Best Response",
  "Compute the best response strategies for a player given an opponent mixed strategy.",
  "best_response(game, player, opponent_strategy, tol = 1e-10)",
  list(game = "A NormalFormGame object.",
       player = "Integer, the player (1 or 2).",
       opponent_strategy = "Numeric vector of opponent's mixed strategy.",
       tol = "Numeric tolerance."),
  "Integer vector of best response strategy indices.")

# is_pareto_optimal
write_rd("is_pareto_optimal", "Check Pareto Optimality",
  "Check whether a strategy profile is Pareto optimal.",
  "is_pareto_optimal(game, profile)",
  list(game = "A NormalFormGame object.",
       profile = "Integer vector of strategy indices."),
  "Logical scalar.")

# CooperativeGame
write_rd("CooperativeGame", "Cooperative (TU) Game",
  "R6 class representing a cooperative game with transferable utility.",
  NULL,
  list(),
  "An R6 class object.")

# cooperative_game
write_rd("cooperative_game", "Create a Cooperative Game",
  "Constructor function for CooperativeGame.",
  "cooperative_game(players, value)",
  list(players = "Character vector of player names.",
       value = "Function mapping a coalition to a numeric scalar."),
  "A CooperativeGame object.")

# is_superadditive
write_rd("is_superadditive", "Check Superadditivity",
  "Check if a cooperative game is superadditive.",
  "is_superadditive(game)",
  list(game = "A CooperativeGame object."),
  "Logical scalar.")

# shapley_value
write_rd("shapley_value", "Shapley Value",
  "Compute the Shapley value for a cooperative game.",
  "shapley_value(game, n_samples = NULL)",
  list(game = "A CooperativeGame object.",
       n_samples = "Number of Monte Carlo samples, or NULL for exact."),
  "Named numeric vector of Shapley values.")

# in_core
write_rd("in_core", "Check Core Membership",
  "Check if an allocation is in the core of a cooperative game.",
  "in_core(game, allocation, tol = 1e-08)",
  list(game = "A CooperativeGame object.",
       allocation = "Named numeric vector of payoffs.",
       tol = "Numerical tolerance."),
  "Logical scalar.")

# core_constraints
write_rd("core_constraints", "Core Constraint Matrix",
  "Returns the constraint system defining the core.",
  "core_constraints(game)",
  list(game = "A CooperativeGame object."),
  "A list with A, b, and v_N.")

# nucleolus
write_rd("nucleolus", "Nucleolus",
  "Compute the nucleolus of a cooperative game.",
  "nucleolus(game, tol = 1e-10)",
  list(game = "A CooperativeGame object.",
       tol = "Numerical tolerance."),
  "Named numeric vector of nucleolus payoffs.")

# ExtensiveFormGame
write_rd("ExtensiveFormGame", "Extensive Form Game",
  "R6 class representing an extensive form (game tree).",
  NULL,
  list(),
  "An R6 class object.")

# extensive_form
write_rd("extensive_form", "Create an Extensive Form Game",
  "Initializes an empty extensive form game tree.",
  "extensive_form()",
  list(),
  "An ExtensiveFormGame object.")

# add_node
write_rd("add_node", "Add a Decision Node",
  "Adds a decision node to the game tree.",
  "add_node(game, parent_id, player, actions, info_set = NULL)",
  list(game = "An ExtensiveFormGame object.",
       parent_id = "Character, the node id to attach to.",
       player = "Integer, which player decides.",
       actions = "Character vector of available actions.",
       info_set = "Optional character, information set label."),
  "The modified game object.")

# add_terminal
write_rd("add_terminal", "Add a Terminal Node",
  "Adds a terminal (leaf) node with payoffs to the game tree.",
  "add_terminal(game, node_id, payoffs)",
  list(game = "An ExtensiveFormGame object.",
       node_id = "Character, the path-based id for this terminal.",
       payoffs = "Numeric vector of payoffs."),
  "The modified game object.")

# to_normal_form
write_rd("to_normal_form", "Convert Extensive to Normal Form",
  "Converts an extensive form game to normal form.",
  "to_normal_form(game)",
  list(game = "An ExtensiveFormGame object."),
  "A NormalFormGame object.")

# backward_induction
write_rd("backward_induction", "Backward Induction",
  "Find the subgame perfect equilibrium using backward induction.",
  "backward_induction(game)",
  list(game = "An ExtensiveFormGame object with perfect information."),
  "A list with actions, outcome, and path.")

# cournot
write_rd("cournot", "Cournot Oligopoly Game",
  "Create an N-player Cournot oligopoly game.",
  "cournot(n, quantities, demand, costs)",
  list(n = "Integer, number of firms.",
       quantities = "Numeric vector of possible quantities.",
       demand = "Function mapping total quantity to price.",
       costs = "Marginal costs (vector or scalar)."),
  "A NormalFormGame object.")

# bertrand
write_rd("bertrand", "Bertrand Competition Game",
  "Create an N-player Bertrand competition game.",
  "bertrand(n, prices, demand, costs)",
  list(n = "Integer, number of firms.",
       prices = "Numeric vector of possible prices.",
       demand = "Function mapping price to demand.",
       costs = "Marginal costs (vector or scalar)."),
  "A NormalFormGame object.")

# public_goods_game
write_rd("public_goods_game", "Public Goods Game",
  "Create an N-player public goods game.",
  "public_goods_game(n, endowment, multiplier, contribution_levels)",
  list(n = "Integer, number of players.",
       endowment = "Numeric, each player's endowment.",
       multiplier = "Numeric, the public goods multiplier.",
       contribution_levels = "Numeric vector of possible contributions."),
  "A NormalFormGame object.")

# first_price_auction
write_rd("first_price_auction", "First-Price Sealed-Bid Auction",
  "Create a discrete N-bidder first-price sealed-bid auction.",
  "first_price_auction(n_bidders, values, bid_levels)",
  list(n_bidders = "Integer, number of bidders.",
       values = "Numeric vector of private values.",
       bid_levels = "Numeric vector of possible bid levels."),
  "A NormalFormGame object.")

# second_price_auction
write_rd("second_price_auction", "Second-Price Sealed-Bid Auction",
  "Create a discrete N-bidder second-price (Vickrey) auction.",
  "second_price_auction(n_bidders, values, bid_levels)",
  list(n_bidders = "Integer, number of bidders.",
       values = "Numeric vector of private values.",
       bid_levels = "Numeric vector of possible bid levels."),
  "A NormalFormGame object.")

# all_pay_auction
write_rd("all_pay_auction", "All-Pay Auction",
  "Create a discrete N-bidder all-pay auction.",
  "all_pay_auction(n_bidders, values, bid_levels)",
  list(n_bidders = "Integer, number of bidders.",
       values = "Numeric vector of private values.",
       bid_levels = "Numeric vector of possible bid levels."),
  "A NormalFormGame object.")

# optimal_bid_first_price
write_rd("optimal_bid_first_price", "Optimal Bid in First-Price Auction",
  "Compute the BNE bid for a first-price auction with i.i.d. uniform values.",
  "optimal_bid_first_price(value, n_bidders, distribution = \"uniform\", lower = 0, upper = 1)",
  list(value = "Numeric, the bidder's private value.",
       n_bidders = "Integer, number of bidders.",
       distribution = "Character, value distribution (only 'uniform').",
       lower = "Lower bound of uniform distribution.",
       upper = "Upper bound of uniform distribution."),
  "Numeric, the optimal bid.")

# expected_revenue
write_rd("expected_revenue", "Expected Revenue of Standard Auctions",
  "Compute expected revenue using analytical formulas.",
  "expected_revenue(auction_type, n_bidders, distribution = \"uniform\", lower = 0, upper = 1)",
  list(auction_type = "Character: 'first_price', 'second_price', or 'all_pay'.",
       n_bidders = "Integer, number of bidders.",
       distribution = "Character, value distribution.",
       lower = "Lower bound of uniform distribution.",
       upper = "Upper bound of uniform distribution."),
  "Numeric, the expected revenue.")

# vcg_mechanism
write_rd("vcg_mechanism", "VCG Mechanism",
  "Compute VCG mechanism allocation and payments.",
  "vcg_mechanism(valuations, allocation_rule)",
  list(valuations = "Named list of agent valuations.",
       allocation_rule = "Function computing the efficient allocation."),
  "List with allocation, payments, and utilities.")

# is_incentive_compatible
write_rd("is_incentive_compatible", "Check Incentive Compatibility",
  "Check whether truthful reporting is a dominant strategy.",
  "is_incentive_compatible(mechanism, n_agents, type_space, tol = 1e-08)",
  list(mechanism = "List with allocation and payment functions.",
       n_agents = "Number of agents.",
       type_space = "Numeric vector of possible types.",
       tol = "Numeric tolerance."),
  "Logical scalar.")

# is_individually_rational
write_rd("is_individually_rational", "Check Individual Rationality",
  "Check that no agent gets negative utility from truthful participation.",
  "is_individually_rational(mechanism, n_agents, type_space, tol = 1e-08)",
  list(mechanism = "List with allocation and payment functions.",
       n_agents = "Number of agents.",
       type_space = "Numeric vector of possible types.",
       tol = "Numeric tolerance."),
  "Logical scalar.")

# replicator_dynamics
write_rd("replicator_dynamics", "Replicator Dynamics",
  "Simulate replicator dynamics for a symmetric 2-player evolutionary game.",
  "replicator_dynamics(game, x0, t_max, dt = 0.01)",
  list(game = "A symmetric 2-player NormalFormGame.",
       x0 = "Initial population state (sums to 1).",
       t_max = "Time horizon.",
       dt = "Time step."),
  "A list with trajectory matrix and time vector.")

# fictitious_play
write_rd("fictitious_play", "Fictitious Play",
  "Simulate fictitious play for a 2-player game.",
  "fictitious_play(game, n_rounds)",
  list(game = "A 2-player NormalFormGame.",
       n_rounds = "Integer, number of rounds."),
  "A list with actions, beliefs, and belief_history.")

# best_response_dynamics
write_rd("best_response_dynamics", "Best Response Dynamics",
  "Simulate continuous best response dynamics for a symmetric 2-player game.",
  "best_response_dynamics(game, x0, n_rounds, alpha = 0.5)",
  list(game = "A symmetric 2-player NormalFormGame.",
       x0 = "Initial population state (sums to 1).",
       n_rounds = "Number of update steps.",
       alpha = "Adjustment speed in (0, 1]."),
  "A list with trajectory matrix.")

# is_ess
write_rd("is_ess", "Check Evolutionarily Stable Strategy",
  "Test whether a mixed strategy is an ESS.",
  "is_ess(game, strategy, tol = 1e-10)",
  list(game = "A symmetric 2-player NormalFormGame.",
       strategy = "Numeric vector (mixed strategy, sums to 1).",
       tol = "Numeric tolerance."),
  "Logical scalar.")

# econtk-package
lines <- c(
  "\\name{econtk-package}",
  "\\alias{econtk-package}",
  "\\alias{econtk}",
  "\\docType{package}",
  "\\title{Economic Toolkit for Game Theory}",
  "\\description{",
  "Research-grade game theory toolkit for R.",
  "}",
  "\\keyword{internal}"
)
writeLines(lines, file.path(man_dir, "econtk-package.Rd"))

cat("Generated", length(list.files(man_dir, pattern = "\\.Rd$")), "man pages\n")
