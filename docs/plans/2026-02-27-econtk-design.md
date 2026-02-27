# econtk Design Document

## Overview

`econtk` is a research-grade R package for game theory — both non-cooperative and cooperative. It fills a major gap in the R ecosystem: no CRAN package currently implements standard algorithms (Lemke-Howson, support enumeration, vertex enumeration) for computing Nash equilibria of finite strategic-form games.

**Target audience**: Academic researchers in economics.

**Goal**: Replace existing limited packages (rgamer, GameTheory, CoopGame) with a single comprehensive, computationally rigorous toolkit.

## Package Structure

One monolithic package with six modules:

1. **Game representations** — Normal form, extensive form, Bayesian games
2. **Equilibrium solvers** — Pure strategy enumeration, support enumeration, Lemke-Howson (C), vertex enumeration, homotopy continuation (C) for N-player
3. **Auction theory** — First/second-price sealed bid, English/Dutch, Vickrey, revenue equivalence
4. **Mechanism design** — VCG mechanisms, incentive compatibility checking, revelation principle utilities
5. **Evolutionary dynamics** — Replicator dynamics, best response dynamics, fictitious play
6. **Cooperative game theory** — Shapley value, core, nucleolus, voting power indices

## Architecture

### Class Hierarchy (R6)

```
Game (abstract base)
├── NormalFormGame       # Payoff tensors, N-player
│   ├── BimatrixGame     # Optimized 2-player specialization
│   └── ZeroSumGame      # Further specialization
├── ExtensiveFormGame    # Tree structure with info sets
├── BayesianGame         # Type spaces + beliefs
└── CooperativeGame      # Characteristic function v(S)
```

R6 classes for mutable state, intuitive OOP, and pipe compatibility.

### Payoff Storage

- Dense arrays for small games
- Sparse representation for large games
- Automatic switching based on game size

### Solver Dispatch

```
nash_equilibria(game, method = "auto")
  → 2-player zero-sum  → LP solver (C)
  → 2-player general   → Lemke-Howson (C), support enum, vertex enum
  → N-player           → homotopy continuation (C), simplicial subdivision
```

## API Design

Functional style with standalone functions (no generic method dispatch):

### Game Construction

```r
# Normal form with explicit payoffs
g <- normal_form(
  players = c("Firm A", "Firm B"),
  strategies = list(c("High", "Low"), c("High", "Low")),
  payoffs = list(c(3,3, 1,4, 4,1, 2,2))
)

# Built-in classic games
g <- prisoners_dilemma()
g <- battle_of_sexes()
g <- matching_pennies()
g <- cournot(n = 3, cost = c(10, 12, 11), demand = function(Q) 100 - Q)

# Extensive form (pipe-friendly builder)
g <- extensive_form() |>
  add_node("root", player = 1, actions = c("L", "R")) |>
  add_node("L", player = 2, actions = c("l", "r")) |>
  add_terminal("Ll", payoffs = c(2, 1)) |>
  add_terminal("Lr", payoffs = c(0, 0)) |>
  add_terminal("R", payoffs = c(1, 3))

# Cooperative game
g <- cooperative_game(
  players = c("A", "B", "C"),
  value = function(S) { ... }
)
```

### Solving and Analysis

```r
nash_equilibria(g)                              # find all Nash equilibria
nash_equilibria(g, method = "lemke_howson")      # specific algorithm
dominant_strategy(g)                             # dominant strategies
best_response(g, player = 1, opponent_strategy = c(0.5, 0.5))

# Cooperative
shapley_value(g)
core_check(g, allocation)
nucleolus(g)

# Properties
is_zero_sum(g)
is_symmetric(g)
pareto_optimal(g)
```

## C Layer

### Implemented in C (via .Call())

- Lemke-Howson pivoting
- Support enumeration (combinatorial + linear system solving)
- Vertex enumeration (polytope vertex computation)
- Homotopy continuation for N-player games
- GMP-based rational arithmetic for exact methods
- LP solver for zero-sum games

### Stays in R

- Input validation and game construction
- Pretty-printing and summary output
- High-level algorithm selection logic

### C Interface

- Standard R package `src/` directory
- `.Call()` with `SEXP` types
- `R_RegisterCDynamicCallEntry` for proper registration
- No external C library dependencies for core algorithms
- Optional GMP linkage for exact arithmetic (graceful fallback to double)

## Testing

- `testthat` framework
- Known game solutions as test cases (textbook games with known equilibria)
- Degenerate games that break naive algorithms
- Large random games for performance benchmarks
- Cross-validation against Gambit/Nashpy results

## Documentation

- `roxygen2` for function documentation
- Vignettes: "Getting Started", "Equilibrium Algorithms", "Game Theory for Economists"
- CRAN compliance: `R CMD check` clean, no warnings, portable C code

## Competitive Landscape

| Package | Language | Scope | Limitations |
|---------|----------|-------|-------------|
| rgamer | R (GitHub) | 2-player normal form | Educational only, no standard algorithms |
| GameTheory | R (CRAN) | Cooperative only | No non-cooperative support |
| GNE | R (CRAN) | Continuous games | No finite game support |
| Nashpy | Python | 2-player finite | No N-player support |
| Gambit | Python/C | Full (N-player) | Not available in R |
| GameTheory.jl | Julia | 2-player + homotopy | Not available in R |
| **econtk** | **R/C** | **Full (N-player, coop + non-coop)** | **Fills the R gap** |
