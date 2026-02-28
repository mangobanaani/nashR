# nashR

An R package for game theory. Provides tools for constructing and solving non-cooperative games, cooperative games, Bayesian games, extensive form games, auctions, and related problems. Some algorithms are implemented in C.

## Installation

```r
R CMD INSTALL .
```

## Examples

### Nash equilibria

```r
library(nashR)

# Battle of the Sexes: 2 pure + 1 mixed equilibrium
bos <- battle_of_sexes()
eqs <- nash_equilibria(bos)
print_equilibria(eqs, bos)
#> Nash Equilibria: 3 found
#>
#> --- Equilibrium 1 ---
#>   Player 1: A=1, B=0
#>   Player 2: A=1, B=0
#>   Payoffs: (3, 2)
#> ...
```

### Evolutionary dynamics

```r
# Hawk-Dove replicator dynamics
hd <- hawk_dove(V = 4, C = 6)
traj <- replicator_dynamics(hd, x0 = c(0.1, 0.9), t_max = 20, dt = 0.01)

# Plot trajectory toward ESS
plot(traj$time, traj$x[, 1], type = "l", xlab = "Time", ylab = "Pr(Hawk)",
     main = "Hawk-Dove Replicator Dynamics")
abline(h = 2/3, lty = 2, col = "grey")  # ESS at V/C = 2/3
```

### Cooperative games

```r
# Shapley value of a 3-player majority game
game <- cooperative_game(
  players = c("A", "B", "C"),
  value = function(S) if (length(S) >= 2) 1 else 0
)
shapley_value(game)
#>         A         B         C
#> 0.3333333 0.3333333 0.3333333
```

### Fictitious play

```r
# Fictitious play on matching pennies converges to mixed NE
mp <- matching_pennies()
fp <- fictitious_play(mp, n_rounds = 200)

plot(fp$belief_history[[1]][, 1], type = "l", ylim = c(0, 1),
     xlab = "Round", ylab = "Empirical Pr(Heads)",
     main = "Fictitious Play: Matching Pennies")
lines(fp$belief_history[[2]][, 1], col = "red")
abline(h = 0.5, lty = 2, col = "grey")
legend("topright", c("Player 1", "Player 2"), col = c("black", "red"), lty = 1)
```

### Extensive form games

```r
# Backward induction on a simple game tree
efg <- extensive_form() |>
  add_node("root", player = 1, actions = c("L", "R")) |>
  add_node("L", player = 2, actions = c("l", "r")) |>
  add_terminal("Ll", payoffs = c(2, 1)) |>
  add_terminal("Lr", payoffs = c(0, 0)) |>
  add_terminal("R", payoffs = c(1, 3))

backward_induction(efg)
#> $actions
#>  root     L
#>   "L"   "l"
#> $outcome
#> [1] 2 1
```

### Auctions

```r
# Revenue equivalence: first-price and second-price yield the same revenue
expected_revenue("first_price", n_bidders = 5)
#> [1] 0.6666667
expected_revenue("second_price", n_bidders = 5)
#> [1] 0.6666667
```

See `vignettes/` for more detailed walkthroughs.

## License

GPL (>= 3)
