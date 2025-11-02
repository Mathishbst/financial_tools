Portfolio Insurance Strategies
================
Mathis Herbst
Mercredi 15 octobre 2025

- [Pricing & Portfolio Insurance: BS Call, Floor, CPPI, and
  OBPI](#pricing--portfolio-insurance-bs-call-floor-cppi-and-obpi)
  - [Paramètres :](#paramètres-)
    - [1) Brownian Motion Simulation](#1-brownian-motion-simulation)
    - [2) Black–Scholes Call (`bs_call`)](#2-blackscholes-call-bs_call)
    - [3) Floor Function (`floor_at`)](#3-floor-function-floor_at)
    - [4) CPPI Terminal Wealth
      (`cppli_path_terminal`)](#4-cppi-terminal-wealth-cppli_path_terminal)
    - [5) Single-Path Illustration &
      Plot](#5-single-path-illustration--plot)
    - [Interpretation of the Chart](#interpretation-of-the-chart)
  - [Monte Carlo Simulation (10,000
    paths)](#monte-carlo-simulation-10000-paths)
    - [4) Summary Table of Indicators](#4-summary-table-of-indicators)
    - [1) Equity Prices at Maturity](#1-equity-prices-at-maturity)
    - [2) OBPI Prices at Maturity](#2-obpi-prices-at-maturity)
    - [3) CPPI Prices at Maturity](#3-cppi-prices-at-maturity)

# Pricing & Portfolio Insurance: BS Call, Floor, CPPI, and OBPI

For this project, we will not use data sets but simulate stock prices
using a Geometric Brownian Motion (GBM) model. We will then implement
and compare two portfolio insurance strategies: Option-Based Portfolio
Insurance (OBPI) and Constant Proportion Portfolio Insurance (CPPI).

## Paramètres :

``` r
mu     <- 0.05
sigma  <- 0.20
delta  <- 1/12
mat    <- 1
rf     <- 0.02
strike <- 100      # capital garanti à maturité (K)
S0     <- 100
m_cppi <- 6    # multiplicateur CPPI
nsimul <- 10000
```

### 1) Brownian Motion Simulation

We will **simulate the evolution of an asset price** using a **geometric
Brownian motion (GBM)**.  
The idea is to generate one or more price trajectories from an **initial
price**, a **drift (mu)**, a **volatility (sigma)**, a **time step
(delta)**, and a **maturity (mat)**.

- **Define** a `sim_gbm` function that produces a price trajectory
  according to the GBM.
- **Choose** a set of parameters (S0, mu, sigma, delta, mat) and, if
  necessary, a **seed** for reproducibility.
- **Simulate** one or more trajectories.
- **Visualize** the simulated path (price curve over time).
- **Briefly comment** on the impact of mu and sigma on the dynamics
  (trend vs. dispersion).

``` r
sim_gbm <- function(S0, mu, sigma, delta, mat, seed = NULL) {
  if (!is.null(seed)) set.seed(seed)
  n_steps <- ceiling(mat / delta)
  Z <- rnorm(n_steps)
  S <- numeric(n_steps + 1); S[1] <- S0
  for (i in 1:n_steps) {
    S[i+1] <- S[i] * exp((mu - 0.5*sigma^2)*delta + sigma*sqrt(delta)*Z[i])
  }
  S
}
```

### 2) Black–Scholes Call (`bs_call`)

We compute the time-$t$ value of a European call with: $$
d_1=\frac{\ln(S/K)+\left(r+\tfrac{1}{2}\sigma^2\right)\tau}{\sigma\sqrt{\tau}},\quad
d_2=d_1-\sigma\sqrt{\tau},\quad
C=S\,\Phi(d_1)-K e^{-r\tau}\Phi(d_2).
$$ - If $\tau \le 0$, we return the payoff $\max(S-K,0)$. - Inputs:
current **price** $S$, **strike** $K$, **rate** $r$, **volatility**
$\sigma$, **time to maturity** $\tau$.

``` r
bs_call <- function(S, K, r, sigma, tau) {
  ifelse(tau <= 0,
         pmax(S - K, 0),
         {
           d1 <- (log(S/K) + (r + 0.5*sigma^2)*tau) / (sigma*sqrt(tau))
           d2 <- d1 - sigma*sqrt(tau)
           S*pnorm(d1) - K*exp(-r*tau)*pnorm(d2)
         })
}
```

### 3) Floor Function (`floor_at`)

The **floor** is the minimum wealth allowed at any time $t$, set to the
present value of the guaranteed terminal amount $K$: $$
\text{Floor}(t)=K\,e^{-r\,(T-t)}.
$$ This represents what can be locked in by investing safely from $t$ to
$T$.

``` r
floor_at <- function(K, r, T, t) K * exp(-r * (T - t))  # t en années
```

### 4) CPPI Terminal Wealth (`cppli_path_terminal`)

We implement **Constant Proportion Portfolio Insurance** with multiplier
$m$: - **Cushion** at time $t_i$: $\max(0, W_i - \text{Floor}_i)$. -
**Risky exposure**: $\min(W_i, m \times \text{cushion})$ (capped by
current wealth). - **Safe allocation**: $W_i - \text{risky}$, growing at
$e^{r\Delta t}$. - Over each period, wealth updates with the realized
equity return and the safe growth. - The function returns **terminal
wealth** over the provided equity path.

``` r
cppli_path_terminal <- function(S_path, K, r, delta, m = 4, W0 = 100, T = 1) {
  n <- length(S_path)             # n = n_steps + 1
  W <- numeric(n); W[1] <- W0
  for (i in 1:(n-1)) {
    t_i <- (i-1) * delta                     
    Floor_i <- floor_at(K, r, T, t_i)         
    R_eq <- S_path[i+1]/S_path[i] - 1        
    
    cushion <- max(0, W[i] - Floor_i)
    risky   <- min(W[i], m * cushion)         
    safe    <- W[i] - risky
    
    W[i+1] <- risky * (1 + R_eq) + safe * exp(r * delta)
  }
  W[n]
}
```

### 5) Single-Path Illustration & Plot

- We simulate one **GBM** equity path (already defined via `sim_gbm`).
- We build the **time grid** and corresponding **time to maturity**
  $\tau$.
- We compute and plot four series:
  - **Equity** (the raw simulated price),
  - **Floor** (minimum admissible value over time),
  - **OBPI** (floor + BS call value),
  - **CPPI** (wealth from CPPI rebalancing).

``` r
OBPI_value_T <- function(S_T, K) K + pmax(S_T - K, 0)

seed_used <- 1234
S_path_demo <- sim_gbm(S0, mu, sigma, delta, mat, seed = seed_used)
t_grid <- seq(0, mat, by = delta); t_grid <- t_grid[1:length(S_path_demo)]
T_total <- max(t_grid); tau <- T_total - t_grid
Floor_path <- floor_at(strike, rf, T_total, t_grid)
Equity_demo <- S_path_demo
OBPI_demo   <- Floor_path + bs_call(Equity_demo, strike, rf, sigma, tau)
CPPI_demo   <- {
  Wn <- numeric(length(S_path_demo))
  # reconstruire la trajectoire complète pour la démo :
  n <- length(S_path_demo); W <- numeric(n); W[1] <- S0
  for (i in 1:(n-1)) {
    t_i <- (i-1)*delta
    F_i <- floor_at(strike, rf, T_total, t_i)
    R_eq <- S_path_demo[i+1]/S_path_demo[i] - 1
    cushion <- max(0, W[i] - F_i)
    risky <- min(W[i], m_cppi * cushion)
    safe  <- W[i] - risky
    W[i+1] <- risky*(1+R_eq) + safe*exp(rf*delta)
  }
  W
}

df_plot <- tibble(
  t = t_grid,
  Equity = Equity_demo,
  Floor  = Floor_path,
  OBPI   = OBPI_demo,
  CPPI   = CPPI_demo
) |>
  pivot_longer(-t, names_to = "strategy", values_to = "value")

ggplot(df_plot, aes(t, value, color = strategy, linetype = strategy)) +
  geom_line(linewidth = 1) +
  labs(title = "Single-path illustration (Equity / Floor / OBPI / CPPI)",
       subtitle = paste("Seed =", seed_used),
       x = NULL, y = "Value") +
  theme_minimal(base_size = 12) +
  theme(legend.position = "top")
```

![](opba,-cppi_files/figure-gfm/unnamed-chunk-6-1.png)<!-- -->

### Interpretation of the Chart

The chart above illustrates the evolution of four strategies—Equity,
Floor, OBPI, and CPPI—along a single simulated equity path (with seed =
1234). Equity (green dashed line) represents the underlying risky asset.
It fluctuates freely according to market dynamics and can decline
significantly over time. Floor (blue dashed line) is the minimum
guaranteed value at each point in time. It grows smoothly as time
passes, reflecting the accumulation of the risk-free rate until
maturity. OBPI (purple dashed line) combines the floor with a call
option on the equity. It captures part of the upside when the market
rises while preserving the floor protection. Hence, it always stays
above the floor and tends to follow the equity when it performs well.
CPPI (red solid line) dynamically allocates wealth between the risky
asset and the risk-free asset. Here, it stays close to the floor because
the equity underperforms overall, which triggers a reduction in risky
exposure over time. In summary: The Floor provides full capital
protection. The OBPI offers protection with limited upside
participation. The CPPI adapts to market movements: it invests more in
equity when performance is good and retreats to safety when prices fall.
The Equity itself remains the most volatile and unprotected position.
This plot effectively shows how portfolio insurance strategies (OBPI and
CPPI) stabilize wealth compared to the raw equity path, at the cost of
reduced participation in strong upturns.

## Monte Carlo Simulation (10,000 paths)

We run a Monte Carlo over $N=$ `nsimul` simulated GBM paths. For each
path we record: - **Equity_T**: terminal equity price, - **OBPI_T**:
$K+\max(S_T-K,0)$ (option-based insurance at $T$), - **CPPI_T**:
terminal wealth from CPPI rebalancing.

We then compute three indicators for each approach: - **Annualised
return** (simple annualised mean return), - **Volatility** (std. dev. of
simple returns), - **Insurance rate** (share of paths finishing
$\ge K$).

``` r
set.seed(123)
n_steps <- mat / delta
equity_T <- numeric(nsimul)
obpi_T   <- numeric(nsimul)
cppi_T   <- numeric(nsimul)

for (i in 1:nsimul) {
  S_path <- sim_gbm(S0, mu, sigma, delta, mat)
  equity_T[i] <- S_path[n_steps + 1]
  obpi_T[i]   <- OBPI_value_T(S_path[n_steps + 1], strike)
  cppi_T[i]   <- cppli_path_terminal(S_path, strike, rf, delta, m_cppi, S0, T = mat)
}

annualised_return <- function(values, S0, T) mean(values / S0 - 1) / T

results <- tibble(
  Indicators = c("Annualised return", "Volatility", "Insurance rate"),
  Equity = c(
    annualised_return(equity_T, S0, mat),
    sd(equity_T / S0 - 1),
    mean(equity_T >= strike)
  ),
  OBPI = c(
    annualised_return(obpi_T, S0, mat),
    sd(obpi_T / S0 - 1),
    mean(obpi_T >= strike)
  ),
  CPPI = c(
    annualised_return(cppi_T, S0, mat),
    sd(cppi_T / S0 - 1),
    mean(cppi_T >= strike)
  )
)

print(results)
```

    ## # A tibble: 3 × 4
    ##   Indicators        Equity  OBPI   CPPI
    ##   <chr>              <dbl> <dbl>  <dbl>
    ## 1 Annualised return 0.0528 0.111 0.0242
    ## 2 Volatility        0.212  0.154 0.0388
    ## 3 Insurance rate    0.563  1     0.991

### 4) Summary Table of Indicators

| Indicator | Meaning |
|----|----|
| **Annualised return** | Average yearly performance across 10,000 simulations. OBPI achieves the highest average return (~11%) due to its exposure to equity upside, while CPPI remains more conservative (~2.4%). The raw Equity shows a mid-level return (~5.3%). |
| **Volatility** | Standard deviation of returns — a measure of risk. Equity is the most volatile (~21%), OBPI reduces risk (~15%), and CPPI is the most stable (~4%). |
| **Insurance rate** | Percentage of simulations with terminal value ≥ 100. Equity only protects capital in ~56% of cases, OBPI achieves **100% protection**, and CPPI reaches **~99%**, confirming its near-perfect floor preservation. |

### 1) Equity Prices at Maturity

The histogram of **Equity prices** shows the distribution of terminal
stock prices generated by the **Geometric Brownian Motion (GBM)**.  
It follows a **lognormal shape**, centered around the initial value
$S_0 = 100$.  
Most outcomes lie between **80 and 140**, but the distribution has a
**right tail**, meaning that large upward movements are possible though
rare.  
This asymmetry reflects the nature of equity markets: prices can rise
substantially, but cannot fall below zero.

``` r
hist(equity_T, breaks = 80, col = "gray", main = "Equity prices at maturity",
     xlab = "Equity", border = "black")
```

![](opba,-cppi_files/figure-gfm/unnamed-chunk-8-1.png)<!-- -->

``` r
cat("\n\n\\newpage\n\n")
```

    ## 
    ## 
    ## \newpage

### 2) OBPI Prices at Maturity

The **OBPI (Option-Based Portfolio Insurance)** histogram shows a strong
spike at **100**, which corresponds to the **guaranteed floor value**
$K = 100$.  
This indicates that when the underlying equity performs poorly, the call
option expires worthless and the investor simply receives the guaranteed
capital.  
When markets perform well, the payoff increases due to the **option
component**, generating a **long right tail**.  
In short, OBPI provides **full capital protection** with **partial
participation** in market upside.

``` r
hist(obpi_T, breaks = seq(strike - 0.5, max(obpi_T) + 1, by = 1),
     col = "gray", main = "OBPI prices at maturity", xlab = "OBPI", border = "black")
```

![](opba,-cppi_files/figure-gfm/unnamed-chunk-9-1.png)<!-- -->

``` r
cat("\n\n\\newpage\n\n")
```

    ## 
    ## 
    ## \newpage

### 3) CPPI Prices at Maturity

The **CPPI (Constant Proportion Portfolio Insurance)** histogram looks
similar to OBPI but slightly different:  
- A **large mass near 100**, representing cases where the portfolio
locked in the floor.  
- A **small right tail**, showing some participation in rising markets,
though generally less than OBPI.  
Occasionally, small values below 100 can appear (“micro-breaches”) when
markets drop too fast between rebalancing dates.  
Overall, CPPI ensures **almost full protection** with **moderate upside
participation** thanks to its dynamic allocation.

``` r
hist(cppi_T, breaks = 80, col = "gray", main = "CPPI prices at maturity",
     xlab = "CPPI", border = "black")
```

![](opba,-cppi_files/figure-gfm/unnamed-chunk-10-1.png)<!-- -->

``` r
nsimul <- 10000
mu <- 0.05 
sigma <- 0.20
rf <- 0.02 
mat <- 1
strike <- 100 
m_cppi <- 4 
S0 <- 100 
delta <- 1/12 
n_steps <- mat / delta
```
