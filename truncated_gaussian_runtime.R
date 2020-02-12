library(tmg)
library(TruncatedNormal)
library(tictoc)

# d = 100 ----
n <- 1000
d <- 3000
mu <- rep(0, d)
Sigma <- diag(d)
lb <- rep(0, d)
ub <- rep(Inf, d)

tic()
botev_samples <- TruncatedNormal::rtmvnorm(n, mu, Sigma, lb, ub)
botev_time <- toc()

init <- rep(1, d)
f <- diag(d)
g <- lb
tic()
pp_samples <- tmg::rtmg(n, Sigma, mu, init, f, g)
toc()

