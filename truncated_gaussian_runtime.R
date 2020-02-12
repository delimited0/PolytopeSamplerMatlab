library(tmg)
library(TruncatedNormal)
library(cdists)
library(tictoc)


# Identity covariance

# d = 100 ----
n <- 1000
d <- 1000
mu <- rep(0, d)
Sigma <- diag(d)
lb <- rep(0, d)
ub <- rep(Inf, d)

tic()
botev_samples <- TruncatedNormal::rtmvnorm(n, mu, Sigma, lb, ub)
botev_time <- toc()
botev_ess <- coda::effectiveSize(botev_samples)

tic()
exact_samples <- cdists::rtmvn(n, Sigma, mu, lb, ub)
exact_time <- toc()
exact_ess <- coda::effectiveSize(exact_samples)

init <- rep(1, d)
f <- diag(d)
g <- lb
tic()
pp_samples <- tmg::rtmg(n, Sigma, mu, init, f, g)
toc()

# d = 1000 ----
