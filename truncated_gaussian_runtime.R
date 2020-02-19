library(tmg)
library(TruncatedNormal)
library(cdists)
library(tictoc)

library(data.table)

# ptnorm <- function(x, lb = -Inf, ub = Inf) {
#   # 1d truncated normal
#   truncdist::ptrunc(x, spec = "norm", a = lb, b = ub)
# }

n <- 1000
dimensions <- c(100, 500, 1000, 2000, 4000, 6000, 8000, 10000)


# Identity covariance
esses <- vector(mode = "list", length = length(dimensions))
names(esses) <- dimensions
samples <- vector(mode = "list", length = length(dimensions))
names(samples) <- dimensions
times <- vector(mode = "list", length = length(dimensions))
names(times) <- dimensions
dists <- vector(mode = "list", length = length(dimensions))
names(dists) <- dimensions

set.seed(1)
uni_sample <- tmvtnorm::rtmvnorm(n, 0, 1, 0)
for (d in dimensions) {
  print(paste("--- Dimension ", d, " ---"))
  mu <- rep(0, d)
  Sigma <- diag(d)
  lb <- rep(0, d)
  ub <- rep(Inf, d)
  
  # print("Botev")
  # tic()
  # botev_samples <- TruncatedNormal::rtmvnorm(n, mu, Sigma, lb, ub)
  # botev_time <- toc()
  # botev_ess <- coda::effectiveSize(t(botev_samples))
  # botev_dist <- apply(botev_samples, 2, 
  #                         function(samp) ks.test(samp, uni_sample)$statistic)
  
  print("Exact")
  tic()
  exact_samples <- cdists::rtmvn(n, Sigma, mu, lb, ub)
  exact_time <- toc()
  exact_ess <- coda::effectiveSize(exact_samples)
  exact_dist <- apply(exact_samples, 2,
                          function(samp) ks.test(samp, uni_sample)$statistic)
  
  entry_name <- as.character(d)
  
  # esses[[entry_name]] <- data.frame(botev = botev_ess, exact = exact_ess, 
  #                                   d = d, dim_idx = 1:d)
  # times[[entry_name]] <- data.frame(
  #   botev = with(botev_time, toc - tic),
  #   exact = with(exact_time, toc - tic), d = d)
  # dists[[entry_name]] <- data.frame(botev = botev_dist, exact = exact_dist, 
  #                                   d = d, dim_idx = 1:d)
  
  esses[[entry_name]] <- data.frame(exact = exact_ess, d = d, dim_idx = 1:d)
  times[[entry_name]] <- data.frame(exact = with(exact_time, toc - tic), d = d)
  dists[[entry_name]] <- data.frame(exact = exact_dist, d = d, dim_idx = 1:d)
}

save(esses, samples, times, dists, file = "experiments_2020_2_18.RData")


# visualization ----
library(ggplot2)
library(ggridges)

rhmc <- R.matlab::readMat("rhmc_2020_2_18.mat")
rhmc_samples <- 
  sapply(rhmc$samples, function(x) rbind(t(x[[1]]), ncol(x[[1]])))
rhmc_dims <- as.numeric(rhmc$dimensions)

rhmc_time_df <- 
  data.frame(variable = "rhmc", value = rhmc$times, d = dimensions)
rhmc_dist <- sapply(rhmc_samples, function(x) {
  apply(x, 2, function(samp) 
    ks.test(samp, uni_sample)$statistic)
  })
rhmc_dist_df <- mapply(function(distances, d) {
  data.frame(d = d, variable = "rhmc", value = distances)
}, rhmc_dist, rhmc_dims, SIMPLIFY=FALSE)
rhmc_dist_df <- do.call(rbind, rhmc_dist_df)

dist_df <- do.call(rbind, dists)
dist_df <- melt(data.table(dist_df[, -4]), id.vars = "d")
times_df <- melt(data.table(do.call(rbind, times)), id.vars = "d")

ggplot(dist_df, aes(x = variable, y = value)) +
  geom_violin() +
  facet_wrap(vars(d), scales = "free")
ggplot(dist_df, aes(x = dim_idx, y = botev)) +
  geom_point() +
  facet_wrap(vars(d), scales = "free")

dist_df <- do.call(rbind, dists)
dist_df <- melt(data.table(dist_df[, -3]), id.vars = "d")
all_dist_df <- rbind(dist_df, rhmc_dist_df)
ggplot(all_dist_df, 
       aes(x = d, y = value, group = interaction(variable, d), fill = variable)) +
  geom_boxplot(width = 1000) +
  labs(y = "KS distance to univariate truncated normal", x = "dimension")


ggplot(rbind(rhmc_time_df, times_df), 
       aes(x = d, y = value, color = variable)) + 
  geom_point(size = 3) + geom_line() + 
  labs(x = "Dimension", y = "Seconds") + 
  theme(legend.title = element_blank())

par(mfrow = c(5, 1))
plot()

init <- rep(1, d)
f <- diag(d)
g <- lb
tic()
pp_samples <- tmg::rtmg(n, Sigma, mu, init, f, g)
toc()



