prob_dist <- function(type, x, params, cumulative = FALSE) {
  if (!type %in% c("normal", "binomial", "poisson")) {
    stop("Supported types are: 'normal', 'binomial', 'poisson'")
  }

  switch(type,
         normal = {
           if (!all(c("mean", "sd") %in% names(params))) {
             stop("Parameters for normal distribution must include 'mean' and 'sd'")
           }
           if (cumulative) {
             pnorm(x, mean = params$mean, sd = params$sd)
           } else {
             dnorm(x, mean = params$mean, sd = params$sd)
           }
         },
         binomial = {
           if (!all(c("size", "prob") %in% names(params))) {
             stop("Parameters for binomial distribution must include 'size' and 'prob'")
           }
           if (cumulative) {
             pbinom(x, size = params$size, prob = params$prob)
           } else {
             dbinom(x, size = params$size, prob = params$prob)
           }
         },
         poisson = {
           if (!"lambda" %in% names(params)) {
             stop("Parameter for poisson distribution must include 'lambda'")
           }
           if (cumulative) {
             ppois(x, lambda = params$lambda)
           } else {
             dpois(x, lambda = params$lambda)
           }
         })
}



