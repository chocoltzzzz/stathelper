#' Probability Distribution Function Calculator
#'
#' This function calculates the probability or cumulative probability for the given type of distribution:
#' normal, binomial, or poisson. It supports both probability density and cumulative probability
#' calculations based on the provided input.
#'
#' @param type A character string specifying the type of distribution.
#' Must be one of: "normal", "binomial", or "poisson".
#' @param x A numeric value or vector at which the probability mass or density function is evaluated.
#' @param params A named list containing the parameters for the selected distribution.
#'   - For "normal", provide `mean` and `sd`.
#'   - For "binomial", provide `size` and `prob`.
#'   - For "poisson", provide `lambda`.
#' @param cumulative A logical value. If `TRUE`, the cumulative distribution function (CDF) is returned;
#'   if `FALSE`, the probability density or mass function (PDF or PMF) is returned.
#'
#' @return A numeric value or vector representing the evaluated probability (or cumulative probability)
#'   based on the distribution type and the provided parameters.
#'
#' @details
#' The function uses the following distribution functions from R:
#' - `dnorm` and `pnorm` for the normal distribution.
#' - `dbinom` and `pbinom` for the binomial distribution.
#' - `dpois` and `ppois` for the Poisson distribution.
#'
#' @examples
#' # Normal distribution with mean = 0, sd = 1, calculate the probability density at x = 0
#' prob_dist("normal", 0, list(mean = 0, sd = 1))
#'
#' # Binomial distribution with size = 10, prob = 0.5, calculate cumulative probability at x = 3
#' prob_dist("binomial", 3, list(size = 10, prob = 0.5), cumulative = TRUE)
#'
#' # Poisson distribution with lambda = 2, calculate the probability mass at x = 3
#' prob_dist("poisson", 3, list(lambda = 2))
#'
#' @export

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



