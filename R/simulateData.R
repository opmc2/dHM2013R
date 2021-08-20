## --------------------------------------------------------------------------- #
## simulateData.R
##
## Project: UK HE EM determinants
## Purpose: Simulate data on which to run dH+M(2013)
## Author: Oliver Cassagneau-Francis
## Date: Wed Aug 18 14:26:33 2021
## --------------------------------------------------------------------------- #
## Notes:

#' Simulate data to test my code for implementing the dH+M (2013) estimation.
#'
#' @param n The number of observations to simulate.
#' @param beta0 The coefficients in the outcome equation for d = 0.
#' @param beta1 The coefficients in the outcome equation for d = 1.
#' @param gamma0 The coefficients in the decision equation.
#' @param delta0 The constant in the decision equation.
#' @return The sum of \code{x} and \code{y}.
simulateData <- function(
  n = 1000,
  beta0 = c(0, 1, 1),
  beta1 = c(2, 0, 0.5),
  gamma0 = c(-0.5, 0.5, -0.8),
  delta0 = 0.8
) {
  dtSim <- data.table(
    id = 1:n,
    x1 = runif(n, 0, 4),
    x2 = runif(n, 0, 4),
    x3 = as.numeric(purrr::rbernoulli(n, p = 0.5))
  )

  eta <- MASS::mvrnorm(n, mu = c(0,1), Sigma = matrix(c(1, .5, .5, 1), nrow = 2))
  dtSim[, paste0("eta", 0:1) := .(eta[, 1], eta[, 2])]

  x1 <- dtSim[, x1]
  x2 <- dtSim[, x2]

  listNu <- list()
  for (i in 1:n) {
    listNu[[i]] <- MASS::mvrnorm(
      mu = c(0,0),
      Sigma = matrix(
        c(
          exp(x2[[i]]/5), .5*sqrt(exp(x2[[i]]/5 + x1[[i]]/5)),
          .5*sqrt(exp(x2[[i]]/5 + x1[[i]]/5)), exp(x1[[i]]/5)
        ),
        nrow = 2
      )
    )
  }
  nu <- matrix(unlist(listNu), ncol = 2, byrow = TRUE)

  dtSim[, paste0("nu", 0:1) := .(nu[, 1], nu[, 2])]

  dtSim[, `:=`(
    y0 = x2 * beta0[[2]] + x3 * beta0[[3]] + eta0 + nu0,
    y1 = x1 * beta1[[1]] + x3 * beta1[[3]] + eta1 + nu1,
    d = (-delta0 + x1 * (beta1[[1]] - gamma0[[1]]) +
           x2 * (-beta0[[2]] - gamma0[[2]]) +
           x3 * (beta1[[3]] - beta0[[3]] - gamma0[[3]]) +
           eta1 - eta0 > 0)
  )]

  dtSim[, y := fcase(d == TRUE, y1, d == FALSE, y0)]

  return(dtSim)
}
