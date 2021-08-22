## --------------------------------------------------------------------------- #
## progDHM2013.R
##
## Project: dHM2013
## Purpose: Run the estimation procedure for d'Haultfoeuille + Maurel (2013)
## Author: Oliver Cassagneau-Francis
## Date: Mon Aug 16 10:11:33 2021
## --------------------------------------------------------------------------- #
## Notes:

#' Run the estimation procedure for d'Haultfoeuille + Maurel (2013)
#'
#' This function runs the d'Haultfoeuille + Maurel (2013) estimation procedure
#' on the data in dt, returning a data.table containing the estimates.
#'
#' @param dt A data.table containing the data on which to estimate the model.
#' @param varList A list of variables on which to estimate the model.
#' @param ivY1,ivY0 Character variables containing the names of the excluded
#'   variables from each of the equations. Default (`NULL`) is no excluded
#'   variables.
progDHM2013 <- function(dt, varList, bw = .5, allVarsD = TRUE,
                        x1 = "x1", d = "d", y = "y",
                        ivY1 = NULL, ivY0 = NULL, ivD = NULL) {

  # ---- Setup ----
  n <- nrow(dt)
  setnames(dt, old = c(x1, d, y), new = c("x1", "d", "y"), skip_absent = TRUE)
  varList <- c("x1", varList[-match(x1, varList)])

  # ---- Step 1 ----
  #* Probit model ####
  if (is.null(ivD)) {
    step1Formula <- as.formula(paste0("d~", paste0(varList, collapse = "+")))
  } else {
    step1Formula <- as.formula(
      paste0("d~", paste0(varList[-match(ivD, varList)], collapse = "+"))
    )
  }

  step1Probit <- glm(
    formula = step1Formula,
    data = dt
  )
  p <- length(step1Probit$coefficients)-1

  #* Coppejans (2003) estimator ####
  sigma_p <- sd(step1Probit$residuals)

  # sigma_lb <- min(.5 * sigma_p / (n^.2), 0.5)

  # c2Psi_ub <- min((exp(-.5)/sqrt(2*pi)) * max(1/4, (.5 * sigma_p / (n^.2))^(-1.5)), 1000)

  k <- 3

  pi_k <- rep(1/k, k-1)

  mu <- rep(0, k)
  sigma <- sigma_p

  zeta <- step1Probit$coefficients / step1Probit$coefficients["x1"]

  x <- model.matrix(
    object = step1Probit$formula,
    data = step1Probit$model
  )

  d <- step1Probit$y

  Q <- function(zeta, pik, mu, sigma, x, y) {
    mean((y - (1 - (
      pik[[1]] * pnorm(- x %*% zeta, mean = mu[[1]], sd = sigma) +
        pik[[2]] * pnorm(- x %*% zeta, mean = mu[[2]], sd = sigma) +
        (1 - pik[[1]] - pik[[2]]) * pnorm(- x %*% zeta, mean = mu[[3]], sd = sigma)
    )))^2)
  }

  Q2 <- function(theta) {
    Q(zeta = c(theta[[1]], 1, theta[2:p]), pik = theta[(p+1):(p+2)],
      mu = theta[(p+3):(p+5)],
      sigma = theta[(p+6)], x = x, y = d)
  }

  step1Coppejans <- optim(
    par = c(zeta[c(1, 3:(p+1))], pi_k, mu, sigma),
    fn = Q2, method = "BFGS", control = list(maxit = 500)
  )

  dtEstimates <- data.table(
    varName = c("x1", names(step1Coppejans$par[2:p])),
    zeta0 = c(1, step1Coppejans$par[2:p])
  )

  # ---- Step 2: Newey (2009) estimator ----
  xZetaHat <- (x[, 2:(p+1)] %*% c(1, step1Coppejans$par[2:p]))[, 1]

  dt[, vhat1 := xZetaHat]
  dt[, paste0("vhat", 2:6) := .(vhat1^2, vhat1^3, vhat1^4, vhat1^5, vhat1^6)]
  dt[, imrVhat := dnorm(vhat1) / pnorm(-vhat1)]
  dt[, `:=`(
    leg1 = imrVhat,
    leg2 = .5 * (3 * imrVhat^2 - 1),
    leg3 = .5 * (5 * imrVhat^3 - 3 * imrVhat),
    leg4 = .125 * (35 * imrVhat^4 - 30 * imrVhat^2 + 3),
    leg5 = .125 *(53 * imrVhat^5 - 70 * imrVhat^3 + 15 * imrVhat),
    leg6 = .0625 * (231 * imrVhat^6 - 315 * imrVhat^4 + 105 * imrVhat^2 - 5)
  )]

  if (is.null(ivY1)) {
    step2d1Formula <- as.formula(
      paste0("y~", paste0(c(varList, paste0("leg", 1:6)),
                          collapse = "+"))
    )
  } else {
    step2d1Formula <- as.formula(
      paste0("y~", paste0(c(varList[-match(ivY1, varList)], paste0("leg", 1:6)),
                          collapse = "+"))
    )
  }

  step2d1 <- lm(
    formula = step2d1Formula,
    data = dt[d == TRUE]
  )

  if (is.null(ivY0)) {
    step2d0Formula <- as.formula(
      paste0("y~", paste0(c(varList, paste0("leg", 1:6)),
                          collapse = "+"))
    )
  } else {
    step2d0Formula <- as.formula(
      paste0("y~", paste0(c(varList[-match(ivY0, varList)], paste0("leg", 1:6)),
                          collapse = "+"))
    )
  }

  step2d0 <- lm(
    formula = step2d0Formula,
    data = dt[d == FALSE]
  )

  dtEstimates <- dtEstimates %>%
    merge(
      data.table(
        varName = names(step2d0$coefficients)[
          !(names(step2d0$coefficients) %like% "leg|(Intercept)")],
        beta0 = step2d0$coefficients[
          !(names(step2d0$coefficients) %like% "leg|(Intercept)")]
      ), by = "varName", all = TRUE
    ) %>%
    merge(
      data.table(
        varName = names(step2d1$coefficients)[
          !(names(step2d1$coefficients) %like% "leg|(Intercept)")],
        beta1 = step2d1$coefficients[
          !(names(step2d1$coefficients) %like% "leg|(Intercept)")]
      ), by = "varName", all = TRUE
    )
  dtEstimates[is.na(dtEstimates)] <- 0

  # ---- Step 3: estimating delta0 and gamma0 ----
  #* Calculate V ----
  sigmaU <- sd(xZetaHat)
  d <- as.numeric(d)

  K <- function(u, xZ, h) {
    uu <- (u - xZ) / h
    ifelse(abs(uu) <= 1, (15/16) * (1 - uu^2)^2, 0)
  }

  q0 <- function(u) {
    uMat <- matrix(rep(u, each = length(xZetaHat)),
                   nrow = length(xZetaHat), ncol = length(u))
    Ku <- K(uMat, xZ = xZetaHat, h = bw * sigmaU * n^(-1/7))
    if (length(u) > 1) {
      res <- colSums(d * Ku) / colSums(Ku)
    } else {
      res <- sum(d * Ku) / sum(Ku)
    }
    # res[is.na(res)] <- 0
    return(res)
  }

  u0 <- min(xZetaHat)

  intq0 <- function(u, subdiv = 550) {
    res <- list()
    for (i in 1:length(u)) {
      res[[i]] <- integrate(q0, lower = u0, upper = u[[i]], subdivisions = subdiv)$value
    }
    unlist(res)
  }

  iq0xZ <- intq0(xZetaHat)
  V <- d * xZetaHat - iq0xZ

  ## Calculate instruments Z = (1, h1 h2)
  aModel <- glm(d ~ xZetaHat)
  a0 <- aModel$coefficients[[1]]
  a1 <- aModel$coefficients[[2]]

  h1 <- pnorm(a0 + a1 * xZetaHat)
  h2 <- xZetaHat * h1 - iq0xZ

  Z <- cbind(1, h1, h2)

  ## Calculate theta = (lambda, delta0, alpha0)
  b0 <- dtEstimates[, beta0]
  b1 <- dtEstimates[, beta1]
  xWage <- model.matrix(
    object = as.formula(paste0("d~", paste0(varList, collapse = "+"))),
    data = dt
  )[, 2:(p+1)]

  b <- d * matrix(rep(b1, each = n), nrow = n) + (1-d) * matrix(rep(b0, each = n), nrow = n)

  eps <- dt[, y] - rowSums(xWage * b)

  W <- cbind(1, d, V)

  theta <- solve(t(Z) %*% W / n) %*% (t(Z) %*% eps / n)

  dtEstimates[, `:=`(
    gamma0 = beta1 - beta0 + theta[[3]] * zeta0,
    alpha0 = theta[[3]], lambda0 = theta[[1]], delta0 = theta[[2]]
  )]

  return(dtEstimates)
}
