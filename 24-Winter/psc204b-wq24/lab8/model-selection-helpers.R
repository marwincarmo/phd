#===========================================================
# this document defines functions to help 
# us compute information that is useful for model selection.
#
# Josue Rodriguez - PSC 204B - 2/16/22
#===========================================================


#' Mallow's Cp
#'
#' @param lm_object Your model that you fit with `lm()`  
#'
#' @details 
#'   * n: number of observations
#'   * RSS: residual sum of squares
#'   * sigma.hat: estimate of the residual variance
#'   * d: the number of predictors + the intercept
#'
mcp <- function(lm_object) {
  n <- length(resid(lm_object))
  RSS <- sum(resid(lm_object)^2)
  d <- length(lm_object$coefficients)
  sigma2.hat <- sigma(lm_object)^2
  
  # formula: https://en.wikipedia.org/wiki/Mallows%27s_Cp#Alternative_definition
  mcp <- (1/n) * (RSS + 2*d*sigma2.hat)
  return(mcp)
}




#' adjusted R^2
#'
#' @param lm_object Your model that you fit with `lm()`
#' 
#' @details 
#'   * n: number of observations
#'   * RSS: residual sum of squares
#'   * d: the number of predictors + the intercept
#'   * r2: the original R^2
#' 
adj_r2 <- function(lm_object) {
  n <- length(resid(lm_object))
  d <- length(lm_object$coefficients)
  r2 <- summary(lm_object)$r.squared
  
  # formula: https://en.wikipedia.org/wiki/Coefficient_of_determination#Adjusted_R2
  adj_r2 <- 1 - (1 - r2)*((n - 1) / (n - d))
  return(adj_r2)
}




#' aic
#'
#' @param lm_object Your model that you fit with `lm()`
#' 
#' @details 
#'   * n: number of observations
#'   * RSS: residual sum of squares
#'   * d: the number of predictors + the intercept
#' 
aic <- function(lm_object) {
  n <- length(resid(lm_object))
  RSS <- sum(resid(lm_object)^2)
  d <- length(lm_object$coefficients)
  
  # formula can be seen at: en.wikipedia.org/wiki/Akaike_information_criterion#Comparison_with_least_squares
  aic <- 2*d + n*log(RSS)
  return(aic)
}




#' bic
#'
#' @param lm_object Your model that you fit with `lm()`
#' 
#' @details 
#'   * n: number of observations
#'   * RSS: residual sum of squares
#'   * d: the number of predictors + the intercept
#' 
bic <- function(lm_object) {
  n <- length(resid(lm_object))
  RSS <- sum(resid(lm_object)^2)
  d <- length(lm_object$coefficients)
  
  # formula can be seen at: https://en.wikipedia.org/wiki/Bayesian_information_criterion#Gaussian_special_case
  bic <- d*log(n) + n*log(RSS/n)
  return(bic)
}




#' loocv
#'
#' @param lm_object Your model that you fit with `lm()`
#' 
#' @details 
#'   * r_i: residuals
#'   * h_i: leverage values
#' 
loocv <- function(lm_object) {
  r_i <- residuals(lm_object)
  h_i <- influence(lm_object)$hat
  
  loocv <- mean((r_i / (1 - h_i))^2)
  return(loocv)
}
