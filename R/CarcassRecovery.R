#' @title CarcassRecovery
#'
#' @description
#' Estimate the fraction of cetacean carcasses documented.
#'
#' @usage CarcassRecovery(df)
#'
#' @param df an object of class 'data.frame'
#'
#' @author Jim Carretta <jim.carretta@noaa.gov>
#'
#' @export
#'
#
#
CarcassRecovery <- function(year, PopSize, CV.PopSize, carcasses, ASR, removals) {  
  
  year <- seq(1990, 2000, 1)
  PopSize <- rnorm(length(year), 5000, 100)
  CV.PopSize <- sample(seq(0.05, 0.95, 0.01), length(year), replace=TRUE)
  carcasses <- as.integer(rnorm(length(year), 25, 5))
  ASR <- seq(0.96, 0.98, 0.005)
  removals <- as.integer(rnorm(length(year), 5, 1))
  
                                   
                                   }