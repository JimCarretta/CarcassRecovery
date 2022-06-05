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

CarcassDetection <- function(Abund, CV.Abund, carcassDetected, ASR, removals, propResidency) {  
  
    nsim <- 1000
    ASR <- seq(ASR-0.02, ASR+0.02, 0.001)
    Sim.Abund <- rlnorm(nsim, log(Abund) - 0.5 * log(CV.Abund^2 + 1), sqrt(log(CV.Abund^2 + 1)))
    
    carcassExpected <- sample(Sim.Abund, nsim, replace=TRUE) * (1 - sample(ASR, nsim, replace=TRUE)) * propResidency + removals
    L95Expected <- quantile(carcassExpected, probs=c(0.025))
    U95Expected <- quantile(carcassExpected, probs=c(0.975))
    
    MeanDetected <- carcassDetected/mean(carcassExpected)
    L95Detected <- carcassDetected/quantile(carcassExpected, probs=c(0.975))
    U95Detected <- carcassDetected/quantile(carcassExpected, probs=c(0.025))
    
    CarcassSummary <- as.list(c(Abund, CV.Abund, carcassDetected, mean(carcassExpected), L95Expected, U95Expected, MeanDetected, L95Detected, U95Detected))  
    row.names(CarcassSummary) <- NULL
    CarcassSummary
  
} 


# example data frame

df <- cbind.data.frame(seq(1990,2000,1), rnorm(11, 2500, 100), rnorm(11, 0.50, 0.1), as.integer(rnorm(11, 7, 2)), rep(0.96, 11), as.integer(rnorm(11, 20, 5)), rep(0.583, 11))
names(df) <- c("Year", "Abund", "CV.Abund", "carcassDetected", "ASR", "removals", "propResidency")

empty.df <- as.data.frame(matrix(NA, nrow=11, ncol=11))


for (i in 1:nrow(df)) {  Year <- df$Year[i]
                          Abund <- df$Abund[i]
                           CV.Abund <- df$CV.Abund[i]
                            carcassDetected <- df$carcassDetected[i]
                             ASR <- df$ASR[i]
                              removals <- df$removals[i]
                               propResidency <- df$propResidency[i] 
                           
                           temp <- CarcassDetection(Abund, CV.Abund, carcassDetected, ASR, removals, propResidency) 
                           empty.df[i,] <- cbind.data.frame(Year, temp, removals) }

names(empty.df) <- c("Year", "Abund", "CV.Abund", "carcassDetected", "carcassExpected", "L95Expected", "U95Expected", "MeanDetected", "L95Detected", "U95Detected", "removals")



