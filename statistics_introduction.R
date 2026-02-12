#' Stats Review for Master's Students Summer 2025
#' @description these are R code review for the Master's orientation Summer 2025
#' @author Pedro Izquierdo Lehmann
#' adapted from Joshua Agterbergm and Long Wang, AMS Department
#' 
#' 
#' Note: RStudio can be downloaded from:
#' https://www.rstudio.com/

#-------------------------One sample testing-----------------
#initialize variables:
n <- 100
mu <- 0
sigma <- 1

#generate 100 normally distributed rv with mean 0 and sd 1: 
set.seed(124) #set for reproducibility
x <- rnorm(n = n, mean = mu, sd = sigma)
summary(x)

# Confidence Interval for mu with known variance
#see formula in notes
alpha <- 0.05
#use built-in R functions qnorm, mean, etc.
mu_LB <- mean(x) - qnorm(alpha/2, lower.tail = FALSE) * sigma / sqrt(n)
mu_UB <-mean(x) + qnorm(alpha/2, lower.tail = FALSE) * sigma / sqrt(n)
cat('Confidence interval for mu:', mu_LB, mu_UB)

#' Testing for mu with known variance
#' define a function that tests the hypothesis
#' that mu_0 = .1 with a known sigma = 1 and alpha =.05
test_hypothesis_known_variance <- function(mu_0 = .1,dat = NULL,alpha = .05, sigma = 1) {
  #error handling:
  if (is.null(dat)) {
    error('data not provided')
  }
  
  toReturn <- list()
  #list access:
  toReturn[[1]] <- paste0('H0: mu_0 = ',mu_0)
  n <- length(dat)
  test_stat <- (mean(dat) - mu_0) / (sigma / sqrt(n))
  
  
  if (abs(test_stat) > qnorm(alpha/2, lower.tail = FALSE)){
    toReturn[[2]] <- 'Yes'
  } else {
    toReturn[[2]] <- 'No'
  }
  
  toReturn[[3]] <- 2 * pnorm(abs(test_stat), lower.tail = FALSE)
  names(toReturn) <- c('Null Hypothesis'
                              ,paste0("Do We Reject Null at alpha = ",alpha,"?")
                              , "p-Value")  
  return(toReturn)
                       
}

result <- test_hypothesis_known_variance(mu_0=.3,dat = x)
result
result <- test_hypothesis_known_variance(mu_0=.001,dat = x)
result


rm(list = ls())  #removes everything
#----------------------------------
#Now, we do not know the variance
#initialize variables:

n <- 100
mu <- 0
sigma <- 1

#generate 100 normally distributed rv with mean 0 and sd 1: 
set.seed(2) #set for reproducibility
x <- rnorm(n = n, mean = mu, sd = sigma)
summary(x)
alpha <- 0.05

# Confidence interval for mu with unknown variance
# however, we know the distribution is t-distributed
# so we can usej qt instead of qnorm
s2 <- sd(x)
mu_LB <- mean(x) - qt(alpha/2, df = n-1, lower.tail = FALSE) * sqrt(s2 / n)
mu_UB <-mean(x) + qt(alpha/2, df = n-1, lower.tail = FALSE) * sqrt(s2 / n)
cat('Confidence interval for mu:', mu_LB, mu_UB)

# C.I. for variance
sigma2_LB <- (n-1) * s2 / qchisq(alpha/2, df = n-1, lower.tail = FALSE)
sigma2_UB <- (n-1) * s2 / qchisq(alpha/2, df = n-1, lower.tail = TRUE)
cat('Confidence interval for sigma2:', sigma2_LB, sigma2_UB)

# Testing for mu with unknown variance
test_hypothesis_unknown_variance <- function(dat, mu_0 = .1,alpha = .05) {
  #error handling:
  if (is.null(dat)) {
    error('data not provided')
  }
  toReturn <- list()
  toReturn[[1]] <- paste0('H0: mu_0 = ',mu_0)
  s2 <- sd(dat)
  n <- length(dat)
  test_stat <- (mean(x) - mu_0) / (s2 / sqrt(n))
  if (abs(test_stat) > qt(alpha/2, df = n-1, lower.tail = FALSE)){
    toReturn[[2]] <- 'Yes'
  }else {
    toReturn[[2]] <- 'No'
  }
  toReturn[[3]] <- 2 * pt(abs(test_stat), df = n-1, lower.tail = FALSE)
 
  names(toReturn) <- c('Null Hypothesis'
                              ,paste0("Do We Reject Null at alpha = ",alpha,"?")
                              , "p-Value")
  
  return(toReturn)
}

result <- test_hypothesis_unknown_variance(dat = x, mu_0 = .3)
result
result <- test_hypothesis_unknown_variance(dat = x, mu_0 = .0001)
result


#-------------------------Two sample testing-----------------
rm(list = ls())
# true parameters
mu_X <- 1
mu_Y <- 1
sigma_X <- 1
sigma_Y <- 2
n_X <- 1000
n_Y <- 1000

# generate data
data_X <- rnorm(n_X, mean = mu_X, sd = sigma_X)
data_Y <- rnorm(n_Y, mean = mu_Y, sd = sigma_Y)
#generate histograms of data:
hist(data_X, breaks = 20, freq = FALSE)
hist(data_Y, breaks = 20, freq = FALSE)
# point estimation
x_bar <- mean(data_X)
y_bar <- mean(data_Y)
cat("point estimation:", x_bar - y_bar)

# interval estimation
LCB <- x_bar - y_bar + qnorm(0.025) * sqrt(sigma_X^2 / n_X + sigma_Y^2 / n_Y)
UCB <- x_bar - y_bar + qnorm(0.975) * sqrt(sigma_X^2 / n_X + sigma_Y^2 / n_Y)
cat("interval estimation", LCB, UCB)

s_X <- sd(data_X)
s_Y <- sd(data_Y)
LCB <- x_bar - y_bar + qt(0.025,n_X + n_Y - 2) * sqrt(s_X^2 / n_X + s_Y^2 / n_Y)
UCB <- x_bar - y_bar + qt(0.975,n_X + n_Y - 2) * sqrt(s_X^2 / n_X + s_Y^2 / n_Y)
cat("interval estimation", LCB, UCB)



