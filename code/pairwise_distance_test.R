rm(list=ls())

library(tidyverse)
library(MASS)
library(doParallel)
library(proxy)

## Set up cluster for parallelization (Method 3)
cl <- parallel::makeCluster(detectCores())
# Activate cluster
doParallel::registerDoParallel(cl)

sizes <- c(5, 10, 50, 100, 250, 500)
times_method1 <- c()
times_method2 <- c()
times_method3 <-c()
times_method4 <- c()

for (s in 1:length(sizes)){
  N = sizes[s]
  # generate data and covariance matrices
  Sigma = diag(nrow=N)
  data <- mvrnorm(n=N, mu=rep(0,N), Sigma=Sigma)
  mat <- solve(Sigma)
  nr <- nrow(data)
  nc <- ncol(data)
  
  # Initialize distance matrix
  dist_mat <- matrix(NA, nrow=nr, ncol=nr)
  
  ## METHOD 1: Brute Force (Two Loops)
  # Loops to compute distances
  time_method1 <- system.time(
  for (i in 1:nr){
    for (j in 1:i){
      if (i != j) {
        dist_mat[i, j] <- sqrt((t(data[i,]-data[j,]) %*% mat %*% (data[i,]-data[j,])))
      }
    }
    })
  
  times_method1 <- c(times_method1, time_method1[3])
  
  ## METHOD 2: Using Matrices (One Loop)
  # Loop to compute distances
  time_method2 <- system.time(
    for (i in 1:(nr-1)){
      rep_data <- matrix(rep(data[i,], nr-i), ncol=nc, byrow=T)
      diff <- rep_data - data[(i+1):nr,]
      dist_mat[i,(i+1):nr] <- sqrt(diag(diff %*% mat %*% t(diff)))
    })
  
  times_method2 <- c(times_method2, time_method2[3])
  
  ## METHOD 3: FOREACH (Parallel)
  time_method3 <- system.time(
  foreach (i = 1:(nr-1)) %dopar% {
    rep_data <- matrix(rep(data[i,], nr-i), ncol=nc, byrow=T)
    diff <- rep_data - data[(i+1):nr,]
    dist_mat[i,(i+1):nr] <- sqrt(diag(diff %*% mat %*% t(diff)))
  })

  times_method3 <- c(times_method3, time_method3[3])
  
  ## METHOD 4: DIST function (Proxy library)
  time_method4 <- system.time(proxy::dist(data, method="Euclidean"))
  
  times_method4 <- c(times_method4, time_method4[3])
  
}
parallel::stopCluster(cl) # Stop cluster

plot(sizes, times_method1, type="l", col="black", ylim=c(0,60))
lines(sizes, times_method2, col="red")
lines(sizes, times_method3, col="blue")
lines(sizes, times_method4, col="green")