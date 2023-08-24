## Function to compute pairwise mahalanobis distances
mahalanobis_pairwise <- function(data, mat){
  dist_mat <- matrix(NA, nrow=nrow(data), ncol=nrow(data))
  for (i in 1:nrow(data)){
    for (j in 1:i){
      if (i != j) {
        dist_mat[i, j] <- sqrt((t(data[i,]-data[j,]) %*% mat %*% (data[i,]-data[j,])))
      }
    }
  }
  return(dist_mat)
}

## Function to extract named vectors from list
list_extract <- function(x, name){
  y <- c()
  for (i in 1:length(x)){
    if(names(x[i]) == name){
      y <- c(y, x[i])
    }
  }
  z <- unlist(y)
  return(z)
}

# Function to compute running tally of low-for-long quarters
cumsum_reset <- function(x) {
  cumsum_value <- 0
  result <- numeric(length(x))
  
  for (i in seq_along(x)) {
    if (x[i] == 0) {
      cumsum_value <- 0
    } else {
      cumsum_value <- cumsum_value + x[i]
    }
    result[i] <- cumsum_value
  }
  
  return(result)
}