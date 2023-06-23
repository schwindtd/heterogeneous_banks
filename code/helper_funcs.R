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