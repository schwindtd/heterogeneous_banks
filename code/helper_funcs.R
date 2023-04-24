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