#'Delta om, returns delta omega and updated omega
#'
#' This function uses Hebb's rule to update omega. It returns the new omega matrix and a matrix with delta omega.
#'
#' @param omega is a (symmetrical) matrix with the strength of connection between each nodes. Set the the diagonal to zero.
#' @param X is a configuration of the network (e.g. if there are three nodes, c(-1,1,-1))
#' @param e is the learning parameter (default setting = .001)
#' @param lambda is the decay parameter (default setting = .001)
#'@export

delta_om <- function(omega,X,e = .001,lambda = .001) {

  delta_om <- matrix(0,nrow(omega),ncol(omega))

  for(i in 1:length(X)) {
    for(j in 1:length(X)) {
      if(i!=j) {
        delta_om[i,j] <- e*(1-abs(omega[i,j]))*X[i]*X[j]-(lambda*omega[i,j])
      }
    }
  }
  #diag(delta_om) <- 0

  omega_new <- omega+delta_om
  diag(omega_new) <- 0

  output <- list("delta omega" = delta_om, "omega new" = omega_new)
  return(output)
}



