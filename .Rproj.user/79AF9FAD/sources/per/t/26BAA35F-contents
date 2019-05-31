#' Hamiltonian
#'
#' Takes in a vector of thresholds, a matrix of interactions, and a configuration of the network
#' @param tau is a vector with a threshold for each node
#' @param omega is a (symmetrical) matrix with the strength of connection between each nodes. Set the the diagonal to zero.
#' @param X is a configuration of the network (e.g. if there are three nodes, c(-1,1,-1))
#' @export
#'
ham <- function(tau, omega, X) {
  ham <- -(t(X)%*%tau)-(1/2*t(X)%*%omega%*%X)
  if(is.infinite(ham)) {
    ham <- 0
  }
  return(ham)
}


