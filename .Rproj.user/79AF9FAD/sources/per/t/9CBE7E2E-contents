#' Energy
#'
#' Takes in a configuration of the network, a vector of thresholds, and a matrix with connections.
#' @param tau is a vector with a threshold for each node
#' @param omega is a (symmetrical) matrix with the strength of connection between each nodes. Set the the diagonal to zero.
#' @param X is a configuration of the network (e.g. if there are three nodes, c(-1,1,-1))
#' @export


energy <- function(tau,omega, X) {

energy <- -t(X)%*%tau-sum(1/2*t(X)%*%omega%*%X)
return(energy)

}
