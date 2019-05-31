#' Delta Tau
#'
#'@param tau is vector that contains a threshold for each node
#'@param X is the configuration of the network for which you want to update tau
#'@param e is the learning parameter (default = .001)
#'@param l is the decay parameter (default = .001)
#'
#'@export


delta_t <- function(tau,X,e = .001,l = .001) {

  delta_t <- numeric(length(tau))

  for(i in 1:length(tau)) {
    delta_t[i] <- e*(1-abs(tau[i]))*X[i]-l*tau[i]
  }

  tau_new <- tau+delta_t
  return(tau_new)
}

