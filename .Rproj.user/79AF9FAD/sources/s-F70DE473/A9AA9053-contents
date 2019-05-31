#' Flip-probability
#'
#' This function is used to simulate Glauber dynamics calculated the probability a randomly selected node to flip to another state (e.g. -1 -> 1)
#' @param tau is a vector with a threshold for each node
#' @param omega is a (symmetrical) matrix with the strength of connection between each nodes. Set the the diagonal to zero.
#' @param X is a configuration of the network (e.g. if there are three nodes, c(-1,1,-1))
#' @param g is a node pciked at random
#' @param beta is the dependency parameter in the Ising model
#'@export



p_flip <- function(tau, omega, X, g, beta = 1) {

  X2 <- X
  X2[g] <- -X2[g]

  p_flip <- 1/(1+exp(beta*(energy(tau,omega,X2)-energy(tau,omega,X))))
  #p_flip <- exp(-beta*(energy(tau,omega,X)-energy(tau,omega,X2)))

  return(p_flip)

}

