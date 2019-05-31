#' Gibbs Entropy
#'
#' Calculates Gibbs entropy for a configuration of the network
#'
#' @param tau is a vector with a threshold for each node
#' @param omega is a (symmetrical) matrix with the strength of connection between each nodes. Set the the diagonal to zero.
#' @param X is the configuration of the network
#' @param beta is the dependency parameter
#' @param pi_bm is a vector that contains Boltzmann probabilities for all states.
#'
#' @export


entropyS <- function(omega, tau, X, beta = 1, pi_bm) {
  Si <- rep(0,2^length(X))
  Si <- -sum(pi_bm*log2(pi_bm))
  #
  # for(i in 1:length(Si)) {
  #   Si[i] <- pi_bm[i]*log2(pi_bm[i])
  # }
  # gibbs_entropy <- -1*sum(Si)
  return("gibbs entropy" = Si)
}
