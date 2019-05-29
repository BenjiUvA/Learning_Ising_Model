#' Gibbs Entropy
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
