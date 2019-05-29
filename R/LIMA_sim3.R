#' #' Simulation 3
#' #'
#'
#'
#' nreps <- 6000
#' n <- 10
#'
#' # initiele omega:
#' omega <- rnorm(n*(n-1)/2,mean = .05, sd = .01)
#' omega <- matrix(omega,n,n)
#' omega[lower.tri(omega)] <- t(omega)[lower.tri(omega)]
#' diag(omega) <- 0
#'
#' tau <- rep(0,n)
#'
#' dat <- matrix(0, nrow = nreps, ncol = n)
#' omega_save <- list(nreps)
#' gibbs_entropy <- numeric(nreps)
#' beta <- numeric(nreps)
#' beta[1] <- .001
#' Hebb <- T
#'
#' did_flip <- numeric(nreps)
#'
#' X1 <- sample(c(-1,1),n,T)
#'
#' bm <- as.matrix(expand.grid(rep(list(0:1),n)))
#' bm[bm==0] <- -1
#'
#'
#' # beta update:
#' zeta <- .001
#' little_i <- 10
#'
#' i <- 2
#'
#' for(i in 1:nreps) {
#'
#'   if (i > 1 & 1 < little_i+1) { # updates beta for inital i's
#'     beta[i] <- (1-zeta)+beta[i]*zeta*(i+i*sum(did_flip[1:(i-1)]-beta[i]))
#'   }
#'
#'   if(i > little_i+1) { # updates beta for all i's
#'     beta[i] <- (1-zeta)+beta[i]*zeta*(i+i*((sum(did_flip[(i-little_i):(i-1)])/little_i)-beta[i]))
#'   }
#'
#'
#'   g <- sample(1:n,1)
#'
#'   p <- p_flip(tau = tau, omega = omega, X = X1, g = g, beta = beta[i])
#'
#'   if (p > runif(1)) {
#'     X1[g] <- X1[g]*-1
#'     did_flip[i] <- 1
#'   }
#'
#'   pi_bm <- probability_bm(omega = omega, tau = tau, n = n, beta = beta, bm = bm) # boltzmann probabilities
#'
#'   gibbs_entropy[i] <- entropyS(omega = omega, tau = tau, beta = beta, pi_bm = pi_bm, n = n)
#'
#'   if(Hebb) {
#'
#'     omega <- delta_om(omega = omega, X = X1) # delta_om returns delta omega and omega new
#'     omega <- omega[[2]] # omega[[2]] returns the updated omega
#'
#'     omega_save[[i]] <- omega
#'
#'   }
#'   dat[i,] <- X1
#'
#'
#'
#' }
#'
#'
#'
#'
#'
#'
#'
#'
#'
#'
