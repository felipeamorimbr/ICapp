#'Generates interval-censored Failure Time data with schedule visits
#'
#'
#'@keywords interval censored
#'@examples
#'ICsimple(50,0.8,0.8)



ICSchedule <- function(n,shape, scale, pc, visits, p, X = as.matrix(0), beta = as.matrix(0)){
  X <- as.matrix(X)
  beta <- as.matrix(beta)
  visits <- as.vector(visits)
  #CHECAGENS
  if(!is.numeric(n)) stop('"n" must be  numeric')
  if(n < 0) stop('"n" must be greater than or equal to zero')
  if(!is.numeric(scale)) stop('"scale" must be numeric')
  if(scale < 0) stop('"scale" must be greater than or equal to zero')
  if(!is.numeric(shape)) stop('"shape" must be numeric')
  if(shape < 0) stop('"shape" must be greater than or equal to zero')
  if(!is.numeric(pc)) stop('"pc" must be numeric')
  if(pc > 1 | pc < 0) stop('"pc" must be greater than zero and less than 1')
  if(!all(visits > 0)) stop('"visits" all visits times must be greater than zero')
  if(!is.numeric((visits))) stop('"visits" must be numeric')
  if(p > 1 | p < 0) stop('"p" must be greater than zero and less than 1')
  if(!is.numeric(p)) stop('"p" must be numeric')
  if(!all(X == 0) & nrow(X) != n) stop('Number of observations of "x" must be the
                                       same number of observations
                                       random generated')
  if((!all(X == 0)) & (!all(beta == 0)) & (ncol(X) != length(beta))) stop(
    'Lenght of "beta" must be the same number of columns of "x".')
  if(any(!is.numeric(X))) stop('"x" must be numeric')
  if(any(!is.numeric(beta))) stop('"beta" must be numeric')
  # generating times from the following Weibull distribution:
  u <- runif(n)
  lambda <- scale*exp(X%*%beta)
  t <- rweibull(n, shape = shape, scale = lambda)
  visits <- visits[order(visits)]

  L=c(rep(0,n)) #left interval limit
  R=c(rep(0,n)) #right interval limit
  nvisits <- length(visits)
  occurance <- rep(0,nvisits)
  nvisitsaux <- 0
  auxvisits <- 0
  for (i in 1:n){
    occurance <- rbinom(nvisits,1,p)
    auxvisits <- c(0,visits[occurance == 0])
    nvisits <- length(auxvisits)
    if(t[i] > max(auxvisits)){
      L[i] <- max(auxvisits)
      R[i] <- Inf
    }else{
      L[i] <- max(auxvisits[auxvisits < t[i]])
      R[i] <- min(auxvisits[auxvisits >= t[i]])
    }
  }
  y <- cbind(L, R)
  dados<-data.frame(y,X)
  return(dados)
}

