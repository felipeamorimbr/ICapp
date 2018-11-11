#'Generates Case II interval-censored Failure Time data
#'
#'@keywords interval censored
#'@examples
#'ICsimple(50,0.8,0.8)



ricI <- function(n,shape, scale, pc, X = as.matrix(0), beta = as.matrix(0)){
  X <- as.matrix(X)
  beta <- as.matrix(beta)
  #CHECAGENS
  if(!is.numeric(n)) stop('"n" must be  numeric')
  if(n < 0) stop('"n" must be greater than or equal to zero')
  if(!is.numeric(scale)) stop('"scale" must be numeric')
  if(scale < 0) stop('"scale" must be greater than or equal to zero')
  if(!is.numeric(shape)) stop('"shape" must be numeric')
  if(shape < 0) stop('"shape" must be greater than or equal to zero')
  if(!is.numeric(pc)) stop('"pc" must be numeric')
  if(pc > 1 | pc < 0) stop('"pc" must be greater than zero and less than 1')
  if(!all(X == 0) & nrow(X) != n) stop('Number of observations of "x" must be the
                                       same number of observations
                                       random generated')
  if((!all(X == 0)) & (!all(beta == 0)) & (ncol(X) != length(beta))) stop(
    'Lenght of "beta" must be the same number of columns of "x".')
  if(any(!is.numeric(X))) stop('"x" must be numeric')
  if(any(!is.numeric(beta))) stop('"beta" must be numeric')
  # generating times from the following Weibull distribution:
  lambda <- scale*exp(X%*%beta)
  gama <- pc*lambda/(1-pc)
  u <- rweibull(n, shape = shape, scale = gama)
  tau <- qweibull(rep(pc,n), shape = shape, scale = lambda)
  t <- rweibull(n, shape = shape, scale = lambda)

  L <- rep(-Inf,n)
  R <- rep(0,n)
  for(i in 1:n){
    if(t[i] >= u[i]){
      R[i] <- Inf
    }else{
      R[i] <- t[i]
    }
  }
  dados <- data.frame(L,R,X)
  return(dados)
}
