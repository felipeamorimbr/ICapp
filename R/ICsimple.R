#'Generates Case II interval-censored Failure Time data
#'
#'
#'@keywords interval censored
#'@examples
#'ICsimple(50,0.8,0.8)



ICSimple <- function(n,shape, scale, pc, h, X = as.matrix(0), beta = as.matrix(0)){
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
if(!is.numeric(h)) stop('"h" must be numeric')
if(h < 0) stop('"h" must be greater than zero')
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
tau <- qweibull(rep(pc,n), shape = shape, scale = lambda)
t <- rweibull(n, shape = shape, scale = lambda)


L=c(rep(0,n)) #left interval limit
R=c(rep(0,n)) #right interval limit

for (i in 1:n){
  while (R[i]<=min(tau[i],t[i])) {
    a<-runif(1,0,2*h)
    R[i]<-L[i]+a
    L[i]<-R[i]
  }
  L[i]<-R[i]-a
}

delta <-as.numeric(t<tau & tau<R)
R[delta==1] <- tau[delta == 1]

delta <-as.numeric(tau<t)
R[delta==1] <- Inf
y <- cbind(L, R)
dados<-data.frame(y,X)
return(dados)
}

