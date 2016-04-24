#wlsxv - cross-validated wls
require(aautil)
require(pasi)

p <- 40 #p, lags in AC
x <- rnorm(1000+p)
y <- filter(x,.5*(p:1)/(p*(p+1)/2),sides=1,meth='rec') #+rnorm(1000)/1e2
yy <- matrix(lagf(y,p),1000,p+1,dimnames=list(1:1000,1:(p+1)))
ww <- wls0(yy,rep(1,1000),rr=rr*44)
barplot(ww$coef,main=ww$r.squared)

#initial value for bayes strength set to 1; search values .01 to 10
#separate the 'solve' and 'evaluate' parts of wls0


wls0b(yy,rep(1,1000),rr=sdl2Fun(yy,la=-(p:1),b1=0,b2=.06,bb=6))

#' @export
wls0b <- function(b,yx,rr0,w=rep(1,nrow(yx)))
{
  rr <- rbind(0,cbind(0,rr0*b))
  y <- yx[,1,drop=FALSE]
  x <- cbind(1,yx[,-1,drop=FALSE])
  xwgt <- sweep(x,MARGIN=1,STATS=w,FUN="*")
  re <- y*0
  for(i in 1:length(y)) {
    iout <- (1:length(y))==i
    xxinv <- solve(t(xwgt[!iout,,drop=F])%*%x[!iout,,drop=F] + rr)#in
    co <- xxinv %*% t(xwgt[!iout,])%*%y[!iout,,drop=F]#in
    re[i] <- y[iout,,drop=F]-x[iout,,drop=F]%*%co
  }
  (length(y)-1)*cov.wt(re,wt=w,meth="unb",center=F)$cov[1,1,drop=TRUE]
}

rr <- sdl2Fun(yy,la=-(p:1),b1=0,b2=0,bb=1)
wls0b(2,yy,rr)

optimise(f=wls0b,int=c(10,50),yx=yy,rr=rr,tol=.01)
