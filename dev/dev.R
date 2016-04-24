#wlsxv - cross-validated wls
require(aautil)
require(pasi)

p <- 40 #p, lags in AC
x <- rnorm(1000+p)
y <- filter(x,.5*(p:1)/(p*(p+1)/2),sides=1,meth='rec') #+rnorm(1000)/1e2
yy <- matrix(lagf(y,p),1000,p+1,dimnames=list(1:1000,1:(p+1)))

#slow
optimise(f=wls0b,int=c(10,50),yx=yy,rr=rr,tol=2)

#single solution returns coeffs etc
ww <- wls0(yy,rep(1,1000),rr=sdl2Fun(yy,la=-(p:1),b1=0,b2=0,bb=1))
barplot(ww$coef,main=ww$r.squared)

#returns objective to minimise: mse(b|loocv)
wls0b(b=1,yy,rr=sdl2Fun(yy,la=-(p:1),b1=0,b2=0,bb=1))
