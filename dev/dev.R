#wlsxv - cross-validated wls
require(aautil)
require(pasi)


p <- 40 #p, lags in AC
x <- rnorm(1000+p)
ff <- .5*(p:1)/(p*(p+1)/2)  #triangular
ff <- rnorm(p)/10           #random
y <- filter(x,ff,sides=1,meth='rec') #+rnorm(1000)/1e2 #recursive
yy <- matrix(lagf(y,p),1000,p+1,dimnames=list(1:1000,1:(p+1)))  #this is just assigning dimnames

yconv <- filter(x,c(0,ff),sides=1,meth='conv') #+rnorm(1000)/1e2 #convolutional length p
xx <- matrix(lagf(x,p),1000,p+1,dimnames=list(1:1000,1:(p+1))) #lags 0:p
yx <- cbind(yconv[(p+1):length(yconv)],xx[,-1])

xx[1,1]==x[p+1] #first row is p+1
xx[1,p+1]==x[1] #last col of first row is 1
yconv[p+1]==xx[1,-1]%*%ff  #first row in x is for yconv[p] so yconv[p] = f.xx1
yconv[p+11]==xx[1+10,-1]%*%ff #and so on

length(yconv)==length(x) #this is true by definition; the first non-na value in yconv is p

#to show that yx[,-1]%*%ff = yx[,1]
all.equal(as.numeric(solve(yx[1:p,2:(p+1)],yx[1:p,1,drop=F])),ff)

#to show tha wls0(yx,w=1,rr=0) gives ff and r2=1
sol <- wls0(yx)
all.equal(sol$coef[-1],ff)
abs(sol$r.squared-1)<1e-10

#to show that wls0b(b=0) is the minimum
rr <- sdl2Fun(yx,la=-(p:1),b1=0,b2=0,bb=1)
expect_gt(wls0b(b=1e-10,yx=yx,rr0=rr)-wls0b(b=0,yx=yx,rr0=rr*0),1e-15)

#to show that optimise estimates b=0
fold <- folder(nrow(yx),20,meth='cyc')
optimise(f=wls0b,int=c(0,1),yx=yx,rr=rr,tol=1e-6,fold=fold)$minimum < 1e-6

###############################################################################
#recursive filter now, apply linear filter
kfold <- 10
p <- 30 #p, lags in AC
x <- rnorm(1000+p)
#ff <- (1:p)*(p:1)
ff <- p:1
ff <- .3*ff/sum(ff)
y <- filter(x,ff,sides=1,meth='rec') #+rnorm(1000)/1e2 #recursive
yx <- matrix(lagf(y,p),1000,p+1,dimnames=list(1:1000,1:(p+1)))  #this is just assigning dimnames
rr <- sdl2Fun(yx,la=-(p:1),b1=0,b2=1,bb=1)

par(mfrow=c(1,1))
plot(as.zoo(yx[,1]))


#no longer an exact solution 
sol <- wls0(yx)
all(abs(sol$coef[-1]-ff)>1e-6)
sol$r.squared<1-1e-3

#to show that wls0b(b=0) is not the minimum ie can descend from this reference point
expect_lt(wls0b(b=1e-10,yx=yx,rr0=rr)-wls0b(b=0,yx=yx,rr0=rr*0),-1e-15)

#to show that optimise estimates b>0
fold <- folder(nrow(yx),kfold,meth='ran')
oo <- optimise(f=wls0b,int=c(0,1e3),yx=yx,rr=rr,tol=1e-10,fold=fold)
oo$minimum
wwb <- wls0(yx=yx,rr=rr*oo$minimum)
ww0 <- wls0(yx=yx,rr=rr*0)
ccb <- wwb$coef
cc0 <- ww0$coef
ffmax <- max(ff)*8

par(mfrow=c(1,3))
barplot(ff,ylim=c(0,ffmax))
barplot(ccb,main=paste0('r2=',round(wwb$r.squared,4)),ylim=c(0,ffmax))
barplot(cc0,main=paste0('r2=',round(ww0$r.squared,4)),ylim=c(0,ffmax))

#got this far - broadly works but many concerns esp with noisy regressors





####from here on may no longer work

#check that the solution is invariant to scaling
rr <- sdl2Fun(yy,la=-(p:1),b1=0,b2=0,bb=1)
rr1 <- sdl2Fun(yy*100,la=-(p:1),b1=0,b2=0,bb=1)
w <- rep(1,1000)
cc <- wls0(yx=yy,w=w,rr=rr)$coef[-1]
cc1 <- wls0(yx=yy*100,w=w,rr=rr1)$coef[-1]
barplot(cc,main=ww$r.squared)
barplot(cc1,main=ww$r.squared)
expect_equal(cc,cc1) #scaling invariant

#returns objective to minimise: mse(b|loocv)
fold <- folder(nrow(yy),20,meth='ran')
wls0b(b=1,yy,rr=sdl2Fun(yy,la=-(p:1),b1=0,b2=0,bb=1),fold=fold)

