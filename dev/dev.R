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

expect_equal(xx[1,1],x[p+1]) #first row is p+1
expect_equal(xx[1,p+1],x[1]) #last col of first row is 1
expect_true(yconv[p+1]==xx[1,-1]%*%ff)  #first row in x is for yconv[p] so yconv[p] = f.xx1
expect_true(yconv[p+11]==xx[1+10,-1]%*%ff) #and so on

expect_equal(length(yconv),length(x)) #this is true by definition; the first non-na value in yconv is p

#to show that yx[,-1]%*%ff = yx[,1]
expect_equal(as.numeric(solve(yx[1:p,2:(p+1)],yx[1:p,1,drop=F])),ff)

#to show tha wls0(yx,w=1,rr=0) gives ff and r2=1
sol <- wls0(yx)
expect_equal(sol$coef[-1],ff)
expect_lt(abs(sol$r.squared-1),1e-10)

#to show that wls0b(b=0) is the minimum
rr <- sdl2Fun(yx,la=-(p:1),b1=0,b2=0,bb=1)
expect_gt(wls0b(b=1e-10,yx=yx,rr0=rr)-wls0b(b=0,yx=yx,rr0=rr*0),1e-15)

#to show that optimise estimates b=0
expect_lt(optimise(f=wls0b,int=c(0,1),yx=yx,rr=rr,tol=1e-6,fold=folder(nrow(yx),20,meth='cyc'))$minimum,1e-6)

###############################################################################
#recursive filter now, apply linear filter
kfold <- 10
p <- 40 #p, lags in AC
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
expect_true(all(abs(sol$coef[-1]-ff)>1e-6))
expect_lt(sol$r.squared,1-1e-3)

#to show that wls0b(b=0) is not the minimum ie can descend from this reference point
expect_lt(wls0b(b=1e-10,yx=yx,rr0=rr)-wls0b(b=0,yx=yx,rr0=rr*0),-1e-15)

#to show that optimise estimates b>0
fold <- folder(nrow(yx),lev=kfold,meth='ran')
oo <- optimise(f=wls0b,int=c(0,1e3),yx=yx,rr=rr,tol=1e-10,fold=fold)
expect_gt(oo$minimum,1e-3)
wwb <- wls0(yx=yx,rr=rr*oo$minimum)

#
wwbsdld <- wwbsdl(yx)

ww0 <- wls0(yx=yx,rr=rr*0)
ccb <- wwb$coef
cc0 <- ww0$coef
ffmax <- max(ff)*8

par(mfrow=c(1,3))
barplot(ff,ylim=c(0,ffmax))
barplot(ccb,main=paste0('r2=',round(wwb$r.squared,4)),ylim=c(0,ffmax))
barplot(cc0,main=paste0('r2=',round(ww0$r.squared,4)),ylim=c(0,ffmax))

#check that the solution is invariant to scaling
rr <- sdl2Fun(yy,la=-(p:1),b1=0,b2=0,bb=1)
rr1 <- sdl2Fun(yy*100,la=-(p:1),b1=0,b2=0,bb=1)
w <- rep(1,1000)
cc <- wls0(yx=yy,w=w,rr=rr)$coef[-1]
cc1 <- wls0(yx=yy*100,w=w,rr=rr1)$coef[-1]
expect_equal(cc,cc1) #scaling invariant

#check that sampling variance of coeffs goes down

ff <- p:1
ff <- .3*ff/sum(ff)
cc <- vector('list',1)
cc0 <- vector('list',1)
for(i in 1:10) {
  x <- rnorm(1000+p)
  y <- filter(x,ff,sides=1,meth='rec') #+rnorm(1000)/1e2 #recursive
  yx <- matrix(lagf(y,p),1000,p+1,dimnames=list(1:1000,1:(p+1)))  #this is just assigning dimnames
  cc[[i]] <- data.table(wwbsdl(yx)$coef)
  cc0[[i]] <- data.table(wls0(yx=yx,rr=rr*0)$coef)
}
mm <- as.matrix(rbindlist(cc))
mm0 <- as.matrix(rbindlist(cc0))
vc <- sd(sweep(mm[,-1], STAT=ff, FUN=`-`,MAR=2))^2 #sampling variance of all the coeffs, conditioned
vu <- sd(sweep(mm0[,-1], STAT=ff, FUN=`-`,MAR=2))^2 #sampling variance of all the coeffs, unconditioned
expect_true(vc<vu)
#barplot(apply(mm,2,mean)[-1])
#barplot(apply(mm0,2,mean)[-1])
#barplot(ff)

#mean of estimates has lower sample variance
expect_true(mean((apply(mm,2,mean)[-1]-ff)^2)<mean((apply(mm0,2,mean)[-1]-ff)^2))

