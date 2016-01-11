
require(aautil)
require(testthat)
require(lrmest)
aatopselect('test')


irdstart <- ird <- as.numeric(dirrd()[,max(num)])

#expect_equal(length(greprdatv(app='a',type='B',ver=9)),0)
#putrdatv
i <- putrdatv(letters,app='a',type='B',ver=9,i=ird+1)
expect_equal(i,ird+1)
#greprdatv
expect_equal(i,greprdatv(app='a',type='B',ver=9))
#getrdatv
expect_equal(letters,getrdatv(app='a',type='B',ver=9))
delrd(i)
expect_equal(length(greprdatv(app='a',type='B',ver=9)),0)

###rdatv
if(exists("ver.g")) rm("ver.g",envir=globalenv())
#while(idxrd()>0) delrd()
idxrd()
putrdatv(letters,app='jo',type='x',ver=0)
expect_equal(getv(),list(app='jo',type='x',ver=0))
putv(app='test',ver=1)
expect_equal(getv(),list(app='test',type='x',ver=1))
x <-1:3
putrdatv(x)
expect_identical(x,getrd())
#expect_identical(idxrd(),greprdatv())
i0 <- idxrd()
putrdatv(LETTERS)
expect_identical(idxrd(),i0) #overwrites
expect_identical(LETTERS,getrdatv())
#expect_identical(greprdatv(),idxrd()) #grep finds this entry
putv(app='test',ver=2)
expect_equal(getv(),list(app='test',type='x',ver=2))
putrdatv(x)
expect_identical(x,getrd())
expect_identical(idxrd(),greprdatv()) #grep finds this entry
#expect_identical(idxrd(),i0+1) #increments
putv(app='test',type='jo',ver=2)
expect_equal(getv(),list(app='test',type='jo',ver=2))
putrdatv(x)
expect_equal(greprdatv(app='test',type='jo',ver=2),idxrd())
putv("test",'jo',2)
i <- idxrd()
#expect_equal(i,nrow(ddv()))
delrd(greprdatv())
expect_equal(idxrd(),i-1)
irdend <- idxrd()
if(irdend>irdstart) {
  delrd((irdstart+1):irdend)
}

#memonly parts
memrdatv(F)
dirrd()
setv(app='test',v=1,type='tttp')
ttyp1 <- letters
putt(ttyp1)
ttyp2 <- LETTERS
putt(ttyp2)
ddv()
memrdatv(T,T)
setv(app='test',v=1,type='ttyp1')
expect_identical(getrdatv(),ttyp1)
expect_identical(gett('ttyp1'),ttyp1)
expect_identical(getrdatv(ty='ttyp2'),ttyp2)
expect_identical(gett('ttyp2'),ttyp2)
putrdatv(123,ty='ttyp1')
expect_identical(getrdatv(ty='ttyp1'),123)
expect_identical(gett('ttyp1'),123)
memrdatv(F)
delrd(greprdatv(ty='ttyp1'))
delrd(greprdatv(ty='ttyp2'))


if(FALSE) {
  m<-as.matrix(airquality) #this has colnames as the key
  rownames(m)<-paste0(m[,'Month'],'-',m[,'Day'])
  df<-mattotab(m)
  dt2 <- setkey(data.table(tabtomat(df)),Month,Day)
  setcolorder(dt2,colnames(airquality))[]
  #but modes differ after round trip - this is messy
  
  require(reshape2)
  dfx <- melt(airquality) #corresponds to mattotab
  head(dcast(dfx,value ~ variable)) #not what is wanted... leave this for now
}

if(FALSE) {
#small example of mdl
require(zoo)
require(aautil)
require(lrmest)
z <- zoo(rnorm(1000),1:1000)
yx <- z
nj <- 40
for(i in 1:nj) yx <- cbind(yx,lag(z,i))
yx <- yx[1:500,]
colnames(yx)<- c('Y',paste0('X',1:nj))
yx <- data.frame(yx)
m1 <- mvp(nj)

xx <- mixe(Y~.-1,r=rep(0,nj),R=m1$r,dpn=as.numeric(m1$dpn)/1e6,delt=rep(0,nj),data=yx)[[1]]

plot(xx[,3]) #yay!
grid()
abline(0,0)



}



if(F) {
  #check lm estimates a distributed lag correctly
  #dl has quadratic coeffs at lags 0:10, with zeros at 0,10
  len <- 1000
  ww <- (5:0)^2
  delt <- zoo(matrix(c(rep(0,10),1:3,rep(0,10))))
  yx <- cbind(delt,delt)
  
  #delta has a decaying impact
  #get the lag convention the right way around
  #args are delt, ff and should get ww
  
  #this version preps data to generate synthetic
  yxprep <- function(yx,minlag,maxlag,ww=rep(1,maxlag)) {
    stopifnot(is.zoo(yx) && ncol(yx)==2)
    stopifnot((0<=minlag) && (0<=maxlag) && (minlag<=maxlag))
    stopifnot(length(ww)==maxlag)
    xlag <- yx[,2,drop=F]
    for(i in 1:maxlag) {
      xlag <- cbind(xlag,lag(yx[,2],-i)*ww[i])
    }
    colnames(xlag) <- c(paste0('x',0:maxlag))
    if(0<minlag) xlag <- xlag[,-(1:minlag),drop=F]
    cbind(yx[,1,drop=F],xlag)[-(1:maxlag),]
  }
  
  
  #this version sets up lagged regressors for an autoregression
  yxprep1 <- function(y,lags=1:3) {
    stopifnot(is.zoo(y) && ncol(y)==1)
    stopifnot(0<=min(lags))
    xlag <- y
    for(i in 1:max(lags)) {
      xlag <- cbind(xlag,lag(y,-i))
    }
    colnames(xlag) <- paste0('x',0:max(lags))
    if(0<min(lags)) xlag <- xlag[,-(1:min(lags)),drop=F]
    cbind(y,xlag)[-(1:max(lags)),]
  }
  
  shar <- function(y,lags=1:3,...) { #shiller autoregression
    colnames(y) <- 'y'
    yx <<- data.frame(yxprep1(y,lags)) #have to assign to globalenv of not seen by mixe
    m1 <- mvp(length(lags),...)
    nj <- length(lags)
    mixe(y~.-1,r=rep(0,nj),R=m1$r,dpn=as.numeric(m1$dpn),delt=rep(0,nj),data=yx)
  }
  
  
  #rolling estimation of autoregressive sdl
  sharr <- function(y,lags,win,...) {
    nn <- nrow(y)-win+1
    res <- vector('list',nn)
    for (i in 1:nn) {
      res[[i]] <- shar(y[(1:win)+(i-1),,drop=F],lags=lags,...)
    }
    res
  }
  
  
  co <- shar(zoo(as.matrix(rnorm(100))),dpnc=1e-4,lags=1:10)
  barplot(co[[1]][,1])
  
  co <- sharr(zoo(as.matrix(rnorm(1000))),win=100,dpnc=1e-4,lags=1:10)
  plot(zoo((Reduce(cbind,lapply(lapply(co,'[[',1),'[',,j=1))))[,],scr=1)
  
  x<-setnames(data.table(data.frame(lapply(lapply(co,'[[',1),'[',,1))),as.character(1:length(co)))
}
  
  
  
if(F) {  #here the data is synthesised using an ma model
  y <- yx[,1,drop=F]
  nj <- 9
  
  qq <- (1:nj)*(nj:1)
  #yx <- yxprep(cbind(delt,delt),1,nj,nj)

  #use yxprep to generate the data
  yx <- cbind(delt,delt)
  yx1 <- yxprep(yx,1,9,qq)
  yx2 <- zoo(apply(yx1[,2:ncol(yx1)],1,sum),index(yx1))
  yx3 <- yxprep(yx,1,9)
  yx4 <- cbind(yx2,yx3[,-1])
  
  df <- data.frame(yx4)
  df$yx2 <- df$yx2+rnorm(14)*20 # add some measurement noise
  
  #estimate ols
  co1 <- lm(yx2~.-1,data=df)$coefficients
  barplot(co1)
  
  #same with mixe
  r <- rep(0,nj)
  m1 <- mvp(nj)
  co <- mixe(yx2~.-1,r=rep(0,nj),R=m1$r,dpn=as.numeric(m1$dpn),delt=rep(0,nj),data=df)
  barplot(co[[1]][,1])
}

