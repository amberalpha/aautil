
require(aautil)
require(testthat)
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

#expect_true(F)


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

plot(xx[,1]) #yay!
}