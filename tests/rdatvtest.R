require(aautil)
require(testthat)
aatopselect('test')
ird <- as.numeric(dirrd()[,max(num)])

expect_equal(length(greprdatv(app='a',type='B',ver=9)),0)
#putrdatv
i <- putrdatv(letters,app='a',type='B',ver=9,i=ird+1)
expect_equal(i,ird+1)
#greprdatv
expect_equal(i,greprdatv(app='a',type='B',ver=9))
#getrdatv
expect_equal(letters,getrdatv(app='a',type='B',ver=9))
delrd(i)
expect_equal(length(greprdatv(app='a',type='B',ver=9)),0)


m<-as.matrix(airquality) #this has colnames as the key
rownames(m)<-paste0(m[,'Month'],'-',m[,'Day'])
df<-mattotab(m)
dt2 <- setkey(data.table(tabtomat(df)),Month,Day)
setcolorder(dt2,colnames(airquality))[]
#but modes differ after round trip - this is messy

require(reshape2)
dfx <- melt(airquality) #corresponds to mattotab
head(dcast(dfx,value ~ variable)) #not what is wanted... leave this for now
