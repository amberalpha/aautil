.onLoad <- function(libname, pkgname) {
  root.global <<- aatopselect("prod")
  #bui.global <<- buiindir(paste0(root.global,"BDH/RAW/BEST_TARGET_PRICE/TFU")) #somewhat arbitrary?
  #da.global <<- commonda(patt="CUR_MKT_CAP_TFU.RData")
  #daw.global <<- as.Date(intersect(as.character(da.global),getca()))
  #aapaenv <<- environment(x0100BTP_TFU) #this is the package environment - could be done better?
  #aapafun <<- ls(aapaenv)
  options("stringsAsFactors"=FALSE)
}

#.onLoad <- function(...){options(stringsAsFactors=FALSE)}

#rddelim - delimiter between fields in filename
rddelim <- function(){"_"}

#rdroot - root directory containing rd
#' @export
rdroot <- function(){root.global}
#rdroot <- function(){".."}

#abbrev - abbreviate and remove forbidden characters
abbrev <- function(x,len=30,rep="",patt=list("\\.","/","&","\\*",":"),nospace=TRUE) {
  if(nospace) patt <- union(patt," ")
  x <- abbreviate(x,minl=len)
  for(i in 1:length(patt)) x <- gsub(x=x,patt=patt[i],rep=rep)
  x
}

#' put
#'
#' save an object
#' @param x object to save
#' @param desc description
#' @param i index number, defaults to next available
#' @keywords data
#' @export
#' @examples
#' putrd(letters,'my alphabet')
#putrd
putrd <- function(x,desc=abbrev(deparse(substitute(x)),nospace=FALSE),i=idxrd()+1) {
  n <- formatC(i, width = 5, format = "d", flag = "0")
  fnam <- paste(c(n,as.character(as.Date(Sys.time())),desc),collapse=rddelim())
  save(x,file=paste0(rdroot(),"/rd/",fnam,".RData"))
}
#' new
#'
#' create the directory
#' @keywords data
#' @export
#' @examples
#' newrd()

#newrd
newrd <- function() {
  system(paste0('mkdir ',rdroot(),'/rd'))
  x <- NULL
  putrd(x,i=0)
}
#' get
#'
#' get an object
#' @param i index number to retrieve
#' @keywords data
#' @export
#' @examples
#' getrd(1)

#getrd
getrd <- function(i=idxrd()) {
  n <- formatC(i, width = 5, format = "d", flag = "0")
  fnam <- paste0(paste0(dirrd()[n],collapse=rddelim()),".RData")
  load(file=paste0(rdroot(),"/rd/",fnam))
  x
}

#' dir
#'
#' directory of objects
#' @keywords data
#' @export
#' @examples
#' dirrd(1)
#dirrd
dirrd <- function() {
  dd <- paste0(rdroot(),"/rd")
  l1 <- lapply(lapply(lapply(dir(dd),strsplit,split="\\."),"[[",1),"[",1)
  num <- unlist(lapply(lapply(lapply(l1,strsplit,split=rddelim()),"[[",1),"[",1))
  dat <- unlist(lapply(lapply(lapply(l1,strsplit,split=rddelim()),"[[",1),"[",2))
  des <- unlist(lapply(lapply(lapply(l1,strsplit,split=rddelim()),"[[",1),"[",3))
  setkeyv(data.table(data.frame(num=num,dat=dat,des=des)),'num')[]
}

#' index
#'
#' index of repo
#' @keywords data
#' @export
#' @examples
#' idxrd()
#idxrd - get final index
idxrd <- function() { ifelse(length(dirrd()),as.numeric(max(dirrd()[,num])),0) }

#' delete
#'
#' delete from repo
#' @param i index number to delete
#' @keywords data
#' @export
#' @examples
#' delrd()
#delrd
delrd <- function(i=idxrd()) {
  if(i==0) return()
  n <- formatC(i, width = 5, format = "d", flag = "0")
  fnam <- paste0(paste0(dirrd()[n],collapse=rddelim()),".RData")
  system(paste0('rm "',paste0(rdroot(),"/rd/",fnam,'"')))
}

#' grep
#'
#' grep for pattern in des
#' @param patt pattern to match using grep
#' @keywords data
#' @export
#' @examples
#' greprd("^su ")
greprd <- function(patt="^su ") {
  dd<-dirrd()
  dd[grepl(patt,dirrd()[,des]),as.numeric(num)]
}


#' last observation carry forward
#'
#' locf
#' @keywords utility
#' @param z zoo object for NA adjustment
#' @param wd weekdays to include in output sequence
#' @param roll see data.table documentation
#' @param rollends see data.table documentation
#' @export
#' @examples
#' dtlocf(z<-zoo(matrix(c(NA,1:6,rep(NA,3)),10,2)))
#ROLL=INTEGER WORKS ONLY FOR NUMERIC INDEX SO MODS 2014-06-26 (transform to Date and back)
#minvar2.dtlocf - modified with roll to allow focb, using -ve values for roll
dtlocf <- function(z,dates=seq(from=min(index(z)),to=max(index(z)),by=1),wd=1:5,roll=TRUE,rollends=FALSE) {
  index(z) <- as.Date(index(z))
  j <- colnames(z)
  colnames(z)<-paste0("a",100000+(1:ncol(z)))
  #dates <- as.character(dates[as.POSIXlt(dates)$wday%in%wd])
  dates <- as.Date(dates[as.POSIXlt(dates)$wday%in%wd])
  rownames(z)<-as.character(index(z))
  #dt <- setkey(data.table(mattotab(z)),bui,date)
  dt <- setkey(data.table(mattotab(z))[,date:=as.Date(fastPOSIXct(date))],bui,date)
  dt1 <- dt[!is.na(field)][CJ(unique(dt[,bui]),dates),roll=roll,rollends=rollends]
  dt1[,date:=as.character(date)] #this is slow, lubridate is slow, fastposixct slow
  mat <- as.matrix(tabtomat(data.frame(setcolorder(dt1,c('date','bui','field'))))[,colnames(z)])
  colnames(mat) <- j
  zoo(mat,as.Date(rownames(mat)))
}


#' triangular weighted mean, high weights on right
#'
#' @keywords utility
#' @param x vector
#' @param minweight tail weight
#' @param pow power x is raised to before mean
#' @export
`meantri` <- function(x,minweight=(1/3),pow=1,...)
{
  if(length(x)==0) return()
  stopifnot(length(minweight)==1 && 0<=minweight && minweight<=1)
  wgt <- seq(from=minweight,to=1,length=length(x))
  weighted.mean(x=x**pow,w=wgt,...)
}


#' rolling average
#'
#' @keywords utility
#' @param x vector
#' @param what character name of function
#' @param n window over which applied
#' @export
`rollxts` <- function(x,what="max",n=5,...)
{
  #    rollapply(data=x,width=n,FUN=get(what),na.rm=TRUE,fill=NA,align="right",...)
  rollapply(data=x,width=n,FUN=get(what),na.rm=TRUE,na.pad=TRUE,align="right",...) #changed to deprecated alternative as workaround for bug in deraats 2013-12-31
}


#' normalisation
#'
#' @keywords utility
#' @param x vector
#' @param sdv standard deviation target
#' @param meanarg mean target
#' @param final logical flag to return just final point
#' @export
`mynorm` <- function(x,sdv=sd(as.numeric(x),na.rm=TRUE),meanarg=mean(x,na.rm=TRUE),final=FALSE,...)
{
  if(sum(!is.na(x))<2) return(NA) 
  stopifnot(is(x,"numeric"))
  res <- qnorm(p=rank(x,na.last='keep')/(1+sum(!is.na(x))),sd=sdv,mean=meanarg)
  if(final) { res[length(res)] } else {res}
}


#' transforms rows, cols, or entire zoo to normal, preserving moments 1,2
#'
#' @keywords utility
#' @param x zoo
#' @param dimension one of 'ts' 'xs' 'tsxs'
#' @export
`zoonorm` <- function(x,dimension=c("ts","xs","tsxs"),...)
{
  stopifnot(class(x)%in%c("matrix","zoo"))
  stopifnot(all(dim(x)>0))
  stopifnot(any(!is.na(x)))
  stopifnot(mode(x)=="numeric")
  dimension <- match.arg(dimension)
  res <- x*NA
  if(dimension=="ts") {
    for(j in 1:ncol(x)) {
      res[,j] <- mynorm(as.numeric(x[,j]))
    }
  }
  if(dimension=="xs") {
    for(i in 1:nrow(x)) {
      res[i,] <- mynorm(as.numeric(x[i,]))
    }
  }
  if(dimension=="tsxs") {
    res[] <- mynorm(as.numeric(x))
  }
  stopifnot(identical(dim(res),dim(x)) && identical(sum(is.na(x)),sum(is.na(res))))
  res
}

#' make directory
#'
#' make directory if it does not exist
#' @param dd path
#' @keywords utility
#' @export
mkdirn <- function(dd) {
  if(all(is.na(file.info(dd))))  suppressWarnings(system(paste0("mkdir ",dd)))
}


#' extract dates
#'
#' @export
extractDates <-
  function(
    dates,
    weekday=FALSE,
    find=c("all","last","first"),
    period=c("week","month","year"),
    partials=TRUE,
    firstlast=FALSE,
    select)
  {    
    find <- match.arg(find)
    period <- match.arg(period)
    myindex1 <- 1:length(dates)
    #1  optionally point only to weekdays
    if(weekday) {
      wday <- as.POSIXlt(dates)$wday
      myindex1 <- which( (0 < wday) & (wday < 6) )
    }
    if(period=="month") {
      theperiod <- 100*as.POSIXlt(dates[myindex1])$year+as.POSIXlt(dates[myindex1])$mon
      dayinperiod <- as.POSIXlt(dates[myindex1])$mday
    } else if(period=="year") {
      theperiod <- as.POSIXlt(dates[myindex1])$year
      dayinperiod <- as.POSIXlt(dates[myindex1])$yday
    } else if(period=="week") {
      theweek <- as.numeric(format(as.POSIXct(dates[myindex1]), "%U") )
      theyear <- as.numeric(format(dates[myindex1],"%Y"))
      incorrectPartialWeek <- theweek==0
      theyear[incorrectPartialWeek] <- theyear[incorrectPartialWeek]-1    #first partial week in January assigned to last year
      theweek[incorrectPartialWeek] <- as.numeric(format(ISOdate(theyear[incorrectPartialWeek]-1,12,31),"%U"))    #only incomplete Jan weeks are indexed 0 (see Jan 1995)
      theperiod <- 100*theyear+theweek
      dayinperiod <- as.POSIXlt(dates[myindex1])$wday
    }
    #2  if selecting based on 'find'
    if(find=="all"){
      myindex2 <- 1:length(myindex1)
    } else {
      myindex2 <- setdiff(which(diff(c(theperiod[1],theperiod))!=0),1)
      if(find=="last") {
        myindex2 <- myindex2-1
      }
      if(partials){
        if(find=="last") {
          myindex2 <- unique(c(myindex2,length(myindex1)))
        } else {
          myindex2 <- unique(c(1,myindex2))
        }
      }
    }
    #3 select based on 'select'
    if(missing(select)||is.na(select)||is.null(select)) {
      myindex3 <- 1:length(myindex2)
    } else {
      myindex3 <- which(dayinperiod[myindex2]%in%select)
    }
    myindex <- myindex1[myindex2][myindex3]
    if(firstlast) {
      myindex <- unique(c(1,myindex,myindex1[length(myindex1)]))
    }
    if(all(is.na(myindex))) myindex <- NULL
    return(dates[myindex])
  }

#' table to matrix
#'
#' @export
tabtomat <- function(x) {
  stopifnot(is.matrix(x)|is.data.frame(x))
  stopifnot(ncol(x)==3)
  stopifnot(!any(duplicated(paste(x[,1],x[,2]))))
  da <- sort(unique(x[,1]))
  su <- sort(unique(x[,2]))
  res <- matrix(NA,nrow=length(da),ncol=length(su),dimnames=list(da,su))
  i <- match(x[,1],da)
  j <- match(x[,2],su)
  res[cbind(i,j)] <- x[,3]
  res
}


#create very top dirs
#' @export
aatopcreate <- function() {
  mkdirn(paste0(rappdirs::user_data_dir(),"\\aabb"))
  mkdirn(paste0(rappdirs::user_data_dir(),"\\aabb\\test"))
  mkdirn(paste0(rappdirs::user_data_dir(),"\\aabb\\prod"))
}



#create very top dirs
#' @export
aatopselect <- function(ver=c("prod","test")) {
  ver <- match.arg(ver)
  root.global <<- paste0(rappdirs::user_data_dir(),"\\aabb\\",ver,"\\")
}

#' @export
buiindir <- function(dd="../gvs11sector/blra/",test=TRUE,...){
  cc <- list.files(dd,...)
  teststring <- ifelse(test,".?EQ\\d\\d\\d\\d\\d\\d\\d\\d\\d\\d\\d\\d\\d\\d\\d\\d.RData",".?.RData")
  cc1 <-cc[grepl(teststring,cc)]
  if(test) {
    unique(substr(cc1,nchar(cc1)-23,nchar(cc1)-6))
  } else {
    unique(substr(cc1,1,nchar(cc1)-6))    
  }
}

#' @export
buiindirs <- function(dd=paste0(root.global,"/BDP/key1/",dir(paste0(root.global,"/BDP/key1/")),"/")){ 
  sort(unique(unlist(lapply(as.list(dd),buiindir)))) 
}


#' unit tests
#'
#' @export
aatests <- function() {
  require(aautil)
  aatopselect("test")
  require(testthat)
  require(aabd)
  require(aapa)
  require(aaco)
  require(aate)
  require(aara)
  require(aafa)
  system.time(test_file("../aa020bd/tests/aa020bdtest.R"))
  system.time(test_file("../aa030pa/tests/aa030patest.R"))
  system.time(test_file("../aa040co/tests/aa040cotest.R")) 
  system.time(test_file("../aa050te/tests/aa050tetest.R")) 
  #test_file("../aa060ra/tests/aa060ratest.R")
  #test_file("../aa070fa/tests/aa070fatest.R")
  #test_file("./tests/aa080titest.R")
}
