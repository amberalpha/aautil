
#' @export
dern <- function(root = root.global, n = "001", type = c("BDH", "BDP", "macro")) {
    type <- match.arg(type)
    paste0(root, type, "/derive-", n, "/")
}


# rddelim - delimiter between fields in filename
rddelim <- function() {
    "_"
}
# rdroot - root directory containing rd
#' @export
rdroot <- function() {
    root.global
}

# abbrev - abbreviate and remove forbidden characters
#' @export
abbrev <- function(x, len = 30, rep = "", patt = list("\\.", "/", "&", "\\*", ":",","), nospace = TRUE) {
    if (nospace) 
        patt <- union(patt, " ")
    x <- abbreviate(x, minl = len)
    for (i in 1:length(patt)) x <- gsub(x = x, patt = patt[i], rep = rep)
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
# putrd
putrd <- function(x, desc = deparse(substitute(x)), i = idxrd() + 1,usedesc=FALSE) {
    if(usedesc && 'desc'%in%names(attributes(x))) { desc <- attr(x,'desc') }
    n <- numtotxt(i) #formatC(i, width = 5, format = "d", flag = "0")
    fnam <- paste(c(n, as.character(as.Date(Sys.time())), abbrev(desc)), collapse = rddelim())
    if (i == 0) {
        save(x, file = paste0(rdroot(), "/rd/", fnam, ".RData"))
    } else {
        i0 <- idxrd()
        save(x, file = paste0(rdroot(), "/rd/", fnam, ".RData"))
        i1 <- idxrd()
        ifelse(i1 == i0 + 1, i1, NA)
    }
}

#' descrd
#'
#' directory entry for index i
#' @export
#' @examples
#' descrd()
#' @param i index number, defaults to last
#' @export
#' @examples
#' \dontrun{
#' descrd()
#' }
descrd <- function(i=idxrd()) {
  dirrd()[formatC(i, width = 5, format = "d", flag = "0")]
}



#' put with structured description
#'
#' save an object with app, type, version in description
#' @param x object to save
#' @param app character mnemonic for application
#' @param type character mnemonic for user-defined 'type' ie description
#' @param ver numeric version
#' @param i index number, defaults to next available if over=FALSE
#' @param over logical flag to override i and overwrite last existing entry, default is TRUE
#' @keywords data
#' @export
#' @examples
#' \dontrun{
#' putrdatv(letters,app='myapp',type='testdata',ver=1)
#' getrdatv(app='myapp',type='testdata',ver=1)
#' }
putrdatv <- function(x,app=getv()$app,type=getv()$type,ver=getv()$ver,i = idxrd() + 1,over=TRUE) {
  ii <- greprdatv(app=app,typ=type,ver=ver)
  if((0<length(ii)) & over) {
    i <- max(ii)
    delrd(i=ii)
  }
  putrd(x,desc=paste0('app',app,'type',type,'ver',ver),i=i)
}
#' get using structured description
#'
#' save an object with app, type, version in description
#' @param x object to save
#' @param app character mnemonic for application
#' @param type character mnemonic for user-defined 'type' ie description
#' @param ver numeric version
#' @param i index number, defaults to next available
#' @keywords data
#' @export
#' @examples
#' \dontrun{
#' putrdatv(letters,app='myapp',type='testdata',ver=1)
#' getrdatv(app='myapp',type='testdata',ver=1)
#' }
getrdatv <- function(app=getv()$app,type=getv()$type,ver=getv()$ver) {
  ird <- greprdatv(app,type,ver)
  if(0==length(ird)) return()
  getrd(max(ird))
}


#' @export
gett <- function(ty) {getrdatv(ty=ty)} #this should be same as other copies and put in util

#' @export
putt <- function(x,ty=deparse(substitute(x))) {
  putrdatv(x,ty=ty)
  x
}

#' grep existing entry with structured description
#'
#' grep for an object with app, type, version in description
#' @param app character mnemonic for application
#' @param type character mnemonic for user-defined 'type' ie description
#' @param ver numeric version
#' @keywords data
#' @export
#' @examples
#' \dontrun{
#' putrdatv(letters,app='myapp',type='testdata',ver=1)
#' greprdatv(app='myapp',type='testdata',ver=1)
#' }
greprdatv <- function(app=getv()$app,type=getv()$type,ver=getv()$ver) {
  dd <- dirrd()
  dd[grepl(descrdatv(app,type,ver), dirrd()[, des]), as.numeric(num)]
}

#' @export
numtotxt <- function(i) {
  formatC(i, width = 5, format = "d", flag = "0")
}

#' structured description
#'
#' paste up description from mnemonics
#' @param app character mnemonic for application
#' @param type character mnemonic for user-defined 'type' ie description
#' @param ver numeric version
#' @keywords data
#' @export
descrdatv <- function(app=getv()$app,type=getv()$type,ver=getv()$ver) {
  paste0('app',app,'type',abbrev(type),'ver',ver)
}

#' @export
ddv <- function(ver=getv()$ver,app=getv()$app) { #return all dd matching app,ver
  dd <- dirrd()
  dd[grep(paste0('^app',app,'type.+ver',ver,'$'),des)]
}

#' @export
ddv1 <- function(app=getv()$app,type=getv()$type,ver=getv()$ver) { #return all dd matching app,ver
  dd <- dirrd()[!is.na(des)]
  subg <- function(x){ifelse(x=='*','.+',x)}
  ver <- paste0(subg(ver),'$')
  app <- subg(app)
  type <- subg(type)
  i <- greprd(perl=T,patt=paste(paste(c('app','type','ver'),c(app,type,ver),sep=''),collapse=''),dirrd()[,des])
  dd[zeroprepend(i,5)]
}

#' @export
nextv <- function(app=getv()$app) { #return all dd matching app,ver
  suppressWarnings(max(c(0,as.numeric(as.matrix(dirrd()[grep(paste0('^app',app),des),strsplit(des,'ver')][2,]))),na.rm=T)+1)
}



#' @export
newv <- function(isu,ver=nextv(),des=paste0(dirrd()[numtotxt(isu),des],'ird=',isu)) { #next version
  putv(v=ver)
  putrdatv(x=des,type='desc')
  putrdatv(x=cleansu(getrd(isu)),type='su')
}

#' put version number
#'
#' @param n number
#' @keywords data
#' @export
putv <- function(app="jo",type="x",ver=1) {
  ver.g <<- list(app=app,type=type,ver=ver)
}

#' put version number
#'
#' @param n number
#' @keywords data
#' @export
setv <- function(app=getv()$app, type=getv()$type, ver=getv()$ver) {
  ver.g <<- list(app=app,type=type,ver=ver)
}


#' get version number
#'
#' @keywords data
#' @export
getv <- function() {
  if(!exists("ver.g",envir=globalenv())) return(list(app="jo",type='x',ver=0))
  eval(expression(ver.g),envir=globalenv())
}


#' new
#'
#' create the directory
#' @keywords data
#' @export
#' @examples
#' \dontrun{
#' newrd()
#' }
newrd <- function(hard=FALSE) {
    if(hard) {  
      shell(paste0("rd /s /q ",rdroot(),"\\rd"))  
      load("./RD/bics.RData")
    }
    system(paste0("mkdir ", rdroot(), "/rd"))
    putrd(bics, "bicsindustrydescription", i = 0)
}
#' get
#'
#' get an object
#' @param i index number to retrieve
#' @keywords data
#' @export
#' @examples
#' getrd(1)

# getrd
getrd <- function(i = idxrd()) {
    n <- formatC(i, width = 5, format = "d", flag = "0")
    fnam <- paste0(paste0(dirrd()[n], collapse = rddelim()), ".RData")
    load(file = paste0(rdroot(), "/rd/", fnam))
    x
}

#' dir
#'
#' directory of objects
#' @keywords data
#' @export
#' @examples
#' dirrd(1)
# dirrd
dirrd <- function() {
    dd <- paste0(rdroot(), "/rd")
    l1 <- lapply(lapply(lapply(dir(dd), strsplit, split = "\\."), "[[", 1), "[", 1)
    num <- unlist(lapply(lapply(lapply(l1, strsplit, split = rddelim()), "[[", 1), "[", 1))
    dat <- unlist(lapply(lapply(lapply(l1, strsplit, split = rddelim()), "[[", 1), "[", 2))
    des <- unlist(lapply(lapply(lapply(l1, strsplit, split = rddelim()), "[[", 1), "[", 3))
    setkeyv(data.table(data.frame(num = num, dat = dat, des = des)), "num")[]
}


  
#' @export
dd <- function() {
  x <- edit(dirrd())
}


#' index
#'
#' index of repo
#' @keywords data
#' @export
#' @examples
#' idxrd()
# idxrd - get final index
idxrd <- function() {
  dd <- dirrd()
  ifelse(length(dd), as.numeric(max(dd[, num])), 0)
}

#' delete
#'
#' delete from repo
#' @param i index number to delete
#' @keywords data
#' @export
#' @examples
#' delrd()
# delrd
delrd <- function(i = idxrd()) {
    if (length(i)==1 && i == 0) 
        return()
    i <- intersect(i,dirrd()[,as.numeric(num)])
    for(j in seq_along(i)) {
      n <- formatC(i[j], width = 5, format = "d", flag = "0")
      fnam <- paste0(paste0(dirrd()[n], collapse = rddelim()), ".RData")
      system(paste0("rm \"", paste0(rdroot(), "/rd/", fnam, "\"")))
    }
}

#' grep
#'
#' grep for pattern in des
#' @param patt pattern to match using grep
#' @keywords data
#' @export
#' @examples
#' greprd('^su ')
greprd <- function(patt = "^su ",...) {
    dd <- dirrd()
    dd[grepl(patt, dirrd()[, des],...), as.numeric(num)]
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
# ROLL=INTEGER WORKS ONLY FOR NUMERIC INDEX SO MODS 2014-06-26 (transform to Date and back) minvar2.dtlocf - modified
# with roll to allow focb, using -ve values for roll
dtlocf <- function(z, dates = seq(from = min(index(z)), to = max(index(z)), by = 1), wd = 1:5, roll = TRUE, rollends = FALSE) {
    index(z) <- as.Date(index(z))
    j <- colnames(z)
    colnames(z) <- paste0("a", 1e+05 + (1:ncol(z)))
    # dates <- as.character(dates[as.POSIXlt(dates)$wday%in%wd])
    dates <- as.Date(dates[as.POSIXlt(dates)$wday %in% wd])
    rownames(z) <- as.character(index(z))
    # dt <- setkey(data.table(mattotab(z)),bui,date)
    dt <- setkey(data.table(mattotab(z))[, `:=`(date, as.Date(fastPOSIXct(date)))], bui, date)
    dt1 <- dt[!is.na(field)][CJ(unique(dt[, bui]), dates), roll = roll, rollends = rollends]
    dt1[, `:=`(date, as.character(date))]  #this is slow, lubridate is slow, fastposixct slow
    mat <- as.matrix(tabtomat(data.frame(setcolorder(dt1, c("date", "bui", "field"))))[, colnames(z)])
    colnames(mat) <- j
    zoo(mat, as.Date(rownames(mat)))
}


#' triangular weighted mean, high weights on right
#'
#' @keywords utility
#' @param x vector
#' @param minweight tail weight
#' @param pow power x is raised to before mean
#' @export
meantri <- function(x, minweight = (1/3), pow = 1, ...) {
    if (length(x) == 0) 
        return()
    stopifnot(length(minweight) == 1 && 0 <= minweight && minweight <= 1)
    wgt <- seq(from = minweight, to = 1, length = length(x))
    weighted.mean(x = x^pow, w = wgt, ...)
}


#' rolling average
#'
#' @keywords utility
#' @param x vector
#' @param what character name of function
#' @param n window over which applied
#' @export
rollxts <- function(x, what = "max", n = 5, ...) {
    # rollapply(data=x,width=n,FUN=get(what),na.rm=TRUE,fill=NA,align='right',...)
    x1 <- rollapply(data = x, width = n, FUN = get(what), na.rm = TRUE, na.pad = TRUE, align = "right", ...)  #changed to deprecated alternative as workaround for bug in deraats 2013-12-31
    rownames(x1) <- as.character(index(x1))
    x1
}

#' @export
notail <- function(x,quant=.05) {
  x[(quantile(x,prob=quant,na.rm=TRUE)<=x) & (x<=quantile(x,prob=1-quant,na.rm=TRUE))]
}


#' normalisation
#'
#' @keywords utility
#' @param x vector
#' @param sdv standard deviation target
#' @param meanarg mean target
#' @param final logical flag to return just final point
#' @export
mynorm <- function(x, sdv = sd(as.numeric(notail(x,quant)), na.rm = TRUE), meanarg = mean(notail(x,quant), na.rm = TRUE), final = FALSE, quant=0., ...) {
    if (sum(!is.na(x)) < 2) 
        return(NA)
    stopifnot(is(x, "numeric"))
    stopifnot((0<=quant) & (quant<.5))
    res <- qnorm(p = rank(x, na.last = "keep")/(1 + sum(!is.na(x))), sd = sdv, mean = meanarg)
    if (final) {
        res[length(res)]
    } else {
        res
    }
}


#' transforms rows, cols, or entire zoo to normal, preserving moments 1,2
#'
#' @keywords utility
#' @param x zoo
#' @param dimension one of 'ts' 'xs' 'tsxs'
#' @export
zoonorm <- function(x, dimension = c("ts", "xs", "tsxs"), ...) {
    stopifnot(class(x) %in% c("matrix", "zoo"))
    stopifnot(all(dim(x) > 0))
    stopifnot(any(!is.na(x)))
    stopifnot(mode(x) == "numeric")
    dimension <- match.arg(dimension)
    res <- x * NA
    if (dimension == "ts") {
        for (j in 1:ncol(x)) {
            res[, j] <- mynorm(as.numeric(x[, j]))
        }
    }
    if (dimension == "xs") {
        for (i in 1:nrow(x)) {
            res[i, ] <- mynorm(as.numeric(x[i, ]))
        }
    }
    if (dimension == "tsxs") {
        res[] <- mynorm(as.numeric(x))
    }
    stopifnot(identical(dim(res), dim(x)) && identical(sum(is.na(x)), sum(is.na(res))))
    res
}

#' make directory
#'
#' make directory if it does not exist
#' @param dd path
#' @keywords utility
#' @export
mkdirn <- function(dd) {
    if (all(is.na(file.info(dd)))) 
        suppressWarnings(system(paste0("mkdir ", dd)))
}


#' extract dates
#'
#' @export
extractDates <- function(dates, weekday = FALSE, find = c("all", "last", "first"), period = c("week", "month", "year"), 
    partials = TRUE, firstlast = FALSE, select) {
    find <- match.arg(find)
    period <- match.arg(period)
    myindex1 <- 1:length(dates)
    # 1 optionally point only to weekdays
    if (weekday) {
        wday <- as.POSIXlt(dates)$wday
        myindex1 <- which((0 < wday) & (wday < 6))
    }
    if (period == "month") {
        theperiod <- 100 * as.POSIXlt(dates[myindex1])$year + as.POSIXlt(dates[myindex1])$mon
        dayinperiod <- as.POSIXlt(dates[myindex1])$mday
    } else if (period == "year") {
        theperiod <- as.POSIXlt(dates[myindex1])$year
        dayinperiod <- as.POSIXlt(dates[myindex1])$yday
    } else if (period == "week") {
        theweek <- as.numeric(format(as.POSIXct(dates[myindex1]), "%U"))
        theyear <- as.numeric(format(dates[myindex1], "%Y"))
        incorrectPartialWeek <- theweek == 0
        theyear[incorrectPartialWeek] <- theyear[incorrectPartialWeek] - 1  #first partial week in January assigned to last year
        theweek[incorrectPartialWeek] <- as.numeric(format(ISOdate(theyear[incorrectPartialWeek] - 1, 12, 31), "%U"))  #only incomplete Jan weeks are indexed 0 (see Jan 1995)
        theperiod <- 100 * theyear + theweek
        dayinperiod <- as.POSIXlt(dates[myindex1])$wday
    }
    # 2 if selecting based on 'find'
    if (find == "all") {
        myindex2 <- 1:length(myindex1)
    } else {
        myindex2 <- setdiff(which(diff(c(theperiod[1], theperiod)) != 0), 1)
        if (find == "last") {
            myindex2 <- myindex2 - 1
        }
        if (partials) {
            if (find == "last") {
                myindex2 <- unique(c(myindex2, length(myindex1)))
            } else {
                myindex2 <- unique(c(1, myindex2))
            }
        }
    }
    # 3 select based on 'select'
    if (missing(select) || is.na(select) || is.null(select)) {
        myindex3 <- 1:length(myindex2)
    } else {
        myindex3 <- which(dayinperiod[myindex2] %in% select)
    }
    myindex <- myindex1[myindex2][myindex3]
    if (firstlast) {
        myindex <- unique(c(1, myindex, myindex1[length(myindex1)]))
    }
    if (all(is.na(myindex))) 
        myindex <- NULL
    return(dates[myindex])
}

#' table to matrix
#'
#' @export
tabtomat <- function(x) {
    stopifnot(is.matrix(x) | is.data.frame(x))
    stopifnot(ncol(x) == 3)
    stopifnot(!any(duplicated(paste(x[, 1], x[, 2]))))
    da <- sort(unique(x[, 1]))
    su <- sort(unique(x[, 2]))
    res <- matrix(NA, nrow = length(da), ncol = length(su), dimnames = list(da, su))
    i <- match(x[, 1], da)
    j <- match(x[, 2], su)
    res[cbind(i, j)] <- x[, 3]
    res
}


# create very top dirs
#' @export
aatopcreate <- function() {
    mkdirn(paste0(rappdirs::user_data_dir(), "\\aabb"))
    mkdirn(paste0(rappdirs::user_data_dir(), "\\aabb\\test"))
    mkdirn(paste0(rappdirs::user_data_dir(), "\\aabb\\prod"))
}



# create very top dirs
#' @export
aatopselect <- function(ver = c("prod", "test",".")) {
    ver <- match.arg(ver)
    if(ver!='.') {
      root.global <<- paste0(rappdirs::user_data_dir(), "\\aabb\\", ver, "\\")
    } else {
      root.global <<- "."
    }
}

#' @export
buiindir <- function(dd = "../gvs11sector/blra/", test = TRUE, ...) {
    cc <- list.files(dd, ...)
    teststring <- ifelse(test, ".?EQ\\d\\d\\d\\d\\d\\d\\d\\d\\d\\d\\d\\d\\d\\d\\d\\d.RData", ".?.RData")
    cc1 <- cc[grepl(teststring, cc)]
    if (test) {
        unique(substr(cc1, nchar(cc1) - 23, nchar(cc1) - 6))
    } else {
        unique(substr(cc1, 1, nchar(cc1) - 6))
    }
}

#' @export
buiindirs <- function(dd = paste0(root.global, "/BDP/key1/", dir(paste0(root.global, "/BDP/key1/")), "/")) {
    sort(unique(unlist(lapply(as.list(dd), buiindir))))
}


#' @export
writezip <- function(ver = "test") {
  path <- paste0(rappdirs::user_data_dir(), "\\aabb\\",ver)
  fnam <- paste0(rappdirs::user_data_dir(), "\\aabb\\",ver,format(Sys.Date(),"%Y%m%d"),".zip")
  shell(paste0("rm ",fnam))
  shell(paste("zip -r ",fnam,path))
}

#' unit tests
#'
#' @export
aatests <- function(hard=FALSE,do=list(aabd=T,aapa=T,aaco=T,aate=T,aara=T,aafa=T)) {
  require(aautil)
  writezip()
  putv(ver=0)
  aatopselect("test")
  if(hard) { newrd(hard) }
  require(testthat)
  require(aabd)
  test_dir("../aa010util/tests/")
  putv(ver=0)
  #deraasu(year=2013:2014) #defaults to dax
  deraasu(year=2004:2005) #defaults to dax
  test_dir("../aa020bd/tests/") # pretty sure this does the derive actions
  require(aapa)
  require(aaco)
  require(aate)
  require(aara)
  require(aafa)
  deraasi()
  if(do$aapa) test_dir("../aa030pa/tests/")
  deraapa() # pretty sure this does the derive actions
  if(do$aaco) test_dir("../aa040co/tests/")
  deraaco()
  if(do$aate) test_dir("../aa050te/tests/")
  deraate()
  if(do$aara) test_dir("../aa060ra/tests/")
  #deraara() not exists
  if(do$aafa) test_dir("../aa070fa/tests/")
}




#as.Date("1996-01-03")

#' @export
derca <- function(start = "1989-01-11", end = "2020-12-24", select = 3, ...) {
    ca <<- data.table(date = extractDates(seq(from = as.Date(start), to = as.Date(end), by = 1), select = select, ...), 
        key = "date")
}

#' @export
offda <- function(x = ca[, max(date)], lags = -20:0) {
    ca[match(x, date) + lags, date]
}

#' @export
dttozoo <- function(dt = cart()[, `:=`(x, 1:.N)], value.var = "x") {
    x <- dcast.data.table(dt[, c("bui", "date", value.var), with = FALSE], date ~ bui, value.var = value.var)
    zoo(as.matrix(x[, -1, with = FALSE]), as.Date(x[, date]))
}

#' @export
zootodt <- function(z = dttozoo(), field = "x") {
    rownames(z) <- as.character(index(z))
    setkey(setcolorder(data.table(mattotab(z, field = field))[, `:=`(date, as.Date(date))], c(2, 1, 3)), bui, date)[]
}


#' @export
mattotab <- function(x, field = "field", fieldmode = "numeric", rclabel = c("date", "bui")) {
    stopifnot(!is.null(rownames(x)) && all(!duplicated(rownames(x))))
    stopifnot(!is.null(colnames(x)) && all(!duplicated(colnames(x))))
    # if(fieldmode!=mode(x)) print('changing mode in mattotab()')
    da <- rownames(x)
    su <- colnames(x)
    ij <- as.matrix(expand.grid(lapply(list(date = da, bui = su), seq)))
    res <- data.frame(cbind(date = da[ij[, 1]], bui = su[ij[, 2]], field = as.matrix(x)[as.matrix(ij)]))
    colnames(res) <- c(rclabel, field)
    res[, 3] <- as(res[, 3], fieldmode)
    res
}

#' @export
retxts <- function(x, ...) {
    #x1 <- diff(x)/lag(x)  #recall that for xts lag(x,k=1) moves older->newer ie feasible
    #rownames(x1) <- as.character(index(x))  #this does not work, xts has null rownames
    #x1
    diff(x)/lag(x)    #recall that for xts lag(x,k=1) moves older->newer ie feasible
}

#' @export
xz <- function(x) {
  x1 <- as.zoo(x)
  rownames(x1) <- as.character(index(x1))
  x1
}

# focb - first observation carry back
#' @export
focb.mat <- function(x, maxperiods = Inf) {
    if (nrow(x) < 1) 
        return(x)
    if (!any(is.na(x))) 
        return(x)
    y <- x[nrow(x):1, , drop = FALSE]
    locf.local(y, maxperiods = maxperiods)[nrow(x):1, , drop = FALSE]
}

# locf - last observation carry forward, adapted from 'its' lib
#' @export
locf.local <- function(x, maxperiods = Inf, ...) {
    if (nrow(x) < 1) 
        return(x)
    if (!any(is.na(x))) 
        return(x)
    y <- x
    jna <- which(apply(is.na(x), 2, any))
    for (j in jna) {
        i <- 1:nrow(y) - pmin((1:nrow(y)) - most.recent.local(!is.na(y[, j])), maxperiods)
        suppressWarnings(y[, j] <- y[i, j])  #the warning arises from one:many mapping
        stopifnot(!any(duplicated(index(y))))  #check that y has no repeats, ie warning was not relevant
    }
    return(y)
}

# 
#' @export
most.recent.local <- function(x) {
    if (!is.logical(x)) 
        stop("x must be logical")
    x.pos <- which(x)
    if (length(x.pos) == 0 || x.pos[1] != 1) 
        x.pos <- c(1, x.pos)
    rep(x.pos, c(diff(x.pos), length(x) - x.pos[length(x.pos)] + 1))
}

#' @export
psz <- paste0

#' @export
newcon <- function() {
    DBcon <<- NULL
}


#' @export
zoolist <- function(patt = "0700", fnam = dir(dern())[grep(patt, dir(dern()))]) {
    mnem <- vector("list")
    for (i in 1:length(fnam)) mnem[i] <- strsplit(fnam[i], split = "\\.")[[1]][1]
    x <- lapply(mnem, getstep)
    names(x) <- mnem
    x
}

#' @export
dtlist <- function(...) {
    x <- zoolist(...)
    dt1 <- lapply(lapply(x, zootodt), setkeyv, c("bui", "date"))
    for (i in 1:length(dt1)) {
        setnames(dt1[[i]], old = "x", new = names(dt1[i]))
    }
    dt1
}

#' @export
combokey0 <- function(x = zoolist(), fun = c("union", "intersect"), ij = c("rownames", "colnames")) {
    ij <- match.arg(ij)
    setnames(data.table(sort(Reduce(match.arg(fun), lapply(x, get(ij)))), key = "V1"), ifelse(ij == "rownames", "date", 
        "bui"))[]
}
#' @export
combokey <- function(..., drop = "VIX") {
    x <- combokey0(...)
    if (identical(colnames(x), "date")) 
        x[[1]] <- as.Date(x[[1]])
    x[!(unlist(x[, 1, with = FALSE]) %in% drop)]
}
#' @export
buidate <- function(bui = combokey(ij = "col"), da = combokey(ij = "row")[ca]) {
    expand.grid(unique(unlist(bui)), as.Date(unique(unlist(da))))
}

#' @export
cart <- function(bui = combokey(ij = "col"), da = xda(1)) {
    setkey(setnames(data.table(expand.grid(bui[, bui], da[, date], stringsAsFactors = FALSE)), c("bui", "date")), bui, 
        date)[]
}

#' @export
xda <- function(extend = 10, da = combokey()) {
    unique(rbindlist(list(da, data.table(offda(da[, max(date)], 0:extend)))))
}

#' @export
mergedt <- function(x = dtlist(), initial = cart()) {
    Reduce(f = function(x, y) merge(x, y, all = TRUE), x = x)
}

# rows with no na
#' @export
nonadt <- function(x = mergedt()) {
    x[x[, all(!is.na(.SD)), key(x)][V1 == TRUE]][, `:=`(V1, NULL)][]
}

#' @export
nonasu <- function(x = nonadt(), nda = x[, length(unique(date))], nbui = x[, length(unique(bui))]) {
    dtot <- unique(x[, list(date)])
    cart(bui = unique(x[, list(bui)])[1:nbui], da = dtot[(nrow(dtot) - (nda - 1)):nrow(dtot)])[!is.na(bui) & !is.na(date)]
}

#' @export
sfLapplyWrap <- function(X, FUN, ...) {
    sfLapply(x = X, fun = FUN, ...)
}

# mkdirn - make one directory
#' @export
mkdirn <- function(dd) {
    if (all(is.na(file.info(dd)))) 
        suppressWarnings(system(paste0("mkdir ", dd)))
}

# aacol1 - color scale, for interpolating qualitative scale 'Set2' which can only have n<=8
#' @export
aacol1 <- function(m = k, k = 6, nbrew = 8, name = "Set2") {
    c0 <- brewer.pal(n = nbrew, name = name)[1:k]
    oversample <- (k < m)
    if (oversample) {
        n <- max(m, 60)
        x1 <- (k - 1) * ((1:n)/n)
        i <- ceiling(x1 - as.numeric(options("ts.eps")))
        c1 <- c0[i]
        c2 <- c0[i + 1]
        cc <- rep(NA, n)
        for (j in seq_along(i)) {
            ii <- which(i == i[j])
            cc[ii] <- colorRampPalette(colors = c(unique(c1[ii]), unique(c2[ii])), space = "Lab")(length(ii))
        }
        im <- round(seq(from = 1/n, to = 1, length = m) * n)
    } else {
        cc <- c0
        im <- 1:m
    }
    cc[im]
}
# testaacol <- function(...){plot.new();par(mfcol=c(4,4));for(i in
# 1:15){barplot(1:i,col=aacol1(i,...),border=NA,space=0)}}

# bdp1con
#' @export
bdp1con <- function() {
    # fields only
    data.table(data.frame(field = c("CRNCY_ADJ_MKT_CAP", "BICS_LEVEL_3_NAME", "BICS_LEVEL_CODE_ASSIGNED", "BICS_LEVEL_NAME_ASSIGNED", 
        "CIE_DES", "CNTRY_ISSUE_ISO", "COMPANY_WEB_ADDRESS", "COUNTRY_FULL_NAME", "CRNCY", "CUR_MKT_CAP", "EQY_PRIM_EXCH", 
        "ICB_SUBSECTOR_NAME", "INDUSTRY_SUBGROUP", "NAME", "REGION_OF_LARGEST_REVENUE", "TICKER_AND_EXCH_CODE", "ICB_SUBSECTOR_NUM"), 
        override_fields = "EQY_FUND_CRNCY", override_values = "USD"), key = "field")
}

# bdp2con
#' @export
bdp2con <- function() {
    # fields only
    data.table(data.frame(field = c("BICS_REVENUE_%_LEVEL_ASSIGNED")))
}

#' Paths to reference data directory tree
#'
#' Generates paths for directory creation
#' @param flds data.table with fields 'field' and 'subdir' - can be generated with bdp1con
#' @keywords directory
#' @export
#' @examples
#' bdp1dir()
bdp1dir <- function(flds = bdp1con()) {
    x <- paste0("BDP/key1/", flds[, field])
    x[order(nchar(x))]
}

#' Paths to BICS data directory tree
#'
#' Generates paths for directory creation
#' @param flds data.table with fields 'field' and 'subdir' - can be generated with bdp2con
#' @keywords directory
#' @export
#' @examples
#' bdp2dir()

bdp2dir <- function(flds = bdp2con()) {
    x <- paste0("BDP/key2/", flds[, field])
    x[order(nchar(x))]
}
# *con----ends


#' resample for devol
#'
#' part 1 : rescale values to 'rolling quantile' ie the quantile of the last observation in a trailing window
#' @param x normally volatility
#' @param n number of quantiles, defaults to quintiles
#' @param start no output until this index
#' @param maxwin the window expands until it is this length, then rolls
#' @examples
#' vix <- bdh(conn,paste0('vix index'),'px_last','20060101','20150227')
#' lastqtile(vix[,2])
#' @export
lastqtile <- function(x, n = 5, start = 2 * n, maxwin = 200 * n) {
    if (is.zoo(x)) 
        x <- coredata(x)
    res <- x * NA
    for (i in seq(from = start, to = length(x), by = 1)) {
        i1 <- max(1, i - maxwin):i
        xx <- x[i1][!is.na(x[i1])]
        ii <- length(xx)
        if (n < length(xx)) 
            res[i] <- ceiling(n * rank(xx)[ii]/ii)
    }
    res
}

#' resample for devol
#'
#' part 2 : resample so equal amount 'flows' in each period
#' @param volq volatility quantile, defaults to quantil
#' @param n quantity of volq before a new sample taken, default 5
#' @examples
#' x <- bdh(conn,paste0('vix index'),'px_last','20060101','20150227')
#' vix <- zoo(x[,2],as.Date(x[,1]))
#' vixquantile <- zoo(lastqtile(x[,2]),as.Date(x[,1]))
#' i <- resvol(vixquantile,5)
#' par(mfrow=c(2,1))
#' plot(vix[i])
#' plot(coredata(vix[i]),type='l')
#' par(mfrow=c(1,1))
#' @export
resvol <- function(volq, n = 5) {
    i1 <- 1
    i2 <- 1
    sumvol <- 0
    dd <- index(volq)
    dseq <- dd[1]
    dseq[i1] <- as.Date(dd[i1])
    while (i1 < length(dd)) {
        while (sumvol < n & i1 < length(dd)) {
            sumvol <- sumvol + ifelse(is.na(volq[i1]), 0, volq[i1])
            i1 <- i1 + 1
        }
        sumvol <- 0
        i2 <- i2 + 1
        dseq[i2] <- dd[i1]
        print(paste(i1, i2))
    }
    dseq
} 


#commonda - applies joinfun to derive-000 directory contents
#' @export
commonda <- function(joinfun=intersect,nn="000",patt=".?") {
  dd0 <- paste0(root.global,"BDH/derive-",nn,"/")
  dd <- dir(dd0)[grepl(patt,dir(dd0))]
  if(0==length(dd)) { return(NA) #unsatisfactory
  } else {
    pp <- paste0(dd0,dd)
    ll <- vector("list",length(dd))
    for(i in seq_along(pp)) {
      load(pp[i])
      ll[[i]] <- index(x)
    }
    bar <- Reduce(joinfun,ll[0<lapply(ll,length)]) #no idea why needed
    as.Date(bar)
  }
}

#commonbuida - applies joinfun to derive-000 directory contents
#' @export
commonbuida <- function(joinfun=intersect,nn="000",patt=".?_....RData") {
  dd0 <- paste0(root.global,"BDH/derive-",nn,"/")
  dd <- dir(dd0)[grepl(patt,dir(dd0))]
  if(0==length(dd)) { return(NA) #unsatisfactory
  } else {
    pp <- paste0(dd0,dd)
    da <- vector("list",length(dd))
    bui <- vector("list",length(dd))
    for(i in seq_along(pp)) {
      load(pp[i])
      da[[i]] <- index(x)
      bui[[i]] <- colnames(x)
    }
    allda <- Reduce(joinfun,da[0<lapply(da,length)])
    allbui <- Reduce(joinfun,bui[0<lapply(bui,length)])
    x <- as.Date(allda,origin = "1970-01-01") #no idea why origin is needed here
    list(da=x,bui=allbui)
  }
}

#' @export
bodgebuida <- function() { #allows aapa to build
  putrdatv(list(bui=letters,da=seq.Date(from=Sys.Date()-100,to=Sys.Date(),by=1)),app='jo',v=0,ty='buida')
}

#putbuida
#' @export
putbuida <- function(x=commonbuida()) {
  putrdatv(x,app="jo",type="buida",ver=0) #this is hardcoded to ensure persistence
}

#getbuida
#' @export
getbuida <- function(...) {
  getrdatv(app="jo",type="buida",ver=0,...)
}

# turn - median daily value
turn <- function(iday = 1:5, nweek = 26) {
  x <- getbdh("PX_VOLUME_TFU") * getbdh("EQY_WEIGHTED_AVG_PX_TFU")
  dates <- index(x)
  weekday <- dates[as.POSIXlt(dates)$wday %in% iday]
  xi <- coredata(x[weekday])
  xi[is.na(xi)] <- 0
  zi <- zoo(xi, index(x[weekday]))
  rollapplyr(zi, FUN = median, width = length(iday) * nweek, partial = TRUE)
}

# tradefrac - fraction of days traded
tradefrac <- function(iday = 1:5, nweek = 26) {
  x <- getbdh("PX_VOLUME_TFU") * getbdh("EQY_WEIGHTED_AVG_PX_TFU")
  dates <- index(x)
  weekday <- dates[as.POSIXlt(dates)$wday %in% iday]
  xi <- coredata(x[weekday])
  xi[!is.na(xi) & (0 < xi)] <- 1
  xi[is.na(xi)] <- 0
  zi <- zoo(xi, index(x[weekday]))
  rollapplyr(zi, FUN = mean, width = length(iday) * nweek, partial = TRUE)
}

# valuenday - value transacted n days, total
valuenday <- function(iday = 1:5, nweek = 26, ndays = 20) {
  x <- getbdh("PX_VOLUME_TFU") * getbdh("EQY_WEIGHTED_AVG_PX_TFU")
  dates <- index(x)
  weekday <- dates[as.POSIXlt(dates)$wday %in% iday]
  xi <- coredata(x[weekday])
  xi[is.na(xi)] <- 0
  zi <- zoo(xi, index(x[weekday]))
  z1 <- rollapplyr(zi, FUN = sum, width = ndays, partial = TRUE)
  rollapplyr(z1, FUN = median, width = length(iday) * nweek, partial = TRUE)
}

# freemcap - free float market cap
freemcap <- function(iday = 1:5, nweek = 26) {
  ff <- getbdh("EQY_FREE_FLOAT_PCT_TFU")/100
  for (j in 1:ncol(ff)) {
    if (all(is.na(ff[, j]))) {
      print(j)
      ff[, j] <- 1
    }
  }
  x <- getbdh("CUR_MKT_CAP_TFU") * ff
  dates <- index(x)
  weekday <- dates[as.POSIXlt(dates)$wday %in% iday]
  xi <- coredata(x[weekday])
  zi <- zoo(xi, index(x[weekday]))
  rollapplyr(zi, FUN = mean, width = length(iday) * nweek, na.rm = TRUE, partial = TRUE)
}


#zeroprepend

#' @export
zeroprepend <- function(x,ntotal) {
  x <- as.character(x)
  stopifnot(all(nchar(x)<=ntotal)) #otherwise x is right-truncated
  z <- paste(rep("0",ntotal),collapse="")
  zz <- rep(z,length(x))
  substr(zz,1+nchar(zz)-nchar(x), nchar(zz)) <- x
  zz
}

#' @export
winsorise <- function (x, minval = quantile(x = x, probs = probs[1], na.rm = na.rm), 
          maxval = quantile(x = x, probs = probs[2], na.rm = na.rm), 
          probs = c(0.05, 0.95), na.rm = FALSE) {
  pmax(pmin(x, maxval), minval)
}

#this is a duplicate of a more sophisticated version found in this library
# #mynorm - preserves rankings, places on a normal distribution of same [from 00lib]
# #' @export
# mynorm <- function(x,sdv=sd(as.numeric(x),na.rm=TRUE),meanarg=mean(x,na.rm=TRUE))
# {
#   qnorm(p=rank(x,na.last='keep')/(1+sum(!is.na(x))),sd=sdv,mean=meanarg)
# }



#' @export
winsoriser <- function(dt = xx, field = "redoto", thresh = 0.001) {
    pt <- paste0("dt[,quantile(", field, ",", thresh, ",na.rm=TRUE)]")
    xmin <- dt[, eval(parse(text = pt))]
    pt <- paste0("dt[,quantile(", field, ",", 1 - thresh, ",na.rm=TRUE)]")
    xmax <- dt[, eval(parse(text = pt))]
    pt <- paste0(field, ":=min(max(", field, ",xmin),xmax)")
    x <- dt[, eval(parse(text = pt)), by = c("bui", "date")]
}

#' @export
saferecip <- function(x,eps=sqrt(.Machine$double.eps)) {
  1/(x+.Machine$double.eps)
}

#' @export
mz <-
  function(x) {
    stopifnot(is(as.Date(rownames(x)),"Date"))
    zoo(x,order.by=as.Date(rownames(x)))
  }

#' @export
ncpus <- function(
  nsplit=3 #number of separate entire tasks
)
{
  max(as.numeric(strsplit(shell("wmic cpu get NumberOfCores,NumberOfLogicalProcessors",intern=TRUE)," ")[[2]]),na.rm=T)
}


#' Get panel
#'
#' get a timeseries/cross-section panel or cross-section of reference data
#' @param mnem filename without extension
#' @param mydir directory
#' @param myclass character flag for 'zoo' or 'dt'
#' @param ... passed to dern to construct mydir
#' @examples getstep('NAME',n='000',typ='BDP')
#' @export
#' @family accessor
getstep <- function(mnem = strsplit(dir(mydir)[1], split = "\\.")[[1]][1], mydir = dern(...), myclass=c("zoo","dt"), ...) {
  myclass <- match.arg(myclass)   
  fnam <- ifelse(myclass=="dt",paste0(mydir, mnem, "_dt.RData"),paste0(mydir, mnem, ".RData"))
  #load(paste0(mydir, mnem, ".RData"))
  if(myclass=="dt") {
    load(paste0(mydir, mnem, ".RData"))
    rownames(x)<-as.character(index(x))
    x <- data.table(mattotab(coredata(x)))
    
  } else {
    load(paste0(mydir, mnem, ".RData"))
  }
  x
}

#' Get multiple reference data for all bui
#'
#' the root of all the filenames in the directory
#' @param mydir directory
#' @param mnem mnemonics (fields)
#' @export
#' @family toplevel
getbdp <- function(mydir = dern(n = "000", typ = "BDP"), mnem = bdp1con()[, field]) {
  # loadx <- function(mydir,mnem){{load(paste0(mydir,mnem,'.RData'));x}}
  dt <- getstep(mnem = mnem[1], mydir = mydir)
  if (1 < length(mnem)) 
    for (i in 2:length(mnem)) dt <- dt[getstep(mnem = mnem[i], mydir = mydir)]
  dt
}

#what is this?
#' @export
xpkg <- function(x) {x[,bui]}


#' @export
imgzoo <- function(z,                 #zoo
                     orderby=NULL,...)       #how to reorder columns
{
  stopifnot(is(z,"zoo"))
  if (is.null(orderby)) {
    if (any(is.na(z))) 
      orderby <- is.na(z)
    else if (mode(z) == "character") 
      orderby <- z == "0"
    else orderby <- z == 0
  }
  zz <- as.matrix(z)
  mode(zz) <- "numeric"
  jorder <- suppressWarnings(order(unlist(lapply(lapply(data.frame(!orderby),which),min)), #first value where orderby is not true
                                   unlist(lapply(lapply(data.frame(orderby),which),min)),  #first value where orderby is true
                                   as.numeric(unlist(lapply(lapply(data.frame(!orderby[(rev(seq(along=orderby[,1,drop=FALSE]))),,drop=FALSE]),which),min)))*-1 #first value from end where orderby is true
  ))
  zzz <- t(zz[,jorder])[,nrow(zz):1]
  image(zzz,xlab="bui",ylab="date",axes=FALSE,...)
}

#####ssc section (futures)

#right blank append to achieve a fixed total number of chars nch
#' @export
sscrba <- function(bui=sscread()[,ticker],nch=2) {
  as.character(sapply(bui,function(x,nch){paste0(x,paste(rep(' ',max(0,nch-nchar(x))),collapse=''))},nch=nch))
}

#right trim to n 
#' @export
sscrt <- function(bui=sscread()[,ticker],nch=3) {
  substr(bui,1,nch)
}

#right blank/number strip
#' @export
sscrbs <- function(bui=sscrba(),trim=c("space","number")) {
  trim <- match.arg(trim)
  patt <- switch(trim, 
                 space=' $',
                 number='(0|[1-9][0-9]*)$'
  )
  stripr <- function(x){
    while(grepl(patt=patt,x=x,perl=TRUE)) {
      x <- substr(x,1,nchar(x)-1)
    } 
    x
  }
  as.character(sapply(bui,stripr))
}

#month append
#' @export
sscma <- function(bui=sscrba(),nm=1:3) {
  sort(as.character(outer(FUN=paste0,bui,nm)))
}

#yellow strip/append
#' @export
sscy <- function(bui=sscma(),append=TRUE) {
  for(i in seq_along(bui)) {
    if(grepl(patt=' Comdty$',x=bui[i],perl=TRUE,ignore.case=TRUE)) {
      bui[i] <- substr(bui[i],1,nchar(bui[i])-nchar(" Comdty"))
    }
    bui[i] <- sscrbs(bui[i])
  }
  if(append) {
    bui <- paste(bui,' Comdty')
  }
  bui
}

#convert vector of tickers to names with/without month
#' @export
sscname <- function(bui=sscy(sscma()),withn=TRUE) {
  buin <- substr(sscy(bui,append=FALSE),3,3) #number as string
  buix <- sscrbs(sscy(bui,append=FALSE),trim='num') #bui without number
  name1 <- sscread()[buix,name]
  if(withn) {name1 <- paste0(name1,substr(sscy(bui,append=FALSE),3,3))}
  name1
}

#construct ticker modifier
#' @export
sscadj <- function(bui=sscy(),roll=c('A','B','R','F','N','D','O'),adjust=c('N','D','R','W'),days=0) { #active/rel.to.expiry/option.expiry ; none/difference
  roll <- match.arg(roll)
  adjust <- match.arg(adjust)
  datepart <- paste0(':',zeroprepend(days,2),'_0_')
  sscy(paste0(sscy(bui,append=FALSE),paste0(' ',roll,datepart,adjust,' ')))
}

#construct a mnemonic
#' @export
sscmnem <- function(roll=c('A','B','R','F','N','D','O'),adjust=c('N','D','R','W'),mnem=c('P','R','T','O'),nahandle=c('N','L','F'),class=c('Z','D'),days=0) {
  paste0(match.arg(roll),zeroprepend(days,2),match.arg(adjust),match.arg(mnem),match.arg(nahandle),match.arg(class))
}

#dates with data
#' @export
sscda <- function(buida=gett("buida")) {
  ca[(date>=buida$da[1])&(date<=max(buida$da)),date]
}


#' @export
all_identical <- function(x) {
  if (length(x) == 1L) {
    warning("'x' has a length of only 1")
    return(TRUE)
  } else if (length(x) == 0L) {
    warning("'x' has a length of 0")
    return(logical(0))
  } else {
    TF <- vapply(1:(length(x)-1),
                 function(n) identical(x[[n]], x[[n+1]]),
                 logical(1))
    if (all(TF)) TRUE else FALSE
  }
}

#sundry library routines for sdl, all small mods from other libraries as indicated below
#main mod is to expunge all SQL references so that it runs on spark
#these need tidying up because should not rely on ca - not currently in use, transferred from sdl 2015-08

####from 00lib [I think]
#iz - test for valid zoo
#' @export
iz <- function(x) {
  class(x)=="zoo" && class(index(x))=="Date"
}

####from folib
#sdl - shiller's smoothness prior
#' @export
sdl <- function(
  yxraw,
  la=-(10:1),
  w=seq(from=1,to=3,length=nrow(yx)),
  b1=1,   #tail=0
  b2=1,   #head=0
  b3=1,   #curv=0
  b4=0,   #triangular
  bb=1,   #overall bayes
  napex=floor(length(la)/2)  #apex of triangle
)
{
  #rm(list="DBcon",envir=globalenv())
  if(ncol(yxraw)==2) {yx <- yxraw} else if(ncol(yxraw)==1) {yx <- cbind(yxraw,yxraw)}
  nn <- length(la)
  x <- folagpad(yx[,-1,drop=FALSE],k=la,pad=TRUE)
  da <- index(x)
  dafit <- da[apply(!is.na(x),1,all)]              #fit: only x required
  daest <- as.Date(intersect(dafit,index(yx)[!is.na(yx[,1])]))    #estimation: both y and x available
  dum1 <- t(c(1,rep(0,nn-1)))
  dum2 <- rev(dum1)
  dum <- rbind(
    dum1*b1*6,
    dum2*b2*3
  )
  if(nn>2) {    
    dum3 <- sdlcurv(nn)
    if(b4!=0) {
      n1 <- floor(napex)-1
      n2 <- length(la)-(n1+1)
      dum4a <- c( rep(0,n1-1), 1, -(n1-1)/n1,  rep(0,n2) )
      dum4b <- c( rep(0,n1), (n2-1)/n2, -1,  rep(0,n2-1) )
      idum3 <- which(dum3[,floor(napex)]==-2)
      dum3 <- dum3[-idum3,]
      dum <- rbind(dum4a*b4,dum4b*b4,dum1*b4,dum2*b4,dum3*b4)
    }
    dum <- rbind(dum,
                 dum3*b3*20 
    )
  }
  dum <- bb*dum
  rr <- crossprod(dum)*as.numeric(crossprod(yx[,2]))
  yx <- cbind(yx[daest,1],x[daest,,drop=FALSE])
  res <- wls(
    yx=coredata(yx),
    w=w,
    rr=rr
  )
  res <- c(res,fit=NA)
  res$fit <- zoo(cbind(NA,cbind(1,coredata(x[dafit,,drop=FALSE]))%*%t(res$coef)),dafit)
  res$fit[match(daest,index(res$fit)),1] <- yx[daest,1] #bug in [<-.zoo so workaround with match
  res
}
#sdlcurv - finite difference curvature
#' @export
sdlcurv <- function(nn)
{
  dum <- matrix(0,nn-2,nn)
  ij <- cbind(1:(nn-2),1:(nn-2))
  dum[ij] <- 1
  ij[,2] <- ij[,2]+1
  dum[ij] <- -2
  ij[,2] <- ij[,2]+1
  dum[ij] <- 1
  dum
}
#folagpad - lag a single column zoo without losing data, output has k additional rows which contain NA, nb lag direction is as for lag()
#' @export
folagpad <- function(
  x,      #zoo
  k,      #lags
  pad=TRUE
)
{
  #stopifnot(length(k)>=1 && valla(k))
  #stopifnot( iz(x) && ncol(x)==1 )
  d1 <- min(index(x))
  d2 <- max(index(x))
  if(pad) {
    mydates <- as.Date(extrca(offda(x=d1,lags=-max(c(k,0))),offda(x=d2,lags=-min(c(k,0)))))
  } else {
    mydates <- index(x)
  }
  #stopifnot(all(mydates%in%as.Date(getca())))
  res <- lags(as.numeric(coredata(x)),k,pad=pad)
  zoo(res,mydates)
}
#wls - weighted least squares for use in sdl, allows bayesian mod to xx
#' @export
wls <- function(yx,w=rep(1,nrow(yx)),rr=NULL)
{
  if(is.null(rr)) rr <- matrix(0,ncol(yx)-1,ncol(yx)-1)
  rr <- rbind(0,cbind(0,rr))
  y <- yx[,1,drop=FALSE]
  x <- cbind(1,yx[,-1,drop=FALSE])
  xwgt <- sweep(x,MARGIN=1,STATS=w,FUN="*")
  co <- solve(t(xwgt)%*%x + rr) %*% t(xwgt)%*%y
  yvar <- cov.wt(y,wt=w,meth="ML")$cov[1,1,drop=TRUE]
  residvar <- cov.wt(y-x%*%co,wt=w,meth="ML")$cov[1,1,drop=TRUE]
  r.squared <- 1-residvar/yvar
  list(
    coef=t(co),
    r.squared=r.squared
  )
}

#' @export
getca <- function(){ca}

#' @export
extrca <- function(t1, t2) {
  ca[(as.character(t1)<=ca)&(ca<=as.character(t2))]  
}

#' @export
deploydata <- function(vin=getv()$ver,vout=nextv(),type=c('segexd','setdad','scoxd','decd','yxtad','ldgxd','yxtapd','wimad','dezod','xvmd','xvijd','ijsed','segsumd','fosumd','fisumd','celid')) {
  stopifnot(!any(is.na(list(vin,vout,type))) && length(vin)==1 && length(vout)==1 && length(type)>0)
  for(i in seq_along(type)) {
    print(type[i])
    x <- getrdatv(type=type[i],v=vin)
    stopifnot(!is.na(x) && !is.null(x)) #input exists
    stopifnot(is.null(getrdatv(v=vout,type=type[i]))) #no overwrite
    putrdatv(x,v=vout,type=type[i])
  }
}

#multivariate prior: returns the pattern matrix R and tightness dpn for mixe()
#' @export
mvp <- function(
  len=3:6, #number of lags in the distribution (vector)
  dpnc=seq_along(len), #dp for curvature (slackness on prior, see mixe)
  dpnh=dpnc, #dp for head
  dpnt=dpnc #dp for tail
  ) {
  stopifnot(all(3<=len))
  stopifnot(all(unlist(lapply(list(dpnc,dpnh,dpnt),length))==length(len)))
  s1 <- lapply(len,sdlcurv)  #strengths
  i0 <- unlist(lapply(s1,nrow))
  i2 <- cumsum(i0)
  i1 <- i2+1-i0
  j0 <- unlist(lapply(s1,ncol))
  j2 <- cumsum(j0)
  j1 <- j2+1-j0
  xcurv <- matrix(0,max(i2),max(j2))
  xtail <- xhead <- matrix(0,length(s1),max(j2))
  dpn <- NULL
  for(i in seq_along(s1)) {
    xcurv[i1[i]:i2[i],j1[i]:j2[i]] <- s1[[i]]
    dpn <- rbind(dpn,as.matrix(rep(dpnc[i],i0[i])))
    xhead[i,j1[i]] <- 1
    dpn <- rbind(dpn,dpnh[i])
    xtail[i,j2[i]] <- 1
    dpn <- rbind(dpn,dpnt[i])
  }
  rr <- rbind(xcurv,xhead,xtail)
  list(r=rr,dpn=dpn)
}

#tgt.solve.QP - uses uniroot to adjust risk aversion to achieve a target

#' @export
tgt.solve.QP <- function(
  ce,         #ce
  dvec,       #return, colnames(dvec) in ce
  constr,     #constraints list(Am,bv,meq) - nrows(constr$Am)=ncol(dvec)
  tgt=.1,     #target for vol or gross
  ttyp=c("gross","vol"),   #target type
  tol=.1,      #tolerance
  vcomp=c("T","S","R","M","precalc")
)
{
  ttyp <- match.arg(ttyp)
  vcomp <- match.arg(vcomp)
  stopifnot(tgt>0)
  stopifnot(tol>1.e-18)
  stopifnot(!is.null(rownames(dvec))&& (all(rownames(dvec)%in%buice(ce)) || vcomp=="precalc"))
  `volqp` <- function(w,Dmat) {sqrt(as.numeric(t(w)%*%Dmat%*%w))} #these functions inside due to limited error checking for performance
  `grossqp` <- function(w,Dmat) {sum(abs(w))}
  `tgtqp` <- function(x, Dmat, dvec, constr, tgt, ttyp, objfun) {
    w <- solve.QP(Dmat=x*Dmat, dvec=dvec, Amat=constr$Am, bvec=constr$bv, meq=constr$meq)$solution
    return(objfun(w=w,Dmat=Dmat) - tgt)
  }
  bui <- rownames(dvec)
  objfun <- switch(ttyp,'vol'=volqp,'gross'=grossqp)
  if(vcomp=="precalc") {Dmat <- ce} else {Dmat <- vcvce(ce)[[vcomp]][bui,bui]}
  w0 <- solve(Dmat,dvec) 
  scal <- objfun(w=w0,Dmat=Dmat)
  upr <- 5*scal/tgt #gives tgt-tgt*<0 because tgt decreases with lambda (checked on next line)
  if(tgtqp(x=upr, Dmat=Dmat, dvec=dvec, constr=constr, tgt=tgt, ttyp=ttyp, objfun=objfun)>0) stop("unexpected condition in tgt.solve.QP")
  lwr <- .05*scal/tgt #first estimate for upper bound
  while(tgtqp(x=lwr, Dmat=Dmat, dvec=dvec, constr=constr, tgt=tgt, ttyp=ttyp, objfun=objfun)<0) {lwr <- lwr*.2;print(paste("lowering lambda for soln. -",lwr))}
  estim.prec <- root <- 1
  while(estim.prec>0.01*root && tol>1.e-18) {
    sol <- uniroot(
      f=tgtqp,
      interval=c(upr,lwr),
      Dmat=Dmat, 
      dvec=dvec, 
      constr=constr,
      tgt=tgt,
      ttyp=ttyp,
      tol=tol,
      objfun=objfun)
    root <- sol$root
    estim.prec <- sol$estim.prec
    tol <- tol/10
    if(estim.prec>0.01*root) print("lowering tolerance to achieve accuracy")
  }
  sol <- solve.QP(Dmat=root*Dmat, dvec=dvec, Amat=constr$Am, bvec=constr$bv, meq=constr$meq)
  list(root=root,solution=sol) 
}

#combine y with an x lag distribution ; return single zoo


#' @export
getp <- function(sname1=NULL,pname1=NULL,pars=gett('pars'),j='pvalue') {
  if(is.null(sname1)&is.null(pname1)) return(NULL)
  text1 <- ifelse(is.null(sname1),'TRUE',paste0('sname==sname1'))
  text2 <- ifelse(is.null(pname1),'TRUE',paste0('pname== pname1'))
  text <- paste0(text1,'&',text2) #browser()
  if(j=='pvalue') {
    x <- pars[eval(parse(text=text)),]
    if(nrow(x)==0) return(NULL) #dk why it hangs otherwise
    x <- x[,x:=as(pvalue,pmode),iseq][,x]
  } else {
    x <- setkey(pars[eval(parse(text=text)),],pname)[]
  }
  x
}

#' @export
maxver <- function(ver='*',type='*') {
  max(as.numeric(unlist(lapply(strsplit(ddv1(v=ver,t=type)[,des],split='ver'),'[',2))))
}

#' @export
getlast <- function(ty='edppd') {
  getrd(ddv1(ty=ty,ver=maxver(ty=ty))[,as.numeric(num)])
}
