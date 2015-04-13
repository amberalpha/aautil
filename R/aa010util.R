
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
# rdroot <- function(){'..'}

# abbrev - abbreviate and remove forbidden characters
#' @export
abbrev <- function(x, len = 30, rep = "", patt = list("\\.", "/", "&", "\\*", ":"), nospace = TRUE) {
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
putrd <- function(x, desc = deparse(substitute(x)), i = idxrd() + 1) {
    n <- formatC(i, width = 5, format = "d", flag = "0")
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


#' new
#'
#' create the directory
#' @keywords data
#' @export
#' @examples
#' newrd()

#' put with structured description
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
putrdatv <- function(x,app='s6',type=deparse(substitute(x)),ver=1,i = idxrd() + 1) {
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
getrdatv <- function(app='s6',type='xbdp',ver=1) {
  type <- abbrev(type)
  desc1=paste0('app',app,'type',type,'ver',ver)
  ird <- greprd(desc1)
  if(0<length(ird)) getrd(max(ird))
}


# newrd
newrd <- function() {
    system(paste0("mkdir ", rdroot(), "/rd"))
    x <- NULL
    putrd(x, "init", i = 0)
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

#' index
#'
#' index of repo
#' @keywords data
#' @export
#' @examples
#' idxrd()
# idxrd - get final index
idxrd <- function() {
    ifelse(length(dirrd()), as.numeric(max(dirrd()[, num])), 0)
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
    if (i == 0) 
        return()
    n <- formatC(i, width = 5, format = "d", flag = "0")
    fnam <- paste0(paste0(dirrd()[n], collapse = rddelim()), ".RData")
    system(paste0("rm \"", paste0(rdroot(), "/rd/", fnam, "\"")))
}

#' grep
#'
#' grep for pattern in des
#' @param patt pattern to match using grep
#' @keywords data
#' @export
#' @examples
#' greprd('^su ')
greprd <- function(patt = "^su ") {
    dd <- dirrd()
    dd[grepl(patt, dirrd()[, des]), as.numeric(num)]
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
    rollapply(data = x, width = n, FUN = get(what), na.rm = TRUE, na.pad = TRUE, align = "right", ...)  #changed to deprecated alternative as workaround for bug in deraats 2013-12-31
}


#' normalisation
#'
#' @keywords utility
#' @param x vector
#' @param sdv standard deviation target
#' @param meanarg mean target
#' @param final logical flag to return just final point
#' @export
mynorm <- function(x, sdv = sd(as.numeric(x), na.rm = TRUE), meanarg = mean(x, na.rm = TRUE), final = FALSE, ...) {
    if (sum(!is.na(x)) < 2) 
        return(NA)
    stopifnot(is(x, "numeric"))
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
aatopselect <- function(ver = c("prod", "test")) {
    ver <- match.arg(ver)
    root.global <<- paste0(rappdirs::user_data_dir(), "\\aabb\\", ver, "\\")
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

#' unit tests
#'
#' @export
aatests <- function() {
    require(aautil)
    aatopselect("test")
    require(testthat)
    require(aabd)
    system.time(test_file("../aa020bd/tests/aa020bdtest.R"))
    require(aapa)
    aapa::.global()
    require(aaco)
    require(aate)
    require(aara)
    require(aafa)
    system.time(test_file("../aa030pa/tests/aa030patest.R"))
    system.time(test_file("../aa040co/tests/aa040cotest.R"))
    system.time(test_file("../aa050te/tests/aa050tetest.R"))
    system.time(test_file("../aa060ra/tests/aa060ratest.R"))
    system.time(test_file("../aa070fa/tests/aa070fatest.R"))
    # test_file('./tests/aa080titest.R')
}





#' @export
derca <- function(start = "1996-01-03", end = "2020-12-24", select = 3, ...) {
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
    diff(x)/lag(x)  #recall that for xts lag(x,k=1) moves older->newer ie feasible
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
    #lapply(lapply(lapply(as.list(paste0(dd,dir(dd))),load),assign,value=x),function(x){length(index(x))})
    as.Date(Reduce(joinfun,ll[0<lapply(ll,length)]))
  }
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