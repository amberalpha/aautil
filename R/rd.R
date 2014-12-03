.onLoad <- function(...){options(stringsAsFactors=FALSE)}

#rddelim - delimiter between fields in filename
rddelim <- function(){"_"}

#rdroot - root directory containing rd
rdroot <- function(){".."}

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
  putrd("",i=0)
  
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



