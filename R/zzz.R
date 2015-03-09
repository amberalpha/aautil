.onLoad <- function(libname, pkgname) {
  
  options(stringsAsFactors = FALSE)
  
  if (!exists("root.global")) { aatopselect() } # old version, now set in options, see below
  
  derca()
  
  op <- options()
  op.aa <- list(
    aa.path = paste0(rappdirs::user_data_dir(),"\\aabb\\prod\\")
  )
  
  toset <- !(names(op.aa) %in% names(op))
  if(any(toset)) options(op.aa[toset])
  
  invisible()
}
.onUnload <- function(libname, pkgname) {
  options(stringsAsFactors = FALSE)
  invisible()
}
