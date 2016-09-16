.onLoad <- function(libname, pkgname) {
  
  options(stringsAsFactors = FALSE)
  
  if (!exists("root.global")) { aatopselect() } # old version, now set in options, see below
  
  ca <- suppressWarnings(derca())
  
  op <- options()
  op.aa <- list(
    aa.path = paste0(rappdirs::user_data_dir(),"\\aabb\\aabb\\") #second level is 'for historical reasons' - could eliminate this in a new install
  )
  
  toset <- !(names(op.aa) %in% names(op))
  if(any(toset)) options(op.aa[toset])
  
  invisible()
}
.onUnload <- function(libname, pkgname) {
  options(stringsAsFactors = FALSE)
  invisible()
}
