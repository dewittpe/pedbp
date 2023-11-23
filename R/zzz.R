.onUnload <- function(libpath) {
  library.dynam.unload("pedbp", libpath)
}
