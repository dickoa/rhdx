rhdx_cache <- NULL

.onLoad <- function(libname, pkgname) {
  x <- hoardr::hoard()
  x$cache_path_set("rhdx")
  rhdx_cache <<- x
}
