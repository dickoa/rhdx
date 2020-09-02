rhdx_cache <- NULL # nocov start

.onLoad <- function(libname, pkgname) {
  x <- hoardr::hoard()
  x$cache_path_set("rhdx")
  rhdx_cache <<- x
  set_rhdx_config()
} # nocov end
