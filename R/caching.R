#' Caching HDX downloaded files
#'
#' Manage cached HDX downloaded files
#'
#' @name rhdx_cache
#'
#' @details The default cache directory is
#' `~/.cache/R/rhdx_cache`, but you can set
#' your own path using `rhdx_cache_set_dir()`
#'
#'
#' @examples \dontrun{
#' rhdx_cache
#' ## change the default cache directory
#' tmp <- tempdir()
#' rhdx_cache_set_dir(tmp)
#'
#' ## print current cache directory
#' rhdx_cache_get_dir()
#'
#' ## List available files in the current cache directory
#' rhdx_cache_list()
#'
#' l <- rhdx_cache_list()[1] ## get the first file
#' rhdx_cache_delete(l) ## delete it
#'
#' rhdx_cache_clear() ## delete all cached files
#' }
NULL

#' Set the cache directory
#'
#' @rdname rhdx_cache
#'
#' @param path Character directory to set
#'
#' @return the cache directory
#' @export
rhdx_cache_set_dir <- function(path) {
  assert_cache(rhdx_cache)
  rhdx_cache$cache_path_set(path)
}

#' Print the cache directory
#'
#' @rdname rhdx_cache
#'
#' @return the cache directory
#' @export
rhdx_cache_get_dir <- function() {
  assert_cache(rhdx_cache)
  rhdx_cache$cache_path_get()
}

#' List of files available in the cache directory
#'
#' @rdname rhdx_cache
#'
#' @return list of files in the cache
#' @export
rhdx_cache_list <- function() {
  assert_cache(rhdx_cache)
  list.files(rhdx_cache$cache_path_get())
}

#' Delete a given file from cache
#'
#' @rdname rhdx_cache
#'
#' @param file Character, the file to delete
#'
#' @export
rhdx_cache_delete <- function(file) {
  assert_cache(rhdx_cache)
  rhdx_cache$delete(file)
}

#' Clear cache directory
#'
#' @rdname rhdx_cache
#'
#' @export
rhdx_cache_clear <- function() {
  assert_cache(rhdx_cache)
  rhdx_cache$delete_all()
}
