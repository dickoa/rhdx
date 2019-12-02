#' Caching
#'
#' Manage cached HDX downloaded resources with \pkg{hoardr}
#'
#' @name rhdx_cache
#'
#' @details The default cache directory is
#' `~/.cache/R/rhdx_cache`, but you can set
#' your own path using `cache_path_set()`
#'
#' `cache_delete` only accepts 1 file name, while
#' `cache_delete_all` doesn't accept any names, but deletes all files.
#' For deleting many specific files, use `cache_delete` in a [lapply()]
#' type call
#'
#' @section Useful user functions:
#' \itemize{
#'  \item `rhdx_cache$cache_path_get()` get cache path
#'  \item `rhdx_cache$cache_path_set()` set cache path
#'  \item `rhdx_cache$list()` returns a character vector of full path file names
#'  \item `rhdx_cache$files()` returns file objects with metadata
#'  \item `rhdx_cache$details()` returns files with details
#'  \item `rhdx_cache$delete()` delete specific files
#'  \item `rhdx_cache$delete_all()` delete all files, returns nothing
#' }
#'
#' @examples \dontrun{
#' rhdx_cache
#'
#' # list files in cache
#' rhdx_cache$list()
#'
#' # delete certain database files
#' # rhdx_cache$delete("file path")
#' # rhdx_cache$list()
#'
#' # delete all files in cache
#' # rhdx_cache$delete_all()
#' # rhdx_cache$list()
#'
#' # set a different cache path from the default
#' }
NULL

#' Print the cache directory
#'
#' @return the cache directory
#' @export
rhdx_cache_dir <- function() {
  assert_cache(rhdx_cache)
  rhdx_cache$cache_path_get()
}

#' List of files available in the cache directory
#'
#' @return list of files in the cache
#' @export
rhdx_cache_list <- function() {
  assert_cache(rhdx_cache)
  list.files(rhdx_cache$cache_path_get())
}

#' Delete a given file from cache
#'
#' @param file Character, the file to delete
#' @export
rhdx_cache_delete <- function(file) {
  assert_cache(rhdx_cache)
  rhdx_cache$delete(file)
}

#' Clear cache directory
#'
#' @export
rhdx_cache_clear <- function() {
  assert_cache(rhdx_cache)
  rhdx_cache$delete_all()
}
