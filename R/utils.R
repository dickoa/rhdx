nc <- function(x) Filter(Negate(is.null), x)

merge_list <- function (x, y, ...) {
  if (length(x) == 0) 
    return(y)
  if (length(y) == 0) 
    return(x)
  i = match(names(y), names(x))
  i = is.na(i)
  if (any(i)) 
    x[names(y)[which(i)]] = y[which(i)]
  x
}

sift_res <- function(z, key = "name") {
  if (!is.null(z) && length(z) > 0) {
    if (!key %in% names(z)) key <- "name"
    paste0(na.omit(purrr::map_chr(z, ~ .[[key]])[1:5]), collapse = ", ")
  } else {
    ""
  }
}
