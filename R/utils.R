nc <- function(x) Filter(Negate(is.null), x)

check4X <- function(x) {
  if (!requireNamespace(x, quietly = TRUE)) {
    stop("Please install ", x, call. = FALSE)
  }
}

read_sheet <- function(path = NULL, sheet = NULL, format = c("xlsx", "xls")) {
  check4X("readxl")
  if (is.null(sheet)) {
    sheet <- readxl::excel_sheets(path)[[1]]
    cat("reading sheet: ", sheet, "\n")
  }
  format <- match.arg(format)
  if (format == "xls") {
    df <- readxl::read_xls(path, sheet = sheet)
  } else {
    df <- readxl::read_xlsx(path, sheet = sheet)
  }
  df
}

read_spatial <- function(path, layer) {
  check4X("sf")
  path <- file.path("/vsizip", path)
  if (is.null(layer)) {
    layer <- sf::st_layers(path)[[1]][1]
    message("reading layer: ", layer, "\n")
  }
  sf::read_sf(path, layer = layer)
}

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
    paste0(na.omit(vapply(z, "[[", key, FUN.VALUE = "character")[1:5]), collapse = ", ")
  } else {
    ""
  }
}
