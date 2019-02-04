#' @importFrom magrittr %>%
#' @export
magrittr::`%>%`

#' @noRd
is_null_recursive <- function(x) is.null(x) | all(vapply(x, is.null, logical(1)))

#' @noRd
drop_nulls <- function(x) {
  x <- Filter(Negate(is_null_recursive), x)
  lapply(x, function(x) if (is.list(x)) drop_nulls(x) else x)
}

#' @noRd
nc <- drop_nulls

#' @noRd
check_config_params <- function(hdx_site = NULL, hdx_key = NULL, hdx_config_file = NULL, read_only = NULL, user_agent = NULL) {
  
  if (!is.null(hdx_site) && !hdx_site %in% c("prod", "test", "feature", "demo"))
    stop("hdx_site can be either `prod`, `test`, `feature` or `demo`", call. = FALSE)
  
  if (!is.null(read_only) && !is.logical(read_only))
    stop("read_only should be take a logical, either `TRUE` or `FALSE`", call. = FALSE)
  
  if (!is.null(user_agent) && !is.character(user_agent))
    stop("user_agent should be a character", call. = FALSE)

  if (!is.null(hdx_key) && !is_valid_uuid(hdx_key))
    stop("hdx_key not valid!", call. = FALSE)
  
  if (!is.null(hdx_config_file) && !file.exists(hdx_config_file)) 
    stop("HDX config file not found!", call. = FALSE)
  
  if (!is.null(hdx_config_file) && file.exists(hdx_config_file)) {
    file_ext <- tools::file_ext(hdx_config_file)
    if (!file_ext %in% c("yml", "json"))
      stop("Only YAML and JSON configuration file are supported for the moment!", call. = FALSE)
  }
}

#' @noRd
assert_configuration <- function(configuration)
  if (is.null(configuration) | !inherits(configuration, "Configuration"))
    stop("HDX configuration not set! Use `set_rhdx_config`", call. = FALSE)

#' @noRd
assert_dataset <- function(x, requestable = NULL) {
  if (!inherits(x, "Dataset"))
    stop("Not an HDX dataset!", call. = FALSE)
  if (!is.null(requestable) && isTRUE(requestable) && isFALSE(x$is_requestable()))
    stop("Not an HDX requestable dataset", call. = FALSE)
  if (!is.null(requestable) && isFALSE(requestable) && isTRUE(x$is_requestable()))
    stop("Not a non requestable HDX dataset", call. = FALSE)
}

#' @noRd
assert_datasets_list <- function(x)
  if (!inherits(x, "datasets_list"))
    stop("Not a list of HDX Datasets!", call. = FALSE)

#' @noRd
assert_resource <- function(x)
  if (!inherits(x, "Resource"))
    stop("Not an HDX Resource object!", call. = FALSE)

#' @noRd
assert_organization <- function(x)
  if (!inherits(x, "Organization"))
    stop("Not an HDX Organization object!", call. = FALSE)

#' @noRd
assert_location <- function(x) {
  loc <- countrycode::codelist$iso3c
  cond <- any(grepl(x, loc, ignore.case = TRUE))
  if (!cond)
    stop("Not a valid HDX condition!", call. = FALSE)
}

#' @noRd
check_packages <- function(x) {
  if (!requireNamespace(x, quietly = TRUE)) {
    stop("Please install ", x, call. = FALSE)
  }
}

#' @noRd
`[.datasets_list` <- function(x, i, ...) {
    structure(NextMethod("["), class = class(x))
 }

#' @noRd
`[.resources_list` <- function(x, i, ...) {
    structure(NextMethod("["), class = class(x))
 }

#' @noRd
is_valid_uuid <- function(x) {
  regex <- "^[0-9a-f]{8}-[0-9a-f]{4}-[1-5][0-9a-f]{3}-[89ab][0-9a-f]{3}-[0-9a-f]{12}$"
  grepl(regex, x, ignore.case = TRUE)
}

#' @noRd
get_user_agent <- function(x) {
    rhdx_version <- packageVersion("rhdx")
    os <- Sys.info()[["sysname"]]
    os_version <- paste(Sys.info()[["release"]], Sys.info()[["version"]])
    r_version <- paste0(R.version$major, ".", R.version$minor, 
        ifelse(R.version$status == "", "", paste0("-", R.version$status)))
    header <- paste0("rhdx/", rhdx_version, " (", os, "/",
                    os_version, "; ", "R/", r_version, ")")
    header
}

#' @noRd
read_hdx_json <- function(path, simplify_json = FALSE, ...) {
  check_packages("jsonlite")
  jsonlite::read_json(path, simplifyVector = simplify_json, ...)
}

#' @noRd
read_hdx_csv <- function(path, hxl = FALSE, ...) {
  check_packages("readr")
  df <- readr::read_csv(path, ...)
  if (isTRUE(hxl) | hxl_tags)
    df <- rhxl::as_hxl(df)
  df
}


#' @noRd
read_hdx_excel <- function(path = NULL, sheet = NULL, hxl = FALSE, ...) {
  check_packages("readxl")
  if (is.null(sheet)) {
    sheet <- readxl::excel_sheets(path)[[1]]
    cat("Reading sheet: ", sheet, "\n")
  }
  df <- readxl::read_excel(path, sheet = sheet, ...)
  if (isTRUE(hxl) | hxl_tags)
    df <- rhxl::as_hxl(df)
  df
}

#' @noRd
get_hdx_layers_ <- function(path = NULL, zipped = TRUE) {
  check_packages("sf")
  if (zipped)
    path <- file.path("/vsizip", path)
  sf::st_layers(path)
}

#' @noRd
get_hdx_sheets_ <- function(path = NULL) {
  check_packages("readxl")
  readxl::excel_sheets(path)
}

#' @noRd
read_vector <- function(path = NULL, layer = NULL, zipped = TRUE, ...) {
  check_packages("sf")
  if (zipped)
    path <- file.path("/vsizip", path)
  if (is.null(layer)) {
    layer <- sf::st_layers(path)[[1]][1]
    message("reading layer: ", layer, "\n")
  }
  sf::read_sf(dsn = path, layer = layer, ...)
}

#' @noRd
read_raster <- function(path = NULL, layer = NULL, zipped = TRUE, ...) {
  check_packages("raster")
  if (zipped)
    path <- file.path("/vsizip", path, layer)
  raster::raster(path, ...)
}

#' @noRd
sift_res <- function(z, key = "name") {
  if (!is.null(z) && length(z) > 0) {
    if (!key %in% names(z)) key <- "name"
    r <- na.omit(vapply(z, function(x) if (length(x) > 0 ) paste0(x[[key]], ", ") else "", FUN.VALUE = "character")[1:5])
    gsub(", $", "", paste0(r, collapse = ""))
  } else {
    ""
  }
}

#' @noRd
check_required_fields <- function(data, configuration = NULL, type = "dataset") {
  if (is.null(configuration))
    config <- yaml::read_yaml(system.file("config", "hdx_configuration.yml", package = "rhdx"))
  n2 <- names(data)
  n1 <- config$data$dataset$required_fields
  if (!all(n1 %in% n2)) stop(sprintf("Field %s is missing in the dataset!", setdiff(n1, n2), "\n"))
}


#' @export
browse <- function(x, ...)
  UseMethod("browse", x)

#' @export
browse.default <- function(x, ...)
  x$browse()

#' @export
create_in_hdx <- function(x, ...)
  UseMethod("create_in_hdx", x)

#' @export
create_in_hdx.default <- function(x, ...)
  cat("Not an HDX object")
