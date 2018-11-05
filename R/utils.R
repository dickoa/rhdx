is_null_recursive <- function(x) is.null(x) | all(vapply(x, is.null, logical(1)))

drop_nulls <- function(x) {
  x <- Filter(Negate(is_null_recursive), x)
  lapply(x, function(x) if (is.list(x)) drop_nulls(x) else x)
}

nc <- drop_nulls

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

assert_configuration <- function(configuration)
  if (is.null(configuration) | !inherits(configuration, "Configuration"))
    stop("HDX configuration not set! Use `set_rhdx_config`", call. = FALSE)

assert_dataset <- function(x, requestable = NULL) {
  if (!inherits(x, "Dataset"))
    stop("Not an HDX dataset!", call. = FALSE)
  if (!is.null(requestable) && isTRUE(requestable) && isFALSE(x$is_requestable()))
    stop("Not an HDX requestable dataset", call. = FALSE)
  if (!is.null(requestable) && isFALSE(requestable) && isTRUE(x$is_requestable()))
    stop("Not a non requestable HDX dataset", call. = FALSE)
}

assert_datasets_list <- function(x)
  if (!inherits(x, "datasets_list"))
    stop("Not a list of HDX Datasets!", call. = FALSE)


assert_resource <- function(x)
  if (!inherits(x, "Resource"))
    stop("Not an HDX Resource object!", call. = FALSE)

assert_organization <- function(x)
  if (!inherits(x, "Organization"))
    stop("Not an HDX Organization object!", call. = FALSE)

assert_location <- function(x) {
  loc <- countrycode::codelist$iso3c
  cond <- any(grepl(x, loc, ignore.case = TRUE))
  if (!cond)
    stop("Not a valid HDX condition!", call. = FALSE)
}

check_packages <- function(x) {
  if (!requireNamespace(x, quietly = TRUE)) {
    stop("Please install ", x, call. = FALSE)
  }
}

`[.datasets_list` <- function(x, i, ...) {
    structure(NextMethod("["), class = class(x))
 }

`[.resources_list` <- function(x, i, ...) {
    structure(NextMethod("["), class = class(x))
 }

is_valid_uuid <- function(x) {
  regex <- "^[0-9a-f]{8}-[0-9a-f]{4}-[1-5][0-9a-f]{3}-[89ab][0-9a-f]{3}-[0-9a-f]{12}$"
  grepl(regex, x, ignore.case = TRUE)
}

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

read_sheet <- function(path = NULL, sheet = NULL, hxl = FALSE, ...) {
  check_packages("readxl")
  if (is.null(sheet)) {
    sheet <- readxl::excel_sheets(path)[[1]]
    cat("Reading sheet: ", sheet, "\n")
  }
  df <- readxl::read_excel(path, sheet = sheet, ...)
  if (isTRUE(hxl))
    df <- rhxl::as_hxl(df)
  df
}

get_layers_ <- function(path = NULL, zipped = TRUE) {
  check_packages("sf")
  if (zipped)
    path <- file.path("/vsizip", path)
  sf::st_layers(path)
}

get_sheets_ <- function(path = NULL) {
  check_packages("readxl")
  readxl::excel_sheets(path)
}

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

read_raster <- function(path = NULL, layer = NULL, zipped = TRUE, ...) {
  check_packages("raster")
  if (zipped)
    path <- file.path("/vsizip", path, layer)
  raster::raster(path, ...)
}

merge_list <- function (x, y, ...) {
  if (length(x) == 0)
    return(y)
  if (length(y) == 0)
    return(x)
  i <- match(names(y), names(x))
  i <- is.na(i)
  if (any(i))
    x[names(y)[which(i)]] <- y[which(i)]
  x
}


sift_res <- function(z, key = "name") {
  if (!is.null(z) && length(z) > 0) {
    if (!key %in% names(z)) key <- "name"
    r <- na.omit(vapply(z, function(x) if (length(x) > 0 ) paste0(x[[key]], ", ") else "", FUN.VALUE = "character")[1:5])
    gsub(", $", "", paste0(r, collapse = ""))
  } else {
    ""
  }
}

check_required_fields <- function(data, configuration = NULL, type = "dataset") {
  if (is.null(configuration))
    config <- yaml::read_yaml(system.file("config", "hdx_configuration.yml", package = "rhdx"))
  n2 <- names(data)
  n1 <- config$data$dataset$required_fields
  if (!all(n1 %in% n2)) stop(sprintf("Field %s is missing in the dataset!", setdiff(n1, n2), "\n"))
}


#' Function to search HDX object
#'
#' 
#' 
#' @export
#' @examples
#' \dontrun{
#' search_in_hdx("mali idps", type = "dataset") %>%
#'  filter("ocha-mali" %in% organization, "idp" %in% tags) %>%
#'  get_resources() %>%
#'  filter("zipped shapefile" %in% format) %>%
#'  read() -> mali_idps_shp
#' 
#'  search_in_hdx("ocha mali", type = "organization")
#'  }
#' 
search_in_hdx <- function(query = "*:*", rows = 10L, page_size = 1000L, configuration = NULL, type = "dataset") {
  if (!type %in% c("dataset", "resource", "organization"))
    stop("`type` should be `dataset`, `resource`, or `organization`")
  switch(type,
         dataset = search_datasets(query = query, rows = rows, page_size = page_size, configuration = configuration),
         resource = search_resources(query = query, rows = rows, page_size = page_size, configuration = configuration),
         organization = search_organizations(query = query, rows = rows, page_size = page_size, configuration = configuration))
}


#' Function to search HDX object
#'
#' 
#' 
#' @export
#' @examples
#' \dontrun{
#'  read_from_hdx("ocha-mali", type = "organization")
#'  }
#' 
read_from_hdx <- function(identifier, configuration = NULL, type = "dataset") {
  if (!type %in% c("dataset", "resource", "organization"))
    stop("`type` should be `dataset`, `resource`, or `organization`")
  switch(type,
         dataset = read_dataset(identifier = identifier, configuration = configuration),
         resource = read_resource(identifier = identifier, configuration = configuration),
         organization = read_organization(identifier = identifier, configuration = configuration)
         )
}

#' @export
browse <- function(x, ...)
  UseMethod("browse", x)

#' @export
browse.default <- function(x, ...)
  x$browse()
