nc <- function(x) Filter(Negate(is.null), x)

check4X <- function(x) {
  if (!requireNamespace(x, quietly = TRUE)) {
    stop("Please install ", x, call. = FALSE)
  }
}

is_valid_uuid <- function(x) {
  regex <- "^[0-9a-f]{8}-[0-9a-f]{4}-[1-5][0-9a-f]{3}-[89ab][0-9a-f]{3}-[0-9a-f]{12}$"
  grepl(regex, x, ignore.case = TRUE)
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


read_vector <- function(path = NULL, layer = NULL, zipped = TRUE) {
  check4X("sf")
  if (zipped)
    path <- file.path("/vsizip", path)
  if (is.null(layer)) {
    layer <- sf::st_layers(path)[[1]][1]
    message("reading layer: ", layer, "\n")
  }
  sf::read_sf(dsn = path, layer = layer)
}

get_layers_ <- function(path = NULL, zipped = TRUE) {
  check4X("sf")
  if (zipped)
    path <- file.path("/vsizip", path)
  sf::st_layers(path)
}

get_sheets_ <- function(path = NULL) {
  check4X("readxl")
  readxl::excel_sheets(path)
}


read_raster <- function(path = NULL, layer = NULL, zipped = TRUE) {
  check4X("raster")
  if (zipped)
    path <- file.path("/vsizip", path, layer)
  raster::raster(path)
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

check_required_fields <- function(data, config = NULL, type = "dataset") {
  if (config == NULL)
    config <- yaml::yaml.load_file(system.file("config", "hdx_configuration.yml", package = "rhdx"))
  n2 <- names(data)
  n1 <- config$data$dataset$required_fields
  if (!all(n1 %in% n2)) stop(sprintf("Field %s is missing in the dataset!", setdiff(n1, n2), "\n"))
}

update_frequencies <- list(
  '-2' = 'Adhoc',
  '-1' = 'Never',
  '0' = 'Live',
  '1'= 'Every day',
  '7' = 'Every week',
  '14' = 'Every two weeks',
  '30' = 'Every month',
  '90' = 'Every three months',
  '180' = 'Every six months',
  '365' = 'Every year',
  'adhoc' = '-2',
  'never' = '-1',
  'live' = '0',
  'every day' = '1',
  'every week' = '7',
  'every two weeks' = '14',
  'every month' = '30',
  'every three months' = '90',
  'every six months' = '180',
  'every year' = '365',
  'daily' = '1',
  'weekly' = '7',
  'fortnightly' = '14',
  'every other week' = '14',
  'monthly' = '30',
  'quarterly' = '90',
  'semiannually' = '180',
  'semiyearly' = '180',
  'annually' = '365',
  'yearly' = '365')


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
  if (!type %in% c("dataset", "resource", "organization")) stop("`type` should be `dataset`, `resource`, or `organization`")
  switch(type,
         dataset = {
           ds <- Dataset$new()
           ds <- ds$search_in_hdx(query = query, rows = rows, page_size = page_size, configuration = configuration)
           ds
           ## purrr::map_df(ds, as_tibble)
         },
         resource = {
           rs <- Resource$new()
           rs <- rs$search_in_hdx(query = query, configuration = configuration)
           purrr::map_df(rs, as_tibble)
         },
         organization = {
           org <- Organization$new()
           org$search_in_hdx(query = query, rows = rows, page_size = page_size, configuration = configuration)
           purrr::map_df(org, as_tibble)
         }) 
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
  if (!type %in% c("dataset", "resource", "organization")) stop("`type` should be `dataset`, `resource`, or `organization`")
  switch(type,
         dataset = {
           ds <- Dataset$new()
           ds$read_from_hdx(identifier = identifier, configuration = configuration)
         },
         resource = {
           rs <- Resource$new()
           rs$read_from_hdx(identifier = identifier, configuration = configuration)
         },
         organization = {
           org <- Organization$new()
           org$read_from_hdx(identifier = identifier, configuration = configuration)
         }) 
}


#' @export
filter.HDXObject <- function(x, ...) {
}


