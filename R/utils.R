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
parse_hdx_date_range <- function(x) {
  reg <- gregexpr("\\d{4}\\-\\d{2}\\-\\d{2}", x)
  regmatches(x, reg)[[1]]
}


#' @noRd
check_config_params <- function(hdx_site = NULL, hdx_key = NULL, hdx_config_file = NULL, read_only = NULL, user_agent = NULL) {

  if (!is.null(hdx_site) && !hdx_site %in% c("prod", "test", "feature", "demo"))
    stop("hdx_site can be either `prod`, `test`, `feature` or `demo`", call. = FALSE)

  if (!is.null(read_only) && !is.logical(read_only))
    stop("read_only should take a logical value, either `TRUE` or `FALSE`", call. = FALSE)

  if (!is.null(user_agent) && !is.character(user_agent))
    stop("user_agent should be a character", call. = FALSE)

  if (!is.null(hdx_key) && !is_valid_uuid(hdx_key))
    stop("HDX API key not valid!", call. = FALSE)

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
  if (is.null(configuration) | !inherits(configuration, "HDXConfig"))
    stop("HDX configuration not set! Use `set_rhdx_config`", call. = FALSE)

#' @noRd
assert_dataset <- function(x, requestable = NULL) {
  if (!inherits(x, "HDXDataset"))
    stop("Not an HDX dataset!", call. = FALSE)
  if (!is.null(requestable) && isTRUE(requestable) && isFALSE(x$is_requestable()))
    stop("Not an HDX requestable dataset", call. = FALSE)
  if (!is.null(requestable) && isFALSE(requestable) && isTRUE(x$is_requestable()))
    stop("Not a non requestable HDX dataset", call. = FALSE)
}

#' @noRd
assert_datasets_list <- function(x) {
  if (!inherits(x, "hdx_datasets_list"))
    stop("Not a list of HDX Datasets!", call. = FALSE)
  invisible(x)
}

#' @noRd
assert_resource <- function(x) {
  if (!inherits(x, "HDXResource"))
    stop("Not an HDX Resource object!", call. = FALSE)
  invisible(x)
}

#' @noRd
assert_organization <- function(x) {
  if (!inherits(x, "HDXOrganization"))
    stop("Not an HDX Organization object!", call. = FALSE)
  invisible(x)
}

#' @noRd
assert_location <- function(x) {
  loc <- countrycode::codelist$iso3c
  pattern <- paste0("^", x, "$")
  cond <- any(grepl(pattern, loc, ignore.case = TRUE))
  if (!cond)
    stop("Not a valid HDX Location, enter the ISO3 code of the country!",
         call. = FALSE)
  invisible(x)
}

#' @noRd
assert_cache <- function(x)
  if (!inherits(x, "HoardClient"))
    stop("Not a `hoardr` cache object", call. = FALSE)

#' @noRd
parse_response <- function(res) {
  if(!inherits(res, "HttpResponse"))
    stop("Not a API call response object!", call. = FALSE)
  if (res$status_code < 400) {
    x <- jsonlite::fromJSON(res$parse(encoding = "UTF-8"),
                            simplifyVector = FALSE)
    x <- x$result
  } else {
    x <- list()
  }
  x
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
#' @importFrom utils packageVersion
get_user_agent <- function(x) {
  rhdx_version <- packageVersion("rhdx")
  os <- Sys.info()[["sysname"]]
  os_version <- paste(Sys.info()[["release"]], Sys.info()[["version"]])
  r_version <- paste0(R.version$major, ".",
                      R.version$minor,
                      ifelse(R.version$status == "", "",
                             paste0("-", R.version$status)))
  header <- paste0("rhdx/", rhdx_version, " (", os, "/",
                   os_version, "; ", "R/", r_version, ")")
  header
}

#' @noRd
#' @author Dirk Schumascher
find_schema_row <- function(tbl) {
  stopifnot(is.data.frame(tbl))
  if (any(is_valid_tag(colnames(tbl)))) {
    return(0)
  } else {
    for (i in seq_len(pmin(nrow(tbl), 25))) {
      row <- unlist(apply(tbl[i, ], 2, as.character))
      if (any(is_valid_tag(row))) {
        return(i)
      }
    }
  }
  -1
}

#' Strip HXL tags from tibble
#'
#' Strip HXL tags from tibble
#' @importFrom readr type_convert
#' @param x a tibble with HXL tags
#' @return tibble
#' @noRd
strip_hxl <- function(x) {
  tbl <- tibble::as_tibble(x)
  schema_row <- find_schema_row(tbl)
  base_tbl <- if (schema_row > 0) {
    new_tbl <- tbl[-1 * 1L:schema_row, ]
    suppressMessages(type_convert(new_tbl))
  } else {
    tbl
  }
  base_tbl
}

#' @noRd
#' @author Dirk Schumascher
is_valid_tag <- function(tag) {
  ltag <- tolower(trimws(tag))
  pattern <- "^#[a-z][a-z0-9_]*(\\s+(\\+|-)\\s*[a-z][a-z0-9_]*)*"
  grepl(x = ltag, pattern = pattern)
}

#' @importFrom jsonlite fromJSON
#' @noRd
read_hdx_json <- function(file, simplify_json = FALSE, ...) {
  check_packages("jsonlite")
  fromJSON(file, simplifyVector = simplify_json, ...)
}

#' @importFrom readr read_delim default_locale locale
#' @noRd
read_hdx_delim <- function(file, hxl = FALSE, delim = NULL, locale = default_locale(), ...) {
  check_packages("readr")
  if (is.null(delim))
    delim <- ","
  df <- read_delim(file, delim = delim, locale = locale, ...)
  if (isTRUE(hxl))
    df <- strip_hxl(df)
  df
}

#' @importFrom readxl excel_sheets read_excel
#' @noRd
read_hdx_excel <- function(file = NULL, sheet = NULL, hxl = FALSE, ...) {
  check_packages("readxl")
  if (is.null(sheet)) {
    sheet <- excel_sheets(file)[[1]]
    cat("Reading sheet: ", sheet, "\n")
  }
  df <- read_excel(file, sheet = sheet, ...)
  if (isTRUE(hxl))
    df <- strip_hxl(df)
  df
}

#' @importFrom sf st_layers
#' @noRd
get_hdx_layers_ <- function(file = NULL) {
  check_packages("sf")
  zipped <- grepl("\\.zip$", file, ignore.case = TRUE)
  if (zipped)
    file <- file.path("/vsizip", file)
  st_layers(file)$name
}

#' @importFrom readxl excel_sheets
#' @noRd
get_hdx_sheets_ <- function(file = NULL) {
  check_packages("readxl")
  excel_sheets(file)
}

#' @importFrom sf read_sf st_layers
#' @noRd
read_hdx_vector <- function(file = NULL, layer = NULL, ...) {
  check_packages("sf")
  zipped <- grepl("\\.zip$", file, ignore.case = TRUE)
  if (zipped)
    file <- file.path("/vsizip", file)
  if (is.null(layer)) {
    layer <- st_layers(file)$name[1]
    message("Reading layer: ", layer, "\n")
  }
  read_sf(dsn = file, layer = layer, ...)
}


#' @importFrom stars read_stars
#' @noRd
read_hdx_geotiff <- function(file = NULL, raster_file_path = "",...) {
  check_packages("stars")
  zipped <- grepl("\\.zip$", file, ignore.case = TRUE)
  if (zipped) {
    l <- unzip(file, list = TRUE)
    geo_file <- grep("\\.tif$", l$Name, value = TRUE)
    geo_file <- geo_file[1]
    if (length(geo_file) <= 0)
      geo_file <- ""
    file <- file.path("/vsizip", file, geo_file)
  }
  read_stars(file, ...)
}

#' Encode URL from proxy.hxlstandard
#'
#' URL using are partially encoded we need to change space into %20
#'
#' @return Character encoded url
#' @noRd
url_encode_proxy <- function(url)
  gsub("\\s", "%20", url)

#' @noRd
#' @param z object to display
#' inspired by Scott Chamberlain function sift_res
#' @importFrom stats na.omit
sift_res <- function(z, key = "name", n = 5) {
  if (!is.null(z) && length(z) > 0) {
    if (!key %in% names(z)) key <- "name"
    r <- na.omit(vapply(z,
                        function(x) if (length(x) > 0) paste0(x[[key]], ", ") else "",
                        FUN.VALUE = "character")[1:n])
    gsub(", $", "", paste0(r, collapse = ""))
  } else {
    ""
  }
}

#' Browse a HDX object
#'
#' Browse a HDX object
#'
#' @param x an HDX object
#' @param ... Extra parameters
#' @rdname browse
#'
#'
#' @return Character Tags of the dataset
#' @export
#'
#' @examples
#' \dontrun{
#' # Setting the config to use HDX default server
#'  set_rhdx_config()
#'  res <- search_dataset(rows = 3L)
#'  browse(res[[1]])
#' }
browse <- function(x, ...)
  UseMethod("browse", x)

#' @rdname browse
#' @export
browse.default <- function(x, ...)
  x$browse()
