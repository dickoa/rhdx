#' HDX Location
#'
#' HDX location mostly countries
#'
#' @format NULL
#' @usage NULL
Location <- R6::R6Class(
  classname = "Location",

  private = list(
    configuration = NULL
  ),

  public = list(
    data = NULL,

    initialize = function(initial_data = NULL, configuration = NULL) {
      if (is.null(configuration) | !inherits(configuration, "Configuration")) {
        private$configuration <- get_rhdx_config()
      } else {
        private$configuration <- configuration
      }
      if (is.null(initial_data))
        initial_data <- list()
      initial_data <- drop_nulls(initial_data)
      self$data <- initial_data
    },

    get_required_fields = function() {
      private$configuration$data$hdx_config$resource$required_fields
    },

    check_required_fields = function() {
      n2 <- names(self$data)
      n1 <- self$get_required_fields()
      if (!all(n1 %in% n2)) {
        stop(sprintf("Field %s is missing in the dataset!\n", setdiff(n1, n2)),
             call. = FALSE)
      } else {
        TRUE
      }
    },

    browse = function() {
      url <- private$configuration$get_hdx_site_url()
      browseURL(url = paste0(url, "group/", self$data$name))
    },

    as_list = function() {
      self$data
    },

    print = function(x, ...) {
    cat(paste0("<HDX Location> ", self$data$id), "\n")
    cat("  Name: ", self$data$name, "\n", sep = "")
    cat("  Display Name: ", self$data$display_name, "\n", sep = "")
    cat("  No Datasets: ", self$data$package_count, "\n", sep = "")
    invisible(self)
    }
  )
)

#' @noRd
.pull_location <- function(identifier = NULL, include_datasets = FALSE, configuration = NULL, ...) {
  if (!is.null(configuration) & inherits(configuration, "Configuration"))
    set_rhdx_config(configuration = configuration)
  configuration <- get_rhdx_config()
  identifier <- assert_location(tolower(identifier))
  res <- configuration$call_action("group_show", list(id = identifier, include_datasets = include_datasets, ...))
  Location$new(initial_data = res, configuration = configuration)
}

#' Read an HDX location
#'
#' Read an HDX location
#'
#' @param identifier Character location uuid
#' @param configuration Configuration a configuration object
#' @param ... Extra parameters
#'
#' @rdname pull_location
#' @return Location
#'
#' @export
#' @examples
#' \dontrun{
#' #Setting the config to use HDX default server
#'  set_rhdx_config()
#'  res <- pull_location("mli")
#'  res
#' }
pull_location <- memoise::memoise(.pull_location)

#' @export
#' @aliases Location
#' @importFrom tibble as_tibble
as_tibble.Location <- function(x, ...) {
  df <- tibble::tibble(
    location_id = x$data$id,
    location_name = x$data$name)
  df$location <- list(x)
  df
}

#' @export
#' @aliases Location
as.list.Location <- function(x, ...) {
  x$as_list()
}

#' List locations
#'
#' List locations
#'
#' @param limit  Integer limit
#' @param offset Integer offset
#' @param configuration a Configuration
#'
#' @rdname list_locations
#' @return A vector of locations names
#'
#' @examples
#' \dontrun{
#' # Setting the config to use HDX default server
#'  set_rhdx_config()
#'  list_locations(limit = 10L)
#' }
.list_locations  <-  function(sort = "name asc", all_fields = FALSE, configuration = NULL, ...) {
  if (!sort %in% c("name asc", "name", "package_count", "title"))
    stop("You can just sort by the following parameters `name asc`, `name`, `package_count` or `title`", call. = FALSE)
  if (!is.null(configuration) & inherits(configuration, "Configuration"))
    set_rhdx_config(configuration = configuration)
  configuration <- get_rhdx_config()
  res <- configuration$call_action("group_list", list(sort = sort, all_fields = all_fields, ...))
  if (!all_fields)
    res <- unlist(res)
  res
}

#' @rdname list_locations
#' @importFrom memoise memoise
#' @export
list_locations <- memoise::memoise(.list_locations)
