#' HDX Location
#'
#' HDX location mostly countries
#'
#' @format NULL
#' @usage NULL
HDXLocation <- R6::R6Class(
  classname = "HDXLocation",
  inherit = HDXObject,

  private = list(
    configuration = NULL
  ),

  public = list(
    #' @field data placeholder location
    data = NULL,

    #' @description
    #' Create a new Location object
    #'
    #' @param initial_data list with required field to create a dataset
    #' @param configuration a Configuration object
    #' @return A Location object
    initialize = function(initial_data = NULL, configuration = NULL) {
      if (is.null(configuration) | !inherits(configuration, "HDXConfig")) {
        private$configuration <- get_rhdx_config()
      } else {
        private$configuration <- configuration
      }
      if (is.null(initial_data))
        initial_data <- list()
      initial_data <- drop_nulls(initial_data)
      self$data <- initial_data
    },

    #' @description
    #' Get dataset required fields
    #'
    #' @return list of required fields for a dataset
    get_required_fields = function() {
      private$configuration$data$hdx_config$resource$required_fields
    },

    #' @description
    #' Check dataset required field
    #'
    #' @return a logical value, TRUE if the the dataset is not
    #' missing a required field and throws an error otherwise
    check_required_fields = function() {
      n2 <- names(self$data)
      n1 <- self$get_required_fields()
      if (!all(n1 %in% n2)) {
        stop(sprintf("Field %s is missing in the dataset!\n",
                     setdiff(n1, n2)),
             call. = FALSE)
      } else {
        TRUE
      }
    },

    #' @description
    #' Browser the Location page on HDX
    browse = function() {
      url <- private$configuration$get_hdx_site_url()
      browseURL(url = paste0(url, "/group/", self$data$name))
    },

    #' @description
    #' Get location field into list
    #'
    #' @return a list with dataset field
    as_list = function() {
      self$data
    },

    #' @description
    #' Print a Dataset object
    print = function() {
    cat(paste0("<HDX Location> ", self$data$id), "\n")
    cat("  Name: ", self$data$name, "\n", sep = "")
    cat("  Display Name: ", self$data$display_name, "\n", sep = "")
    cat("  No Datasets: ", self$data$package_count, "\n", sep = "")
    invisible(self)
    }
  )
)

#' @noRd
#' @rdname pull_location
.pull_location <- function(identifier = NULL, include_datasets = FALSE,
                           configuration = NULL, ...) {
  if (!is.null(configuration) & inherits(configuration, "HDXConfig"))
    set_rhdx_config(configuration = configuration)
  configuration <- get_rhdx_config()
  identifier <- assert_location(tolower(identifier))
  res <- configuration$call_action("group_show",
                                   list(id = identifier,
                                        include_datasets = include_datasets,
                                        ...))
  HDXLocation$new(initial_data = res,
                  configuration = configuration)
}

#' Read an HDX location
#'
#' Read an HDX location
#'
#' @importFrom memoise memoise
#'
#' @param identifier Character location uuid
#' @param configuration Configuration a configuration object
#' @param include_datasets Logical whether to include or not dataset
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
pull_location <- memoise(.pull_location)

#' @export
#' @aliases Location
#' @importFrom tibble as_tibble
as_tibble.HDXLocation <- function(x, ...) {
  df <- tibble::tibble(
    location_id = x$data$id,
    location_name = x$data$name)
  df$location <- list(x)
  df
}

#' @export
#' @aliases Location
as.list.HDXLocation <- function(x, ...) {
  x$as_list()
}

#' @noRd
#' @rdname list_locations
.list_locations  <-  function(sort = "name asc",
                              all_fields = FALSE, configuration = NULL, ...) {
  if (!sort %in% c("name asc", "name", "package_count", "title"))
    stop("You can just sort by the following parameters `name asc`, `name`, `package_count` or `title`", call. = FALSE)
  if (!is.null(configuration) & inherits(configuration, "HDXConfig"))
    set_rhdx_config(configuration = configuration)
  configuration <- get_rhdx_config()
  res <- configuration$call_action("group_list",
                                   list(sort = sort,
                                        all_fields = all_fields, ...))
  if (!all_fields)
    res <- unlist(res)
  res
}


#' List locations
#'
#' List locations
#'
#' @importFrom memoise memoise
#'
#' @param sort Character sorting of the search results. Default: “name asc”, the allowed fields are ‘name’, ‘package_count’ and ‘title’
#' @param all_fields Logical if TRUE returns list instead of just names
#' @param ... Extra parameters to group_list
#' https://docs.ckan.org/en/ckan-2.8.2/api/index.html#ckan.logic.action.get.group_list
#' @param configuration a Configuration
#'
#' @return A vector of locations names
#'
#' @rdname list_locations
#' @importFrom memoise memoise
#' @export
#'
#' @examples
#' \dontrun{
#' # Setting the config to use HDX default server
#'  set_rhdx_config()
#'  list_locations(limit = 10L)
#' }
list_locations <- memoise(.list_locations)
