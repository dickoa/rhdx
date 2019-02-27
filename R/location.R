#' Create and manipulate HDX Location
#'
#' HDX location mostly countries
#'
#' @export
#' @details
#' **Methods**
#'   \describe{
#'   }
#'
#' @format NULL
#' @usage NULL
#' @examples
#' # ---------------------------------------------------------
#' \dontrun{
#' Configuration$create(hdx_site = "demo")
#' org <- Location$read_from_hdx("ocha-mali", include_dataset = TRUE)
#' org
#' }
#' 
#' @export
Location <- R6::R6Class(
  "Location",
  
  private = list(
    configuration = NULL
  ),
  
  public = list(
    data = NULL,
    
    initialize = function(initial_data = NULL, configuration = NULL) {
      if (is.null(configuration) | !inherits(configuration, "Configuration")) {
        private$configuration <- Configuration$read()
      } else {
        private$configuration <- configuration
      }
      if (is.null(initial_data)) initial_data <- list()
      initial_data <- nc(initial_data)
      self$data <- initial_data
    },
    
    update_from_yaml = function(hdx_location_static_yaml) {
      self$data <- yaml::read_yaml(hdx_location_static_yaml)
    },
    
    update_from_json = function(hdx_location_static_json) {
      self$data <- jsonlite::read_json(hdx_location_static_json, simplifyVector = TRUE)
    },
    
    read_from_hdx = function(identifier = NULL, include_datasets = FALSE, configuration = NULL, ...) {
      if (is.null(configuration))
        configuration <- private$configuration
      res <- configuration$call_remoteclient("group_show", list(id = identifier, include_datasets = include_datasets, ...))
      Location$new(initial_data = res$result, configuration = configuration)
    },
    
    list_all_locations = function(sort = "name asc", all_fields = FALSE, configuration = NULL, ...) {
      if (!sort %in% c("name asc", "name", "package_count", "title")) stop("You can just sort by the following parameters `name asc`, `name`, `package_count` or `title`", call. = FALSE)
      if (is.null(configuration))
        configuration <- private$configuration
      res <- configuration$call_remoteclient("group_list", list(sort = sort, all_fields = all_fields, ...))
      if (!all_fields)
        unlist(res$result)
      res$result
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

#' @aliases Location
Location$read_from_hdx <- function(identifier = NULL, include_datasets = FALSE, configuration = NULL, ...) {
  loc <- Location$new()
  loc$read_from_hdx(identifier = identifier,
                    include_datasets = include_datasets,
                    configuration = configuration, ...)
}

#' @aliases Location
Location$list_all_locations <- function(sort = "name asc", all_fields = FALSE, configuration = NULL, ...) {
  loc <- Location$new()
  loc$list_all_location(sort = sort,
                        all_fields = all_fields,
                        configuration = configuration, ...)
}

#' @aliases Location
.pull_location <- function(identifier = NULL, configuration = NULL, ...) {
  loc <- Location$new()
  loc$read_from_hdx(identifier = identifier, configuration = configuration, ...)
}

#' Read an HDX location
#'
#' Read an HDX location
#'
#' @param identifier character location uuid
#' @param configuration an HDX configuration object
#' 
#'
#' 
#' @return Location
#' @export
#'
#' @examples
#' \dontrun{
#' #Setting the config to use HDX default server
#'  set_rhdx_config()
#'  res <- read_location("mli")
#'  res
#' }
pull_location <- memoise::memoise(.pull_location)

#' @aliases read_location
#' @export
read_location <- pull_location


#' @export
#' @aliases Location 
#' @importFrom tibble as_tibble
as_tibble.Location <- function(x, ...) {
  df <- tibble::data_frame(
    location_id = x$data$id,
    location_name = x$data$name)
  df$location <- list(x)
  df
}

#' @export
#' @aliases Location 
as.list.Location <- function(x) {
  x$as_list()
}
