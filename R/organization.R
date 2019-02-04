#' Create and manipulate HDX Organization
#'
#'
#' @export
#' @details
#' **Methods**
#'   \describe{
#'   }
#'
#' @format NULL
#' @usage NULL
#'
#' @examples
#' \dontrun{
#' }
#' 
Organization <- R6::R6Class(
  "Organization",
  
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
    
    read_from_hdx = function(identifier = NULL, include_datasets = FALSE, configuration = NULL, ...) {
      if (is.null(configuration) | !inherits(configuration, "Configuration"))
        configuration <- private$configuration
      res <- configuration$call_remoteclient("organization_show", list(id = identifier, include_datasets = include_datasets, ...))
      Organization$new(initial_data = res$result, configuration = configuration)
    },
    
    list_organizations = function(sort = "name asc", all_fields = FALSE, include_dataset_count = TRUE, include_groups = FALSE, include_user = FALSE, include_tags = FALSE, configuration = NULL, ...) {
      if (!sort %in% c("name asc", "name", "package_count", "title"))
        stop("You can just sort by the following parameters `name asc`, `name`, `package_count` or `title`", call. = FALSE)
      if (is.null(configuration) | !inherits(configuration, "Configuration"))
        configuration <- private$configuration
      data <- drop_nulls(list(sort = sort, all_fields = all_fields, include_dataset_count = include_dataset_count,
                              include_groups = include_groups, include_user = include_user, include_tags = include_tags))
      res <- configuration$call_remoteclient("organization_list", data)
      if (isFALSE(all_fields))
        unlist(res$result)
      res$result
    },
    
    get_datasets = function() {
      if (!"packages" %in% names(self$data)) stop("No datasets available, please run Organization$read_from_hdx with `include_datasets = TRUE` and try again!", call. = FALSE)
      list_of_ds <- lapply(self$data$packages, function(x) Dataset$new(initial_data = x))
      list_of_ds
    },
    
    browse = function() {
      url <- private$configuration$get_hdx_site_url()
      browseURL(url = paste0(url, "organization/", self$data$name))
    },
    
    as_list = function() {
      self$data
    },
    
    print = function() {
      cat(paste0("<HDX Organization> ", self$data$id), "\n")
      cat("  Name: ", self$data$name, "\n", sep = "")
      cat("  Display name: ", self$data$display_name, "\n", sep = "")
      cat("  No. Datasets: ", self$data$package_count, "\n", sep = "")
      cat("  No. Members: ", length(self$data$users), "\n", sep = "")
      invisible(self)
    }
  )
)

#' @aliases Organization
Organization$read_from_hdx <- function(identifier = NULL, include_datasets = FALSE, configuration = NULL, ...) {
  org <- Organization$new()
  org$read_from_hdx(identifier = identifier, include_datasets = include_datasets, configuration = configuration, ...)
}

#' @aliases Organization
Organization$search_in_hdx <- function(query = "*:*", configuration = NULL, ...) {
  org <- Organization$new()
  org$search_in_hdx(query = query, configuration = configuration, ...)
}

#' @aliases Organization
Organization$list_all_organizations <- function(sort = "name asc", all_fields = FALSE, include_groups = FALSE, configuration = NULL, ...) {
  org <- Organization$new()
  org$list_all_organizations(sort = sort, all_fields = all_fields, include_groups = include_groups, configuration = configuration, ...)
}

#' @aliases Organization
Organization$get_datasets <- function(identifier = NULL, configuration = NULL, ...) {
  org <- Organization$new()
  res <- org$read_from_hdx(identifier = identifier, include_datasets = TRUE, configuration = configuration, ...)
  res$get_datasets()
}

 
#' @export
#' @aliases Organization 
#' @importFrom tibble as_tibble
as_tibble.Organization <- function(x, ...) {
  df <- tibble::data_frame(
    organization_id = x$data$id,
    organization_name = x$data$name)
  df$organization <- list(x)
  df
}


#' @export
#' @aliases Organization 
as.list.Organization <- function(x) {
  x$as_list()
}

#' @aliases Organization
.read_organization <- function(identifier = NULL, include_datasets = FALSE, configuration = NULL, ...) {
  org <- Organization$new()
  org$read_from_hdx(identifier = identifier, include_datasets = include_datasets, configuration = configuration, ...)
}

#' Read an HDX organization
#'
#' Read an HDX organization 
#'
#' @param identifier character resource uuid
#' @param configuration an HDX configuration object
#' 
#'
#' @return HDX organization
#' @export
#'
#' @examples
#' \dontrun{
#' }
#' 
read_organization <- memoise::memoise(.read_organization)

#' @export
#' @aliases Organization 
browse.Organization <- function(x, ...)
  x$browse()