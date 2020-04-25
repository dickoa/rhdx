#' HDX Organization
#'
#' HDX Organization
#'
#' @format NULL
#' @usage NULL
Organization <- R6::R6Class(
  classname = "Organization",

  private = list(
    configuration = NULL
  ),

  public = list(
    #' @field data placeholder for the Organization fields element
    data = NULL,

    #' @description
    #' Create a Organization object
    #'
    #' @param initial_data list with required field to create a dataset
    #' @param configuration a Configuration object
    #' @return A Organization object
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


    #' @description
    #' Get the list of datasets
    #' @return list of Dataset objects
    get_datasets = function() {
      if (!"packages" %in% names(self$data))
        stop("No datasets available, please run Organization$pull with `include_datasets = TRUE` and try again!", call. = FALSE)
      list_of_ds <- lapply(self$data$packages,
                           function(x) Dataset$new(initial_data = x))
      list_of_ds
    },

    #' @description
    #' Browse the Organization page on HDX
    browse = function() {
      url <- private$configuration$get_hdx_site_url()
      browseURL(url = paste0(url, "organization/", self$data$name))
    },

    #' @description
    #' Get dataset field into list
    #'
    #' @return a list with organization field element
    as_list = function() {
      self$data
    },

    #' @description
    #' Print a Dataset object
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

#' @export
#' @aliases Organization
#' @importFrom tibble as_tibble
as_tibble.Organization <- function(x, ...) {
  df <- tibble::tibble(organization_id = x$data$id,
                       organization_name = x$data$name)
  df$organization <- list(x)
  df
}

#' @export
#' @aliases Organization
as.list.Organization <- function(x, ...) {
  x$as_list()
}

#' @noRd
.pull_organization  <-  function(identifier = NULL, include_datasets = FALSE, configuration = NULL, ...) {
  if (!is.null(configuration) & inherits(configuration, "Configuration"))
    set_rhdx_config(configuration = configuration)
  configuration <- get_rhdx_config()
  res <- configuration$call_action("organization_show", list(id = identifier, include_datasets = include_datasets, ...))
  Organization$new(initial_data = res, configuration = configuration)
}

#' Read an HDX organization
#'
#' Read an HDX organization
#'
#' @param identifier character resource uuid
#' @param configuration an HDX configuration object
#' @param include_datasets Logical, include datasets if TRUE
#' @param ... Extra parameters
#' @rdname pull_organization
#'
#' @return HDX organization
#' @export
pull_organization <- memoise::memoise(.pull_organization)

#' @rdname browse
#' @export
browse.Organization <- function(x, ...)
  x$browse()

#' List HDX organization
#'
#' List HDX organization
#'
#' @param sort Character how to sort the results. Default is "name asc"
#' @param all_fields Logical, include all fields
#' @param include_dataset_count Logical include count in the result
#' @param include_groups Logical, whether or not to include locations
#' @param include_user Logical, whether or not to include user
#' @param include_tags Logical whether or not to include tags
#' @param configuration Configuration
#' @param ... extra paramaters
#'
#' @rdname list_organizations
#' @return A list of organizations on HDX
.list_organizations  <-  function(sort = "name asc", all_fields = FALSE, include_dataset_count = TRUE, include_groups = FALSE, include_user = FALSE, include_tags = FALSE, configuration = NULL, ...) {
  if (!is.null(configuration) & inherits(configuration, "Configuration"))
    set_rhdx_config(configuration = configuration)
  configuration <- get_rhdx_config()
  data <- drop_nulls(list(sort = sort, all_fields = all_fields, include_dataset_count = include_dataset_count,
                          include_groups = include_groups, include_user = include_user, include_tags = include_tags))
  res <- configuration$call_action("organization_list", data)
  if (isFALSE(all_fields))
    res <- unlist(res)
  res
}

#' @rdname list_organizations
#' @export
list_organizations <- memoise::memoise(.list_organizations)
