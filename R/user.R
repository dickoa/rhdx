#' HDX User
#'
#' HDX user
#'
#' @format NULL
#' @usage NULL
User <- R6::R6Class(
  "User",

  private = list(
    configuration = NULL
  ),

  public = list(
    data = NULL,

    initialize = function(initial_data = NULL, configuration = NULL) {
      if (is.null(configuration) | !inherits(configuration, "Configuration")) {
        private$configuration <- configuration_read()
      } else {
        private$configuration <- configuration
      }
      if (is.null(initial_data)) initial_data <- list()
      initial_data <- nc(initial_data)
      self$data <- initial_data
    },

    update_from_yaml = function(hdx_user_static_yaml) {
      self$data <- yaml::read_yaml(hdx_user_static_yaml)
    },

    update_from_json = function(hdx_user_static_json) {
      self$data <- jsonlite::read_json(hdx_user_static_json,
                                       simplifyVector = TRUE)
    },

    pull = function(identifier = NULL, include_datasets = FALSE, configuration = NULL, ...) {
      if (is.null(configuration) | !inherits(configuration, "Configuration"))
        configuration <- private$configuration
      res <- configuration$call_remoteclient("user_show", list(id = identifier, include_datasets = include_datasets, ...))
      User$new(initial_data = res$result, configuration = configuration)
    },

    list_users = function(order_by = "number_created_packages", configuration = NULL, ...) {
      if (!sort %in% c("name", "number_of_edits", "number_created_packages")) stop("You can just sort by the following parameters `name`, `number_of_edits` or `number_created_packages`")
      if (is.null(configuration) | !inherits(configuration, "Configuration"))
        configuration <- private$configuration
      res <- configuration$call_remoteclient("user_list", list(order_by = order_by, ...))
      if (!all_fields)
        unlist(res$result)
      res$result
    },

    as_list = function() {
      self$data
    },

    print = function() {
      cat(paste0("<HDX User> ", self$data$id), "\n")
      cat("  Name: ", self$data$name, "\n", sep = "")
      cat("  Display Name: ", self$data$display_name, "\n", sep = "")
      cat("  No Packages: ", self$data$number_created_packages, "\n", sep = "")
      cat("  No Edits: ", self$data$number_of_edits, "\n", sep = "")
      invisible(self)
    }
  )
)


#' @export
#' @aliases User
#' @importFrom tibble as_tibble
as_tibble.User <- function(x, ...) {
  df <- tibble::tibble(
    user_id = x$data$id,
    user_name = x$data$name)
  df$user <- list(x)
  df
}

#' @export
#' @aliases User
as.list.User <- function(x, ...) {
  x$as_list()
}


#' Read an HDX user
#'
#'
#' Read an HDX user from its name or id
#'
#' @param identifier character user keyword
#' @param configuration a Configuration object
#' @param include_datasets Logical, if TRUE add datasets
#' @param ... Extra parameters
#'
#' @rdname pull_user
#' @return User the user
#'
#' @examples
#' \dontrun{
#' # Setting the config to use HDX default server
#'  set_rhdx_config()
#'  res <- pull_user("xxxx")
#'  res
#' }
.pull_user <- function(identifier, include_datasets = FALSE, configuration = NULL, ...) {
  user <- User$new()
  user$pull(identifier = identifier, include_datasets = include_datasets, configuration = configuration, ...)
}

#' @rdname pull_user
#' @export
pull_user <- memoise::memoise(.pull_user)


#' List all users
#' @param sort Logical user sorted is TRUE
#' @param all_fields Logical if TRUE get all field
#' @param configuration Configuration the configuration to use
#' @param ... Extra parameters
#'
#' @export
list_users <- function(sort = "name asc", all_fields = FALSE, configuration = NULL, ...) {
  user <- User$new()
  user$lists_all_user(sort = sort, all_fields = all_fields, configuration = configuration, ...)
}
