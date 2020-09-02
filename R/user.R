#' HDX User
#'
#' HDX user
#'
#' @format NULL
#' @usage NULL
User <- R6::R6Class(
  classname = "User",
  inherit = HDXObject,

  private = list(
    configuration = NULL
  ),

  public = list(
    #' @field data placeholder for Dataset field element
    data = NULL,

    #' @description
    #' Create a new
    #'
    #' @param initial_data list of field required to create a dataset
    #' @param configuration Configuration configuration to use
    #' @return a new User object
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
    #' Get dataset field into list
    #'
    #' @return a list with dataset field
    as_list = function() {
      self$data
    },

    #' @description
    #' Print a User object
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

#' @noRd
#' @rdname pull_user
.pull_user  <-  function(identifier = NULL,
                         include_datasets = FALSE, configuration = NULL, ...) {
  if (is.null(configuration) | !inherits(configuration, "Configuration"))
    set_rhdx_config(configuration = configuration)
  configuration <- get_rhdx_config()
  res <- configuration$call_action("user_show",
                                   list(id = identifier,
                                        include_datasets = include_datasets, ...))
  User$new(initial_data = res, configuration = configuration)
}

#' Read an HDX user
#'
#'
#' Read an HDX user from its name or id
#'
#' @importFrom memoise memoise
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
pull_user <- memoise(.pull_user)

#' @noRd
.list_users  <-  function(order_by = "number_created_packages", configuration = NULL, ...) {
  if (!order_by %in% c("name", "number_of_edits", "number_created_packages"))
    stop("You can just sort by the following parameters `name`, `number_of_edits` or `number_created_packages`",
         call. = TRUE)
  if (!is.null(configuration) & inherits(configuration, "Configuration"))
    set_rhdx_config(configuration = configuration)
  configuration <- get_rhdx_config()
  res <- configuration$call_action("user_list",
                                   list(order_by = order_by, ...))
  res
}


#' List all users
#'
#' List all users
#'
#' @importFrom memoise memoise
#'
#' @param order_by Logical user sorted is TRUE
#' @param configuration Configuration the configuration to use
#' @param ... Extra parameters
#'
#' @rdname list_users
#' @export
#'
#' @examples
#' \dontrun{
#' # Setting the config to use HDX default server
#'  set_rhdx_config()
#'  list_user()
#' }
list_users <- memoise(.list_users)
