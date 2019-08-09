#' HDX Organization
#'
#' HDX Organization
#'
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
Organization <- R6::R6Class(
  "Organization",
  
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
    
    pull = function(identifier = NULL, include_datasets = FALSE, configuration = NULL, ...) {
      if (is.null(configuration) | !inherits(configuration, "Configuration"))
        configuration <- private$configuration
      res <- configuration$call_remoteclient("organization_show", list(id = identifier, include_datasets = include_datasets, ...))
      Organization$new(initial_data = res$result, configuration = configuration)
    },

    update = function(verbose = FALSE) {
      invisible(self$check_required_fields())
      configuration <- private$configuration
      org <- self$data

      if (!is.null(self$data$id))
        stop("Organization already exists on HDX use `update_in_hdx`", call. = FALSE)
      
      org_req <- configuration$call_remoteclient(action = "organization_update",
                                                 data = org,
                                                 verb = "post",
                                                 verbose = verbose)
      
      if (org_req$status_code == 200L) {
        ## Replace message by logger
        message(paste0("Organization updated with id: ", ds_req$result$id))
        self$data <- org_req$result
      } else {
        ## Replace message by logger
        warning("Organization not updated, check the parameters!", call. = FALSE)
        message(paste0(org_req$error[[1]], ": ", org_req$error[[2]]))
      }
      invisible(list(organization = org_req))
    },
    
    push = function(verbose = FALSE) {
      invisible(self$check_required_fields())
      configuration <- private$configuration
      org <- self$data

      if (!is.null(self$data$id))
        stop("Organization already exists on HDX use `update`", call. = FALSE)
      
      org_req <- configuration$call_remoteclient(action = "organization_create",
                                                data = org,
                                                verb = "post",
                                                verbose = verbose)
      
      if (org_req$status_code == 200L) {
        ## Replace message by logger
        message(paste0("Organization created with id: ", ds_req$result$id))
        self$data <- org_req$result
      } else {
        ## Replace message by logger
        warning("Organization not created, check the parameters!", call. = FALSE)
        message(paste0(org_req$error[[1]], ": ", org_req$error[[2]]))
      }
      invisible(list(organization = org_req))
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
      if (!"packages" %in% names(self$data)) stop("No datasets available, please run Organization$pull with `include_datasets = TRUE` and try again!", call. = FALSE)
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
as.list.Organization <- function(x) {
  x$as_list()
}

#' @aliases Organization
.pull_organization <- function(identifier = NULL, include_datasets = FALSE, configuration = NULL, ...) {
  org <- Organization$new()
  org$pull(identifier = identifier, include_datasets = include_datasets, configuration = configuration, ...)
}

#' Read an HDX organization
#'
#' Read an HDX organization
#'
#' @param identifier character resource uuid
#' @param configuration an HDX configuration object
#'
#' @return HDX organization
#' @export
#'
#' @examples
#' \dontrun{
#' }
pull_organization <- memoise::memoise(.pull_organization)

#' @aliases read_organization
#' @export
read_organization <- pull_organization

#' @export
#' @aliases Organization 
browse.Organization <- function(x, ...)
  x$browse()


#' List HDX organization
#'
#' List HDX organization
#'
#' @param sort character resource uuid
#' @param all_fields an HDX configuration object
#' @param include_dataset_count Logical include count in the result
#' @param include_groups Logical, whether or not to include locations
#' @param include_user Logical, whether or not to include user
#' @param include_tags Logical whether or not to include tags
#' @param configuration Configuration
#'
#' @return A list of organization
#' @export
#'
#' @examples
#' \dontrun{
#' }
list_organizations <- function(sort = "name asc", all_fields = FALSE, include_dataset_count = TRUE, include_groups = FALSE, include_user = FALSE, include_tags = FALSE, configuration = NULL, ...) {
    org <- Organization$new()
    org$list_organizations(sort = sort, all_fields = all_fields, include_user = include_user, include_groups = include_groups, include_tags = include_tags, include_dataset_count = include_dataset_count, configuration = configuration, ...)
}

#' Create organization in HDX
#'
#' Create organization in HDX
#'
#' @param organization Organization
#'
#' 
#' @return an HDX organization
#' @export
#'
#' @examples
#' \dontrun{
#' }
push_organization <- function(organization, verbose = FALSE) {
  assert_organization(organization)
  organization$push(verbose = FALSE)
}
