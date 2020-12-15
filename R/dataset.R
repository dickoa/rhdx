#' HDX Dataset
#'
#' Dataset class containing all logic for accessing,
#' creating, and updating datasets and associated resources.
#'
Dataset <- R6::R6Class(
  classname = "Dataset",
  inherit = HDXObject,

  private = list(
    configuration = NULL
  ),
  public = list(
    #' @field resources list of Resource object within the dataset
    resources = NULL,
    #' @field data placeholder for Dataset field element
    data = list(),
    #' @description
    #' Create a new Dataset object
    #'
    #' @param initial_data list with required field to create a dataset
    #' @param configuration a Configuration object
    #' @return A Dataset object
    initialize = function(initial_data = NULL, configuration = NULL) {
      if (is.null(configuration) | !inherits(configuration, "Configuration")) {
        private$configuration <- get_rhdx_config()
      } else {
        private$configuration <- configuration
      }
      if (is.null(initial_data))
        initial_data <- list()
      initial_data <- drop_nulls(initial_data)
      key <- names(initial_data)
      self$data <- initial_data
      if ("resources" %in% key)
        self$resources <- lapply(self$data$resources,
                                 function(x)
                                   Resource$new(initial_data = x,
                                                configuration = configuration))
    },

    #' @description
    #' Get a specific resource of the dataset
    #'
    #' @param index, the index of the resource to access
    #' @return a Resource object, the selected resource
    get_resource = function(index) {
      n_res <- self$data$num_resources
      if (index > n_res)
        stop("Just ", n_res, "resource(s) available!",
             call. = FALSE)
      self$resources[[index]]
    },

    #' @description
    #' Get all resources of the dataset
    #'
    #' @param pattern regex pattern in resource name
    #' @param format format of the resources
    #'
    #' @return a list of Resource objects, all resources available in the dataset
    get_resources = function(pattern = NULL, format = NULL) {
      l <- self$resources

      if (!is.null(pattern)) {
        b <- sapply(self$data$resources,
                    function(x) grepl(pattern, x$name, ignore.case = TRUE))
        l <- l[b]
      }

      if (!is.null(format)) {
        b <- sapply(self$data$resources,
                    function(x) tolower(x$format) %in% tolower(format))
        l <- l[b]
      }

      if (is.null(l) | length(l) < 1)
        l <- list()

      class(l) <- "resources_list"
      l
    },

    #' @description
    #' Get number of dataset resources
    #'
    #' @return The number of Resource objects
    number_of_resources = function() {
      length(self$resources)
    },

    #' @description
    #' Delete a resource by its index
    #'
    #' @param index, the index of the resource to delete
    delete_resource = function(index = 1L) {
      n_resources <- self$data$num_resources
      if (n_resources == 0)
        stop("No resources to delete!", call. = FALSE)
      if (index > n_resources)
        stop("Just ", n_resources, " resource(s) available!",
             call. = FALSE)
      self$data$resources[[index]] <- NULL
      self$resources[[index]] <- NULL
      self$data$num_resources <- max(0, self$data$num_resources - 1)
    },

    #' @description
    #' Delete all resources from a dataset
    delete_resources = function() {
      self$resources <- NULL
      self$data$resources <- NULL
      self$data$num_resources <- 0
    },

    #' @description
    #' Browse the dataset page on HDX
    browse = function() {
      url <- private$configuration$get_hdx_site_url()
      browseURL(url = paste0(url, "dataset/", self$data$name))
    },

    #' @description
    #' Get the current configuration in use
    #'
    #' @return A configuration object, the configuration in use
    get_configuration = function() {
      private$configuration
    },

    #' @description
    #' Get the dataset date
    #'
    #' @return a date, the dataset date.
    get_dataset_date = function() {
      date <- self$data$dataset_date
      if (is.null(date))
        date <- ""
      date
    },

    #' @description
    #' Get dataset update frequency
    #'
    #' @return a character, the dataset update frequency
    get_update_frequency = function() {
      self$data$data_update_frequency
    },

    #' Get dataset tags
    #'
    #' @return a list of Tag objects, datasets tags
    get_tags = function() {
      lapply(self$data$tags, function(x)
        pull_tag(x$id))
    },

    #' @description
    #' Get the datasets location
    #'
    #' @return a list of Location objects, all locations covered by the dataset
    get_locations = function() {
      lapply(self$data$groups, function(x)
        pull_location(x$id))
    },

    #' @description
    #' Get the dataset maintainer
    #'
    #' @return An User object, the maintainer of the dataset
    get_maintainer = function() {
      id <- self$data$maintainer
      pull_user(id)
    },

    #' @description
    #' Get the dataset organization
    #'
    #' @return an Organization object, the organization that shared the data
    get_organization = function() {
      id <- self$data$organization$id
      pull_organization(id)
    },

    #' @description
    #' Get the Showcase associated to the dataset
    #'
    #' @return a Showcase object containing the dataset
    get_showcases = function() {
      configuration <- private$configuration
      id <- self$data$id
      res <- configuration$call_action("ckanext_package_showcase_list",
                                       body = list(package_id = id), verb = "post")
      lapply(res, function(r)
        Showcase$new(initial_data = r, configuration = configuration))
    },

    #' @description
    #' Check if the dataset is requestable
    #'
    #' @return a logical value, `TRUE` if it's a requestable dataset
    is_requestable = function() {
      self$data$is_requestdata_type
    },

    #' @description
    #' Get dataset required fields
    #'
    #' @return list of required fields for a dataset
    get_required_fields = function() {
      hdx_conf <- private$configuration$data$hdx_config
      if (!is.null(self$is_requestable()) && self$is_requestable()) {
        fields <- hdx_conf$`dataset-requestable`$required_fields
      } else {
        fields <- hdx_conf$dataset$required_fields
      }
      fields
    },

    #' @description
    #' Check dataset required field
    #'
    #' @return a logical value, TRUE if the the dataset
    #' is not missing a required field and throws an error otherwise
    check_required_fields = function() {
      n2 <- names(self$data)
      n1 <- self$get_required_fields()
      if (!all(n1 %in% n2)) {
        stop(sprintf("Field %s is missing in the Dataset object!\n",
                     setdiff(n1, n2)),
             call. = FALSE)
      } else {
        TRUE
      }
    },

    #' @description
    #' Get dataset field into list
    #'
    #' @return a list with dataset field
    as_list = function() {
      self$data
    },

    #' @description
    #' Print a Dataset object
    print = function() {
      if (!is.null(self$is_requestable()) && self$is_requestable()) {
        cat(paste0("<HDX Requestable Dataset> ", self$data$id), "\n")
        cat("  Title: ", self$data$title, "\n", sep = "")
        cat("  Name: ", self$data$name, "\n", sep = "")
        cat("  Date: ", self$get_dataset_date(), "\n", sep = "")
        cat("  Tags (up to 5): ", sift_res(self$data$tags), "\n", sep = "")
        cat("  Locations (up to 5): ",
            sift_res(self$data$groups, "title"), "\n", sep = "")
      } else {
        cat(paste0("<HDX Dataset> ", self$data$id), "\n")
        cat("  Title: ", self$data$title, "\n", sep = "")
        cat("  Name: ", self$data$name, "\n", sep = "")
        cat("  Date: ", self$get_dataset_date(), "\n", sep = "")
        cat("  Tags (up to 5): ", sift_res(self$data$tags), "\n", sep = "")
        cat("  Locations (up to 5): ",
            sift_res(self$data$groups, "title"), "\n", sep = "")
        cat("  Resources (up to 5): ",
            sift_res(self$data$resources), "\n", sep = "")
      }
      invisible(self)
    }
  )
)

#' @export
#' @aliases Dataset
as.list.Dataset <- function(x, ...) {
  x$as_list()
}

#' @export
#' @aliases Dataset
#' @importFrom tibble as_tibble
as_tibble.Dataset <- function(x, ...) {
  tibble::tibble(dataset_title = tolower(x$data$title),
                 dataset_name = x$data$name,
                 dataset_date = x$get_dataset_date(),
                 requestable = x$is_requestable(),
                 locations_name = list(get_locations_names(x)),
                 organization_name = get_organization_name(x),
                 resources_format = list(get_resources_formats(x)),
                 tags_name = list(get_tags_names(x)),
                 resources = list(x$get_resources()),
                 dataset = list(x))
}

#' Add resource to dataset
#'
#' Add resource to dataset
#'
#' @param dataset Dataset
#' @param index integer; resource position in the dataset
#' @export
#' @return Resource
get_resource <- function(dataset, index) {
  assert_dataset(dataset)
  dataset$get_resource(index)
}

#' Add resource to dataset
#'
#' Add resource to dataset
#'
#' @param dataset Dataset
#' @param pattern regex pattern in resource name
#' @param format format of the resources
#'
#' @export
#' @return resource_list
get_resources <- function(dataset, pattern = NULL, format = NULL) {
  assert_dataset(dataset)
  dataset$get_resources(pattern = pattern, format = format)
}

#' Delete resource from dataset
#'
#' Delete resource from dataset
#'
#' @details Delete resource from dataset
#'
#' @param dataset Dataset the dataset from which we one to remove the resource
#' @param index Integer the index of the resource to be removed
#'
#' @return Dataset the dataset without the resource
#' @export
delete_resource <- function(dataset, index) {
  assert_dataset(dataset)
  dataset$delete_resource(index)
  dataset
}


#' Delete all resource from dataset
#'
#' Delete all resource from dataset
#'
#' @param dataset A Dataset, the dataset to remove
#'
#' @details Delete all resources from dataset
#'
#' @return Dataset without resources
#' @export
delete_resources <- function(dataset) {
  assert_dataset(dataset)
  dataset$delete_resources()
  dataset
}

#' need to solve the issue with start and paginator
#' @importFrom jsonlite fromJSON
#' @rdname search_datasets
#' @noRd
.search_datasets  <-  function(query = "*:*", filter_query = NULL,
                               rows = 10L, start = 0L, page_size = 1000L,
                               configuration = NULL, ...) {
  if (!is.null(configuration) & inherits(configuration, "Configuration"))
    set_rhdx_config(configuration = configuration)
  configuration <- get_rhdx_config()
  cc <- crul::Paginator$new(client = configuration$remoteclient(),
                            by = "query_params",
                            limit_param = "rows",
                            offset_param = "start",
                            limit = rows,
                            limit_chunk = page_size)
  suppressMessages(cc$get(path = paste0("/api/3/action/", "package_search"),
                          list(q = query, fq = filter_query, ...)))
  list_of_ds <- fromJSON(cc$parse(),
                         simplifyVector = FALSE)$result$results
  list_of_ds <- lapply(list_of_ds,
                       function(x)
                         Dataset$new(initial_data = x,
                                     configuration = configuration))
  class(list_of_ds) <- "datasets_list"
  list_of_ds
}


#' Search for datasets on HDX
#'
#' Search for datasets on HDX
#'
#' @param query Character Query terms, use solr format
#' and default to "*:*" (match everything)
#' @param filter_query Character Filter Query results
#' @param rows integer; Number of matching records to return. Defaults to 10.
#' @param start integer; the offset in the complete result for where
#' the set of returned datasets should begin.
#' @param page_size integer; Size of page to return. Defaults to 1000.
#' @param configuration Configuration object.
#' @param ... Extra parameters for `package_search` endpoints
#'
#' @details Search and find datasets on HDX
#'
#'
#' @return A list of HDX datasets
#'
#' @examples
#' \dontrun{
#'  # Setting the config to use HDX default server
#'  search_datasets("displaced nigeria", rows = 3L)
#' }
#' @rdname search_datasets
#' @importFrom memoise memoise
#' @export
search_datasets <- memoise(.search_datasets)

#' @export
#' @aliases Dataset
as_tibble.datasets_list <- function(x, ...) {
  l <- lapply(x, as_tibble)
  Reduce(rbind, l)
}

#' @noRd
.pull_dataset <-  function(identifier, configuration = NULL) {
  if (!is.null(configuration) & inherits(configuration, "Configuration"))
    set_rhdx_config(configuration = configuration)
  configuration <- get_rhdx_config()
  res <- configuration$call_action("package_show",
                                   list(id = identifier))
  Dataset$new(initial_data = res,
              configuration = configuration)
}

#' Pull HDX dataset into R
#'
#' Read an HDX dataset from its name or id
#'
#' @param identifier Character dataset keyword
#' @param configuration a Configuration object
#'
#' @rdname pull_dataset
#' @return Dataset the dataset
#'
#' @importFrom memoise memoise
#' @export
#' @examples
#' \dontrun{
#' # Setting the config to use HDX default server
#'  set_rhdx_config()
#'  res <- pull_dataset("mali-3wop")
#'  res
#' }
pull_dataset <- memoise(.pull_dataset)


#' List datasets
#'
#' List datasets
#'
#' @param limit  integer; limit
#' @param offset integer; offset
#' @param configuration a Configuration object
#'
#' @rdname list_datasets
#' @return A vector of datasets names
#'
#' @examples
#' \dontrun{
#' # Setting the config to use HDX default server
#'  set_rhdx_config()
#'  list_datasets(limit = 10L)
#' }
.list_datasets  <-  function(limit = NULL, offset = NULL, configuration = NULL) {
  if (!is.null(configuration) & inherits(configuration, "Configuration"))
    set_rhdx_config(configuration = configuration)
  configuration <- get_rhdx_config()
  data <- drop_nulls(list(offset = offset, limit = limit))
  res <- configuration$call_action("package_list", data)
  unlist(res)
}

#' @rdname list_datasets
#' @importFrom memoise memoise
#' @export
list_datasets <- memoise(.list_datasets)

#' @noRd
.count_datasets  <-  function(configuration = NULL) {
  if (!is.null(configuration) & inherits(configuration, "Configuration"))
    set_rhdx_config(configuration = configuration)
  configuration <- get_rhdx_config()
  stats <- hdx_general_statistics()
  stats$datasets$total
}

#' Count all datasets on HDX
#'
#' Count all datasets on HDX
#' @param configuration an HDX Configuration object
#' @rdname count_datasets
#' @importFrom memoise memoise
#' @return An integer, the number of datasets
#' @export
count_datasets <- memoise(.count_datasets)

#' @rdname browse
#' @export
browse.Dataset <- function(x, ...)
  x$browse()

#' Dataset locations name
#'
#' Gets locations name from the datasets
#'
#' @param dataset Dataset
#'
#'
#' @return Character locations of the dataset
#' @export
#'
#' @examples
#' \dontrun{
#' # Setting the config to use HDX default server
#'  set_rhdx_config()
#'  res <- search_dataset(rows = 3L)
#'  get_location_names(res[[1]])
#' }
get_locations_names <- function(dataset) {
  assert_dataset(dataset)
  vapply(dataset$get_locations(),
         function(location)
           location$data$name, character(1))
}

#' Dataset tags name
#'
#' Gets dataset tags name
#'
#' @param dataset Dataset
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
#'  get_tags_names(res[[1]])
#' }
get_tags_names <- function(dataset) {
  assert_dataset(dataset)
  vapply(dataset$data$tags,
         function(tag) tag$name, character(1))
}

#' Dataset organization name
#'
#' Get the organization sharing the data
#'
#' @param dataset Dataset
#'
#'
#' @return Character The name of the organization sharing the data
#' @export
#'
#' @examples
#' \dontrun{
#' # Setting the config to use HDX default server
#'  set_rhdx_config()
#'  res <- search_dataset(rows = 3L)
#'  get_organization_name(res[[1]])
#' }
get_organization_name <- function(dataset) {
  assert_dataset(dataset)
  dataset$data$organization$name
}

#' Dataset resources format
#'
#' Gets format of all resources from the datasets
#'
#' @param dataset Dataset
#'
#'
#'
#' @return Character Format of the resources
#' @export
#'
#' @examples
#' \dontrun{
#' # Setting the config to use HDX default server
#'  set_rhdx_config()
#'  res <- search_dataset(rows = 3L)
#'  get_resources_formats(res[[1]])
#' }
get_resources_formats <- function(dataset) {
  assert_dataset(dataset)
  vapply(dataset$get_resources(),
         function(resource)
           resource$get_format(), character(1))
}

#' Get the Dataset date
#'
#' Date of dataset
#'
#' @param dataset Dataset
#'
#' @return Date, date of the dataset
#' @export
#'
#' @examples
#' \dontrun{
#' # Setting the config to use HDX default server
#'  set_rhdx_config()
#'  res <- search_dataset(rows = 3L)
#'  get_dataset_date(res[[1]])
#' }
get_dataset_date <- function(dataset) {
  assert_dataset(dataset)
  dataset$get_dataset_date()
}
