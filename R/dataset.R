#' HDX Dataset
#'
#' Dataset class containing all logic for creating, checking, and updating datasets and associated resources.
#' 
#' @export
#'
#' @format NULL
#' @usage NULL
#' @details Possible parameters (not all are allowed in each HTTP verb):
#' \itemize{
#'  \item read_from_hdx - query terms, as a named list
#'  \item search_in_hdx  - body as an R list
#'  \item update_from_yaml - one of form, multipart, json, or raw
#'  \item add_tags
#'  \item list_all_datasets 
#'  \item add_update_resources - one of form, multipart, json, or raw
#' }
#'
#' @examples
#' # ---------------------------------------------------------
#' \dontrun{
#' Configuration$create(hdx_site = "demo")
#' dataset <- Dataset$read_from_hdx("acled-conflict-data-for-africa-realtime-2016")
#' dataset
#' }
#' 
Dataset <- R6::R6Class(
  "Dataset",
  private = list(
    configuration = NULL
  ),
  public = list(
    resources = NULL,
    data = list(),
    initialize = function(initial_data = NULL, configuration = NULL) {
      if (is.null(configuration) | !inherits(configuration, "Configuration")) {
        private$configuration <- Configuration$read()
      } else {
        private$configuration <- configuration
      }
      if (is.null(initial_data)) initial_data <- list()
      initial_data <- nc(initial_data)
      key <- names(initial_data)
      self$data <- initial_data
      if ("resources" %in% key)
        self$resources <- lapply(self$data$resources,
                                function(x) Resource$new(initial_data = x, configuration = configuration))
    },
    read_from_hdx = function(identifier, configuration = NULL) {
      if (is.null(configuration) | !inherits(configuration, "Configuration"))
        configuration <- private$configuration
      res <- configuration$call_remoteclient("package_show", list(id = identifier))
      Dataset$new(initial_data = res$result, configuration = configuration)
    },
    get_resources = function() {
      self$resources
    },
    add_resource = function(resource, ignore_dataset_id = FALSE) {
      if (!inherits(resource, "Resource"))
        stop("Not of class `Resource` please use `Resource$new()` to create a resource first!", call. = FALSE)
      if ("package_id" %in% names(resource$data))
        stop("Resource already have a dataset id", call. = FALSE)
      if (length(self$data$resources) > 0) {
        i <- self$data$num_resources
        self$data$resources[[i + 1]] <- resource$data
        self$resources[[i + 1]] <- Resource$new(resource$data)
        self$data$num_resources <- self$data$num_resources + 1
      } else {
        self$data$resources[[1]] <- resource$data
        self$resources <- list(Resource$new(resource$data))
        self$data$num_resources <- 1L
      }
    },
    delete_resource = function(index = 1L) {
      n_resources <- self$data$num_resources
      if (n_resources == 0)
        stop("No resources to delete!", call. = FALSE)
      if (index > n_resources)
        stop("Just ", n_resources, "resource(s) available!")
      self$data$resources[[index]] <- NULL
      self$resources[[index]] <- NULL
      self$data$num_resources <- max(0, self$data$num_resources - 1)
    },
    delete_all_resources = function() {
      self$resources <- NULL
      self$data$resources <- NULL
      self$data$num_resources <- NULL
    },
    browse = function() {
      url <- private$configuration$get_hdx_site_url()
      browseURL(url = paste0(url, "dataset/", self$data$name))
    },
    search_in_hdx = function(query = "*:*", rows = 10L, page_size = 1000L, configuration = NULL, ...) {
      if (is.null(configuration) | !inherits(configuration, "Configuration"))
        configuration <- private$configuration
      cc <- crul::Paginator$new(client = configuration$remoteclient(),
                               by = "query_params",
                               limit_param = "rows",
                               offset_param = "start",
                               limit = rows,
                               limit_chunk = page_size)
      suppressMessages(cc$get(path = paste0("/api/3/action/", "package_search"), list(q = query, ...)))
      ds_list <- unlist(lapply(cc$parse(),
                              function(x) jsonlite::fromJSON(x, simplifyVector = FALSE)$result$results), recursive = FALSE)
      ds_list <- lapply(ds_list,
                       function(x) Dataset$new(initial_data = x, configuration = configuration))
      ds_list
    },
    delete_from_hdx = function() {
      configuration <- private$configuration
      res <- configuration$call_remoteclient("package_delete", list(id = self$data$id))
      res$result$status_code == 200L
    },
    list_all_datasets = function(sort = "name asc", all_fields = FALSE,
                                 include_groups = FALSE, configuration = NULL, ...) {
      if (!sort %in% c("name asc", "name", "package_count", "title"))
        stop("You can just sort by the following parameters `name asc`, `name`, `package_count` or `title`", call. = FALSE)
      if (is.null(configuration) | !inherits(configuration, "Configuration"))
        configuration <- private$configuration
      res <- configuration$call_remoteclient("package_list",
                                            list(sort = sort, all_fields = all_fields, include_groups = include_groups, ...))
      if (!all_fields)
        unlist(res$result)
      res$result
    },
    list_showcases = function() {
      configuration <- private$configuration
      dataset_id <- self$data$id
      res <- configuration$call_remoteclient("ckanext_showcase_list", list(package_id = dataset_id))
      res$result
    },
    update_from_file = function(hdx_dataset_static_file) {
      if (!file.exists(hdx_dataset_static_file))
        stop("HDX static dataset file not found!", call. = FALSE)
      file_ext <- tools::file_ext(hdx_dataset_static_file)
      if (!file_ext %in% c("yml", "json"))
        stop("Only YAML and JSON configuration file are supported for the moment!", call. = FALSE)
      self$data <- switch(file_ext,
                         yml = yaml::yaml.load_file(hdx_dataset_static_file),
                         json = jsonlite::fromJSON(hdx_dataset_static_file, simplifyVector = FALSE))
      if ("resources" %in% names(self$data))
        self$resources <- lapply(self$data$resources,
                                function(x) Resource$new(initial_data = x, configuration = configuration))
    },
    get_configuration = function() {
      private$configuration
    },
    get_dataset_date = function() {
      date <- self$data$dataset_date
      if (is.null(date))
        date <- ""
      date
    },
    set_dataset_date = function(date, format = "%m/%d/%Y") {
      self$data$dataset_date <- format.Date(date, format = format)
    },
    get_update_frequency = function() {
      self$data$data_update_frequency
    },
    set_update_frequency = function(frequency) {
      if (frequency %in% names(update_frequencies))
        stop("Wrong argument for frequency!", call. = FALSE) 
      self$data$data_update_frequency <- update_frequencies[[frequency]]
    },
    get_tags = function() {
      self$data$tags
    },
    add_tags = function(tags) {
      self$data$tags <- lapply(tags, function(tag) list(names = tag))
    },
    get_locations = function() {
      self$data$groups
    },
    add_locations = function(locations) {
      self$data$groups <- lapply(locations,
                                function(location) list(names = location))
    },
    get_maintainer = function() {
      self$data$maintainer
    },
    set_maintainer = function(maintainer) {
      self$data$maintainer <- maintainer
    },
    get_organization = function() {
      self$data$owner_org
    },
    set_organization = function(organization) {
      self$data$owner_org <- organization
    },
    is_requestable = function() {
      self$data$is_requestdata_type
    },
    set_requestable = function(requestable = TRUE) {
      self$data$is_requestable_type <- requestable
      if (requestable)
        self$data$private <- FALSE
    },
    get_required_fields = function() {
      if (!is.null(self$is_requestable()) && self$is_requestable()) {
        fields <- private$configuration$data$hdx_config$`dataset-requestable`$required_fields
      } else {
        fields <- private$configuration$data$hdx_config$dataset$required_fields
      }
      fields
    },
    check_required_fields = function() {
      n2 <- names(self$data)
      n1 <- self$get_required_fields()
      if (!all(n1 %in% n2)) {
        stop(sprintf("Field %s is missing in the dataset!", setdiff(n1, n2)))
      } else {
        TRUE
      }
    },
    count = function(configuration = NULL) {
      if (is.null(configuration) | !inherits(configuration, "Configuration"))
        configuration <- private$configuration
      res <- configuration$call_remoteclient("package_search",
                                            list(q = "*:*", rows = 1L))
      res$result$count
    },
    as_list = function() {
      self$data
    },
    patch_in_hdx = function() {
    },
    update_in_hdx = function(field = NULL, update_resources = FALSE) {
      configuration <- private$configuration
      dataset_id <- self$data$id
      if (is.null(dataset_id))
        stop("Dataset not on HDX use `create_in_hdx` method")
      rs <- self$get_resources()
      ds <- nc(self$data)
      res1 <- configuration$call_remoteclient("package_update",
                                             ds,
                                             verb = "post",
                                             encode = "json")
      if (res1$status_code != 200L)
        stop("Dataset not created check the parameters")
      
      if (update_resources) {
        res2 <- lapply(rs, function(r) r$update_in_hdx(dataset_id))
      } else {
        res2 <- NULL
      }
      invisible(nc(list(dataset = res1, resources = res2)))
    },
    create_in_hdx = function(upload_resources = FALSE) {
      invisible(self$check_required_fields())
      configuration <- private$configuration
      rs <- self$get_resources()
      ds <- self$data
      ds$resources <- NULL
      ds$num_resources <- NULL
      if (!is.null(self$data$id))
        stop("Dataset already exists on HDX use `update_in_hdx`")
      res1 <- configuration$call_remoteclient("package_create",
                                             ds,
                                             verb = "post")
      if (res1$status_code != 200L)
        stop("Dataset not created check the parameters")
      res2 <- lapply(rs, function(r) r$create_in_hdx(res1$result$id))
      invisible(list(dataset = res1, resources = res2))
    },
    print = function() {
      if (!is.null(self$is_requestable()) && self$is_requestable()) {
        cat(paste0("<HDX Requestable Dataset> ", self$data$id), "\n")
        cat("  Title: ", self$data$title, "\n", sep = "")
        cat("  Name: ", self$data$name, "\n", sep = "")
        cat("  Date: ", self$get_dataset_date(), "\n", sep = "")
        cat("  Tags (up to 5): ", sift_res(self$data$tags), "\n", sep = "")
        cat("  Locations (up to 5): ", sift_res(self$data$groups, "title"), "\n", sep = "")
      } else {
        cat(paste0("<HDX Dataset> ", self$data$id), "\n")
        cat("  Title: ", self$data$title, "\n", sep = "")
        cat("  Name: ", self$data$name, "\n", sep = "")
        cat("  Date: ", self$get_dataset_date(), "\n", sep = "")
        cat("  Tags (up to 5): ", sift_res(self$data$tags), "\n", sep = "")
        cat("  Locations (up to 5): ", sift_res(self$data$groups, "title"), "\n", sep = "")
        cat("  Resources (up to 5): ", sift_res(self$data$resources), "\n", sep = "")
      }
      invisible(self)
    }
  )
)

#' @aliases Dataset 
Dataset$read_from_hdx <- function(identifier, configuration = NULL, ...) {
  ds <- Dataset$new()
  ds$read_from_hdx(identifier, configuration = configuration, ...)
}

#' @aliases Dataset 
Dataset$search_in_hdx <- function(query = "*:*", rows = 10L, page_size = 1000L, configuration = NULL, ...) {
  ds <- Dataset$new()
  ds$search_in_hdx(query = query, rows = rows,
                   page_size = page_size, configuration = configuration, ...)
}


#' @aliases Dataset 
Dataset$update_from_file <- function(hdx_dataset_static_file) {
  ds <- Dataset$new()
  ds$update_from_file(hdx_dataset_static_file)
}


#' @aliases Dataset 
Dataset$count <- function(configuration = NULL) {
  ds <- Dataset$new()
  ds$count(configuration = configuration)
}


#' @export
#' @aliases Dataset 
as.list.Dataset <- function(x) {
  x$as_list()
}

#' @export
#' @aliases Dataset
#' @importFrom tibble as_tibble
as_tibble.Dataset <- function(x, ...) {
  df <- tibble::data_frame(dataset_title = tolower(x$data$title),
                          dataset_name = x$data$name,
                          dataset_date = x$get_dataset_date(),
                          requestable = x$is_requestable(),
                          locations_name = lapply(x$get_locations(),
                                                  function(x) x$name),
                          organization_name = x$data$organization$name)
  df$resources_format <- list(tolower(vapply(x$get_resources(),
                                            function(l) l$get_file_type(), FUN.VALUE = "")))   
  df$tags_name <- list(tolower(vapply(x$get_tags(),
                                     function(l) l$name, FUN.VALUE = "")))
  df$resources <- list(x$get_resources())
  df$dataset <- list(x)
  df 
}


#' @export
#' @aliases Dataset 
get_resources <- function(dataset) {
  if (!inherits(dataset, "Dataset"))
    stop("Not a HDX Dataset object!", call. = FALSE)
  dataset$get_resources()
}

#' @export
#' @aliases Dataset 
add_resource <- function(dataset, resource, ignore_dataset_id = FALSE) {
  if (!inherits(dataset, "Dataset"))
    stop("Not a HDX Dataset object!", call. = FALSE)
  dataset$add_resource(resource, ignore_dataset_id = ignore_dataset_id)
  dataset
}

#' @export
#' @aliases Dataset 
delete_resource <- function(dataset, index) {
  if (!inherits(dataset, "Dataset"))
    stop("Not a HDX Dataset object!", call. = FALSE)
  dataset$delete_resource(index)
  dataset
}

#' @export
#' @aliases Dataset 
count_datasets <- function(configuration = NULL) {
  ds <- Dataset$new()
  ds <- ds$count(configuration = configuration)
}

#' @export
#' @aliases Dataset 
search_datasets <- function(query = "*:*", rows = 10L, page_size = 1000L, configuration = NULL, ...) {
  ds <- Dataset$new()
  ds$search_in_hdx(query = query, rows = rows,
                   page_size = page_size, configuration = configuration, ...)
}

#' @export
#' @aliases Dataset 
read_dataset <- function(identifier, configuration = NULL, ...) {
  ds <- Dataset$new()
  ds$read_from_hdx(identifier, configuration = configuration, ...)
}

browse.Dataset <- function(x, ...)
  x$browse()