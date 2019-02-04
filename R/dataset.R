#' HDX Dataset
#'
#' Dataset class containing all logic for accessing, creating, and updating datasets and associated resources.
#'
#' 
#' @section Details:
#' @details
#' **Methods**
#'   \describe{
#'     \item{`Dataset$new(initial_data, configuration)`}{
#'       Dataset class enabling operations on datasets and associated resources
#'     }
#' 
#'    \item{`add_resource(resource, ignore_datasetid = FALSE)`}{
#'       Add new resource in dataset with new metadata
#'     }
#'
#'   \item{`check_required_fields()`}{
#'      Check that metadata for dataset and its resources is complete.
#'     }
#'
#'    \item{`create_in_hdx(upload_resources)`}{
#'     Check if dataset exists in HDX and if so, update it, otherwise create it
#'     }
#'
#'    \item{`delete_from_hdx(identifier, configuration)`}{
#'       Reads the dataset given by identifier from HDX and returns Dataset object
#'     }
#' 
#'    \item{`get_configuration(identifier, configuration)`}{
#'       Returns the actual config used to get the dataset
#'     }
#' 
#'    \item{`get_dataset_date()`}{
#'       Get dataset date as string.
#'     }
#' 
#'     \item{`read_from_hdx(identifier, configuration)`}{
#'       Reads the dataset given by identifier from HDX and returns Dataset object
#'     }
#' 
#'     \item{`get_resource(index)`}{
#'       Get one resource from dataset by index
#'     }
#' 
#'     \item{`get_resources()`}{
#'       Get dataset’s resources
#'     }
#' 
#'     \item{`get_update_frequency()`}{
#'       Get expected update frequency.
#'     }
#' 
#'     \item{`set_update_frequency()`}{
#'       Set expected update frequency.
#'     }
#'
#' 
#'   }
#' 
#' @format NULL
#' @usage NULL
#'
#' @examples
#' \dontrun{
#'  set_rhdx_config(hdx_site = "prod")
#'  acled_mali_rs <- read_dataset("acled-data-for-mali")
#'  
#' }
#' 
#' @export
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
      initial_data <- drop_nulls(initial_data)
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
    
    get_resource = function(index) {
      n_res <- self$data$num_resources
      if (index > n_res)
        stop("You have ", n_res, " resources in this dataset", call. = FALSE)
      self$resources[[index]]
    },
    
    get_all_resources = function() {
      self$resources
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

    update_resource = function(resource, ignore_dataset_id = FALSE) {
      if (!inherits(resource, "Resource"))
        stop("Not of class `Resource` please use `Resource$new()` to create a resource first!", call. = FALSE)
      if ("package_id" %in% names(resource$data))
        warning("Resource already have a dataset id", call. = FALSE)
      self$data$resources <- resource$data
      self$resources <- list(Resource$new(resource$data))
      self$data$num_resources <- 1L
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
    
    delete_resources = function() {
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
      list_of_ds <- unlist(lapply(cc$parse(),
                                  function(x) jsonlite::fromJSON(x, simplifyVector = FALSE)$result$results), recursive = FALSE)
      list_of_ds <- lapply(list_of_ds,
                           function(x) Dataset$new(initial_data = x, configuration = configuration))
      class(list_of_ds) <- "datasets_list"
      list_of_ds
    },
    
    delete_from_hdx = function(purge = FALSE) {
      configuration <- private$configuration
      if (isTRUE(purge))
        res <- configuration$call_remoteclient("hdx_dataset_purge", data = list(id = self$data$id), verb = "post")
      res <- configuration$call_remoteclient("package_delete", data = list(id = self$data$id), verb = "post")
      res$result$status_code == 200L
    },
    
    list_datasets = function(limit = NULL, offset = NULL, configuration = NULL) {
      if (is.null(configuration) | !inherits(configuration, "Configuration"))
        configuration <- private$configuration
      data <- drop_nulls(list(offset = offset, limit = limit))
      res <- configuration$call_remoteclient("package_list", data)
      unlist(res$result)
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
                         yml = yaml::read_yaml(hdx_dataset_static_file),
                         json = jsonlite::read_json(hdx_dataset_static_file, simplifyVector = FALSE))
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
      self$data$tags <- lapply(tags, function(tag) list(name = tag))
    },
    
    get_locations = function() {
      self$data$groups
    },
    
    add_locations = function(locations) {
      self$data$groups <- lapply(locations,
                                 function(location) {
                                   assert_location(location)
                                   list(name = location)
                                 })
    },
    
    get_maintainer = function() {
      self$data$maintainer
    },
    
    set_maintainer = function(maintainer) {
      self$data$maintainer <- maintainer
    },
    
    get_organization = function() {
      self$data$organization
    },
    
    set_organization = function(organization) {
      self$data$organization <- organization
    },
    
    is_requestable = function() {
      self$data$is_requestdata_type
    },
    
    set_requestable = function(requestable = TRUE) {
      self$data$is_requestable_type <- requestable
      if (requestable) {
        self$data$private <- FALSE
        self$delete_all_resources()
      }
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
    
    update_in_hdx = function(field = NULL, update_resources = FALSE) {
      configuration <- private$configuration
      dataset_id <- self$data$id
      if (is.null(dataset_id))
        warning("Dataset not on HDX use `create_in_hdx` to create a Dataset", call. = FALSE)
      rs <- self$get_all_resources()
      ds <- drop_nulls(self$data)
      res1 <- configuration$call_remoteclient("package_update",
                                              ds,
                                              verb = "post",
                                              encode = "json")
      if (res1$status_code != 200L) {
          warning("Dataset not created check the parameters", call. = FALSE)
      }
      if (update_resources) {
        res2 <- lapply(rs, function(r) r$update_in_hdx(dataset_id))
      } else {
        res2 <- NULL
      }
      invisible(drop_nulls(list(dataset = res1, resources = res2)))
    },
    
    create_in_hdx = function(upload_resources = TRUE, verbose = verbose) {
      invisible(self$check_required_fields())
      configuration <- private$configuration
      rs <- self$get_resources()
      ds <- self$data
      ds$resources <- NULL
      ds$num_resources <- NULL
      if (!is.null(self$data$id))
        stop("Dataset already exists on HDX use `update_in_hdx`")
      ds_req <- configuration$call_remoteclient(action = "package_create",
                                                data = ds,
                                                verb = "post",
                                                verbose = verbose)
      if (ds_req$status_code == 200L) {
        message(paste0("Dataset created with id: ", ds_req$result$id))
        self$data <- ds_req$result
        res <- invisible(list(dataset = ds_req))
      } else {
        ## replace message by logger
        warning("Dataset not created, check the parameters!", call. = FALSE)
        message(paste0(ds_req$error[[1]], ": ", ds_req$error[[2]]))
      }
      if (isTRUE(upload_resources) && length(rs) > 0) {
        ## use logger
        rs_req <- lapply(rs, function(r) r$create_in_hdx(dataset_id = self$data$id, verbose = verbose))
        res <- invisible(list(dataset = ds_req, resources = rs_req))
      }
      res
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
Dataset$read_from_hdx <- memoise::memoise(function(identifier, configuration = NULL, ...) {
  ds <- Dataset$new()
  ds$read_from_hdx(identifier, configuration = configuration, ...)
})

#' @aliases Dataset 
Dataset$search_in_hdx <- memoise::memoise(function(query = "*:*", rows = 10L, page_size = 1000L, configuration = NULL, ...) {
  ds <- Dataset$new()
  ds$search_in_hdx(query = query, rows = rows,
                   page_size = page_size, configuration = configuration, ...)
})


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
  tibble::data_frame(dataset_title = tolower(x$data$title),
                     dataset_name = x$data$name,
                     dataset_date = x$get_dataset_date(),
                     requestable = x$is_requestable(),
                     locations_name = list(get_locations_name(x)),
                     organization_name = get_organization_name(x),
                     resources_format = list(get_formats(x)),
                     tags_name = list(get_tags_name(x)),
                     resources = list(x$get_resources()),
                     dataset = list(x))
}

#' @export
#' @aliases Dataset 
get_resource <- function(dataset, index) {
  assert_dataset(dataset)
  dataset$get_resource(index)
}

#' @export
#' @aliases Dataset 
get_resources <- function(dataset) {
  assert_dataset(dataset)
  dataset$get_resources()
}

#' Add resource to dataset
#'
#' Add resource to dataset
#'
#' @param dataset
#' 
#'
#' @details Delete resource from dataset
#'
#' 
#' @return A Dataset
#' @export
#'
#' @examples
#' \dontrun{
#'  # Setting the config to use HDX default server
#'  delete_resource(dataset, 1) # first resource
#' }
#'
add_resource <- function(dataset, resource, ignore_dataset_id = FALSE) {
  assert_dataset(dataset)
  dataset$add_resource(resource, ignore_dataset_id = ignore_dataset_id)
  dataset
}


#' Delete resource from dataset
#'
#' Delete resource from dataset
#'
#' @param dataset
#' 
#'
#' @details Delete resource from dataset
#'
#' 
#' @return A Dataset
#' @export
#'
#' @examples
#' \dontrun{
#'  # Setting the config to use HDX default server
#'  delete_resource(dataset, 1) # remove first resource
#' }
#'
delete_resource <- function(dataset, index) {
  assert_dataset(dataset)
  dataset$delete_resource(index)
  dataset
}


#' Delete all resource from dataset
#'
#' Delete all resource from dataset
#'
#' @param dataset
#' 
#'
#' @details Delete all resources from dataset
#'
#' 
#' @return Dataset without resources
#' @export
#'
#' @examples
#' \dontrun{
#'  # Setting the config to use HDX default server
#'  delete_all_resources(dataset) # remove first resource
#' }
#' 
delete_resources <- function(dataset) {
  assert_dataset(dataset)
  invisible(lapply(seq(dataset$data$num_resources),
                   function(index) delete_resource(dataset, index)))
}


#' @export
#' @aliases Dataset 
count_datasets <- function(configuration = NULL) {
  ds <- Dataset$new()
  ds <- ds$count(configuration = configuration)
}

#' @noRd
.search_datasets <- function(query = "*:*", rows = 10L, page_size = 1000L, configuration = NULL, ...) {
  ds <- Dataset$new()
  ds$search_in_hdx(query = query, rows = rows,
                   page_size = page_size,
                   configuration = configuration,
                   ...)
}

#' Search datasets on HDX
#'
#' Sets the configuration settings for using rhdx.
#'
#' @param query Character Query (in Solr format). Defaults to ‘*:*’
#' @param rows Number of matching rows to return. Defaults to 10.
#' @param page_size Integer Size of page to return. Defaults to 1000.
#' @param configuration Configuration object.
#' 
#'
#' @rdname search_datasets
#'
#' @details Search and find datasets on HDX
#'
#' 
#' @return A list of HDX datasets
#' @export
#'
#' @examples
#' \dontrun{
#'  # Setting the config to use HDX default server
#'  search_datasets("displaced nigeria", rows = 3L)
#' }
#'
search_datasets <- memoise::memoise(.search_datasets)


#' @aliases Dataset
#' @noRd
.read_dataset <- function(identifier, configuration = NULL, ...) {
  ds <- Dataset$new()
  ds$read_from_hdx(identifier, configuration = configuration, ...)
}


#' Read dataset
#'
#' Read dataset 
#'
#' @param identifier character dataset keyword
#' @param configuration an HDX configuration object 
#'
#' 
#' @return Dataset the dataset
#' @export
#'
#' @examples
#' \dontrun{
#' # Setting the config to use HDX default server
#'  set_rhdx_config()
#'  res <- read_dataset("mali-3wop")
#'  res
#' } 
read_dataset <- memoise::memoise(.read_dataset)


#' Read dataset
#'
#' Read dataset 
#'
#' @param identifier character dataset keyword
#' @param configuration an HDX configuration object 
#'
#' 
#' @return Dataset the dataset
#' @export
#'
#' @examples
#' \dontrun{
#' # Setting the config to use HDX default server
#'  set_rhdx_config()
#'  res <- read_dataset("mali-3wop")
#'  res
#' } 
list_datasets <- function(limit = NULL, offset = NULL, configuration = NULL) {
  ds <- Dataset$new()
  ds$list_datasets(limit = limit, offset = offset, configuration = configuration)
}


#' Delete dataset in HDX
#' 
#' Dataset delete
#'
#' @param dataset Dataset 
#' 
#' 
#' @return "None"
#' @export
#'
#' @examples
#' \dontrun{
#'  #Setting the config to use HDX default server
#'  set_rhdx_config(read_only = FALSE, hdx_key = "xxxx")
#'  res <- search_dataset(rows = 3L)
#'  delete_dataset(res[[1]])
#' }
#' 
delete_dataset <- function(dataset) {
  assert_dataset(dataset)
  dataset$delete_from_hdx()
}

#' @export
#' @aliases Dataset 
browse.Dataset <- function(x, ...)
  x$browse()


#' Filter a list of HDX datasets
#'
#' Filter a list of HDX datasets 
#'
#' @param datasets_list A list of dataset 
#' @param format character Format of a resource in the dataset 
#' @param locations character Locations of the dataset
#' @param tags character Dataset with specified tags
#' @param hxl logical dataset with HXL tags
#' @param cod logical dataset with COD tags
#' @param quick_charts logical dataset with Quich Charts
#' @param requestable logical requestable dataset
#' 
#' 
#' @return Dataset the dataset
#' @export
#'
#' @examples
#' \dontrun{
#' # Setting the config to use HDX default server
#'  set_rhdx_config()
#'  res <- read_dataset("mali-3wop")
#'  res
#' }
#' 
refine_search <- function(datasets_list, format = NULL, locations = NULL, hxl = FALSE, tags = NULL, organization = NULL, cod = FALSE, requestable = FALSE) {
  
  assert_datasets_list(datasets_list)

  lgl <- !logical(length = length(datasets_list))

  if (!is.null(format)) {
    lgl_format <- vapply(datasets_list, function(dataset) format %in% get_formats(dataset), logical(1))
    lgl <- lgl & lgl_format
  }

  if (!is.null(organization)) {
    lgl_org <- vapply(datasets_list, function(dataset) organization %in% get_organization_name(dataset), logical(1))
    lgl <- lgl & lgl_org
  }
  
  if (isTRUE(requestable)) {
    lgl_req <- vapply(datasets_list, function(dataset) dataset$is_requestable(), logical(1))
    if (isFALSE(requestable))
      lgl_req <- !lgl_req
    lgl <- lgl & lgl_req
  }

  if (isTRUE(hxl)) {
    lgl_hxl <- vapply(datasets_list, function(dataset) "hxl" %in% get_tags_name(dataset), logical(1))
    if (isFALSE(hxl))
      lgl_hxl <- !lgl_hxl
    lgl <- lgl & lgl_hxl
  }
  
  if (isTRUE(cod)) {
    lgl_cod <- vapply(datasets_list, function(dataset) "cod" %in% get_tags_name(dataset), logical(1))
    if (isFALSE(cod))
      lgl_cod <- !lgl_cod
    lgl <- lgl & lgl_cod
  }
  
  if (!is.null(tags)) {
    lgl_tags <- lapply(datasets_list, function(dataset) vapply(tags, function(tag) tag %in% get_tags_name(dataset), logical(1)))
    lgl_tags <- vapply(lgl_tags, function(x) Reduce(`|`, x), logical(1))
    lgl <- lgl & lgl_tags
  }

  if (!is.null(locations)) {
    lgl_locs <- lapply(datasets_list, function(dataset) vapply(locations, function(loc) loc %in% get_locations_name(dataset), logical(1)))
    lgl_locs <- vapply(lgl_locs, function(x) Reduce(`|`, x), logical(1))
    lgl <- lgl & lgl_locs
  }
 
  datasets_list[lgl]
}


#' Dataset locations
#'
#' Gets locations from the datasets
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
#'  get_locations_name(res[[1]])
#' }
#' 
get_locations_name <- function(dataset) {
  assert_dataset(dataset)
  vapply(dataset$get_locations(), function(location) location$name, character(1))
}



#' Dataset tags name
#'
#' Gets dataset tags name
#'
#' @param dataset Dataset 
#' 
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
#'  get_tags_name(res[[1]])
#' }
#' 
get_tags_name <- function(dataset) {
  assert_dataset(dataset)
  vapply(dataset$get_tags(), function(tag) tag$name, character(1))
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
#' 
#'
get_organization_name <- function(dataset) {
  assert_dataset(dataset)
  dataset$get_organization()[["name"]]
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
#'  get_formats(res[[1]])
#' }
#' 
get_formats <- function(dataset) {
  assert_dataset(dataset)
  vapply(dataset$get_resources(), function(resource) resource$get_format(), character(1))
}


#' @export
#' @rdname dataset_date
get_dataset_date <- function(dataset) {
  assert_dataset(dataset)
  dataset$get_dataset_date()
}


#' Dataset date utilities
#'
#' Sets and gets date Dataset
#'
#' @param dataset Dataset 
#' @param date Date the date to add the metadata
#' 
#'
#' @rdname dataset_date
#'
#' @details Allow to add/modify the dataset dates
#'
#' 
#' @return Dataset dates or dataset date ranges
#' @export
#'
#' @examples
#' \dontrun{
#' # Setting the config to use HDX default server
#'  set_rhdx_config()
#'  res <- search_dataset(rows = 3L)
#'  set_dataset_date(res[[1]])
#' }
#' 
set_dataset_date <- function(dataset, date) {
    assert_dataset(dataset)
    stopifnot(is.Date(date))
    dataset$set_dataset_date(date, format = "%m/%d/%Y")
}
