#' Create and manipulate HDX Showcase
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
#' @export
Showcase <- R6::R6Class(
  "Showcase",

  private = list(
    configuration = NULL
  ),
  
  public = list(
    datasets = NULL,
    data = NULL,
    initialize = function(initial_data = NULL, configuration = NULL) {
      if (is.null(configuration) | !inherits(configuration, "Configuration")) {
        private$configuration <- Configuration$read()
      } else {
        private$configuration <- configuration
      }
      if (is.null(initial_data)) initial_data <- list()
      initial_data <- drop_nulls(initial_data)
      self$data <- initial_data
      key <- names(initial_data)
      if ("dataset" %in% key)
        self$datasets <- lapply(self$data$datasets,
                                function(x) Dataset$new(initial_data = x, configuration = configuration))
      
    },
    
    read_from_hdx = function(identifier = NULL, configuration = NULL) {
      if (is.null(configuration) | !inherits(configuration, "Configuration"))
        configuration <- private$configuration
      res <- configuration$call_remoteclient("ckanext_showcase_show", list(id = identifier))
      Showcase$new(initial_data = res$result, configuration = configuration)
    },
    
    add_dataset = function(dataset) {
      if (!inherits(dataset, "Dataset"))
        stop("Not of class Dataset, use `Dataset$new`!")
      if (is.null(self$data$id))
        stop("Not of class Dataset, use `Dataset$new`!")
      configuration <- private$configuration
      dataset_id <- dataset$data$id
      showcase_id <- self$data$id
      res <- configuration$call_remoteclient("ckanext_showcase_package_association_create", list(showcase_id = showcase_id, package_id = dataset_id))
      if (res$status_code != 200L)
        stop("Dataset not added to the showcase")
      res$result
    },
    
    delete_dataset = function(dataset) {
      if (!inherits(dataset, "Dataset"))
        stop("Not of class Dataset, use `Dataset$new`!")
      if (is.null(self$data$id))
        stop("Showcase not on HDX uses Showcase$create_in_hdx first")
      configuration <- private$configuration
      dataset_id <- dataset$data$id
      showcase_id <- self$data$id
      res <- configuration$call_remoteclient("ckanext_showcase_package_association_delete", list(showcase_id = showcase_id, package_id = dataset_id))
      if (res$status_code != 200L)
        stop("Dataset not added to the showcase")
      res$result
    },
    
    list_datasets = function() {
      configuration <- private$configuration
      showcase_id <- self$data$id
      res <- configuration$call_remoteclient("ckanext_showcase_list", data = list(showcase_id = showcase_id))
      if (res$status_code != 200L)
        stop("Dataset not added to the showcase")
      res$result
    },
    
    add_tag = function(tag) {
      if (!inherits(tag, "Tag"))
        stop("Not of class Tag, use `Tag$new`!")
        if (is.null(self$data$id))
        stop("Not of class Tag, use `Tag$new`!")
      configuration <- private$configuration
      tag_id <- tag$data$id
      showcase_id <- self$data$id
      res <- configuration$call_remoteclient("ckanext_showcase_package_association_create",
                                             list(showcase_id = showcase_id, package_id = tag_id),
                                             verb = "post",
                                            encode = "json")
      if (res$status_code != 200L)
        stop("Tag not added to the showcase")
      res$result
    },
        
    delete_tag = function(tag) {
      if (!inherits(tag, "Tag"))
        stop("Not of class Tag, use `Tag$new`!")
      if (is.null(self$data$id))
        stop("Showcase not on HDX uses Showcase$create_in_hdx first")
      configuration <- private$configuration
      tag_id <- tag$data$id
      showcase_id <- self$data$id
      res <- configuration$call_remoteclient("ckanext_showcase_package_association_delete",
                                            list(showcase_id = showcase_id, tag_id = tag_id),
                                            verb = "post",
                                            encode = "json")
      if (res$status_code != 200L)
        stop("Tag not added to the showcase")
      res$result
    },
    
    list_tags = function() {
      configuration <- private$configuration
      showcase_id <- self$data$id
      res <- configuration$call_remoteclient("ckanext_showcase_list", list(showcase_id = showcase_id))
      if (res$status_code != 200L)
        stop("Tag not added to the showcase")
      res$result
    },
    
    create_in_hdx = function() {
      configuration <- private$configuration
      ds <- nc(self$data)
      res <- configuration$call_remoteclient("ckanext_showcase_create",
                                            ds,
                                            verb = "post",
                                            encode = "json")
      if (res1$status_code != 200L)
        stop("Showcase not created check the parameters")
      invisible(res1$result)
    },
    
    delete_from_hdx = function(name = NULL) {
      configuration <- private$configuration
      ds <- drop_nulls(self$data)
      res <- configuration$call_remoteclient("ckanext_showcase_delete",
                                            list(name = name),
                                            verb = "post",
                                            encode = "json")
      if (res$status_code != 200L)
        stop("Showcase not created check the parameters")

    },
    
    browse = function() {
      url <- private$configuration$get_hdx_site_url()
      browseURL(url = paste0(url, "showcase/", self$data$name))
    },
    
    as_list = function() {
      self$data
    },
    
    print = function() {
      cat(paste0("<HDX Showcase> ", self$data$id), "\n")
      cat("  Title: ", self$data$title, "\n", sep = "")
      cat("  Name: ", self$data$name, "\n", sep = "")
      cat("  Description: ", self$data$notes, "\n", sep = "")
      cat("  Type: ", self$data$type, "\n", sep = "")
      invisible(self)
    }
  )
)

#' @aliases Showcase
Showcase$read_from_hdx <- function(identifier = NULL, configuration = NULL) {
  org <- Showcase$new()
  org$read_from_hdx(identifier = identifier, configuration = configuration)
}

 
#' @export
#' @aliases Showcase 
#' @importFrom tibble as_tibble
as_tibble.Showcase <- function(x) {
  df <- tibble::data_frame(
    showcase_id = x$data$id,
    showcase_name = x$data$name)
  df$showcase <- list(x)
  df
}


#' @export
#' @aliases Showcase 
as.list.Showcase <- function(x) {
  x$as_list()
}

#' @aliases Showcase
.read_showcase <- function(identifier = NULL, configuration = NULL) {
  org <- Showcase$new()
  org$read_from_hdx(identifier = identifier, configuration = configuration)
}


#' Read Showcase
#'
#' Read HDX Showcase
#'
#' @param identifier Showcase name or id
#' @param configuration an HDX configuration object
#' 
#' 
#'
#' @details Delete resource from dataset
#'
#' 
#' @return A showcase
#' @export
#'
#' @examples
#' \dontrun{
#'  # Setting the config to use HDX default server
#'  delete_resource(dataset, 1) # first resource
#' }
read_showcase <- memoise::memoise(.read_showcase)
