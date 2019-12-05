#' HDX Tag
#'
#' HDX tag
#'
#' @format NULL
#' @usage NULL
Tag <- R6::R6Class(
  classname = "Tag",

  private = list(
    configuration = NULL
  ),

  public = list(
    #' @field data list of tag field element
    data = NULL,

    #' @description
    #' Create a new Tag object
    #'
    #' @param initial_data list data with required fields to create a tag object
    #' @param configuration Configuration configuration to use
    #' @return a Tag object
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
    #' Tag object to list
    #'
    #' @return a list with of tag fields element
    as_list = function() {
      self$data
    },

    #' @description
    #' Print a Tag object
    #'
    #' @return
    print = function() {
      cat(paste0("<HDX Tag> ", self$data$id), "\n")
      cat("  Name: ", self$data$name, "\n", sep = "")
      cat("  Display Name: ", self$data$display_name, "\n", sep = "")
      cat("  Vocabulary id: ", self$data$vocabulary_id, "\n", sep = "")
      invisible(self)
    }
  )
)

#' @export
#' @aliases Tag
#' @importFrom tibble as_tibble
as_tibble.Tag <- function(x, ...) {
  df <- tibble::tibble(
    tag_id = x$data$id,
    tag_name = x$data$name)
  df$tag <- list(x)
  df
}

#' @export
#' @aliases Tag
as.list.Tag <- function(x, ...) {
  x$as_list()
}

#' @noRd
#' @rdname pull_tag
.pull_tag  <-  function(identifier = NULL, vocabulary_id = NULL, include_datasets = FALSE, configuration = NULL, ...) {
  if (is.null(configuration) | !inherits(configuration, "Configuration"))
    set_rhdx_config(configuration = configuration)
  configuration <- get_rhdx_config()
  res <- configuration$call_action("tag_show", list(id = identifier, vocabulary_id = vocabulary_id, include_datasets = include_datasets, ...))
  Tag$new(initial_data = res, configuration = configuration)
}

#' Read an HDX tag
#'
#'
#' Read an HDX tag from its name or id
#'
#' @param identifier character the name or id of the tag
#' @param vocabulary_id character the id or name of the tag vocabulary that the tag is in - if it is not specified it will assume it is a free tag.
#' @param configuration a Configuration object
#' @param include_datasets logical, include a list of the tag’s datasets.
#' @param ... Extra parameters
#'
#' @rdname pull_tag
#' @return Tag the tag
#'
#' @examples
#' \dontrun{
#' # Setting the config to use HDX default server
#'  set_rhdx_config()
#'  res <- pull_tag("xxxx")
#'  res
#' }
pull_tag <- memoise::memoise(.pull_tag)

#' @noRd
#' @rdname list_tag
.list_tags  <-  function(query = NULL, vocabulary_id = NULL, all_fields = FALSE, configuration = NULL, ...) {
  if (!is.null(configuration) & inherits(configuration, "Configuration"))
    set_rhdx_config(configuration = configuration)
  configuration <- get_rhdx_config()
  res <- configuration$call_action("tag_list", list(query = query, vocabulary_id = vocabulary_id, all_fields = all_fields, configuration = configuration, ...))
  res
}

#' List all tags
#' @param query a tag name query to search for, if given only tags whose names contain this string will be returned
#' @param vocabulary_id the id or name of a vocabulary, if give only tags that belong to this vocabulary will be returned
#' @param all_fields logical return full Tag object instead of just names
#' @param configuration Configuration the configuration to use
#' @param ... Extra parameters
#'
#' @rdname list_tags
#' @export
#'
#' @examples
#' \dontrun{
#' # Setting the config to use HDX default server
#'  set_rhdx_config()
#'  list_tag()
#' }
list_tags <- memoise::memoise(.list_tags)
