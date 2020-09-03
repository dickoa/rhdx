#' HDX Vocabulary
#'
#' HDX Vocabulary
#'
Vocabulary <- R6::R6Class(
  classname = "Vocabulary",
  inherit = HDXObject,

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
    #' List of accepted tags on HDX
    #'
    #' @return a vector of tags name
    authorized_tags_name = function() {
      url <- private$configuration$data$hdx_config$tags_list_url
      read.csv(url, header = FALSE, col.names = "tag_name")$tag_name
    },

    #' @description
    #' Print a Vocabulary object
    print = function() {
      cat(paste0("<HDX Vocabulary> ", self$data$id), "\n")
      cat("  Name: ", self$data$name, "\n", sep = "")
      cat("  Display Name: ", self$data$display_name, "\n", sep = "")
      invisible(self)
    }
  )
)

#' @export
#' @aliases Vocabulary
#' @importFrom tibble as_tibble
as_tibble.Vocabulary <- function(x, ...) {
  df <- tibble::tibble(
    tag_id = x$data$id,
    tag_name = x$data$name)
  df$tag <- list(x)
  df
}

#' @export
#' @aliases Vocabulary
as.list.Vocabulary <- function(x, ...) {
  x$as_list()
}

#' @noRd
#' @rdname pull_vocabulary
.pull_vocabulary  <-  function(identifier = NULL, configuration = NULL) {
  if (is.null(configuration) | !inherits(configuration, "Configuration"))
    set_rhdx_config(configuration = configuration)
  configuration <- get_rhdx_config()
  res <- configuration$call_action("vocabulary_show",
                                   list(id = identifier))
  Vocabulary$new(initial_data = res, configuration = configuration)
}

#' Read an HDX Vocabulary
#'
#'
#' Read an HDX vocabulary from its name or id
#'
#' @importFrom memoise memoise
#'
#' @param identifier character the name or id of the vocabulary
#' @param configuration Configuration
#'
#' @rdname pull_vocabulary
#' @return The Vocabulary object
#'
#' @examples
#' \dontrun{
#' # Setting the config to use HDX default server
#'  set_rhdx_config()
#'  res <- pull_vocabulary("xxxx")
#'  res
#' }
pull_vocabulary <- memoise(.pull_vocabulary)


#' @noRd
#' @rdname list_vocabularies
.list_vocabularies  <-  function(identifier = NULL, configuration = NULL) {
  if (is.null(configuration) | !inherits(configuration, "Configuration"))
    set_rhdx_config(configuration = configuration)
  configuration <- get_rhdx_config()
  res <- configuration$call_action("vocabulary_list")
  lapply(res, function(r)
    Vocabulary$new(initial_data = r, configuration = configuration))
}

#' List available HDX Vocabulary objects
#'
#' List available HDX Vocabulary objects
#'
#' @importFrom memoise memoise
#'
#' @param identifier character identifier
#' @param configuration Configuration
#'
#' @rdname list_vocabularies
#' @return A list of Vocabulary object
#'
#' @examples
#' \dontrun{
#' # Setting the config to use HDX default server
#'  set_rhdx_config()
#'  res <- list_vocabularies()
#'  res
#' }
list_vocabularies <- memoise(.list_vocabularies)


#' @rdname autorized_tags
#' @noRd
.authorized_tags <- function(configuration = NULL) {
  if (is.null(configuration) | !inherits(configuration, "Configuration"))
    set_rhdx_config(configuration = configuration)
  configuration <- get_rhdx_config()
  vc <- Vocabulary$new(configuration = configuration)
  vc$authorized_tags_name()
}

#' List all authorized tags in HDX
#'
#' List all authorized tags in HDX
#'
#' @importFrom memoise memoise
#'
#' @param configuration Configuration
#'
#' @rdname authorized_tags
#' @return A vector of character, the authorized tags name
#'
#' @examples
#' \dontrun{
#' autorized_tags()
#' }
authorized_tags <- memoise(.authorized_tags)
