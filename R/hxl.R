#' Strip HXL tags from tibble
#'
#' Strip HXL tags from tibble
#' @importFrom readr type_convert
#' @param x a tibble with HXL tags
#' @return tibble
#' @noRd
strip_hxl <- function(x) {
  tbl <- tibble::as_tibble(x)
  schema_row <- find_schema_row(tbl)
  base_tbl <- if (schema_row > 0) {
    new_tbl <- tbl[-1 * 1L:schema_row, ]
    suppressMessages(readr::type_convert(new_tbl))
  } else {
    tbl
  }
  base_tbl
}

#' @noRd
#' @author Dirk Schumascher
find_schema_row <- function(tbl) {
  stopifnot(is.data.frame(tbl))
  if (any(is_valid_tag(colnames(tbl)))) {
    return(0)
  } else {
    for (i in seq_len(pmin(nrow(tbl), 25))) {
      row <- unlist(apply(tbl[i, ], 2, as.character))
      if (any(is_valid_tag(row))) {
        return(i)
      }
    }
  }
  -1
}

#' @noRd
#' @author Dirk Schumascher
is_valid_tag <- function(tag) {
  ltag <- tolower(trimws(tag))
  pattern <- "^#[a-z][a-z0-9_]*(\\s+(\\+|-)\\s*[a-z][a-z0-9_]*)*"
  grepl(x = ltag, pattern = pattern)
}
