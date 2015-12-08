#' Generating Transforms for Various Data Types
#'
#' A suite of transforms for \code{numeric}, \code{character}, \code{factor}, and \code{date} data types.
#'
#' @param formula a \code{formula} to determine which variables to transform
#' @param data a \code{data.frame} to use for data transforms
#' @param numeric transform \code{numeric} data?
#' @param character transform \code{character} data?
#' @param factor transform \code{factor} data?
#' @param date transform \code{date} data?
#' @param ... Other arguments passed to \code{\link[base]{summary}}
#'
#' @return Something
#'
#' @author Derek McCrae Norton <dnorton1@@gmail.com>
#'
#' @export

generateTransforms <- function(formula, data, transform_numeric = TRUE, transform_character = TRUE, transform_factor = TRUE, transform_date = TRUE, ...) {
  type_list <- list(numeric = c("numeric", "integer"),
                    character = "character",
                    factor = "factor",
                    date = c("Date"))
  var_types <- sapply(data, class)
  number_type <- lapply(type_list, function(x) sum(var_types %in% x))
  if (transform_numeric & number_type$numeric > 0) {
    numeric_vars <- sapply(data, is.numeric)
  }
  if (transform_character & number_type$character > 0) {
    character_vars <- sapply(data, is.character)
  }
  if (transform_factor & number_type$factor > 0) {
    factor_vars <- sapply(data, is.factor)
  }
  if (transform_date & number_type$date > 0) {
    date_vars <- sapply(data, function(x) inherits(x, "Date"))
    #NOTE: Use lubridate here?
    # lubridate: sapply(data, lubridate::is.Date)
  }



}
