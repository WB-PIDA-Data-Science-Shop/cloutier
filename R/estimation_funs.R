#' A function to create indices by the standard transformation, aggregation and transformation
#' process
#'
#' This function applies some transformation function either user defined or standard R function
#' to a set a variables (could be the same function or a list of functions equal to the length of
#' variables) and then applies an aggregation function of choice to the set of transformed
#' indicators and then finally applies a transformation to the result of the aggregation
#'
#' @param vars a character or list, the variable name(s) of the numeric variables
#' @param dt `data.frame`/`data.table`, contains the set of variables
#' @param std_funs the transformation function to be applied to each variable.
#' length(std_fun) should be 1 if the same function is to be applied to all `vars`, otherwise,
#' length(std_fun) must be equal to length(vars)
#' @param agg_fun the aggregation function to combine transformed `vars`
#' @param index_fun the function applied to the combined transformed variables typically used
#' to convert the result to the original scale of `std_funs`.
#'
#' @import data.table
#'
#'
#'


compute_transdices <- function(vars,
                               dt,
                               std_funs,
                               agg_fun,
                               index_fun){


  dt <- data.table::as.data.table(dt[, vars])

  ### ensure std_fun is the same length
  if (length(vars) != length(std_funs) & length(std_funs) > 1){

    stop("If length(std_funs) > 1, then Length of vars must match length of std_funs")

  }

  if (length(std_funs) == 1){

    dt[, (vars) := lapply(.SD, FUN = std_funs), .SDcols = vars]

  } else {

    for (i in seq_along(vars)) {

      dt[, (vars[i]) := std_funs[[i]](get(vars[i]))]  # Apply each function to its variable

    }

  }


  dt[, index := apply(.SD, 1, agg_fun), .SDcols = vars]

  dt[, index := index_fun(index)]

  return(dt$index)

}


# Function to preserve labels while ensuring numeric output
preserve_labels_as_numeric <- function(x) {

  lbls <- attr(x, "label")  # Extract variable label

  x <- as.numeric(x)  # Convert to numeric

  attr(x, "label") <- lbls  # Reapply label

  return(x)

}











