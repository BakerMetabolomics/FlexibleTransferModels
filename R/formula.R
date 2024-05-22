#' @title Model Formula for ftmglm or ftmlm objects
#'
#' @description Retrieves the formula used to create a \code{ftmglm} or \code{ftmlm} model
#'
#' @param x Object of class inheriting from \code{ftmglm} or \code{ftmlm}.
#'
#' @note If the \code{ftmglm} or \code{ftmlm} object was created from a \code{cv.glmnet} object,
#' the outcome variable will be named "y", rather than the original name.
#'
#' @return an object of class 'formula'
#' @export
#' @include ftmglm.R ftmlm.R
setMethod("formula", "ftmglm", function(x) {

    # Get the names of the predictors
    predictors <- lapply(row.names(x@XtWz), as.name)

    # Get the name of the outcome
    outcome <- as.name(colnames(x@XtWz))

    # Create the formula
    ff <- call("~", outcome, Reduce(function(x, y) call("+", x, y), predictors))

    # Return the formula
    return(ff)
})


#' @title Model Formula for ftmglm or ftmlm objects
#' @rdname formula-ftmglm-method
#' @export
setMethod("formula", "ftmlm", function(x) {

    # Get the names of the predictors
    predictors <- lapply(row.names(x@Xty), as.name)

    # Get the name of the outcome
    outcome <- as.name(colnames(x@Xty))

    # Create the formula
    ff <- call("~", outcome, Reduce(function(x, y) call("+", x, y), predictors))

    # Return the formula
    return(ff)
})
