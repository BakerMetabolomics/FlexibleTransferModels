#' @title Extract/Subset the variables used in a ftmglm or ftmlm object
#'
#' @description Allows the subsetting of \code{ftmglm} or \code{ftmlm} objects in order to restrict the
#' model to use only the specified variables.
#'
#' @param x Object of class inheriting from \code{ftmglm} or \code{ftmlm}.
#' @param subset A character string of variables or indices to use.
#'
#' @return A \code{ftmglm} or \code{ftmlm} object that only uses the specified variables.
#' @export
#' @method subset ftmglm
#' @include ftmglm.R ftmlm.R
setMethod("subset", "ftmglm", function(x, subset) {

    # If subset are numeric, convert to character
    if (is.numeric(subset)) {
        subset <- colnames(x@XtWX)[subset]
    }

    # Ensure "intercept" is included
    if (!"(Intercept)" %in% subset) {
        subset <- c("(Intercept)", subset)
    }

    # Get the intersection of the variables in the model and the subset
    subset <- intersect(colnames(x@XtWX), subset)

    # Ensure we have more than one variable
    if (length(subset) < 2) {
        stop("Ensure that at least two variables (including the intercept) are retained in the model.")
    }

    # Create a new ftmglm object
    new_ftmglm <- new("ftmglm",
                      XtWX = x@XtWX[subset, subset, drop = FALSE],
                      XtWz = x@XtWz[subset, , drop = FALSE])

    # Return the new object
    return(new_ftmglm)
})


#' @title Extract/Subset the variables used in a ftmglm or ftmlm object
#' @rdname subset-ftmglm-method
#' @export
#' @method subset ftmlm
setMethod("subset", "ftmlm", function(x, subset) {

    # If subset are numeric, convert to character
    if (is.numeric(subset)) {
        subset <- colnames(x@XtX)[subset]
    }

    # Ensure "intercept" is included
    if (!"(Intercept)" %in% subset) {
        subset <- c("(Intercept)", subset)
    }

    # Get the intersection of the variables in the model and the subset
    subset <- intersect(colnames(x@XtX), subset)

    # Ensure we have more than one variable
    if (length(subset) < 2) {
        stop("Ensure that at least two variables (including the intercept) are retained in the model.")
    }

    # Create a new ftmlm object
    new_ftmlm <- new("ftmlm", 
                     XtX = x@XtX[subset, subset, drop = FALSE],
                     Xty = x@Xty[subset, , drop = FALSE],
                     s = x@s,
                     n = x@n,
                     yty = x@yty,
                     y_mean = x@y_mean)

    # Return the new object
    return(new_ftmlm)
})

