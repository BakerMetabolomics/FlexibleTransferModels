#' @title Extract/Subset the variables used in a ftmglm or ftmlm object
#'
#' @description Allows the subsetting of \code{ftmglm} or \code{ftmlm} objects in order to restrict the
#' model to use only the specified variables.
#'
#' @param x Object of class inheriting from \code{ftmglm} or \code{ftmlm}.
#' @param i Ignored.
#' @param j A character string of variables or indices to use.
#' @param subset A character string of variables or indices to use.
#'
#' @note \code{i} is not used and will be ignored.
#'
#' @return A \code{ftmglm} or \code{ftmlm} object that only uses the specified variables.
#' @export
#' @include ftmglm.R ftmlm.R
setMethod("subset", "ftmglm", function(x, subset) {

    # If subset are numeric, convert to character
    if (is.numeric(subset)) {
        subset <- colnames(x@XtWX)[subset]
    }

    # Get the intersection of the variables in the model and the subset
    subset <- intersect(colnames(x@XtWX), subset)

    # Ensure "intercept" is included
    if (!"(Intercept)" %in% subset) {
        subset <- c("(Intercept)", subset)
    }

    # Ensure we have more than one variable
    if (length(subset) < 2) {
        stop("Ensure that at least two variables (including the intercept) are retained in the model.")
    }

    # Create a new ftmglm object
    new_ftmglm <- new("ftmglm", XtWX = x@XtWX[subset, subset, drop = FALSE], XtWz = x@XtWz[subset, , drop = FALSE])

    # Return the new object
    return(new_ftmglm)
})


#' @title Extract/Subset the variables used in a ftmglm or ftmlm object
#' @rdname subset-ftmglm-method
#' @export
setMethod("subset", "ftmlm", function(x, subset) {

    # If subset are numeric, convert to character
    if (is.numeric(subset)) {
        subset <- colnames(x@XtX)[subset]
    }

    # Get the intersection of the variables in the model and the subset
    subset <- intersect(colnames(x@XtX), subset)

    # Ensure "intercept" is included
    if (!"(Intercept)" %in% subset) {
        subset <- c("(Intercept)", subset)
    }

    # Ensure we have more than one variable
    if (length(subset) < 2) {
        stop("Ensure that at least two variables (including the intercept) are retained in the model.")
    }

    # Create a new ftmlm object
    new_ftmlm <- new("ftmlm", XtX = x@XtX[subset, subset, drop = FALSE], Xty = x@Xty[subset, , drop = FALSE])

    # Return the new object
    return(new_ftmlm)
})


#' @title Extract/Subset the variables used in a ftmglm or ftmlm object
#' @rdname subset-ftmglm-method
#' @export
setMethod("[", "ftmglm", function(x, i, j) {

    # If subset are numeric, convert to character
    if (is.numeric(j)) {
        j <- colnames(x@XtWX)[j]
    }

    # Get the intersection of the variables in the model and the subset
    j <- intersect(colnames(x@XtWX), j)

    # Ensure "intercept" is included
    if (!"(Intercept)" %in% j) {
        j <- c("(Intercept)", j)
    }

    # Ensure we have more than one variable
    if (length(j) < 2) {
        stop("Ensure that at least two variables (including the intercept) are retained in the model.")
    }

    # Create a new ftmglm object
    new_ftmglm <- new("ftmglm", XtWX = x@XtWX[j, j, drop = FALSE], XtWz = x@XtWz[j, , drop = FALSE])

    # Return the new object
    return(new_ftmglm)
})


#' @title Extract/Subset the variables used in a ftmglm or ftmlm object
#' @rdname subset-ftmglm-method
#' @export
setMethod("[", "ftmlm", function(x, i, j) {

    # If subset are numeric, convert to character
    if (is.numeric(j)) {
        j <- colnames(x@XtX)[j]
    }

    # Get the intersection of the variables in the model and the subset
    j <- intersect(colnames(x@XtX), j)

    # Ensure "intercept" is included
    if (!"(Intercept)" %in% j) {
        j <- c("(Intercept)", j)
    }

    # Ensure we have more than one variable
    if (length(j) < 2) {
        stop("Ensure that at least two variables (including the intercept) are retained in the model.")
    }

    # Create a new ftmlm object
    new_ftmlm <- new("ftmlm", XtX = x@XtX[j, j, drop = FALSE], Xty = x@Xty[j, , drop = FALSE])

    # Return the new object
    return(new_ftmlm)
})

