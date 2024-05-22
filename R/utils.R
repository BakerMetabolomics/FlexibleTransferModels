
# Helper functions for logistic and logit transformations
logit <- function(p) {
    log(p / (1 - p))
}

logistic_function <- function(x) {
    1 / (1 + exp(-x))
}

# Helper function to validate input for models
validate_input_input <- function(XtWX, XtWz, s = NULL) {
    if (!is.matrix(XtWX) || !is.matrix(XtWz)) {
        stop("XtWX and XtWz must be matrices")
    }
    if (nrow(XtWX) != ncol(XtWX) || nrow(XtWX) != nrow(XtWz)) {
        stop("Dimensions of XtWX and XtWz are not compatible")
    }
    if (!is.null(s) && !is.numeric(s)) {
        stop("s must be a numeric value")
    }
}

# Helper function to get the outcome name from a glmnet object
get_outcome_name <- function(glmnetObj) {

    # Get the model call
    model_call <- glmnetObj$call

    # Extract the y data language object
    y_data <- model_call[["y"]]

    # Handle simple cases
    if (y_data[[1]] == "$") {
        return(deparse(y_data[[length(y_data)]]))
    }
    if (y_data[[1]] == "as.matrix") {
        return(deparse(y_data[[2]]))
    }

    # Return the whole thing
    return(deparse(y_data))
}

# Get the schur complement
schur_complement <- function(XtX) {

    # Get the location of the intercept
    inter_loc <- which(colnames(XtX) == "(Intercept)")

    # Get the submatrix
    sub1 <- XtX[-inter_loc, -inter_loc, drop = FALSE]

    ## Get the correction matrix
    sub2 <-  XtX[-inter_loc, inter_loc, drop = FALSE] %*%
        solve(XtX[inter_loc, inter_loc, drop = FALSE]) %*%
        XtX[inter_loc, -inter_loc, drop = FALSE]

    # Get the schur complement
    schur_complement <- sub1 - sub2

    # Return the schur complement
    return(schur_complement)
}
