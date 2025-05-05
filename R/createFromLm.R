#' @title Create a Flexible Transfer Model (ftmlm) from a lm object
#'
#' @description This function extracts necessary components from a lm object to create an ftmlm object.
#' It extracts the necessary components and adapts them into the FTM framework.
#'
#' @param lmObj a lm object from the stats package
#' @param outcome_name Optional name of the outcome variable; if not provided, it will be extracted from the model object.
#'
#' @return A ftmlm object.
#'
#' @examples
#' \dontrun{
#' # Fitting a linear model to the mtcars dataset
#' data(mtcars)
#' lm_model <- lm(mpg ~ cyl + hp + wt, data = mtcars)
#' ftmlm_model <- createFromLm(lm_model)
#' }
#' @export
#' @importFrom stats formula model.matrix
createFromLm <- function(lmObj, outcome_name = NULL) {

    # Validation for lmObj
    if (!inherits(lmObj, "lm")) {
        stop(sprintf("lmObj must be a lm object. Provided object class: %s", class(lmObj)))
    }

    # Extract the outcome variable
    y <- lmObj$model[[all.vars(formula(lmObj))[1]]]

    # Extract the model matrix
    X <- model.matrix(formula(lmObj), lmObj$model)

    # Ensure that an intercept is included
    if (!any(colnames(X) %in% "(Intercept)")) {
        stop("An intercept term should be included in the model.")
    }

    # Calculate Xty and XtWX
    Xty <- t(X) %*% y
    XtX <- t(X) %*% X

    # Calculate the sample size
    n <- nrow(X)
    # Calculate the mean of the outcome variable
    y_mean <- mean(y, na.rm = TRUE)
    # Calculate the sum of squared responses
    yty <- as.numeric(crossprod(y))

    # Get the outcome name (if not provided)
    if (is.null(outcome_name)) {
        outcome_name <- all.vars(formula(lmObj))[1]
    }

    # Update the outcome name
    colnames(Xty) <- outcome_name

    # Create ftmlm object
    ftmlm(XtX = XtX, Xty = Xty, n = n, yty = yty, y_mean = y_mean)
}
