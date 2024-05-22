#' @title Predict Method for Flexible Transfer Models (ftmglm and ftmlm)
#'
#' @description Generates predictions from an \code{ftmglm} or \code{ftmlm} object based on new input data.
#' This method utilizes the intersecting variables between the model's predictors and the new dataset.
#'
#' @param object Object of class inheriting from \code{ftmglm} or \code{ftmlm}.
#' @param newdata Data frame of new data for prediction. Must contain at least some of
#'   the predictors used in the model training.
#' @param type A character string specifying the type of prediction: either \code{"response"}
#'   (default) for predicted probabilities, or \code{"link"} for linear predictors.
#' @param s Ridge penalty to apply during flexible reweighting. Default is the stored optimal penalty, or 0 if missing.
#'
#' @details
#' The predict method works by:
#' \itemize{
#'   \item Identifying variables common to both the model and the new data.
#'   \item Preparing the data by subsetting to these intersecting variables and including an intercept.
#'   \item Inverting the XtWX or XtX matrix, including a ridge penalty or using truncated SVD to handle
#'   potentially singular matrices.
#'   \item Estimating coefficients and calculating predictions based on the specified type.
#' }
#'
#' It is crucial that \code{newdata} contains variables that intersect with the model's predictors.
#' If no intersecting variables are found, the function throws an error.
#'
#' @return
#' A numeric vector of predictions. If \code{type} is \code{"link"}, these are the linear predictors;
#' if \code{"response"}, these are the probabilities, transformed via the logistic function.
#'
#' @examples
#' \dontrun{
#' # Load mtcars dataset
#' data(mtcars)
#'
#' # Fit a glmnet model
#' fit <- glmnet::cv.glmnet(as.matrix(mtcars[, c("hp", "wt", "cyl")]), mtcars$am, family = "binomial")
#'
#' # Create an ftmglm object
#' ftmglm_model <- createFromGlmnet(fit, as.matrix(mtcars[, c("hp", "wt", "cyl")]))
#'
#' # Predict on "new" data
#' new_data <- mtcars[1:10, c("hp", "wt", "cyl")]
#' predictions <- predict(ftmglm_model, newdata = new_data)
#' print(predictions)
#'
#' # Fit a linear model
#' lm_model <- lm(mpg ~ cyl + hp + wt, data = mtcars)
#'
#' # Create an ftmlm object
#' ftmlm_model <- createFromLm(lm_model)
#'
#' # Predict on "new" data
#' new_data <- mtcars[1:10, c("hp", "wt", "cyl")]
#' predictions <- predict(ftmlm_model, newdata = new_data)
#' print(predictions)
#' }
#'
#' @export
#' @include ftmglm.R ftmlm.R
setMethod("predict", "ftmglm",
    function(object, newdata, type = c("response", "link")) {

        # Validate type
        type <- match.arg(type)

        # Validation for object
        if (!inherits(object, "ftmglm")) {
            stop("object must be of class ftmglm")
        }

        # Validation for newdata
        if (missing(newdata)) {
            stop("newdata must be provided")
        }

        # Validation for newdata
        if (!is.data.frame(newdata)) {
            stop("newdata must be a data frame")
        }

        # Get the intersection of variables between newdata and model
        intersecting_vars <- intersect(colnames(newdata),
                                       colnames(object@XtWX))

        # If there are no overlapping variables, throw an error
        if (length(intersecting_vars) == 0) {
            stop("No overlapping variables between new data and model.")
        }

        ## Subset newdata to the intersecting variables
        newdata <- cbind("intercept" = 1,
                         newdata[, intersecting_vars, drop = FALSE])

        ## Subset object to the intersecting variables
        XtWX <- object@XtWX[c("intercept", intersecting_vars),
                          c("intercept", intersecting_vars), drop = FALSE]
        XtWz <- object@XtWz[c("intercept", intersecting_vars), , drop = FALSE]

        # Compute the design matrix
        X <- as.matrix(newdata)

        # Perform SVD on the XtWX matrix
        XtWX_SVD <- svd(XtWX)

        # Get the inverse of the eigen values that are greater than 1e-16
        eigenvalues <- 1 / XtWX_SVD$d[XtWX_SVD$d > 1e-16]

        ## Calculate the truncated SVD inverse
        XtWX_inv <- XtWX_SVD$u[, seq(length(eigenvalues))] %*% diag(eigenvalues) %*% t(XtWX_SVD$v[, seq(length(eigenvalues))])

        # Estimate the beta coefficients
        beta <- XtWX_inv %*% XtWz

        # Compute the predicted values
        linear_predictors <- X %*% beta

        # Return either the link function or the response
        if (type == "link") {

            ## Return the linear predictors
            return(linear_predictors)

        } else {

            ## Calculate the predicted probabilities
            pp <- logistic_function(linear_predictors)

            return(pp)
        }
    }
)
