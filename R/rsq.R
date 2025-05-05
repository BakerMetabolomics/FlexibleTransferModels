#' @title Extract R-squared from model objects
#'
#' @description Generic function to extract R-squared values from various model types
#' in the FTM package.
#'
#' @param object A model object from which to extract the R-squared value
#' @param ... Additional arguments passed to methods
#'
#' @return A numeric value representing the R-squared or pseudo-R-squared value
#'
#' @export
setGeneric("rsq",
    function(object, ...) standardGeneric("rsq")
)

#' @title Extract R-squared from a ftmglm or ftmlm object
#'
#' @description Extracts the R-squared value from a fitted ftmglm or ftmlm object.
#' This function provides a measure of how well the model explains the variability
#' of the response/outcome and updates based on the available variables.
#' 
#' @param object An object of class ftmglm or ftmlm.
#' @param select (optional) A character string of variables or indices to use. If omitted,
#' all variables are used to compute R-squared.
#' 
#' @details
#' When \code{select} is specified, only the selected variables are used to calculate 
#' R-squared.
#' 
#' For linear models (ftmlm objects), R-squared is calculated using:
#' \deqn{TSS = yty - n \times y\_mean^2}
#' \deqn{SSR = B^T X^T y - n \times y\_mean^2}
#' \deqn{R^2 = \frac{SSR}{TSS}}
#' 
#' Where:
#' \itemize{
#'   \item TSS = Total Sum of Squares
#'   \item SSR = Sum of Squares due to Regression
#'   \item yty = Sum of squared outcome values
#'   \item n = Number of observations
#'   \item y_mean = Mean of the outcome variable
#'   \item B = Model coefficients
#' }
#' 
#' @return A numeric value representing the R-squared (for ftmlm) or pseudo-R-squared 
#' (for ftmglm) of the model. Values range from 0 to 1, with higher values indicating 
#' better model fit.
#' 
#' @examples
#' \dontrun{
#' # Assuming ftmglmModel and ftmlmModel are pre-fitted model objects
#' # Calculate R-squared for a ftmglm model using all variables
#' r_squared_glm <- rsq(ftmglmModel)
#'
#' # Calculate R-squared using specific variables from a ftmglm model
#' r_squared_glm_select <- rsq(ftmglmModel, select = c("var1", "var2"))
#'
#' # Calculate R-squared for a ftmglm model using all variables
#' r_squared_lm <- rsq(ftmlmModel)
#'
#' # Calculate R-squared using specific variables from a ftmlm model
#' r_squared_lm_select <- rsq(ftmlmModel, select = c("var1", "var2"))
#' }
#'
#' @seealso \code{\link{predict,ftmglm-method}} and \code{\link{predict,ftmlm-method}} for methods to make
#' predictions using model objects.
#' @export
#' @importFrom methods slotNames
#' @include ftmglm.R ftmlm.R
setMethod("rsq", "ftmglm",
    function(object, select = NULL) {
    }
)


#' @title Extract R-squared from a ftmlm object
#' @rdname rsq-ftmglm-method
#' @export
setMethod("rsq", "ftmlm",
    function(object, select = NULL) {

        # Check if the object contains the necessary slots
        if (!all(c("XtX", "Xty", "yty", "n", "y_mean") %in% slotNames(object))) {
            message("The object does not contain the necessary slots for R-squared calculation.")
            return(NA)
        }
        # Check if the object has NAs in the yty, n, or y_mean slots
        if (any(is.na(object@yty), is.na(object@n), is.na(object@y_mean))) {
            message("The object contains NAs in the yty, n, or y_mean slots.")
            return(NA)
        }

        # If no variables are specified, select all variables
        if (is.null(select)) {
            select <- colnames(object@XtX)
        }

        # Ensure that we select the intercept
        if (!"(Intercept)" %in% select) {
            select <- c("(Intercept)", select)
        }

        # Subset object to the intersecting variables
        XtX <- object@XtX[select, select, drop = FALSE]
        Xty <- object@Xty[select, , drop = FALSE]

        # Perform SVD on the XtX matrix
        XtX_SVD <- svd(XtX)

        # Get the inverse of the eigen values that are greater than 1e-16
        eigenvalues <- 1 / XtX_SVD$d[XtX_SVD$d > 1e-16]

        # Calculate the truncated SVD inverse
        XtX_inv <- XtX_SVD$u[, seq(length(eigenvalues))] %*% diag(eigenvalues) %*% t(XtX_SVD$v[, seq(length(eigenvalues))])

        # Estimate the beta coefficients
        beta <- c(XtX_inv %*% Xty)

        # Extract the yty, n, and y_mean from the object
        yty <- object@yty
        n <- object@n
        y_mean <- object@y_mean

        # Calculate the total sum of squares (TSS)
        TSS <- yty - n * y_mean^2
        # Calculate the sum of squares due to regression (SSR)
        SSR <- as.numeric(crossprod(beta, Xty)) - n * y_mean^2

        # Calculate R-squared
        R_squared <- SSR / TSS

        # Return the R-squared value
        return(R_squared)
    }
)
