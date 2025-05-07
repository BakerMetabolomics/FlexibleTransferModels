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
#' @param s (optional) Ridge penalty to apply during reweighting. Default is 0, indicating no penalty.
#' 
#' @details
#' When \code{select} is specified, only the selected variables are used to calculate 
#' R-squared.
#' 
#' For linear models (ftmlm objects), R-squared is calculated using:
#' \deqn{TSS = yty - n \times y\_mean^2}
#' \deqn{RSS = yty - 2 \times \beta^T \times Xty + \beta^T \times XtX \times \beta}
#' \deqn{R^2 = 1 - \frac{RSS}{TSS}}
#' 
#' Where:
#' \itemize{
#'   \item TSS = Total Sum of Squares
#'   \item RSS = Residual Sum of Squares
#'   \item yty = Sum of squared outcome values
#'   \item n = Number of observations
#'   \item y_mean = Mean of the outcome variable
#'   \item B = Model coefficients
#' }
#' 
#' When \code{s} is specified, a ridge penalty is applied while calculating R-squared.
#' The estimate of R-squared is no longer exact under this condition, but it still provides
#' a useful measure of model fit.
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
#' @note The use of a ridge penalty is not applicable to \code{ftmglm} models and will be ignored if specified.
#' @export
#' @importFrom methods slotNames
#' @include ftmglm.R ftmlm.R
setMethod("rsq", "ftmglm",
    function(object, select = NULL, s = NULL) {
        ## Warn user this is not implemented yet
        stop("R-squared calculation for ftmglm objects is not implemented yet.")
    }
)


#' @title Extract R-squared from a ftmlm object
#' @rdname rsq-ftmglm-method
#' @export
setMethod("rsq", "ftmlm",
    function(object, select = NULL, s = NULL) {

        # Extract s from the object if not provided
        if (is.null(s)) {
            s <- ifelse(is.null(object@s), 0, object@s)
        }

        # Validation for s
        if (!is.numeric(s) || s < 0) {
            stop("s must be numeric and positive")
        }

        # Check if the object contains the necessary slots
        if (!all(c("yty", "n", "y_mean") %in% slotNames(object))) {
            stop("The object does not contain the necessary slots for R-squared calculation.")
        }
        # Check if the object has NAs in the yty, n, or y_mean slots
        if (any(is.na(object@yty), is.na(object@n), is.na(object@y_mean))) {
            stop("The object contains NAs in the yty, n, or y_mean slots.")
        }

        # If no variables are specified, select all variables
        if (is.null(select)) {
            select <- colnames(object@XtX)
        }

        # Ensure that we select the intercept
        if (!"(Intercept)" %in% select) {
            select <- c("(Intercept)", select)
        }
        
        # Get the intersection of variables between select and model
        select <- intersect(select,
                            colnames(object@XtX))

        # Ensure we have more than one variable
        if (length(select) < 2 && select == "(Intercept)") {
            stop("Ensure that at least two variables (including the intercept) are retained in the model.")
        }

        # Subset object to the intersecting variables
        XtX <- object@XtX[select, select, drop = FALSE]
        Xty <- object@Xty[select, , drop = FALSE]

        # If s is provided, we add a ridge penalty and invert XtX
        if (s > 0) {

            # Get the schur complement of the XtX matrix
            XtX_comp <- schur_complement(XtX)

            # Get the diagonal to use for the ridge penalty
            ridge_diag <- c(0, s * diag(XtX_comp))

            # Calculate the inverse of XtX + lambda * I, accounting for the scale of variables
            XtX_inv <- solve(XtX + diag(ridge_diag, nrow = nrow(XtX), ncol = ncol(XtX)))

        } else {

            # Perform SVD on the XtX matrix
            XtX_SVD <- svd(XtX)

            # Get the inverse of the eigen values that are greater than 1e-16
            eigenvalues <- 1 / XtX_SVD$d[XtX_SVD$d > 1e-16]

            # Calculate the truncated SVD inverse
            XtX_inv <- XtX_SVD$u[, seq(length(eigenvalues))] %*% diag(eigenvalues) %*% t(XtX_SVD$v[, seq(length(eigenvalues))])

        }

        # Estimate the beta coefficients
        beta <- c(XtX_inv %*% Xty)

        # Extract the yty, n, and y_mean from the object
        yty <- object@yty
        n <- object@n
        y_mean <- object@y_mean

        # Calculate the total sum of squares (TSS)
        TSS <- yty - n * y_mean^2
        # Calculate the sum of squares due to regression (SSR)
        # SSR <- as.numeric(crossprod(beta, Xty)) - n * y_mean^2
        # Calculate the residual sum of squares (RSS)
        RSS <- as.numeric(yty - 2 * crossprod(beta, Xty) + crossprod(beta, XtX %*% beta))

        # Calculate R-squared
        # R_squared <- SSR / TSS
        R_squared <- 1 - (RSS / TSS)

        # Return the R-squared value
        return(R_squared)
    }
)
