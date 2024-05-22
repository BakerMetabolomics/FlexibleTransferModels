#' @title Extract Model Coefficients from a ftmglm or ftmlm object
#'
#' @description Extracts the beta coefficients from a \code{ftmglm} or
#' \code{ftmlm} model. Allows coefficients to be estimated using a given set of variables.
#'
#' @param object Object of class inheriting from \code{ftmglm} or \code{ftmlm}.
#' @param select (optional) A character string of variables or indices to use. If omitted,
#' coefficients for all variables are returned.
#' @param s (optional) Ridge penalty to apply during reweighting. Default is 0, indicating no penalty.
#'
#' @details
#' When \code{select} is specified, only the coefficients for the selected variables are calculated.
#'
#' The ridge penalty parameter \code{s} is only applicable to \code{ftmlm} models.
#'
#' @return A named numeric vector of coefficients.
#'
#' @examples
#' \dontrun{
#' # Assuming ftmglmModel and ftmlmModel are pre-fitted model objects
#' # Extract all coefficients from a ftmglm model
#' coeffs_glm <- coef(ftmglmModel)
#'
#' # Extract coefficients for specific variables from a ftmglm model
#' coeffs_glm_select <- coef(ftmglmModel, select = c("var1", "var2"))
#'
#' # Extract all coefficients from a ftmlm model with a ridge penalty
#' coeffs_lm <- coef(ftmlmModel, s = 0.1)
#'
#' # Extract coefficients for specific variables from a ftmlm model with no penalty
#' coeffs_lm_select <- coef(ftmlmModel, select = c("var1", "var2"))
#'}
#'
#' @seealso \code{\link{predict,ftmglm-method}} and \code{\link{predict,ftmlm-method}} for methods to make
#' predictions using model objects.
#' @note The use of a ridge penalty is not applicable to \code{ftmglm} models and will be ignored if specified.
#' @export
#' @include ftmglm.R ftmlm.R
setMethod("coef", "ftmglm",
    function(object, select = NULL) {

        # If no variables are specified, select all variables
        if (is.null(select)) {
            select <- colnames(object@XtWX)
        }

        # Ensure that we select the intercept
        if (!"(Intercept)" %in% select) {
            select <- c("(Intercept)", select)
        }

        # Subset object to the intersecting variables
        XtWX <- object@XtWX[select, select, drop = FALSE]
        XtWz <- object@XtWz[select, , drop = FALSE]

        # Perform SVD on the XtWX matrix
        XtWX_SVD <- svd(XtWX)

        # Get the inverse of the eigen values that are greater than 1e-16
        eigenvalues <- 1 / XtWX_SVD$d[XtWX_SVD$d > 1e-16]

        # Calculate the truncated SVD inverse
        XtWX_inv <- XtWX_SVD$u[, seq(length(eigenvalues))] %*% diag(eigenvalues) %*% t(XtWX_SVD$v[, seq(length(eigenvalues))])

        # Estimate the beta coefficients
        beta <- c(XtWX_inv %*% XtWz)

        # Add names to the coefficients
        names(beta) <- select

        # Return the beta coefficients
        return(beta)
    }
)


#' @title Extract Model Coefficients from a ftmglm or ftmlm object
#' @rdname coef-ftmglm-method
#' @export
setMethod("coef", "ftmlm",
    function(object, select = NULL, s = NULL) {

        # Extract s from the object if not provided
        if (is.null(s)) {
            s <- ifelse(is.null(object@s), 0, object@s)
        }

        # Validation for s
        if (!is.numeric(s) || s < 0) {
            stop("s must be numeric and positive")
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

        # Add names to the coefficients
        names(beta) <- select

        # Return the beta coefficients
        return(beta)
    }
)
