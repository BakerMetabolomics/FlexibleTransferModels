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
#' @importFrom MASS ginv
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
        
        # Get the intersection of variables between select and model
        select <- intersect(select,
                            colnames(object@XtWX))

        # Ensure we have more than one variable
        if (length(select) < 2 && select == "(Intercept)") {
            stop("Ensure that at least two variables (including the intercept) are retained in the model.")
        }

        # Subset object to the intersecting variables
        XtWX <- object@XtWX[select, select, drop = FALSE]
        XtWz <- object@XtWz[select, , drop = FALSE]

        # Invert the XtWX matrix using MASS::ginv
        XtWX_inv <- MASS::ginv(XtWX)

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
            XtX_inv <- MASS::ginv(XtX + diag(ridge_diag, nrow = nrow(XtX), ncol = ncol(XtX)))

        } else {

            # Invert the XtX matrix using MASS::ginv
            XtX_inv <- MASS::ginv(XtX)

        }

        # Estimate the beta coefficients
        beta <- c(XtX_inv %*% Xty)

        # Add names to the coefficients
        names(beta) <- select

        # Return the beta coefficients
        return(beta)
    }
)
