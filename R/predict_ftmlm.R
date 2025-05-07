#' Predict Method for Flexible Transfer Models (ftmglm and ftmlm)
#' @rdname predict-ftmglm-method
#' @export
setMethod("predict", "ftmlm",
    function(object, newdata, s = NULL) {

        # Validation for object
        if (!inherits(object, "ftmlm")) {
            stop("object must be of class ftmlm")
        }

        # Validation for newdata
        if (missing(newdata)) {
            stop("newdata must be provided")
        }

        # Validation for newdata
        if (!is.data.frame(newdata)) {
            newdata <- as.data.frame(newdata)
            warning("newdata was coerced to a data frame")
        }

        # If s is not provided, use the s from the object
        if (is.null(s)) {
            s <- ifelse(is.null(object@s), 0, object@s)
        }

        # Validation for s
        if (!is.numeric(s) || s < 0) {
            stop("s must be numeric and positive")
        }

        # Get the intersection of variables between newdata and model
        intersecting_vars <- intersect(colnames(newdata),
                                       colnames(object@XtX))

        # If there are no overlapping variables, throw an error
        if (length(intersecting_vars) == 0) {
            stop("No overlapping variables between new data and model.")
        }

        # Subset newdata to the intersecting variables
        newdata <- cbind("(Intercept)" = 1,
                         newdata[, intersecting_vars, drop = FALSE])

        # Subset object to the intersecting variables
        XtX <- object@XtX[c("(Intercept)", intersecting_vars),
                          c("(Intercept)", intersecting_vars), drop = FALSE]
        Xty <- object@Xty[c("(Intercept)", intersecting_vars), , drop = FALSE]

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
        beta <- XtX_inv %*% Xty

        # Compute the design matrix
        X <- as.matrix(newdata)

        # Compute the predicted values
        linear_predictors <- X %*% beta

        # Return the predicted values
        return(linear_predictors)

    }
)
