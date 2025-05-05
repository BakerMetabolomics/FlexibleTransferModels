#' @title Show Method for ftmglm or ftmlm objects
#'
#' @description
#' This method formats and prints key statistics and information about the \code{ftmglm} or \code{ftmlm} object,
#' including the number of predictors used in the model, the formula, and a subset of the model's coefficients.
#' If there are many coefficients, only the first 10 are displayed followed by an ellipsis to indicate truncation.
#'
#' The method enhances readability and quick assessment of the model without cluttering the output, which is
#' particularly useful for complex models with many predictors or parameters.
#'
#' @param object Object of class inheriting from \code{ftmglm} or \code{ftmlm}.
#'
#' @details
#' The output includes:
#' \itemize{
#'   \item {\strong{Model Type}:} Identifies the model type of model.
#'   \item {\strong{Number of Predictors}:} Displays the number of predictors used in the model, not counting the intercept.
#'   \item {\strong{Formula}:} Shows the formula used to create the model, derived from the `formula` method for \code{ftmglm} or \code{ftmlm} objects.
#'   \item {\strong{Coefficients}:} Lists the model's coefficients. If the model has more than 10 coefficients, only the first 10 are displayed
#'     followed by "..." to indicate more coefficients are available but not shown.
#' }
#'
#' Coefficients are formatted to three decimal places for clarity. This method is useful for quick checks and summaries
#' of the model's parameters and settings without needing to invoke separate summary methods.
#'
#' @return This method does not return a value; it is used for its side effect of printing to the console.
#' @examples
#' \dontrun{
#' data(mtcars)
#' fit <- glmnet::cv.glmnet(as.matrix(mtcars[, c("hp", "wt", "cyl")]), mtcars$am, family = "binomial")
#' model <- createFromGlmnet(fit, as.matrix(mtcars[, c("hp", "wt", "cyl")]))
#' print(model)  # This will invoke the custom 'show' method
#' }
#' @export
#' @include ftmglm.R ftmlm.R
#' @importFrom utils head
#' @importFrom methods show
setMethod("show", "ftmglm", function(object) {
    cat("Flexible Transfer Model - Generalized Linear Model\n")
    cat("------------------------------------------------------\n")

    # Basic stats or summaries about the model
    cat(sprintf("  Number of predictors: %d\n", ncol(object@XtWX) - 1))

    # Show the formula
    cat("\nFormula:\n", deparse(formula(object)), "\n\n", sep = "")

    # Print a subset of coefficients (if needed)
    coefficients <- coef(object)
    if (length(coef(object)) > 10) {

        # Trim to the first 10
        coefficients <- head(coef(object), 10)

        # Add ellipsis
        coefficients <- c(coefficients, "..." = NA)

        cat("Coefficients:\n", head(coef(object), 10))
    }

    # Display coefficients if calculated and stored
    cat("Coefficients:\n")
    print.default(format(coefficients, digits = 3L), print.gap = 2L, quote = FALSE)

    # Finish with a newline
    cat("\n")
})


#' @title Show Method for ftmglm or ftmlm objects
#' @rdname show-ftmglm-method
#' @export
#' @importFrom utils head
#' @importFrom methods show
setMethod("show", "ftmlm", function(object) {
    cat("Flexible Transfer Model - Linear Model\n")
    cat("------------------------------------------------------\n")

    # Basic stats or summaries about the model
    cat(sprintf("  Number of predictors: %d\n", ncol(object@XtX) - 1))
    cat(sprintf("  Optimal lambda: %f\n", object@s))

    # Show the formula
    cat("\nFormula:\n", deparse(formula(object)), "\n\n", sep = "")

    # Print a subset of coefficients (if needed)
    coefficients <- coef(object)
    if (length(coef(object)) > 10) {

        # Trim to the first 10
        coefficients <- head(coef(object), 10)

        # Add ellipsis
        coefficients <- c(coefficients, "..." = NA)
    }

    # Display coefficients if calculated and stored
    cat("Coefficients:\n")
    print.default(format(coefficients, digits = 3L), print.gap = 2L, quote = FALSE)
    cat("\n")

    # Display the prediction R-squared
    cat(sprintf("  R-squared: %f\n", rsq(object)))

    # Finish with a newline
    cat("\n")
})
