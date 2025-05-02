#' @title Create a Flexible Transfer Model (ftmglm or ftmlm) from a glmnet object
#'
#' @description This function extracts necessary components from a glmnet object (either cv.glmnet or glmnet)
#' to create an ftmglm object. It uses the optimal, or specified, penalty parameter (lambda) and the data used in
#' the glmnet model to adapt them into the FTM framework.
#'
#' @param glmnetObj a cv.glmnet or glmnet object from the glmnet package
#' @param s value of the penalty parameter (lambda) to use; default for cv.glmnet is lambda.min
#' @param x matrix of predictors used in the glmnet model
#' @param y vector of outcomes used in the glmnet model
#' @param outcome_name Optional name of the outcome variable; if not provided, it will be extracted from the model object.
#'
#' @return A ftmglm or ftmlm object.
#'
#' @note The data used in the glmnet model must be available in the global environment.
#'
#' @examples
#' \dontrun{
#' # Fitting a glmnet model to the mtcars dataset
#' data(mtcars)
#' predictors <- as.matrix(mtcars[, c("hp", "wt", "cyl")])
#' glmnet_model <- glmnet::cv.glmnet(predictors, mtcars$am, family = "binomial", alpha = 0)
#' ftmglm_model <- createFromGlmnet(glmnet_model, x = predictors, y = mtcars$am)
#' }
#' @import glmnet
#' @export
createFromGlmnet <- function(glmnetObj, s = NULL, x = NULL, y = NULL, outcome_name = NULL) {

    # Validation for glmnetObj
    if (!inherits(glmnetObj, c("cv.glmnet", "glmnet"))) {
        stop(sprintf("glmnetObj must be a cv.glmnet or glmnet object. Provided object class: %s", class(glmnetObj)))
    }

    # Determine the lambda to use
    if (is.null(s)) {
        if (inherits(glmnetObj, "cv.glmnet")) {
            s <- glmnetObj$lambda.min
        } else {
            stop("s must be specified for glmnet objects unless a cv.glmnet object is provided.")
        }
    }

    # Convert "lambda.min" or "lambda.1se" to numeric
    if (is.character(s)) {

        # Ensure that s is either "lambda.min" or "lambda.1se"
        if (!s %in% c("lambda.min", "lambda.1se")) {
            stop("s must be either a numeric value, 'lambda.min' or 'lambda.1se'.")
        }

        # Extract the lambda values from the glmnet object
        s <- glmnetObj[[s]]
    }

    # Get the outcome variable name
    if (is.null(outcome_name)) {
        # Extract the outcome name from the column names of the response variable, or from the glmnet object
        if (is.data.frame(y) || is.matrix(y)) {
            outcome_name <- colnames(y)
        } else if (inherits(glmnetObj, "cv.glmnet")) {
            outcome_name <- get_outcome_name(glmnetObj$glmnet.fit)
        } else {
            outcome_name <- get_outcome_name(glmnetObj)
        }
    }

    # Error if either the predictors or outcome are not found
    if (is.null(x) || is.null(y)) {
        stop("The predictors and outcome must be provided.")
    }

    # Convert the predictors to a matrix if it is a data frame
    if (!is.matrix(x)) {
        x <- as.matrix(x)
    }
    # Convert the outcome to a vector if it is a vector or data frame
    if (!is.matrix(y)) {
        y <- as.matrix(y)
    }
    # Ensure the outcome is a single column
    if (ncol(y) > 1) {
        stop("The outcome must be a single column.")
    }
    # Ensure the number of rows in the predictors and outcome match
    if (nrow(x) != nrow(y)) {
        stop("The number of rows in the predictors and outcome must match.")
    }

    # Create a new X matrix that includes the predictors and intercept
    X <- cbind("(Intercept)" = 1,
               x)

    # If glmnetObj is a cv.glmnet object, extract the glmnet.fit object
    if (inherits(glmnetObj, "cv.glmnet")) {
        glmnetObj <- glmnetObj$glmnet.fit
    }

    # Determine whether the model is a binomial or linear model
    if (class(glmnetObj)[1] == "elnet") {

        # Calculate Xty and XtWX
        Xty <- t(X) %*% y
        XtX <- t(X) %*% X

        # Update the outcome name
        colnames(Xty) <- outcome_name

        # Create ftmlm object
        return(ftmlm(XtX = XtX, Xty = Xty, s = s))

    } else if (class(glmnetObj)[1] == "lognet") {

        # Calculate necessary components
        p <- predict(glmnetObj, newx = x, type = "response", s = s)

        # Calculate the weights
        w <- p * (1 - p)

        # Calculate the log odds
        z <- logit(p)

        # Calculate XtWz and XtWX
        XtWz <- t(X) %*% diag(as.vector(w)) %*% z
        XtWX <- t(X) %*% diag(as.vector(w)) %*% X

        # Update outcome name
        colnames(XtWz) <- outcome_name

        # Create ftmglm object
        return(ftmglm(XtWX = XtWX, XtWz = XtWz))

    } else {
        # Warn user that the model is not binomial or linear
        stop(sprintf("glmnetObj must be a binomial or linear (gaussian) model. Provided model is a: %s", class(glmnetObj$glmnet.fit)[1]))
    }

}
