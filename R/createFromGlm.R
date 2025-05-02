#' @title Create a Flexible Transfer Model (ftmglm or ftmlm) from a glm object
#'
#' @description This function extracts necessary components from a glm object to create an ftmglm object.
#' It extracts the necessary components and adapts them into the FTM framework.
#'
#' @param glmObj a glm object from the stats package
#' @param outcome_name Optional name of the outcome variable; if not provided, it will be extracted from the model object.
#'
#' @return A ftmglm object.
#'
#' @examples
#' \dontrun{
#' # Fitting a glm to the mtcars dataset
#' data(mtcars)
#' glm_model <- glm(am ~ hp + wt + cyl, data = mtcars, family = "binomial")
#' ftmglm_model <- createFromGlm(glm_model)
#' }
#' @export
#' @importFrom stats formula model.matrix
createFromGlm <- function(glmObj, outcome_name = NULL) {

    # Validation for glmObj
    if (!inherits(glmObj, "glm")) {
        stop(sprintf("glmObj must be a glm object. Provided object class: %s", class(glmObj)))
    }

    # Calculate necessary components
    p <- glmObj$fitted.values

    # Extract the model matrix
    X <- model.matrix(formula(glmObj), glmObj$data)

    # Calculate the weights
    w <- p * (1 - p)

    # Calculate the log odds
    z <- logit(p)

    # Calculate XtWz and XtWX
    XtWz <- t(X) %*% diag(as.vector(w)) %*% z
    XtWX <- t(X) %*% diag(as.vector(w)) %*% X

    # Get the outcome name (if not provided)
    if (is.null(outcome_name)) {
        outcome_name <- all.vars(formula(glmObj))[1]
    }

    # Update the outcome name
    colnames(XtWz) <- outcome_name

    # Create ftmglm object
    ftmglm(XtWX = XtWX, XtWz = XtWz)
}
