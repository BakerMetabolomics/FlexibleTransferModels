#' @title Flexible Transfer Model (FTM) Generalized Linear Model Class
#'
#' @description An S4 class designed to encapsulate the necessary components for a flexible
#' transfer model generalized linear model. This class stores matrices that
#' represent the weighted cross-product of predictors and the cross-product of predictors
#' with outcomes, essential for the model calculations.
#'
#' @section Usage:
#' This class is internally used by functions within the package to perform
#' regression analysis. It is not intended for direct use.
#'
#' @section Slots:
#' \describe{
#'   \item{XtWX}{\code{matrix} representing the weighted cross product of the predictor variables.}
#'   \item{XtWz}{\code{matrix} representing the weighted cross product of the predictor variables and outcomes.}
#'}
#'
#' @export
setClass("ftmglm",
         slots = list(XtWX = "matrix", XtWz = "matrix"))


#' @title Constructor for ftmglm Objects
#'
#' @description Creates an instance of the \code{\linkS4class{ftmglm}} class, initializing
#' it with specific matrices necessary for the flexible transfer model calculations.
#' This function is typically called internally within higher-level functions that
#' prepare data and perform model fitting.
#'
#' @param XtWX A numeric matrix representing the weighted cross product of predictors.
#' @param XtWz A numeric matrix representing the weighted cross product of predictors and outcomes.
#'
#' @return An object of class \code{\linkS4class{ftmglm}}.
#'
#' @seealso \code{\link{ftmglm-class}} for details on the class structure.
#' @examples
#' \dontrun{
#'   XtWX <- matrix(rnorm(100), 10, 10)
#'   XtWz <- matrix(rnorm(10), 10, 1)
#'   model <- ftmglm(XtWX, XtWz)
#' }
#' @export
#' @importFrom methods new
ftmglm <- function(XtWX, XtWz) {
    validate_input_input(XtWX, XtWz)
    new("ftmglm", XtWX = XtWX, XtWz = XtWz)
}

