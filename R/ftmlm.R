#' @title Flexible Transfer Model (FTM) Linear Model Class
#'
#' @description An S4 class designed to encapsulate the necessary components for a flexible
#' transfer model linear model. This class stores matrices that
#' represent the weighted cross-product of predictors and the cross-product of predictors
#' with outcomes, essential for the model calculations.
#'
#' @section Usage:
#' This class is internally used by functions within the package to perform
#' regression analysis. It is not intended for direct use.
#'
#' @section Slots:
#' \describe{
#'   \item{XtX}{\code{matrix} representing the cross product of the predictor variables.}
#'   \item{Xty}{\code{matrix} representing the cross product of the predictor variables and outcomes.}
#'   \item{s}{\code{numeric} representing the optimal Ridge penalty from cross-validation (if available).}
#'   \item{n}{\code{numeric} representing the number of observations.}
#'   \item{yty}{\code{numeric} representing the sum of squared responses.}
#'   \item{y_mean}{\code{numeric} representing the mean of the outcome variable.}
#'}
#'
#' @export
setClass("ftmlm",
         slots = list(XtX = "matrix",
                      Xty = "matrix",
                      s = "ANY",
                      n = "numeric",
                      yty = "numeric",
                      y_mean = "numeric"),
         prototype = prototype(s = 0,
                               n = NA_real_,
                               yty = NA_real_,
                               y_mean = NA_real_))


#' @title Constructor for ftmlm Objects
#'
#' @description Creates an instance of the \code{\linkS4class{ftmlm}} class, initializing
#' it with specific matrices necessary for the flexible transfer model calculations.
#' This function is typically called internally within higher-level functions that
#' prepare data and perform model fitting.
#'
#' @param XtX A numeric matrix representing the cross product of predictors.
#' @param Xty A numeric matrix representing the cross product of predictors and outcomes.
#' @param s A numeric value representing the optimal Ridge penalty from cross-validation (if available).
#' @param n A numeric value representing the number of observations.
#' @param yty A numeric value representing the sum of squared responses.
#' @param y_mean A numeric value representing the mean of the outcome variable.
#'
#' @return An object of class \code{\linkS4class{ftmlm}}.
#'
#' @seealso \code{\link{ftmlm-class}} for details on the class structure.
#' @examples
#' \dontrun{
#'   XtX <- matrix(rnorm(100), 10, 10)
#'   Xty <- matrix(rnorm(10), 10, 1)
#'   model <- ftmlm(XtX, Xty)
#' }
#' @export
ftmlm <- function(XtX,
                  Xty,
                  s = 0,
                  n = NA_real_,
                  yty = NA_real_,
                  y_mean = NA_real_) {
    validate_input_input(XtX, Xty, s, n, yty, y_mean)
    new("ftmlm",
        XtX = XtX,
        Xty = Xty,
        s = s,
        n = n,
        yty = yty,
        y_mean = y_mean)
}

