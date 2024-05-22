#' @title Create a Flexible Transfer Model (FTM) Object from Different Model Types
#'
#' @description This wrapper function creates an FTM object from various types of model objects, including
#' glm, glmnet, and lm, by extracting necessary components and adapting them into the FTM framework.
#'
#' @param modelObj A model object from glm, glmnet, or lm.
#' @param s Optional value of the penalty parameter (lambda); applicable only for glmnet or lm models.
#'
#' @return An FTM object either of type ftmglm or ftmlm depending on the input model.
#' @export
#' @examples
#' \dontrun{
#' data(mtcars)
#' lm_model <- lm(mpg ~ cyl + hp + wt, data = mtcars)
#' ftm_object <- createFTM(lm_model)
#'
#' glm_model <- glm(am ~ hp + wt + cyl, data = mtcars, family = "binomial")
#' ftm_object <- createFTM(glm_model)
#'
#' predictors <- as.matrix(mtcars[, c("hp", "wt", "cyl")])
#' glmnet_model <- glmnet::cv.glmnet(predictors, mtcars$am, family = "binomial")
#' ftm_object <- createFTM(glmnet_model)
#' }
createFTM <- function(modelObj, s = NULL) {

    # Determine if the model is a linear model from lm
    if (inherits(modelObj, "lm")) {
        return(createFromLm(modelObj))
    }

    # Determine if the model is a logistic model from glm
    if (inherits(modelObj, "glm")) {
        return(createFromGlm(modelObj))
    }

    # Determine if the model is a glmnet model
    if (!inherits(modelObj, "glmnet")) {
        stop(sprintf("modelObj must be a lm, glm, cv.glmnet, or glmnet object. Provided object class: %s", class(modelObj)))
    }

    # Create an FTM object from glmnet model
    return(createFromGlmnet(modelObj, s))
}
