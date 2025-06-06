% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/createFTM.R
\name{createFTM}
\alias{createFTM}
\title{Create a Flexible Transfer Model (FTM) Object from Different Model Types}
\usage{
createFTM(modelObj, s = NULL, x = NULL, y = NULL, outcome_name = NULL)
}
\arguments{
\item{modelObj}{A model object from glm, glmnet, or lm.}

\item{s}{Optional value of the penalty parameter (lambda); applicable only for glmnet or lm models.}

\item{x}{matrix of predictors used in the glmnet model.}

\item{y}{vector of outcomes used in the glmnet model.}

\item{outcome_name}{Optional name of the outcome variable; if not provided, it will be extracted from the model object.}
}
\value{
An FTM object either of type ftmglm or ftmlm depending on the input model.
}
\description{
This wrapper function creates an FTM object from various types of model objects, including
glm, glmnet, and lm, by extracting necessary components and adapting them into the FTM framework.
}
\examples{
\dontrun{
data(mtcars)
lm_model <- lm(mpg ~ cyl + hp + wt, data = mtcars)
ftm_object <- createFTM(lm_model)

glm_model <- glm(am ~ hp + wt + cyl, data = mtcars, family = "binomial")
ftm_object <- createFTM(glm_model)

predictors <- as.matrix(mtcars[, c("hp", "wt", "cyl")])
glmnet_model <- glmnet::cv.glmnet(predictors, mtcars$am, family = "binomial")
ftm_object <- createFTM(glmnet_model)
}
}
