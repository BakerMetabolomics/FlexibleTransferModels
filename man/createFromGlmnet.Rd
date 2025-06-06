% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/createFromGlmnet.R
\name{createFromGlmnet}
\alias{createFromGlmnet}
\title{Create a Flexible Transfer Model (ftmglm or ftmlm) from a glmnet object}
\usage{
createFromGlmnet(glmnetObj, s = NULL, x = NULL, y = NULL, outcome_name = NULL)
}
\arguments{
\item{glmnetObj}{a cv.glmnet or glmnet object from the glmnet package}

\item{s}{value of the penalty parameter (lambda) to use; default for cv.glmnet is lambda.min}

\item{x}{matrix of predictors used in the glmnet model}

\item{y}{vector of outcomes used in the glmnet model}

\item{outcome_name}{Optional name of the outcome variable; if not provided, it will be extracted from the model object.}
}
\value{
A ftmglm or ftmlm object.
}
\description{
This function extracts necessary components from a glmnet object (either cv.glmnet or glmnet)
to create an ftmglm object. It uses the optimal, or specified, penalty parameter (lambda) and the data used in
the glmnet model to adapt them into the FTM framework.
}
\note{
The data used in the glmnet model must be available in the global environment.
}
\examples{
\dontrun{
# Fitting a glmnet model to the mtcars dataset
data(mtcars)
predictors <- as.matrix(mtcars[, c("hp", "wt", "cyl")])
glmnet_model <- glmnet::cv.glmnet(predictors, mtcars$am, family = "binomial", alpha = 0)
ftmglm_model <- createFromGlmnet(glmnet_model, x = predictors, y = mtcars$am)
}
}
