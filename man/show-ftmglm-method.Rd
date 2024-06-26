% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/show.R
\name{show,ftmglm-method}
\alias{show,ftmglm-method}
\alias{show,ftmlm-method}
\title{Show Method for ftmglm or ftmlm objects}
\usage{
\S4method{show}{ftmglm}(object)

\S4method{show}{ftmlm}(object)
}
\arguments{
\item{object}{Object of class inheriting from \code{ftmglm} or \code{ftmlm}.}
}
\value{
This method does not return a value; it is used for its side effect of printing to the console.
}
\description{
This method formats and prints key statistics and information about the \code{ftmglm} or \code{ftmlm} object,
including the number of predictors used in the model, the formula, and a subset of the model's coefficients.
If there are many coefficients, only the first 10 are displayed followed by an ellipsis to indicate truncation.

The method enhances readability and quick assessment of the model without cluttering the output, which is
particularly useful for complex models with many predictors or parameters.
}
\details{
The output includes:
\itemize{
  \item {\strong{Model Type}:} Identifies the model type of model.
  \item {\strong{Number of Predictors}:} Displays the number of predictors used in the model, not counting the intercept.
  \item {\strong{Formula}:} Shows the formula used to create the model, derived from the `formula` method for \code{ftmglm} or \code{ftmlm} objects.
  \item {\strong{Coefficients}:} Lists the model's coefficients. If the model has more than 10 coefficients, only the first 10 are displayed
    followed by "..." to indicate more coefficients are available but not shown.
}

Coefficients are formatted to three decimal places for clarity. This method is useful for quick checks and summaries
of the model's parameters and settings without needing to invoke separate summary methods.
}
\examples{
\dontrun{
data(mtcars)
fit <- glmnet::cv.glmnet(as.matrix(mtcars[, c("hp", "wt", "cyl")]), mtcars$am, family = "binomial")
model <- createFromGlmnet(fit, as.matrix(mtcars[, c("hp", "wt", "cyl")]))
print(model)  # This will invoke the custom 'show' method
}
}
