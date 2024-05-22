# Setup
data(mtcars)
# binomial model
predictors <- as.matrix(mtcars[, c("hp", "wt", "cyl")])
binom_glmnet_model <- glmnet::cv.glmnet(predictors, mtcars$am, family = "binomial")
# gaussian model
gaussian_glmnet_model <- glmnet::cv.glmnet(predictors, mtcars$mpg, family = "gaussian")

# Test that ftmglm and ftmlm object is properly created
test_that("createFromGlmnet returns correctly structured ftmglm or ftmlm object", {
    # binomial model
    result <- createFromGlmnet(binom_glmnet_model, x = predictors, y = mtcars$am)
    expect_s4_class(result, "ftmglm")
    expect_true(is.matrix(slot(result, "XtWX")))
    expect_true(is.matrix(slot(result, "XtWz")))
    # gaussian model
    result <- createFromGlmnet(gaussian_glmnet_model, x = predictors, y = mtcars$mpg)
    expect_s4_class(result, "ftmlm")
    expect_true(is.matrix(slot(result, "XtX")))
    expect_true(is.matrix(slot(result, "Xty")))
    expect_equal(slot(result, "s"), gaussian_glmnet_model$lambda.min)
})

# Test that mismatched rows in data is caught
test_that("createFromGlmnet checks and handles mismatched predictors and outcomes", {
    expect_error(result <- createFromGlmnet(binom_glmnet_model, x = predictors, y = mtcars$am[1:20]),
                 "The number of rows in the predictors and outcome must match")
    expect_error(result <- createFromGlmnet(gaussian_glmnet_model, x = predictors, y = mtcars$mpg[1:20]),
                 "The number of rows in the predictors and outcome must match")
})

# Test that ftmglm contains the correct data
test_that("createFromGlmnet calculates correct matrix dimensions and values", {
    # binomial model
    result <- createFromGlmnet(binom_glmnet_model, x = predictors, y = mtcars$am)
    p <- predict(binom_glmnet_model, newx = predictors, type = "response", s = binom_glmnet_model$lambda.min)
    w <- p * (1 - p)
    expected_XtWz <- t(cbind("(Intercept)" = 1, predictors)) %*% (w * log(p / (1 - p)))
    colnames(expected_XtWz) <- "am"
    expect_equal(slot(result, "XtWz"), expected_XtWz)
    # gaussian model
    result <- createFromGlmnet(gaussian_glmnet_model, x = predictors, y = mtcars$mpg)
    expected_XtX <- crossprod(cbind("(Intercept)" = 1, predictors))
    expected_Xty <- crossprod(cbind("(Intercept)" = 1, predictors), mtcars$mpg)
    colnames(expected_Xty) <- "mpg"
    expect_equal(slot(result, "XtX"), expected_XtX)
    expect_equal(slot(result, "Xty"), expected_Xty)
})
