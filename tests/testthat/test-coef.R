# Setup
require(glmnet)
data(mtcars)
predictors <- as.matrix(mtcars[, c("hp", "wt", "cyl")])
# glmnet model
glmnet_model <- glmnet::cv.glmnet(predictors, mtcars$am, family = "binomial", alpha = 0)
ftmglm_object <- createFromGlmnet(glmnet_model, x = predictors, y = mtcars$am)
# lm model
lm_model <- lm(mpg ~ hp + wt + cyl, data = mtcars)
ftmlm_object <- createFromLm(lm_model)


# Test full coefficient extraction for ftmglm
test_that("All coefficients are correctly extracted from ftmglm", {
    coeffs <- coef(ftmglm_object)
    expected_coeffs <- coef(glmnet_model, s = glmnet_model$lambda.min)[, "s1"]
    expect_length(coeffs, 4)
    expect_named(coeffs, c("(Intercept)", "hp", "wt", "cyl"))
    expect_equal(coeffs, expected_coeffs)
})

# Test full coefficient extraction for ftmlm
test_that("All coefficients are correctly extracted from ftmlm", {
    coeffs <- coef(ftmlm_object)
    expected_coeffs <- coef(lm_model)
    expect_length(coeffs, 4)
    expect_named(coeffs, c("(Intercept)", "hp", "wt", "cyl"))
    expect_equal(coeffs, expected_coeffs)
})

# Test selective coefficient extraction for ftmglm
test_that("Selective coefficients are correctly extracted from ftmglm", {
    coeffs <- coef(ftmglm_object, select = c("hp", "wt"))
    expect_length(coeffs, 3)  # Includes intercept by default
    expect_named(coeffs, c("(Intercept)", "hp", "wt"))
})

# Test full coefficient extraction for ftmlm
test_that("All coefficients with ridge penalty are extracted from ftmlm", {
    coeffs <- coef(ftmlm_object, select = c("hp", "wt"))
    expect_length(coeffs, 3)  # Includes intercept by default
    expect_named(coeffs, c("(Intercept)", "hp", "wt"))
})

# Test selective coefficient extraction with ridge penalty for ftmlm
test_that("Selective coefficients with ridge penalty are extracted from ftmlm", {
    coeffs <- coef(ftmlm_object, select = c("hp", "wt"), s = 0.1)
    no_penalty_coeffs <- coef(ftmlm_object, select = c("hp", "wt"), s = 0)
    expect_length(coeffs, 3)
    expect_named(coeffs, c("(Intercept)", "hp", "wt"))
    squared_diff <- sum((coeffs - no_penalty_coeffs)^2)
    expect_gt(squared_diff, 1.35)
    expect_lt(squared_diff, 1.45)
})

# Test error handling for invalid s value in ftmlm
test_that("Error is thrown for negative s in ftmlm", {
    expect_error(coef(ftmlm_object, s = -1), "s must be numeric and positive")
})

# Test error handling for non-numeric s value in ftmlm
test_that("Error is thrown for non-numeric s in ftmlm", {
    expect_error(coef(ftmlm_object, s = "invalid"), "s must be numeric and positive")
})

# Test error handling for non-existent variable in ftmglm
test_that("Error is thrown for non-existent variable in ftmglm", {
    expect_error(coef(ftmglm_object, select = c("non_existent_var")), "at least two variables")
})

# Test coef when asking for more variables than available
test_that("coef handles more variables than available in ftmglm", {
    expect_silent(coeffs <- coef(ftmglm_object, select = c("hp", "wt", "var1")))
    expect_length(coeffs, 3)  # Includes intercept by default
    expect_named(coeffs, c("(Intercept)", "hp", "wt"))
})