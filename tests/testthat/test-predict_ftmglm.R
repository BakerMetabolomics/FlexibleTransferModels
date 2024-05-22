# Setup
data(mtcars)
predictors <- as.matrix(mtcars[, c("hp", "wt", "cyl")])
glmnet_model <- glmnet::cv.glmnet(predictors, mtcars$am, family = "binomial")
ftmglm_object <- createFromGlmnet(glmnet_model, x = predictors, y = mtcars$am)

# Test response and link function for ftmglm
test_that("predict computes correct link and response for ftmglm", {
    new_data <- data.frame(hp = c(110, 150), wt = c(2.5, 3.2), cyl = c(4, 6))
    predictions <- predict(ftmglm_object, newdata = new_data, type = "response")
    # response
    expected_predictions <- predict(glmnet_model, newx = as.matrix(new_data), s = glmnet_model$lambda.min, type = "response")
    colnames(expected_predictions) <- "am"
    expect_type(predictions, "double")
    expect_length(predictions, 2)
    expect_equal(predictions, expected_predictions)
    # link
    predictions <- predict(ftmglm_object, newdata = new_data, type = "link")
    expected_predictions <- predict(glmnet_model, newx = as.matrix(new_data), s = glmnet_model$lambda.min, type = "link")
    colnames(expected_predictions) <- "am"
    expect_equal(predictions, expected_predictions)
})

# Test predictions work, even with missing variables
test_that("predict provides correct responses with missing variables", {
    missing_var_data <- data.frame(hp = c(110, 150), wt = c(2.5, 3.2))  # Missing 'cyl'
    expect_silent(predictions <- predict(ftmglm_object, newdata = missing_var_data))
    expect_type(predictions, "double")
    expect_length(predictions, 2)
})

# Test error handling for incorrect data type
test_that("predict provides warning for incorrect data type input", {
    new_data <- data.frame(hp = c(110, 150), wt = c(2.5, 3.2), cyl = c(4, 6))
    expect_warning(predict(ftmglm_object, newdata = as.matrix(new_data), type = "response"),
                   "newdata was coerced to a data frame")
})

# Test error handling for no overlapping variables
test_that("predict handles newdata with no overlapping variables", {
    bad_data <- data.frame(var1 = c(110, 150), var2 = c(2.5, 3.2), var3 = c(4, 6))
    expect_error(predict(ftmglm_object, newdata = bad_data), "No overlapping variables")
})
