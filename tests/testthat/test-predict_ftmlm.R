# Setup
data(mtcars)
lm_model <- lm(mpg ~ hp + wt + cyl, data = mtcars)
ftmlm_object <- createFromLm(lm_model)

# Test predictions with correct data input for ftmlm
test_that("predict computes correct predictions for ftmlm", {
    new_data <- data.frame(hp = c(110, 150), wt = c(2.5, 3.2), cyl = c(4, 6))
    predictions <- predict(ftmlm_object, newdata = new_data)
    # Calculate expected predictions using the lm predict function
    expected_predictions <- matrix(predict(lm_model, newdata = new_data))
    colnames(expected_predictions) <- "mpg"
    expect_type(predictions, "double")
    expect_length(predictions, 2)
    expect_equal(predictions, expected_predictions)
})

# Test predictions work, even with missing variables
test_that("predict provides correct responses with missing variables in ftmlm", {
    missing_var_data <- data.frame(hp = c(110, 150), wt = c(2.5, 3.2))  # Missing 'cyl'
    expect_silent(predictions <- predict(ftmlm_object, newdata = missing_var_data))
    expect_type(predictions, "double")
    expect_length(predictions, 2)
})

# Test error handling for incorrect data type
test_that("predict provides warning for incorrect data type input in ftmlm", {
    new_data <- data.frame(hp = c(110, 150), wt = c(2.5, 3.2), cyl = c(4, 6))
    expect_warning(predict(ftmlm_object, newdata = as.matrix(new_data)),
                   "newdata was coerced to a data frame")
})

# Test error handling for no overlapping variables
test_that("predict handles newdata with no overlapping variables in ftmlm", {
    bad_data <- data.frame(var1 = c(110, 150), var2 = c(2.5, 3.2), var3 = c(4, 6))
    expect_error(predict(ftmlm_object, newdata = bad_data), "No overlapping variables")
})

# Test error handling when s is negative
test_that("predict handles negative s values in ftmlm", {
    new_data <- data.frame(hp = c(110, 150), wt = c(2.5, 3.2), cyl = c(4, 6))
    expect_error(predict(ftmlm_object, newdata = new_data, s = -0.1), "s must be numeric and positive")
})

# Test that passing in more variables than available in the model works
test_that("predict handles being passed more variables than available in the model works", {
    new_data <- data.frame(hp = c(110, 150), wt = c(2.5, 3.2), cyl = c(4, 6), extra_var = c(1, 2))
    expect_silent(predictions <- predict(ftmlm_object, newdata = new_data))
})
