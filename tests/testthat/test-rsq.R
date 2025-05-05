# Setup
require(glmnet)
data(mtcars)
predictors <- as.matrix(mtcars[, c("hp", "wt", "cyl")])
# lm model
lm_model <- lm(mpg ~ hp + wt + cyl, data = mtcars)
ftmlm_object <- createFromLm(lm_model)

# Test R-squared calculation for ftmlm with all variables
test_that("R-squared is correctly calculated for ftmlm with all variables", {
    # Calculate R-squared from our method
    r2 <- rsq(ftmlm_object)
    
    # Calculate expected R-squared from the original lm model
    expected_r2 <- summary(lm_model)$r.squared
    
    # Check that our calculation is close to the expected value
    expect_equal(r2, expected_r2, tolerance = 1e-4)
})

# Test R-squared calculation for ftmlm with selected variables
test_that("R-squared is correctly calculated for ftmlm with selected variables", {
    # Calculate R-squared using only hp and wt
    r2_subset <- rsq(ftmlm_object, select = c("hp", "wt"))
    
    # Calculate expected R-squared using a model with only those variables
    subset_lm <- lm(mpg ~ hp + wt, data = mtcars)
    expected_r2_subset <- summary(subset_lm)$r.squared
    
    # The values might not be exactly equal due to different calculation methods,
    # but they should be close
    expect_lt(abs(r2_subset - expected_r2_subset), 0.01)
})

# Test that R-squared is between 0 and 1 for ftmlm
test_that("R-squared value is between 0 and 1 for ftmlm", {
    r2 <- rsq(ftmlm_object)
    expect_gte(r2, 0)
    expect_lte(r2, 1)
})

# Test that R-squared with fewer predictors is less than or equal to full model
test_that("R-squared with subset of variables is less than or equal to full model", {
    r2_full <- rsq(ftmlm_object)
    r2_subset <- rsq(ftmlm_object, select = c("hp", "wt"))
    
    # R-squared should generally decrease with fewer predictors
    expect_lte(r2_subset, r2_full)
})

# Test handling of ftmlm objects with missing slots
test_that("rsq handles ftmlm objects with missing slots", {
    # Create a partial ftmlm object missing required slots
    partial_ftmlm <- new("ftmlm", 
                         XtX = matrix(1:4, 2, 2), 
                         Xty = matrix(1:2, 2, 1),
                         s = NULL,
                         n = NA_real_,
                         yty = 100,
                         y_mean = 5)
    
    # Should return NA with a message
    expect_message(result <- rsq(partial_ftmlm), 
                   "The object contains NAs in the yty, n, or y_mean slots.")
    expect_true(is.na(result))
})
