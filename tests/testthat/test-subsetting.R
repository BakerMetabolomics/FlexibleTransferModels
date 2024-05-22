# Setup
data(mtcars)
predictors <- as.matrix(mtcars[, c("hp", "wt", "cyl")])
# glmnet model
glmnet_model <- glmnet::cv.glmnet(predictors, mtcars$am, family = "binomial")
ftmglm_object <- createFromGlmnet(glmnet_model, x = predictors, y = mtcars$am)
# lm model
lm_model <- lm(mpg ~ hp + wt + cyl, data = mtcars)
ftmlm_object <- createFromLm(lm_model)

# Test subsetting using character vector for ftmglm
test_that("subset works correctly for ftmglm with character vector", {
    result <- subset(ftmglm_object, subset = c("hp", "cyl", "var1"))
    expect_s4_class(result, "ftmglm")
    expect_equal(dim(result@XtWX), c(3, 3))  # Including intercept
    expect_true(all(c("(Intercept)", "hp", "cyl") %in% colnames(result@XtWX)))
    result <- ftmglm_object[, c("hp", "cyl", "var1")]
    expect_s4_class(result, "ftmglm")
    expect_equal(dim(result@XtWX), c(3, 3))  # Including intercept
    expect_true(all(c("(Intercept)", "hp", "cyl") %in% colnames(result@XtWX)))
})

# Test subsetting using numeric indices for ftmglm
test_that("subset works correctly for ftmglm with numeric indices", {
    result <- subset(ftmglm_object, subset = c(2, 4, 6))
    expect_s4_class(result, "ftmglm")
    expect_equal(dim(result@XtWX), c(3, 3))  # Including intercept
    expect_true(all(c("(Intercept)", "hp", "cyl") %in% colnames(result@XtWX)))
    result <- ftmglm_object[, c(2, 4, 6)]
    expect_s4_class(result, "ftmglm")
    expect_equal(dim(result@XtWX), c(3, 3))  # Including intercept
    expect_true(all(c("(Intercept)", "hp", "cyl") %in% colnames(result@XtWX)))
})

# Test subsetting for ftmlm using character vector
test_that("subset works correctly for ftmlm with character vector", {
    result <- subset(ftmlm_object, subset = c("hp", "cyl", "var1"))
    expect_s4_class(result, "ftmlm")
    expect_equal(dim(result@XtX), c(3, 3))
    expect_true(all(c("(Intercept)", "hp", "cyl") %in% colnames(result@XtX)))
    result <- ftmlm_object[, c("hp", "cyl", "var1")]
    expect_s4_class(result, "ftmlm")
    expect_equal(dim(result@XtX), c(3, 3))
    expect_true(all(c("(Intercept)", "hp", "cyl") %in% colnames(result@XtX)))
})

# Test subsetting using numeric indices for ftmlm
test_that("subset works correctly for ftmlm with numeric indices", {
    result <- subset(ftmlm_object, subset = c(2, 4, 6))
    expect_s4_class(result, "ftmlm")
    expect_equal(dim(result@XtX), c(3, 3))
    expect_true(all(c("(Intercept)", "hp", "cyl") %in% colnames(result@XtX)))
    result <- ftmlm_object[, c(2, 4, 6)]
    expect_s4_class(result, "ftmlm")
    expect_equal(dim(result@XtX), c(3, 3))
    expect_true(all(c("(Intercept)", "hp", "cyl") %in% colnames(result@XtX)))
})

# Test error handling when too few variables are selected
test_that("subset throws an error with too few variables", {
    expect_silent(subset(ftmglm_object, subset = c("hp")))
    expect_silent(subset(ftmlm_object, subset = c("hp")))
    expect_error(subset(ftmglm_object, subset = c("(Intercept)")), "at least two variables")
    expect_error(subset(ftmlm_object, subset = c("(Intercept)")), "at least two variables")
    expect_silent(ftmglm_object[, c("hp")])
    expect_silent(ftmlm_object[, c("hp")])
    expect_error(ftmglm_object[, c("(Intercept)")], "at least two variables")
    expect_error(ftmlm_object[, c("(Intercept)")], "at least two variables")
})

# Test error handling for subset with no overlapping variables
test_that("subset handles non-existing variables", {
    expect_error(subset(ftmglm_object, subset = c("var1")), "at least two variables")
    expect_error(subset(ftmlm_object, subset = c("var1")), "at least two variables")
    expect_error(ftmglm_object[, c("var1")], "at least two variables")
    expect_error(ftmlm_object[, c("var1")], "at least two variables")
})
