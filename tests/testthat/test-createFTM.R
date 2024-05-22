# Test for correct lm object handling
test_that("createFTM correctly handles lm objects", {
    data(mtcars)
    lm_model <- lm(mpg ~ cyl + hp + wt, data = mtcars)
    ftm_object <- createFTM(lm_model)
    expect_s4_class(ftm_object, "ftmlm")
})

# Test for correct glm object handling
test_that("createFTM correctly handles glm objects", {
    data(mtcars)
    glm_model <- glm(am ~ hp + wt + cyl, data = mtcars, family = "binomial")
    ftm_object <- createFTM(glm_model)
    expect_s4_class(ftm_object, "ftmglm")
})

# Test for cv.glmnet binomial model handling
test_that("createFTM correctly handles cv.glmnet binomial models", {
    data(mtcars)
    predictors <- as.matrix(mtcars[, c("hp", "wt", "cyl")])
    glmnet_model <- glmnet::cv.glmnet(predictors, mtcars$am, family = "binomial")
    ftm_object <- createFTM(glmnet_model, x = predictors, y = mtcars$am)
    expect_s4_class(ftm_object, "ftmglm")
})

# Test for cv.glmnet gaussian model handling
test_that("createFTM correctly handles cv.glmnet binomial models", {
    data(mtcars)
    predictors <- as.matrix(mtcars[, c("hp", "wt", "cyl")])
    glmnet_model <- glmnet::cv.glmnet(predictors, mtcars$mpg, family = "gaussian")
    ftm_object <- createFTM(glmnet_model, x = predictors, y = mtcars$mpg)
    expect_s4_class(ftm_object, "ftmlm")
})

# Test error handling for unsupported model types
test_that("createFTM throws an error for unsupported model types", {
    expect_error(createFTM(list()), "modelObj must be a lm, glm, cv.glmnet, or glmnet object")
})
