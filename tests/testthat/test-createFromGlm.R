# Test that ftmglm object is properly created
test_that("createFromGlm returns correctly structured ftmglm object", {
    data(mtcars)
    glm_model <- glm(am ~ hp + wt + cyl, data = mtcars, family = "binomial")
    result <- createFromGlm(glm_model)
    expect_s4_class(result, "ftmglm")
    expect_true(is.matrix(slot(result, "XtWX")))
    expect_true(is.matrix(slot(result, "XtWz")))
    expect_equal(ncol(slot(result, "XtWz")), 1)
    expect_equal(nrow(slot(result, "XtWX")), ncol(model.matrix(glm_model)))
    expect_identical(colnames(slot(result, "XtWz")), "am")
})

# Test that ftmglm contains the correct data
test_that("createFromGlm calculates correct matrix dimensions and values", {
    data(mtcars)
    glm_model <- glm(am ~ hp + wt, data = mtcars, family = "binomial")
    result <- createFromGlm(glm_model)
    p <- predict(glm_model, type = "response")
    w <- p * (1 - p)
    X <- model.matrix(glm_model)
    expected_XtWz <- t(X) %*% (w * log(p / (1 - p)))
    colnames(expected_XtWz) <- "am"
    expect_equal(slot(result, "XtWz"), expected_XtWz)
})
