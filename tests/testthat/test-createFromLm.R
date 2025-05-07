# Test that ftmlm object is properly created
test_that("createFromLm returns correctly structured ftmlm object", {
    data(mtcars)
    lm_model <- lm(mpg ~ cyl + hp + wt, data = mtcars)
    result <- createFromLm(lm_model)
    expect_s4_class(result, "ftmlm")
    expect_true(is.matrix(slot(result, "XtX")))
    expect_true(is.matrix(slot(result, "Xty")))
    expect_equal(slot(result, "s"), 0)
    expect_equal(ncol(slot(result, "Xty")), 1)
    expect_equal(nrow(slot(result, "XtX")), ncol(model.matrix(lm_model)))
    expect_identical(colnames(slot(result, "Xty")), "mpg")
})

# Test that ftmlm contains the correct data
test_that("createFromLm calculates correct matrix dimensions and values", {
    data(mtcars)
    lm_model <- lm(mpg ~ cyl + hp + wt, data = mtcars)
    result <- createFromLm(lm_model)
    expected_XtX <- crossprod(model.matrix(lm_model))
    expected_Xty <- crossprod(model.matrix(lm_model), mtcars$mpg)
    colnames(expected_Xty) <- "mpg"
    expect_equal(slot(result, "XtX"), expected_XtX)
    expect_equal(slot(result, "Xty"), expected_Xty)
})
