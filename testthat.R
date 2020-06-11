library(testthat)
library(Blblmpar)

#test_check("Blblmpar")

lmfit <- lm(mpg ~ wt * hp, data = mtcars)
lm_co <- lmfit$coefficients

# Bag of little bootstrap method:
blblmfit <- blblmpar(mpg ~ wt * hp, data = mtcars, m = 3, B = 100, cl = 1)
blblm_co <- coef(blblmfit)

# Test if coefficients are equal
test_that("Run tests", {
  #expect_s3_class(blblmfit, "blblmpar")
  expect_that(blblm_co, lm_co)
})
