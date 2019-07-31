context("Synthetic class")

test_that("Synthetic is a matrix", {
  expect_equal(class(synthetic), "matrix")
})