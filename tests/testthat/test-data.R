context("test-data.R")

test_that("dimensions are as documented", {
  expect_identical(dim(street_type_decoder),
                   c(48L, 2L))
})
