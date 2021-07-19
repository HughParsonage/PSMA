context("test-utils")

test_that("dist2km works", {
  expect_equal(dist2km("123km"), 123)
  expect_equal(dist2km("12.3 km"), 12.3)
  expect_equal(dist2km("12.3m"), 12.3/1000)
  expect_equal(dist2km("123 m"), 123/1000)
})
