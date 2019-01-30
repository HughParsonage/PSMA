context("test-utils")

test_that("dist2km works", {
  expect_equal(dist2km("12.3km", 123))
  expect_equal(dist2km("12.3 km", 123))
  expect_equal(dist2km("12.3m", 123/1000))
  expect_equal(dist2km("12.3 m", 123/1000))
})
