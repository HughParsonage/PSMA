context("Reverse geocoding")

test_that("Works for Grattan Institute", {
  Grattan <- revgeocode(-37.80058, 144.9618)
  # May be 8 or 10
  expect_equal(Grattan[["NUMBER_FIRST"]], 9, tol = 1.5)
  expect_equal(Grattan[["STREET_NAME"]], "MALVINA")
})

test_that("Works for Random ACT location", {
  Grattan <- revgeocode(-35.4, 149.1)
  expect_equal(Grattan[["NUMBER_FIRST"]], 17)
  expect_equal(Grattan[["STREET_NAME"]], "MCWHAE")
  expect_equal(Grattan[["STREET_TYPE_CODE"]], "CIRCUIT")
})
