context("fetch_postcodes")

test_that("Postcode fails if not a number", {
  expect_error(fetch_postcodes("XXX"))
})

test_that("Random samples match the geocode", {
  pcodes <- c(4171, 4032, 4122)
  bnedata <- fetch_postcodes(pcodes)
  set.seed(89)
  three.per.postcode <- bnedata[, .SD[sample(.N, 3)], by=.(POSTCODE)]
  coordinates <- with(three.per.postcode,
                      geocode(flat_number = FLAT_NUMBER,
                         number_first = NUMBER_FIRST,
                         street_name = STREET_NAME,
                         street_type = STREET_TYPE_CODE,
                         postcode = POSTCODE)
  )
  expect_equal(coordinates$LATITUDE, three.per.postcode$LATITUDE)
  expect_equal(coordinates$LONGITUDE, three.per.postcode$LONGITUDE)

})
