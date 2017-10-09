context("geocode")

test_that("Malvina Place is returned", {
  out <- geocode(flat_number = NA_character_,
                 number_first = 8L,
                 street_name = "MALVINA",
                 street_type = "PLACE",
                 postcode = 3053L)
  # About 10 m
  expect_equal(round(out$LATITUDE, 4), -37.8006, tol = 0.0001)
  expect_equal(round(out$LONGITUDE, 4), 144.9617, tol = 0.0001)
})
