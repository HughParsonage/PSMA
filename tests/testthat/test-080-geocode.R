test_that("inexact geocode", {
  malvina_10 <- geocode(number_first = 1:12,
                        street_name = "MALVINA",
                        street_type = "PLACE",
                        postcode = 3053L,
                        approx = 1L)
  # Only 8 and 10 are valid, 9 should be inferred
  expect_false(anyNA(malvina_10$LATITUDE[8:10]))

})
