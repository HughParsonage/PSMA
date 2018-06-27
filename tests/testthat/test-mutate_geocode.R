context("test-mutate_geocode.R")

test_that("Formals match", {
  skip_on_cran()
  req_formals <-
    c("flat_number", "number_first", "building_name", "street_name",
      "street_type", "postcode")
  expect_true(all(req_formals %in% names(formals(geocode))),
              label = "geocode() has consistent arguments.")
  expect_true(all(req_formals %in% names(formals(mutate_geocode))),
              label = "mutate_geocode() has consistent arguments.")
})

test_that("mutate works with Nick's example", {
  if (exists("bne_addresses")) {
    skip_if_not(is.data.frame(bne_addresses) &&
                  identical(dim(bne_addresses),
                            c(10L, 12L)))
  } else {
    load(system.file("extdata", "bne_addresses.rda", package = "PSMA"))
  }
  bne_addresses$flat_number <- NA_character_
  bne_addresses$building_name <- NA_character_

  A <- mutate_geocode(bne_addresses,
                      number_first = house_number)
  A5 <- A[5L, ]
  expect_equal(A5[["lat"]], -27.467, tol = 0.01)
  expect_equal(A5[["lon"]], 153.06, tol = 0.01)


})
