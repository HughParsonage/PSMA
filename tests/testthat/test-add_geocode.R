context("test-add_geocode.R")

test_that("Error handling", {
  expect_error(add_geocode(list()),
               "`DT` .* data.frame")
  if (exists("bne_addresses")) {
    skip_if_not(is.data.frame(bne_addresses) &&
                  identical(dim(bne_addresses),
                            c(10L, 12L)))
  } else {
    load(system.file("extdata", "bne_addresses.rda", package = "PSMA"))
  }
  expect_error(add_geocode(bne_addresses,
                              number_first = house_number,
                              new_names = ""),
               regexp = "two names are required")
  expect_error(add_geocode(bne_addresses,
                              number_first = house_number,
                              new_names = 1:2),
               regexp = "character vector")
  expect_error(add_geocode(add_geocode(bne_addresses,
                                             number_first = house_number),
                              number_first = house_number,
                              overwrite = FALSE),
               regexp = "First entry.*but this is already a column name in DT")
  expect_error(add_geocode(add_geocode(bne_addresses,
                                       number_first = house_number),
                           number_first = house_number,
                           new_names = c("foo", "longitude"),
                           overwrite = FALSE),
               regexp = "Second entry.*but this is already a column name in DT")


})

test_that("tibbles", {
  skip_if_not_installed("tibble")
  out <-
    add_geocode(tibble::tibble(flat_number = NA_character_,
                               number_first = 1L,
                               building_name = NA_character_,
                               street_name = "George",
                               street_type = "Street",
                               postcode = 2000L))
  expect_true(tibble::is_tibble(out))
  expect_true("latitude" %in% names(out))
  expect_true("longitude" %in% names(out))
  expect_gt(out$latitude[1L], -34)

})

test_that("Formals match", {
  skip_on_cran()
  req_formals <-
    c("flat_number", "number_first", "building_name", "street_name",
      "street_type", "postcode")
  expect_true(all(req_formals %in% names(formals(geocode))),
              label = "geocode() has consistent arguments.")
  expect_true(all(req_formals %in% names(formals(add_geocode))),
              label = "add_geocode() has consistent arguments.")
})

test_that("Building names", {
  library(data.table)
  library(magrittr)
  dt <-
    data.table(flat_number = 3L,
               building_name = "AINSLIE FLATS",
               number_first = 11L,
               street_name = "QUICK",
               street_type = "Street",
               postcode = 2602L) %>%
    add_geocode



  dt2 <-
    data.table(flat_number = 3L,
               building_name = "AINSLIE FLATS",
               number_first = 11L,
               street_name = "QUICK",
               street_type = "Street",
               postcode = 2602L) %>%
    add_geocode(new_names = c("latitude", "longitude"))

  expect_identical(dt, dt2)
  expect_gt(dt[, latitude], -35.5)
  expect_lt(dt[, latitude], -35.2)
  expect_lt(dt[, longitude], 149.15)
  expect_gt(dt[, longitude], 149.14)
  setnames(dt, "flat_number", "unit_no")
  dt4 <- add_geocode(dt, flat_number = 'unit_no', overwrite = TRUE)
  expect_lt(dt[, longitude], 149.15)
  expect_gt(dt[, longitude], 149.14)

  dt5 <-
    data.table(flat = 3L,
               bldg = "AINSLIE FLATS",
               number = 11L,
               street = "QUICK",
               type = "Street",
               Postcode = 2602L) %>%
    add_geocode(flat_number = flat,
                number_first = number,
                building_name = bldg,
                street_name = street,
                street_type = type,
                postcode = Postcode,
                new_names = c("lat", "lon"))
  expect_lt(dt5[, lon], 149.15)
  expect_gt(dt5[, lon], 149.14)
})
