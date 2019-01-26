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

test_that("Works for random PMQ location", {
  HillcrestAveHudsonAve <- revgeocode(c(-31.454144, -31.436520),
                                      c(152.920535, 152.889133))

  HillcrestAve <- HillcrestAveHudsonAve[1]

  # Test ids are in order in output
  expect_equal(HillcrestAve[["id"]], 1)
  expect_equal(as.character(HillcrestAve[["NUMBER_FIRST"]]), "5")
  expect_equal(HillcrestAve[["STREET_NAME"]], "HILLCREST")
  expect_equal(HillcrestAve[["STREET_TYPE_CODE"]], "AVENUE")
  HudsonAve <- HillcrestAveHudsonAve[2]
  expect_equal(as.character(HudsonAve[["NUMBER_FIRST"]]), "42")
  expect_equal(HudsonAve[["STREET_NAME"]], "HUDSON")
  expect_equal(HudsonAve[["STREET_TYPE_CODE"]], "AVENUE")
})

test_that("Works for both", {
  skip("Implementation suspended")
  geoc <- revgeocode(c(-37.80058, -35.4), c(144.9618, 149.1), topn = 2L)
  expect_equal(nrow(geoc), 4L)
  expect_equal(geoc[ordering == 1L, unique(STREET_NAME)], "MALVINA")

  geoc <- revgeocode(c(-35.4, -37.80058), c(149.1, 144.9618), topn = 2L)
  expect_equal(geoc[ordering == 2L, unique(STREET_NAME)], "MALVINA")
})

test_that("Works for repeated lat, lon", {
  skip_if_not_installed("data.table")
  library(data.table)
  NN <- 10L
  out <- revgeocode(rep(-37.8006, NN), rep(144.9618, NN))
  expect_equal(nrow(out), NN)
  expect_equal(out[, uniqueN(STREET_NAME)], 1L)
})
