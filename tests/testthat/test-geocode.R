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

test_that("Multi-length", {
  input <-
    list(number_first = c(186L, 30L, 2L, 2L, 36L),
         street_name = c("SUTHERLAND", "THRELFALL", "FREEMAN", "LUGANO", "PARRAMATTA"),
         postcode = c(2021L, 2122L, 2067L, 2230L, 2230L),
         street_type = c("STREET", "ST", "RD", "AVE", "ST"))

  out <-
    geocode(number_first = input$number_first,
            street_name = input$street_name,
            postcode = input$postcode,
            street_type = input$street_type,
            attempt_decode_street_abbrev = TRUE)
  expect_equal(nrow(out), 5)
  expect_equal(out$ordering, seq_along(out$ordering))
})
