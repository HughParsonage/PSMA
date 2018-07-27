context("test-get_fst.R")

test_that("retrieves defaults", {
  library(data.table)
  options("PSMA_env" = new.env())
  expect_true(all(c("ADDRESS_DETAIL_INTRNL_ID", "LATITUDE", "LONGITUDE") %in%
                    names(get_fst("ADDRESS_DETAIL_ID__by__LATLON"))))
  expect_equal(indices(get_fst("STREET_ID_vs_ADDRESS_ID")),
               "POSTCODE")
  expect_true(all(c("ADDRESS_DETAIL_INTRNL_ID", "POSTCODE") %in%
                    names(get_fst("STREET_ID_vs_ADDRESS_ID"))))
})
