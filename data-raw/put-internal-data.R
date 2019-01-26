library(PSMA)
library(hutilscpp)
address2 <- PSMA:::get_fst()

address2_lon_range <- address2[, range_rcpp(LONGITUDE)]
address2_lat_range <- address2[, range_rcpp(LATITUDE)]
L1_20 <- PSMA:::author_rstar_pages(PSMA:::get_fst("ADDRESS_DETAIL_ID__by__LATLON"),
                                   LATITUDE,
                                   LONGITUDE)
median_xbreaks13 <- 5852L


usethis::use_data(address2_lat_range,
                  address2_lon_range,
                  median_xbreaks13,
                  L1_20,
                  internal = TRUE,
                  overwrite = TRUE)




