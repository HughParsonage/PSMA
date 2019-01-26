library(data.table)
library(magrittr)
library(hutils)
attach(asNamespace("PSMA"))
library(hutilscpp)

seg_len <- 100L
DT <- fread("inst/extdata/213730.csv", select = c("bay_id", "lat", "lon"))
DT_ids <- DT$bay_id
n_DT_ids <- nrow(DT)
DT[, LATITUDE := round(lat, 10)]
DT[, LONGITUDE := round(lon, 10)]

lat_min <- DT[, min(lat)] - 0.01
lat_max <- DT[, max(lat)] + 0.01
lon_min <- DT[, min(lon)] - 0.01
lon_max <- DT[, max(lon)] + 0.01
bench::system_time({
nearby_add <- get_fst('ADDRESS_DETAIL_ID__by__LATLON')
nearby_add <- nearby_add[LATITUDE %between% c(lat_min, lat_max)]
nearby_add <- nearby_add[LONGITUDE %between% c(lon_min, lon_max)]
nearby_add[, address_LATITUDE := coalesce(LATITUDE, LATITUDE)]
nearby_add[, address_LONGITUDE := coalesce(LONGITUDE, LONGITUDE)]
nearby_add <- unique(nearby_add, by = c("LATITUDE", "LONGITUDE"))  ## This reduces computation by 10%!
})
bench::system_time(nearby_add[, L1_metric := address_LATITUDE - lat_min + address_LONGITUDE - lon_min])
bench::system_time(nearby_add[, L1_metric := L1_metric / (lat_max - lat_min + lon_max - lon_min)])

all_ids <- nearby_add$ADDRESS_DETAIL_INTRNL_ID

bench_time_DT <-
  bench::system_time({
    parking0 <- match_nrst_haversine(DT[["lat"]], DT[["lon"]],
                                   DT[["lat"]], DT[["lon"]],
                                   excl_self = TRUE,
                                   close_enough = 100)
  })

bench_time_DT_self <-
  bench::system_time({
    parking <- match_nrst_haversine(DT[["lat"]], DT[["lon"]],
                                   DT[["lat"]], DT[["lon"]],
                                   excl_self = TRUE)
  })

bench_time_1 <-
  bench::system_time({
    Addresses <- match_nrst_haversine(DT[["lat"]], DT[["lon"]],
                                     nearby_add[["LATITUDE"]],
                                     nearby_add[["LONGITUDE"]],
                                     # 1:15)
                                     all_ids,
                                     close_enough = 10)
  })

bench_time_1_nv <-
  bench::system_time({
    addresses_no_v <- match_nrst_haversine(DT[["lat"]], DT[["lon"]],
                                      nearby_add[["LATITUDE"]],
                                      nearby_add[["LONGITUDE"]],
                                      # 1:15)
                                      all_ids,
                                      .verify_box = FALSE,
                                      close_enough = 10)
  })
# process    real  # unitless before sin(x/2)^2
#  6.172s  6.184s
# process    real  # unitless after
# 10.953s 10.945s

bench_time_1_cartR <-
  bench::system_time({
    addresses_cartR <- match_nrst_haversine(DT[["lat"]], DT[["lon"]],
                                      nearby_add[["LATITUDE"]],
                                      nearby_add[["LONGITUDE"]],
                                      # 1:15)
                                      all_ids,
                                      cartesian_R = 0.0005, # about every 250m
                                      close_enough = 10)
  })
testthat::expect_equal(addresses, addresses_cartR)
# process    real
#  4.297s  4.312s

bench_time_3 <-
  bench::system_time({
    addresses3 <- match_nrst_haversine(DT[["lat"]], DT[["lon"]],
                                     nearby_add[["LATITUDE"]],
                                     nearby_add[["LONGITUDE"]],
                                     # 1:15)
                                     all_ids,
                                     close_enough = 3)
  })

testthat::expect_lt(as.double(bench_time_1[[2]]), 15)

bench_time_50 <-
  bench::system_time({
    addresses50 <- match_nrst_haversine(DT[["lat"]], DT[["lon"]],
                                         nearby_add[["LATITUDE"]],
                                         nearby_add[["LONGITUDE"]],
                                         # 1:15)
                                         all_ids,
                                         close_enough = 50)
  })
# process    real
#  3.562s  3.553s
# testthat::expect_equal(addresses, addresses50)

bench_time_100 <-
  bench::system_time({
    addresses100 <- match_nrst_haversine(DT[["lat"]], DT[["lon"]],
                                     nearby_add[["LATITUDE"]],
                                     nearby_add[["LONGITUDE"]],
                                     # 1:15)
                                     all_ids,
                                     close_enough = 100)
  })
# process    real
# 2.016s  2.015s

testthat::expect_lt(as.double(bench_time_100[[2]]), 5)

bench_time_25 <-
  bench::system_time({
    addresses25 <- match_nrst_haversine(DT[["lat"]], DT[["lon"]],
                                      nearby_add[["LATITUDE"]],
                                      nearby_add[["LONGITUDE"]],
                                      # 1:15)
                                      all_ids,
                                      close_enough = 25)
  })

# bench_time_5 <-
#   bench::system_time({
#     addresses5 <- match_nrst_haversine(DT[["lat"]], DT[["lon"]],
#                                        nearby_add[["LATITUDE"]],
#                                        nearby_add[["LONGITUDE"]],
#                                        # 1:15)
#                                        all_ids,
#                                        close_enough = 5)
#   })


distGeo <- function(lat1, lon1, lat2, lon2) {
  p1 <- matrix(c(lon1, lat1), ncol = 2)
  p2 <- matrix(c(lon2, lat2), ncol = 2)
  geosphere::distGeo(p1, p2)
}


