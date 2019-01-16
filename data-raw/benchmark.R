library(data.table)
library(magrittr)
library(hutils)
attach(asNamespace("PSMA"))
library(hutilscpp)

seg_len <- 100L
DT <- fread("~/MelbourneParking2019/data-raw/2019/1/10/213730.csv", select = c("bay_id", "lat", "lon"))
DT_ids <- DT$bay_id
n_DT_ids <- nrow(DT)
DT[, LATITUDE := round(lat, 10)]
DT[, LONGITUDE := round(lon, 10)]

lat_min <- DT[, min(lat)] - 0.01
lat_max <- DT[, max(lat)] + 0.01
lon_min <- DT[, min(lon)] - 0.01
lon_max <- DT[, max(lon)] + 0.01
bench::system_time({
nearby_add <- copy(get_fst('ADDRESS_DETAIL_ID__by__LATLON'))
nearby_add <- nearby_add[LATITUDE %between% c(lat_min, lat_max)]
nearby_add <- nearby_add[LONGITUDE %between% c(lon_min, lon_max)]
nearby_add[, address_LATITUDE := coalesce(LATITUDE, LATITUDE)]
nearby_add[, address_LONGITUDE := coalesce(LONGITUDE, LONGITUDE)]
nearby_add <- unique(nearby_add, by = c("LATITUDE", "LONGITUDE"))  ## This reduces computation by 10%!
})
all_ids <- nearby_add$ADDRESS_DETAIL_INTRNL_ID

bench_time_DT <-
  bench::system_time({
    parking0 <- match_nrst_haversine(DT[["lat"]], DT[["lon"]],
                                   DT[["lat"]], DT[["lon"]],
                                   0L,
                                   excl_self = TRUE,
                                   close_enough = 100)
  })

bench_time_DT_self <-
  bench::system_time({
    parking <- match_nrst_haversine(DT[["lat"]], DT[["lon"]],
                                   DT[["lat"]], DT[["lon"]],
                                   0L,
                                   excl_self = TRUE)
  })

bench_time_1 <-
  bench::system_time({
    addresses <- match_nrst_haversine(DT[["lat"]], DT[["lon"]],
                                     nearby_add[["LATITUDE"]],
                                     nearby_add[["LONGITUDE"]],
                                     # 1:15)
                                     all_ids,
                                     close_enough = 10)
  })
# process    real
# 10.953s 10.945s

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

bench_time_100 <-
  bench::system_time({
    addresses100 <- match_nrst_haversine(DT[["lat"]], DT[["lon"]],
                                     nearby_add[["LATITUDE"]],
                                     nearby_add[["LONGITUDE"]],
                                     # 1:15)
                                     # all_ids,
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





