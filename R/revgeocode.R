#' Match Latitude-Longitude to Australian Address
#' @param lat,lon Coordinates. Negative \code{lat} entries correspond to parallels south of the equator.
#' @param topn How many addresses to return for each \code{lat}, \code{lon}.
#' @export revgeocode
#' @import data.table
#' @importFrom hutilscpp match_nrst_haversine
#' @importFrom magrittr %>%
#' @importFrom hutils haversine_distance
#' @importFrom hutils set_cols_first
#' @importFrom hutils %notin%
#' @importFrom fastmatch %fin%
#' @examples
#' revgeocode(-37.8006, 144.9618)
#'
#'
revgeocode <- function(lat, lon, id = seq_along(lat),
                       topn = 1,
                       l_infty_e = 0.25,
                       close_enough = "10m") {
  if (!missing(topn)) {
    message("`topn` has been disabled.")
    topn <- 1L
  }
  input <- data.table(lat, lon, id)
  min_lat <- min(lat)
  min_lon <- min(lon)
  max_lat <- max(lat)
  max_lon <- max(lon)


  stopifnot(length(close_enough) == 1L)

  if (is.character(close_enough)) {
    if (endsWith(close_enough, "km")) {
      dist0 <- sub("\\s*km$", "", close_enough)
      # use as.double here and as.numeric later to separate warning msgs
      dist0 <- as.double(dist0) * 1000
    } else if (endsWith(close_enough, "m")) {
      dist0 <- sub("\\s*m$", "", close_enough)
      dist0 <- as.numeric(dist0)
    }
    if (anyNA(dist0) || !is.numeric(dist0)) {
      stop("`close_enough = ", close_enough, "`, not interpretable as a distance. ",
           "Use a string of the form\n\t^[0-9]+\\.?[0-9]*\\sk?m$")
    }
  }

  dist0_km <- dist0 / 1000

  round_latlon <- 0
  # Determine at runtime how many digits can be rounded safely
  digs2round <- 1L
  while (digs2round < 10L && haversine_distance(0, 0, 0, 9 * 10^{-digs2round}) > {dist0_km / 2}) {
    digs2round <- digs2round + 1L
  }

  # round vs my_trunc : 300 ms vs 40 ms
  # my_trunc <- function(x, m) {
  #   trunc(x * 10^m) / 10^m
  # }

  nearby_addresses <-
    get_fst('ADDRESS_DETAIL_ID__by__LATLON') %>%
    .[LATITUDE %between% c(min_lat - l_infty_e, max_lat + l_infty_e)] %>%
    .[LONGITUDE %between% c(min_lon - l_infty_e, max_lon + l_infty_e)] %>%

    # I thought the following would be worth it, but only eliminates around 1%
    # .[, c("LATITUDE", "LONGITUDE") := lapply(.SD, my_trunc, digs2round)] %>%


    unique(by = c("LATITUDE", "LONGITUDE"))  ## many addresses occupy same coordinates

  # Had problems matching street_address output to the columns requested
  # use select and () := rather than hard-naming the columns inside DT[...]
  address_cols <-
    c("NUMBER_FIRST", "STREET_NAME", "STREET_TYPE_CODE", "POSTCODE",
      "BUILDING_NAME", "LOT_NUMBER", "FLAT_NUMBER",
      "ADDRESS_DETAIL_INTRNL_ID")

  input[, c("ADDRESS_DETAIL_INTRNL_ID", "dist_km") := match_nrst_haversine(lat,
                                                                           lon,
                                                                           .subset2(nearby_addresses, "LATITUDE"),
                                                                           .subset2(nearby_addresses, "LONGITUDE"),
                                                                           Table = .subset2(nearby_addresses, "ADDRESS_DETAIL_INTRNL_ID"),
                                                                           close_enough = close_enough)] %>%
    .[, c(address_cols) := street_address(ADDRESS_DETAIL_INTRNL_ID, select = address_cols)] %>%
    .[, "ordering" := id] %>%
    .[]
}
#
street_address <- function(address_detail_pid, select = NULL) {
  # Given an ADDRESS_DETAIL_INTRNL_ID, create a table
  input <- setDT(list(ADDRESS_DETAIL_INTRNL_ID = address_detail_pid))

  STREET_ID_vs_ADDRESS_ID <-
    get_fst("STREET_ID_vs_ADDRESS_ID")


  STREET_LOCALITY_ID__STREET_NAME_STREET_TYPE_CODE <-
    get_fst("STREET_LOCALITY_ID__STREET_NAME_STREET_TYPE_CODE")

  street_pid_these_address_pid <-
    STREET_ID_vs_ADDRESS_ID[input,
                            list(ADDRESS_DETAIL_INTRNL_ID,
                                 BUILDING_NAME,
                                 LOT_NUMBER,
                                 FLAT_NUMBER,
                                 NUMBER_FIRST,
                                 POSTCODE,
                                 STREET_LOCALITY_INTRNL_ID),
                            on = "ADDRESS_DETAIL_INTRNL_ID"]

  street_addresses <-
    STREET_LOCALITY_ID__STREET_NAME_STREET_TYPE_CODE[street_pid_these_address_pid,
                                                     on = "STREET_LOCALITY_INTRNL_ID"]

  out <-
    hutils::set_cols_first(street_addresses,
                           c("NUMBER_FIRST", "STREET_NAME", "STREET_TYPE_CODE", "POSTCODE"))
  if (!is.null(select)) {
    setcolorder(out, select)
    return(hutils::selector(out, cols = select, shallow = TRUE))
  }
  out
}
#
# revgeocode_DT <- function(DT) {
#   stopifnot(hasName(DT, "id"),
#             hasName(DT, "lon"),
#             hasName(DT, "lat"),
#             length(DT) == 3L)
#
#   DT[, LATITUDE := round(lat, 10)]
#   DT[, LONGITUDE := round(lon, 10)]
#
#   lat_min <- DT[, min(lat)] - 0.01
#   lat_max <- DT[, max(lat)] + 0.01
#   lon_min <- DT[, min(lon)] - 0.01
#   lon_max <- DT[, max(lon)] + 0.01
#
#   nearby_add <- copy(get_fst('ADDRESS_DETAIL_ID__by__LATLON'))
#   nearby_add[, address_LATITUDE := coalesce(LATITUDE, LATITUDE)]
#   nearby_add[, address_LONGITUDE := coalesce(LONGITUDE, LONGITUDE)]
#   nearby_add <- nearby_add[LATITUDE %between% c(lat_min, lat_max)]
#   nearby_add <- nearby_add[LONGITUDE %between% c(lon_min, lon_max)]
#
#   bench::system_time({
#   addresses <- match_min_Haversine(DT[["lat"]], DT[["lon"]],
#                                    nearby_add[["LATITUDE"]],
#                                    nearby_add[["LONGITUDE"]],
#                                    tabl = nearby_add[["ADDRESS_DETAIL_INTRNL_ID"]])
#   })
#
#
#   nearby_add[DT,
#      .(bay_id, address_LATITUDE, address_LONGITUDE, lat, lon),
#      by = .EACHI,
#      on = "LATITUDE", roll = Inf, rollends = c(TRUE, TRUE), allow.cartesian = TRUE] %>%
#     .[, distance := haversine_distance(lat, lon, address_LATITUDE, address_LONGITUDE)] %>%
#     setkey(distance) %>%
#     unique(by = "bay_id")
#
#   output <- nearby_add[DT, on = "LATITUDE", roll = 0.05, rollends = c(TRUE, TRUE)]
#   output2 <- output[DT, on = "LONGITUDE", roll = 0.05, rollends = c(TRUE, TRUE)]
#
#   output2[, distance := haversine_distance(lat, lon, i.LATITUDE, i.LONGITUDE)]
# }
#
# revgeocode_single <- function(DT1, address_tbl) {
#   stopifnot(nrow(DT1) == 1L)
#   nrby_lat <-
#     DT1[address_tbl, on = "LATITUDE", roll = Inf, rollends = c(TRUE, TRUE)] %>%
#     .[, distance := haversine_distance(DT1[["lat"]], DT1[["lon"]],
#                                        address_LATITUDE,
#                                        address_LONGITUDE)]
#
#
# }
#
# revgeocode_cj <- function(DT) {
#   all_ids <- nearby_add$ADDRESS_DETAIL_INTRNL_ID
#   DT_ids <- DT[order(LATITUDE)]$bay_id
#
#   n_DT_ids <- length(DT_ids)
#   seg_len <- 50
#
#   nrst_with_ii_seg <- function(ii) {
#     the_seq <- seq(seg_len * (ii - 1) + 1,
#                    min(seg_len * ii, n_DT_ids),
#                    by = 1L)
#     cat("seq\t", paste0(range(the_seq), collapse = ":"),
#         "\t", as.character(Sys.time(), "%H:%M:%OS2"), "\n")
#     the_ids <- DT_ids[the_seq]
#     o <- CJ(ADDRESS_DETAIL_INTRNL_ID = all_ids,
#             bay_id = the_ids)
#     o <- o[nearby_add, on = "ADDRESS_DETAIL_INTRNL_ID", nomatch=0L]
#     o <- o[DT, on = "bay_id", nomatch=0L]
#     o[, distance := haversine_distance(LATITUDE, LONGITUDE, i.LATITUDE, i.LONGITUDE)]
#     setkey(o, distance)
#     unique(o, by = "bay_id")
#   }
#
#   out <-
#     lapply(seq_len(ceiling(n_DT_ids / seg_len)), function(ii) {
#       the_seq <- seq(seg_len * (ii - 1) + 1,
#                      min(seg_len * ii, n_DT_ids),
#                      by = 1L)
#       cat("seq\t", paste0(range(the_seq), collapse = ":"),
#           "\t", as.character(Sys.time(), "%H:%M:%OS2"), "\n")
#       the_ids <- DT_ids[the_seq]
#
#       o <- CJ(bay_id = the_ids, ADDRESS_DETAIL_INTRNL_ID = all_ids)
#
#       o <- o[DT, on = "bay_id", nomatch=0L]
#       o <- selector(o,
#                     cols = c("ADDRESS_DETAIL_INTRNL_ID", "bay_id", "LATITUDE", "LONGITUDE"),
#                     shallow = TRUE)
#       o <- o[nearby_add, on = "ADDRESS_DETAIL_INTRNL_ID", nomatch=0L]
#       o <- selector(o,
#                     cols = c("LATITUDE",
#                              "LONGITUDE",
#                              "i.LATITUDE",
#                              "i.LONGITUDE",
#                              "bay_id",
#                              "ADDRESS_DETAIL_INTRNL_ID"),
#                     shallow = TRUE)
#       o[, distance := haversine_distance(LATITUDE, LONGITUDE, i.LATITUDE, i.LONGITUDE)]
#       o <- selector(o,
#                     cols = c("bay_id", "ADDRESS_DETAIL_INTRNL_ID", "distance"),
#                     shallow = TRUE)
#       setkey(o, distance)
#       unique(o, by = "bay_id")
#     })
#   out <- rbindlist(out)
#
#   the_cj <- CJ(ADDRESS_DETAIL_INTRNL_ID = all_ids,
#                bay_id = DT_ids) %>%
#     .[nearby_add, on = "ADDRESS_DETAIL_INTRNL_ID"] %>%
#     .[DT, on = "bay_id"] %>%
#     .[, distance := haversine_distance(LATITUDE, LONGITUDE, i.LATITUDE, i.LONGITUDE)] %>%
#     setkey(distance) %>%
#     unique(by = "bay_id")
# }
#
#
# dist_geo <- function(lat1, lon1, lat2, lon2) {
#   p1 <- matrix(c(lon1, lat1), byrow = FALSE, ncol = 2L)
#   p2 <- matrix(c(lon2, lat2), byrow = FALSE, ncol = 2L)
#   geosphere::distGeo(p1, p2)
# }
#
#
# profvis::profvis({
# haversine_distance_sep <- function(lat1, lon1, lat2, lon2,
#                                    unitless = FALSE){
#   # to radians
#   lat1 <- lat1 * pi / 180
#   lat2 <- lat2 * pi / 180
#   lon1 <- lon1 * pi / 180
#   lon2 <- lon2 * pi / 180
#
#   delta_lat <- abs(lat1 - lat2) / 2
#   delta_lon <- abs(lon1 - lon2) / 2
#
#
#   a <- sin(delta_lat)
#   b <- sin(delta_lon)
#   cos_lat1 <- cos(lat1)
#   cos_lat2 <- cos(lat2)
#
#   X <- a ^2
#   Y <- cos_lat1 * cos_lat2 * b ^ 2
#   Z <- X + Y
#   if (unitless) {
#     return(Z)
#   }
#   Z <- sqrt(Z)
#   O <- asin(Z)
#   6371 * 2 * O
# }
# o[, haversine_distance_sep(LATITUDE, LONGITUDE, i.LATITUDE, i.LONGITUDE)]})
#
#
#
#
