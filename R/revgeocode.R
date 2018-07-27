#' Match Latitude-Longitude to Australian Address
#' @param lat,lon Coordinates. Negative \code{lat} entries correspond to parallels south of the equator.
#' @param topn How many addresses to return for each \code{lat}, \code{lon}.
#' @export revgeocode
#' @import data.table
#' @importFrom magrittr %>%
#' @importFrom hutils haversine_distance
#' @importFrom hutils set_cols_first
#' @importFrom hutils %notin%
#' @importFrom fastmatch %fin%
#' @examples
#' revgeocode(-37.8006, 144.9618)
#'
#'
revgeocode <- function(lat, lon, topn = 1) {
  stopifnot(length(lat) == length(lon))

  LAT_input <- LON_input <- NULL

  latkey <-
    setDT(list(LATITUDE = lat,
               LAT_input = lat,
               ordering = seq_along(lat))) %>%
    setkeyv("LATITUDE")

  lonkey <-
    setDT(list(LONGITUDE = lon,
               LON_input = lon)) %>%
    setkeyv("LONGITUDE")

  max_lat <- latkey[, last(LATITUDE)]
  min_lat <- latkey[, first(LATITUDE)]

  ADDRESS_DETAIL_ID__by__LATLON <- get_fst('ADDRESS_DETAIL_ID__by__LATLON')

  # Could make use of integer coordinate indexing
  curr_dt_auto_index <- getOption("datatable.auto.index")
  on.exit(options(datatable.auto.index = curr_dt_auto_index))
  options(datatable.auto.index = FALSE)
  if (between(max_lat %% 1, 0.11, 0.89) &&
      between(min_lat %% 1, 0.11, 0.89)) {
    the_lat_ints <- seq(as.integer(min_lat), as.integer(max_lat))
    if (length(the_lat_ints) == 1L) {
      close_lats <- ADDRESS_DETAIL_ID__by__LATLON[lat_int == the_lat_ints]
    } else {
      close_lats <- ADDRESS_DETAIL_ID__by__LATLON[lat_int %in% the_lat_ints]
    }
    lat_range <- c(min_lat, max_lat) + c(-0.1, 0.1)
    close_lats <- close_lats[LATITUDE %between% lat_range]
    setkeyv(close_lats, "LATITUDE")
  } else {
    close_lats <-
      ADDRESS_DETAIL_ID__by__LATLON %>%
      .[LATITUDE %between% (range(lat) + c(-0.1, 0.1))] %>%
      setkeyv("LATITUDE")
  }

  close_latlons <-
    latkey[close_lats, roll = 0.05, rollends = c(TRUE, TRUE), nomatch=0L] %>%
    setkeyv("LONGITUDE") %>%
    lonkey[., roll = 0.05, rollends = c(TRUE, TRUE), nomatch=0L]

  distance <- NULL

  latlons_by_dist <-
    close_latlons %>%
    .[complete.cases(.)] %>%
    .[, distance := haversine_distance(lat1 = LAT_input,
                                         lon1 = LON_input,
                                         lat2 = LATITUDE,
                                         lon2 = LONGITUDE)]
  setorderv(latlons_by_dist, c("ordering", "distance"))
  latlons_by_dist_top_n <-
    latlons_by_dist[, .(distance = distance[seq_len(min(.N, topn))],
                        ADDRESS_DETAIL_INTRNL_ID = ADDRESS_DETAIL_INTRNL_ID[seq_len(min(.N, topn))]),
                    keyby = "ordering"]
  street_addresses <- street_address(latlons_by_dist_top_n[["ADDRESS_DETAIL_INTRNL_ID"]])
  cbind(data.table(distance = latlons_by_dist_top_n[["distance"]],
                   ordering = latlons_by_dist_top_n[["ordering"]]),
        street_addresses)
}

street_address <- function(address_detail_pid) {
  input <-
    setDT(list(ADDRESS_DETAIL_INTRNL_ID = address_detail_pid)) %>%
    .[, order := seq_len(.N)] %>%
    setkeyv("ADDRESS_DETAIL_INTRNL_ID")

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
  inputs_street_addresses <-
    street_addresses[input,
                     list(order,
                          BUILDING_NAME,
                          LOT_NUMBER,
                          FLAT_NUMBER,
                          NUMBER_FIRST,
                          STREET_NAME,
                          STREET_TYPE_CODE,
                          POSTCODE,
                          ADDRESS_DETAIL_INTRNL_ID),
                     on = "ADDRESS_DETAIL_INTRNL_ID"]

  hutils::set_cols_first(inputs_street_addresses,
                         c("NUMBER_FIRST", "STREET_NAME", "STREET_TYPE_CODE", "POSTCODE")) %>%
    .[]
}


