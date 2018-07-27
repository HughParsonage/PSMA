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
    data.table(LATITUDE = lat,
               LAT_input = lat,
               ordering = seq_along(lat))
  setkeyv(latkey, "LATITUDE")

  lonkey <-
    data.table(LONGITUDE = lon,
               LON_input = lon,
               ordering = seq_along(lon))
  setkeyv(lonkey, c("ordering", "LONGITUDE"))

  close_lats <-
    get_fst('ADDRESS_DETAIL_ID__by__LATLON') %>%
    .[LATITUDE %between% (range(lat) + c(-0.1, 0.1))] %>%
    setkeyv("LATITUDE") %>%
    latkey[., roll = 0.05, rollends = c(TRUE, TRUE), nomatch=0L] %>%
    setkeyv(c("ordering", "LONGITUDE"))

  close_latlons <-
    close_lats %>%
    setkeyv(c("ordering", "LONGITUDE")) %>%
    lonkey[., roll = 0.05, rollends = c(TRUE, TRUE), nomatch=0L]

  distance <- NULL

  latlons_by_dist <-
    close_latlons %>%
    .[complete.cases(.)] %>%
    .[, distance := haversine_distance(lat1 = LAT_input,
                                       lon1 = LON_input,
                                       lat2 = LATITUDE,
                                       lon2 = LONGITUDE)]
  setkeyv(latlons_by_dist, c("ordering", "distance"))
  latlons_by_dist_top_n <-
    latlons_by_dist[, .(distance = distance[seq_len(min(.N, topn))],
                        ADDRESS_DETAIL_INTRNL_ID = ADDRESS_DETAIL_INTRNL_ID[seq_len(min(.N, topn))]),
                    keyby = "ordering"]
  street_addresses <- street_address(latlons_by_dist_top_n[["ADDRESS_DETAIL_INTRNL_ID"]])
  street_addresses[latlons_by_dist_top_n,
                   on = c("ADDRESS_DETAIL_INTRNL_ID")] %>%
    set_cols_first(c("ordering", "order")) %>%
    .[]
}

street_address <- function(address_detail_pid) {
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
  inputs_street_addresses <-
    street_addresses[input,
                     list(BUILDING_NAME,
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


