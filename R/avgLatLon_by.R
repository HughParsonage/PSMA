#' Summary coordinates by postcode and street
#' @description Tables with the average latitude and longitude of each postcode,
#' and each street within each postcode. Weighted by addresses, provided when
#' exact geocoding cannot identify an address (either because of incomplete or
#' faulty addresses).
#'
#' @return A keyed \code{data.table} with columns \code{avgLat} and \code{avgLon}.
#'
#' @export

avgLatLon_by_POSTCODE <- function() {
  .Assign("_avg_LatLon_by_POSTCODE", {
    ADDRESS_DETAIL_ID__by__LATLON <- get_fst("ADDRESS_DETAIL_ID__by__LATLON")
    STREET_ID_vs_ADDRESS_ID <- get_fst("STREET_ID_vs_ADDRESS_ID")

    lat <- lon <- NULL
    # In case we've already assigned elsewhere
    if (!hasName(STREET_ID_vs_ADDRESS_ID, "lat") ||
        !hasName(STREET_ID_vs_ADDRESS_ID, "lon")) {
      # Should be true:
      if (identical(ADDRESS_DETAIL_ID__by__LATLON$ADDRESS_DETAIL_INTRNL_ID,
                    STREET_ID_vs_ADDRESS_ID$ADDRESS_DETAIL_INTRNL_ID)) {
        STREET_ID_vs_ADDRESS_ID[, c("lat", "lon") := list(ADDRESS_DETAIL_ID__by__LATLON$LATITUDE,
                                                          ADDRESS_DETAIL_ID__by__LATLON$LONGITUDE)]
      } else {
        i.LATITUDE <- i.LONGITUDE <- NULL
        STREET_ID_vs_ADDRESS_ID[ADDRESS_DETAIL_ID__by__LATLON,
                                c("lat", "lon") := list(i.LATITUDE, i.LONGITUDE),
                                on = "ADDRESS_DETAIL_INTRNL_ID"]
      }
    }


    STREET_ID_vs_ADDRESS_ID[, .(avgLat = mean(lat),
                                avgLon = mean(lon)),
                            keyby = "POSTCODE"]
  })
}

avgLatLon_by_STREET_POSTCODE <- function() {
  .Assign("_avg_LatLon_by_STREET_POSTCODE", {
    ADDRESS_DETAIL_ID__by__LATLON <- get_fst("ADDRESS_DETAIL_ID__by__LATLON")
    STREET_ID_vs_ADDRESS_ID <- get_fst("STREET_ID_vs_ADDRESS_ID")
    STREET_LOCALITY_ID__STREET_NAME_STREET_TYPE_CODE <- get_fst("STREET_LOCALITY_ID__STREET_NAME_STREET_TYPE_CODE")

    lat <- lon <- NULL
    # In case we've already assigned elsewhere
    if (!hasName(STREET_ID_vs_ADDRESS_ID, "lat") ||
        !hasName(STREET_ID_vs_ADDRESS_ID, "lon")) {
      # Should be true:
      if (identical(ADDRESS_DETAIL_ID__by__LATLON$ADDRESS_DETAIL_INTRNL_ID,
                    STREET_ID_vs_ADDRESS_ID$ADDRESS_DETAIL_INTRNL_ID)) {
        STREET_ID_vs_ADDRESS_ID[, c("lat", "lon") := list(ADDRESS_DETAIL_ID__by__LATLON$LATITUDE,
                                                          ADDRESS_DETAIL_ID__by__LATLON$LONGITUDE)]
      } else {
        i.LATITUDE <- i.LONGITUDE <- NULL
        STREET_ID_vs_ADDRESS_ID[ADDRESS_DETAIL_ID__by__LATLON,
                                c("lat", "lon") := list(i.LATITUDE, i.LONGITUDE),
                                on = "ADDRESS_DETAIL_INTRNL_ID"]
      }
    }


    ans <-
      STREET_ID_vs_ADDRESS_ID[, .(avgLat = mean(lat),
                                  avgLon = mean(lon)),
                              keyby = c("STREET_LOCALITY_INTRNL_ID", "POSTCODE")]
    ans[STREET_LOCALITY_ID__STREET_NAME_STREET_TYPE_CODE, on = "STREET_LOCALITY_INTRNL_ID", nomatch = 0L]
  })
}

