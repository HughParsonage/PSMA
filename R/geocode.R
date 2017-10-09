#' Geocode address
#' @param flat_number,number_first,street_name,street_type,postcode The address to be geocoded.
#' @param building_name If \code{street_name} is not provided, searches for building names in that
#' \code{postcode}
#' @param postcode Mandatory. Postcode as an integer in which the address is located.
#' @export



geocode <- function(flat_number = NULL,
                    number_first = NULL,
                    building_name = NULL,
                    street_name = NULL,
                    street_type = NULL,
                    postcode) {
  stopifnot(is.integer(postcode))


  if (is.null(street_name)) {
    if (!is.null(number_first) || !is.null(flat_number)) {
      warning("street_name not given, but number_first and flat_number ",
              "are provided and will not be used.")
    }

    if (!is.null(building_name)) {
      input <- data.table(BUILDING_NAME = toupper(building_name),
                          POSTCODE = as.integer(POSTCODE))
      intput[, ordering := .I]
    } else {
      input <- data.table(POSTCODE = as.integer(POSTCODE),
                          ordering = seq_along(POSTCODE))
    }
  } else {
    if (!is.null(street_type)) {
      # STREET_LOCALITY_ID__STREET_NAME_STREET_TYPE_CODE %$% unique(STREET_TYPE_CODE) %>% sort %>% dput
      permitted_street_cds <- c("ACCESS", "ACRE", "ALLEY", "AMBLE", "APPROACH", "ARCADE", "ARTERIAL",
                                "AVENUE", "BANAN", "BANK", "BAY", "BEACH", "BEND", "BOARDWALK",
                                "BOULEVARD", "BOULEVARDE", "BOWL", "BRACE", "BRAE", "BRANCH",
                                "BREAK", "BRIDGE", "BROADWAY", "BROW", "BUSWAY", "BYPASS", "BYWAY",
                                "CAUSEWAY", "CENTRE", "CENTREWAY", "CHASE", "CIRCLE", "CIRCUIT",
                                "CIRCUS", "CLOSE", "CLUSTER", "COMMON", "COMMONS", "CONCORD",
                                "CONCOURSE", "CONNECTION", "COPSE", "CORNER", "CORSO", "COURSE",
                                "COURT", "COURTYARD", "COVE", "CRESCENT", "CREST", "CRIEF", "CROSS",
                                "CROSSING", "CRUISEWAY", "CUL-DE-SAC", "CUTTING", "DALE", "DASH",
                                "DELL", "DENE", "DEVIATION", "DIP", "DISTRIBUTOR", "DIVIDE",
                                "DOCK", "DOMAIN", "DOWN", "DOWNS", "DRIVE", "DRIVEWAY", "EASEMENT",
                                "EAST", "EDGE", "ELBOW", "END", "ENTRANCE", "ESPLANADE", "ESTATE",
                                "EXPRESSWAY", "EXTENSION", "FAIRWAY", "FIREBREAK", "FIRELINE",
                                "FIRETRACK", "FIRETRAIL", "FLAT", "FLATS", "FOLLOW", "FORD",
                                "FORESHORE", "FORK", "FORMATION", "FREEWAY", "FRONTAGE", "GAP",
                                "GARDEN", "GARDENS", "GATE", "GATEWAY", "GLADE", "GLEN", "GRANGE",
                                "GREEN", "GROVE", "GULLY", "HARBOUR", "HAVEN", "HEATH", "HEIGHTS",
                                "HIGHWAY", "HIKE", "HILL", "HILLS", "HOLLOW", "HUB", "INTERCHANGE",
                                "ISLAND", "JUNCTION", "KEY", "KEYS", "LADDER", "LANDING", "LANE",
                                "LANEWAY", "LINE", "LINK", "LOOKOUT", "LOOP", "LYNNE", "MALL",
                                "MANOR", "MART", "MAZE", "MEAD", "MEANDER", "MEWS", "MILE", "MOTORWAY",
                                "NOOK", "NORTH", "OUTLET", "OUTLOOK", "PALMS", "PARADE", "PARK",
                                "PARKWAY", "PASS", "PASSAGE", "PATH", "PATHWAY", "PLACE", "PLAZA",
                                "POCKET", "POINT", "PORT", "PRECINCT", "PROMENADE", "PURSUIT",
                                "QUADRANT", "QUAY", "QUAYS", "RAMBLE", "RAMP", "RANGE", "REACH",
                                "RESERVE", "REST", "RETREAT", "RETURN", "RIDE", "RIDGE", "RISE",
                                "RISING", "RIVER", "ROAD", "ROADS", "ROADWAY", "ROTARY", "ROUND",
                                "ROUTE", "ROW", "RUN", "SERVICEWAY", "SKYLINE", "SLOPE", "SOUTH",
                                "SPUR", "SQUARE", "STEPS", "STRAIGHT", "STRAIT", "STREET", "STRIP",
                                "SUBWAY", "TARN", "TERRACE", "THROUGHWAY", "TOP", "TOR", "TRACK",
                                "TRAIL", "TRAMWAY", "TRAVERSE", "TRUNKWAY", "TUNNEL", "TURN",
                                "TWIST", "UNDERPASS", "VALE", "VALLEY", "VIEW", "VIEWS", "VILLA",
                                "VILLAGE", "VISTA", "WALK", "WALKWAY", "WATERS", "WATERWAY",
                                "WAY", "WEST", "WHARF", "WOODS", "WYND", "YARD")

      STREET_TYPE <- toupper(street_type)
      if (any(STREET_TYPE %notin% permitted_street_cds)) {
        # TODO: Assume the first letter is correct:
        which_bad_STREET_TYPES <- which(STREET_TYPE %notin% permitted_street_cds)

        stop("street_type entry ", which_bad_STREET_TYPES,
             " (", street_type[which_bad_STREET_TYPES], ") ",
             " was not a permitted street type.")


      }


      street_addresses_in_postcodes <-
        STREET_ID_vs_ADDRESS_ID %>%
        .[POSTCODE %fin% postcode]

      input <-
        data.table(FLAT_NUMBER = flat_number,
                   NUMBER_FIRST = number_first,
                   STREET_NAME = toupper(street_name),
                   STREET_TYPE_CODE = STREET_TYPE,
                   POSTCODE = postcode) %>%
        .[, ordering := .I]

      input_by_STREET_CODE <-
        STREET_LOCALITY_ID__STREET_NAME_STREET_TYPE_CODE[input,
                                                          on = c("STREET_NAME",
                                                                 "STREET_TYPE_CODE")]

      addresses_by_ADDRESS_DETAIL_INTRNL_ID <-
        street_addresses_in_postcodes[input_by_STREET_CODE,
                                      on = c("NUMBER_FIRST", "STREET_LOCALITY_INTRNL_ID", "POSTCODE"),
                                      nomatch=0L] %>%
        setkeyv("ADDRESS_DETAIL_INTRNL_ID")

      out <- ADDRESS_DETAIL_ID__by__LATLON[addresses_by_ADDRESS_DETAIL_INTRNL_ID,
                                           j = list(ordering, LATITUDE, LONGITUDE),
                                           on = "ADDRESS_DETAIL_INTRNL_ID"]



    }
  }

  out


}
