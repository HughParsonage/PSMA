#' Geocode address
#' @param flat_number,number_first,street_name,street_type,postcode The address to be geocoded. Argument \code{postcode} is mandatory.
#' @param building_name If \code{street_name} is not provided, searches for building names in that
#' \code{postcode}
#' @param attempt_decode_street_abbrev Should abbreviated street types be decoded during the geocoding attempt?
#' @examples
#' geocode(flat_number = NA_character_,
#'         number_first = 8L,
#'         street_name = "MALVINA",
#'         street_type = "PLACE",
#'         postcode = 3053L)
#'
#' load(system.file("extdata", "bne_addresses.rda", package = "PSMA"))
#' with(bne_addresses,
#'     geocode(flat_number = NA_character_,
#'             number_first = house_number,
#'             street_name = street_name,
#'             street_type = street_type,
#'             postcode = postcode))
#'
#' @return A \code{data.table} of three columns and the same number of rows as the longest argument.
#'
#'
#' @export



geocode <- function(flat_number = NULL,
                    number_first = NULL,
                    building_name = NULL,
                    street_name = NULL,
                    street_type = NULL,
                    postcode,
                    attempt_decode_street_abbrev = FALSE) {

  # Put here so the subsequent tryCatch doesn't
  # look confusing.
  if (missing(postcode)) {
    stop('argument "postcode" is missing, with no default')
  }

  tryCatch(postcode <- as.integer(postcode),
           warning = function(e) {
             cat(e$m)
             stop("Postcode must be an integer (or coercible to such).")
           })


  if (is.null(street_name)) {
    if (!is.null(number_first) || !is.null(flat_number)) {
      warning("street_name not given, but number_first and flat_number ",
              "are provided and will not be used.")
    }

    if (!is.null(building_name)) {
      input <- data.table(BUILDING_NAME = toupper(building_name),
                          POSTCODE = as.integer(postcode))
      input[, ordering := .I]
    } else {
      input <- setDT(list(POSTCODE = as.integer(postcode),
                          ordering = seq_along(postcode)))
    }
  } else {
    if (!is.null(street_type)) {
      flat_number_null <- is.null(flat_number)
      if (flat_number_null) {
        flat_number <-  NA_integer_
      }
      if (is.null(number_first)) number_first <- NA_integer_

      tryCatch(flat_number <- as.integer(flat_number),
               warning = function(e) {
                 cat(e$m)
                 stop("flat_number must be an integer (or coercible to such).")
               })

      tryCatch(number_first <- as.integer(number_first),
               warning = function(e) {
                 cat(e$m)
                 stop("number_first must be an integer (or coercible to such).")
               })

      # STREET_LOCALITY_ID__STREET_NAME_STREET_TYPE_CODE %$% unique(STREET_TYPE_CODE) %>% sort %>% dput
      permitted_street_cds <-
        c("ACCESS", "ACRE", "ALLEY", "AMBLE", "APPROACH", "ARCADE", "ARTERIAL",
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

        if (attempt_decode_street_abbrev) {
          STREET_TYPE[which_bad_STREET_TYPES] <-
            data.table(street_abbrev = STREET_TYPE[which_bad_STREET_TYPES]) %>%
            street_type_decoder[., on = "street_abbrev"] %>%
            # If match failed, fall-back
            # .[, street_type := coalesce(street_type, street_abbrev)] %>%
            .[["street_type"]]

        } else {

          stop("street_type entry ", which_bad_STREET_TYPES,
               " (",
               paste0(head(unique(STREET_TYPE[which_bad_STREET_TYPES])),
                      collapse = "\n"),
               if (uniqueN(STREET_TYPE[which_bad_STREET_TYPES]) > 5) {
                 " (list truncated)"
               },
               ") ",
               " was not a permitted street type.")

        }
      }

      get_fst <- function(x) {
        file.fst <- system.file("extdata", "STREET_ID_vs_ADDRESS_ID.fst")
        fst::read_fst()
      }

      psma_env <- getOption("PSMA_env", new.env())

      STREET_ID_vs_ADDRESS_ID <-
        if (exists("STREET_ID_vs_ADDRESS_ID", envir = psma_env)) {
          get("STREET_ID_vs_ADDRESS_ID",
                 envir = psma_env)
        } else {
          STREET_ID_vs_ADDRESS_ID <-
            fst::read_fst(system.file("extdata", "STREET_ID_vs_ADDRESS_ID.fst",
                                      package = "PSMA"),
                          as.data.table = TRUE,
                          columns = c("ADDRESS_DETAIL_INTRNL_ID",
                                      "STREET_LOCALITY_INTRNL_ID",
                                      "FLAT_NUMBER",
                                      "NUMBER_FIRST",
                                      "POSTCODE"))
          setindexv(STREET_ID_vs_ADDRESS_ID, "POSTCODE")
          assign("STREET_ID_vs_ADDRESS_ID",
                 value = STREET_ID_vs_ADDRESS_ID,
                 envir = psma_env)
        }

      STREET_LOCALITY_ID__STREET_NAME_STREET_TYPE_CODE <-
        fst::read_fst(system.file("extdata", "STREET_LOCALITY_ID__STREET_NAME_STREET_TYPE_CODE.fst",
                                  package = "PSMA"),
                      as.data.table = TRUE)
      ADDRESS_DETAIL_ID__by__LATLON <-
        if (file.exists(ADDRESS_DETAIL_ID__by__LATLON.fst <-
                        system.file("extdata", "ADDRESS_DETAIL_ID__by__LATLON.fst",
                                    package = "PSMA"))) {
          fst::read_fst(ADDRESS_DETAIL_ID__by__LATLON.fst,
                        as.data.table = TRUE)
        } else {
          address3 <- fst::read_fst(system.file("extdata", "address2.fst",
                                    package = "PSMA"),
                        as.data.table = TRUE)
          address3[, LATITUDE := lat_int + lat_rem / 10^7]
          address3[, LONGITUDE := lon_int + lon_rem / 10^7]
          fst::write_fst(address3,
                         path = file.path(system.file(package = "PSMA"),
                                          "extdata",
                                          "ADDRESS_DETAIL_ID__by__LATLON.fst"))
          address3
        }

      street_addresses_in_postcodes <-
        STREET_ID_vs_ADDRESS_ID %>%
        .[POSTCODE %in% postcode] %>%
        .[STREET_LOCALITY_ID__STREET_NAME_STREET_TYPE_CODE,
          on = "STREET_LOCALITY_INTRNL_ID",
          nomatch=0L] %>%
        .[STREET_NAME %in% toupper(street_name)] %>%
        setkeyv(c("POSTCODE",
                  "STREET_NAME",
                  "STREET_TYPE_CODE",
                  "NUMBER_FIRST"))

      input <-
        data.table(FLAT_NUMBER = flat_number,
                   NUMBER_FIRST = number_first,
                   STREET_NAME = toupper(street_name),
                   STREET_TYPE_CODE = STREET_TYPE,
                   POSTCODE = postcode) %>%
        .[, ordering := .I] %>%
        setkeyv(c("POSTCODE",
                  "STREET_NAME",
                  "STREET_TYPE_CODE",
                  "NUMBER_FIRST"))

      addresses_by_ADDRESS_DETAIL_INTRNL_ID <-
        street_addresses_in_postcodes[input,
                                      on = c("POSTCODE",
                                             "STREET_NAME",
                                             "STREET_TYPE_CODE",
                                             "NUMBER_FIRST"),
                                      nomatch=0L,
                                      roll="nearest",
                                      # If multiple places match, must return
                                      # only one. TODO : flat_number
                                      mult='first'] %>%
        setkeyv("ADDRESS_DETAIL_INTRNL_ID") %>%
        .[input, on = "ordering", mult='first']

      out <- ADDRESS_DETAIL_ID__by__LATLON[addresses_by_ADDRESS_DETAIL_INTRNL_ID,
                                           list(ordering, LATITUDE, LONGITUDE),
                                           on = "ADDRESS_DETAIL_INTRNL_ID"]

    }
  }

  setorderv(out, "ordering")
  out[]
}



