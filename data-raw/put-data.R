library(data.table)

ADDRESS_DETAIL_PID__by__LATLON <-
  dir(pattern = "_ADDRESS_DEFAULT_GEOCODE_psv",
      recursive = TRUE,
      full.names = TRUE,
      path = "~/Data/PSMA-Geocoded-Address-2017/") %>%
  lapply(fread,
         na.strings = "",
         select = c("ADDRESS_DETAIL_PID",
                    "LATITUDE",
                    "LONGITUDE"),
         key = "ADDRESS_DETAIL_PID") %>%
  rbindlist %>%
  setkeyv("ADDRESS_DETAIL_PID")

STREET_PID_vs_ADDRESS_PID <-
  dir(pattern = "_ADDRESS_DETAIL_psv.psv$",
      path = "~/Data/PSMA-Geocoded-Address-2017/",
      recursive = TRUE,
      full.names = TRUE) %>%
  lapply(fread,
         na.strings = "",
         select = c("ADDRESS_DETAIL_PID",
                    # "DATE_CREATED",
                    # "DATE_LAST_MODIFIED",
                    # "DATE_RETIRED",
                    "BUILDING_NAME",
                    # "LOT_NUMBER_PREFIX",
                    "LOT_NUMBER",
                    # "LOT_NUMBER_SUFFIX",
                    # "FLAT_TYPE_CODE",
                    # "FLAT_NUMBER_PREFIX",
                    "FLAT_NUMBER",
                    # "FLAT_NUMBER_SUFFIX",
                    # "LEVEL_TYPE_CODE",
                    # "LEVEL_NUMBER_PREFIX",
                    # "LEVEL_NUMBER",
                    # "LEVEL_NUMBER_SUFFIX",
                    # "NUMBER_FIRST_PREFIX",
                    "NUMBER_FIRST",
                    # "NUMBER_FIRST_SUFFIX",
                    # "NUMBER_LAST_PREFIX",
                    # "NUMBER_LAST",
                    # "NUMBER_LAST_SUFFIX",
                    "STREET_LOCALITY_PID",
                    # "LOCATION_DESCRIPTION",
                    # "LOCALITY_PID",
                    # "ALIAS_PRINCIPAL",
                    "POSTCODE"
                    # "PRIVATE_STREET",
                    # "LEGAL_PARCEL_ID",
                    # "CONFIDENCE",
                    # "ADDRESS_SITE_PID",
                    # "LEVEL_GEOCODED_CODE",
                    # "PROPERTY_PID",
                    # "GNAF_PROPERTY_PID",
                    # "PRIMARY_SECONDARY"
         )) %>%
  rbindlist(use.names = TRUE, fill = TRUE) %>%
  setkey(ADDRESS_DETAIL_PID)

STREET_LOCALITY_PID__STREET_NAME_STREET_TYPE_CODE <-
  dir(pattern = "_STREET_LOCALITY_psv.psv$",
      path = "~/Data/PSMA-Geocoded-Address-2017/",
      recursive = TRUE,
      full.names = TRUE) %>%
  lapply(fread,
         na.strings = "",
         select = c("STREET_LOCALITY_PID",
                    "STREET_NAME",
                    "STREET_TYPE_CODE")) %>%
  rbindlist(use.names = TRUE, fill = TRUE)


# Reduce the size of lookup tables by converting
# character columns to ints
ADDRESS_DETAIL_PID_by_ID <-
  ADDRESS_DETAIL_PID__by__LATLON %>%
  .[, list(ADDRESS_DETAIL_INTRNL_ID = .I,
           ADDRESS_DETAIL_PID)]

ADDRESS_DETAIL_ID__by__LATLON <-
  ADDRESS_DETAIL_PID__by__LATLON[ADDRESS_DETAIL_PID_by_ID,
                                 j = list(ADDRESS_DETAIL_INTRNL_ID,
                                          LATITUDE,
                                          LONGITUDE),
                                 on = "ADDRESS_DETAIL_PID"]

STREET_PID_vs_ADDRESS_ID <-
  STREET_PID_vs_ADDRESS_PID[ADDRESS_DETAIL_PID_by_ID,
                            on = "ADDRESS_DETAIL_PID"] %>%
  .[, "ADDRESS_DETAIL_PID" := NULL] %>%
  set_cols_first("ADDRESS_DETAIL_INTRNL_ID") %>%
  setkeyv("ADDRESS_DETAIL_INTRNL_ID") %>%
  .[]

STREET_ID_vs_STREET_PID <-
  STREET_LOCALITY_PID__STREET_NAME_STREET_TYPE_CODE %>%
  .[, list(STREET_LOCALITY_INTRNL_ID = .I,
           STREET_LOCALITY_PID)]

STREET_ID_vs_ADDRESS_ID <-
  STREET_ID_vs_STREET_PID[STREET_PID_vs_ADDRESS_ID, on = "STREET_LOCALITY_PID"] %>%
  .[, "STREET_LOCALITY_PID" := NULL] %>%
  set_cols_first("ADDRESS_DETAIL_INTRNL_ID") %>%
  setkeyv("ADDRESS_DETAIL_INTRNL_ID") %>%
  .[]

STREET_LOCALITY_ID__STREET_NAME_STREET_TYPE_CODE <-
  STREET_ID_vs_STREET_PID[STREET_LOCALITY_PID__STREET_NAME_STREET_TYPE_CODE, on = "STREET_LOCALITY_PID"] %>%
  .[, "STREET_LOCALITY_PID" := NULL] %>%
  set_cols_first("STREET_LOCALITY_INTRNL_ID") %>%
  setkeyv("STREET_LOCALITY_INTRNL_ID") %>%
  .[]


devtools::use_data(ADDRESS_DETAIL_ID__by__LATLON,
                   STREET_ID_vs_ADDRESS_ID,
                   STREET_LOCALITY_ID__STREET_NAME_STREET_TYPE_CODE,
                   internal = TRUE,
                   overwrite = TRUE)

