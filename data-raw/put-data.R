library(data.table)
library(magrittr)
library(hutils)
library(hutilscpp)

LATEST <- "~/Data/PSMA-Geocoded-Address-2018/"

ADDRESS_DETAIL_PID__by__LATLON <-
  dir(pattern = "_ADDRESS_DEFAULT_GEOCODE_psv",
      recursive = TRUE,
      full.names = TRUE,
      path = LATEST) %>%
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
      path = LATEST,
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
      path = LATEST,
      recursive = TRUE,
      full.names = TRUE) %>%
  lapply(fread,
         na.strings = "",
         select = c("STREET_LOCALITY_PID",
                    "STREET_NAME",
                    "STREET_TYPE_CODE")) %>%
  rbindlist(use.names = TRUE, fill = TRUE) %>%
  # Some unnamed streets
  .[complete.cases(.)]


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
           STREET_LOCALITY_PID)] %>%
  setkey(STREET_LOCALITY_INTRNL_ID)

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

street_type_decoder <- fread("data-raw/street_type_decoder.tsv")
set_unique_key(street_type_decoder, street_abbrev)
# 2big4Github
#
# devtools::use_data(ADDRESS_DETAIL_ID__by__LATLON,
#                    STREET_ID_vs_ADDRESS_ID,
#                    STREET_LOCALITY_ID__STREET_NAME_STREET_TYPE_CODE,
#                    street_type_decoder,
#                    internal = TRUE,
#                    overwrite = TRUE)

provide.dir("tsv")
fwrite(ADDRESS_DETAIL_ID__by__LATLON, "tsv/ADDRESS_DETAIL_ID__by__LATLON.tsv", sep = "\t")
fwrite(STREET_ID_vs_ADDRESS_ID, "tsv/STREET_ID_vs_ADDRESS_ID.tsv", sep = "\t")
fwrite(STREET_LOCALITY_ID__STREET_NAME_STREET_TYPE_CODE, "tsv/STREET_LOCALITY_ID__STREET_NAME_STREET_TYPE_CODE.tsv", sep = "\t")


write_dat_fst <- function(x) {
  fst::write_fst(x, paste0("inst/extdata/", deparse(substitute(x)), ".fst"), compress = 100)
}
write_dat_fst2 <- function(x) {
  fst::write_fst(get(x), paste0("inst/extdata/", x, ".fst"), compress = 100)
}

address2 <-
  ADDRESS_DETAIL_ID__by__LATLON %>%
  .[, .(ADDRESS_DETAIL_INTRNL_ID,
        lat_int = as.integer(LATITUDE),
        lat_rem = as.integer(10^7 * (LATITUDE - as.integer(LATITUDE))),
        lon_int = as.integer(LONGITUDE),
        lon_rem = as.integer(10^7 * (LONGITUDE - as.integer(LONGITUDE))))] %>%
  setkey(ADDRESS_DETAIL_INTRNL_ID)

# Breaks 13
addressB13 <-
  ADDRESS_DETAIL_ID__by__LATLON %>%
  unique(by = c("LATITUDE", "LONGITUDE"))

lon_range <- addressB13[, range_rcpp(LONGITUDE)[1:2]]
lat_range <- addressB13[, range_rcpp(LATITUDE)[1:2]]

hutilscpp:::cut_DT(addressB13,
                   depth = 13L,
                   x_range = lon_range,
                   y_range = lat_range)
setkeyv(addressB13, c("xbreaks13", "ybreaks13"))
stopifnot(addressB13[, last(xbreaks13)] == 8192L)

the_8192_seq_lon <- seq(lon_range[1], lon_range[2], length.out = 8092L)

addressB13_ranges <-
  addressB13[, .(lat_min = min(LATITUDE),
                 lat_max = max(LATITUDE),
                 lon_min = min(LONGITUDE),
                 lon_max = max(LONGITUDE)),
             keyby = .(xbreaks13, ybreaks13)] %>%
  .[, all_min_lat := min(lat_min), keyby = "ybreaks13"] %>%
  .[, all_max_lat := max(lat_max), keyby = "ybreaks13"] %>%
  .[, all_min_lon:= min(lon_min), keyby = "xbreaks13"] %>%
  .[, all_max_lon:= max(lon_max), keyby = "xbreaks13"] %>%
  .[, d_lon_s := all_min_lon - shift(all_max_lon)] %>%
  .[, d_lon_n := shift(all_min_lon, type = "lead", fill = 180) - all_max_lon] %>%
  setkey(ybreaks13) %>%
  .[, d_lat_e := all_min_lat - shift(all_max_lat, fill = 0)] %>%
  .[, d_lat_w := shift(all_min_lat, type = "lead", fill = 90) - all_max_lat] %>%
  .[]


# Need to break up to avoid GitHub file size limits
# Australia is skewed...
median_xbreaks13 <- PSMA:::median_xbreaks13
addressB13_west <- addressB13[.(1:median_xbreaks13), on = "xbreaks13"]
addressB13_east <- addressB13[.(median_xbreaks13:8192), on = "xbreaks13"] # overlaps


for (i in 6:12) {
  assign(paste0("addressB", i),
         value = {
           x <-
             copy(address2) %>%
             .[, .(ADDRESS_DETAIL_INTRNL_ID, LATITUDE, LONGITUDE)] %>%
             unique(by = c("LATITUDE", "LONGITUDE"))
           hutilscpp:::cut_DT(x,
                              depth = i,
                              x_range = lon_range,
                              y_range = lat_range)
           setkeyv(x, paste0(c("xbreaks", "ybreaks"), i))
           the_L <- L1_20[[i]]
           col_xbreak <- paste0("xbreaks", i)
           col_ybreak <- paste0("ybreaks", i)

           centre <-
             x[the_L, on = key(x), nomatch=0L]
         })
}
for (addressBs in paste0("addressB", 6:12)) {
  write_dat_fst2(addressBs)
}



write_dat_fst(address2)
write_dat_fst(addressB13_west)
write_dat_fst(addressB13_east)
# write_dat_fst(ADDRESS_DETAIL_ID__by__LATLON)
write_dat_fst(STREET_ID_vs_ADDRESS_ID)
write_dat_fst(STREET_LOCALITY_ID__STREET_NAME_STREET_TYPE_CODE)

devtools::use_data(street_type_decoder, overwrite = TRUE)

