#' Update data
#'
#' @param path Directory of unzipped folder of the `GNAF` unzipped folder. If
#' \code{NULL}, the default, an attempt will be made to infer the latest PSMA
#' url and unzipped in a temporary directory.
#'
#' @param just_tsv \code{bool}. If \code{TRUE}, only the .tsv files are produced.
#' This is intended to be useful only for this package's development.
#'
#' @return
#' Called for its side-effect. When called, data from `path` are incorporated
#' into the PSMA package folder.
#'
#' @export

update_data <- function(path = NULL, just_tsv = FALSE) {
  if (is.null(path)) {
    return(update_data_auto())
  }
  stopifnot(is.character(path), length(path) == 1, dir.exists(path),
            length(grep("\\.psv$", dir(path = path, recursive = TRUE))) >= 8)
  .latest2fst(path, just_tsv = just_tsv)
}

update_data_auto <- function() {
  url <- readLines("https://raw.githubusercontent.com/HughParsonage/PSMA/master/data-raw/LATEST_PSMA_URL_ZIP")
  tempf.zip <- tempfile(fileext = ".zip")
  status <- download.file(url, destfile = tempf.zip, mode = "wb")
  if (status) {
    stop("status code ", status)
  }
  current_dir <- getwd()
  setwd(dirname(tempf.zip))
  new_dir <- paste0("./", basename(url), "/")
  unzipped.files <- unzip(tempfile, exdir = new_dir)
  update_data(new_dir)
}

.latest2fst <- function(LATEST, progress = 2L, just_tsv = FALSE) {
  cat <- function(...) {
    base::cat(format(Sys.time(), "%H:%M"), ...)
  }

  ADDRESS_DETAIL_PID <- LATITUDE <- LONGITUDE <- NULL

  ADDRESS_DETAIL_PID__by__LATLON <-
    dir(pattern = "_ADDRESS_DEFAULT_GEOCODE_psv",
        recursive = TRUE,
        full.names = TRUE,
        path = LATEST) %>%
    lapply(fread,
           na.strings = "",
           showProgress = FALSE,
           select = c("ADDRESS_DETAIL_PID",
                      "LATITUDE",
                      "LONGITUDE"),
           key = "ADDRESS_DETAIL_PID") %>%
    rbindlist %>%
    setkeyv("ADDRESS_DETAIL_PID")
  if (progress) {
    cat("ADDRESS_DETAIL_PID__by__LATLON created.\n")
  }

  STREET_PID_vs_ADDRESS_PID <-
    dir(pattern = "_ADDRESS_DETAIL_psv.psv$",
        path = LATEST,
        recursive = TRUE,
        full.names = TRUE) %>%
    lapply(fread,
           showProgress = FALSE,
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
    setkeyv("ADDRESS_DETAIL_PID")

  if (progress) {
    cat("STREET_PID_vs_ADDRESS_PID created.\n")
  }
  STREET_LOCALITY_PID <- STREET_NAME <- STREET_TYPE_CODE <- NULL

  STREET_LOCALITY_PID__STREET_NAME_STREET_TYPE_CODE <-
    dir(pattern = "_STREET_LOCALITY_psv.psv$",
        path = LATEST,
        recursive = TRUE,
        full.names = TRUE) %>%
    lapply(fread,
           showProgress = FALSE,
           na.strings = "",
           select = c("STREET_LOCALITY_PID",
                      "STREET_NAME",
                      "STREET_TYPE_CODE")) %>%
    rbindlist(use.names = TRUE, fill = TRUE) %>%
    # Some unnamed streets
    .[]

  if (progress) {
    cat("STREET_LOCALITY_PID__STREET_NAME_STREET_TYPE_CODE created.\n")
  }


  # meshblocks
  ADDRESS_DETAIL_PID__ADDRESS_MESHBLOCK_2021 <-
    dir(pattern = "_MESH_BLOCK_2021_psv.psv$",
        path = LATEST,
        recursive = TRUE,
        full.names = TRUE) %>%
    lapply(fread,
           showProgress = FALSE,
           na.strings = "",
           select = c("ADDRESS_MESH_BLOCK_2021_PID",
                      "ADDRESS_DETAIL_PID",
                      "MB_2021_PID"
                      )) %>%
    rbindlist(use.names = TRUE, fill = TRUE) %>%
    .[]

  # mapping to code
  MB_2021 <- dir(pattern = "_MB_2021_psv.psv$",
                 path = LATEST,
                 recursive = TRUE,
                 full.names = TRUE) %>%
    lapply(fread,
           showProgress = FALSE,
           na.strings = "",
           select = c("MB_2021_PID",
                      "MB_2021_CODE"
           )) %>%
    rbindlist(use.names = TRUE, fill = TRUE) %>%
    .[]

  ADDRESS_DETAIL_PID__ADDRESS_MESHBLOCK_2021[MB_2021,
                                             on = "MB_2021_PID",
                                             MB_2021_CODE := i.MB_2021_CODE]

  ADDRESS_DETAIL_PID__ADDRESS_MESHBLOCK_2021[,MB_2021_PID := NULL]
  ADDRESS_DETAIL_PID__ADDRESS_MESHBLOCK_2021[,ADDRESS_MESH_BLOCK_2021_PID := NULL]

  ADDRESS_DETAIL_INTRNL_ID <- NULL

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

  ADDRESS_DETAIL_ID__ADDRESS_MESHBLOCK_2021 <-
    ADDRESS_DETAIL_PID__ADDRESS_MESHBLOCK_2021[ADDRESS_DETAIL_PID_by_ID,
                                               j=list(ADDRESS_DETAIL_INTRNL_ID, MB_2021_CODE),
                                               on = "ADDRESS_DETAIL_PID"]
  setkeyv(ADDRESS_DETAIL_ID__ADDRESS_MESHBLOCK_2021, "ADDRESS_DETAIL_INTRNL_ID")

  STREET_PID_vs_ADDRESS_ID <-
    STREET_PID_vs_ADDRESS_PID[ADDRESS_DETAIL_PID_by_ID,
                              on = "ADDRESS_DETAIL_PID"] %>%
    .[, "ADDRESS_DETAIL_PID" := NULL] %>%
    set_cols_first("ADDRESS_DETAIL_INTRNL_ID") %>%
    setkeyv("ADDRESS_DETAIL_INTRNL_ID") %>%
    .[]

  STREET_LOCALITY_INTRNL_ID <- NULL

  STREET_ID_vs_STREET_PID <-
    STREET_LOCALITY_PID__STREET_NAME_STREET_TYPE_CODE %>%
    .[, list(STREET_LOCALITY_INTRNL_ID = .I,
             STREET_LOCALITY_PID)] %>%
    setkeyv("STREET_LOCALITY_INTRNL_ID")

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

  # 2big4Github
  #
  # devtools::use_data(ADDRESS_DETAIL_ID__by__LATLON,
  #                    STREET_ID_vs_ADDRESS_ID,
  #                    STREET_LOCALITY_ID__STREET_NAME_STREET_TYPE_CODE,
  #                    street_type_decoder,
  #                    internal = TRUE,
  #                    overwrite = TRUE)

  if (dir.exists("tsv")) {
    fwrite(ADDRESS_DETAIL_ID__by__LATLON, "tsv/ADDRESS_DETAIL_ID__by__LATLON.tsv", sep = "\t")
    fwrite(STREET_ID_vs_ADDRESS_ID, "tsv/STREET_ID_vs_ADDRESS_ID.tsv", sep = "\t")
    fwrite(STREET_LOCALITY_ID__STREET_NAME_STREET_TYPE_CODE, "tsv/STREET_LOCALITY_ID__STREET_NAME_STREET_TYPE_CODE.tsv", sep = "\t")
    fwrite(ADDRESS_DETAIL_ID__ADDRESS_MESHBLOCK_2021, "tsv/ADDRESS_DETAIL_ID__ADDRESS_MESHBLOCK_2021.csv", sep = "\t")
    if (just_tsv) {
      return(invisible(NULL))
    }

  }


  write_dat_fst <- function(x) {
    bd <- "./inst/extdata/"
    if (dir.exists("./inst/extdata/")) {
      b.fst <- paste0("inst/extdata/", deparse(substitute(x)), ".fst")
    } else {
      b.fst <- paste0(deparse(substitute(x)), ".fst")
      b.fst <- system.file("extdata", b.fst, package = "PSMA")
    }
    fst::write_fst(x, b.fst, compress = 100)
  }
  write_dat_fst2 <- function(x) {
    stopifnot(is.character(x), length(x) == 1)
    if (dir.exists("./inst/extdata/")) {
      b.fst <- paste0("inst/extdata/", x, ".fst")
    } else {
      b.fst <- paste0(x, ".fst")
      b.fst <- system.file("extdata", b.fst, package = "PSMA")
    }
    fst::write_fst(get(x), b.fst, compress = 100)
  }

  lat_int <- lat_rem <- lon_int <- lon_rem <- NULL

  address2 <-
    ADDRESS_DETAIL_ID__by__LATLON %>%
    .[, .(ADDRESS_DETAIL_INTRNL_ID, LATITUDE, LONGITUDE,
          lat_int = as.integer(LATITUDE),
          lat_rem = as.integer(10^7 * (LATITUDE - as.integer(LATITUDE))),
          lon_int = as.integer(LONGITUDE),
          lon_rem = as.integer(10^7 * (LONGITUDE - as.integer(LONGITUDE))))] %>%
    setkeyv("ADDRESS_DETAIL_INTRNL_ID")

  # Breaks 13
  addressB13 <-
    ADDRESS_DETAIL_ID__by__LATLON %>%
    unique(by = c("LATITUDE", "LONGITUDE"))

  lon_range <- addressB13[, minmax(LONGITUDE)]
  lat_range <- addressB13[, minmax(LATITUDE)]

  xbreaks13 <- ybreaks13 <- NULL

  ..cut_DT(addressB13,
           depth = 13L,
           x_range = lon_range,
           y_range = lat_range)
  setkeyv(addressB13, c("xbreaks13", "ybreaks13"))
  stopifnot(addressB13[, last(xbreaks13)] == 8192L)

  the_8192_seq_lon <- seq(lon_range[1], lon_range[2], length.out = 8092L)

  all_min_lat <-
    all_max_lat <-
    all_min_lon <-
    all_max_lon <-
    lat_min <-
    lat_max <-
    lon_min <-
    lon_max <-
    d_lon_s <-
    d_lon_n <-
    d_lat_e <-
    d_lat_w <- NULL

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
  median_xbreaks13 <- copy(median_xbreaks13)
  addressB13_west <- addressB13[.(1:median_xbreaks13), on = "xbreaks13"]
  addressB13_east <- addressB13[.(median_xbreaks13:8192), on = "xbreaks13"] # overlaps


  for (i in 6:12) {
    if (progress) {
      cat("Assigning `addressB", i, "`\r", sep = "")
    }
    assign(paste0("addressB", i),
           value = {
             x <-
               copy(address2) %>%
               .[, .(ADDRESS_DETAIL_INTRNL_ID, LATITUDE, LONGITUDE)] %>%
               unique(by = c("LATITUDE", "LONGITUDE"))
             ..cut_DT(x,
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
  if (progress) cat("\n")
  for (addressBs in paste0("addressB", 6:12)) {
    if (progress) {
      cat("Writing `addressB", i, "`\r", sep = "")
    }
    write_dat_fst2(addressBs)
  }

  address2[, c("LATITUDE", "LONGITUDE") := NULL]
  write_dat_fst(address2)
  write_dat_fst(addressB13_west)
  write_dat_fst(addressB13_east)
  write_dat_fst(STREET_ID_vs_ADDRESS_ID)
  write_dat_fst(STREET_LOCALITY_ID__STREET_NAME_STREET_TYPE_CODE)
  write_dat_fst(ADDRESS_DETAIL_ID__ADDRESS_MESHBLOCK_2021)

  find_data_with_noms <- function(noms, just_act = TRUE) {
    all_files.psv <- dir(LATEST,
                         pattern = "\\.psv$",
                         full.names = TRUE,
                         recursive = TRUE)
    have_noms <-
      sapply(all_files.psv,
             function(file.psv) {
               if (just_act && !grepl("ACT", file.psv)) {
                 return(FALSE)
               }
               all(noms %chin% names(fread(file = file.psv, nrows = 0, sep = "|")))
             })
    all_files.psv[have_noms]
  }

  rbindfiles <- function(files.psv, ste = TRUE,
                         showProgress = FALSE,
                         sep = "|",
                         na.strings = c("", "NA"),
                         select = NULL) {
    L <- lapply(files.psv,
                fread,
                showProgress = showProgress,
                sep = "|",
                na.strings = na.strings,
                select = select)
    names(L) <- hutils::trim_common_affixes(files.psv)
    if (ste) {
      rbindlist(L, use.names = TRUE, idcol = "STE")
    } else {
      rbindlist(L, use.names = TRUE)
    }
  }

  LOCALITY_vs_LOCALITY_PID <-
    find_data_with_noms(c("LOCALITY_PID", "NAME"), just_act = FALSE) %>%
    rbindfiles

  write_dat_fst(LOCALITY_vs_LOCALITY_PID)

  LOCALITY_VS_POSTCODE <-
    dir(LATEST, pattern = "ADDRESS_DETAIL_psv", recursive = TRUE,
        full.names = TRUE) %>%
    lapply(function(file.psv) {
      fread(file = file.psv,
            showProgress = FALSE,
            sep = "|",
            na.strings = c("", "NA"),
            select = c("ADDRESS_DETAIL_PID",
                       "LOCALITY_PID", "POSTCODE"))
    }) %>%
    setNames(trim_common_affixes(
      dir(LATEST, pattern = "ADDRESS_DETAIL_psv", recursive = TRUE,
          full.names = TRUE))) %>%
    rbindlist(use.names = TRUE,
              idcol = "STE") %>%
    .[, .N, keyby = c("STE", "LOCALITY_PID", "POSTCODE")]

  write_dat_fst(LOCALITY_VS_POSTCODE)

  STREET_NAMES_BY_ID <-
    dir(LATEST, pattern = "STREET_LOCALITY_psv\\.psv$", recursive = TRUE,
        full.names = TRUE) %>%
    lapply(function(file.psv) {
      fread(file = file.psv,
            showProgress = FALSE,
            sep = "|",
            na.strings = c("", "NA"),
            select = c("STREET_LOCALITY_PID",
                       "LOCALITY_PID",
                       "STREET_NAME",
                       "STREET_TYPE_CODE"))
    }) %>%
    rbindlist(use.names = TRUE, fill = TRUE)

  STREET_BY_POSTCODE <-
    STREET_NAMES_BY_ID[LOCALITY_VS_POSTCODE, on = "LOCALITY_PID", nomatch = 0L] %>%
    .[, .(STREET_NAME, STREET_TYPE_CODE, POSTCODE)] %>%
    unique %>%
    setkey(POSTCODE, STREET_NAME) %>%
    .[]

  write_dat_fst(STREET_BY_POSTCODE)
}

# from hutilscpp
..cut_DT <- function (DT, depth = 1L, x_range = NULL, y_range = NULL) {
  if (anyNA(match(c("LATITUDE", "LONGITUDE"), names(DT), nomatch = NA_integer_))) {
    stop("`DT` lacked columns 'LATITUDE' and 'LONGITUDE'.")
  }
  LONGITUDE <- LATITUDE <- NULL
  if (is.null(x_range)) {
    x_range <- minmax(.subset2(DT, "LONGITUDE"))
  }
  if (is.null(y_range)) {
    y_range <- minmax(.subset2(DT, "LATITUDE"))
  }
  DT[, `:=`("xbreaks", .bincode(LONGITUDE,
                                include.lowest = TRUE,
                                breaks = seq.int(from = x_range[1],
                                                 to = x_range[2],
                                                 length.out = 2^depth + 1)))]
  setnames(DT, "xbreaks", paste0("xbreaks", depth))
  DT[, `:=`("ybreaks", .bincode(LATITUDE,
                                include.lowest = TRUE,
                                breaks = seq.int(from = y_range[1],
                                                 to = y_range[2],
                                                 length.out = 2^depth + 1)))]
  setnames(DT, "ybreaks", paste0("ybreaks", depth))
  DT[]
}



#' Import meshblock hierarchy
#'
#' @param mbexcel path to excel file from link below
#'
#' @details converts the statistical area hierarchy, not including shapes, to a fst
#' https://www.abs.gov.au/statistics/standards/australian-statistical-geography-standard-asgs-edition-3/jul2021-jun2026/access-and-downloads/allocation-files/MB_2021_AUST.xlsx
#'
#' Should be integrated with the main update function
#' @export
#' @examples
#' \dontrun{
#' destxl <- file.path(tempdir(), "MB_2021_AUST.xlsx")
#' download.file("https://www.abs.gov.au/statistics/standards/australian-statistical-geography-standard-asgs-edition-3/jul2021-jun2026/access-and-downloads/allocation-files/MB_2021_AUST.xlsx",
#' destfile=destxl )
#' update_meshblock_map(destxl)
#' }
#'
update_meshblock_map <- function(mbexcel) {

  write_dat_fst <- function(x) {
    bd <- "./inst/extdata/"
    if (dir.exists("./inst/extdata/")) {
      b.fst <- paste0("inst/extdata/", deparse(substitute(x)), ".fst")
    } else {
      b.fst <- paste0(deparse(substitute(x)), ".fst")
      b.fst <- system.file("extdata", b.fst, package = "PSMA")
    }
    fst::write_fst(x, b.fst, compress = 100)
  }
  mb <- readxl::read_excel(mbexcel)
  mb <- as.data.table(mb)
  mb <- mb[MB_CATEGORY_2021 != "Outside Australia"]
  mb[, MB_CODE_2021 := bit64::as.integer64(MB_CODE_2021)][, SA1_CODE_2021 := bit64::as.integer64(SA1_CODE_2021)][, SA2_CODE_2021 := as.integer(SA2_CODE_2021)][, SA3_CODE_2021 := as.integer(SA3_CODE_2021)][, SA4_CODE_2021 := as.integer(SA4_CODE_2021)]
  setkeyv(mb, "MB_CODE_2021")
  ABS_GEOGRAPHY_HIERARCHY <- mb
  ABS_GEOGRAPHY_HIERARCHY[]
  write_dat_fst(ABS_GEOGRAPHY_HIERARCHY)
}
