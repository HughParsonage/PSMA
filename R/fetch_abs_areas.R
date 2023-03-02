#' Fetch addresses based on abs statistical areas
#'
#' @param areas IDs of statistical areas to be returned
#' @param area_type The type of statistical area requested. Beware of larger areas
#'
#' @return address table containing all addresses in the specified areas
#' @export
#'
#' @examples
#' fetch_abs_areas(109011176, area_type="SA2")
fetch_abs_areas <- function(areas, area_type = c("MB", "SA1", "SA2", "SA3", "SA4")) {
  if (missing(areas)) {
    stop('argument "areas" is missing, with no default')
  }
  area_type <-  match.arg(area_type)

  tryCatch(
    areas <- switch(area_type,
           MB = bit64::as.integer64(areas),
           SA1 = bit64::as.integer64(areas),
           SA2 = as.integer(areas),
           SA3 = as.integer(areas),
           SA4 = as.integer(areas)),
    warning = function(e) {
      cat(e$m)
      stop("areas must be an integer (or coercible to such).")
    })

  # create the area name
  area_type_col <- paste0(area_type, "_CODE_2021")
  ADDRESS_DETAIL_ID__by__LATLON <- get_fst("ADDRESS_DETAIL_ID__by__LATLON")
  STREET_ID_vs_ADDRESS_ID <- get_fst("STREET_ID_vs_ADDRESS_ID")
  STREET_LOCALITY_ID__STREET_NAME_STREET_TYPE_CODE <- get_fst("STREET_LOCALITY_ID__STREET_NAME_STREET_TYPE_CODE")
  ABS_GEOGRAPHY_HIERARCHY <- get_fst("ABS_GEOGRAPHY_HIERARCHY")
  ADDRESS_DETAIL_ID__ADDRESS_MESHBLOCK_2021 <- get_fst("ADDRESS_DETAIL_ID__ADDRESS_MESHBLOCK_2021")

  # transform the areas into meshblocks
  # is this the best syntax?
  A <- ABS_GEOGRAPHY_HIERARCHY[get(area_type_col) %in% areas]
  AID <- ADDRESS_DETAIL_ID__ADDRESS_MESHBLOCK_2021[A, .(ADDRESS_DETAIL_INTRNL_ID, MB_2021_CODE), on=c("MB_2021_CODE" = "MB_CODE_2021")]
  hr <- STREET_ID_vs_ADDRESS_ID[AID, on="ADDRESS_DETAIL_INTRNL_ID"]
  hr1 <- hr[STREET_LOCALITY_ID__STREET_NAME_STREET_TYPE_CODE,
            on = "STREET_LOCALITY_INTRNL_ID",
            nomatch=0L]
  hh <- hr1[ADDRESS_DETAIL_ID__by__LATLON, on = "ADDRESS_DETAIL_INTRNL_ID", nomatch=0L]

  return(hh)
}


#' Load ABS geography hierarchy
#'
#' @return Table of region names and IDs
#' @export
#' @details Useful for checking names of regions
#' @examples
#' H <- abs_hierarchy()
#' H[stringr::str_detect(SA2_NAME_2021, "Franks")]
#'
abs_hierarchy <- function() {
  return(get_fst("ABS_GEOGRAPHY_HIERARCHY"))
}
