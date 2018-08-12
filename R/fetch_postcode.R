#' Fetch entire postcodes
#'
#' @param postcodes one or more postcodes (as numbers, which will be coerced to integers)
#'
#' @details Returns an entire postcode of address data. This is useful for generating samples on
#' a per-postcode basis.
#' @return data.table containing all addresses and coordinates
#' @export
#'
#' @examples
#' load(system.file("extdata", "bne_addresses.rda", package = "PSMA"))
#'
#' bnedata <- fetch_postcodes(unique(bne_addresses$postcode))
#' # Select 3 addresses at random from each postcode
#' bnedata[, .SD[sample(.N, 3)], by=.(POSTCODE)]
#'
fetch_postcodes <- function(postcodes) {

  # Put here so the subsequent tryCatch doesn't
  # look confusing.
  if (missing(postcodes)) {
    stop('argument "postcode" is missing, with no default')
  }

  tryCatch(PC <- as.integer(postcodes),
           warning = function(e) {
             cat(e$m)
             stop("Postcode must be an integer (or coercible to such).")
           })

  ADDRESS_DETAIL_ID__by__LATLON <- get_fst("ADDRESS_DETAIL_ID__by__LATLON")
  STREET_ID_vs_ADDRESS_ID <- get_fst("STREET_ID_vs_ADDRESS_ID")
  STREET_LOCALITY_ID__STREET_NAME_STREET_TYPE_CODE <- get_fst("STREET_LOCALITY_ID__STREET_NAME_STREET_TYPE_CODE")

  hr <- STREET_ID_vs_ADDRESS_ID[POSTCODE %in% PC]
  hr1 <- hr[STREET_LOCALITY_ID__STREET_NAME_STREET_TYPE_CODE,
            on = "STREET_LOCALITY_INTRNL_ID",
            nomatch=0L]
  hh <- hr1[ADDRESS_DETAIL_ID__by__LATLON, on = "ADDRESS_DETAIL_INTRNL_ID", nomatch=0L]
  #hh <- hh[, c("lat_int", "lat_rem", "lon_int", "lon_rem"):=NULL]


  return(hh)
}
