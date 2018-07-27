.onLoad <- function(libname, pkgname = "PSMA") {
  if (is.null(getOption("PSMA_env"))) {
    options('PSMA_env' = new.env())
  }
  if (getRversion() >= "2.15.1") {
    utils::globalVariables(
      c(".",
        # names of fst files
        c("ADDRESS_DETAIL_INTRNL_ID", "lat_int", "lat_rem", "lon_int",
          "lon_rem", "LATITUDE", "LONGITUDE", "STREET_LOCALITY_INTRNL_ID",
          "BUILDING_NAME", "LOT_NUMBER", "FLAT_NUMBER", "NUMBER_FIRST",
          "POSTCODE", "STREET_NAME", "STREET_TYPE_CODE"),
        "street_type_decoder"))
  }

}

.onUnload <- function(libpath) {
  if (!is.null(getOption("PSMA_env"))) {
    rm(list = ls(envir = getOption("PSMA_env")),
       envir = getOption("PSMA_env"),
       inherits = FALSE)
  }
}
