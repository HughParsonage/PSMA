#' Get data from fst extdata
#' @name get_fst
#' @param dt A string, the \code{data.table} to retrieve.
#' @param cache_env (logical, default: \code{TRUE}) If \code{FALSE}, the result
#' is not saved between occurrences. Possibly beneficial if memory is limited.
#' @return The \code{data.table}.
#' @details Purpose is to provide an API to the \code{fst} data
#' in \code{extdata}. If the \code{\link{PSMA_env}} contains \code{dt}
#' it is returned; if not, it is retrieved from the \code{fst} file, then
#' assigned.
#'
#' @export

get_fst <- function(dt = c("ADDRESS_DETAIL_ID__by__LATLON",
                           "STREET_ID_vs_ADDRESS_ID",
                           "STREET_LOCALITY_ID__STREET_NAME_STREET_TYPE_CODE",
                           "addressB13_west",
                           "addressB13_east"),
                    cache_env = TRUE) {
  dt <- dt[1]
  psma_env <- getOption("PSMA_env", new.env())

  if (exists(dt, envir = psma_env, inherits = FALSE) && cache_env) {
    x <- get(dt, envir = psma_env, inherits = FALSE)
  } else {
    if (dt == "ADDRESS_DETAIL_ID__by__LATLON") {
      x <- fst::read_fst(system.file("extdata",
                                     "address2.fst",
                                     package = "PSMA"),
                         as.data.table = TRUE)
      x[, "LATITUDE" := lat_int + lat_rem / 10^7]
      x[, "LONGITUDE" := lon_int + lon_rem / 10^7]
    } else {
      x <- fst::read_fst(system.file("extdata", paste0(dt, ".fst"),
                                     package = "PSMA"),
                         as.data.table = TRUE)
    }
    switch(dt,
           "STREET_ID_vs_ADDRESS_ID" = {
             setindexv(x, "POSTCODE")
           })


    if (cache_env) {
      assign(dt,
             value = x,
             envir = psma_env)
    }
  }

  x[]
}

.Exists <- function(nom) {
  is.environment(the_env <- getOption("PSMA_env")) && exists(nom, the_env)
}

.Assign <- function(nom, value) {
  if (.Exists(nom)) {
    return(.Get(nom))
  }
  if (is.environment(the_env <- getOption("PSMA_env"))) {
    assign(nom, value, envir = the_env)
  }
  invisible(value)
}

.Get <- function(nom) {
  if (.Exists(nom)) {
    get0(nom, envir = getOption("PSMA_env"), inherits = FALSE)
  }
}

.Clear <- function() {
  if (is.environment(the_env <- getOption("PSMA_env"))) {
    rm(list = ls(envir = the_env), envir = the_env)
  }
}


