#' Add latitude and longitude columns to a data frame of addresses
#' @param DT
#' @export
#'

mutate_geocode <- function(DT, new_names = c("lat", "lon"),
                           overwrite = FALSE) {
  dt_was_tibble <- FALSE
  if (!is.data.table(DT)) {
    if (!is.data.frame(DT)) {
      stop("`DT` must be a data.frame.")
    }
    if (requireNamespace("tibble", quietly = TRUE) &&
        tibble::is_tibble(DT)) {
      dt_was_tibble <- TRUE
    }
    DT <- as.data.table(DT)
  }
  if (any(names(DT) %notin% c(names(formals(geocode)),
                              "attempt_decode_street_abbrev"))) {
    stop("DT requires the following names:\n\t",
         paste0(setdiff(names(formals(geocode)),
                        "attempt_decode_street_abbrev"),
                sep = "\n\t"))
  }

  if (length(new_names) != 2L) {
    stop("`new_names` had length ", length(new_names), ", yet ",
         "two names are required. Change `new_names` to be length-2.")
  }
  if (!is.character(new_names)) {
    stop("`new_names` was type ", typeof(new_names), ". ",
         "Change `new_names` to be a character vector.")
  }
  if (!overwrite && new_names[1L] %in% names(DT)) {
    stop("First entry of new_names was ", new_names[1L], ", ",
         "but this is already a column name in DT.")
  }
  if (!overwrite && new_names[2L] %in% names(DT)) {
    stop("Second entry of new_names was ", new_names[2L], ", ",
         "but this is already a column name in DT.")
  }

  DT[, (new_names) := geocode(flat_number = flat_number,
                              number_first = number_first,
                              building_name = building_name,
                              street_name = street_name,
                              street_type = street_type,
                              postcode = postcode)[, "ordering" := NULL]]
  DT[]
}





