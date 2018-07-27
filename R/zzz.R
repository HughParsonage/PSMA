.onLoad <- function(libname, package = "PSMA") {
  if (is.null(getOption("PSMA_env"))) {
    options('PSMA_env' = new.env())
  }
}
