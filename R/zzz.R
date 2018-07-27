.onLoad <- function(libname, package = "PSMA") {
  if (is.null(getOption("PSMA_env"))) {
    options('PSMA_env' = new.env())
  }
}

.onUnload <- function(libpath) {
  if (!is.null(getOption("PSMA_env"))) {
    rm(list = ls(env = getOption("PSMA_env")),
       envir = getOption("PSMA_env"),
       inherits = FALSE)
  }
}
