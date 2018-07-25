
isAttached <- function(pkg) {
  if (requireNamespace("hutils", quietly = TRUE) &&
      packageVersion("hutils") > "1.2.0") {
    hutils::isAttached(pkg)
  } else {
    .pkg <- as.character(substitute(pkg))
    .pkg %in% .packages()
  }
}


