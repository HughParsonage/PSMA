

author_rstar_pages <- function(DT, lat, lon, shallow = FALSE, quiet = FALSE) {
  if (quiet) {
    cat <- function(...) invisible()
  }
  LATITUDE <- as.character(substitute(lat))
  LONGITUDE <- as.character(substitute(lon))

  DT0 <- hutils::selector(DT,
                          cols = c(LATITUDE,
                                   LONGITUDE),
                          shallow = shallow)

  LATITUDE <- LONGITUDE <- NULL
  as.character(Sys.time())
  DT0 <- unique(DT0, by = c("LATITUDE", "LONGITUDE"))
  npoints <- DT0[, .N]
  xrange <- DT0[, range_rcpp(LONGITUDE)]
  yrange <- DT0[, range_rcpp(LATITUDE)]
  for (idepth in 1:31) {  # 2^31 maximum integer
    if (npoints < 2^idepth) {
      break
    }
    cat("depth" = idepth)
    hutilscpp:::cut_DT(DT0,
                       depth = idepth,
                       x_range = xrange,
                       y_range = yrange)
  }
  # Issue: what if the nearest neighbnour of a point in a page
  # is *outside* that page?
  # A: find for each page or page level
  # the distance to the 'next' page's edge

  XenoPages <-
    lapply(1:13, function(level) {
      o <-
        DT0[,
            .(minLAT = min(LATITUDE),
              maxLAT = max(LATITUDE),
              minLON = min(LONGITUDE),
              maxLON = max(LONGITUDE)),
            # sort to make x before y
            keyby = c(sort(grep(paste0("breaks", level, "$"),
                                names(DT0),
                                value = TRUE)))]

      o_ranges <-
        DT0[, .(rangeLAT = range_rcpp(LATITUDE)[1:2]),
            keyby = c(paste0("ybreaks", level))] %>%
        .[, I := .I] %>%
        .[, d_lat := rangeLAT - shift(rangeLAT)] %>%
        .[I %% 2L == 1L]
      o_ranges[, d_lat := rangeLAT - shift(rangeLAT)][]
      o[, d_lon := minLON - shift(maxLON)]
      o[, d_lat := minLAT - shift(maxLAT)]

    })

  as.character(Sys.time())

  # About a minute
  Ns <- integer(length(DT0))
  DT1 <- copy(DT0)
  L1_20 <- lapply(1:20, function(x) data.table())
  for (P in 1:20) {
    cat(P, "\n")
    cat(as.character(Sys.time()), "\n")
    DTp <- DT1[, N := .N, keyby = c(paste0("xbreaks", P),
                                    paste0("ybreaks", P))]
    if (DTp[, min(N)] < 4096L) {
      out <- DTp[N < 4096L][, .(minLATITUDE = min(LATITUDE),
                                maxLATITUDE = max(LATITUDE),
                                minLONGITUDE = min(LONGITUDE),
                                maxLONGITUDE = max(LONGITUDE),
                                Chapter = P,
                                Page = 2^P + .GRP,
                                xbreaks13_min = min(xbreaks13),
                                xbreaks13_max = max(xbreaks13),
                                ybreaks13_min = min(ybreaks13),
                                ybreaks13_max = max(ybreaks13)),
                            keyby = c(paste0("xbreaks", P),
                                      paste0("ybreaks", P))]
      L1_20[[P]] <- out
      DT1 <- DT1[!out, on = c(key(DTp))]
      cat(as.character(Sys.time()), "\t", nrow(DT1), "\n")
    } else {
      data.table()
    }
    if (nrow(DT1) == 0L) {
      break
    }

    cat(as.character(Sys.time()), "\n\n")
  }
  L1_20
}
