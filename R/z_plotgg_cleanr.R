plotgg_cleanr <- function(
  station_file = NULL,
  pipe_house = NULL,
  tbl_name = NULL,
  phen_key = NULL,
  mn = NULL,
  mx = NULL,
  slide = 1,
  dflt_nobs = NULL,
  verbose = FALSE,
  xtra_v = FALSE,
  chunk_v = FALSE,
  ...
) {
  "%chin%" <- "." <- ":=" <- NULL
  "date_time" <- "n" <- NULL
  # open station file connection
  sfc <- open_sf_con(pipe_house = pipe_house, station_file = station_file)
  sn <- names(sfc)
  if (!tbl_name %in% sn && !paste0(tbl_name, "_fltr_vals") %in% sn) {
    cli::cli_abort(c(
      "No station file with table named ({.var tbl_name}):",
      "\'{tbl_name}\' detected!"
    ))
  }

  dta_in <- sf_dta_read(sfc = sfc, tv = tbl_name)
  dto_in <- sf_dta_read(sfc = sfc, tv = paste0(tbl_name, "_fltr_vals"))

  # open data
  ppsij <- data.table::data.table(
    start_dttm = as.POSIXct(mn), end_dttm = as.POSIXct(mx)
  )
  dta_inf <- dt_dta_filter(dta_link = dta_in, ppsij = ppsij)
  dta <- dt_dta_open(dta_link = dta_inf[[1]])

  # open filter table
  dto_inf <- dt_dta_filter(dta_link = dto_in, ppsij = ppsij)
  dto <- dt_dta_open(dta_link = dto_inf[[1]])

  # setup data for dygraphs
  dto <- split.data.frame(dto, f = factor(dto$phen))
  dto <- lapply(seq_along(dto), function(i) {
    pn <- names(dto[i])
    data.table::setnames(dto[[i]], c("original_v", "replace_v"),
      c(paste0(pn, "_orgl"), paste0(pn, "_fltr"))
    )
    return(dto[[i]][, -c("phen"), with = FALSE])
  })

  # join dta and dto
  dtaj <- lapply(seq_along(dto), function(i) {
    dtoj <- dto[[i]][dta, on = .(date_time)]
    dtoj <- dtoj[, names(dtoj)[!names(dtoj) %in% names(dta)], with = FALSE]
    return(dtoj)
  })
  d <- do.call(cbind, c(list(dta), dtaj))
  s <- unique(names(d)[names(d)
    %in% c(phen_key, paste0(phen_key, "_orgl"), paste0(phen_key, "_fltr"))
  ])
  p <- ggplot2::ggplot(d, ggplot2::aes(
    x = date_time, y = !!as.name(phen_key)
  )) +
    geom_line(colour = "blue") +
    geom_point(aes(y = !!as.name(s[3])), colour = "green") +
    geom_point(aes(y = !!as.name(s[2])), colour = "red")
  return(p)
}