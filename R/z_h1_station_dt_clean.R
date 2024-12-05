#' @title Dev function: Apply filter windows to time series data
#' @description Function under development. Uses the non-parametric hampel filter to detect anomalies and impute values in a univariate series of numeric data.
#' @param sfc  List of file paths to the temporary station file directory. Generated using `ipayipi::open_sf_con()`.
#' @param station_file Name of the station being processed.
#' @param clean_f Algorithm name. Only "hampel" supported.
#' @param phens Vector of the phenomena names that will be evaluated by the hampel filter. If NULL the function will not run.
#' @param seg Name of column to segment filtering on. Cleaning within each data segment or slice will be run independently.
#' @param w Window size for the hempel filter. Defaults to ten.
#' @param madf Scalar factor of MAD (median absolute deviation). Higher values relax oulier detection. Defaults to the standard of three.
#' @param seg_fuzz Not yet implemented. String representing the threshold time interval between the list of segment date-time 
#' @param na_t Not yet implemented. Fractional tolerace of the amount of NA values in a segment for linear interpolation of missing values.
#' @param tighten Not yet implemented. Scaling fraction between zero and one that sensitizes the detection of outliers near the head and tail ends of segments. The fraction is multiplied by the mad factor.
#' @param ppsij Data processing `pipe_seq` table from which function parameters and data are extracted/evaluated. This is parsed to this function automatically by `ipayipi::dt_process()`.
#' @param verbose Logical. Whether or not to report messages and progress.
#' @param xtra_v Logical. Whether or not to report xtra messages, progess, plus print data tables.
#' @param chunk_v Logical. Print data chunking messages. Useful for debugging/digging into chunking methods.
#' @keywords outlier detection, value imputation, univariate data,
#' @details This function employs a user-customised hampel filter to check data for outliers and impute missing values with the median of the filter window. Function under development.
#' - Need to add generated table details to the phen_ds summary table!
#' @export
#' @author Paul J. Gordijn
dt_clean <- function(
  phens = NULL,
  w = 21,
  madf = 3,
  align = "left",
  seg = NULL,
  cush = TRUE,
  clean_f = "hampel",
  owrite = TRUE,
  na_t = 0.75,
  tighten = 0.65,
  station_file = NULL,
  station_file_ext = ".ipip",
  ppsij = NULL,
  f_params = NULL,
  sfc = NULL,
  verbose = FALSE,
  xtra_v = FALSE,
  chunk_v = FALSE,
  ...
) {

  "%ilike%" <- ":=" <- "." <- ".N" <- ".SD" <- NULL
  "table_name" <- "flag" <- "fff" <- "seg_dflt" <- "segn" <- "date_time" <-
    "fout" <- "n" <- "fouti" <- "flagi" <- "flag" <- "original_v" <-
    "replace_v" <- "phen" <- "s" <- "stage" <- NULL
  # set default args (f_params) from ppsij
  #  - generate a table for this purpose
  # determine how far back to reach when opening data
  # open data
  #  - filter bt start and end dttm
  #  - check for phens
  #  - subset data by phens
  # loop through runs with froll
  #  - generate outlier dt tbl for each sequential loop
  #    this needs to be sequential where the next run depends on
  #    a cleaned version of the last [do while]
  # perform the tighten arg
  # save data and outlier dt tbl

  # prep data read ----
  sfcn <- names(sfc)
  dta_in <- NULL
  hsf_dta <- NULL
  if ("dt_working" %in% sfcn) {
    dta_in <- sf_dta_read(sfc = sfc, tv = "dt_working")
    ng <- sf_dta_read(sfc = sfc, tv = "gaps")[["gaps"]][
      table_name %in% ppsij$output_dt[1]
    ]
  }

  if (is.null(dta_in) && any(sfcn %ilike% "_hsf_table_")) {
    pstep <- paste0("^", ppsij$dt_n[1], "_.+_hsf_table_")
    hsf_dta <- sfcn[sfcn %ilike% pstep]
    hsf_dta <- hsf_dta[length(hsf_dta)]
    dta_in <- sf_dta_read(sfc = sfc, tv = hsf_dta)
    ng <- dta_in[[1]]$gaps
    ng$table_name <- ppsij$output_dt[1]
  }

  # eindx filter
  # *need to build in an extra filter for moving the window back
  dta_in_o <- dt_dta_filter(dta_link = dta_in, ppsij = ppsij)
  dta_in <- dta_in_o

  # prep clean args ----
  z_default <- data.table::data.table(table_name = NA_character_,
    phens = NA_character_, seg = NULL, madf = madf,
    w = NA_integer_, clean_f = NA_character_
  )[0]
  z <- lapply(f_params, function(x) {
    x <- eval(parse(text = sub("^~", "data.table::data.table", x)))
    x
  })
  z <- rbind(z_default, data.table::rbindlist(z, fill = TRUE), fill = TRUE)[
    order(phens)
  ]
  z$table_name <- ppsij$output_dt
  z$madf <- data.table::fifelse(is.na(z$madf), madf, z$madf)
  p <- c("date_time", z$phens)
  segs <- z[!is.na(seg)]$seg
  # prep centre spelling for data.table
  z[align %ilike% "centre|center", "align"] <- "center"

  # split by phen so each phen is evaluated seperately
  z <- split.data.frame(z, f = factor(z$phens))

  # open data ----
  dt <- dt_dta_open(dta_link = dta_in[[1]])
  dt <- subset(dt, select = c(unique(p), unique(segs)))
  if (xtra_v) {
    cli::cli_inform(c("i" = "Pre-clean data -- head"))
    print(head(dt))
  }

  # set default segment of whole data set
  dt[c(1, .N), seg_dflt := 1]
  # run filters ----
  zouts <- lapply(z, function(zx) {
    sqrw <- 1
    phenx <- zx$phens[1]
    # set to default seg is noe is defined
    zx[is.na(seg), seg := "seg_dflt"]
    while (sqrw <= nrow(zx)) {
      ## prep segments ----
      # prep segments from column of dates joined to agg data
      # deal with first and last segs, and too short segs?
      t <- dt[, names(dt)[!names(dt) %in% c("seg", "segn")], with = FALSE]
      t[!is.na(s), seg := seq_len(.N), env = list(s = zx[sqrw]$seg)]
      t[c(1, .N), seg := 0][!is.na(seg) & !.N & !1, seg := seq_len(.N)]
      t[which(seg > 0) + 1, seg := 0][!is.na(seg), seg := seq_len(.N)]
      t[!is.na(seg), segn := rep(seq_len((max(seg) / 2)), each = 2)]
      if (!"fff" %in% names(t)) t[, fff := NA_real_]
      t[, ":="(
        flag = NA_real_, # value replacement if outlier
        # modified phen value - cycled in multiple runs
        fff = data.table::fifelse(!is.na(fff), fff, phenx),
        fout = FALSE # logical indicating outliers
      ), env = list(phenx = phenx)]
      # divide into segments before checking outliers
      dtj <- lapply(seq_len(max(t$seg, na.rm = TRUE) / 2), function(j) {
        dtj <- t[which(t$segn %in% j)[1]:which(t$segn %in% j)[2]]
        dtj <- dtj[, n := j][, .(date_time, fff, fout)]
        return(dtj)
      })
      ### work segments with hampel filter ----
      dtj <- lapply(dtj, function(xj) {
        xj[, flag := data.table::frollapply(
          fff, FUN = function(x) {
            hampel(x, w = zx$w[sqrw], d = zx$madf[sqrw], align = zx[sqrw]$align)
          }, n = zx$w[sqrw], align = zx[sqrw]$align
        )]
        #### cushion edges ----
        # if left cushion tail with rigth
        # if right cushion head with left
        # if centre cushion head and tail

        # setup window size for center cushions
        if (zx[sqrw]$align %in% "center") {
          ws <- zx$w[sqrw] / 2
          if (round(ws) != ws) ws <- (zx$w[sqrw] + 1) / 2
        } else {
          ws <- zx$w[sqrw]
        }
        # only cushion if enough rows in segment
        # set up cushion rows for evaluation
        if (nrow(xj) > (2 * ws) && cush == TRUE) {
          if (zx[sqrw]$align %in% "right") {
            r2 <- NULL
          } else {
            r2 <- c((nrow(xj) - (ws * 2)):nrow(xj))
          }
          if (zx[sqrw]$align %in% "left") {
            r1 <- 0
          } else {
            r1 <- seq_len(ws * 2)
          }
        } else {
          r1 <- 0
          r2 <- 0
        }
        xj <- xj[r1, cush := data.table::frollapply(
          fff, FUN = function(x) {
            hampel(x, w = zx$w[sqrw], d = zx$madf[sqrw], align = "left")
          }, n = zx$w[sqrw], align = "left"
        )]
        xj <- xj[r2, cush := data.table::frollapply(
          fff, FUN = function(x) {
            hampel(x, w = zx$w[sqrw], d = zx$madf[sqrw], align = "right")
          }, n = zx$w[sqrw], align = "right"
        )]
        # generate summary table
        # replace values if option is specified
        xj <- xj[is.na(flag),
          flag := data.table::fifelse(!is.na(cush), cush, flag)
        ][!is.na(flag)][, -c("cush"), with = FALSE]
        xo <- xj[!is.na(flag), fout := TRUE][, fout := data.table::fifelse(
          flag == fff, FALSE, fout
        )]
        return(xo)
      })
      dtj <- data.table::rbindlist(dtj)
      data.table::setnames(dtj,
        c("fff", "fout", "flag"), c("fffi", "fouti", "flagi")
      )
      # join dtj to t and owrite water level
      t <- dtj[t, on = .(date_time)][order(date_time)]
      # fill NAs in fff with 'flag' values
      t <- t[,
        fff := data.table::fifelse(is.na(fff) & !is.na(flagi), flagi, fff)
      ]
      # transfer 'flag tag' from fouti to fout
      t[,
        fout := data.table::fifelse(fouti == TRUE & fout != TRUE, TRUE, FALSE)
      ]
      t <- t[
        , fff := data.table::fifelse(!is.na(flagi) & flagi != fff, flagi, fff)
      ]
      t <- t[, names(t)[!names(t) %in% c("fffi", "fouti", "flagi")],
        with = FALSE
      ]
      sqrw <- sqrw + 1
    }
    t <- t[
      fout == TRUE | (is.na(phenx) & !is.na(fff)), .(date_time, phenx, fff),
      env = list(phenx = phenx)
    ]
    return(t)
  })
  # organise outlier/replacement value summary tables
  zouts <- lapply(seq_along(zouts), function(i) {
    zouts[[i]][, phen := names(zouts)[i]
    ][, ":="(stage = ppsij$dt_n[1], step = ppsij$dtp_n[1])
    ][, original_v := p, env = list(p = names(zouts)[i])
    ][, replace_v := fff
    ][, .(date_time, step, stage, phen, original_v, replace_v)]
  })
  names(zouts) <- names(z)
  # replace original values if requested (owrite = T)
  tn <- dta_in[[1]][[hsf_dta]]$indx$table_name
  if (owrite) {
    # prep filtered phenomena
    zps <- lapply(seq_along(zouts), function(i) {
      t <- zouts[[i]][, .(date_time, replace_v)]
      t <- t[dt, on = .(date_time)
      ][, p := data.table::fifelse(!is.na(replace_v), replace_v, p),
        env = list(p = names(zouts)[i])
      ]
      t <- t[, .(p), env = list(p = names(zouts)[i])]
      return(t)
    })
    # overwrite old phenomena in dt with 'zouts'
    zps <- do.call(cbind, zps)
    names(zps) <- paste0(names(zps), "_zps")
    dt <- dt_dta_open(dta_link = dta_in[[hsf_dta]])
    dt <- cbind(dt, zps)
    dt <- dt[, (names(zouts)) := .SD, .SDcols = names(zps)
    ][, names(dta_in[[1]][[hsf_dta]]$indx$dta_n), with = FALSE]
    if (chunk_v) cli::cli_inform(c(" " = "Chunking data"))
    sf_dta_wr(dta_room = file.path(dirname((sfc[1])), tn), dta = dt,
      overwrite = TRUE, tn = tn, ri = ppsij[1]$time_interval,
      verbose = verbose, xtra_v = xtra_v, chunk_v = chunk_v
    )
  }
  # save outlier/filter table to station ----
  # read in old table if it exists
  # overwrite old results (need to filter out older dates)
  zouts <- data.table::rbindlist(zouts)
  class(zouts) <- c(class(zouts), "fltr_vals")
  # purge extant data over the same time period, then chunk
  dta_room <- file.path(dirname((sfc[1])), paste0(tn, "_fltr_vals"))
  tn <- paste0(tn, "_fltr_vals")
  if (tn %in% sfcn) {
    # purging
    mn <- min(zouts$date_time)
    mx <- max(zouts$date_time)
    dta_in <- sf_dta_read(sfc = sfc, tv = tn)
    # need to redo purge to only purge values in the same stange and step
    sf_dta_chunkr_purge(dta_room = dta_room, dta_in = dta_in, tn = tn,
      chlck = list(stage = ppsij$dtp_n[1], step = ppsij$dt_n[1]),
      mn = mn, mx = mx, chunk_v = chunk_v
    )
  }
  sf_dta_wr(dta_room = dta_room, dta = zouts, overwrite = TRUE, tn = tn,
    rit = "event_based", ri = "discnt", verbose = verbose, xtra_v = xtra_v,
    chunk_v = chunk_v
  )
  ppsij <- ppsij[, ":="(start_dttm = dta_in_o[[1]][[hsf_dta]]$indx$mn,
      end_dttm = dta_in_o[[1]][[hsf_dta]]$indx$mx
    )
  ]
  return(list(ppsij = ppsij))
}