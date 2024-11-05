#' @title Apply filter windows to time series data
#' @description Uses the non-parametric hampel filter to detect anomalies and impute values in a univariate series of numeric data.
#' @param sfc  List of file paths to the temporary station file directory. Generated using `ipayipi::open_sf_con()`.
#' @param station_file Name of the station being processed.
#' @param clean_f Algorithm name. Only "hampel" supported.
#' @param phens Vector of the phenomena names that will be evaluated by the hampel filter. If NULL the function will not run.
#' @param seg Name of column to segment filtering on. Cleaning within each data segment or slice will be run independently.
#' @param w Window size for the hempel filter. Defaults to ten.
#' @param mad Scalar factor of MAD (median absolute deviation). Higher values relax oulier detection. Defaults to the standard of three.
#' @param seg_fuzz Not yet implemented. String representing the threshold time interval between the list of segment date-time 
#' @param na_t Not yet implemented. Fractional tolerace of the amount of NA values in a segment for linear interpolation of missing values.
#' @param tighten Not yet implemented. Scaling fraction between zero and one that sensitizes the detection of outliers near the head and tail ends of segments. The fraction is multiplied by the mad factor.
#' @param ppsij Data processing `pipe_seq` table from which function parameters and data are extracted/evaluated. This is parsed to this function automatically by `ipayipi::dt_process()`.
#' @param verbose Logical. Whether or not to report messages and progress.
#' @param xtra_v Logical. Whether or not to report xtra messages, progess, plus print data tables.
#' @param chunk
#' _v Logical. Print data chunking messages. Useful for debugging/digging into chunking methods.
#' @keywords outlier detection, value imputation, univariate data,
#' @export
#' @author Paul J. Gordijn
dt_clean <- function(
  phens = NULL,
  w = 21,
  mad = 3,
  align = "left",
  seg = NULL,
  cush = TRUE,
  clean_f = "hampel",
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

  "%ilike%" <- ":=" <- NULL
  "table_name" <- "flag" <- NULL
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
  dta_in <- dt_dta_filter(dta_link = dta_in, ppsij = ppsij)

  # prep clean args ----
  z_default <- data.table::data.table(table_name = NA_character_,
    phens = NA_character_, seg = NULL, mad = mad,
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
  z$mad <- data.table::fifelse(is.na(z$mad), mad, z$mad)
  p <- c("date_time", z$phens)
  segs <- z[!is.na(seg)]$seg
  # prep centre spelling for data.table
  z[align %ilike% "centre|center", "align"] <- "center"
  z <- split.data.frame(z, f = factor(z$phens))

  # open data ----
  dt <- dt_dta_open(dta_link = dta_in[[1]])
  dt <- subset(dt, select = c(unique(p), unique(segs)))
  ipayipi::msg(cat(crayon::bgWhite(" Pre-clean data -- head  ")), xtra_v)
  if (xtra_v) print(head(dt))

  # run filters ----
  lapply(z, function(zx) {
    sqrw <- 1
    phenx <- zx$phens[1]
    while (sqrw <= nrow(zx)) {
      ## prep segments ----
      # prep segments from column of dates joined to agg data
      # deal with first and last segs, and too short segs?
      dt[!is.na(s), seg := seq_len(.N), env = list(s = zx[sqrw]$seg)]
      dt[c(1, .N), seg := 0][!is.na(seg) & !.N & !1, seg := seq_len(.N)]
      dt[which(seg > 0) + 1, seg := 0][!is.na(seg), seg := seq_len(.N)]
      dt[!is.na(seg), segn := rep(seq_len((max(seg) / 2)), each = 2)]
      dt[!is.na(seg)]
      dtj <- lapply(seq_len(max(dt$seg, na.rm = TRUE) / 2), function(j) {
        dtj <- dt[which(dt$segn %in% j)[1]:which(dt$segn %in% j)[2]]
        dtj <- dtj[, n := j][, c("date_time", phenx), with = FALSE]
        return(dtj)
      })
      ### work segments with hampel filter ----
      dtj <- lapply(dtj, function(xj) {
        xj <- xj[, flag := data.table::frollapply(phenx,
          FUN = function(x) {
            hampel(x, w = zx$w[sqrw], d = zx$mad[sqrw], align = zx[sqrw]$align)
          }, n = zx$w[sqrw], align = zx[sqrw]$align
        ), env = list(phenx = phenx)]
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
        xj <- xj[r1, cush := data.table::frollapply(phenx,
          FUN = function(x) {
            hampel(x, w = zx$w[sqrw], d = zx$mad[sqrw], align = zx[sqrw]$align)
          }, n = zx$w[sqrw], align = "left"
        ), env = list(phenx = phenx)]
        xj <- xj[r2, cush := data.table::frollapply(phenx,
          FUN = function(x) {
            hampel(x, w = zx$w[sqrw], d = zx$mad[sqrw], align = zx[sqrw]$align)
          }, n = zx$w[sqrw], align = "right"
        ), env = list(phenx = phenx)]
        # generate summary table
        # replace values if option is specified
      })
      sqrw <- sqrw + 1
    }
  })
  emmasf <- sapply(slist, FUN = function(emm) {
    ### Function inputs
    emma <- sub(x = emm, pattern = ".rds", replacement = "")
    cr_msg <- padr(
      core_message = paste0(" segmenting ", emma, "... ", collapse = ""),
      pad_char = "-", pad_extras = c("|", "", "", "|"),
      force_extras = FALSE, justf = c(0, 0), wdth = 80
    )
    message(cr_msg)
    sol_temp <- readRDS(file = file.path(input_dir, emm))
    rlog <- sol_temp$log_retrieve[which(sol_temp$log_retrieve$QA == TRUE), ]
    drifting <- sol_temp$log_t

    # Continue with f if there is data in all the tables
    if (nrow(rlog) > 0 & nrow(sol_temp$log_t) > 0) {
      # rlog - Interval join rlog
      rlog_reads_t <- rlog[, c("Date_time")]
      rlog_reads_t$event <- "dips"
      dt <- data.table::data.table(
        Date_time = as.POSIXct(c(min(drifting$Date_time),
          max(drifting$Date_time)
        ))
      )
      dt$event <- "se"
      rlog_reads_t <- rbind(rlog_reads_t, dt)
      # Define intervals of NA values as breakpoints in the data
      drifting_ttt <- drifting[, c("Date_time", "bt_level_m")]
      drifting_ttt$id <- seq_len(nrow(drifting_ttt))
      drifting_ttt$j <- ifelse(is.na(drifting_ttt$bt_level_m),
        drifting_ttt$id, NA
      )
      x <- which(!is.na(drifting_ttt$j))
      if (length(x) > 0) {
        drifting_ttt <- drifting_ttt[x, ]
        drifting_ttt$k <- c(1, drifting_ttt$j[c(2:(nrow(drifting_ttt)))]
          - drifting_ttt$j[c(1:(nrow(drifting_ttt) - 1))]
        )
        drifting_ttt$l <- c(c(drifting_ttt$j[c(1:(nrow(drifting_ttt) - 1))]
            - drifting_ttt$j[c(2:(nrow(drifting_ttt)))]
          ) * -1, 1
        )
        drifting_ttt$l[1] <- 2
        drifting_ttt$l[nrow(drifting_ttt)] <- 2
        dt <- drifting_ttt[which(drifting_ttt$k > 1 | drifting_ttt$l > 1),
          "Date_time"
        ]
        dt$event <- "nd"
        rlog_reads_t <- rbind(rlog_reads_t, dt)
      }
      dt <- data.table::data.table(
        Date_time = as.POSIXct(c(as.POSIXct(sol_temp$xle_FileInfo$Start),
          as.POSIXct(sol_temp$xle_FileInfo$End)
        ))
      )
      dt$event <- "dwn"
      rlog_reads_t <- rbind(rlog_reads_t, dt)
      rlog_reads_t <- rlog_reads_t[order(Date_time), ]
      rlog_reads_t <- unique(rlog_reads_t, by = "Date_time")
      rlog_reads_t$i <- TRUE
      data.table::setkey(drifting, Date_time)
      data.table::setkey(rlog_reads_t, Date_time)
      drifting_tt <- merge(y = rlog_reads_t, x = drifting, all.x = TRUE)

      # Assign each interference event with reading a consecutive number
      segs <- as.numeric(
        seq_len(nrow(drifting_tt[which(drifting_tt$i == TRUE), ])) - 1
      )
      drifting_tt[which(drifting_tt$i == TRUE), "iN"] <- segs
      segs_n <- max(drifting_tt$iN, na.rm = TRUE)

      # If the segment length is one, the segments need to be adjusted
      for (i in seq_len(segs_n)) {
        if (i == 1) {
          r1 <- which(drifting_tt$iN == 0)
          r2 <- which(drifting_tt$iN == 1)
        }
        if (i > 1) {
          r1 <- which(drifting_tt$iN == (i - 1))
          r2 <- which(drifting_tt$iN == i)
        }
        if ((r2 - r1) == 1) {
          if (drifting_tt$event[r1] == "rdu") { # rdu means redundant
            drifting_tt$i[r2] <- NA
            drifting_tt$event[r2] <- "rdu"
          }
          if (drifting_tt$event[r1] == "dwn" | # dwn means download event
                drifting_tt$event[r2] == "dwn") {
            if (drifting_tt$event[r1] == "dwn") {
              drifting_tt$i[r2] <- NA
              drifting_tt$event[r2] <- "rdu"
            } else {
              drifting_tt$i[r1] <- NA
              drifting_tt$event[r1] <- "rdu"
            }
          } else {
            if (is.na(drifting_tt$bt_level_m[r1]) |
                is.na(drifting_tt$bt_level_m[r2])
            ) {
              if (is.na(drifting_tt$bt_level_m[r1])) {
                drifting_tt$i[r1] <- NA
                drifting_tt$event[r1] <- "rdu"
              } else {
                drifting_tt$i[r2] <- NA
                drifting_tt$event[r2] <- "rdu"
              }
            }
          }
        }
      }

      # Assign modified interference event with reading a consecutive number
      segs <- as.numeric(
        seq_len(nrow(drifting_tt[which(drifting_tt$i == TRUE), ])) - 1
      )
      drifting_tt[which(!is.na(drifting_tt$iN)), "iN"] <- NA
      drifting_tt[which(drifting_tt$i == TRUE), "iN"] <- segs
      segs_n <- max(drifting_tt$iN, na.rm = TRUE)

      # Work through each of the naartjie segments and check for outliers
      naarbs <- lapply(seq_len(segs_n), function(z) {
        if (z == 1) {
          r1 <- which(drifting_tt$iN == 0)
          r2 <- which(drifting_tt$iN == 1)
        }
        if (z > 1) {
          r1 <- which(drifting_tt$iN == (z - 1)) + 1
          r2 <- which(drifting_tt$iN == z)
        }

        tab <- drifting_tt[r1:r2, ]
        cr_msg <- padr(core_message = paste0("seg ", z, ": ",
            tab$Date_time[1], " --> ", tab$Date_time[nrow(tab)], collapse = ""
          ), pad_char = c(" "), pad_extras = c("|", "", "", "|"),
          force_extras = TRUE, justf = c(-1, 2), wdth = 59
        )
        message("\r", appendLF = FALSE)
        message("\r", cr_msg, appendLF = FALSE)
        nasc <- sum(is.na(tab$t_bt_level_m))
        nnasc <- sum(!is.na(tab$t_bt_level_m))
        if (nasc / nnasc <= na_t & !is.infinite(nasc / nnasc)) {
          should_i <- TRUE
        } else {
          should_i <- FALSE
        }
        tab_ts <- tab
        if (nrow(tab_ts) > 3 & should_i == TRUE) {
          # hampel rule import
          if (last_rule == TRUE) {
            rule <- tab_ts$bt_Outlier_rule[1]
          } else {
            rule <- NA
          }
          if (is.factor(rule)) rule <- as.character(rule)
          if (is.na(rule)) {
            msg <- paste0(" Dflt rule: ", "hf_", w, "_", mad, " |")
            message(msg, appendLF = TRUE)
          } else {
            msg <- paste0(" Last rule: ", rule, " |")
            message(msg, appendLF = TRUE)
            rule <- strsplit(rule, "_")
            w <- as.integer(rule[[1]][2])
            x_devs <- as.integer(rule[[1]][3])
          }

          tab_ts_cleen_f <- function() {
            cleen_seg <- hampel_f(
              srs = tab_ts[, c("Date_time", "bt_level_m")],
              w_width = w,
              x_devs = mad,
              tighten = tighten
            )
            return(cleen_seg)
          }
          cleen_seg <- tab_ts_cleen_f()

          # need to change this return to a cleaned segment
          tab_ts_cleen <- data.table::data.table(
            Date_time = as.POSIXct(cleen_seg$hamper$Date_time),
            t_bt_level_m = as.numeric(cleen_seg$hamper$hf),
            bt_Outlier = as.logical(cleen_seg$hamper$bt_Outlier),
            bt_Outlier_rule = as.factor(
              rep(paste0("hf_", w, "_", mad),
                nrow(cleen_seg$hamper)
              )
            )
          )
        } else {
          tab_ts_cleen <- data.table::data.table(
            Date_time = as.POSIXct(tab_ts$Date_time),
            t_bt_level_m = as.numeric(rep(NA, nrow(tab_ts))),
            bt_Outlier = as.logical(rep(NA, nrow(tab_ts))),
            bt_Outlier_rule = as.factor(rep(NA, nrow(tab_ts)))
          )
          message(" Too many NAs to clean segment.", appendLF = TRUE)
        }
        if (nrow(tab_ts_cleen) != nrow(tab_ts)) {
          message("------------------------------FUNKY")
        }
        return(tab_ts_cleen)
      })

      naarbs <- data.table::rbindlist(naarbs)

      ## Prepare to save the output
      sol_temp$log_t$t_bt_level_m <- naarbs$t_bt_level_m
      sol_temp$log_t$bt_Outlier <- naarbs$bt_Outlier
      sol_temp$log_t$bt_Outlier_rule  <- naarbs$bt_Outlier_rule
      ## Flag manually detected outliers
      for (i in seq_len(nrow(sol_temp$log_t_man_out))) {
        start <- as.POSIXct(as.character(sol_temp$log_t_man_out$Start[i]))
        end <- as.POSIXct(as.character(sol_temp$log_t_man_out$End[i]))
        sol_temp$log_t[
          which(sol_temp$log_t$Date_time >= start &
              sol_temp$log_t$Date_time <= end
          ), "bt_Outlier"
        ] <- TRUE
        sol_temp$log_t[
          which(sol_temp$log_t$Date_time >= start &
              sol_temp$log_t$Date_time <= end
          ), "t_bt_level_m"
        ] <- NA
      }
      saveRDS(sol_temp, file.path(input_dir, emm))
    }
    cr_msg <- padr(core_message = paste0(" naartjies consumed ", collapse = ""),
      pad_char = "=", wdth = 80, pad_extras = c("|", "", "", "|"),
      force_extras = FALSE, justf = c(0, 0)
    )
    message(cr_msg)
  }
  )
}