#' @title 'dt' processing pipeline: genearte data harvest criteria
#' @description Sets up a data harvest from a desired station file.
#' @param full_eval Defaults to `FALSE`.
#' @param station_file *The path and name of the main station to
#'  which data is being harvested.
#' @param hsf_station *The path and name of the station, or a keyword, to search
#'  for the station from which data will be harvested. _If `NA` the
#'  `hsf_station` is set to `station_file`, i.e., data will be harvested from
#'  the `station_file`._ If a string is provided the function will search
#'  for a matching station during full evaluation (`eval_full` set to TRUE`).
#' @param harvest_dir *The directory in which to search for the `hsf_station`.
#' @param hsf_table *The name of the table from which to harvest data. If the
#'  keyword "raw" is used data will extracted from stations standardised 'raw'
#'  data. The linked "phen" table will be used for describing the extracted
#'  "raw" data. If there are no phenomena descriptions the function will use
#'  the default format of the fields/columns of the harvested table. This will
#'  be added to the processing phenomena information ('phens_dt').
#' @param time_interval *The desired time interval of the `output_dt`. The
#'  full evaluation will extract phenomena from 'raw' data tables accordingly.
#' @param phen_names *Names of phenomena to extract from tables. If `NULL`
#'  all phenomena possible will be harvested. If the desired `time_interval`
#'  is shorter than the time interval or frequency of recordings of the data
#'  to be harvested, phenomena will not be harvested.
#' @param recurr Logical. Whether to search recursively in directories
#'  for the `hsf_station`. Parsed to `ipayipi::dta_list()`.
#' @param harvest_station_ext Parsed to `ipayipi::dta_list()`. Defaults
#'  to ".ipip". Must include the period (".").
#' @param prompt Parsed to `ipayipi::dta_list()`. Set to `TRUE` to use
#'  interactive harvest station selection.
#' @param single_out Forces through an interactive process the singling
#'  out of a harvest station. Useful for example where `ipayipi::dta_list()`
#'  returns from than one option.
#' @param f_params For the partial evaluation multiple phenomena can
#'  be described using `ipayipi::agg_params()`. If left blank defaults will
#'  be used on all phenomena harvested.
#' @param f_summary Summary information used in full evaluation. Includes
#'  any tables in the station file with keywords, "phens" or "summary".
#' @param ppsij Summary pipe process table for the function. If provided this
#'  data will be used to overwrite similar arguments provided in the function.
#'  This must be provided for the full function evaluation.
#' @param sf Station file object.
#' @details _Parameters indicated by '*' are used in the partial evaluation.
#'  The results of partial evaluations are fed to the full evaluation.
#'
#' _Time_interval_: In order to minimise the memory requried to
#'  aggregate harvested data, if there are more than one 'raw' tables with
#'  different record intervals, phenomena will be exrtacted from the 'raw'
#'  table whose record interval most closely matches the specified
#'  `time_interval`.
#' @export
hsf_param_eval <- function(
  station_file = NULL,
  hsf_station = NULL,
  harvest_dir = ".",
  hsf_table = "raw",
  time_interval = NULL,
  phen_names = NULL,
  recurr = TRUE,
  harvest_station_ext = ".ipip",
  prompt = TRUE,
  single_out = TRUE,
  full_eval = FALSE,
  f_params = NULL,
  f_summary = NULL,
  ppsij = NULL,
  sf = NULL,
  ...
) {
  "%ilike%" <- "phen_name" <- "table_name" <- "input_dt" <- "phen_syn" <-
    "dfft_diff" <- "dt_n" <- "dtp_n" <- "decis" <- "output_dt" <-
      "dfft_secs" <- "orig_table_name" <- ":=" <- NULL
  # partial function evaluation --- returns shorter hsf_param list
  #  full evaluation returns a list of available phenomena and
  #  their phenomena details.
  if (full_eval != TRUE) {
    if (harvest_dir == ".") harvest_dir <- NULL
    hsf_params <- list(
      station_file = station_file,
      hsf_station = hsf_station,
      harvest_dir = harvest_dir,
      hsf_table = hsf_table,
      phen_names = phen_names
    )
    f_params <- hsf_params[!sapply(hsf_params, is.null)]
    class(f_params) <- c(class(f_params), "dt_harvest_params")
  } else { # full function evaluation --------------------------------------
    if (!is.null(ppsij)) {
      # extract parts of ppsij and assign values in env
      ppsi_names <- names(ppsij)[!names(ppsij) %in% c("n", "f", "f_params")]
      for (k in seq_along(ppsi_names)) {
        assign(ppsi_names[k], ppsij[[ppsi_names[k]]][1])
      }
    }

    # extract parts of f_params and assign values in environment
    hsf_param_names <- names(f_params)
    for (k in seq_along(hsf_param_names)) {
      if (exists(hsf_param_names[k])) {
        assign(hsf_param_names[k],
          f_params[[hsf_param_names[k]]])
      }
    }
    if (!is.null(hsf_station)) {
      hsfn <- ipayipi::dta_list(input_dir = harvest_dir, recurr = recurr,
        file_ext = harvest_station_ext, wanted = hsf_station,
        unwanted = NULL, prompt = prompt, single_out = single_out)
      hsfn <- gsub(pattern = "^./", replacement = "", hsfn)
    } else {
      station_file <- gsub(pattern = "^./", replacement = "", station_file)
      hsfn <- station_file
    }
    # open hsf file if different from sf
    if (basename(hsfn) != basename(station_file)) {
      hsf <- attempt::try_catch(expr = readRDS(hsfn), .w = ~stop)
      hsf_names <- names(hsf)
      hsf_summary <- hsf[hsf_names %ilike% "summary|phens"]
    } else {
      hsf_summary <- f_summary
      hsf_names <- f_summary$sf_names
    }
    # proceedure below dependent on input data types
    # --- if extracting from standardised pipe data we can extract phenomena
    # details...
    # initial phenomena name organisation
    if (exists("input_dt")) hsf_table <- input_dt

    # check hsf_table -- name of the input table
    # identify & extract correct phen table
    phen_tabs <- hsf_summary[names(hsf_summary) %ilike% "phens"]
    if (!is.null(phen_names)) {
      phen_tabs <- lapply(phen_tabs, function(x) x[phen_name %in% phen_names])
    }
    # extract from raw data
    if (hsf_table %ilike% "raw") {
      p <- phen_tabs$phens[table_name %ilike% "raw",
        c("phid", "phen_name", "units", "measure", "var_type", "table_name")]
      if (hsf_table != "raw") p <- p[table_name == hsf_table]
      # join on data summary info
      s <- unique(hsf_summary$data_summary, by = "table_name")
    }
    # extract from processed data ('_dt')
    if (hsf_table %ilike% "_dt") {
      p <- phen_tabs$phens_dt[table_name %ilike% "_dt",
        c("phid", "phen_name", "units", "measure", "var_type", "table_name")]
      if (hsf_table != "_dt") p <- p[table_name == hsf_table]
      # join on data summary info
      s <- unique(hsf_summary$data_summary_dt, by = "table_name")
    }
    # standardise types of measures, units and var_types ...
    sts_phen_var_type <- data.table::data.table(
      phen_prop = c("num", "int", "chr", "fac", "posix", "logi"),
      phen_syn = c("numeric", "integer", "str|string|chr|character|char",
        "fac|factor|fact", "date|time|posix|date-time|posixct",
        "logical|lgcl|lcl|lgc|logi")
    )
    # extract from other source
    if (!any(sapply(phen_tabs, function(x) any(x$table_name == hsf_table))) &&
      hsf_table != "raw") {
      # open table
      ht <- attempt::try_catch(expr = readRDS(hsfn), .w = ~stop)[[hsf_table]]
      # get date time info
      dttm <- names(ht)[names(ht) %ilike% "date_time|date-time"][1]
      dtti <- ipayipi::record_interval_eval(dt = ht[[dttm]], dta_in = ht)
      p <- data.table::data.table(
        phid = NA_integer_, phen_name = names(ht), units = NA_character_,
        measure = NA_character_, var_type = sapply(ht, function(x) class(x)[1]),
        table_name = hsf_table
      )
      # standardise var type nomenclature
      s <- data.table::data.table(record_interval_type =
        dtti$record_interval_type, record_interval = dtti$record_interval,
        start_dttm = min(ht[[dttm]], na.rm = TRUE), end_dttm =
        min(ht[[dttm]], na.rm = TRUE), table_name = hsf_table
      )
      p$var_type <- as.vector(sapply(p$var_type, function(x) {
        sts_phen_var_type[phen_syn %ilike% x][["phen_prop"]][1]
      }))
    }
    if (is.null(phen_names[1]) || is.na(phen_names[1])) {
      phen_names <- unique(p$phen_name)
    }
    phen_names <- unique(p$phen_name)
    phen_names <- phen_names[order(phen_names)]
    # add record interval to p to organise aggregation intervals and phenomena
    # selection
    s <- s[, c("record_interval_type", "record_interval", "table_name",
      "start_dttm", "end_dttm")]
    sx <- lapply(s$record_interval, ipayipi::sts_interval_name)
    sx <- data.table::rbindlist(sx)
    s <- cbind(s, sx)
    p <- merge(x = p, y = s, all.y = TRUE, by = "table_name")
    agg_time_interval <- ipayipi::sts_interval_name(time_interval)
    if ("event_based" %in% p$record_interval_type) {
      dfft_dta <- as.numeric(x = mondate::as.difftime(
          max(s$end_dttm) - min(s$start_dttm)), units = "secs")
      p <- subset(p, select = -dfft_secs)
      p <- transform(p, dfft_secs = dfft_dta)
    }
    if (is.na(agg_time_interval$dfft_secs)) agg_time_interval$dfft_secs <- 0
    p$agg_intv <- agg_time_interval$dfft_secs
    p$dfft_diff <- p$agg_intv - p$dfft_secs
    # can only aggregate if time intervals are more lengthy than raw data
    if (all(p$dfft_diff < 0)) {
      warning(paste0(time_interval, " has a shorter duration than ",
        "available raw data!", collapse = ""))
    }
    p <- p[dfft_diff >= 0]
    data.table::setorderv(p, cols = c("phen_name", "dfft_diff"))
    p <- lapply(split.data.frame(p, f = factor(p$phen_name)), function(x) {
      x$decis <- ifelse(x$dfft_diff == min(x$dfft_diff), TRUE, FALSE)
      x <- x[decis == TRUE][1, ]
      invisible(x)
    })
    p <- data.table::rbindlist(p)
    # summarise p info for actual harvesting
    # - dt_phen summary
    # - dt_harvest parameters
    phens_dt <- data.table::data.table(
      ppsid = paste(dt_n, dtp_n, sep = "_"),
      phid = p$phid,
      phen_name = p$phen_name,
      units = p$units,
      measure = p$measure,
      var_type = p$var_type,
      record_interval_type = p$record_interval_type,
      orig_record_interval = p$record_interval,
      dt_record_interval = gsub(pattern = " ", replacement = "_",
        x = agg_time_interval$sts_intv),
      orig_table_name = p$table_name,
      table_name = output_dt
    )
    hsf_params <- phens_dt[, c("ppsid", "orig_table_name", "phen_name"),
      with = FALSE][, ":="(station_file = station_file, hsf_station = hsfn)][,
        hsf_table := orig_table_name][, c("ppsid", "station_file",
        "hsf_station", "hsf_table", "phen_name"), with = FALSE]

    f_params <- list(f_params = list(hsf_params = hsf_params),
      phens_dt = phens_dt)
  }
  return(f_params)
}