#' @title Unpack data processing step
#' @description Internal function used to unpack a data pipeline processing
#'  step. Essentially converts parameters into a list.
#' @param dt_n The numeric (integer) order of the processing stage.
#' @param dtp_n A processing stage ('dt_n') contains multiple steps. A stage
#'  generally only outputs one processed data table with the specified
#'  `time_interval` given at the harvesting stage.
#' @param f One of the following functions given as a string: "dt_harvest",
#'  "dt_calc_chain", "dt_agg", and "dt_join".
#'  - "dt_harvest": function used to harvest data from a station or other
#'   source.
#'  - "dt_calc_chain": uses `data.table` chaining to perform calculations,
#'   filtering/subsetting, and generate new variables.
#'  - "dt_agg": used to aggregate data by a specified/default function by
#'   a desired `time_interval`.
#'  - "dt_join": merges two datasets via overlap, left, right, inner and
#'   outer joins. _Not yet implemented_.
#' @param input_dt The name/keyword of the input data table (character string).
#'  If not specified at the harvesting stage, "raw" data imported into a
#'  station file will be harvested.
#' @param output_dt The name of the output data table (character string) ---
#'  the final table is appended to a station file object.
#' @param time_interval Only has to be specified in the harvesting step. Only
#'  one time interval per stage.
#' @author Paul J. Gordijn
#' @export
p_step <- function(
  dt_n = 1,
  dtp_n = 1,
  f = NULL,
  f_params = NULL,
  input_dt = NA,
  output_dt = NA,
  output_dt_preffix = "dt_",
  output_dt_suffix = NULL,
  time_interval = NA,
  ...) {

  # mandatory args
  m <- c("f")
  m <- m[sapply(m, function(x) is.null(get(x)))]
  m <- sapply(m[length(m)], function(x) {
    stop(paste0("Missing args: ", paste0(m, collapse = ", ")), call. = FALSE)
  })
  if (!any(f %in% c("dt_calc_chain", "dt_harvest", "dt_agg", "dt_join"))) {
    stop(paste0("Unrecognised function: ", f))
  }
  # specify time_interval for dt_harvest
  if (f %in% c("dt_harvest")) {
    if (is.na(time_interval)) time_interval <- "discnt"
    time_interval <- ipayipi::sts_interval_name(time_interval)
    time_interval <- time_interval$sts_intv
    if (is.na(input_dt)) input_dt <- "raw"
  }

  # auto generate output table name
  if (is.na(output_dt) && !is.na(time_interval)) {
    output_dt <- paste0(
      output_dt_preffix, gsub(" ", "_", time_interval), output_dt_suffix)
  }

  # special formatting for dt_calc_string
  f_params_calc <- f_params[any(class(f_params) %in% "dt_calc_string")]
  f_params_calc <- as.character(f_params_calc)

  # special formatting for dt_harvest
  f_params_harvest <- list(
    f_params[any(class(f_params) %in% "dt_harvest_params")])
  f_params_harvest <- f_params_harvest[
    sapply(f_params_harvest, function(x) length(x) != 0)]
  f_params_harvest <- sapply(f_params_harvest, function(x) {
    deparse1(x, collapse = "")
  })
  f_params_harvest <- gsub(
    pattern = "list", replacement = "hsf_param_eval",
    x = f_params_harvest
  )
  f_params_harvest <- gsub(
        pattern = ", class = c\\(\"list\", \"dt_harvest_params\"\\))",
        replacement = "", x = f_params_harvest)
  f_params_join <- list(
    f_params[any(class(f_params) %in% "dt_join_params")])
  f_params_join <- f_params_join[
    sapply(f_params_join, function(x) length(x) != 0)]
  f_params_join <- sapply(f_params_join, function(x) {
    deparse1(x, collapse = "")
  })
  f_params_join <- gsub(
    pattern = "list", replacement = "join_param_eval",
    x = f_params_join
  )

  # special formatting for dt_agg
  f_params_agg <- list(
    f_params[any(class(f_params) %in% "dt_agg_params")])
  f_params_agg <- f_params_agg[
    sapply(f_params_agg, function(x) length(x) != 0)]

  f_params_text <- list(f_params_calc, f_params_harvest, f_params_join,
    f_params_agg)
  f_params_text <- unlist(f_params_text[
    sapply(f_params_text, function(x) length(x) != 0)])
  if (length(f_params) == 0) {
    f_params_text <- NA_character_
  }
  dt_here <- list(
    dt_n = dt_n,
    dtp_n = dtp_n,
    f = f,
    f_params = f_params_text,
    input_dt = input_dt,
    output_dt = output_dt,
    time_interval = time_interval
  )
  class(dt_here) <- c("list", "pipe_process")
  return(dt_here)
}