#' @title  'dt' processing pipeline: harvest data from another station or table.
#' @description Used to extract data from another table, station, or source.
#' @param station_file File path of the station being processed.
#' @param hsf_table The path of the directory in which to search for the
#'  external data and/or data table.
#' @param ext_station The name (or keyword) of the station from which to extract
#'  data.
#' @param output_dt The output table name where harvested data is extracted too.
#' @param time_interval The desirsed time_tinterval associated with the
#'  extracted data. If this is `NULL` then the table is extracted as is.
#' @param f_params A vector of phenomena name to be extracted from the
#'  harvested data table. If NULL all column names are extracted.
#' @author Paul J. Gordijn
#' @details
#'
#' @export
dt_harvest <- function(
  station_file = station_file,
  f_params = NULL,
  time_interval = NULL,
  sf = NULL,
  ...) {
  # harvest data from tables
  if (station_file == unique(f_params$hsf_station)[1]) {
    hsf <- sf
  } else {
    hsf <- attempt::try_catch(
      expr = readRDS(unique(f_params$hsf_station)[1]), .w = ~stop)
  }
  hsf <- hsf[names(hsf) %in% unique(f_params$hsf_table)]

  # filter out unwanted phens
  hsf_names <- names(hsf)
  hsf <- lapply(hsf_names, function(x) {
    n <- c(names(hsf[[x]])[names(hsf[[x]]) %in%
      f_params[hsf_table == x]$phen_name])
    if ("date_time" %in% names(hsf[[x]])) n <- c("date_time", n)
    hsf <- hsf[[x]][, n, with = FALSE]
    return(hsf)
  })
  names(hsf) <- hsf_names
  # return the results and summary info to be used in the pipe line
  return(list(hsf_dts = hsf))
}