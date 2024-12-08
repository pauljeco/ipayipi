#' @title Highlight 'gaps' (missing data) in station data
#' @description The period between the time a logger is stopped, removed, or discommissioned, and when a logger beings recording data again, counts as missing data. However, for event-based data, where the frequency of recordings is temporarily erratic, identifying gaps is more tricky. This function sets some rules when identifying gaps in event-based time-series data.
#' @param pipe_house Required. List of pipeline directories. __See__ `ipayipi::ipip_house()` __for details__.
#' @param station_file Standardised __ipayipi__ station file.
#' @param gap_problem_thresh_s A duration threshold (in seconds) beyond which a gap, in event-based time series records, is considered problematic --- a 'true' gap. These gaps require infilling or 'patching'. The default for event-based data is six hours (i.e., 6 * 60 * 60 seconds). If the 'record_interval_type' is not 'mixed' or 'event_based', i.e. it is only continuous, then the 'record_interval' parameter is used as the `gap_problem_thresh_s`.
#' @param event_thresh_s A gap can also be specified in the event metadata by providing only a single date-time stamp and an 'event_threshold_s' (in seconds). By taking this date-time stamp the function approximates the start and end-time of the data gap by adding and subtracting the given `event_thresh_s` to the date-time stamp, respectively. If the `event_thresh_s` is supplied in the 'meta_events' data (_see_ `meta_to_station()`) then the value supplied therein is used in preference to the `event_thresh_s` argument in this function. The default event threshold here is ten minutes, i.e., 10 * 60 seconds.
#' @param meta_events Optional. The name of the data.table in the station file with event metadata. Defaults to NA.
#' @param keep_open Logical. Keep _hidden_ 'station_file' open for ease of access. Defaults to `TRUE`.
#' @param prompt Should the function use an interactive station file selection function otherwise all files are returned. `TRUE` or `FALSE`.
#' @param recurr Should the function search recursively into sub directories for station files? `TRUE` or `FALSE`.
#' @param wanted Vector of strings listing files that should not be included in station file search.
#' @param unwanted Vector of strings listing station files that should not be included in the import.
#' @details Batch application of `ipayipi::gap_eval()`.
#'
#' @keywords data gaps; data pipeline; missing data; event data;
#' @author Paul J. Gordijn
#' @return Names of stations with Updated gap period tables.
#' @export
gap_eval_batch <- function(
  pipe_house = NULL,
  gap_problem_thresh_s = 6 * 60 * 60,
  event_thresh_s = 10 * 60,
  phens = NULL,
  phen_eval = FALSE,
  meta_events = "meta_events",
  station_ext = ".ipip",
  prompt = FALSE,
  wanted = NULL,
  unwanted = NULL,
  verbose = FALSE,
  xtra_v = FALSE,
  chunk_v = FALSE,
  ...
) {

  # get list of station names in the ipip directory
  station_files <- ipayipi::dta_list(
    input_dir = pipe_house$ipip_room, file_ext = station_ext, prompt = FALSE,
    recurr = FALSE, baros = FALSE, unwanted = unwanted, wanted = wanted
  )

  if (length(station_files) == 0) {
    cli::cli_abort(c("No station files detected!",
      "i" = "Station files should be housed in the \'ipip_room\' here:",
      " " = "{.var pipe_house$ipip_room}"
    ))
  }

  # open connections to station files
  lapply(station_files, function(x) {
    open_sf_con(pipe_house = pipe_house, station_file = x,
      verbose = verbose, chunk_v = chunk_v
    )
  })

  if (verbose || xtra_v || chunk_v) cli::cli_h1(
    "Evaluating gaps in {length(station_files)} station file{?s}"
  )

  # generate gap table for each station file
  gaps <- lapply(seq_along(station_files), function(i) {
    g <- ipayipi::gap_eval(
      pipe_house = pipe_house, station_file = station_files[i],
      gap_problem_thresh_s = gap_problem_thresh_s, event_thresh_s =
        event_thresh_s, meta_events = meta_events,
      verbose = verbose, phens = phens, phen_eval = phen_eval
    )
    ipayipi::write_station(pipe_house = pipe_house, sf = g,
      station_file = station_files[i], overwrite = TRUE,
      append = TRUE, chunk_v = chunk_v
    )
    if (verbose || xtra_v || chunk_v) cli::cli_inform(c(
      "v" = "{i}: {gsub(station_ext, \'\', station_files[i])}"
    ))
    invisible(station_files[i])
  })
  if (verbose || xtra_v || chunk_v) cli::cli_h1("")
  invisible(gaps)
}
