#' @title Remove csv files from the 'wait_room'
#' @description Uses a list of successfully converted hobo rainfall files from
#'  a 'converted_rain_hobo' object generated by
#'  `ipayipi::rain_hobo_conversion()` to select files from the 'wait_room' and
#'  remove the imported files.
#' @param rain_in The output from `ipayipi::rain_hobo_conversion()`.
#' @details
#'  Extracts all the import file names from the batch of successfully converted
#'  hobo rainfall file exports, which have been extracted from files imported
#'  into the 'wait_room', then deletes the files.
#' @keywords hobo rainfall data; batch processing; append data
#' @return
#' @author Paul J. Gordijn
#' @export
rain_import_remove <- function(
  rain_in = NULL,
  ...
) {
  if (class(rain_in) == "converted_rain_hobo") {
    z <- lapply(rain_in$hobo_converts, function(x) {
      if (file.exists(x$data_summary$import_file_name)) {
        file.remove(x$data_summary$import_file_name)
      } else {
        message(paste0("Can't find ", x$data_summary$import_file_name))
      }
    })
  } else {
    message(paste0("Please make sure that the file is an output from ",
      "ipayipi::rain_hobo_conversion"))
  }
}