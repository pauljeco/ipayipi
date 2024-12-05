#' Short internal functions
#'
#' Turns parameters into a list
#' An alias for list in pipe processing.
#' Paul J. Gordijn
#' @noRd
#' @export

# internal function to return logical if future plan mode is not sequential
fcoff <- function(...) {
  "%ilike%" <- NULL
  x <- future::future(getOption("future.class"))
  if (any(class(x) %ilike% "sequential")) {
    return(FALSE)
  }
  return(TRUE)
}

# flat file imbibe options for fread ----
# default read for imbibing data
#' @export
deft_read <- function(
  file_path = NULL,
  col_dlm = NULL,
  nrows = 250,
  dsi = NULL,
  xtra_v = FALSE,
  ...
) {
  file <- data.table::fread(file = file_path, header = FALSE, nrows = nrows,
    check.names = FALSE, blank.lines.skip = FALSE, sep = col_dlm,
    showProgress = xtra_v, strip.white = FALSE, fill = TRUE
  )
  return(list(file = file, dsi = dsi))
}
#' @export
# function to dynamically read in flat file and correct header info
dyno_read <- function(file_path = NULL, col_dlm = NULL, nrows = 250,
  dsi = NULL, xtra_v = FALSE, ...
) {
  file <- data.table::fread(file_path, header = FALSE, nrows = nrows, ...)
  l <- R.utils::countLines(file_path)[1]
  if (is.infinite(nrows)) {
    r <- nrow(file)
  } else {
    r <- nrow(data.table::fread(file_path, header = FALSE, nrows = Inf, ...))
  }
  file_head <- readLines(file_path)[1:(l - r)]
  file_head <- lapply(file_head, function(x) {
    x <- strsplit(x, split = col_dlm)
    x <- unlist(c(x, rep(NA, ncol(file) - length(x))))
    x <- lapply(x, function(z) gsub("\"", "", z))
    names(x) <- names(file)
    return(data.table::as.data.table(x))
  })
  file_head <- suppressWarnings(data.table::rbindlist(file_head, fill = TRUE))
  file <- suppressWarnings(rbind(file_head, file, use.names = TRUE))
  return(list(file = file, dsi = dsi))
}
#' @export
# function to dynamically read in flat file and correct header info
# relaxed version of dyno read that relaxes the condition of
# inconsistent column numbers with rbind arg, fill = TRUE
dyno_read2 <- function(file_path = NULL, col_dlm = NULL, nrows = 250,
  dsi = NULL, xtra_v = FALSE, ...
) {
  file <- data.table::fread(file_path, header = FALSE, nrows = nrows, ...)
  l <- R.utils::countLines(file_path)[1]
  if (is.infinite(nrows)) {
    r <- nrow(file)
  } else {
    r <- nrow(data.table::fread(file_path, header = FALSE, nrows = Inf, ...))
  }
  file_head <- readLines(file_path)[1:(l - r)]
  file_head <- lapply(file_head, function(x) {
    x <- strsplit(x, split = col_dlm)
    x <- unlist(c(x, rep(NA, ncol(file) - length(x))))
    x <- lapply(x, function(z) gsub("\"", "", z))
    names(x) <- names(file)
    return(data.table::as.data.table(x))
  })
  suppressWarnings(file_head <- data.table::rbindlist(file_head, fill = TRUE))
  file <- suppressWarnings(
    rbind(file_head, file, use.names = TRUE, fill = TRUE)
  )
  return(list(file = file, dsi = dsi))
}

#' @export
# dt param eval ----
params <- function(...) list(...)

#' @export
# general fs ----
# Function that returns the mode
getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

#' @export
# vectoriesd version of gsub for using in data.table
vgsub <- Vectorize(gsub, vectorize.args = c("pattern", "replacement", "x"))
