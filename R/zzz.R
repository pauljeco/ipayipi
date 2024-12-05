.onAttach <- function(libname, ipayipi) {
  packageStartupMessage(
    cat(
      cli::style_blurred(c(cli::col_white("Loading saeon "))),
      cli::style_bold(cli::col_magenta("ipayipi ")),
      cli::col_br_magenta("v0.0.6  "),
      cli::style_blurred(
        cli::col_white("|> https://github.com/SAEONData/ipayipi\n")
      ),
      cli::col_blue(c("\nNB! If migrating from package versions < 0.0.6 |> ",
        "version upgrade notes:"
      ))
    )
  )
  packageStartupMessage(cli::cli_inform(c(
    "*" = cli::col_grey(
      paste0("{.var ipip_house()} |> note the change in arg name ",
        "{.var work_dir} to {.var pipe_house_dir}."
      )
    )
    , "*" = cli::col_grey(
      paste0("Manually add the \'aa_\' preffix to your \'nomtab.rns\' and",
        " \'phentab.rps\' files before running {.var header_sts()}, ",
        "and {.var phenomena_sts()}."
      )
    ), "*" = cli::col_grey(
      paste0("Use options {.var verbose} and {.var xtra_v} as a guide ",
        "setting up and processing"
      )
    ), " " = cli::col_grey(
      "See fuction help files for more info ..."
    )
  )))
}
.onLoad <- function(libname, ipayipi) {
  op <- options()
  op.ipip <- list(
    chunk_dir = tempdir()
  )
  toset <- !(names(op.ipip) %in% names(op))
  if (any(toset)) options(op.ipip[toset])
  invisible()
}