#' @title Pipe directory intitiate---rainfall
#' @description Creates the necessary folders for the ipaypi rainfall
#'  data processing pipeline.
#' @param work_dir The folder, that is, directory in which to create the
#'  pipeline directories.
#' @keywords initiate pipeline, folder creation, directory structure.
#' @author Paul J. Gordijn
#' @return Notification of directory creation.
#' @details This function automates the creation of three folders/diretories
#'  that are requried for bulk processing of files in the ipayipi groundwater
#'  pipeline structure. The directories are:
#' * **wait_room** --- for raw data imports
#' * **nomvet** --- for storing standardized native solonist data files
#' * **rainrr_dta** --- transforming and processing standardized native data
#'  & processing
#' @md
#' @examples
#' # Inititate pipeline
#' dir <- "." # define the working directory
#' pipe_init_rain(work_dir = dir)
#' @export
rain_pipe_init <- function(
    work_dir = "."
) {
    # wait_room dir
    wait_room <- file.path(work_dir, "wait_room")
        if (!dir.exists(wait_room)) {
            dir.create(wait_room)
    }
    nomvet_room <- file.path(work_dir, "nomvet_room")
        if (!dir.exists(nomvet_room)) {
            dir.create(nomvet_room)
    }
    solr_room <- file.path(work_dir, "rainr_room")
        if (!dir.exists(solr_room)) {
            dir.create(solr_room)
    }
}
