# birdnet_results ===========================================================

#' @name birdnet_gather_results
#' @title Gather BirdNET results
#' @description Gather all BirdNET CSV results from a desired folder into a user-friendly data.table / data.frame
#' @param results.directory Path to directory where raw BirdNET result CSVs have been stored
#' @return Returns a data.frame/data.table with the following columns:
#'
#' \itemize{
#' \item{\strong{recordingID}: Site name.}
#' \item{\strong{start.s}: tbd.}
#' \item{\strong{end.s}: tbd}
#' \item{\strong{scientific.name}: tbd.}
#' \item{\strong{common.name}: tbd.}
#' \item{\strong{confidence}: ....}
#' \item{\strong{verify}: ....}
#' \item{\strong{timezone}: ...}
#' }
#'
#' @details
#'
#' This function was developed by the National Park Service Natural Sounds and Night Skies Division to process audio data using BirdNET.

#'
#' @seealso  \code{\link{birdnet_run}}, \code{\link{birdnet_verify}}, \code{\link{birdnet_format_csv}}
#' @export
#' @import data.table
#' @examples
#' \dontrun{
#'
#' # tbd
#' }
#'

birdnet_gather_results <- function(results.directory) {
  paths.formatted <- paste0(results.directory, '/', list.files(path = results.directory, pattern = 'formatted'))
  dat <- rbindlist(lapply(paths.formatted, function(x) data.table(read.csv(x))))
  dat[,verify := as.character(verify)] # make sure this is character
  return(dat)
}
