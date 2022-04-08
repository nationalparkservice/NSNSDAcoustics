# birdnet_results ===========================================================

#' @name birdnet_gather_results
#' @title Gather BirdNET results
#' @description Gather all BirdNET CSV results from a desired folder into one user-friendly data.table / data.frame.
#' @param results.directory Path to directory where raw BirdNET result CSVs have been stored.
#' @param formatted Logical indicating whether to gather formatted (see \code{\link{birdnet_format_csv}}) or unformatted (raw) BirdNET results. Default = TRUE. When FALSE, the function will gather only unformatted (raw) results.
#' @return Returns a data.frame/data.table with the following columns:
#'
#' If formatted = TRUE:
#' \itemize{
#' \item{\strong{recordingID}: Recording identifier for the file, as SITE_YYYYMMDD_HHMMSS.wav.}
#' \item{\strong{start.s}: Start time of detection in seconds }
#' \item{\strong{end.s}: End time of detection in seconds.}
#' \item{\strong{scientific.name}: Species scientific name.}
#' \item{\strong{common.name}: Species common name.}
#' \item{\strong{confidence}: BirdNET's confidence level in this detection ranging from 0 (least confident) to 1 (most confident).}
#' \item{\strong{verify}: A column into which verifications may be populated. When initially created, will be NA. After verifications are conducted, will contain items from a user-specified verification library (see \code{\link{birdnet_verify}})}
#' \item{\strong{timezone}: Timezone setting used in the audio recorder }
#' }
#'
#' If formatted = FALSE:
#' \itemize{
#' \item{\strong{Start (s)}: Start time of detection in seconds.}
#' \item{\strong{End (s)}: End time of detection in seconds.}
#' \item{\strong{Scientific name}: Species scientific name.}
#' \item{\strong{Common name}: Species common name.}
#' \item{\strong{Confidence}: BirdNET's confidence level in this detection ranging from 0 (least confident) to 1 (most confident).}
#' }
#'
#' @details
#'
#' This function was developed by the National Park Service Natural Sounds and Night Skies Division to gather results produced by BirdNET.

#'
#' @seealso  \code{\link{birdnet_run}}, \code{\link{birdnet_verify}}, \code{\link{birdnet_format_csv}}
#' @export
#' @import data.table
#' @examples
#' \dontrun{
#'
#' # Create a BirdNET results directory for this example
#' dir.create('example-results-directory')
#'
#' # Write examples of formatted BirdNET CSV outputs to example results directory
#' data(exampleFormatted1)
#' write.csv(x = exampleFormatted1,
#'           file = 'example-results-directory/BirdNET_formatted_Rivendell_20210623_113602.csv',
#'           row.names = FALSE, )
#'
#' data(exampleFormatted2)
#' write.csv(x = exampleFormatted2,
#'           file = 'example-results-directory/BirdNET_formatted_Rivendell_20210623_114602.csv',
#'           row.names = FALSE)
#'
#' # Write examples of raw BirdNET CSV outputs to example results directory
#' data(exampleBirdNET1)
#' write.csv(x = exampleBirdNET1,
#'           file = 'example-results-directory/BirdNET_Rivendell_20210623_113602.csv',
#'           row.names = FALSE, )
#'
#' data(exampleBirdNET2)
#' write.csv(x = exampleBirdNET2,
#'           file = 'example-results-directory/BirdNET_Rivendell_20210623_114602.csv',
#'           row.names = FALSE)
#'
#' # Gather formatted BirdNET results
#' formatted.results <- birdnet_gather_results(
#'                              results.directory = 'example-results-directory',
#'                              formatted = TRUE)
#'
#' # Gather unformatted (raw) BirdNET results
#' raw.results <- birdnet_gather_results(
#'                        results.directory = 'example-results-directory',
#'                        formatted = FALSE)
#'
#' # Delete all temporary example files when finished
#' unlink(x = 'example-results-directory', recursive = TRUE)
#' }
#'

birdnet_gather_results <- function(results.directory,
                                   formatted = TRUE) {
  ifelse(formatted == TRUE,
         paths <- list.files(path = results.directory,
                             pattern = 'formatted',
                             full.names = TRUE),
         paths <- grep(list.files(path = results.directory, full.names = TRUE),
                       pattern = 'formatted',
                       invert = TRUE, value = TRUE))
  dat <- suppressWarnings(rbindlist(lapply(paths, function(x) fread(x))))
  if (formatted == TRUE) dat[,verify := as.character(verify)]
  if (formatted == FALSE) {
    # If result has " in first and final columns, fix (this should only happen
    # for some reason with the example RData. It doesn't happen with the
    # real files... perhaps an issue with RData encoding.)
    if(any(colnames(dat) %in% c('"Start (s)', 'Confidence"'))) {
      colnames(dat)[1] <- 'Start (s)'
      colnames(dat)[5] <- 'Confidence'
      dat[,`Start (s)` := gsub('"', '', x = `Start (s)`)]
      dat[,`Confidence` := gsub('"', '', x = `Confidence`)]
    }

  }
  return(dat)
}
