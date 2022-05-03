# birdnet_gather ===============================================================

#' @name birdnet_gather
#' @title Gather BirdNET results
#' @description Gather all BirdNET results from a desired folder into one user-friendly data.table / data.frame.
#' @param results.directory Path to directory where raw BirdNET results have been stored.
#' @param formatted Logical indicating whether to gather formatted (see \code{\link{birdnet_format}}) or unformatted (raw) BirdNET results. Default = TRUE. When FALSE, the function will gather only unformatted (raw) results.
#' @return Returns a data.frame/data.table.
#'
#' When input filetype is txt based on rtype = 'r', the following outputs are returned.
#'
#' If formatted = TRUE:
#'
#' \itemize{
#' \item{\strong{filepath}: Filepath for the processed audio file.}
#' \item{\strong{start}: Start time of detection in seconds.}
#' \item{\strong{end}: End time of detection in seconds.}
#' \item{\strong{scientific_name}: Species scientific name.}
#' \item{\strong{common_name}: Species common name.}
#' \item{\strong{confidence}: BirdNET's confidence level in this detection ranging from 0 (least confident) to 1 (most confident).}
#' \item{\strong{lat}: Latitude input used.}
#' \item{\strong{long}: Longitude input used.}
#' \item{\strong{week}: Week of year.}
#' \item{\strong{overlap}: Overlap input used.}
#' \item{\strong{sensitivity}: Sensitivity input used.}
#' \item{\strong{min_conf}: Minimum confidence input used.}
#' \item{\strong{species_list}: Species list used.}
#' \item{\strong{model}: BirdNET model used.}
#' \item{\strong{recordingID}: Recording identifier for the file, e.g., SITE_YYYYMMDD_HHMMSS.wav.}
#' \item{\strong{verify}: A column into which verifications may be populated. When initially created, will be 'NA'.}
#' \item{\strong{timezone}: Timezone setting used in the audio recorder.}
#' }
#'
#' When formatted = FALSE, it returns the same columns without the recordingID, verify, and timezone columns.
#'
#'
#'
#' When working with CSV files, the following ouptuts are returned.
#'
#' If formatted = TRUE:
#' \itemize{
#' \item{\strong{recordingID}: Recording identifier for the file, e.g., SITE_YYYYMMDD_HHMMSS.wav.}
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
#' This function was developed by the National Park Service Natural Sounds and Night Skies Division to gather results produced by BirdNET. It uses the data.table function fread for much faster reading of many csv or txt files.

#'
#' @seealso  \code{\link{birdnet_analyzer}}, \code{\link{birdnet_verify}}, \code{\link{birdnet_format}}
#' @export
#' @import data.table
#' @examples
#' \dontrun{
#'
#' # Create a BirdNET results directory for this example
#' dir.create('example-results-directory')
#'
#' # Write examples of formatted BirdNET outputs to example results directory
#' data(exampleFormatted1)
#' write.table(x = exampleFormatted1,
#'             file = 'example-results-directory/BirdNET_formatted_Rivendell_20210623_113602.txt',
#'             row.names = FALSE, quote = FALSE, sep = ',')
#'
#' data(exampleFormatted2)
#' write.table(x = exampleFormatted2,
#'             file = 'example-results-directory/BirdNET_formatted_Rivendell_20210623_114602.txt',
#'             row.names = FALSE, quote = FALSE, sep = ',')
#'
#' # Write examples of raw BirdNET outputs to example results directory
#' data(exampleBirdNET1)
#' write.table(x = exampleBirdNET1,
#'             file = 'example-results-directory/BirdNET_Rivendell_20210623_113602.txt',
#'             row.names = FALSE, quote = FALSE, sep = ',')

#' data(exampleBirdNET2)
#' write.table(x = exampleBirdNET2,
#'             file = 'example-results-directory/BirdNET_Rivendell_20210623_114602.txt',
#'             row.names = FALSE, quote = FALSE, sep = ',')
#'
#' # Gather formatted BirdNET results
#' formatted.results <- birdnet_gather(
#'                              results.directory = 'example-results-directory',
#'                              formatted = TRUE)
#'
#' # Gather unformatted (raw) BirdNET results
#' raw.results <- birdnet_gather(
#'                        results.directory = 'example-results-directory',
#'                        formatted = FALSE)
#'
#' # Delete all temporary example files when finished
#' unlink(x = 'example-results-directory', recursive = TRUE)
#' }
#'

birdnet_gather <- function(results.directory,
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
