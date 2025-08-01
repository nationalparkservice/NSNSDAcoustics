# birdnet_gather ===============================================================

#' @name birdnet_gather
#' @title Gather BirdNET results
#' @description Gather all BirdNET results from a desired folder into one user-friendly
#' data.table / data.frame.
#' @param results.directory Path to directory where raw csv or txt BirdNET results
#'  using rtype = "r" (v1) or "csv" (v2) have been stored.
#' @param formatted Logical indicating whether to gather formatted
#' (see \code{\link{birdnet_format}}) or unformatted (raw) BirdNET results.
#' Default = TRUE. When FALSE, the function will gather only unformatted (raw) results.
#' @return Returns a data.frame/data.table.
#'
#' If formatted = TRUE:
#'
#' \itemize{
#' \item{\strong{recordingID}: Recording identifier for the file, e.g., SITE_YYYYMMDD_HHMMSS.wav.}
#' \item{\strong{filepath}: Filepath for the processed audio file.}
#' \item{\strong{start}: Start time of detection in seconds.}
#' \item{\strong{end}: End time of detection in seconds.}
#' \item{\strong{scientific_name}: Species scientific name.}
#' \item{\strong{common_name}: Species common name.}
#' \item{\strong{confidence}: BirdNET's confidence level in this detection ranging from 0 (least confident) to 1 (most confident).}
#' \item{\strong{lat}: Latitude input used (if present in the data).}
#' \item{\strong{long}: Longitude input used (if present in the data).}
#' \item{\strong{week}: Week of year (if present in the data).}
#' \item{\strong{overlap}: Overlap input used (if present in the data).}
#' \item{\strong{sensitivity}: Sensitivity input used (if present in the data).}
#' \item{\strong{min_conf}: Minimum confidence input used (if present in the data).}
#' \item{\strong{species_list}: Species list used (if present in the data).}
#' \item{\strong{model}: BirdNET model used (if present in the data).}
#' \item{\strong{verify}: A column into which verifications may be populated. When initially created, will be 'NA'.}
#' \item{\strong{timezone}: Timezone setting used in the audio recorder.}
#' }
#'
#' When formatted = FALSE, it returns whatever the input columns were.
#'
#' @details
#'
#' This function was developed by the National Park Service Natural Sounds and
#' Night Skies Division to gather results produced by BirdNET. It uses the
#' data.table function \code{\link{fread}} for much faster reading of many csv or txt files.
#'
#' @seealso \code{\link{birdnet_analyzer}}, \code{\link{birdnet_verify}}, \code{\link{birdnet_format}}
#' @export
#' @import data.table
#' @importFrom tools file_ext
#' @examples
#' \dontrun{
#'
#' # Create a BirdNET results directory for this example
#' dir.create('example-results-directory')
#'
#' # Write examples of formatted BirdNET outputs to example results directory
#' data(exampleFormatted1)
#' write.table(
#'    x = exampleFormatted1,
#'    file = 'example-results-directory/Rivendell_20210623_113602.BirdNET_formatted_results.csv',
#'    row.names = FALSE, quote = FALSE, sep = ','
#' )
#'
#' data(exampleFormatted2)
#' write.table(
#'    x = exampleFormatted2,
#'    file = 'example-results-directory/Rivendell_20210623_114602.BirdNET_formatted_results.csv',
#'    row.names = FALSE, quote = FALSE, sep = ','
#' )
#'
#' # Write examples of raw BirdNET outputs to example results directory
#' data(exampleBirdNET1)
#' write.table(
#'    x = exampleBirdNET1,
#'    file = 'example-results-directory/Rivendell_20210623_113602.BirdNET.results.r.csv',
#'    row.names = FALSE, quote = FALSE, sep = ','
#' )
#'
#' data(exampleBirdNET2)
#' write.table(
#'    x = exampleBirdNET2,
#'    file = 'example-results-directory/Rivendell_20210623_114602.BirdNET.results.r.csv',
#'    row.names = FALSE, quote = FALSE, sep = ','
#' )
#'
#' # Gather formatted BirdNET results
#' formatted.results <- birdnet_gather(
#'   results.directory = 'example-results-directory',
#'   formatted = TRUE
#' )
#'
#' # Gather unformatted (raw) BirdNET results
#' raw.results <- birdnet_gather(
#'   results.directory = 'example-results-directory',
#'   formatted = FALSE
#' )
#'
#' # Delete all temporary example files when finished
#' unlink(x = 'example-results-directory', recursive = TRUE)
#'
#' }
#'

birdnet_gather <- function(
    results.directory,
    formatted = TRUE
) {

  ifelse(formatted == TRUE,
         paths <- list.files(
           path = results.directory,
           pattern = 'formatted',
           full.names = TRUE,
           recursive = TRUE
         ),
         paths <- grep(list.files(
           path = results.directory,
           full.names = TRUE,
           recursive = TRUE
         ),
         pattern = 'formatted',
         invert = TRUE, value = TRUE))

  # Remove "problem" files, if present (pre v1)
  paths <- paths[grep(pattern = 'problem-files', x = tolower(paths), invert = TRUE)]
  paths <- paths[grep(pattern = 'desktop.ini', x = tolower(paths), invert = TRUE)]

  # Remove analysis_params_file, if present (v2+)
  paths <- paths[grep(pattern = 'birdnet_analysis_params', x = tolower(paths), invert = TRUE)]

  dat <- suppressWarnings(rbindlist(lapply(paths, function(x) fread(x))))

  if (length(dat) == 0) {
    stop('Not finding data in this results.directory -- are you sure the path is correct?')
  }

  if (formatted == TRUE) {

    if(!('verify' %in% colnames(dat))) {
      stop('Your data appear to be unformatted. Did you mean to set "formatted = FALSE" instead? See ?birdnet_gather for details.')
    }
    dat[,verify := as.character(verify)]
  }

  return(dat)
}
