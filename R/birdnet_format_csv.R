# birdnet_format_csv ===========================================================

#' @name birdnet_format_csv
#' @title Format BirdNET CSV outputs
#' @description Reformats BirdNET CSV outputs into separated columns with R-friendly column names, a "recordingID" column for easier data manipulation, a "verify" column to support manual verification of detection results, and a "timezone" column to clarify the timezone setting used by the audio recorder.
#' @param results.directory Path to directory where raw BirdNET result CSVs have been stored
#' @param timezone Timezone setting used in the audio recorder (e.g, "GMT"). This argument allows you to specify the timezone shown by the wave filename. If recordings were taken in local time at your study site, specify an \href{https://en.wikipedia.org/wiki/List_of_tz_database_time_zones#List}{Olson-names-formatted character timezone} for the location (e.g., "America/Los_Angeles"). If recordings were taken in GMT, you can put either "GMT" or "UTC" (both are acceptable in R for downstream date-time formatting). This argument is critical to foster clarity in data analysis through the years and across monitoring locations, because some projects may vary across time and space as to whether the standard operating procedure specifies recordings in GMT vs. local time.
#' @return Saves a new formatted CSV of BirdNET results with filename prefix "BirdNET_formatted_" and the following columns:
#'
#' \itemize{
#' \item{\strong{recordingID}: Recording identifier for the file, as SITE_YYYYMMDD_HHMMSS.wav.}
#' \item{\strong{start.s}: Start time of detection in seconds.}
#' \item{\strong{end.s}: End time of detection in seconds.}
#' \item{\strong{scientific.name}: Species scientific name.}
#' \item{\strong{common.name}: Species common name.}
#' \item{\strong{confidence}: BirdNET's confidence level in this detection ranging from 0 (least confident) to 1 (most confident).}
#' \item{\strong{verify}: A column into which verifications may be populated. When initially created, will be 'NA'.}
#' \item{\strong{timezone}: Timezone setting used in the audio recorder.}
#' }
#'
#' @details
#'
#' This function was developed by the National Park Service Natural Sounds and Night Skies Division to process audio data using BirdNET. It was developed for use on Windows. CSV encodings may vary based on platform and result in errors.

#'
#' @seealso  \code{\link{birdnet_run}}, \code{\link{birdnet_verify}}
#' @export
#' @import data.table
#' @examples
#' \dontrun{
#'
#' # Create a BirdNET results directory for this example
#' dir.create('example-results-directory')
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
#' # Run birdnet_format_csv:
#' birdnet_format_csv(results.directory = 'example-results-directory',
#'                    timezone = 'GMT')
#'
#' # Delete all temporary example files when finished
#' unlink(x = 'example-results-directory', recursive = TRUE)
#'
#' }
#'

birdnet_format_csv <- function(results.directory,
                               timezone) {

  # Consider wrapping this into birdnet_run() so that we don't need an additional step?
  # I don't think so though; leaving it more modularity gives people more options?

  if(missing(timezone)) {
    stop('You are missing an input to the "timezone" argument. Please specify the timezone setting used in the audio recorder (e.g., "GMT", "America/Denver"). This argument allows you to declare the timezone shown by the wave filename. If recordings were taken in local time at your study site, specify an Olson-names-formatted character timezone (https://en.wikipedia.org/wiki/List_of_tz_database_time_zones#List) for the location (e.g., "America/Los_Angeles"). If recordings were taken in GMT, you can put either "GMT" or "UTC" (both are acceptable in R for downstream date-time formatting). This argument is critical to foster clarity in data analysis through the years and across monitoring locations, because some projects may vary across time and space as to whether the standard operating procedure specifies recordings in GMT vs. local time.')
  }

  if (grepl("\\/$", results.directory) == FALSE) {
    results.directory <- paste0(results.directory, '/')
  }

  # Find all unformatted BirdNET CSV results
  fi <- list.files(path = results.directory, pattern = 'BirdNET_')
  format <- fi[grep(pattern = '_formatted_', x = fi, fixed = TRUE)]
  fi <- fi[grep(pattern = '_formatted_', x = fi, invert = TRUE, fixed = TRUE)]
  fi <- fi[grep(pattern = 'problem', x = tolower(fi), invert = TRUE)] # remove "problem" files csv

  check.format <- gsub(pattern = 'formatted_', replacement = '',
                       x = format, fixed = TRUE)

  # Check for whether there are already formatted results for these names!
  # If so, those might have verifications and you don't want to overwrite them
  files.already.formatted <- which(fi %in% check.format)

  # Interactively ask user whether we should proceed:
  if (length(files.already.formatted) > 0) {
    x <- 'x'

    while (length(x) == 0 || !x %in% c("y", "n", NA)) {

      message('**WARNING**: \n', length(files.already.formatted), ' out of ', length(fi), ' files in this directory have already been formatted.\n Are you sure you want to proceed and overwrite these files?\n IF SO, YOU WILL LOSE ANY EXISTING VERIFICATIONS YOU HAVE ALREADY COMPLETED. \n To proceed with overwriting formatted files, input "y". To stop, input "n".\n')

      x <- tolower(readLines(n = 1)[1])

      if (length(x) == 0 ) {
        cat("\nYou didn't enter a response.\n")
        next
      }

      if(x == 'y') break
      if(x == 'n') return(message('Quitting birdnet_format_csv.'))

    }
  }

  message('Formatting ', length(fi), ' files in ', results.directory, '...')
  for (i in 1:length(fi)) {
    message(i)
    finame <- paste0(results.directory, fi[i])
    result <- suppressWarnings(
      fread(file = finame, header = TRUE, quote = "\"", sep = ';'))

    # If result has " in first and final columns, fix (this should only happen
    # for some reason with the example RData. It doesn't happen with the
    # real files... perhaps an issue with RData encoding.)
    if(any(colnames(result) %in% c('"Start (s)', 'Confidence"'))) {
      colnames(result)[1] <- 'Start (s)'
      colnames(result)[5] <- 'Confidence'
      result[,`Start (s)` := gsub('"', '', x = `Start (s)`)]
      result[,`Confidence` := gsub('"', '', x = `Confidence`)]
    }


  # testerfiname <- (r'(C:\Users\cbalantic\OneDrive - DOI\BirdNET-guidance\birdnet-sandbox\Results\LEWI\BirdNET_LEWICLAT_20210803_063002.csv)')

   # If CSV reads correctly, format and write
    if (all(colnames(result) %in%
            c("Start (s)", "End (s)", "Scientific name",
              "Common name", "Confidence")))
    {

      # Add a recordingID column for friendlier downstream data wrangling
      recID <- gsub(x = gsub(x = fi[i], pattern = 'BirdNET_', replacement = ''),
                    pattern = '.csv', replacement = '.wav')
      result[,recordingID := recID]

      # Add a verify column to support downstream manual QA of classifications
      result[,verify := as.character(NA)]

      # Clean up columns and write formatting file
      setcolorder(result, c('recordingID', setdiff(names(result), 'recordingID')))
      colnames(result) <- c('recordingID', 'start.s', 'end.s', 'scientific.name',
                            'common.name', 'confidence', 'verify')

      # Add a timezone column to foster clarity across years with varying equipment
      # This timezone is the timestamp in the audio file name
      result[,timezone := timezone]

      # If no results, add one row of NA so that it is clear nothing was detected
      if (nrow(result) == 0) {
        row1 <- data.table(recordingID = recID,
                           start.s = as.numeric(NA),
                           end.s = as.numeric(NA),
                           scientific.name = as.character(NA),
                           common.name = as.character(NA),
                           confidence = as.numeric(NA),
                           verify = as.character(NA),
                           timezone = as.character(NA))
        result <- rbind(result, row1)
      }
      # Write formatted csv
      newname <- gsub(x = finame, pattern = 'BirdNET_', replacement = 'BirdNET_formatted_')
      write.csv(x = result, file = newname, row.names = FALSE)
    } else {
      message('Found unformatted result ', fi[i], ' but there is a problem. Make sure your folder only contains .CSVs with the prefix "BirdNET", and that you have not manually modified any of these files. Skipping to next...')
    }
  }
  message('Done. All BirdNET result CSVs in ', results.directory, ' are now formatted with cleaner column names, a "recordingID" column for easier data manipulation, a "verify" column to support manual verification of detection results, and a "timezone" column to clarify the timezone setting used by the audio recorder.')
}

