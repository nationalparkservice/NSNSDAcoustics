# birdnet_format_csv ===========================================================

#' @name birdnet_format_csv
#' @title Format BirdNET csv outputs
#' @description tbd
#' @param results.directory Path to directory where raw BirdNET result CSVs have been stored
#' @param timezone Timezone setting used in the audio recorder (e.g, "GMT"). This argument allows you to specify the timezone shown by the wave filename. If recordings were taken in local time at your study site, specify an \href{https://en.wikipedia.org/wiki/List_of_tz_database_time_zones#List}{Olson-names-formatted character timezone} for the location (e.g., "America/Los_Angeles"). If recordings were taken in GMT, you can put either "GMT" or "UTC" (both are acceptable in R for downstream date-time formatting). This argument is critical to foster clarity in data analysis through the years and across monitoring locations, because some projects may vary across time and space as to whether the standard operating procedure specifies recordings in GMT vs. local time.
#' @return Saves a formatted CSV of results with the following columns:
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
#' @seealso  \code{\link{birdnet_run}}, \code{\link{birdnet_verify}}
#' @export
#' @examples
#' \dontrun{
#'
#' # tbd
#' }
#'

birdnet_format_csv <- function(results.directory,
                               timezone) {
  # NEED TO ADD IN TIMEZONE COLUMN?

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

  check.format <- gsub(pattern = 'formatted_', replacement = '',
                       x = format, fixed = TRUE)

  # Check for whether there are already formatted results for these names!
  # If so, those might have verifications and you don't want to overwrite them
  files.already.formatted <- which(fi %in% check.format)

  # Interactively ask user whether we should proceed:
  if (length(files.already.formatted) > 0) {
    x <- 'x'

    while (length(x) == 0 || !x %in% c("y", "n", NA)) {

      message(length(files.already.formatted), ' out of ', length(fi), ' files in this directory have already been formatted. Are you sure you want to proceed and overwrite these files? Make sure you won\'t lose verifications if so. To proceed with overwriting formatted files, input "y". To stop, input "n".')

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
    result <- data.table(read.csv(file = finame, header = TRUE, sep = ';'))

    # If CSV reads correctly and is missing a 'verify' col, format and write
    if (all(colnames(result) %in%
            c('Start..s.', 'End..s.', 'Scientific.name', 'Common.name', 'Confidence'))
        & !('verify' %in% colnames(result)))
    {
      # Add a recordingID column for friendlier downstream data wrangling
      recID <- gsub(x = gsub(x = fi[i], pattern = 'BirdNET_', replacement = ''),
                    pattern = '.csv', replacement = '.wav')
      result[,recordingID := recID]

      # Add a verify column to support downstream manual QA of classifications
      result[,verify := as.logical(NA)]

      # Clean up columns and write formatting file
      setcolorder(result, c('recordingID', setdiff(names(result), 'recordingID')))
      colnames(result) <- c('recordingID', 'start.s', 'end.s', 'scientific.name',
                            'common.name', 'confidence', 'verify')

      # Add a timezone column to foster clarity across years with varying equipment
      # This timezone is the timestamp in the audio file name
      result[,timezone := timezone]

      # If no results, add one row of NA so that it is clear nothing was detected
      if (nrow(result) == 0) {
        row1 <- data.table(recordingID = recID, start.s = as.numeric(NA),
                           end.s = as.numeric(NA), scientific.name = as.character(NA),
                           common.name = as.character(NA), confidence = as.numeric(NA),
                           verify = as.logical(NA), timezone = as.character(NA))
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

