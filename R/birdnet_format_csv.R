# birdnet_format_csv ===========================================================

#' @name birdnet_format_csv
#' @title Format BirdNET csv outputs
#' @description tbd
#' @param results.directory Path to directory where raw BirdNET result CSVs have been stored
#' @param timezone Specify timezone setting used in the audio recorder (e.g, 'GMT'). If recordings were taken in local time at your study site, specify an \href{https://en.wikipedia.org/wiki/List_of_tz_database_time_zones#List}{Olson-names-formatted character timezone} for the location (e.g., 'America/Los_Angeles'). This is extremely important to foster clarity in data analysis through the years, as some projects have varied year to year in whether recordings were taken in GMT vs. local time.
#' @return Saves a formatted CSV of results with the following columns
#'
#' #' \itemize{
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

  if (grepl("\\/$", results.directory) == FALSE) {
    results.directory <- paste0(results.directory, '/')
  }

  # Find all unformatted BirdNET CSV results
  fi <- list.files(path = results.directory, pattern = 'BirdNET_')
  fi <- fi[grep('_formatted_', fi, invert = TRUE, fixed = TRUE)]

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

      # If no results, add one row of NA so that it is clear nothing was detected
      if (nrow(result) == 0) {
        row1 <- data.table(recordingID = recID, start.s = as.numeric(NA),
                           end.s = as.numeric(NA), scientific.name = as.character(NA),
                           common.name = as.character(NA), confidence = as.numeric(NA),
                           verify = as.logical(NA))
        result <- rbind(result, row1)
      }

      # Write formatted csv
      newname <- gsub(x = finame, pattern = 'BirdNET_', replacement = 'BirdNET_formatted_')
      write.csv(x = result, file = newname, row.names = FALSE)

    } else {
      message('Found unformatted result ', fi[i], ' but there is a problem. Make sure your folder only contains .CSVs with the prefix "BirdNET", and that you have not manually modified any of these files. Skipping to next...')
    }
  }
  message('Done. All BirdNET result CSVs in ', results.directory, ' are now formatted with cleaner column names, a "recordingID" column for easier data manipulation, and a "verify" column to support manual verification of detection results.')
}

