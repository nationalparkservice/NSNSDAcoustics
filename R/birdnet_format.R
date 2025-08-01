# birdnet_format ===========================================================

#' @name birdnet_format
#' @title Format BirdNET outputs
#' @description Reformats BirdNET outputs with a "recordingID" column for easier
#'  data manipulation, a "verify" column to support manual verification of
#'  detection results, a "timezone" column to clarify the timezone setting
#'  used by the audio recorder, and R-friendly column headings.
#' @param results.directory Path to directory where raw BirdNET result files
#' have been stored. This directory should not contain anything aside from
#' BirdNET results. All
#' results in a single folder should either be .csv or .txt files from \code{\link{birdnet_analyzer}},
#' or .csv files generated by running BirdNET directly from the command line using
#' rtype = "r" (v1.5.1) or rtype = "csv" (v2+).
#' @param timezone Timezone setting used in the audio recorder (e.g, "GMT").
#' This argument allows you to specify the timezone shown by the wave filename.
#' If recordings were taken in local time at your study site, specify an
#' \href{https://en.wikipedia.org/wiki/List_of_tz_database_time_zones#List}{Olson-names-formatted character timezone}
#' for the location (e.g., "America/Los_Angeles"). If recordings were taken in
#' GMT, you can put either "GMT" or "UTC" (both are acceptable in R for downstream
#'  date-time formatting). This argument is critical to foster clarity in data
#'  analysis through the years and across monitoring locations, because some
#'  projects may vary across time and space as to whether the standard operating
#'   procedure specifies recordings in GMT vs. local time.
#' @return Saves a new formatted file of BirdNET results with filename inclusion
#'  "`_formatted_`" and suffix "csv" (or possibly "txt") depending on input.
#'
#' Output contains the following columns:
#'
#' \itemize{
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
#' \item{\strong{recordingID}: Recording identifier for the file, e.g., SITE_YYYYMMDD_HHMMSS.wav.}
#' \item{\strong{verify}: A column into which verifications may be populated. When initially created, will be 'NA'.}
#' \item{\strong{timezone}: Timezone setting used in the audio recorder.}
#' }
#'
#' @details
#'
#' This function was developed by the National Park Service Natural Sounds and
#' Night Skies Division to process audio data using BirdNET. It was developed
#' for use on Windows. CSV encodings may vary based on platform and result in errors.
#'
#' @seealso  \code{\link{birdnet_analyzer}}, \code{\link{birdnet_verify}}
#' @export
#' @import data.table
#' @importFrom tools file_ext
#' @examples
#' \dontrun{
#'
#' # Create a BirdNET results directory for this example
#' dir.create('example-results-directory')
#'
#'# Write examples of raw BirdNET outputs to example results directory
#' data(exampleBirdNET1)
#' write.table(
#'   x = exampleBirdNET1,
#'   file = 'example-results-directory/Rivendell_20210623_113602.BirdNET.results.r.csv',
#'   row.names = FALSE, quote = FALSE, sep = ','
#' )
#'
#' data(exampleBirdNET2)
#' write.table(
#'   x = exampleBirdNET2,
#'   file = 'example-results-directory/Rivendell_20210623_114602.BirdNET.results.r.csv',
#'   row.names = FALSE, quote = FALSE, sep = ','
#' )
#'
#' # Run birdnet_format:
#' birdnet_format(
#'   results.directory = 'example-results-directory',
#'   timezone = 'GMT'
#' )
#'
#' # Delete all temporary example files when finished
#' unlink(x = 'example-results-directory', recursive = TRUE)
#'
#' }
#'

birdnet_format <- function(
    results.directory,
    timezone
) {

  if(missing(timezone)) {
    stop('You are missing an input to the "timezone" argument. Please specify the timezone setting used in the audio recorder (e.g., "GMT", "America/Denver"). This argument allows you to declare the timezone shown by the wave filename. If recordings were taken in local time at your study site, specify an Olson-names-formatted character timezone (https://en.wikipedia.org/wiki/List_of_tz_database_time_zones#List) for the location (e.g., "America/Los_Angeles"). If recordings were taken in GMT, you can put either "GMT" or "UTC" (both are acceptable in R for downstream date-time formatting). This argument is critical to foster clarity in data analysis through the years and across monitoring locations, because some projects may vary across time and space as to whether the standard operating procedure specifies recordings in GMT vs. local time.')
  }

  if (grepl("\\/$", results.directory) == FALSE) {
    results.directory <- paste0(results.directory, '/')
  }

  # Find all unformatted BirdNET results
  fi <- list.files(path = results.directory,
                   pattern = 'BirdNET_|BirdNET.')

  # Check ext_type
  ext.type <- unique(file_ext(fi))

  if (length(ext.type) != 1) stop('Multiple file extension types found in folder. Please make sure results are all txt or all csv. Do not mix file types.')

  format <- fi[grep(pattern = '_formatted_', x = fi, fixed = TRUE)]
  fi <- fi[grep(pattern = '_formatted_', x = fi, invert = TRUE, fixed = TRUE)]
  fi <- fi[grep(pattern = 'problem', x = tolower(fi), invert = TRUE)] # remove "problem" files (pre v1)
  fi <- fi[grep(pattern = 'BirdNET_analysis_params', x = fi, invert = TRUE)] # remove analysis_params_file, if present (v2+)

  check.format <- gsub(pattern = '_formatted_',
                       replacement = '',
                       x = format,
                       fixed = TRUE)

  # Correct these if they came from command line names
  check.format <- gsub(pattern = 'BirdNETresults',
                    replacement = 'BirdNET.results',
                    x = check.format)

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
      if(x == 'n') return(message('Quitting birdnet_format.'))

    }
  }

  message('Formatting ', length(fi), ' files in ', results.directory, '...')
  for (i in 1:length(fi)) {

    message(i)
    finame <- paste0(results.directory, fi[i])

    if (ext.type == 'csv') {

      result <- suppressWarnings(
        fread(file = finame, header = TRUE, quote = "\"", sep = ','))

      v1.colnames <- c('filepath', 'start', 'end', 'scientific_name',
                       'common_name', 'confidence', 'lat', 'lon',
                       'week', 'overlap', 'sensitivity', 'min_conf',
                       'species_list', 'model')

      # Base columns for rtype csv in v2 (not including optional additional.columns)
      v2.colnames <- c('Start (s)', 'End (s)', 'Scientific name',
                       'Common name', 'Confidence', 'File')

      # Figure out if we are dealing with BirdNET Analyzer v1 or v2 input
      v1 <- all(colnames(result) %in% v1.colnames)
      v2 <- all(v2.colnames %in% colnames(result)) # switch %in% due to add'l opt cols

      # If CSV reads correctly, format and write
      if (v1|v2) {

        if(v2){result[,filepath := File]}

        # Add a recordingID column for friendlier downstream data wrangling
        catch.error <- tryCatch(
          recID <- basename(unique(result$filepath)), # this is more robust than guessing
          # but doesn't work if there are no results for the file. Annoying. =/,
          error = function(e) e
        )

        # Guess on the recID being wave if there are no results in the file
        if (inherits(catch.error, 'error')) {
          recID <- gsub(x = gsub(x = fi[i],
                                 pattern = 'BirdNET_|.BirdNET.results|.BirdNET.results.r',
                                 replacement = ''),
                        pattern = '.csv',
                        replacement = '.wav')
        } # end trycatch

        result[,recordingID := recID]

        # Add a verify column to support downstream manual QA of classifications
        result[,verify := as.character(NA)]

        # Clean up columns
        final.col.order <- c('recordingID', 'filepath', 'start', 'end', 'scientific_name',
                             'common_name', 'confidence', 'lat', 'lon',
                             'week', 'overlap', 'sensitivity', 'min_conf',
                             'species_list', 'model', 'verify')
        if (v1) {
          setcolorder(result, c('recordingID', setdiff(names(result), 'recordingID')))
          colnames(result) <- final.col.order
        }
        if (v2) {
          colnames(result)[colnames(result) %in% v2.colnames] <-
            c('start', 'end', 'scientific_name',
              'common_name', 'confidence', 'filepath')

          # Figure out if any of the metadata columns are missing and remove
          # from final.col.order if so:
          final.col.order.v2 <- final.col.order[which(final.col.order %in% colnames(result))]

          result <- result[, ..final.col.order.v2]

          missing.cols <- final.col.order[!final.col.order %in% colnames(result)]
          if(length(missing.cols) >= 1) {
            message('Your BirdNET data is missing the following optional columns: ', paste0(missing.cols, collapse = ', '),
                    '\nConsider rerunning your data through BirdNET and adding these optional columns using the default value in the `additional.columns` argument of `birdnet_analyzer()`.\n')
          }

        }

        # Add a timezone column to foster clarity across years with varying equipment
        # This timezone is the timestamp in the audio file name
        result[,timezone := timezone]

        # If no results, add one row of NA so that it can be clear in downstream
        # analysis that nothing was detected
        if (nrow(result) == 0) {

          row1 <- data.table(
            recordingID = recID,
            filepath = as.character(NA),
            start = as.numeric(NA),
            end = as.numeric(NA),
            scientific_name = as.character(NA),
            common_name = as.character(NA),
            confidence = as.numeric(NA),
            lat = as.numeric(NA),
            lon = as.numeric(NA),
            week = as.numeric(NA),
            overlap = as.numeric(NA),
            sensitivity = as.numeric(NA),
            min_conf = as.numeric(NA),
            species_list = as.character(NA),
            model = as.character(NA),
            verify = as.character(NA),
            timezone = as.character(NA)
          )

          if (v2) {
           # remove extraneous columns if not used in v2
            final.col.order.v2 <- c(final.col.order.v2, 'timezone')
            row1 <- row1[,..final.col.order.v2]
          }

          result <- rbind(result, row1)
        }

      } else {

        message('Found unformatted result ', fi[i], ' but there is a problem. Skipping to next...')
      }

      # Write formatted file
      newname <- paste0(results.directory,
                        gsub(x = basename(finame),
                             pattern = 'BirdNET_|BirdNET.results.|BirdNET.results.r.',
                             replacement = 'BirdNET_formatted_results.'))
      write.csv(x = result, file = newname, row.names = FALSE)

    } # end if csv


    # .txt type is deprecated from earlier version of BirdNET (pre 2024 and v1 GUI)
    # and shouldn't typically be encountered.
    if (ext.type == 'txt') {

      result <- suppressWarnings(
        fread(file = finame, header = TRUE, quote = "\"", sep = ','))

      txt.cols <- c('filepath', 'start', 'end', 'scientific_name', 'common_name',
                    'confidence', 'lat','lon', 'week', 'overlap', 'sensitivity',
                    'min_conf', 'species_list', 'model')

      # If txt reads correctly, format and write
      if (all(colnames(result) %in% txt.cols )) {

        if (nrow(result) == 0) {

          # If no results, add one row of NA so that it can be clear in
          # downstream analysis that nothing was detected
          row1 <- data.table(
            filepath = as.character(NA),
            start = as.numeric(NA),
            end = as.numeric(NA),
            scientific_name = as.character(NA),
            common_name = as.character(NA),
            confidence = as.numeric(NA),
            lat = as.numeric(NA),
            lon = as.numeric(NA),
            week = as.numeric(NA),
            overlap = as.numeric(NA),
            sensitivity = as.numeric(NA),
            min_conf = as.numeric(NA),
            species_list = as.character(NA),
            model = as.character(NA),
            # making assumption that it's wav
            recordingID = gsub(x = gsub(x = fi[i], pattern = 'BirdNET_', replacement = ''),
                               pattern = '.txt', replacement = '.wav'),
            verify = as.character(NA),
            timezone = as.character(NA)
          )
          result <- rbind(result, row1, fill = TRUE)
        } else {
          # If results are detected, add recordingID, verify, and timezone columns

          # Add a recordingID column for friendlier downstream data wrangling
          result[,recordingID := basename(filepath)]

          # Add a verify column to support downstream manual QA of classifications
          result[,verify := as.character(NA)]

          # Add a timezone column to foster clarity across years with varying equipment
          # This timezone is the timestamp in the audio file name
          result[,timezone := timezone]

        }

      } else {

        message('Found unformatted result ', fi[i], ' but there is a problem. Skipping to next...')
      }

      # Write formatted file
      newname <- paste0(results.directory,
                        gsub(x = basename(finame),
                             pattern = 'BirdNET_',
                             replacement = 'BirdNET_formatted_'))
      write.table(x = result, file = newname, row.names = FALSE, quote = FALSE, sep = ',')


    } # end if txt

  } # for length files

  message('Done. All BirdNET results in ', results.directory, ' are now formatted with a "recordingID" column for easier data manipulation, a "verify" column to support manual verification of detection results, a "timezone" column to clarify the timezone setting used by the audio recorder, and R-friendly column names.')
}

