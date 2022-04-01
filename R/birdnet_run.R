# birdnet_run ==================================================================
# process wave files through BirdNET
#' @name birdnet_run
#' @title Run BirdNET from RStudio
#' @description This function uses the reticulate package to run Python from RStudio in order to process files through BirdNET. It assumes that all files in a folder come from the same site, and that the audio files are wave format and follow a SITEID_YYYYMMDD_HHMMSS.wav naming convention. To use this function, users must first successfully install \href{https://github.com/kahst/BirdNET-Lite}{BirdNET-Lite}, set up a conda environment, and make necessary modifications to the BirdNET analyze.py file. See \href{https://cbalantic.github.io/Install-BirdNET-Windows-RStudio/}{here} for one way to accomplish these steps. Please input absolute paths for directory arguments. This is necessary due to the way RStudio is communicating with the underlying Python code.
#' @param audio.directory Absolute path to top-level input directory path to audio files to be processed. Files are expected to have the naming convention SITEID_YYYYMMDD_HHMMSS.wav.
#' @param results.directory Absolute path to directory where BirdNET results should be stored. Logical flag to specify whether audio files are housed in 'Data' subdirectories
#' @param birdnet.directory Absolute path to directory where BirdNET is installed on your machine.
#' @param birdnet.script Name of Python file that will be sourced to run BirdNET (e.g., 'analyze.py')
#' @param lat Recording location latitude. Set -1 to ignore.
#' @param lon Recording location latitude. Set -1 to ignore.
#' @param ovlp Overlap in seconds between extracted spectrograms. Values from 0.0 to 2.9. Default = 0.0.
#' @param sens Detection sensitivity; higher values result in higher sensitivity. Values from 0.5 to 1.5. Default = 1.0.
#' @param min.conf Minimum confidence threshold. Values from 0.01 to 0.99. Default = 0.1.
#' @param custom.list Not incorporated yet! From BirdNET:  Path to text file containing a list of species. Not used if not provided.
#' @param timezone Specify timezone setting used in the audio recorder (e.g, 'GMT'). If recordings were taken in local time at your study site, specify an \href{https://en.wikipedia.org/wiki/List_of_tz_database_time_zones#List}{Olson-names-formatted character timezone} for the location (e.g., 'America/Los_Angeles'). This is extremely important to foster clarity in data analysis through the years, as some projects have varied year to year in whether recordings were taken in GMT vs. local time.
#' @return Saves an unformatted CSV of results for each wave file in results.directory. Files have prefix "BirdNET_". If there is an issue with any audio files (e.g., file corrupted), problematic files that were not processed will be recorded in a file named 'BirdNET_Problem-Files_DATE.csv'.
#'
#'
#' @details
#'
#' This function was developed by the National Park Service Natural Sounds and Night Skies Division to act as a wrapper to process audio data using BirdNET.

#'
#' @seealso  \code{\link{birdnet_format_csv}}, \code{\link{birdnet_verify}}
#' @import tuneR
#' @export
#' @examples
#' \dontrun{
#'
#'
#' Sys.setenv(RETICULATE_PYTHON = "C:/Your/Python/Path/Here/python.exe")
#' library(reticulate)
#' use_condaenv(condaenv = "pybirdnet", required = TRUE)
#'
#' # can't really do an reproducible example here since I don't know what their install situation is
#' # birdnet_run()
#'
#' }
#'
birdnet_run <- function(audio.directory, # absolute path for now
                        results.directory,
                        birdnet.directory, # absolute path for now e.g. 'C:/Users/cbalantic/OneDrive - DOI/CathleenPythonTest/'
                        birdnet.script, # e.g. 'analyze.py'
                        lat,
                        lon,
                        ovlp = 0.0,
                        sens = 1.0,
                        min.conf = 0.1,
                        custom.list = NULL) {

  # FIGURE OUT HOW TO DEAL WITH ABSOLUTE VS RELATIVE FILE PATH OF THE INPUT??
  # ... handled differently in windows and linux + the BirdNET analyze fxn... is there a robust workaround? Or just make users put an absolute path?

  # Ensure forward slash at end ($) of directories
  if (grepl("\\/$", audio.directory) == FALSE) {
    audio.directory <- paste0(audio.directory, '/')
  }
  if (grepl("\\/$", results.directory) == FALSE) {
    results.directory <- paste0(results.directory, '/')
  }
  if (grepl("\\/$", birdnet.directory) == FALSE) {
    birdnet.directory <- paste0(birdnet.directory, '/')
  }

  # Get current working directory and make sure it is reset after function exits
  #  (to deal with fact that wd must be set to BirdNET python directory to
  #   run correctly from reticulate, unless you want to modify the .py file
  #   to not use relative file paths
  current.wd <- getwd()
  on.exit(setwd(current.wd))

  # Identify filepaths and recordingIDs
  rec.paths <- list.files(audio.directory, pattern = '.wav', recursive = TRUE)
  i.strings <- paste0(audio.directory, rec.paths) # absolute paths to recs
  recIDs <- basename(rec.paths)

  # Identify recording week
  wk <- week(as.Date(unlist(lapply(strsplit(x = recIDs, split = '_'), '[[', 2)),
                     format = '%Y%m%d'))

  # File path for results
  result.fp <- paste0(results.directory, 'BirdNET_',
                      unlist(lapply(strsplit(x = recIDs, split = '.', fixed = TRUE),
                                    '[[', 1)),
                      '.csv')

  # Loop through wav files to process through BirdNET
  problem.list <- list()

  for (i in 1:length(recIDs)) {
    message('Working on ', i, ' of ', length(recIDs), ': ', recIDs[i], '\n')
    setwd(birdnet.directory)
    py_run_string(paste0("args_i = '", i.strings[i], "'"))
    py_run_string(paste0("args_lat = ", lat))
    py_run_string(paste0("args_lon = ", lon))
    py_run_string(paste0("args_week = ", wk[i]))
    py_run_string(paste0("args_overlap = ", ovlp))
    py_run_string(paste0("args_sensitivity = ", sens))
    py_run_string(paste0("args_min_conf = ", min.conf))
    py_run_string(paste0("args_o = '", result.fp[i], "'"))

    catch.error <- tryCatch(
      source_python(birdnet.script),
      error = function(e) e
    )

    if (inherits(catch.error, 'error')) {
      message('\nThere is a problem with ', recIDs[i], '; skipping to next')
      problem.list[[i]] <- recIDs[i]
      next
    } # end trycatch
  }

  # Return the names of problematic files
  problem.files <- data.table(recordingID = unlist(problem.list))
  if (nrow(problem.files) > 0) {
    prob.name <- paste0(results.directory, 'BirdNET_Problem-Files_', as.character(Sys.Date()), '.csv')
    write.csv(x = problem.files, file = prob.name)
    message(nrow(problem.files), ' files could not be processed. \nSee list of unprocessed files here: ', prob.name)
  }

  message('\nFINISHED! CSV results for each wave file are saved in ', results.directory, ' with the prefix "BirdNET_"')

}

