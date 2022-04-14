# birdnet_run ==================================================================
# process wave files through BirdNET
#' @name birdnet_run
#' @title Run BirdNET from RStudio
#' @description This function uses the reticulate package to run Python from RStudio in order to process files through BirdNET. It assumes that all files in a folder come from the same site, and that the audio files are wave format and follow a SITEID_YYYYMMDD_HHMMSS.wav naming convention. To use this function, users must first successfully install \href{https://github.com/kahst/BirdNET-Lite}{BirdNET-Lite}, set up a \href{https://github.com/cbalantic/cbalantic.github.io/blob/master/_posts/2022-03-07-Install-BirdNET-Windows-RStudio.md#1-set-up-a-conda-environment}{conda environment for BirdNET}, and save \href{https://github.com/cbalantic/cbalantic.github.io/blob/master/_posts/2022-03-07-Install-BirdNET-Windows-RStudio.md#3-modify-the-birdnet-analyze-script}{a modified version of the BirdNET analyze.py file}. More details are available \href{https://cbalantic.github.io/Install-BirdNET-Windows-RStudio/}{here} (note that this does not serve as official guidance). \strong{Please input absolute paths for all directory arguments. This is necessary due to the way RStudio is communicating with the underlying Python code.} Note that the option to input a customized species list has not been implemented in this function.
#' @param audio.directory Absolute path to audio files to be processed. Files are expected to have the naming convention SITEID_YYYYMMDD_HHMMSS.wav. Default behavior for this function is to process every file in the audio.directory through BirdNET.
#' @param audio.files OPTIONAL character vector of specific file names to process within the audio.directory. If missing, all files in audio.directory will be processed.
#' @param results.directory Absolute path to directory where BirdNET results should be stored.
#' @param birdnet.directory Absolute path to directory where BirdNET is installed on your machine.
#' @param birdnet.script Name of Python file that will be sourced to run BirdNET (e.g., 'analyze.py')
#' @param lat Recording location latitude. Set -1 to ignore.
#' @param lon Recording location latitude. Set -1 to ignore.
#' @param ovlp Overlap in seconds between extracted spectrograms. Values from 0.0 to 2.9. Default = 0.0.
#' @param sens Detection sensitivity; higher values result in higher sensitivity. Values from 0.5 to 1.5. Default = 1.0.
#' @param min.conf Minimum confidence threshold. Values from 0.01 to 0.99. Default = 0.1.
#' @return Saves an unformatted CSV of results for each wave file in results.directory. Files have prefix "BirdNET_". If there is an issue with any audio files (e.g., wave file corrupt or too short), problematic files that were not processed will be recorded in a file named 'BirdNET_Problem-Files_DATE.csv'. Note that problem files may also occur if you have CSV results open from previous runs and are attempting to re-run the results while the CSV is still open; the program will not have write permissions to overwrite an open CSV file.
#'
#' Each CSV produced by BirdNET contains a single column with detections in semi-colon separated values of the following information:
#'
#' \itemize{
#' \item{\strong{Start (s)}: Start time of detection in seconds.}
#' \item{\strong{End (s)}: End time of detection in seconds.}
#' \item{\strong{Scientific name}: Species scientific name.}
#' \item{\strong{Common name}: Species common name.}
#' \item{\strong{Confidence}: BirdNET's confidence level in this detection ranging from 0 (least confident) to 1 (most confident).}
#' }
#'
#'
#'The following list provides an example of these outputs:
#' \itemize{
#' \item{Start (s);End (s);Scientific name;Common name;Confidence}
#' \item{3.0;6.0;Catharus ustulatus;Swainson's Thrush;0.7575617}
#' \item{3.0;6.0;Setophaga occidentalis;Hermit Warbler;0.1371203}
#' \item{3.0;6.0;Setophaga townsendi;Townsend's Warbler;0.11865085}
#' \item{15.0;18.0;Catharus ustulatus;Swainson's Thrush;0.95675665}
#' \item{15.0;18.0;Setophaga occidentalis;Hermit Warbler;0.15624857}
#' \item{21.0;24.0;Catharus ustulatus;Swainson's Thrush;0.43174374}
#' \item{24.0;27.0;Catharus ustulatus;Swainson's Thrush;0.6530585}
#' }
#'
#' @details
#'
#' This function was developed by the National Park Service Natural Sounds and Night Skies Division to act as a wrapper to process audio data using BirdNET. The example given in this function's documentation below will not run unless you have installed \href{https://github.com/kahst/BirdNET-Lite}{BirdNET-Lite}, set up a \href{https://github.com/cbalantic/cbalantic.github.io/blob/master/_posts/2022-03-07-Install-BirdNET-Windows-RStudio.md#1-set-up-a-conda-environment}{conda environment for BirdNET}, and saved a \href{https://github.com/cbalantic/cbalantic.github.io/blob/master/_posts/2022-03-07-Install-BirdNET-Windows-RStudio.md#3-modify-the-birdnet-analyze-script}{a modified version of the BirdNET analyze.py file}.
#'
#' @seealso  \code{\link{birdnet_format_csv}}, \code{\link{birdnet_verify}}
#' @import reticulate tuneR
#' @export
#' @examples
#' \dontrun{
#'
#' ##### View examples of CSV outputs produced by birdnet_run(): #####
#' data(exampleBirdNET1)
#' write.csv(x = exampleBirdNET1,
#'           file = 'BirdNET_Rivendell_20210623_113602.csv',
#'           row.names = FALSE)
#'
#' data(exampleBirdNET2)
#' write.csv(x = exampleBirdNET2,
#'           file = 'BirdNET_Rivendell_20210623_114602.csv',
#'           row.names = FALSE)
#'
#'
#'
#' ##### Please consider the following example as pseudocode ######
#'
#' # Because this function uses two external programs (Python and BirdNET-Lite),
#' # the example function below will not be modifiable to run for you unless
#' # you follow the instructions given in "Description".
#'
#' # Must set environment BEFORE calling in the reticulate package
#' Sys.setenv(RETICULATE_PYTHON = "C:/Your/Python/Path/Here/python.exe")
#' library(reticulate)
#'
#' # Set your conda environment
#' use_condaenv(condaenv = "pybirdnet", required = TRUE)
#'
#' # Create an audio directory for this example
#' dir.create('example-audio-directory')
#'
#' # Create a results directory for this example
#' dir.create('example-results-directory')
#'
#' # Read in example wave files
#' data(exampleAudio1)
#' data(exampleAudio2)
#'
#' # Write example waves to example audio directory
#' tuneR::writeWave(object = exampleAudio1,
#'                  filename = 'example-audio-directory/Rivendell_20210623_113602.wav')
#' tuneR::writeWave(object = exampleAudio2,
#'                  filename = 'example-audio-directory/Rivendell_20210623_114602.wav')
#'
#' # Run all audio data in a directory through BirdNET
#' birdnet_run(audio.directory = 'absolute/path/to/example-audio-directory',
#'             results.directory = 'absolute/path/to/example-results-directory',
#'             birdnet.directory = 'absolute/path/to/BirdNET',
#'             birdnet.script = 'BirdNET-Reticulate.py',
#'             lat = 46.09924,
#'             lon = -123.8765)
#'
#' # Use optional "audio.files" argument to process specific files
#' birdnet_run(audio.directory = 'absolute/path/to/example-audio-directory',
#'             audio.files = 'Rivendell_20210623_113602.wav',
#'             results.directory = 'absolute/path/to/example-results-directory',
#'             birdnet.directory = 'absolute/path/to/BirdNET',
#'             birdnet.script = 'BirdNET-Reticulate.py',
#'             lat = 46.09924,
#'             lon = -123.8765)
#'
#' # Delete all temporary example files when finished
#' unlink(x = 'example-audio-directory', recursive = TRUE)
#' unlink(x = 'example-results-directory', recursive = TRUE)
#'
#' }

birdnet_run <- function(audio.directory,   # absolute path for now
                        audio.files,
                        results.directory, # absolute path for now
                        birdnet.directory, # absolute path for now
                        birdnet.script,    # e.g. 'analyze.py'
                        lat,
                        lon,
                        ovlp = 0.0,
                        sens = 1.0,
                        min.conf = 0.1) {


  # TO DO: Integrate custom.list arg?

  # FIGURE OUT HOW TO DEAL WITH ABSOLUTE VS RELATIVE FILE PATH OF THE INPUT??
  # ... handled differently in windows and linux + the BirdNET analyze fxn... is there a robust workaround?
  # Or just make users put an absolute path?

  if (!requireNamespace("reticulate", quietly = TRUE)) {
    stop(
      "Package \"reticulate\" must be installed to use this function.",
      call. = FALSE
    )
  }

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
  #   to not use relative file paths)
  current.wd <- getwd()
  on.exit(setwd(current.wd))

  # Identify filepaths and recordingIDs
  rec.paths <- list.files(audio.directory, pattern = '.wav', recursive = TRUE)

  if(!missing(audio.files)) {
    rec.paths <- rec.paths[rec.paths %in% audio.files]
    paths.orig <- list.files(audio.directory, pattern = '.wav', recursive = TRUE)
    if(length(rec.paths) == 0) {
      stop('You have input something to the audio.files argument, but we can\'t locate audio files with names like ', audio.files[1],' in ', audio.directory, '. Did you mean something like ', paths.orig[1], '?')
    }
  }

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
  problem.list <- error.list <- list()

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
      message('There is a problem with ', recIDs[i], '; skipping to next\n')
      problem.list[[i]] <- recIDs[i]
      error.list[[i]] <- catch.error
      next
    } # end trycatch
  }

  # Return names of problematic files
  problem.files <- data.table(recordingID = unlist(problem.list))
  if (nrow(problem.files) > 0) {
    prob.name <- paste0(results.directory, 'BirdNET_Problem-Files_', as.character(Sys.Date()), '.csv')
    write.csv(x = problem.files, file = prob.name)
    message(nrow(problem.files), ' files could not be processed. This may be due to an issue with the wave file itself, you may have had a CSV with the same name open at the time of running the function, or there is an error in your BirdNET -> Python -> Reticulate setup. \nSee list of unprocessed files here: ', prob.name)
  }

  if (length(unlist(error.list)) > 0){
    # Return only unique errors
    unq.errs <- unique(error.list)
    message('\nReturning error list...')
    message('\n', unlist(unq.errs))
  }

  message('\nFINISHED! CSV results for each wave file are saved in ', results.directory, ' with the prefix "BirdNET_"')

}

