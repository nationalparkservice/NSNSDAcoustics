# birdnet_analyzer ====================================================
#' @name birdnet_analyzer
#' @title Run BirdNET-Analyzer from RStudio
#' @description This function uses the reticulate package to run Python from RStudio in order to process files through \href{https://github.com/kahst/BirdNET-Analyzer}{BirdNET-Analyzer}. It is meant for Windows users and may have unexpected results on other systems. To use this function, follow the steps outlined in the \href{https://github.com/nationalparkservice/NSNSDAcoustics#running-birdnet-from-rstudio-with-birdnet_analyzer}{NSNSDAcoustics ReadME}. In brief, these steps are to (1) Install BirdNET using the "Install BirdNET from zip" instructions at \href{https://github.com/kahst/BirdNET-Analyzer#setup-windows}{BirdNET-Analyzer Setup (Windows)}, (2) Download \href{https://www.anaconda.com/}{Anaconda}, (3) Set up a \href{https://github.com/nationalparkservice/NSNSDAcoustics/blob/main/README.md#3-set-up-a-conda-environment-for-birdnet-analyzer}{conda environment for BirdNET}, (4) Copy the "checkpoints" folder and eBird_taxonomy_codes_2021E.json file into your BirdNET conda environment folder. Function assumes that all files in a folder come from the same site, and that the audio files follow a SITEID_YYYYMMDD_HHMMSS naming convention. Please input absolute paths for all directory arguments (necessary due to the way RStudio is communicating with the underlying Python code). Note that BirdNET's option to input a customized species list has not been implemented in this function. Supported audio file types are wave and mp3. Please see \href{https://github.com/kahst/BirdNET-Analyzer}{BirdNET-Analyzer} usage documentation for more details.
#' @param audio.directory Absolute path to audio files to be processed. Files are expected to have the naming convention SITEID_YYYYMMDD_HHMMSS. Default behavior is to process every file in the audio.directory through BirdNET.
#' @param audio.files Optional character vector of specific file names to process within the audio.directory. If missing, all files in audio.directory will be processed.
#' @param start Optional file number in folder to start on if data processing fails or is interrupted. Default = 1. This argument provides an alternative to the 'audio.files' argument.
#' @param results.directory Absolute path to directory where BirdNET results should be stored.
#' @param birdnet.directory Absolute path to directory where BirdNET is installed on your machine.
#' @param lat Recording location latitude. Set -1 to ignore.
#' @param lon Recording location longitude Set -1 to ignore.
#' @param ovlp Overlap in seconds between extracted spectrograms. Values from 0.0 to 2.9. Default = 0.0.
#' @param sens Detection sensitivity; higher values result in higher sensitivity. Values from 0.5 to 1.5. Default = 1.0.
#' @param min.conf Minimum confidence threshold. Values from 0.01 to 0.99. Default = 0.1.
#' @param threads Number of CPU threads.
#' @param batchsize Number of samples to process at the same time. Defaults to 1.
#' @param locale Locale for translated species common names. Values in c('af', 'de', 'it', ...). Defaults to 'en'.
#' @param sf.thresh Minimum species occurrence frequency threshold for location filter. Values in c(0.01, 0.99). Defaults to 0.03.

#' @return Saves a txt file of results for each audio file in results.directory. Files have prefix "BirdNET_". Files contain the following columns:
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
#' }
#'
#' @details
#'
#' This function was developed by the National Park Service Natural Sounds and Night Skies Division to act as a wrapper to process audio data using BirdNET. The example given in this function's documentation below will not run unless you have set up BirdNET-Analyzer and a conda environment as conveyed in the Description.
#'
#'  BirdNET-Analyzer model versions 2.1 and 2.2 are currently supported. \href{https://github.com/kahst/BirdNET-Analyzer/tree/main/checkpoints}{View information on BirdNET version history.}
#'
#' The function can handle .wav or .mp3 audio files. The current behavior for .mp3 files is to convert to a temporary wave file for processing, and then delete the temporary file when finished. This behavior may not be necessary on all platforms and Python / conda installations.
#'
#' Internally, BirdNET-Analyzer expects a week of the year as an input. The behavior of birdnet_analyzer() is to parse the week of year from the SITEID_YYYYMMDD_HHMMSS filename using lubridate::week().
#'
#' If there is an issue with any audio files (e.g., audio file corrupt or too short), error messaging will be returned and problematic files that were not processed in this call to the function will be recorded in a file named 'BirdNET_Problem-Files_results.directory_YYYY-MM-DD HHMMSS.csv'. Note that problem files may also occur if you have results open from previous runs and are attempting to rewrite the results while the file is still open. To help diagnose problems, birdnet_analyzer() attempts to catch error messaging and return errors to the user at the end of the function run. However, internal error catching in R from BirdNET-Analyzer's underlying Python code does not always work; you may need to rely on the "Problem-Files" result to rerun problematic files and diagnose issues.
#'
#' NSNSDAcoustics suggests the reticulate package but does not install it for you. To use this function, please install reticulate using install.packages('reticulate').
#'
#' @seealso  \code{\link{birdnet_format}}, \code{\link{birdnet_verify}}
#' @import tuneR
#' @importFrom lubridate week
#' @importFrom reticulate py_run_string source_python
#' @importFrom tools file_ext
#' @export
#' @examples
#' \dontrun{
#'
#' # To view example outputs of raw txt BirdNET results, write to working directory
#' data(exampleBirdNET1)
#' write.table(x = exampleBirdNET1,
#'             file = 'BirdNET_Rivendell_20210623_113602.txt',
#'             row.names = FALSE, quote = FALSE, sep = ',')

#' data(exampleBirdNET2)
#' write.table(x = exampleBirdNET2,
#'             file = 'BirdNET_Rivendell_20210623_114602.txt',
#'             row.names = FALSE, quote = FALSE, sep = ',')
#'
#'
#' ##### The following example is pseudocode ######
#'
#' # Because this function uses two external programs (Python and BirdNET-Analyzer),
#' # the example function below will not be modifiable to run for you unless
#' # you follow the instructions given in "Description".
#'
#' # Must set environment BEFORE calling in the reticulate package
#' Sys.setenv(RETICULATE_PYTHON = "C:/Your/Python/Path/Here/python.exe")
#' library(reticulate)
#'
#' # Set your conda environment
#' use_condaenv(condaenv = "pybirdanalyze", required = TRUE)
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
#' birdnet_analyzer(audio.directory = 'absolute/path/example-audio-directory',
#'                  results.directory = 'absolute/path/example-results-directory',
#'                  birdnet.directory = 'absolute/path/BirdNET-Analyzer-main',
#'                  lat = 46.09924,
#'                  lon = -123.8765)
#'
#' # Use optional "audio.files" argument to process specific files
#' birdnet_analyzer(audio.directory = 'absolute/path/example-audio-directory',
#'                  audio.files = 'Rivendell_20210623_113602.wav',
#'                  results.directory = 'absolute/path/example-results-directory',
#'                  birdnet.directory = 'absolute/path/BirdNET-Analyzer-main',
#'                  lat = 46.09924,
#'                  lon = -123.8765)
#'
#' # Delete all temporary example files when finished
#' unlink(x = 'example-audio-directory', recursive = TRUE)
#' unlink(x = 'example-results-directory', recursive = TRUE)
#'
#' }

birdnet_analyzer <- function(audio.directory,   # absolute path for now
                             audio.files,
                             start = 1,
                             results.directory, # absolute path for now
                             birdnet.directory, # absolute path for now
                             lat = -1,
                             lon = -1,
                             ovlp = 0.0,
                             sens = 1.0,
                             min.conf = 0.1,
                             threads = 4,
                             batchsize = 1,
                             locale = 'en',
                             sf.thresh = 0.03) {

  # # # Set up for profiling
  audio.directory = 'C:/Users/cbalantic/OneDrive - DOI/Code-NPS/NSNSDAcoustics/example-audio-directory'
  results.directory = 'C:/Users/cbalantic/OneDrive - DOI/Code-NPS/NSNSDAcoustics/example-results-directory'
  birdnet.directory = 'C:/Users/cbalantic/OneDrive - DOI/BirdNET-Analyzer-main/'
  lat = 46.09924
  lon = -123.8765
  start = 1
  ovlp = 0.0
  sens = 1.0
  min.conf = 0.1
  threads = 4
  batchsize = 1
  locale = 'en'
  sf.thresh = 0.03




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

  # Figure out what BirdNET-Analyzer Version we are using
  birdnet.version <- list.files(
    path = paste0(birdnet.directory, 'checkpoints'),
    pattern = 'V2.'
  )

  # Read in the modified analyze.py script installed with the package
  if (birdnet.version == 'V2.1') {
    birdnet.script <- paste(system.file(package = "NSNSDAcoustics"),
                            "reticulate-analyze-april2022.py", sep = "/")
  }
  if (birdnet.version == 'V2.2') {
    birdnet.script <- paste(system.file(package = "NSNSDAcoustics"),
                            "reticulate-analyze-v2pt2.py", sep = "/")
  }

  # Get current working directory and make sure it is reset after function exits
  #   (to deal with fact that wd must be set to BirdNET python directory to
  #   run correctly from reticulate, unless you want to modify the .py file
  #   to not use relative file paths)
  current.wd <- getwd()
  on.exit(setwd(current.wd))

  # Identify filepaths and recordingIDs
  rec.paths <- list.files(audio.directory, pattern = '.wav|.mp3',
                          recursive = TRUE, ignore.case = TRUE)

  if (length(rec.paths) == 0){
    stop('No wave or mp3 audio files found in audio.directory.')
  }

  if(!missing(audio.files)) {
    rec.paths <- unique(grep(paste(audio.files,collapse = "|"),
                             rec.paths, value = TRUE))

    # This form only takes the full file path within top-level audio.directory
    # rec.paths <- rec.paths[rec.paths %in% audio.files]
    paths.orig <- list.files(audio.directory, pattern = '.wav|.mp3',
                             recursive = TRUE,
                             ignore.case = TRUE)

    if(length(rec.paths) == 0) {
      stop('You have input something to the audio.files argument. We either can\'t locate audio files with names like ', audio.files[1],' in ', audio.directory, ' or you have input an unsupported audio file type (only wave and mp3 are supported by this function). Did you mean something like ', paths.orig[1], '?')
    }
  }

    i.strings <- paste0(audio.directory, rec.paths) # absolute paths to recs
    recIDs <- basename(rec.paths)

    # Identify recording week
    wk <- week(as.Date(unlist(lapply(strsplit(x = recIDs, split = '_'), '[[', 2)),
                       format = '%Y%m%d'))

    rtype <- 'r'
    if (rtype == 'r') rext <- '.txt'

    # File path for results
    result.fp <- paste0(results.directory, 'BirdNET_',
                        unlist(lapply(strsplit(x = recIDs, split = '.',
                                               fixed = TRUE),
                                      '[[', 1)),
                        rext)

    # Loop through wav files to process through BirdNET
    error.list <- list()


    # IF USING REGULAR FOR LOOP
    for (i in start:length(recIDs)) {
      # First, check to see if the file is an mp3.
      # If it is, since I can't get my python libraries to process mp3, we are
      # going to convert it to a temporary wav file, and process that instead
      message('Working on ', i, ' of ', length(recIDs), ': ', recIDs[i], '\n')
      file <- i.strings[i]
      is.mp3 <- grepl('.mp3', file, ignore.case = TRUE)

      if (is.mp3) {
        message('This is an mp3. Converting to wave...')
        r <- readMP3(file)  ## MP3 file in working directory
        temp.file <- paste0(audio.directory, 'temp-',
                            gsub('.mp3', '.wav', recIDs[i],
                                 ignore.case = TRUE))
        writeWave(r, temp.file, extensible = FALSE)
        file <- temp.file
        message('Done converting temporary wave file.')
      }

      setwd(birdnet.directory)
      py_run_string(paste0("args_i = '", file, "'"))
      py_run_string(paste0("args_o = '", result.fp[i], "'"))
      py_run_string(paste0("args_lat = ", lat))
      py_run_string(paste0("args_lon = ", lon)) # slist ignored if lat long provided
      py_run_string(paste0("args_week = ", wk[i]))
      py_run_string(paste0("args_sensitivity = ", sens))
      py_run_string(paste0("args_min_conf = ", min.conf))
      py_run_string(paste0("args_overlap = ", ovlp))
      py_run_string(paste0("args_rtype = '", rtype, "'"))
      py_run_string(paste0("args_threads = ", threads))
      py_run_string(paste0("args_batchsize = ", batchsize))
      py_run_string(paste0("args_locale = '", locale, "'"))
      py_run_string(paste0("args_sf_thresh = ", sf.thresh))

      catch.error <- tryCatch(
        source_python(birdnet.script),
        error = function(e) e
      )

      if (inherits(catch.error, 'error')) {
        message('There is a problem with ', recIDs[i], '; skipping to next\n')
        error.list[[i]] <- catch.error
        if(exists('temp.file')) unlink(temp.file) # remove temporary wav if needed
        next
      } # end trycatch
      if (exists('temp.file')) unlink(temp.file) # remove temporary wav if needed
    }  # end for loop



    # IF USING FOREACH (parallel)
    # NOTE: this will not work, foreach doesn't work with reticulate
    if (parallel) {
      parallel::detectCores()

      n.cores <- parallel::detectCores() - 1

      #create the cluster
      my.cluster <- parallel::makeCluster(
        n.cores,
        type = "PSOCK"
      )

      #check cluster definition (optional)
      print(my.cluster)

      #register it to be used by %dopar%
      doParallel::registerDoParallel(cl = my.cluster)

      #check if it is registered (optional)
      foreach::getDoParRegistered()

      #how many workers are available? (optional)
      foreach::getDoParWorkers()


      foreach(i = start:length(recIDs)) %dopar% {
        # First, check to see if the file is an mp3.
        # If it is, since I can't get my python libraries to process mp3, we are
        # going to convert it to a temporary wav file, and process that instead
        message('Working on ', i, ' of ', length(recIDs), ': ', recIDs[i], '\n')
        file <- i.strings[i]
        is.mp3 <- grepl('.mp3', file, ignore.case = TRUE)

        if (is.mp3) {
          message('This is an mp3. Converting to wave...')
          r <- readMP3(file)  ## MP3 file in working directory
          temp.file <- paste0(audio.directory, 'temp-',
                              gsub('.mp3', '.wav', recIDs[i],
                                   ignore.case = TRUE))
          writeWave(r, temp.file, extensible = FALSE)
          file <- temp.file
          message('Done converting temporary wave file.')
        }

        setwd(birdnet.directory)
        py_run_string(paste0("args_i = '", file, "'"))
        py_run_string(paste0("args_o = '", result.fp[i], "'"))
        py_run_string(paste0("args_lat = ", lat))
        py_run_string(paste0("args_lon = ", lon)) # slist ignored if lat long provided
        py_run_string(paste0("args_week = ", wk[i]))
        py_run_string(paste0("args_sensitivity = ", sens))
        py_run_string(paste0("args_min_conf = ", min.conf))
        py_run_string(paste0("args_overlap = ", ovlp))
        py_run_string(paste0("args_rtype = '", rtype, "'"))
        py_run_string(paste0("args_threads = ", threads))
        py_run_string(paste0("args_batchsize = ", batchsize))
        py_run_string(paste0("args_locale = '", locale, "'"))
        py_run_string(paste0("args_sf_thresh = ", sf.thresh))

        catch.error <- tryCatch(
          source_python(birdnet.script),
          error = function(e) e
        )

        if (inherits(catch.error, 'error')) {
          message('There is a problem with ', recIDs[i], '; skipping to next\n')
          error.list[[i]] <- catch.error
          if(exists('temp.file')) unlink(temp.file) # remove temporary wav if needed
          next
        } # end trycatch
        if (exists('temp.file')) unlink(temp.file) # remove temporary wav if needed
      }  # end foreach


      parallel::stopCluster(cl = my.cluster)

    } # end if parallel




    # tryCatch does not always catch the errors due to how error handling
    # seems to have changed from BirdNET-Lite to BirdNET-Analyzer
    # So go through and do a comparison
    audio.ext <- file_ext(recIDs[start:length(recIDs)])
    wanted.to.process <- gsub('.wav|.mp3', '', recIDs[start:length(recIDs)], ignore.case = TRUE)
    processed <- gsub('.txt|.csv', '',
                      gsub('BirdNET_', '', list.files(path = results.directory),
                           ignore.case = TRUE),
                      ignore.case = TRUE
    )
    not.processed <- wanted.to.process[!(wanted.to.process %in% processed)]
    if(length(not.processed) > 0) {
      problem.files <-
        data.table(
          recordingID =
            paste0(not.processed, '.', audio.ext[which(!(wanted.to.process %in% processed))])
        )
    } else {
      problem.files <- data.table(recordingID = NULL)
    }

    # Return names of problematic files
    if (nrow(problem.files) > 0) {
      prob.name <- paste0(
        results.directory, 'BirdNET_Problem-Files_',
        basename(audio.directory), '_', gsub(':', '', as.character(Sys.time())),
        '.csv'
      )
      write.csv(x = problem.files, file = prob.name, row.names = FALSE)
      message(nrow(problem.files), ' files could not be processed. Possible reasons: there is an issue with the audio file itself (corrupt, too short), a txt or csv file with the same name was open at the time of running the function, you are processing audio data from an external hard drive and there is an issue with response, or there is an error in your BirdNET -> Python -> Reticulate setup. \nSee list of unprocessed files here: ', prob.name)
    }

    if (length(unlist(error.list)) > 0){
      # Return only unique errors
      unq.errs <- unique(error.list)
      message('\nReturning error list...')
      message('\n', unlist(unq.errs))
    }

    message('\nFINISHED! Results for each audio file are saved in ', results.directory, ' with the prefix "BirdNET_"\n')


 # })  # end profiler

}

