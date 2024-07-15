# birdnet_analyzer ====================================================
#' @name birdnet_analyzer
#' @title Process audio files through BirdNET
#' @description This function allows a user to process audio files through \href{https://github.com/kahst/BirdNET-Analyzer}{BirdNET-Analyzer} via R's \code{\link{system}} function. It is meant for Windows users and may have unexpected results on other systems. To use this function, follow the steps outlined in the \href{https://github.com/nationalparkservice/NSNSDAcoustics/blob/main/README.md#running-birdnet-from-rstudio}{NSNSDAcoustics ReadME}. Please see \href{https://github.com/kahst/BirdNET-Analyzer}{BirdNET-Analyzer} usage documentation for more details.
#' @param birdnet.version Character name of BirdNET release you are using (options in c('v1.1.0', 'v1.2.0', 'previous')). If you downloaded a BirdNET .exe installer prior to that, try putting "previous" in the birdnet.version argument. If this doesn't work, update to a new version or consider constructing the command statement by hand instead of using this function. See releases at \href{https://github.com/kahst/BirdNET-Analyzer/releases}{https://github.com/kahst/BirdNET-Analyzer/releases}.
#' @param birdnet.path Absolute path to BirdNET-Analyzer.exe installation on your machine. e.g., "C:/path/to/BirdNET-Analyzer/BirdNET-Analyzer.exe"
#' @param i.audio Absolute path to input file or folder. If this is a file, o.results needs to be a file too.
#' @param o.results Absolute path to output file or folder. If this is a file, i.audio needs to be a file too.
#' @param lat Recording location latitude. Set -1 to ignore.
#' @param lon Recording location longitude Set -1 to ignore.
#' @param week Week of the year when the recording was made. Values in c(1:48) (4 weeks per month). Set -1 for year-round species list.
#' @param slist Absolute path to species list text file or folder. If only a folder is provided, species list needs to be named "species_list.txt". If lat and lon are provided, BirdNET-Analyzer will ignore this argument. See \href{https://github.com/kahst/BirdNET-Analyzer}{BirdNET-Analyzer} for details on formatting a species list.
#' @param sensitivity Detection sensitivity. higher values result in higher sensitivity. Values from 0.5 to 1.5. Default = 1.0.
#' @param min.conf Minimum confidence threshold. Values from 0.01 to 0.99. Default = 0.1.
#' @param overlap Overlap of prediction segments. Values from 0.0 to 2.9. Default = 0.0.
#' @param rtype Output format. Values in c('table', 'audacity', 'r', 'kaleidoscope', 'csv'). Defaults to 'r', and strongly recommend using rtype 'r' if you want to use other functions in NSNSDAcoustics.
#' @param threads Number of CPU threads.
#' @param batchsize Number of samples to process at the same time. Defaults to 1.
#' @param locale Locale for translated species common names. Values in c('af', 'de', 'it', ...). Defaults to 'en'.
#' @param sf.thresh Minimum species occurrence frequency threshold for location filter. Values from 0.01 to 0.99. Defaults to 0.03.
#' @param classifier Absolute path to custom trained classifier. Defaults to None. If set, lat, lon and locale are ignored.
#' @param fmin Minimum frequency for bandpass filter. Defaults to 0.
#' @param fmax Maximum frequency for bandpass filter. Defaults to 15000.
#' @param skip.existing.results Skip files that have already been analyzed. Default = FALSE.
#' @return Saves a file of results for each audio file in results.directory. For the recommended rtype = 'r', files are csv with suffix "BirdNET.results.csv". Files contain the following columns:
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
#' \item{\strong{week}: Week of year input used.}
#' \item{\strong{overlap}: Overlap input used.}
#' \item{\strong{sensitivity}: Sensitivity input used.}
#' \item{\strong{min_conf}: Minimum confidence input used.}
#' \item{\strong{species_list}: Species list used.}
#' \item{\strong{model}: BirdNET model used.}
#' }
#'
#' @details
#'
#' This function was developed by the National Park Service Natural Sounds and Night Skies Division to act as a wrapper to process audio data using BirdNET.
#' The function has been tested on BirdNET Analyzer \href{https://github.com/kahst/BirdNET-Analyzer/releases/tag/v1.1.0}{v1.1.0}.
#' To use this function, follow the steps outlined in the \href{https://github.com/nationalparkservice/NSNSDAcoustics/blob/main/README.md#running-birdnet-from-rstudio}{NSNSDAcoustics ReadME}.
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
#'# Create an audio directory for this example
#' dir.create('example-audio-directory')
#'
#' # Read in example wave files
#' data(exampleAudio1)
#' data(exampleAudio2)
#'
#' # Write example waves to example audio directory
#' tuneR::writeWave(
#'   object = exampleAudio1,
#'   filename = 'example-audio-directory/Rivendell_20210623_113602.wav'
#' )
#' tuneR::writeWave(
#'   object = exampleAudio2,
#'   filename = 'example-audio-directory/Rivendell_20210623_114602.wav'
#' )
#'
#' # Create a BirdNET results directory for this example
#' dir.create('example-results-directory')
#'
#' # Create a species_list.txt file for this example and write to working directory
#' data(exampleSpeciesList)
#' write.table(
#'    x = exampleSpeciesList,
#'    file = 'species_list.txt',
#'    row.names = FALSE,
#'    col.names = FALSE,
#'    quote = FALSE
#')
#'
#' ##### The following example is pseudocode ######
#'
#' # Because this function calls an external program (BirdNET-Analyzer.exe),
#' # the example function below will not be modifiable to run for you unless
#' # you follow the instructions given the NSNSDAcoustics documentation here:
#' # https://github.com/nationalparkservice/NSNSDAcoustics/blob/main/README.md
#'
#' # Run all audio data in a directory through BirdNET
#' birdnet_analyzer(
#'  birdnet.version = 'v1.1.0',
#'  birdnet.path = 'absolute/path/AppData/Local/Programs/BirdNET-Analyzer/BirdNET-Analyzer.exe',
#'  i.audio = 'absolute/path/example-audio-directory',
#'  o.results = 'absolute/path/example-results-directory',
#'  slist = 'C:/Users/cbalantic/OneDrive - DOI/Code-NPS/NSNSDAcoustics/species_list.txt',
#')
#'
#' # Delete all temporary example files when finished
#' unlink(x = 'example-audio-directory', recursive = TRUE)
#' unlink(x = 'example-results-directory', recursive = TRUE)
#'
#' }

birdnet_analyzer <- function(
    birdnet.version,
    birdnet.path,
    i.audio,
    o.results,
    lat = -1,
    lon = -1,
    week = -1,
    slist = NULL,
    sensitivity = 1.0,
    min.conf = 0.1,
    overlap = 0.0,
    rtype = 'r',
    threads = 1,
    batchsize = 1,
    locale = 'en',
    sf.thresh = 0.03,
    classifier = NULL,
    fmin = 0,
    fmax = 15000,
    skip.existing.results = FALSE
) {

  # If lat and long are provided, birdnet will ignore whatever is in the species_list.txt arg. Is this desired?
  if (!is.null(slist) & any(lat != -1, lon != -1)) {
    stop('You have entered a value for slist, but you have also entered values for lat and lon. Note that if you input values aside from -1 for lat and lon, these will apply an eBird-based filter and will override your species list. Please set lat and lon to -1 if you intend to use a species list in the slist argument. If you didn\'t mean to put something in the slist argument, set slist to NULL.')
  }

  # Generate command for birdnet based on version
  if (birdnet.version %in% c('v1.1.0', 'v1.2.0')) {
    cmd <- paste0(
      '"', birdnet.path,
      '" --i "', i.audio,
      '" --o "', o.results,
      '" --lat "', lat,
      '" --lon "', lon,
      '" --week "', week,
      '" --slist "', slist,
      '" --sensitivity "', sensitivity,
      '" --min_conf "', min.conf,
      '" --overlap "', overlap,
      '" --rtype "', rtype,
      '" --threads "', threads,
      '" --batchsize "', batchsize,
      '" --locale "', locale,
      '" --sf_thresh "', sf.thresh,
      '" --classifier "', classifier,
      '" --fmin "', fmin,
      '" --fmax "', fmax
    )

    # Paste in command to skip existing results, if indicated
    if (skip.existing.results == TRUE) {
      cmd <- paste0(cmd, '" --skip_existing_results')
      message('You set skip.existing.results to TRUE, so no files with existing results in this folder will be analyzed.')
    }
  }

  # If using a previous birdnet.version, do not include unsupported arguments
  if (birdnet.version == 'previous') {
    cmd <- paste0(
      '"', birdnet.path,
      '" --i "', i.audio,
      '" --o "', o.results,
      '" --lat "', lat,
      '" --lon "', lon,
      '" --week "', week,
      '" --slist "', slist,
      '" --sensitivity "', sensitivity,
      '" --min_conf "', min.conf,
      '" --overlap "', overlap,
      '" --rtype "', rtype,
      '" --threads "', threads,
      '" --batchsize "', batchsize,
      '" --locale "', locale,
      '" --classifier "', classifier,
      '" --sf_thresh "', sf.thresh
    )
  }

  # If not using a custom classifier, remove classifier arg from cmd
  # (BirdNET does not accept a NULL value for classifier, though it does for species list?)
  if(is.null(classifier)) {
    cmd <- gsub(pattern = ' --classifier ""', replacement = '', x = cmd)
  }

  # Send command to BirdNET.exe
  run.cmd <- system(cmd)

  # Return useful messaging if command failure?? although this shouldn't happen with the way I've recoded it
  if (run.cmd == 2) {
    stop('Found additional arguments not supported by your version of BirdNET. If this function isn\'t working, it may be because you are using an older or non-compatible version of BirdNET-Analyzer. Try downloading the latest BirdNET version OR try using the alternative code provided in the NSNSDAcoustics ReadMe instructions available at https://github.com/nationalparkservice/nsnsdacoustics.')
  }

}

