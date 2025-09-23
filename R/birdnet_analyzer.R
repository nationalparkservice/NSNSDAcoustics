# birdnet_analyzer ====================================================
#' @name birdnet_analyzer
#' @title Process audio files through BirdNET
#' @description Process audio files through
#' \href{https://github.com/kahst/BirdNET-Analyzer}{BirdNET-Analyzer} via R on Windows. To use this function, follow the steps
#' outlined in the \href{https://github.com/nationalparkservice/NSNSDAcoustics/blob/main/README.md#running-birdnet-from-rstudio}{NSNSDAcoustics ReadME}.
#' See BirdNET Analyzer \href{https://birdnet-team.github.io/BirdNET-Analyzer/usage/cli.html#birdnet-analyzer-analyze}{usage documentation}
#' for more details. BirdNET Analyzer introduced breaking
#' changes with an update to v2 in 2025. This function
#'  attempts to retain capacity for both v1.5.1 and v2.2.0. Please
#'  \href{https://github.com/nationalparkservice/NSNSDAcoustics/issues}{submit an issue}
#'  if you discover problems.
#' @param birdnet.version Character name of BirdNET Analyzer release you are using.
#' E.g., `"v2.2.0"`, `"v1.5.1"`. Support for earlier versions is not prioritized. If the function doesn't
#' work, update to a new version of BirdNET Analyzer or \href{https://github.com/nationalparkservice/NSNSDAcoustics/blob/main/README.md#additional-tips-and-notes}{consider constructing the command statement
#' by hand} instead of using this function. See releases at \href{https://github.com/birdnet-team/BirdNET-Analyzer/releases}{https://github.com/birdnet-team/BirdNET-Analyzer/releases}.
#' \strong{CRITICAL NOTE}: The BirdNET "release" version specified in this argument
#' is different from the BirdNET model version. The only BirdNET model version
#' supported by this function is model version 2.4.
#' @param birdnet.path Absolute path to BirdNET-Analyzer.exe installation on your
#' machine. e.g., "C:/path/to/BirdNET-Analyzer/BirdNET-Analyzer.exe"
#' @param i.audio Absolute path to input audio file or audio folder.
#' @param o.results Absolute path to output folder.
#' @param fmin Minimum frequency for bandpass filter in Hz. Default = 0.
#' @param fmax Maximum frequency for bandpass filter in Hz. Default = 15000.
#' @param lat Recording location latitude. Set -1 to ignore. Default = -1.
#' @param lon Recording location longitude Set -1 to ignore. Default = -1.
#' @param week Week of the year when the recording was made. Values in c(1:48) (4 weeks per month).
#' Set -1 for year-round species list. Default = -1.
#' @param sf.thresh Minimum species occurrence frequency threshold for location
#' filter. Values from 0.0001 to 0.99. Default = 0.03.
#' @param slist Absolute path to species list text file or folder. If
#'  folder is provided, species list needs to be named "species_list.txt". If
#' lat and lon are provided, BirdNET-Analyzer will ignore this argument. See
#' \href{https://birdnet-team.github.io/BirdNET-Analyzer/best-practices/species-lists.html}{BirdNET-Analyzer documentation}
#' for details on formatting and creating a species list.
#' @param sensitivity Detection sensitivity. Higher values result in higher
#' sensitivity. Values from 0.5 to 1.5 for BirdNET Analyzer v1, and values
#' from 0.75 to 1.25 for BirdNET Analyzer v2. Default = 1.0.
#' Values other than 1.0 will shift the sigmoid function on the x-axis. Use
#' complementary to the cut-off threshold.
#' @param overlap Overlap of prediction segments in seconds. Values from 0.0 to 2.9. Default = 0.0.
#' @param audio.speed \strong{v2 ONLY}. Speed factor for audio playback. Values < 1.0 will slow
#' down the audio, values > 1.0 will speed it up. At a 10x decrease (audio speed 0.1),
#' a 384 kHz recording becomes a 38.4 kHz recording. Default = 1.0.
#' @param threads Number of CPU threads. Default = 2.
#' @param min.conf Minimum confidence threshold. Values from 0.00001 to 0.99.
#' Default = 0.1.
#' @param locale Locale for translated species common names. Values in `c("af", "en_UK", "de", "it", ...)`. Default = "en".
#' @param rtype Output format. Values in c('table', 'audacity', 'r', 'kaleidoscope', 'csv').
#'  Defaults to 'r', and strongly recommend using rtype 'r' if you want to use other
#'  functions in NSNSDAcoustics. V2.x: rtype r is no longer an option
#' @param batchsize Number of samples to process at the same time. Default = 1.
#' @param rtype Specifies output format. `Default = "csv"`. For v1, values in `c("table", "audacity", "r", "kaleidoscope", "csv")`.
#' For v2, values in `c("table", "audacity", "kaleidoscope", "csv")` -- note that
#' `rtype = "r"` is no longer an option. Users who previously
#' processed data through BirdNET Analyzer v1 can retain downstream
#' functionality for BirdNET Analyzer v2 in NSNSDAcoustics by using `rtype = "csv"`
#' and `additional.columns = c("lat","lon","week", "overlap","sensitivity","min_conf","species_list","model")`
#' @param additional.columns \strong{v2 ONLY}. Additional columns to include in the output, only
#' available if using `rtype = "csv"`. Values in `c("lat", "lon", "week", "overlap", "sensitivity", "min_conf", "species_list", "model")`. The default and recommended behavior is to add all columns. Put NULL if no additional columns are desired.
#' @param combine.results \strong{v2 ONLY}. In addition to individual files, also outputs a combined file for all the
#' selected result types. Default = FALSE.
#' @param classifier Absolute path to custom trained classifier.
#'  If using `classifier`, any inputs to `lat`, `lon`, and `locale` are ignored. Default = NULL.
#' @param skip.existing.results Skip files that have already been analyzed. Default = FALSE.
#' @param top.n Saves only the top N predictions for each segment independent of
#' their score. If used, `min.conf` threshold will be ignored.
#' @param merge.consecutive \strong{v2 ONLY}. Maximum number of consecutive detections above
#' `min.conf` to merge for each detected species. This will result in fewer entries
#'  in the result file with segments longer than 3 seconds. Set to 0 or 1 to
#'  disable merging. BirdNET Analyzer uses the mean of the top 3 scores from all
#'  consecutive detections for merging. If you want to merge all consecutive detections,
#'  try choosing a high numeric value.
#' @return Saves a file of results for each audio file in `o.results`.
#' Depending on your BirdNET release version, if using `rtype = "r"` (v1)
#' or `rtype = "csv"` (v2), files are csv with suffix `"BirdNET.results.csv"`
#' or `"BirdNET.results.r.csv"`.
#'
#' \strong{Output from V1:}
#'
#' \itemize{
#' \item{\strong{filepath}: Filepath for the processed audio file.}
#' \item{\strong{start}: Start time of detection in seconds.}
#' \item{\strong{end}: End time of detection in seconds.}
#' \item{\strong{scientific_name}: Species scientific name.}
#' \item{\strong{common_name}: Species common name.}
#' \item{\strong{confidence}: BirdNET's confidence level in this detection ranging from 0 (least confident) to 1 (most confident).}
#' \item{\strong{lat}: Latitude input used.}
#' \item{\strong{lon}: Longitude input used.}
#' \item{\strong{week}: Week of year input used.}
#' \item{\strong{overlap}: Overlap input used.}
#' \item{\strong{sensitivity}: Sensitivity input used.}
#' \item{\strong{min_conf}: Minimum confidence input used.}
#' \item{\strong{species_list}: Species list used.}
#' \item{\strong{model}: BirdNET model used.}
#' }
#'
#' \strong{Output from V2:} (optionally also includes lat, lon, week, overlap, sensitivity, min_conf,
#' species_list, and model, if indicated by `additional.columns` argument.)
#'
#' \itemize{
#' \item{\strong{Start (s)}: Start time of detection in seconds.}
#' \item{\strong{End (s)}: End time of detection in seconds.}
#' \item{\strong{File}: Filepath for the processed audio file.}
#' \item{\strong{Scientific name}: Species scientific name.}
#' \item{\strong{Common name}: Species common name.}
#' \item{\strong{Confidence}: BirdNET's confidence level in this detection ranging from 0 (least confident) to 1 (most confident).}
#' }
#
#'
#' @details
#'
#' This function was developed by the National Park Service Natural Sounds and
#' Night Skies Division to process audio data using BirdNET.
#'
#' To use this function, follow the steps outlined in the
#'  \href{https://github.com/nationalparkservice/NSNSDAcoustics/blob/main/README.md#running-birdnet-from-rstudio}{NSNSDAcoustics ReadME}.
#' Breaking changes were introduced in BirdNET Analyzer v2, and this function
#'  attempts to retain capacity both for v1.5.1 and v2. Please
#'  \href{https://github.com/nationalparkservice/NSNSDAcoustics/issues}{submit an issue}
#'  if you discover problems.
#'
#' @seealso  \code{\link{birdnet_format}}, \code{\link{birdnet_verify}}
#' @import tuneR
#' @importFrom lubridate week
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
#' # you follow the instructions given in the NSNSDAcoustics documentation here:
#' # https://github.com/nationalparkservice/NSNSDAcoustics/blob/main/README.md
#'
#' # Run all audio data in a directory through BirdNET
#' birdnet_analyzer(
#'  birdnet.version = 'v2.2.0',
#'  birdnet.path = 'absolute/path/AppData/Local/Programs/BirdNET-Analyzer/BirdNET-Analyzer.exe',
#'  i.audio = 'absolute/path/example-audio-directory',
#'  o.results = 'absolute/path/example-results-directory'
#' )
#'
#' # Delete all temporary example files when finished
#' unlink(x = 'example-audio-directory', recursive = TRUE)
#' unlink(x = 'example-results-directory', recursive = TRUE)
#' unlink(x = 'species_list.txt')
#'
#' }

birdnet_analyzer <- function(
    birdnet.version,
    birdnet.path,
    i.audio,
    o.results,
    fmin = 0,
    fmax = 15000,
    lat = -1,
    lon = -1,
    week = -1,
    sf.thresh = 0.03,
    slist = NULL,
    sensitivity = 1.0,
    overlap = 0.0,
    audio.speed  = 1.0,
    threads = 2,
    min.conf = 0.1, # default is now 0.25 in V2.x
    locale = 'en',
    batchsize = 1,
    rtype = 'csv',
    additional.columns = c('lat', 'lon', 'week',
                           'overlap', 'sensitivity',
                           'min_conf', 'species_list',
                           'model'),
    combine.results = FALSE,
    classifier = NULL,
    skip.existing.results = FALSE,
    top.n = NULL,
    merge.consecutive = 1
) {

  # If lat and long are provided, birdnet will ignore whatever is in the species_list.txt arg.
  if (!is.null(slist) & any(lat != -1, lon != -1)) {
    stop('You have entered a value for slist, but you have also entered values for lat and lon. Note that if you input values aside from -1 for lat and lon, these will apply an eBird-based filter and will override your species list. Please set lat and lon to -1 if you intend to use a species list in the slist argument. If you didn\'t mean to put something in the slist argument, set slist to NULL.')
  }

  if (!grepl(pattern = 'v2.2.0', x = birdnet.version)) {
    message('You are using a BirdNET version prior to BirdNET Analyzer v2. If you have input values for args: additional.columns, combine.results, top.n, and merge.consecutive, note that these are not supported by your BirdNET version and will be ignored. If you are having issues running the function, update BirdNET Analyzer to v2.')
  }

  # Generate command for birdnet based on version
  # Keep this until ~January 2026, then decommission v1.x support after
  # NPS staff have had a chance to transition to V2.x
  if (grepl(pattern = c('v1.1.|v1.2.|v1.3.|v1.5.'), x = birdnet.version)) {
    cmd <- paste0(
      '"', birdnet.path,
      '" --i "', i.audio,
      '" --o "', o.results,
      '" --lat ', lat,
      ' --lon ', lon,
      ' --week ', week,
      ' --slist "', slist,
      '" --sensitivity ', sensitivity,
      ' --min_conf ', min.conf,
      ' --overlap ', overlap,
      ' --rtype "', rtype,
      '" --threads ', threads,
      ' --batchsize ', batchsize,
      ' --locale "', locale,
      '" --sf_thresh ', sf.thresh,
      # Ensure classifier arg is not last, otherwise
      # it can't be stripped out if not used:
      ' --classifier "', classifier,
      '" --fmin ', fmin,
      ' --fmax ', fmax
    )
  }

  if (grepl(pattern = 'v2.2.0', x = birdnet.version)) {

    if (rtype == 'r') {
      stop("You input rtype = 'r' and a birdnet.version in v2.x. Note that rtype = 'r' is no longer an option in BirdNET Analyzer v2. For an equivalent output that will work with NSNSDAcoustics functions, use rtype = 'csv' and additional.columns = c('lat, 'lon', 'week', 'overlap', 'sensitivity', 'min_conf', 'species_list', 'model')")
    }

    if (sensitivity != 1) {
      # Provide message and warning for maximum visibility
      sens.msg.warn <- paste0('\nYou have input sensitivity = ', sensitivity, '. Note that BirdNET Analyzer introduced a breaking change with sensitivity in version 2! If you previously used non-1 values for sensitivity in version 1 of BirdNET Analyzer, note that you will not produce the same results in version 2. See more discussion on this topic here: https://github.com/birdnet-team/BirdNET-Analyzer/issues/758')
      message(sens.msg.warn)
      warning(sens.msg.warn)
    }

    if (!is.numeric(merge.consecutive)) {
      stop('If using merge.consecutive argument, please ensure you have entered a numeric value (e.g., 1, 10, 200...)')
    }

    # Ensure args like classifier and top.n are not last,
    # otherwise can't be stripped out if not used:
    cmd <- paste0(
      '"', birdnet.path,

      # In V2.x, audio path is an unnamed positional argument -- needs one space
      '" "', i.audio,
      '" --output "', o.results,
      '" --fmin ', fmin,
      ' --fmax ', fmax,
      ' --lat ', lat,
      ' --lon ', lon,
      ' --week ', week,
      ' --sf_thresh ', sf.thresh,
      ' --slist "', slist,
      '" --sensitivity ', sensitivity,
      ' --overlap ', overlap,
      ' --audio_speed ', audio.speed,
      ' --threads ', threads,
      ' --min_conf ', min.conf,
      ' --locale "', locale,
      '" --batch_size ', batchsize,
       ' --classifier "', classifier,
       '" --top_n ', top.n,
      ' --merge_consecutive ', merge.consecutive,
      ' --rtype "', rtype, '"'
    )

    # If desired, add additional.columns
    if (rtype == 'csv' ) {
      if (length(additional.columns) >= 1 &
          !is.null(additional.columns)) {
        formatted.string <- paste(shQuote(additional.columns), collapse = " ")
        cleaned.string <- gsub("^\"|\"$", "", formatted.string)
        cmd <- paste0(cmd, ' --additional_columns "', cleaned.string, '"')
      }
    }

    # Paste in command to combine results, if indicated
    if (combine.results == TRUE) {
      cmd <- paste0(cmd, ' --combine_results')
    }

    # Strip out top.n arg if not using
    if(is.null(top.n)) {
      cmd <- gsub(pattern = ' --top_n ', replacement = '', x = cmd)
    }
  }

  # Paste in command to skip existing results, if indicated
  if (skip.existing.results == TRUE) {
    cmd <- paste0(cmd, ' --skip_existing_results')
    message('You set skip.existing.results to TRUE, so no files with existing results in this folder will be analyzed.')
  }

  # If not using a custom classifier, remove classifier arg from cmd
  # Remember any args with risk of a NULL value should not be at the
  # end of the cmd statement, otherwise it will not be possible to
  # strip them out with a gsub
  if(is.null(classifier)) {
    cmd <- gsub(pattern = ' --classifier ""', replacement = '', x = cmd)
  }

  # Send command to BirdNET.exe
  run.cmd <- system(cmd)

  # Return useful messaging if command failure
  if (run.cmd == 2) {
    stop('BirdNET Analyzer command unsuccessful. If this function isn\'t working, it may be because you are using an older or non-compatible version of BirdNET-Analyzer. Try downloading the latest BirdNET version OR try using the alternative code provided in the NSNSDAcoustics ReadMe instructions available at https://github.com/nationalparkservice/nsnsdacoustics. Alternatively, submit an issue here: https://github.com/nationalparkservice/NSNSDAcoustics/issues')
  }

}

