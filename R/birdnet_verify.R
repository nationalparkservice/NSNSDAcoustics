# birdnet_verify ===============================================================

#' @name birdnet_verify
#' @title Verify BirdNET detections
#' @description Interactive function that produces spectrograms and wave clips
#' enabling a user to verify BirdNET detections. Underlying files are updated
#' with user verifications.
#' @param data Data.table or data.frame of subsetted detections that a user would
#'  like to verify \strong{for a single species}. Accepts formatted results
#'  only (see: \code{\link{birdnet_format}}).
#' @param verification.library Character vector specifying which verification
#' options should be shown to the user. Allows finer control to fit user's needs:
#' user may specify whether a detection is a song, a call, a certain song or call
#' type of interest, false alarm, unsure, or whatever the user needs. This enables
#'  maximum flexibility for the user, but also requires some thoughtfulness so
#'  that verification options remain consistent. BirdNET provides only species-level
#'  confidence and does not classify to song or call types. Thus, depending on
#'  the underlying research question and a verifier's expertise and familiarity
#'  with a focal species, a user may find themselves in a situation where songs
#'  are easily verified, but calls are not. Verification library provides the
#'  flexibility to accommodate varying questions and levels of expertise, but
#'  must be thought through by the user beforehand.
#' @param audio.directory Top-level input directory path to audio files to be
#' processed. Files are expected to have the naming convention SITEID_YYYYMMDD_HHMMSS.wav.
#' @param results.directory Path to directory where formatted BirdNET results
#'  have been stored.
#' @param overwrite Logical flag for whether to overwrite existing verifications
#' contained in a txt or csv file. Default = FALSE. If FALSE, no overwriting occurs,
#'  and the user only verifies detections that are currently unverified. If TRUE,
#'   the user is choosing to overwrite existing verifications.
#' @param play Logical value specifying whether a temporary wave file should be
#'  written to the working directory for the user to check during verification.
#'  If TRUE, a temporary wave file for the detection is written to the working
#'  directory, available for the user to listen to, and is deleted after the
#'  user closes the player window and adds a verification. If FALSE, no temporary
#'   wave file is written.
#' @param frq.lim Optional two-element numeric vector specifying frequency limits,
#'  in kHz, to apply to the plotted spectrograms. Default = c(0, 12).
#' @param buffer Numeric buffer, in seconds, to place around BirdNET's 3 second
#' detection area (default = 1). Generally useful for providing acoustic "context"
#'  around a detected event; of particular utility in cases where the target signal
#'   exceeds BirdNET's 3 second detection window, or in which the detection window
#'    overlaps with a partial signal.
#' @param box.col Color of border box drawn around 3-second detection area.
#' @param spec.col The colors used to plot verification spectrograms. Default = gray.3().
#' Spectrogram colors are adjustable, and users may create their own gradients for
#'  display. A few spectrogram color options are provided via the R package monitoR,
#'  including gray.1(), gray.2(), gray.3(), rainbow.1(), and topo.1(), all of
#'  which are based on existing R colors.
#' @return Updates the 'verify' column of formatted BirdNET txt or csv files with
#'  user-input verifications.
#' @details
#'
#' This function was developed by the National Park Service Natural Sounds and
#' Night Skies Division to process audio data using BirdNET. The "data" argument
#'  allows the user precise control over which detections to verify (e.g., a user
#'  could input only detections that exceed a desired confidence threshold, or
#'  only detections that have been identified for verification using a stratified
#'  sampling scheme).
#'
#'
#' Spectrograms show 3-second segment detected by BirdNET. Title of spectrogram
#' indicates the recordingID of the file name, the start and end times of the
#' detection in seconds, the species detection, and the confidence level of the
#' detection from 0 to 1.
#'
#' MP3 files create issues in R unless you install 3rd party software (see
#' ?monitoR::readMP3 for details), so this function will operate very slowly on
#' MP3 files since they have to be converted to wave first. For a faster way to
#' deal with verification of MP3 files, consider using the BirdNET Analyzer GUI
#' "Segments" and "Review" options.
#'
#' @seealso  \code{\link{birdnet_analyzer}}, \code{\link{birdnet_format}}, \code{\link{birdnet_review_verify}}
#' @import data.table monitoR tuneR
#' @importFrom graphics par polygon axis
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
#'    object = exampleAudio1,
#'    filename = 'example-audio-directory/Rivendell_20210623_113602.wav'
#' )
#'
#' tuneR::writeWave(
#'    object = exampleAudio2,
#'    filename = 'example-audio-directory/Rivendell_20210623_114602.wav'
#' )
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
#'  )
#'
#' data(exampleFormatted2)
#' write.table(
#'   x = exampleFormatted2,
#'   file = 'example-results-directory/Rivendell_20210623_114602.BirdNET_formatted_results.csv',
#'   row.names = FALSE, quote = FALSE, sep = ','
#'  )
#'
#' # Gather formatted BirdNET results
#' dat <- birdnet_gather(
#'   results.directory = 'example-results-directory',
#'   formatted = TRUE
#' )
#'
#' # Create a random sample of three detections to verify
#' set.seed(4)
#' to.verify <- dat[common_name == "Swainson's Thrush"][sample(.N, 3)]
#'
#' # Create a verification library for this species
#' ver.lib <- c('y', 'n', 'unsure')
#'
#' # Verify detections
#' birdnet_verify(
#'   data = to.verify,
#'   verification.library = ver.lib,
#'   audio.directory = 'example-audio-directory',
#'   results.directory = 'example-results-directory',
#'   overwrite = FALSE,
#'   play = TRUE,
#'   frq.lim = c(0, 12),
#'   buffer = 1,
#'   box.col = 'blue',
#'   spec.col = monitoR::gray.3()
#' )
#'
#' # Check that underlying files have been updated with user verifications
#' dat <- birdnet_gather(
#'   results.directory = 'example-results-directory',
#'   formatted = TRUE
#' )
#'
#' dat[!is.na(verify)]
#'
#' # Delete all temporary example files when finished
#' unlink(x = 'example-audio-directory', recursive = TRUE)
#' unlink(x = 'example-results-directory', recursive = TRUE)
#'
#' }
#'

birdnet_verify <- function(
    data,
    verification.library,
    audio.directory,
    results.directory,
    overwrite = FALSE,
    play = TRUE,

    # Args that customize the verification experience
    frq.lim = c(0, 12),
    buffer = 1,
    box.col = 'blue',
    spec.col = monitoR::gray.3()
)
{

  # TO DO-- a better way to "break" instead of esc?

  if(missing(verification.library)) {
    stop('Please input verification.library argument. See ?birdnet_verify.')
  }

  if(!'verify' %in% colnames(data)) {
    stop('These data appear to be unformatted. To use this function, please format your data. See ?birdnet_format.')
  }

  if ('s' %in% verification.library) {
    stop('We noticed "s" in your verification library. This function reserves "s" for skip. Please choose a different label in your verification library.')
  }

  # Save existing working directory to reset it after
  #  function is done running:
  owd <- setwd(getwd())
  on.exit(setwd(owd))

  # Ensure forward slash at end ($) of directories
  if (grepl("\\/$", audio.directory) == FALSE) {
    audio.directory <- paste0(audio.directory, '/')
  }
  if (grepl("\\/$", results.directory) == FALSE) {
    results.directory <- paste0(results.directory, '/')
  }

  data <- as.data.table(data)
  setkey(data, recordingID)

  # Gather up all the data
  message('Gathering all results in results.directory...')

  # Check ext_type
  ext.type <- unique(file_ext(list.files(results.directory, recursive = TRUE)))
  if (length(ext.type) != 1) stop('Multiple file extension types found in folder. Please make sure results are all txt or all csv. Do not mix file types.')

  if(length(unique(data$common_name)) > 1) {
    stop("Please input data for one species at a time. You have input a dataset with ", length(unique(data$common_name)), " species.")
  }

  # Create composite key to track results
  results <- birdnet_gather(results.directory = results.directory)
  results[,composite.key := paste(recordingID, start, end, common_name, sep = '-')]
  data[,composite.key := paste(recordingID, start, end, common_name, sep = '-')]

  # Subset the results
  all.focal <- results[composite.key %in% data$composite.key]

  if (overwrite == FALSE) {
    message('Since overwrite == FALSE, only detections from unverified results will be verified.\n')
    all.focal.verify <- all.focal[is.na(verify)]
    if (nrow(all.focal.verify) == 0)
      stop('overwrite == FALSE and there are no more unverified detections in this data. Quitting function.')
  } else {
    message('Since overwrite == TRUE, all detections will be verified, even if verification data already exists. This will overwrite any existing verifications.\n')
    all.focal.verify <- all.focal
  }

  # Only look at waves for verifications we need to do
  # Read in paths for all wavs in folder
  all.wav <- list.files(audio.directory, full.names = TRUE, recursive = TRUE)
  rec.ids <- unique(all.focal.verify$recordingID) # these are the ones contained in the birdnet results -- if converted from mp3 to wav, will have a temp-* .wav extension but really be mp3

  # Check for recIDs that were converted from mp3 and find their real IDs
  og.rec.ids <- rec.ids
  correct.ids <- grep(pattern = 'temp-', x = rec.ids)
  rec.ids[correct.ids] <- gsub(pattern = '.wav|.WAV',
                               replacement = '.mp3',
                               x = rec.ids[correct.ids])
  rec.ids[correct.ids] <- gsub(pattern = 'temp-',
                               replacement = '',
                               x = rec.ids[correct.ids])

  wav.paths <- unique(grep(paste(rec.ids,collapse="|"),
                           all.wav, value = TRUE))

  # Correct for potential .wav/.WAV issue with audiomoth
  if (length(wav.paths) == 0) {
    correct.audio.moth <- gsub(x = rec.ids,
                               pattern = '.wav',
                               replacement = '.WAV',
                               ignore.case = FALSE)
    wav.paths <- unique(grep(paste(correct.audio.moth,
                                   collapse="|"),
                             all.wav, value = TRUE))
  }

  # Figure out which frequency bins to use
  # Unfortunately it is hard to deal with mp3 in R without installing 3rd party software, so we have to do this the hard/slow way (see ?monitoR::readMP3)
  check.file <- wav.paths[1]


  if (file_ext(check.file) == 'mp3') {

    message('It looks like there may be mp3 files in this audio folder, so we\'re checking on a few parameters. Thank you for your patience. NOTE: R and Windows, together, are not the best at handling mp3 files. If you need to use mp3 files instead of wave, then for a much speedier BirdNET validation workflow, we suggest running BirdNET directly from the command line and then using segments.py for validation as described here: https://github.com/kahst/BirdNET-Analyzer/')
    r <- readMP3(check.file)  ## MP3 file in working directory
    temp.file <- paste0(audio.directory, 'temp-',
                        gsub('.mp3', '.wav', basename(check.file),
                             ignore.case = TRUE))
    writeWave(r, temp.file, extensible = FALSE)
    check.file <- temp.file
    message('Done converting temporary wave file.')
  }

  checker <- readWave(filename = check.file, from = 0, to = 3, units = 'seconds')
  check.sp <- monitoR:::spectro(wave = checker) # monitoR:::
  which.frq.bins <- which(check.sp$freq >= frq.lim[1] &
                            check.sp$freq <= frq.lim[2])

  if(exists('temp.file')) unlink(temp.file) # get rid of temporary check.file

  # Start verifying
  counter <- 0
  verify.list <- list()
  for (w in 1:length(wav.paths)) {
    this.wav <- list.files(
      path = results.directory,
      pattern = gsub('.wav|.mp3|.WAV', '', gsub(pattern = 'temp-', '', og.rec.ids[w])),
      full.names = TRUE,
      recursive = TRUE)
    finame <- this.wav[grep(pattern = '_formatted_', x = this.wav)]
    verify <- all.focal.verify[recordingID == og.rec.ids[w]]
    ask <- FALSE
    oldask <- par(ask = par("ask"))
    on.exit(par(oldask))
    vers <- NULL

    is.mp3 <- file_ext(wav.paths[w]) == 'mp3'
    if (is.mp3) {
      # Unfortunately need to convert to wave
      message('This is an mp3. Converting to wave...')
      r <- readMP3(wav.paths[w])  ## MP3 file in working directory
      temp.file <- paste0(audio.directory, 'temp-',
                          gsub('.mp3', '.wav', basename(wav.paths[w]),
                               ignore.case = TRUE))
      writeWave(r, temp.file, extensible = FALSE)
      wav.paths[w] <- temp.file
      message('Done converting temporary wave file.')
    }

    for (i in 1:verify[,.N]) {

      # Set up helpful spectrogram variables
      counter <- counter + 1
      x <- "x"

      t.start <- max(verify[i,start] - buffer, 0)
      t.end <- verify[i,end] + buffer # works even if end.s exceeds rec length

      wav <- tuneR::readWave(wav.paths[w], from = t.start, to = t.end,
                             units = 'seconds')
      reclen <- length(wav@left)/wav@samp.rate
      fft.data <- monitoR:::spectro(wave = wav)
      trec <- fft.data$time
      frec <- fft.data$freq
      arec <- fft.data$amp
      frec <- frec[which.frq.bins]
      arec <- arec[which.frq.bins, ]

      # For plotting, save object to help label hh:mm:ss on x-axis
      trec.times <- as.ITime(trec)
      time.lab <- 'Time in recording (hh:mm:ss)'
      t.step <- trec[2] - trec[1]
      true.times.in.rec <- seq(from = t.start, to = t.end, by = t.step)[1-length(trec)]

      # Plot
      par(mfrow = c(1,1), mar = c(3,3,2,1), mgp = c(2,1,0))
      image(x = trec, y = frec, z = t(arec), col = spec.col,
            xlab = time.lab, ylab = "Frequency (kHz)", xaxt = "n",
            bty = 'n', axes = FALSE,
            main = paste0(verify[i, composite.key], ' [Conf. = ',
                          round(verify$confidence[i], 2), ']'))

      # Add a buffer box around the 3 second clip
      xleft <- trec[which.min(abs(true.times.in.rec - verify[i,start]))]
      xright <- trec[which.min(abs(true.times.in.rec - verify[i,end]))]
      ylwr <- min(frec)
      yupr <- max(frec)
      polygon(x = c(xleft, xleft, xright, xright),
              y = c(ylwr, yupr, yupr, ylwr), border = box.col)
      axis(2, at = pretty(frec), labels = pretty(frec))
      axis(1, at = pretty(trec),
           labels = as.ITime(pretty(true.times.in.rec))[1:length(pretty(trec))])

      # Write a temporary audio clip for this annotation, if desired
      if (play) {
        fn <- paste0(getwd(), '/', 'temp-', verify$recordingID[i],
                     '-', verify$start[i])

        fn <- paste0(getwd(),
                     '/temp-',
                     gsub(pattern = '.wav',
                          replacement = '',
                          x = paste0(verify[i,c('recordingID', 'start', 'common_name')],
                                     collapse = '-')), '.wav')


        # hacky work around -- write to working directory as a temporary file,
        # you need to pull it up manually, then it will delete it when you are done.
        tuneR::writeWave(object = wav, filename = fn)

        message('\nWriting temporary file clip to ', fn, ' so that you can manually check it. Temporary file will be deleted after you are finished with this verification and have closed the player window.')

        # Can't get this to work:
        #tuneR::play(object = wav, ... = )
        #Sys.sleep(duration(wav))
      }

      while (length(x) == 0 || !x %in% c(verification.library, NA)) {
        cat(paste0("\n This is recording ", rec.ids[w], '.',
                   " This is verification ", counter,
                   " out of ",
                   nrow(all.focal.verify),
                   ".\n"))

        cat(paste0("\n", i, ". Showing user input verification library options for ",
                   verify[i]$common_name,
                   ': ', paste0(verification.library, collapse = ', '),
                   "\n Enter an option in ", paste0(verification.library, collapse = ', '), ", s to skip, or q to exit this recording (q will exit and save any verifications you have already completed for this recording). If you have many recordings and wish to escape out of the function completely, press 'Esc': "))

        x <- tolower(readLines(n = 1)[1])

        if (length(x) == 0) {
          cat("\nYou didn't enter a response.\n")
          next
        }
        if (!is.na(x) && x == "na")
          x <- NA
        if (is.na(x)) {
          cat("NA\n")
          break
        }

        if (x %in% verification.library) {
          # Save the library label for this verification
          vers[i] <- x
        }

        if (x == 's') {
          message("Skipping to next verification.\n")
          vers[i] <- NA
          break
        }

        if (x == 'q') {
          message("Quitting out of this recording and saving what you have already verified for this recording.\n")
          break
        }

        if (!x %in% c(verification.library, "s", "q")) {
          message("Value not recognized. Enter an option from your verification library, or enter s or q.\n")
          next
        }

      } # end while x %in% ver.lib or NA

      # If q, break out of the peaks loop
      if (!is.na(x) & x == 'q') break

      if (play) {
        file.remove(fn) # remove the temporary file
      }

    } # end for i in 1:verify[,.N]

    cat("\n")

    # Update verification labels in BirdNET_formatted_ results file:
    update.composite <- verify$composite.key

    # If quit before the end of a recording, update labels for anything verified
    if (x == 'q') {
      update.composite <- verify$composite.key[seq_along(vers)]
    }

    # If there is anything to update
    if (length(update.composite) > 0) {
      verify[composite.key %in% update.composite, verify := vers]
      new.results <- results[recordingID == og.rec.ids[w]]
      new.results[composite.key %in% update.composite,
                  verify := vers]
      new.results[,composite.key := NULL]
      message('Updating ', basename(finame), ' with new verifications.')
      write.csv(x = new.results, file = finame, row.names = FALSE)
    } # end if length(update.these)

    cat("Finished verifying for this recording.\n")

    verify.list[[w]] <- verify

    if(exists('temp.file')) unlink(temp.file) # get rid of temporary check.file

  } # end 'w' wave for loop

  verify.bind <- rbindlist(verify.list)
  verify.bind[,composite.key := NULL]

  # Return scores table updated with verifications
  return(verify.bind)
}
