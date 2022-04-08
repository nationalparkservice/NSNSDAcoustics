# birdnet_verify ===============================================================

#' @name birdnet_verify
#' @title Verify BirdNET detections
#' @description Interactive function that produces spectrograms and wave clips enabling a user to verify BirdNET detections. Underlying CSV files are updated with user verifications.
#' @param data Data.table or data.frame of subsetted detections that a user would like to verify \strong{for a single species}. Takes formatted results only (see: \code{\link{birdnet_format_csv}}; must contain columns named recordingID, start.s, end.s, scientific.name, common.name, confidence, verify, and timezone.
#' @param verification.library List specifying which verification options should be shown to the user. Allows finer control to fit user's needs: user may specify whether a detection is a song, a call, a certain song or call type of interest, false alarm, unsure, or whatever the user needs. This enables maximum flexibility for the user, but also requires some thoughtfulness so that verification options remain consistent. BirdNET provides only species-level confidence and does not classify to song or call types. Thus, depending on expertise and familiarity with a focal species, a user may find themselves in a situation where songs are easily verified, but calls are not. Verification library provides the flexibility to accommodate varying levels of expertise, but must be thought through by the user beforehand.
#'
#' @param audio.directory Top-level input directory path to audio files to be processed. Files are expected to have the naming convention SITEID_YYYYMMDD_HHMMSS.wav.
#' @param results.directory Path to directory where formatted BirdNET result CSVs have been stored.
#' @param overwrite Logical flag for whether to overwrite existing verifications contained in a CSV. Default = FALSE. If FALSE, no overwriting occurs, and the user only verifies detections that are currently unverified. If TRUE, the user is choosing to overwrite existing verifications.
#' @param frq.lim Optional two-element numeric vector specifying frequency limits, in kHz, to apply to the plotted spectrograms. Default = c(0, 12).
#' @param play Logical value specifying whether a temporary wave file should be written to the working directory for the user to check during verification. If TRUE, a temporary wave file for the detection is written to the working directory, available for the user to listen to, and is deleted after the user closes the player window and adds a verification. If FALSE, no temporary wave file is written.
#' @param buffer Numeric buffer, in seconds, to place around BirdNET's 3 second detection area (default = 1). Generally useful for providing acoustic "context" around a detected event; of particular utility in cases where the target signal exceeds BirdNET's 3 second detection window, or in which the detection window overlaps with a partial signal.
#' @param box.col Color of border box drawn around 3-second detection area.
#' @param spec.col Default = gray.3(). The colors used to plot verification spectrograms. Spectrogram colors are adjustable, and users may create their own gradients for display. A few spectrogram color options are provided via the R package monitoR, including gray.1(), gray.2(()), gray.3(), rainbow.1(), and topo.1(), all of which are based on existing R colors.
#' @return Updates the 'verify' column of formatted BirdNET CSV files with user-input verifications.
#' @details
#'
#' This function was developed by the National Park Service Natural Sounds and Night Skies Division to process audio data using BirdNET. The "data" argument allows the user precise control over which detections to verify (e.g., a user could input only detections that exceed a desired confidence threshold, or only detections that have been identified for verification using a stratified sampling scheme).
#'
#'
#' Spectrograms show 3-second segment detected by BirdNET. Title of spectrogram indicates the recordingID of the file name, the start and end times of the detection in seconds, the species detection, and the confidence level of the detection from 0 to 1.
#'
#' @seealso  \code{\link{birdnet_run}}, \code{\link{birdnet_format_csv}}
#' @import tuneR
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
#' tuneR::writeWave(object = exampleAudio1,
#'                  filename = 'example-audio-directory/Rivendell_20210623_113602.wav')
#' tuneR::writeWave(object = exampleAudio2,
#'                  filename = 'example-audio-directory/Rivendell_20210623_114602.wav')
#'
#' # Create a BirdNET results directory for this example
#' dir.create('example-results-directory')
#'
#' # Write examples of formatted BirdNET CSV outputs to example results directory
#' data(exampleFormatted1)
#' write.csv(x = exampleFormatted1,
#'           file = 'example-results-directory/BirdNET_formatted_Rivendell_20210623_113602.csv',
#'           row.names = FALSE)
#'
#' data(exampleFormatted2)
#' write.csv(x = exampleFormatted2,
#'           file = 'example-results-directory/BirdNET_formatted_Rivendell_20210623_114602.csv',
#'           row.names = FALSE)
#'
#' # Gather formatted BirdNET results
#' dat <- birdnet_gather_results(results.directory = 'example-results-directory',
#'                              formatted = TRUE)
#'
#' # Create a random sample of three detections to verify
#' to.verify <- dat[common.name == "Swainson's Thrush"][sample(.N, 3)]
#'
#' # Create a verification library for this species
#' ver.lib <- list("Swainson's Thrush" = c('y', 'n', 'unsure'))

#' # Verify detections
#' birdnet_verify(data = to.verify,
#'                verification.library = ver.lib,
#'                audio.directory = 'example-audio-directory',
#'                results.directory = 'example-results-directory',
#'                overwrite = FALSE)
#'
#' # Check that underlying CSVs have been updated with user verifications
#' dat <- birdnet_gather_results(results.directory = 'example-results-directory',
#'                              formatted = TRUE)
#' dat[!is.na(verify)]
#'
#' # Delete all temporary example files when finished
#' unlink(x = 'example-audio-directory', recursive = TRUE)
#' unlink(x = 'example-results-directory', recursive = TRUE)
#'
#' }
#'

birdnet_verify <- function(data,
                           verification.library,
                           audio.directory,
                           results.directory,
                           overwrite = FALSE,

                           # Args that customize the verification experience
                           frq.lim = c(0, 12),
                           play = TRUE,
                           buffer = 1,
                           box.col = 'blue',
                           spec.col = monitoR::gray.3()
)
{

  # TO DO-- a better way to "break" instead of esc?


  if(missing(verification.library)) {
    stop('Please input verification.library argument. See ?birdnet_verify.')
  }

  if(length(unique(data$common.name)) > 1) {
    stop("Please input data for one species at a time. You have input a dataset with ", length(unique(data$common.name)), " species.")
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
  results <- birdnet_gather_results(results.directory = results.directory)

  results[,composite.key := paste(recordingID, start.s, end.s, common.name,
                                  sep = '-')]
  data[,composite.key := paste(recordingID, start.s, end.s, common.name,
                               sep = '-')]
  # Subset the results
  all.focal <- results[composite.key %in% data$composite.key]
  # if overwrite true..../false... need to change

  if (overwrite == FALSE) {
    cat('Since overwrite == FALSE, only detections from unverified results will be verified.')
    all.focal.verify <- all.focal[is.na(verify)]
  } else {
    cat('Since overwrite == TRUE, all detections will be verified, even if verification data already exists. This will overwrite any existing verifications.')
    all.focal.verify <- all.focal
  }

  # Only look at waves for verifications we need to do
  # Read in paths for all wavs in folder
  all.wav <- list.files(audio.directory, full.names = TRUE, recursive = TRUE)
  rec.ids <- unique(all.focal.verify$recordingID)
  wav.paths <- unique(grep(paste(rec.ids,collapse="|"),
                                  all.wav, value = TRUE))

  # Figure out which frequency bins to use
  checker <- readWave(filename = wav.paths[1], from = 0, to = 3, units = 'seconds')
  check.sp <- monitoR:::spectro(wave = checker)
  which.frq.bins <- which(check.sp$freq >= frq.lim[1] &
                            check.sp$freq <= frq.lim[2])
  counter <- 0
  verify.list <- list()
  for (w in 1:length(wav.paths)) {

    finame <- paste0(results.directory,
                     list.files(results.directory,
                                pattern = paste0('_formatted_',
                                                 gsub('.wav', '', rec.ids[w]))))
    verify <- all.focal.verify[recordingID == rec.ids[w]]
    ask <- FALSE
    oldask <- par(ask = par("ask"))
    on.exit(par(oldask))
    vers <- NULL

    for (i in 1:verify[,.N]) {

      # Narrow down verification.library options to this species
      ver.lib.sp <- unlist(verification.library[names(verification.library)
                                         == verify[i, common.name]])

      # Set up helpful spectrogram variables
      counter <- counter + 1
      x <- "x"
      t.start <- max(verify[i,start.s] - buffer, 0)
      t.end <- verify[i,end.s] + buffer # works even if end.s exceeds rec length
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
      xleft <- trec[which.min(abs(true.times.in.rec - verify[i,start.s]))]
      xright <- trec[which.min(abs(true.times.in.rec - verify[i,end.s]))]
      ylwr <- min(frec)
      yupr <- max(frec)
      polygon(x = c(xleft, xleft, xright, xright),
              y = c(ylwr, yupr, yupr, ylwr), border = box.col)
      axis(2, at = pretty(frec), labels = pretty(frec))
      axis(1, at = pretty(trec),
           labels = as.ITime(pretty(true.times.in.rec))[1:length(pretty(trec))])

      # Pull up audio clip for this annotation, if desired
      if (play) {
        fn <- paste0(getwd(), '/', 'temp-', verify$recordingID[i],
                     '-', verify$start.s[i])

        fn <- paste0(getwd(),
                     '/temp-',
                     gsub(pattern = '.wav',
                          replacement = '',
                          x = paste0(verify[i,c('recordingID', 'start.s', 'common.name')],
                                     collapse = '-')), '.wav')

        # hacky work around -- write to working directory as a temporary file,
        # you need to pull it up manually, then it will delete it when you are done.
        tuneR::writeWave(object = wav, filename = fn)

        message('\nWriting temporary file clip to ', fn, ' so that you can manually check it. Temporary file will be deleted after you are finished with this verification and have closed the player window.')

        # Can't get this to work:
        #tuneR::play(object = wav, ... = )
        #Sys.sleep(duration(wav))
      }

      while (length(x) == 0 || !x %in% c(ver.lib.sp, NA)) {
        cat(paste0("\n This is recording ", rec.ids[w], '.',
                   " This is verification ", counter,
                   " out of ",
                   nrow(all.focal.verify),
                   "\n"))

        cat(paste0("\n", i, ". Showing user input verification library options for ",
                   verify[i]$common.name,': ', paste0(ver.lib.sp, collapse = ', '),
                   "\n Enter an option in ", paste0(ver.lib.sp, collapse = ', '), ", s to skip, r to replay, or q to exit (q will exit and save any verifications you have already completed for this recording). To escape out of the function completely, press 'Esc': "))

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

        if (x %in% ver.lib.sp) {
          # Save the library label for this verification
          vers[i] <- x
        }

        if (x == 's') {
          message("Skipping to next verification.\n")
          break
        }

        if (x == 'r') {
          message("Replaying.\n")
        }

        if (x == 'q') {
          message("Exiting and saving what you have already verified.\n")
          break
        }

        if (!x %in% c(ver.lib.sp, "r", "s", "q")) {
          message("Value not recognized. Enter an option from your verification library, or enter s, r, or q.\n")
          next
        }

      } # end while x %in% ver.lib.sp or NA

      # If q, break out of the peaks loop
      if (!is.na(x) & x == 'q') break

      if (is.na(x) || x != "r")
        vers[i] <- x
      par(ask = ask)
      if (!is.na(x) && x == "r")
        i <- i - 1
      else i <- i + 1
      if (i < 1)
        i <- 1

      if (play) {
        file.remove(fn) # remove the temporary file
      }

    } # end for i in 1:verify[,.N]

    cat("\n")

    # Update verification labels in BirdNET_formatted_ results csv:
    update.composite <- verify$composite.key

    # If quit before the end of a recording, update labels for anything verified
    if (x == 'q') {
      update.composite <- verify$composite.key[seq_along(vers)]
    }

    # If there is anything to update
    if (length(update.composite) > 0) {
      verify[composite.key %in% update.composite, verify := vers]
      new.results <- results[recordingID == rec.ids[w]]
      new.results[composite.key %in% update.composite,
                  verify := vers]
      new.results[,composite.key := NULL]
      message('Updating ', basename(finame), ' with new verifications.')
      write.csv(x = new.results, file = finame, row.names = FALSE)
    } # end if length(update.these)

    cat("Finished verifying for this recording.\n")

    verify.list[[w]] <- verify

  } # end 'w' wave for loop

  verify.bind <- rbindlist(verify.list)

  # Return scores table updated with verifications
  return(verify.bind)
}
