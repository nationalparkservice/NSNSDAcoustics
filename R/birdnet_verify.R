# birdnet_verify ===============================================================


#' @name birdnet_verify
#' @title Verify BirdNET detections
#' @description tbd
#' @param data Data.table or data.frame of subsetted detections that a user would like to verify. This allows the user precise control over which detections to verify.
#' @param verification.library List specifying which verification options should be shown to the user. Allows finer control for user to specify whether a detection is a song, a call, a certain song or call type of interest, false alarm, unsure, or whatever the user needs. This allows maximum flexibility for the user, but also requires some thoughtfulness so that verification options remain consistent.
#' @param audio.directory tbd
#' @param results.directory Path to directory where raw BirdNET result CSVs have been stored
#' @param overwrite tbd
#' @param play tbd
#' @param buffer tbd
#' @param f.lim tbd
#' @param spec.col tbd
#' @return Saves a formatted CSV of results with the following columns, now updated to include any verifications by the user.
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
#'
#' Spectrograms show 3-second segment detected by BirdNET. Title of spectrogram indicates the recordingID of the file name, the start and end times of the detection in seconds, the species detection, and the confidence level of the detection from 0 to 1.
#'
#' @seealso  \code{\link{birdnet_run}}, \code{\link{birdnet_format_csv}}
#' @import tuneR
#' @export
#' @examples
#' \dontrun{
#'
#' # tbd
#' }
#'


birdnet_verify <- function(data,
                           verification.library,
                           audio.directory,
                           results.directory,
                           overwrite = FALSE,

                           # Args that customize the verification experience
                           play = FALSE,
                           buffer = 0, # maybe we do not want to allow a buffer?
                           # since we can't graph a box (no y coordinates)
                           f.lim = c(0, 12),
                           spec.col = monitoR::gray.3()
)
{

  if(missing(verification.library)) {
    stop('Please input verification.library argument. See ?birdnet_verify.')
  }


  # DO SOME CONSISTENCY CHECKING AND MESSAGING HERE TO ENSURE THAT ONLY
  # DATA THAT MATCH TO SPECIES IN THE INPUT LIBRARY ARE SHOWN TO THE VERIFIER
  # SUMMARIZE N RESULTS IN THE DATASET FOR THIS SPECIES NAME, ETC

  # ALSO -- a better way to "break" instead of esc?


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
      wav <- tuneR::readWave(wav.paths[w], from = t.start, to = t.end, units = 'seconds')
      reclen <- length(wav@left)/wav@samp.rate
      fft.data <- monitoR:::spectro(wave = wav)
      trec <- fft.data$time
      frec <- fft.data$freq
      arec <- fft.data$amp
      which.frq.bins <- which(frec >= f.lim[1] & frec <= f.lim[2])
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
        cat(paste0("\n This is recording ", rec.ids[w],
                   " This is verification ", counter,
                   " out of ",
                   nrow(all.focal.verify),
                   "\n"))

        cat(paste0("\n", i, ". Showing user input verification.library options for ",
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
          message("Value not recognized. Enter an option from your verification.library, or enter s, r, or q.\n")
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
