# birdnet_verify_segments ===============================================================

#' @name birdnet_verify_segments
#' @title (BETA) Verify BirdNET segment detections
#' @description (BETA) Interactive function that produces spectrograms and wave clips enabling a user to verify BirdNET "segments".
#' @param verification.library Character vector specifying which verification options should be shown to the user. Allows finer control to fit user's needs: user may specify whether a detection is a song, a call, a certain song or call type of interest, false alarm, unsure, or whatever the user needs. This enables maximum flexibility for the user, but also requires some thoughtfulness so that verification options remain consistent. BirdNET provides only species-level confidence and does not classify to song or call types. Thus, depending on the underlying research question and a verifier's expertise and familiarity with a focal species, a user may find themselves in a situation where songs are easily verified, but calls are not. Verification library provides the flexibility to accommodate varying questions and levels of expertise, but must be thought through by the user beforehand.
#' @param segments.directory Directory path for segments.
#' @param results.directory Directory path where you would like the output csv to be placed.
#' @param frq.lim Optional two-element numeric vector specifying frequency limits, in kHz, to apply to the plotted spectrograms. Default = c(0, 12).
#' @param spec.col The colors used to plot verification spectrograms. Default = gray.3(). Spectrogram colors are adjustable, and users may create their own gradients for display. A few spectrogram color options are provided via the R package monitoR, including gray.1(), gray.2(), gray.3(), rainbow.1(), and topo.1(), all of which are based on existing R colors.
#' @return Returns a CSV of user-input verifications.
#' @details
#'
#' This function was developed by the National Park Service Natural Sounds and Night Skies Division to process and explore audio data using BirdNET.
#'
#' Spectrograms show a segment detected by BirdNET. Title of spectrogram indicates the segment audio file name.
#'
#' MP3 files create issues in R unless you install 3rd party software (see ?monitoR::readMP3 for details), so this function will operate very slowly on MP3 files since they have to be converted to wave first. For a faster way to deal with verification of MP3 files, consider using the segments.py routine from the command line, as described at the \href{https://github.com/kahst/BirdNET-Analyzer}{BirdNET-Analyzer Github page}.
#'
#' @seealso  \code{\link{birdnet_analyzer}}
#' @import data.table monitoR tuneR
#' @importFrom graphics par polygon axis
#' @export
#' @examples
#' \dontrun{
#'
#' }
#'

birdnet_verify_segments <- function(
    verification.library,
    segments.directory,
    results.directory,
    frq.lim = c(0, 12),
    spec.col = monitoR::gray.3()
)
{

  if (!dir.exists(results.directory)) {
    dir.create(results.directory)
  }

  if(missing(verification.library)) {
    stop('Please input verification.library argument. See ?birdnet_verify_segments.')
  }

  # Save existing working directory to reset it after
  #  function is done running:
  owd <- setwd(getwd())
  on.exit(setwd(owd))

  # Ensure forward slash at end ($) of directories
  if (grepl("\\/$", segments.directory) == FALSE) {
    segments.directory <- paste0(segments.directory, '/')
  }
  if (grepl("\\/$", results.directory) == FALSE) {
    results.directory <- paste0(results.directory, '/')
  }

  # Set up segments verification table so that the ouptut
  # can be easily joined to birdnet _formatted_ result tables downstream
  sg <- data.table(segmentID = list.files(segments.directory))
  sg[,common_name := basename(segments.directory)]
  splt <- strsplit(x = sg$segmentID, split = '_')
  sg[, locationID :=  sapply(splt, '[[', 3)]
  sg[,confidence := sapply(splt, '[[', 1)]
  sg[,recordingID := paste0(
    sapply(splt, '[[', 3), '_',
    sapply(splt, '[[', 4), '_',
    sapply(splt, '[[', 5), '.',
    sapply(strsplit(x = sapply(splt, '[[', 7), split = '.', fixed = TRUE), '[[', 3)
  )]
  sg[,start := as.numeric(gsub(pattern = '.0s', replacement = '', x = unlist(lapply(splt, '[[', 6))))]
  sg[,end := as.numeric(gsub(pattern = '.0s.wav|.0s.WAV|.0s.mp3', replacement = '', x = unlist(lapply(splt, '[[', 7))))]
  sg[, verify := as.character(NA)]


  # Read in paths for all wavs in folder
  all.wav <- list.files(segments.directory, full.names = TRUE, recursive = TRUE)

  # Figure out which frequency bins to use
  # hard to deal with mp3 in R without installing 3rd party software, so we have to do this the hard/slow way (see ?monitoR::readMP3)
  wav.paths <- all.wav
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
  for (s in 1:nrow(sg)) {

    ask <- FALSE
    oldask <- par(ask = par("ask"))
    on.exit(par(oldask))
    vers <- NULL

    is.mp3 <- file_ext(wav.paths[s]) == 'mp3'
    if (is.mp3) {
      # Unfortunately need to convert to wave
      message('This is an mp3. Converting to wave...')
      r <- readMP3(wav.paths[s])  ## MP3 file in working directory
      temp.file <- paste0(segments.directory, 'temp-',
                          gsub('.mp3', '.wav', basename(wav.paths[s]),
                               ignore.case = TRUE))
      writeWave(r, temp.file, extensible = FALSE)
      wav.paths[w] <- temp.file
      message('Done converting temporary wave file.')
    }

    # Set up helpful spectrogram variables
    counter <- counter + 1
    x <- "x"
    wav <- tuneR::readWave(wav.paths[s],units = 'seconds')
    reclen <- length(wav@left)/wav@samp.rate
    fft.data <- monitoR:::spectro(wave = wav)
    trec <- fft.data$time
    frec <- fft.data$freq
    arec <- fft.data$amp
    frec <- frec[which.frq.bins]
    arec <- arec[which.frq.bins, ]

    # For plotting, save object to help label hh:mm:ss on x-axis
    trec.times <- as.ITime(trec)
    t.step <- trec[2] - trec[1]

    # Plot
    par(mfrow = c(1,1), mar = c(3,3,2,1), mgp = c(2,1,0))
    image(
      x = trec, y = frec, z = t(arec), col = spec.col,
      xlab = 'Time (s)',
      ylab = "Frequency (kHz)", xaxt = "n",
      bty = 'n', axes = FALSE,
      main = sg[s, segmentID]
    )
    axis(2, at = pretty(frec), labels = pretty(frec))
    axis(1, at = pretty(trec), labels = pretty(trec))

    while (length(x) == 0 || !x %in% c(verification.library, NA)) {
      cat(paste0("\n This is verification ", counter,
                 " out of ",
                 nrow(sg),
                 ".\n"))

      cat(paste0("\n", s, ". Showing user input verification library options: ",
                 paste0(verification.library, collapse = ', '),
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
        sg[s, verify := x]
      }

      if (x == 's') {
        message("Skipping to next verification.\n")
        break
      }

      if (x == 'q') {
        message("Quitting session.\n")
        break
      }

      if (!x %in% c(verification.library, "r", "s", "q")) {
        message("Value not recognized. Enter an option from your verification library, or enter s, r, or q.\n")
        next
      }

    } # end while x %in% ver.lib or NA

    # If q, break out of the peaks loop
    if (!is.na(x) & x == 'q') break

    if (is.na(x) || x != "r")
      sg[s, verify := x]
    par(ask = ask)
    if (!is.na(x) && x == "r")
      s <- s - 1
    else s <- s + 1
    if (s < 1)
      s <- 1


    cat("\n")

  } # end 's' wave for loop

  finame <- paste0(results.directory, '/', basename(segments.directory), '-results.csv')
  write.csv(x = sg, file = finame, row.names = FALSE)
  message('Segment verifications saved as: ', finame)


  cat("Finished verifying for this recording.\n")

  if(exists('temp.file')) unlink(temp.file) # get rid of temporary check.file

  # Return table updated with verifications
  return(sg)
}
