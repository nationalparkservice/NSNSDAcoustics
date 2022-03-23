# birdnet_verify ===============================================================


#' @name birdnet_verify
#' @title Verify BirdNET detections
#' @description tbd
#' @param results.directory Path to directory where raw BirdNET result CSVs have been stored
#' @param common.name tbd
#' @param audio.directory tbd
#' @param results.directory tbd
#' @param n.verifications tbd
#' @param confidence.threshold tbd
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
#' @seealso  \code{\link{birdnet_run}}, \code{\link{birdnet_format_csv}}
#' @import tuneR
#' @export
#' @examples
#' \dontrun{
#'
#' # tbd
#' }
#'


birdnet_verify <- function(recordingID,
                           common.name, # input species common name
                           audio.directory,
                           results.directory,
                           # how many vers you want to do (will be randomly
                           # sampled from unverified results
                           n.verifications,
                           confidence.threshold = 0, #threshold above which to verify
                           overwrite = FALSE,

                           # Args that customize the verification experience
                           play = FALSE,
                           buffer = 0, # maybe we do not want to allow a buffer?
                           # since we can't graph a box (no y coordinates)
                           f.lim = c(0, 12),
                           spec.col = monitoR::gray.3()
)
{

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
  wav.path <- paste0(audio.directory, recordingID)
  finame <- paste0(results.directory,
                   list.files(results.directory,
                              pattern = paste0('_formatted_',
                                               gsub('.wav', '', recordingID))))
  results <- data.table(read.csv(file = finame, header = TRUE))
  results[,scoreID := 1:.N] # add a scoreID locally for updating results
  all.focal <- results[, common.name] == common.name
  all.focal <- results[all.focal]

  message('Only considering results above confidence ', confidence.threshold, '...')
  all.focal <- all.focal[confidence >= confidence.threshold]

  # If not overwriting existing work, only look at unverified results
  if (overwrite == FALSE) {
    cat('You have verified',
        all.focal[!is.na(verify), .N],
        'out of', all.focal[,.N], 'results for this common.name. \nSince overwrite == FALSE, only detections from unverified results will be verified.')
    unver.focal <- all.focal[is.na(verify)]
    verify <- setkey(unver.focal[sample(x = .N, size = n.verifications, replace = FALSE)], start.s)
    if (unver.focal[,.N] == 0) return(message('\n Overwrite == FALSE and you have already verified all results for this common.name and recordingID.'))
  }

  # CHECK TO MAKE SURE THIS IS WORKING AS DESIRED??
  if (overwrite == TRUE) {
    verify <- setkey(all.focal[sample(x = .N, size = n.verifications, replace = FALSE)], start.s)
  }

  if (nrow(verify) == 0) {
    return(message("Nothing to verify for this common.name and recordingID."))
  }

  counter <- 0
  ask <- FALSE
  oldask <- par(ask = par("ask"))
  on.exit(par(oldask))
  verC <- NULL

  for (i in 1:verify[,.N]) {
    x <- "x"
    t.start <- max(verify[i,start.s] - buffer, 0)
    t.end <- verify[i,end.s] + buffer # works even if end.s exceeds rec length
    wav <- tuneR::readWave(wav.path, from = t.start, to = t.end, units = 'seconds')
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
          main = paste0("Result ",i, ' of ', verify[,.N],
                        ' in ', recordingID,' [Confidence = ',
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
      writeWave(object = wav, filename = fn)

      message('\nWriting temporary file clip to ', fn, ' so that you can manually check it. Temporary file will be deleted after you are finished with this verification and have closed the player window.')

      #tuneR::play(object = wav, ... = )
      #Sys.sleep(duration(wav))
    }

    while (length(x) == 0 || !x %in% c("y", "n", NA)) {

      cat(paste0("\n This is recording ", recordingID,
                 " This is verification ", counter + i,
                 " out of ", verify[,.N]), "\n")
      cat(paste0("\n", i, ". True detection for ",
                 common.name,
                 "?\n Enter y for yes, n for no, s to skip, r to replay, or q to exit (q will exit and save any verifications you have already completed): "))

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

      cat(switch(x,
                 n = FALSE,
                 y = TRUE,
                 s = "Skipping to next verification.",
                 r = "Replaying.",
                 q = "Exiting and saving what you have already verified.",
                 "Value not recognized. Enter y, n, s, r, or q."),
          "\n")

      # If skip, skip to next
      if (x == 's') {
        break
      }

      if (!x %in% c("y", "n", "r", "s", "q"))
        next
      if (x == "q")
        break
    } # end while x %in% y, n, NA

    # If q, break out of the peaks loop
    if (!is.na(x) & x == 'q') break

    if (is.na(x) || x != "r")
      verC[i] <- x
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

  counter <- counter + verify[,.N]

  cat("\n")

  # Update verification labels in BirdNET_formatted_ results csv:
  verC[verC == 'y'] <- 1
  verC[verC == 'n'] <- 0
  vers <- as.integer(verC)
  update.these <- verify$scoreID

  # If quit before the end of a recording, update labels for anything verified
  if (x == 'q') {
    update.these <- verify$scoreID[seq_along(vers)]
  }

  # If there is anything to update
  if (length(update.these) > 0) {

    verify[scoreID %in% update.these, verify := vers]
    new.results <- results
    new.results[scoreID %in% update.these, verify := vers]
    new.results[,scoreID := NULL]
    message('Updating ', basename(finame), ' with new verifications.')
    write.csv(x = new.results, file = finame, row.names = FALSE)
  } # end if length(update.these)

  cat("Finished verifying for this recording.\n")

  verify[,scoreID := NULL] # remove temporaryID

  # Return scores table updated with verifications
  return(verify)
}



