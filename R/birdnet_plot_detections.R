# birdnet_plot_detections ======================================================

#' @name birdnet_plot_detections
#' @title Plot BirdNET detections
#' @description tbd
#' @param data Data.table or data.frame of subsetted detections that a user would like to plot \strong{for a single species}. This allows the user precise control over which detections to plot.
#' @param audio.directory tbd
#' @param title Optional title describing which detections are being plotted (e.g., "Confidence > 0.5", "True Positives", "Alarm Calls").
#' @param frq.lim Default = c(0, 12). Optional two-element numeric vector specifying frequency limits to the plotted spectrograms, in kHz.
#' @param new.window Default = TRUE. Logical value for whether to use \code{dev.new} to produce new plot windows.
#' @param spec.col Default = gray.3(). The colors used to plot verification spectrograms. Spectrogram colors are adjustable, and users may create their own gradients for display. A few spectrogram color options are provided via the R package monitoR, including gray.1(), gray.2(()), gray.3(), rainbow.1(), and topo.1(), all of which are based on existing R colors.
#' @param box Default = TRUE. Logical for whether to draw a box around each detection.
#' @param box.lwd Default = 1. Integer value for box line thickness.
#' @param box.col Default = 'black'. Box color.

#' @return At least one plot, and up to N plots based on the number of "verify" categories contained in input data; one of all verified target signals for this template, and the other of all verified false alarms for this template. \code{birdnet_plot_detections} also returns a composite key of each plotted detection, which could be used to backtrack into the CSVs and check or change labels that appear to be incorrect... (not sure if I want to bother including this though).
#' @details
#' This function was developed by the National Park Service Natural Sounds and Night Skies Division to process audio data produced by BirdNET.
#'
#' @seealso  \code{\link{birdnet_run}}, \code{\link{birdnet_format_csv}}
#' @import monitoR tuneR
#' @export
#' @examples
#' \dontrun{
#'
#' # tbd
#' }
#'


birdnet_plot_detections <- function(data,
                                    audio.directory,
                                    title,
                                    frq.lim = c(0, 12),
                                    new.window = TRUE,
                                    spec.col = monitoR::gray.3(),
                                    box = TRUE,
                                    box.lwd = 1,
                                    box.col = 'black'
)
{

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

  data <- as.data.table(data)
  setkey(data, recordingID)

  # Initialize necessary wave paths
  all.wav <- list.files(audio.directory, full.names = TRUE, recursive = TRUE)
  rec.ids <- unique(data$recordingID)
  wav.paths <- unique(grep(paste(rec.ids,collapse="|"), all.wav, value = TRUE))

  # Read in 1 test wave to check and get number of time and frequency bins
  checker <- readWave(filename = wav.paths[1], from = 0, to = 3, units = 'seconds')
  check.sp <- monitoR:::spectro(wave = checker)
  which.frq.bins <- which(check.sp$freq >= frq.lim[1] &
                            check.sp$freq <= frq.lim[2])
  nrows <- length(which.frq.bins)  # n freq bins
  ncols <-  length(check.sp$time) # n time bins

  # Save spectrogram amplitude data for each detection
  mats <- array(data = 0, dim = c(nrows, ncols, nrow(data)))
  cat('Gathering plot data...\n')
  for (n in 1:nrow(data)) {
    cat(n, ' ')
    dat <- data[n]
    det <- readWave(filename = wav.paths[grepl(pattern = dat$recordingID,
                                               x = wav.paths)],
                    from = dat$start.s, to = dat$end.s,
                    units = 'seconds')
    det.spec <- monitoR:::spectro(wave = det)
    mats[,,n] <- det.spec$amp[which.frq.bins, ]
  } # end for n

  if (new.window) dev.new()

  # Arrange plot dimensions
  sqrdim <- ceiling(sqrt(dim(mats)[3]))
  if (sqrdim^2 %% dim(mats)[3] > sqrdim) {
    dim1 <- sqrdim - 1; dim2 <- sqrdim
  } else {
    dim1 <- dim2 <- sqrdim
  }

  # set pars
 # par(mar = c(0,0,0.5,0), mfrow = c(dim1, dim2)) # with a margin where title could go... not working
  par(mar = rep(0, 4), mfrow = c(dim1, dim2)) # no margin

  # Plot detections
  cat('\nPlotting...\n')
  for (pl in 1:dim(mats)[3]) {
    image(x = 1:ncols, y = 1:nrows, t(mats[,,pl]),
          yaxt = 'n', xaxt = 'n', xlab = '', ylab = '', col = spec.col)
    if (box == TRUE) {
      box(col = box.col, lwd = box.lwd)
    }
  }
  if(!(missing(title))) mtext(title, side = 3, line = - 2,
                              outer = TRUE, cex = 1.5)

  cat('\nDone plotting detections.')
}
