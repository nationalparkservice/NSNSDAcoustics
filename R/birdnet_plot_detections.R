# birdnet_plot_detections ======================================================

#' @name birdnet_plot_detections
#' @title Plot BirdNET detections
#' @description Plot spectrograms of user-selected verified or unverified data
#' @param data Data.table or data.frame of subsetted detections that a user would like to plot \strong{for a single species}. This allows the user precise control over which detections to plot.
#' @param audio.directory Top-level input directory path to audio files to be processed. Files are expected to have the naming convention SITEID_YYYYMMDD_HHMMSS.wav.
#' @param title Optional title describing which detections are being plotted (e.g., "Confidence > 0.5", "True Positives", "Alarm Calls").
#' @param frq.lim Optional two-element numeric vector specifying frequency limits to the plotted spectrograms, in kHz. Default = c(0, 12).
#' @param new.window Logical value for whether to use \code{dev.new} to produce new plot windows. Default = TRUE.
#' @param spec.col The colors used to plot verification spectrograms. Default = gray.3(). Spectrogram colors are adjustable, and users may create their own gradients for display. A few spectrogram color options are provided via the R package monitoR, including gray.1(), gray.2(), gray.3(), rainbow.1(), and topo.1(), all of which are based on existing R colors.
#' @param box Logical for whether to draw a box around each detection. Default = TRUE.
#' @param box.lwd Integer value for box line thickness. Default = 1.
#' @param box.col  Box color. Default = 'black'.
#' @param title.size Size of title. Default = 1.
#' @return Plot of verified detections
#' @details
#' This function was developed by the National Park Service Natural Sounds and Night Skies Division to process audio data produced by BirdNET.
#'
#' @seealso  \code{\link{birdnet_run}}, \code{\link{birdnet_format_csv}}, \code{\link{birdnet_verify}}
#' @import monitoR tuneR
#' @export
#' @examples
#' \dontrun{
#'
#' # Create an audio directory for this example
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
#' # Read in example data.table/data.frame for plotting
#' data(examplePlotData)
#'
#' # Plot only detections of Swainson's Thrush verified as "song",
#' # with frequency limits ranging from 0.5 to 12 kHz, gray spectrogram colors,
#' # a custom title, and a gray box around each detection
#' plot.songs <- examplePlotData[common.name == "Swainson's Thrush" & verify == "song"]
#' birdnet_plot_detections(data = plot.songs,
#'                         audio.directory = 'example-audio-directory',
#'                         title = "Swainson's Thrush Songs",
#'                         frq.lim = c(0.5, 12),
#'                         new.window = TRUE,
#'                         spec.col = gray.3(),
#'                         box = TRUE,
#'                         box.lwd = 1,
#'                         box.col = 'gray')
#'
#' # Plot only detections of Swainson's Thrush verified as "call"
#' # with frequency limits ranging from 0.5 to 6 kHz,a custom title, no boxes,
#' # and colors sampled from the viridis color package
#' plot.calls <- examplePlotData[common.name == "Swainson's Thrush" & verify == "call"]
#' birdnet_plot_detections(data = plot.calls,
#'                         audio.directory = 'example-audio-directory',
#'                         title = "Swainson's Thrush Calls",
#'                         frq.lim = c(0.5, 6),
#'                         new.window = TRUE,
#'                         spec.col = viridis::viridis(30),
#'                         box = FALSE)
#'
#' # Loop through to plot detections for selected unverified species
#' # where confidence of detection >= 0.25
#' # with frequency limits ranging from 0.5 to 12 kHz, custom titles, gray boxes,
#' # and gray spectrogram colors
#' sp <- c('Brown-crested Flycatcher', 'Pacific-slope Flycatcher')
#' for (i in 1:length(sp)) {
#'  plot.sp <- examplePlotData[confidence >= 0.25 & common.name == sp[i]]
#'  birdnet_plot_detections(data = plot.sp,
#'                          audio.directory = 'example-audio-directory',
#'                          title = paste0(sp[i], ' Detections >= 0.25'),
#'                          frq.lim = c(0.5, 12),
#'                          new.window = TRUE,
#'                          spec.col = gray.3(),
#'                          box = TRUE,
#'                          box.lwd = 0.5,
#'                          box.col = 'gray',
#'                          title.size = 1.5)
#' }
#'
#' # Delete all temporary example files when finished
#' unlink(x = 'example-audio-directory', recursive = TRUE)
#'
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
                                    box.col = 'black',
                                    title.size = 1
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
  check.sp <- monitoR:::spectro(wave = checker) # monitor:::
  which.frq.bins <- which(check.sp$freq >= frq.lim[1] &
                            check.sp$freq <= frq.lim[2])
  nrows <- length(which.frq.bins)  # n freq bins
  ncols <-  length(check.sp$time) # n time bins

  # Save spectrogram amplitude data for each detection
  mats <- array(data = 0, dim = c(nrows, ncols, nrow(data)))
  cat('\nGathering plot data...\n')
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
  if(!(missing(title))) mtext(title, side = 3, line = - 2, outer = TRUE, cex = title.size)

  cat('\nDone plotting detections.')
}
