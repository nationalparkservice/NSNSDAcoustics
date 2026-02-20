
# Generic utility functions

# add_time_cols ================================================================

# Convenience function to parse recordingIDs into useful columns to allow
# expedient summaries by date, dateTime, hour, etc.
# Returns same data.table as input, but with time columns added
# also adds locationID

#' @name add_time_cols
#' @title Add R-friendly time-based columns to bioacoustics data
#' @description Convenience function to parse recordingIDs into useful columns and
#' enable expedient summaries by date and local time. Input table must
#' have a parseable recordingID column of format LOCATIONID_YYYYMMDD_HHMMSS.wav
#' @param dt Data.frame or data.table containing a recordingID column of format
#' LOCATIONID_YYYYMMDD_HHMMSS.wav
#' @param recording.id.col Column name that contains recordingID with format
#' 'LOCATIONID_YYYYMMDD_HHMMSS.wav'. Default = 'recordingID'.
#' @param tz.recorder Olsen names timezone used by the audio recorder during data
#' collection. For example, you may have collected data using a Wildlife Acoustics
#'  SM4, and may have used UTC/GMT instead of setting a local time. Note
#'  that 'UTC' and 'GMT' are synonymous and both acceptable for this function
#'  argument. This argument accounts for the fact that recordings may have been
#'  taken in UTC. The tz.local argument then allows us to convert the times to
#'  local times that will make sense for analysis.
#' @param tz.local Olsen names timezone for local time at the monitoring
#' location (e.g., 'America/Anchorage').
#' @return A data.table with the same columns as the input, but now including
#' the following additional columns:
#' \itemize{
#' \item{\strong{dateTimeRecorder}: POSIXct-formatted date-time object used by
#' the audio recorder (typically will be the same as UTC or local time, depending
#' on recording configuration parameters used in the field).}
#' \item{\strong{dateTimeUTC}: POSIXct-formatted date-time object for the recordingID, in UTC time.}
#' \item{\strong{dateTimeLocal}: POSIXct-formatted date-time object for the recordingID in local time.}
#' \item{\strong{date}: POSIXct-formatted date-time object in UTC time.}
#' \item{\strong{year}: POSIXct-formatted date-time object in local time.}
#' \item{\strong{detectionTimeLocal}: POSIXct-formatted date-time object for the precise time of a detection, in local time.}
#' \item{\strong{locationID}: Character value for the locationID.}
#'
#' }
#'
#' @details
#'
#' This function was developed by the National Park Service Natural Sounds and
#' Night Skies Division to support bioacoustics projects.
#'
#' @import data.table
#' @importFrom lubridate wday second isoweek yday hour year month week minute mday quarter day round_date with_tz floor_date tz
#' @export
#' @examples
#'
#' # Ensure your data has an appropriate recordingID column and time columns
#' dat <- exampleHeatmapData
#' dat[ , recordingID := basename(filepath)]
#' dat <- add_time_cols(
#'  dt = dat,
#'  tz.recorder = 'America/Los_angeles',
#'  tz.local = 'America/Los_angeles'
#' )
#'

add_time_cols <- function(
    dt,
    recording.id.col = 'recordingID',
    tz.recorder,
    tz.local
) {

  dt <- as.data.table(dt)
  splt <- strsplit(x = dt[,get(recording.id.col)], fixed = TRUE, split = '_')
  locIDs <- sapply(splt, '[[', 1)
  dates <- sapply(splt, '[[', 2)
  times <- gsub(pattern = '.wav|.mp3', ignore.case = TRUE, replacement = '',
                x = sapply(splt, "[[", 3))

  dateTimes <- as.POSIXct(paste0(dates, times), tz = tz.recorder, format = '%Y%m%d%H%M%S')

  if (all(is.na(dateTimes))) {
    stop('Not able to parse recordingID. Your recordingIDs look like: ', dt[1, recordingID], '. Check to ensure your recordingID is in the format LOCATIONID_YYYYMMDD_HHMMSS.wav. If it contains additional elements or doesn\'t follow this format, try making an additional column to your data input (like "parseRecordingID") which follows this naming convention, and input that column to the `recordingID` argument for this function.')
  }

  dt[,dateTimeRecorder := dateTimes]

  # Create a UTC time column for easier conversion
  dt[,dateTimeUTC := with_tz(dateTimeRecorder, tzone = 'UTC')]

  # Create a local time column for downstream interpretability
  dt[,dateTimeLocal :=  with_tz(dateTimeUTC, tzone = tz.local)]

  # Add date and year
  dt[,date := as.Date(dateTimeLocal, tz = tz.local)]
  dt[,year := lubridate::year(date)]

  # Add a detectionTimelocal, date, and year if the object contains a BirdNET start column
  if ('start' %in% colnames(dt)) {
    dt[,detectionTimeLocal := dateTimeLocal + start]
  }

  # Add a locationID
  dt[,locationID := locIDs]

  return(dt)
}


# readable_julian_breaks =======================================================

#' @name readable_julian_breaks
#' @title Create human-readable labels for julian dates
#' @description Create human-readable labels for julian dates, to be used
#' downstream in plots. Particularly useful for generating plots that compare
#' multiple years of data in one graph, especially when dealing with leap years
#' (see Details).
#' @param data A data.frame or data.table that must contain a column of class
#'  "POSIXct" "POSIXt".
#' @param posix.column Character name of the "POSIXct" "POSIXt" column in 'data'.
#' @param format Character string containing your desired date label components;
#' any options in c('%d', '%m', '%b', '%B', '%y', '%Y'). For example,
#' c('%Y', '%m', '%d')
#' used with sep = '-' results in labels like: '2022-01-28'. See Details. Do not
#' use '%y' or '%Y' options if data contains multiple years.
#' @param sep Character value used to separate options in format. Any value may
#' be used, but common uses would be options in c(' ', '-', '/').
#' @param timestep Integer specifying the number of days that should be spaced
#' between each label. For example, a value of 14 means labels will occur every
#' two weeks.
#' @param julian.breaks Optional integer vector specifying the values of julian
#' dates to use; will override timestep argument.
#' @return A data.table with two columns: julian.date (integer) and date.lab
#' (character) which can be used downstream in ggplot with the scale_x_continuous()
#'  element to customize human-readable dates on the x-axis.
#' @details
#'
#' Note: to accommodate multi-year datasets that contain leap years (and thus,
#' differing julian dates for the same human-readable date label), the behavior
#' of this function is to remove leap year labels from the output data for clean
#' plotting.
#'
#' The format argument accepts any of the following options:
#'
#' \strong{Format Option Code:	Value}
#' \itemize{
#' \item{\strong{%d}: Day of month (integer).}
#' \item{\strong{%m}: Month (integer).}
#' \item{\strong{%b}: Month (abbreviated character).}
#' \item{\strong{%B}: Month (full name character).}
#' \item{\strong{%y}: Year (2 digit integer).}
#' \item{\strong{%Y}: Year (4 digit integer).}
#' }
#'
#' @import data.table
#' @importFrom lubridate wday second isoweek yday hour year month week minute mday quarter day round_date with_tz floor_date
#' @importFrom stringr str_extract
#' @export
#' @examples
#'\dontrun{
#' # Read in example data
#' data(exampleHeatmapData)
#'
#' # Add a posix.column either manually or with add_time_cols:
#' dat <- exampleHeatmapData
#' dat[ ,recordingID := basename(filepath)]
#' dat <- add_time_cols(
#'  dt = dat,
#'  tz.recorder = 'America/Los_angeles',
#'  tz.local = 'America/Los_angeles'
#' )
#'
#' # Create human-readable julian breaks
#' brks <- readable_julian_breaks(
#'   data = dat,
#'   posix.column = 'dateTimeLocal',
#'   format = c('%B', '%d'),
#'   sep = ' ',
#'   timestep = 30
#' )
#'
#' # Example plot using breaks with human-readable data across years
#' dat[,julian.date := lubridate::yday(date)]
#' plot.dat <- dat[,mean(confidence), by = c('julian.date', 'year')]
#' ggplot(plot.dat, aes(julian.date, V1, color = factor(year))) +
#'  geom_point() +
#'  scale_x_continuous(expand = c(0, 0),
#'                     breaks = brks$julian.date,
#'                     labels = brks$date.lab) +
#'  xlab('Date') +
#'  ylab('Mean BirdNET Confidence Value') +
#'  theme(axis.text.x = element_text(hjust = 1, angle = 45),
#'        legend.title = element_blank()
#'  )
#'}
#'


# see date formats:
# https://www.stat.berkeley.edu/~s133/dates.html

readable_julian_breaks <- function(
    data,
    posix.column,
    format,
    sep,
    timestep,     # number of days (integer) between each label
    julian.breaks # optional if want to customize
)
{
  data <- data.table(data)
  data[,julian.date := lubridate::yday(get(posix.column))]
  format.opts <- c('%d', '%m', '%b', '%B', '%y', '%Y')

  if (any(!(format %in% format.opts))) {
    stop('In format arugment, please input any combination of the following options: "c(\'%d\', \'%m\', \'%b\', \'%B\', \'%y\', \'%Y\')". See ?readable_julian_breaks for examples.' )
  }

  data[,`%d` := lubridate::day(get(posix.column))][
    ,`%m` := lubridate::month(get(posix.column), label = FALSE)][
      ,`%b` := lubridate::month(get(posix.column), label = TRUE, abbr = TRUE)][
        ,`%B` := lubridate::month(get(posix.column), label = TRUE, abbr = FALSE)][
          ,`%Y` := lubridate::year(get(posix.column))][
            ,`%y` := stringr::str_extract(`%Y`, '(?<=^..).*$')]
  data[, date.lab := do.call(paste, c(.SD, sep = sep)), .SDcols = format]

  if (missing(julian.breaks)) {
    julian.range <- range(data$julian.date, na.rm = TRUE)
    julian.breaks <- seq(from = floor(julian.range[1]/10)*10,
                         to = ceiling(julian.range[2]/10)*10,
                         by = timestep)
  }

  brks <- unique(data[julian.date %in% julian.breaks,
                      c('julian.date', 'date.lab')])
  setkey(brks, julian.date)

  # To accommodate leap years and avoid overlapping labels,
  # Only keep the SECOND occurrence of each julian date
  # (first occurrence would be the leap year label, which is likely less common)
  N <- brks[,.N, julian.date]
  brks <- merge(x = brks, y = N, by = 'julian.date', all.x = TRUE)
  brks[match(unique(brks$julian.date), brks$julian.date), match := TRUE]
  brks <- brks[match == TRUE]
  brks[,c('N', 'match') := NULL]

  return(brks)
}



# normdBA ======================================================================
# Used within NVSPL_To_AI -- internal pkg function
normdBA <- function (x) { (x-(-10)) / (80 - (-10)) }


#  spectrogram colors from monitoR =============================================
# # Colors are defined as:
# gray.1 <- function(n = 30) gray(seq(1, 0, length.out = n))
# gray.2 <- function(n = 30) gray(1-seq(0, 1, length.out = n)^2)
# gray.3 <- function(n = 30) gray(1-seq(0, 1, length.out = n)^3)
# rainbow.1 <- function(n = 15) rev(rainbow(n))
# topo.1 <- function(n = 12) rev(topo.colors(n))


# birdnet_audio_embed ==========================================================
# currently undocumented fxn that serves as a nice helper for
# automatically plotting spectrogram of an example birdnet detection according
# to params specified by user, and also embedding a wav file of in an HTML

birdnet_audio_embed <- function(
    results,
    audio.directory,
    locationID,
    recording.id.col,
    common.name,
    confidence.threshold,
    year,
    frq.lim = c(0, 12)
) {

  # Ensure forward slash at end ($) of directories
  if (grepl("\\/$", audio.directory) == FALSE) {
    audio.directory <- paste0(audio.directory, '/')
  }

  if (!('recordingID' %in% colnames(results))) {
    results[,recordingID := basename(filepath)]
  }

  if (!('locationID' %in% colnames(results))) {
    results[,locationID := sapply(strsplit(recordingID, split = '_', '[[', 1))]
  }

  # Sample one result
  locid <- locationID
  one.res <- results[common_name == common.name
                     & confidence >= confidence.threshold
                     & locationID %in% c(locid, paste0('temp-', locid))][
                       sample(.N, size = 1, replace = FALSE)]

  if(nrow(one.res) == 0) {
    stop('No results for this input combination. Check your inputs or try a lower `confidence.threshold`.')
  }

  # Get the date of the detection (fine to leave in UTC since these are just examples
  # and we only need the date)
  one.res <- add_time_cols(one.res,
                           recording.id.col = recording.id.col,
                           tz.recorder = 'UTC',
                           tz.local = 'UTC')
  one.res[,date := as.Date(dateTimeLocal)]


  # Figure out if we are dealing with wave vs. mp3 vs. a temporary wave

  # If this was processed as a temporary wave, that temp- wave no longer exists
  # and we need to find the underlying mp3 file name:
  if (grepl(pattern = 'temp-', x = one.res$filepath, ignore.case = FALSE)) {
    og.res <- one.res
    one.res$filepath <- gsub(pattern = '.wav|.WAV',
                             replacement = '.mp3',
                             x = one.res$filepath)
    one.res$filepath <- gsub(pattern = 'temp-',
                             replacement = '',
                             x = one.res$filepath)
    one.res$recordingID <- gsub(pattern = '.wav|.WAV',
                                replacement = '.mp3',
                                x = one.res$recordingID)
    one.res$recordingID <- gsub(pattern = 'temp-',
                                replacement = '',
                                x = one.res$recordingID)
  }

  full.pth <- paste0(audio.directory, one.res$recordingID)

  # If original format is wave, we can read in based on the seconds
  if (grepl(pattern = '.wav|.WAV', x = one.res$recordingID)) {
    wav <- readWave(filename = full.pth,
                    from = one.res$start,
                    to = one.res$end,
                    units = 'seconds')
  }

  # If original mp3, we unfortunately have to read in the entire mp3 file and convert it to wave
  # which takes a while if dealing with hour long files, and may crash R
  # if you are dealing with longer files than that.
  # (unless users can easily install the third party software for dealing with mp3s in R --> mp3splt
  if (grepl(pattern = '.mp3|.MP3', x = one.res$recordingID)) {

    wav <- monitoR::readMP3(filename = full.pth)
    wav <- monitoR::cutWave(wav, from = one.res$start, to = one.res$end)
  }

  # Create a directory for clips if needed
  if (!dir.exists('embed-audio')) {
    dir.create('embed-audio')
  }

  message('Saving this clip in "embed-audio" folder for future HTML audio embedding...')

  # Create a wav file for this clip
  pth <- paste0('./embed-audio/',
                paste(gsub(pattern = "'",
                           replacement = '',
                           x = common.name),
                      one.res$start,
                      one.res$end,
                      one.res$recordingID,
                      sep = '-'))

  # Make sure the wave we create has a lowercase extension
  # (so that embed_audio recognizes it)
  pth <- gsub(pattern = '.WAV', replacement = '.wav',
              ignore.case = FALSE, x = pth)
  writeWave(wav, filename = pth)

  # Generate spectrogram
  det.spec <- monitoR:::spectro(wave = wav)
  image(
    x = 1:length(det.spec$time),
    y = 1:length(det.spec$freq),
    t(det.spec$amp),
    yaxt = 'n', xaxt = 'n', xlab = '', ylab = '', col = gray.3(),
    main = paste0(locationID, ' ', common.name, ' (', one.res$date, ')')
  )
  box()

  # Set up time axis (x)
  t.bins <- det.spec$time
  t.bin.ticks <- pretty(t.bins, n = 3)
  t.step <- t.bins[2]-t.bins[1]
  axis(1, at = t.bin.ticks/t.step,
       labels = format(as.POSIXct(t.bin.ticks, origin = "1960-01-01", tz = "GMT"),
                       format='%H:%M:%S'))

  # Set up frequency axis (y)
  if (missing(frq.lim)) {
    frq.lim <- c(0, max(det.spec$freq))
  }
  which.frq.bins <- which(det.spec$freq >= frq.lim[1] & det.spec$freq <= frq.lim[2])
  frq.bins <- det.spec$freq[which.frq.bins]
  frq.bin.ticks <- pretty(det.spec$freq, n = 5)
  frq.step <- frq.bins[2]-frq.bins[1]
  axis(2, at = frq.bin.ticks/frq.step, labels = frq.bin.ticks, las = 1)

  # Add embedded audio
  embed_audio(src = pth, type = 'wav')
}
