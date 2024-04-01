
# Generic utility functions

# add_time_cols ================================================================

# Convenience function to parse recordingIDs into useful columns to allow
# expedient summaries by date, dateTime, hour, etc.
# Returns same data.table as input, but with time columns added
# also adds locationID

#' @name add_time_cols
#' @title Add R-friendly time-based columns to bioacoustics data
#' @description Convenience function to parse recordingIDs into useful columns to allow expedient summaries by date, local time, hour, etc. Input table must have a parseable recordingID column.
#' @param dt Data.frame or data.table containing a recordingID column of format NAME_YYYYMMDD_HHMMSS.wav
#' @param recording.id.col Column name that contains recordingID with format 'NAME_YYYYMMDD_HHMMSS.wav'. Default = 'recordingID'.
#' @param tz.recorder Olsen names timezone used by the audio recorder during data collection. For example, you may have collected data using a Wildlife Acoustics SM4, and may have used UTC/GMT instead of setting a local time. Note that 'UTC' and 'GMT' are synonymous and both acceptable for this function argument. This argument accounts for the fact that recordings may have been taken in UTC. The tz.local argument then allows us to convert the times to local times that will make sense for analysis.
#' @param tz.local Olsen names timezone for local time at the monitoring location (e.g., 'America/Anchorage').
#' @param timestep If adding time columns with the intention of pairing automated detection data with a 'time' column with acoustic indices (AI) data, please add the time increment used to generate the AI data (e.g., "10" for 10 minutes).
#' @return A data.table with the same columns as the input, but now including the following additional columns:
#' \itemize{
#' \item{\strong{dateTimeRecorder}: POSIXct-formatted date-time object used by the audio recorder (typically will be the same as UTC or local time, depending on recording configuration parameters used in the field). }
#' \item{\strong{dateTimeUTC}: POSIXct-formatted date-time object in UTC time. }
#' \item{\strong{dateTimeLocal}: POSIXct-formatted date-time object in local time. }
#' }
#'
#' If the input data.frame or data.table contains a 'time' column as from an AMMonitor scores table, then the timestep argument is utilized in the function and the following two additional columns are returned:
#'
#' #' \itemize{
#' \item{\strong{detectionTime}: POSIXct-formatted date-time object giving the exact detection time, to the second, of the detected event. }
#' \item{\strong{detectionTimeMinute}: POSIXct-formatted date-time object giving the detection time rounded down to the nearest minute.}
#' }
#'
#' @details
#'
#' This function was developed by the National Park Service Natural Sounds and Night Skies Division to support bioacoustics projects such as those related to acoustic indices and automated detection of focal species.
#'
#' @import data.table
#' @importFrom lubridate floor_date with_tz
#' @export
#' @examples
#' \dontrun{
#'
#' # Create basic example data.frame
#' recording.info <- data.frame(
#' recordingID = c('GLBABART_20200528_104200.wav',
#'                 'GLBABART_20200528_114157.wav',
#'                 'GLBABART_20200528_124152.wav',
#'                 'GLBABART_20200529_104000.wav',
#'                 'GLBABART_20200529_113957.wav',
#'                 'GLBABART_20200529_123952.wav'
#'               ))
#'
#' output <- add_time_cols(dt = recording.info,
#'                         recording.id.col = 'recordingID',
#'                         tz.recorder = 'UTC',
#'                         tz.local = 'America/Anchorage')
#'
#'
#' # Read in an example scores table to demonstrate the utility of the timestep argument:
#' data(exampleScores)
#' output <- add_time_cols(dt = exampleScores,
#'                         recording.id.col = 'recordingID',
#'                         tz.recorder = 'America/Los_angeles',
#'                         tz.local = 'America/Los_angeles',
#'                         timestep = 10
#'                         )
#'
#' }

add_time_cols <- function(dt,
                          recording.id.col = 'recordingID',
                          tz.recorder,
                          tz.local,
                          timestep = 10
) {
  dt <- as.data.table(dt)
  splt <- strsplit(x = dt[,get(recording.id.col)], fixed = TRUE, split = '_')
  locIDs <- unlist(lapply(splt, '[[', 1))
  dates <- unlist(lapply(splt, '[[', 2))
  times <- gsub(pattern = '.wav|.mp3', ignore.case = TRUE, replacement = '',
                x = unlist(lapply(splt, "[[", 3)))

  dateTimes <- as.POSIXct(paste0(dates, times), tz = tz.recorder, format = '%Y%m%d%H%M%S')
  dt[,dateTimeRecorder := dateTimes]

  # Create a UTC time column for easier conversion
  dt[,dateTimeUTC := with_tz(dateTimeRecorder, tzone = 'UTC')]

  # Create a local time column for downstream interpretability
  dt[,dateTimeLocal :=  with_tz(dateTimeUTC, tzone = tz.local)]
  dt[,detectionTimeLocal := dateTimeLocal + start]
  dt[,date := as.Date(detectionTimeLocal, tz = tz.local)]
  dt[,year := year(date)]

  # Add a locaitonID
  dt[,locationID := locIDs]

  # Add additional columns for pairing with AI indices
  if ('time' %in% colnames(dt)) {
    if(missing(timestep)) {
      stop('If adding columns to pair with acoustic indices data, please add the time increment used to generate the AI data (e.g., "10" for 10 minutes')
    }

    # If the DT contains the time column in the recording (i.e., when an event
    # was detected during the recording, in seconds) then we add those additional columns
    # for playing nicely with the acoustic indices
    dt[,detectionTime := dateTimeLocal + time]
    dt[, detectionTimeMinute := floor_date(detectionTime, '1 mins')] # round down to nearest minute of detection time to make start.at.beginning joins easier
  } else {
    return(dt)
  }
}


# readable_julian_breaks =======================================================

#' @name readable_julian_breaks
#' @title Create human-readable labels for julian dates
#' @description Create human-readable labels for julian dates, to be used downstream in plots. Particularly useful for generating plots that compare multiple years of data in one graph, especially when dealing with leap years (see Details).
#' @param data A data.frame or data.table that must contain a column of class "POSIXct" "POSIXt".
#' @param posix.column Character name of the "POSIXct" "POSIXt" column in 'data'.
#' @param format Character string containing your desired date label components; any options in c('%d', '%m', '%b', '%B', '%y', '%Y'). For example, c('%Y', '%m', '%d') used with sep = '-' results in labels like: '2022-01-28'. See Details. Do not use '%y' or '%Y' options if data contains multiple years.
#' @param sep Character value used to separate options in format. Any value may be used, but common uses would be options in c(' ', '-', '/').
#' @param timestep Integer specifying the number of days that should be spaced between each label. For example, a value of 14 means labels will occur every two weeks.
#' @param juilan.breaks Optional integer vector specifying the values of julian dates to use; will override timestep argument.
#' @return A data.table with two columns: julian.date (integer) and date.lab (character) which can be used downstream in ggplot with the scale_x_continuous() element to customize human-readable dates on the x-axis.
#' @details
#'
#' Note: to accommodate multi-year datasets that contain leap years (and thus, differing julian dates for the same human-readable date label), the behavior of this function is to remove leap year labels from the output data for clean plotting.
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
#' @importFrom lubridate day month yday year
#' @importFrom stringr str_extract
#' @export
#' @examples
#' \dontrun{
#'
#' # Read in example data
#' data(exampleAI)
#'
#' exampleAI <- data.table(exampleAI)
#'
#' # Add a posix date time column
#' exampleAI[,dateTime := lubridate::ymd_hms(
#'      paste0(Date,' ', Hr, ':', Min, ':', Sec),
#'      tz = 'America/Anchorage')
#' ]
#'
#' # Add year and julian.date columns
#' exampleAI[, Year := factor(lubridate::year(dateTime))]
#' exampleAI[,julian.date := lubridate::yday(dateTime)]
#'
#' # Create human-readable julian breaks
#' brks <- readable_julian_breaks(
#'   data = exampleAI,
#'   posix.column = 'dateTime',
#'   format = c('%B', '%d'),
#'   sep = ' ',
#'   timestep = 30
#'   )
#'
#' # Example plot of using breaks with human-readable data across years
#' # WARNING: May take a moment to plot the following
#' ggplot(exampleAI, aes(julian.date, ACIoutI, col = Year, group = Year)) +
#'  geom_smooth(method = 'loess', aes(fill = Year)) +
#'  scale_x_continuous(expand = c(0, 0),
#'                     breaks = brks$julian.date,
#'                     labels = brks$date.lab) +
#'  xlab('Date') +
#'  ylab('Acoustic Complexity Index') +
#'  theme(axis.text.x = element_text(angle = 90))
#'
#' }


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
  data[,julian.date := yday(get(posix.column))]
  format.opts <- c('%d', '%m', '%b', '%B', '%y', '%Y')

  if (any(!(format %in% format.opts))) {
    stop('In format arugment, please input any combination of the following options: "c(\'%d\', \'%m\', \'%b\', \'%B\', \'%y\', \'%Y\')". See ?readable_julian_breaks for examples.' )
  }

  data[,`%d` := day(get(posix.column))][
    ,`%m` := month(get(posix.column), label = FALSE)][
      ,`%b` := month(get(posix.column), label = TRUE, abbr = TRUE)][
        ,`%B` := month(get(posix.column), label = TRUE, abbr = FALSE)][
          ,`%Y` := year(get(posix.column))][
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
    year
) {

  # Ensure forward slash at end ($) of directories
  if (grepl("\\/$", audio.directory) == FALSE) {
    audio.directory <- paste0(audio.directory, '/')
  }

  if (!('recordingID' %in% colnames(results))) {
    results[,recordingID := basename(filepath)]
  }

  if (!('locationID' %in% colnames(results))) {
    results[,locationID := unlist(lapply(strsplit(recordingID, split = '_'), '[[', 1))]
  }

  # Sample one result
  locid <- locationID
  one.res <- results[common_name == common.name
                     & confidence >= confidence.threshold
                     & locationID %in% c(locid, paste0('temp-', locid))][
                       sample(.N, size = 1, replace = FALSE)]

  if(nrow(one.res) == 0) {
    stop('No results for this input combination. Check your inputs or try a lower confidence.threshold.')
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
  # (unless you want to install the third party software for dealing with mp3s in R --> mp3splt
  # and since IT rejected my application to have overrides to install audio software,
  # I either have to deal with asking IT for help to install this, which they will
  # probably just reject, or just read in the whole mp3 file. So I am going with the latter
  # and just dealing with a slow solution.
  if (grepl(pattern = '.mp3|.MP3', x = one.res$recordingID)) {

    wav <- readMP3(filename = full.pth)
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
  viewSpec(wav, main = paste0(locationID, ' ', common.name, ' (', one.res$date, ')'))
  embed_audio(src = pth, type = 'wav')
}





# KAYLEY FUNCTIONS =============================================================

# birdnet_iNEXT ================================================================

#' @name birdnet_iNEXT
#' @title ADD TITLE
#' @description TBD
#' @param data Data.frame / data.table with....
#' @param confidence.threshold Threshold below which BirdNET results should be excluded.
#' @param n.detections Number of detections of a species within a day to conclude presence.
#' @return DESCRIBE...  additional columns:
#' \itemize{
#' \item{\strong{stuff}: tbd }
#' \item{\strong{stuff}: tbd }
#' \item{\strong{stuff}: tbd }
#' }
#'
#' @details
#'
#' This function was developed by the National Park Service Natural Sounds and Night Skies Division Scientists in Parks intern Kayley Dillon to support species biodiversity characterization in bioacoustics projects.
#'
#' @import iNEXT
#' @importFrom iNEXT iNEXT
#' @export
#' @examples
#' \dontrun{
#'
#' # Read in example BirdNET data
#' data(exampleBarchartData)
#'
#' dat <- add_time_cols(
#'   dt = exampleBarchartData,
#'   recording.id.col = 'recordingID',
#'   tz.recorder = 'America/Los_Angeles',
#'   tz.local = 'America/Los_Angeles'
#' )
#'
#' result <- birdnet_iNEXT(
#'   data = dat,
#'   confidence.threshold = 0,
#'   n.detections = 3
#' )
#'
#' # We can use this result to plot species rarefaction curves
#' # Possibly just script this instead of functionizing
#'
#' # Not sure if this should be "sized-based" vs "coverage-based"
#' # Size-based looks more right
#' size.based <- data.table(
#'   Days = result$iNextEst$size_based$t,
#'   Richness = result$iNextEst$size_based$qD,
#'   Lower_95 = result$iNextEst$size_based$qD.LCL,
#'   Upper_95 = result$iNextEst$size_based$qD.UCL,
#'   Method = result$iNextEst$size_based$Method
#' )
#'
#' cov.based <- data.table(
#'  Days = result$iNextEst$coverage_based$t,
#'  Richness = result$iNextEst$coverage_based$qD,
#'  Lower_95 = result$iNextEst$coverage_based$qD.LCL,
#'  Upper_95 = result$iNextEst$coverage_based$qD.UCL,
#'  Method = result$iNextEst$coverage_based$Method
#')
#'
#' cov.based[,locationID := 'Rivendell']
#' size.based[,locationID := 'Rivendell']
#'
#' cov.based[,Filter := 'Coverage']
#' size.based[,Filter := 'Size']
#'
#' dat <- rbind(cov.based, size.based)
#'
#' # Okay, so the richness values are essentially the same;
#' # it's the confidence values that are different
#' # size.based conf ranges will be much tigher -- why?
#'
#' # Separate out by method
#' interp <- dat[Method == 'Rarefaction']
#' extrap <- dat[Method == 'Extrapolation']
#' obs <- dat[Method == 'Observed']
#'
#' # Transform data to order plots properly
#' levs <- c('Coverage', 'Size')
#' dat$Filter <- factor(dat$Filter, levels = levs)
#' interp$Filter <- factor(interp$Filter, levels = levs)
#' extrap$Filter <- factor(extrap$Filter, levels = levs)
#' obs$Filter <- factor(obs$Filter, levels = levs)
#'
#' ## Graph ####
#' ggplot() +
#'  facet_grid(cols = vars(Filter), scales = "free_x") +
#'  geom_ribbon(dat, mapping =
#'                aes(x = Days, ymin = Lower_95,
#'                    ymax = Upper_95, fill = locationID),
#'              alpha = 0.5) +
#'  geom_point(data = obs, mapping =
#'               aes(x = Days, y = Richness,
#'                   color = locationID), size = 3) +
#'  geom_line(data = interp, mapping =
#'              aes(x = Days, y = Richness,
#'                  color = locationID,
#'                  linetype = "dashed"), size = 1) +
#'  geom_line(data = extrap, mapping =
#'              aes(x = Days, y = Richness,
#'                  color = locationID,
#'                  linetype = "solid"), size = 1) +
#'  theme_classic() +
#'  scale_fill_manual(name = "locationID",
#'                    values = c("#66c2a5", "#fc8d62")) +
#'  scale_color_manual(name = "locationID",
#'                     values = c("#66c2a5", "#fc8d62")) +
#'  labs(x = "Days Sampled", y = "Species Richness") +
#'  guides(linetype = "none")
#'
#'
#' }

birdnet_iNEXT <- function(
    data,
    confidence.threshold = 0,
    n.detections = 1
)
{

  data[ , Date := as.Date(dateTimeLocal)]
  thresh <- data[confidence >= confidence.threshold]

  # Convert to Presence/Absence by Day
  pa.day <- thresh[,.N, by = c('Date', 'common_name')][ N >= n.detections ]

  # Summarize a frequency vector for iNEXT()
  # First value is the total number of unique dates sampled
  #   (IS THIS THOUGH? OR IS IT JUST THE TOTAL NUMBER OF DATES WE DETECTED SOMETHING?
  #   WHAT IF BIRDNET DETECTED NOTHING THAT DAY? it won't account for that... very
  #   unlikely to run into this, but you could...)
  # Remaining values are the number of times each individual species met detection criteria
  samples <- length(unique(data$Date))
  freq <- c(

    # Total number of dates
    samples,

    # Number of times each detected species met the detection criteria
    pa.day[,.N, by = 'common_name'][,N]
  )

  # Observed species richness:
  # length(pa.day[,.N, by = 'common_name'][,N])

  # Shannon Index (H): sum(proportion * ln(proportion)):
  # sum((pa.day[,.N, by = 'common_name'][,N]/samples)*log(pa.day[,.N, by = 'common_name'][,N]))

  # Simpson Index (D): 1 / sum(proportion^2)
  # 1/sum((pa.day[,.N, by = 'common_name'][,N]/samples)^2)

  # These H and D are not the same as what is being reported by iNEXT,
  # which I believe is exponential Shannon entropy, and inverse Simpson index

  # Run iNEXT with incidence-based frequency data
  result <- iNEXT(
    freq,
    q = 0,

    # We use incidence frequency because this is the method that
    # allows us to use occurrence (presence) rather than abundance
    # https://esajournals.onlinelibrary.wiley.com/doi/full/10.1890/13-0133.1
    datatype = "incidence_freq",
    knots = (samples * 2)
  )
}



