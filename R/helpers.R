
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
#' @param tz.recorder Olsen names timezone used by the audio recorder during data collection. For example, you may have collected data using a Wildlife Acoustics SM4, and instead of setting a local time, UTC/GMT likely may have been used. Note that 'UTC' and 'GMT' are synonymous and both acceptable for this function argument. This argument accounts for the fact that recordings may have been taken in UTC. The tz.local argument then allows us to convert the times to local times that will make sense for analysis.
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


# normdBA ======================================================================
# Used within NVSPL_To_AI -- internal pkg function
normdBA <- function (x) { (x-(-10)) / (80 - (-10)) }


# # spectrogram colors from monitoR ==============================================
# # Colors are defined as:
# gray.1 <- function(n = 30) gray(seq(1, 0, length.out = n))
# gray.2 <- function(n = 30) gray(1-seq(0, 1, length.out = n)^2)
# gray.3 <- function(n = 30) gray(1-seq(0, 1, length.out = n)^3)
# rainbow.1 <- function(n = 15) rev(rainbow(n))
# topo.1 <- function(n = 12) rev(topo.colors(n))
