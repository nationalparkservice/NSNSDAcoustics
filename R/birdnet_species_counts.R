# birdnet_species_counts =======================================================

#' @name birdnet_species_counts
#' @title Provide count data of detected species over a selected time unit.
#' @description Summarizes count data of detected species across locations over a selected time unit. Takes formatted results only (see: \code{\link{birdnet_format_csv}}; must contain columns named recordingID, start.s, end.s, scientific.name, common.name, confidence, verify, and timezone. Data may come from different locations, but all locations must come from within the same timezone. Function should not be used if inputting datasets across locations with varying local timezones; if this functionality is desired, use the function in a loop.
#' @param data Data.table or data.frame of BirdNET results to summarize over a selected time unit.
#' @param unit Time unit over which to summarize. See \link[lubridate]{round_date} "Examples" section for options. E.g. 'second', '10 minutes', '2 hours', 'week', 'month', 'quarter', etc.
#' @param tz.local \href{https://en.wikipedia.org/wiki/List_of_tz_database_time_zones#List}{Olson-names-formatted character timezone} indicating the local timezone at the recording site. (Note: this may differ from the "timezone" column in a formatted BirdNET CSV dataset, which denotes the timezone setting used on the audio recorder itself and may be GMT/UTC). Only input datasets with one local timezone.
#' @return
#'
#' Returns a data.table / data.frame summarizing the number of species detections by locationID, common name, and the time detection unit specified by the user.
#'
#' \itemize{
#' \item{\strong{locationID}: Start time of detection in seconds.}
#' \item{\strong{common.name}: Species common name.}
#' \item{\strong{detectionTimeUnit}: Unit of time over which species count data have been summarized (based on input argument "unit").}
#' \item{\strong{N}: Number of species detections for this time unit.}
#' }
#'
#' @details
#'
#' This function was developed by the National Park Service Natural Sounds and Night Skies Division to summarize data outputs produced by BirdNET.

#'
#' @seealso  \code{\link{birdnet_run}}, \code{\link{birdnet_verify}}, \code{\link{birdnet_format_csv}}
#' @import data.table lubridate
#' @export
#' @examples
#' \dontrun{
#'
#' # Create a BirdNET results directory for this example
#' dir.create('example-results-directory')
#'
#' # Write examples of formatted BirdNET CSV outputs to example results directory
#' data(exampleFormatted1)
#' write.csv(x = exampleFormatted1,
#'           file = 'example-results-directory/BirdNET_formatted_Rivendell_20210623_113602.csv',
#'           row.names = FALSE, )
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
#' # Summarize detection species counts by hour
#' out <- birdnet_species_counts(data = dat,
#'                               unit = 'hour',
#'                               tz.local = 'America/Los_Angeles')
#'
#' # Summarize detection species counts by 5 minute intervals
#' out <- birdnet_species_counts(data = dat,
#'                               unit = '5 minutes',
#'                               tz.local = 'America/Los_Angeles')
#'
#'unlink(x = 'example-results-directory', recursive = TRUE)
#'
#' }
#'

birdnet_species_counts <- function(data, unit, tz.local) {

  if(!is.data.table(data) == TRUE) data <- as.data.table(data) # ensure class DT

  if (!('dateTimeLocal' %in% colnames(data))) {
    # Apply add_time_cols
    data <- add_time_cols(dt = data,
                          tz.recorder = unique(data$timezone),
                          tz.local = tz.local)
  }

  data[,detectionTime := dateTimeLocal + start.s]
  data[,detectionTimeUnit := round_date(detectionTime, unit = unit)] # lubridate::round_date

  # Summarize species detection count by minute to join to acoustic inds
  n.by.timestep <- data[,.N, by = c('locationID', 'common.name', 'detectionTimeUnit')]

}
