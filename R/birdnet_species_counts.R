# birdnet_species_counts =======================================================

#' @name birdnet_species_counts
#' @title Provide count data of detection species over a selected time unit.
#' @description tbd
#' @param dt Path to directory where raw BirdNET result CSVs have been stored
#' @param unit See lubridate::round_date for options. E.g. second, 2 hours, etc.
#' @return Saves a formatted CSV of results with the following columns:
#'
#' \itemize{
#' \item{\strong{stuff}: tbd}
#' }
#'
#' @details
#'
#' This function was developed by the National Park Service Natural Sounds and Night Skies Division to summarize audio data using BirdNET.

#'
#' @seealso  \code{\link{birdnet_run}}, \code{\link{birdnet_verify}}, \code{\link{birdnet_format_csv}}
#' @import data.table
#' @export
#' @examples
#' \dontrun{
#'
#' # tbd
#' }
#'

birdnet_species_counts <- function(dt, unit) {

  # Not sure this function is that useful.

  # Add in ifs to add in dateTimeLocal & locationID if add_time_cols has not been applied yet
  # Need to test out whether by minute, hour, day all work

  dt[,detectionTime := dateTimeLocal + start.s]
  dt[,detectionTimeUnit := round_date(detectionTime, unit = unit)] # lubridate::round_date


  # Summarize species detection count by minute to join to acoustic inds
  n.by.timestep <- dt[,.N, by = c('locationID', 'common.name', 'detectionTimeUnit')]


}
