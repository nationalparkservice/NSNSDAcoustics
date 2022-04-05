# Functions for Bayesian Changepoint Analysis processing AI data
# 10/27/2021



# bcp_phenology_ai =============================================================

#' @name bcp_phenology_ai
#' @title Run Bayesian Changepoint Analysis using a single site and year using acoustic indices
#' @description This function uses the bcp package to perform a Bayesian Changepoint Analysis for a desired site and year, based on acoustic index data.
#' @param ai.data File path to acoustic index csv
#' @param eval.data File path to eval days csv that includes dates to exclude from analysis
#' @param locationID Site name of monitoring location for which to perform analysis
#' @param year YYYY year for which to perform analysis
#' @param ai.variable Name of acoustic index variable for which to perform analysis. Options c('ACIoutI', 'AA', 'AR', etc.) (see NVSPL_To_AI for column name metadata)
#' @param keep.all.transition.days Logical flag for whether to keep ALL transition days (TRUE) or only keep the range (starting and ending) of transition days (FALSE)
#' @param lat Latitude in decimal degrees, necessary to compute sunrise times
#' @param long Latitude in decimal degrees, necessary to computer sunrise times
#' @param start.rise Character object indicating how long, in hours, before sunrise to begin data subset (necessary if processing multiple years of data with varied audio recording schedules). See Details.
#' @param end.rise Character object indicating how long, in hours, after sunrise to end data subset (necessary if processing multiple years of data with varied audio recording schedules). See Details
#' @param tz.recorder Olsen names timezone used by the audio recorder during data collection. For example, you may have collected data using a Wildlife Acoustics SM4, and instead of setting a local time, UTC/GMT likely may have been used. Note that 'UTC' and 'GMT' are synonymous and both acceptable for this function argument. This argument accounts for the fact that recordings may have been taken in UTC. The tz.local argument then allows us to convert the times to local times that will make sense for analysis.
#' @param tz.local Olsen names timezone for local time at the monitoring location (e.g., 'America/Anchorage').
#' @param n.reps Number of times to repeat the BCP analysis to account for stochasticity
#' @return Returns a list object with full results, peak days, and transition days. Give a detailed explanation of each item.
#' @details
#'
#' This function was developed by the National Park Service Natural Sounds and Night Skies Division to perform a Bayesian Changepoint Analysis (BCP) in bioacoustics phenology projects for a selected year, acoustic monitoring site, and acoustic index metric.
#'
#' To account for stochasticity, the BCP is performed n.reps times (default n.reps = 100).
#' Transition days are defined as the earliest and latest season date for which the average posterior probability falls above 0.5
#' Peak days are defined as the most frequently occurring(??) days with the highest posterior mean over n.reps (IS THIS HOW I SHOULD DEFINE IT?)
#'
#' A note on timezones. SM4s do not account for daylight saving changes. The tz.local (local timezone) allows us to account for changes between Standard and Daylight time.
#'
#' Because most bcp_phenology projects are going to be looking at sunrise-based audio sampling, the start.rise and end.rise arguments account for the possibility of varied sampling audio recording schemes through the years. E.g. for GLBA, start.rise = 1.5 and end.rise = 0.5 indicates that data subsetted beginning 1.5 hours before sunrise and ending 0.5 hours after sunrise.
#'
#' Audio sampling that did not use a sunrise-based sampling scheme errs on the side of adding an additional ai timestep chunk on each end of the subset.
#'
#' \itemize{
#' \item{Cool list of stuff}
#' \item{More stuff}
#' \item{and more!}
#' }
#'
#' @seealso  \code{\link{NVSPL_To_AI}}, \code{\link{SongMeter_To_NVSPL}}
#' @import maptools svDialogs tuneR
#' @export
#' @examples
#' \dontrun{
#'
#' # examples here
#'
#' }
#'


bcp_phenology_ai <- function(ai.data,
                             eval.data,
                             locationID,
                             year,
                             start.date,
                             end.date,
                             ai.variable,
                             keep.all.transition.days,
                             lat,
                             long,
                             start.rise,
                             end.rise,
                             tz.recorder,
                             tz.local,
                             w0 = 0.2,
                             p0 = 0.2,
                             burnin = 500,
                             mcmc = 1000,
                             n.reps = 100) {

  if(missing(eval.data))
    message('\nSpectrogram evaluation data was not provided in the "eval.data" argument, so no dates will be excluded from analysis.')

  # Read in acoustic indices data
  ai.data <- data.table(read.csv(file = ai.data))

  # Create a time column that reflects the time used for the audio recorder (often, but not always, UTC)
  ai.data[,date := as.Date(gsub('_', '-', Date))]
  ai.data[,dateTimeRecorder := ymd_hms(paste0(date,' ', Hr, ':', Min, ':', Sec), tz = tz.recorder)]

  # Create a UTC time column for computing sunrise-based sampling times that avoid daylight savings issues
  ai.data[,dateTimeUTC := with_tz(dateTimeRecorder, tzone = 'UTC')]

  # Create a local time column for downstream interpretability
  ai.data[,dateTimeLocal :=  with_tz(dateTimeUTC, tzone = tz.local)]

  # Ensure date and year reflect local time
  ai.data[,date := as.Date(dateTimeLocal)]
  ai.data[,yr := year(dateTimeLocal)]


  # Keep only the year & locationID input by argument
  ai.data <- ai.data[yr == year & Site == locationID]

  # Compute sunrise time for this location (using dateTimeUTC to avoid daylight savings issues)
  ai.data[,sunrise := sunriset(crds = matrix(c(long, lat), nrow = 1),
                               dateTime = dateTimeUTC,
                               direction = 'sunrise',
                               POSIXct = TRUE)$time]
  s.start <- start.rise * 3600
  s.end <- end.rise * 3600 # convert to hours
  ai.data[,sample.start := sunrise - s.start]
  ai.data[,sample.end := sunrise + s.end]

  # Subset all data and keep only that which falls within the designated sampling period
  # Below, we are rounding down to the nearest minute for the start, and up to the nearest
  # minute for the end, to avoid accidentally throwing away data
  ai.data <- ai.data[dateTimeLocal >= floor_date(sample.start, unit = '1 minutes') &
                       dateTimeLocal <= ceiling_date(sample.end, unit = '1 minutes')]

  # If start and end dates are specified, subset further
  if (!missing(start.date) & !missing(end.date)) {
    ai.data <- ai.data[date >= as.Date(start.date) & date <= as.Date(end.date)]
  }

  # Recast to long form for ggplot
  ai.long <- data.table(melt(ai.data, measure.vars = ai.variable,
                             variable.name = 'metric', value.name = 'value'))

  # Only keep desired rows & useful columns for analysis
  ai.long <- ai.long[metric == ai.variable,
                     c('Site', 'yr', 'date', 'dateTimeUTC', 'dateTimeLocal', 'metric', 'value')]
  # Add locationID based on site name
  ai.long[,locationID := locationID]

  # If spectrogram evaluation & exclusion data are provided, incorporate it into the bcp
  if(!missing(eval.data)) {

    # Bring in the eval data:
    eval <- data.table(read.csv(eval.data))
    eval[,realDate := as.Date(date, '%m/%d/%Y')] # add R-friendly date

    # Figure out which dates to exclude
    exclude.dates <- eval[exclude == 1, realDate]

    # Exclude dates based on labeled spectrograms that have been analyzed
    ai.long[!(date %in% exclude.dates), exclude := FALSE]
    ai.long[(date %in% exclude.dates), exclude := TRUE]

  } else {
    # if no eval data provided, no dates are excluded
    ai.long[ , exclude := FALSE]
  }

  # Summarize means of each plotting metric by date
  # only perform on non-excluded data
  dat <- ai.long[exclude == FALSE, mean(value), by = c('locationID', 'yr', 'date', 'metric')]

  # Capture rep results in lists
  results <- peaks <- tr.days <- list()

  # Run bcp function using function argument inputs from MFM GLBA phenology code
  message('Running BCP ', n.reps, ' times for ', locationID, ' ', year, ': ')
  for (j in 1:n.reps){

    message(j)

    # Conduct n.reps of each grid combo
    b <- bcp(y = dat$V1, w0 = w0, p0 = p0, burnin = burnin, mcmc = mcmc)

    # Capture full results in a user-friendly way for plotting
    bcp.results <- data.table(locationID = locationID,
                              date = dat$date,
                              metric = ai.variable,
                              value = dat$V1,
                              posterior.prob = b$posterior.prob,
                              posterior.mean = b$posterior.mean[,1],
                              rep = j)
    results[[j]] <- bcp.results

    # Calculate peak day based on posterior mean
    peaks[[j]] <- bcp.results[which.max(bcp.results$posterior.mean)]

  } # end n.rep loop

  all.results <- rbindlist(results)
  all.peaks <- rbindlist(peaks)

  # Take the MEAN posterior prob of all n.reps and only store the ones that exceed 0.5 over n.reps
  # That way we don't get a fake transition day in there that randomly went above 0.5 once
  all.tr.days <- all.results[,list(mean.posterior.prob = mean(posterior.prob)),
                             by = c('locationID', 'date')][mean.posterior.prob >= 0.5]

  # All results
  all.peaks[,peak := 1]
  res <- merge(all.results, all.peaks, by = intersect(names(all.results), names(all.peaks)),
               all.x = TRUE)
  res[is.na(peak), peak := 0]
  setkey(res,locationID, date, metric, rep)

  # Transition days over n.reps
  tr <- unique(all.tr.days[,c('locationID', 'date')])

  # Identify transition days based on user input
  ifelse(test = keep.all.transition.days == TRUE,
         yes = final.tr.days <- tr[,date, by = 'locationID'][, transition.day := 1], # keep all
         no = final.tr.days <- tr[,.(date = range(date)), by = 'locationID'][, transition.day := 1] # only keep range
  )

  # Peak.days over n.reps
  final.peak.days <- unique(res[peak == 1][,c('locationID', 'date')])[, peak.day := 1]

  # Merge info about peak and transition days identified after n.reps
  ai.long <- merge(ai.long, final.peak.days, by = c('locationID', 'date'), all.x = TRUE)
  ai.long <- merge(ai.long, final.tr.days, by = c('locationID', 'date'), all.x = TRUE)

  # Add if eval data are included, add exclusion.category and characterize
  if(!missing(eval.data)) {

    # Add in a column to show whether data were excluded or not
    # Clean up the eval data for easier plotting downstream
    # NOTE: in evaldays, some earlier labels say 0 but give an exclusion category anyway.
    # I am not sure why.
    eval[grep(pattern = 'wind', x = reason, ignore.case = TRUE), exclusion.category := 'Geophony']
    eval[grep(pattern = 'rain', x = reason, ignore.case = TRUE), exclusion.category := 'Geophony']
    eval[grep(pattern = 'anthr', x = reason, ignore.case = TRUE), exclusion.category := 'Anthrophony']
    eval[grep(pattern = 'no data', x = reason, ignore.case = TRUE), exclusion.category := 'No Data']
    eval[,exclusion.subcategory := reason]
    excl.why <- eval[,c('realDate', 'exclusion.category', 'exclusion.subcategory')]
    ai.long <- merge(ai.long, excl.why, by.x = 'date', by.y = 'realDate', all.x = TRUE)
  } else {
    ai.long[,exclusion.category := as.character(NA)]
  }

  ai.long[is.na(exclusion.category), exclusion.category := 'Clean']

  # This line is a failsafe for the fact that some of the early eval data will
  # say "rain" etc as a reason but then label it as 0 to keep it. In these cases I am keeping the data.
  ai.long[exclude == FALSE, exclusion.category := 'Clean']

  # Eliminate columns that become obsolete after merging
  final.tr.days[, transition.day := NULL]
  final.peak.days[, peak.day := NULL]
  ai.long[, Site := NULL]

  # Return excluded dates for this year
  if(!missing(eval.data)) {
    exclude.dates.yr <- exclude.dates[year(exclude.dates) == year ]
  } else {
    exclude.dates.yr <- 'Spectrogram evaluation data was not provided in the "eval.data" argument, so no dates have been excluded from analysis.'
  }

  # Return results
  results <- list(full.bcp.results = res,
                  peak.days = final.peak.days,
                  transition.days = final.tr.days,
                  ai.data = ai.long,
                  excluded.dates = exclude.dates.yr)
}


# bcp_phenology_species ========================================================

#' @name bcp_phenology_species
#' @title Run Bayesian Changepoint Analysis using a single site and year based on species detection counts
#' @description This function uses the bcp package to perform a Bayesian Changepoint Analysis for a desired site and year, based on speciesdetection count data
#' @param db.path tbd
#' @param speciesID tbd
#' @param locationID tbd
#' @param templateID tbd
#' @param amml tbd
#' @param ai.data File path to acoustic index csv
#' @param eval.data File path to eval days csv that includes dates to exclude from analysis
#' @param locationID Site name of monitoring location for which to perform analysis
#' @param year YYYY year for which to perform analysis
#' @param keep.all.transition.days Logical flag for whether to keep ALL transition days (TRUE) or only keep the range (starting and ending) of transition days (FALSE)
#' @param lat Latitude in decimal degrees, necessary to compute sunrise times
#' @param long Latitude in decimal degrees, necessary to computer sunrise times
#' @param start.rise Character object indicating how long, in hours, before sunrise to begin data subset (necessary if processing multiple years of data with varied audio recording schedules). See Details.
#' @param end.rise Character object indicating how long, in hours, after sunrise to end data subset (necessary if processing multiple years of data with varied audio recording schedules). See Details
#' @param tz.recorder Olsen names timezone used by the audio recorder during data collection. For example, you may have collected data using a Wildlife Acoustics SM4, and instead of setting a local time, UTC/GMT likely may have been used. Note that 'UTC' and 'GMT' are synonymous and both acceptable for this function argument. This argument accounts for the fact that recordings may have been taken in UTC. The tz.local argument then allows us to convert the times to local times that will make sense for analysis.
#' @param tz.local Olsen names timezone for local time at the monitoring location (e.g., 'America/Anchorage').
#' @param n.reps Number of times to repeat the BCP analysis to account for stochasticity
#' @return Returns a list object with full results, peak days, and transition days. Give a detailed explanation of each item.
#' @details
#'
#' This function was developed by the National Park Service Natural Sounds and Night Skies Division to perform a Bayesian Changepoint Analysis (BCP) in bioacoustics phenology projects for a selected year, acoustic monitoring site, and acoustic index metric.
#'
#' To account for stochasticity, the BCP is performed n.reps times (default n.reps = 100).
#' Transition days are defined as the earliest and latest season date for which the average posterior probability falls above 0.5
#' Peak days are defined as the most frequently occurring(??) days with the highest posterior mean over n.reps (IS THIS HOW I SHOULD DEFINE IT?)
#'
#' A note on timezones. SM4s do not account for daylight saving changes. The tz.local (local timezone) allows us to account for changes between Standard and Daylight time.
#'
#' Because most bcp_phenology projects are going to be looking at sunrise-based audio sampling, the start.rise and end.rise arguments account for the possibility of varied sampling audio recording schemes through the years. E.g. for GLBA, start.rise = 1.5 and end.rise = 0.5 indicates that data subsetted beginning 1.5 hours before sunrise and ending 0.5 hours after sunrise.
#'
#' Audio sampling that did not use a sunrise-based sampling scheme errs on the side of adding an additional ai timestep chunk on each end of the subset.
#'
#' \itemize{
#' \item{Cool list of stuff}
#' \item{More stuff}
#' \item{and more!}
#' }
#'
#' @seealso  \code{\link{NVSPL_To_AI}}, \code{\link{SongMeter_To_NVSPL}}
#' @import AMMonitor maptools svDialogs tuneR
#' @export
#' @examples
#' \dontrun{
#'
#' # examples here
#'
#' }
#'


bcp_phenology_species <- function(db.path,
                                  speciesID,
                                  templateID,
                                  locationID,
                                  amml,
                                  ensemble.type,
                                  ai.data,
                                  eval.data,
                                  keep.all.transition.days,
                                  year,
                                  start.date,
                                  end.date,
                                  lat,
                                  long,
                                  start.rise,
                                  end.rise,
                                  tz.recorder,
                                  tz.local,
                                  w0 = 0.2,
                                  p0 = 0.2,
                                  burnin = 500,
                                  mcmc = 1000,
                                  n.reps = 100) {

  if(missing(eval.data))
    message('Spectrogram evaluation data was not provided in the "eval.data" argument, so no dates will be excluded from analysis.')

  # Read in acoustic indices data
  ai <- data.table(read.csv(file = ai.data))

  # Create a time column that reflects the time used for the audio recorder (often, but not always, UTC)
  ai[,date := as.Date(gsub('_', '-', Date))]
  ai[,dateTimeRecorder := ymd_hms(paste0(date,' ', Hr, ':', Min, ':', Sec), tz = tz.recorder)]

  # Create a UTC time column for computing sunrise-based sampling times that avoid daylight savings issues
  ai[,dateTimeUTC := with_tz(dateTimeRecorder, tzone = 'UTC')]

  # Create a local time column for downstream interpretability
  ai[,dateTimeLocal :=  with_tz(dateTimeUTC, tzone = tz.local)]

  # Ensure date and year reflect local time
  ai[,date := as.Date(dateTimeLocal)]
  ai[,yr := year(dateTimeLocal)]

  # Keep only the year & locationID input by argument
  ai <- ai[yr == year & Site == locationID]

  # Compute sunrise time for this location (using dateTimeUTC to avoid daylight savings issues)
  ai[,sunrise := sunriset(crds = matrix(c(long, lat), nrow = 1),
                          dateTime = dateTimeUTC,
                          direction = 'sunrise',
                          POSIXct = TRUE)$time]
  s.start <- start.rise * 3600
  s.end <- end.rise * 3600 # convert to hours
  ai[,sample.start := sunrise - s.start]
  ai[,sample.end := sunrise + s.end]

  # Subset all data and keep only that which falls within the designated sampling period
  # Below, we are rounding down to the nearest minute for the start, and up to the nearest
  # minute for the end, to avoid accidentally throwing away data
  ai <- ai[dateTimeLocal >= floor_date(sample.start, unit = '1 minutes') &
             dateTimeLocal <= ceiling_date(sample.end, unit = '1 minutes')]

  # If start and end dates are specified, subset further
  if (!missing(start.date) & !missing(end.date)) {
    ai <- ai[date >= as.Date(start.date) & date <= as.Date(end.date)]
  }

  # Add locationID based on site name
  ai[,locationID := Site]

  # If spectrogram evaluation & exclusion data are provided, incorporate it into the bcp
  if(!missing(eval.data)) {

    # Bring in the eval data:
    eval <- data.table(read.csv(eval.data))
    eval[,realDate := as.Date(date, '%m/%d/%Y')] # add R-friendly date

    # Figure out which dates to exclude
    exclude.dates <- eval[exclude == 1, realDate]

    # Exclude dates based on labeled spectrograms that have been analyzed
    ai[!(date %in% exclude.dates), exclude := FALSE]
    ai[(date %in% exclude.dates), exclude := TRUE]

  } else {
    # if no eval data provided, no dates are excluded
    ai[ , exclude := FALSE]
  }

  message('note to cathleen to make sure ai[exclude] is working properly in the species version')
  browser()

  # Generate automated detection species counts
  tstep <- unique(ai.exclude$timestep)
  sp.ct <- species_counts(db.path = db.path,
                          speciesID = speciesID,
                          templateID = templateID,
                          amml = amml,
                          ensemble.type = ensemble.type,
                          ai.data = ai.data,
                          locationID = locationID,
                          tz.recorder = tz.recorder, tz.local = tz.local,
                          tstep = tstep)

  # Join with ai to ensure only keeping data within specified timeframe
  ai <- ai[,c('locationID', 'dateTimeLocal')]
  sp.ct <- merge(ai, sp.ct, by = c('locationID', 'dateTimeLocal'), all.x = TRUE)

  # Generate counts by day for bcp

  # Summarize means of each plotting metric by date
  # Only keep data from non-excluded days
  dat <- sp.ct[exclude == FALSE, sum(N), by = as.Date(dateTimeLocal)]
  colnames(dat) <- c('date', 'V1')

  # Capture rep results in lists
  results <- peaks <- tr.days <- list()

  # Run bcp function using function argument inputs from MFM GLBA phenology code
  message('Running BCP ', n.reps, ' times for ', toupper(speciesID), ' at ', locationID, ' in ', year, ': ')
  for (j in 1:n.reps){

    message(j)

    # Conduct n.reps of each grid combo
    b <- bcp(y = dat$V1, w0 = w0, p0 = p0, burnin = burnin, mcmc = mcmc)

    # Capture full results in a user-friendly way for plotting
    bcp.results <- data.table(locationID = locationID,
                              speciesID = speciesID,
                              date = dat$date,
                              value = dat$V1,
                              posterior.prob = b$posterior.prob,
                              posterior.mean = b$posterior.mean[,1],
                              rep = j)
    results[[j]] <- bcp.results

    # Calculate peak day based on posterior mean
    peaks[[j]] <- bcp.results[which.max(bcp.results$posterior.mean)]

  } # end n.rep loop

  all.results <- rbindlist(results)
  all.peaks <- rbindlist(peaks)

  # Take the MEAN posterior prob of all n.reps and only store the ones that exceed 0.5 over n.reps
  # That way we don't get a fake transition day in there that randomly went above 0.5 once
  all.tr.days <- all.results[,list(mean.posterior.prob = mean(posterior.prob)),
                             by = c('locationID', 'date')][mean.posterior.prob >= 0.5]

  # All results
  all.peaks[,peak := 1]
  res <- merge(all.results, all.peaks, by = intersect(names(all.results), names(all.peaks)),
               all.x = TRUE)
  res[is.na(peak), peak := 0]
  setkey(res,locationID, date, speciesID, rep)

  # Transition days over n.reps
  tr <- unique(all.tr.days[,c('locationID', 'date')])

  # Identify transition days based on user input
  ifelse(test = keep.all.transition.days == TRUE,
         yes = final.tr.days <- tr[,date, by = 'locationID'][, transition.day := 1], # keep all
         no = final.tr.days <- tr[,.(date = range(date)), by = 'locationID'][, transition.day := 1] # only keep range
  )

  # Peak.days over n.reps
  final.peak.days <- unique(res[peak == 1][,c('locationID', 'date')])[, peak.day := 1]

  # Clean up dat for merge
  dat[,locationID := locationID]
  dat[,N := V1]
  dat[,speciesID := speciesID]
  dat[,V1 := NULL]

  # Merge info about peak and transition days identified after n.reps
  dat <- merge(dat, final.peak.days, by = c('locationID', 'date'), all.x = TRUE)
  dat <- merge(dat, final.tr.days, by = c('locationID', 'date'), all.x = TRUE)

  # Add if eval data are included, add exclusion.category and characterize
  if(!missing(eval.data)) {

    # Add in a column to show whether data were excluded or not
    # Clean up the eval data for easier plotting downstream
    eval[grep(pattern = 'wind', x = reason, ignore.case = TRUE), exclusion.category := 'Geophony']
    eval[grep(pattern = 'rain', x = reason, ignore.case = TRUE), exclusion.category := 'Geophony']
    eval[grep(pattern = 'anthr', x = reason, ignore.case = TRUE), exclusion.category := 'Anthrophony']
    eval[grep(pattern = 'no data', x = reason, ignore.case = TRUE), exclusion.category := 'No Data']
    eval[,exclusion.subcategory := reason]
    excl.why <- eval[,c('realDate', 'exclusion.category', 'exclusion.subcategory')]
    dat <- merge(dat, excl.why, by.x = 'date', by.y = 'realDate', all.x = TRUE)
  } else {
    dat[,exclusion.category := as.character(NA)]
  }

  dat[is.na(exclusion.category), exclusion.category := 'Clean']

  # For cases where early csv data gives an exclusion category, but is labeled as 0 (i.e., not excluded):
  ai.long[exclude == FALSE, exclusion.category := 'Clean']

  # Eliminate columns that become obsolete after merging
  final.tr.days[,transition.day := NULL]
  final.tr.days[,speciesID := speciesID]
  final.peak.days[,peak.day := NULL]
  final.peak.days[,speciesID := speciesID]
  dat[, Site := NULL]

  # Return excluded dates for this year
  if(!missing(eval.data)) {
    exclude.dates.yr <- exclude.dates[year(exclude.dates) == year ]
  } else {
    exclude.dates.yr <- 'Spectrogram evaluation data was not provided in the "eval.data" argument, so no dates have been excluded from analysis.'
  }

  # Return results
  results <- list(full.bcp.results = res,
                  peak.days = final.peak.days,
                  transition.days = final.tr.days,
                  species.count.data = dat,
                  excluded.dates = exclude.dates.yr)
}



# bcp_phenology_smooth =========================================================

#' @name bcp_phenology_smooth
#' @title Run Bayesian Changepoint Analysis using a single site and year based on loess smoothing
#' @description This function uses the bcp package to perform a Bayesian Changepoint Analysis for a desired site and year, based on loess smoothing of acoustic indices or species count data
#' @param db.path tbd
#' @param speciesID tbd
#' @param locationID tbd
#' @param templateID tbd
#' @param locationID Site name of monitoring location for which to perform analysis
#' @param amml tbd
#' @param span LOESS smooth span to use (link to relevant details)
#' @param ai.data File path to acoustic index csv
#' @param eval.data File path to eval days csv that includes dates to exclude from analysis
#' @param year YYYY year for which to perform analysis
#' @param keep.all.transition.days Logical flag for whether to keep ALL transition days (TRUE) or only keep the range (starting and ending) of transition days (FALSE)
#' @param lat Latitude in decimal degrees, necessary to compute sunrise times
#' @param long Latitude in decimal degrees, necessary to computer sunrise times
#' @param start.rise Character object indicating how long, in hours, before sunrise to begin data subset (necessary if processing multiple years of data with varied audio recording schedules). See Details.
#' @param end.rise Character object indicating how long, in hours, after sunrise to end data subset (necessary if processing multiple years of data with varied audio recording schedules). See Details
#' @param tz.recorder OPTIONAL. tz.recorder should now (as of 2022) be preserved in the 'timezone' column of the input CSV. However, if working with old CSVs, this should be an Olsen names timezone used by the audio recorder during data collection. For example, you may have collected data using a Wildlife Acoustics SM4, and instead of setting a local time, UTC/GMT may have been used. Note that 'UTC' and 'GMT' are synonymous and both acceptable for this function argument. This argument accounts for the fact that recordings may have been taken in UTC. Be wary of the possibility that different time settings may have been used in different years.
#' @param tz.local Olsen names timezone for local time at the monitoring location (e.g., 'America/Anchorage').
#' @param n.reps Number of times to repeat the BCP analysis to account for stochasticity
#' @return Returns a list object with full results, peak days, and transition days. Give a detailed explanation of each item.
#' @details
#'
#' This function was developed by the National Park Service Natural Sounds and Night Skies Division to perform a Bayesian Changepoint Analysis (BCP) in bioacoustics phenology projects for a selected year, acoustic monitoring site, and acoustic index metric.
#'
#' To account for stochasticity, the BCP is performed n.reps times (default n.reps = 100).
#' Transition days are defined as the earliest and latest season date for which the average posterior probability falls above 0.5
#' Peak days are defined as the most frequently occurring(??) days with the highest posterior mean over n.reps (IS THIS HOW I SHOULD DEFINE IT?)
#'
#' A note on timezones. SM4s do not account for daylight saving changes. The tz.local (local timezone) allows us to account for changes between Standard and Daylight time.
#'
#' Because most bcp_phenology projects are going to be looking at sunrise-based audio sampling, the start.rise and end.rise arguments account for the possibility of varied sampling audio recording schemes through the years. E.g. for GLBA, start.rise = 1.5 and end.rise = 0.5 indicates that data subsetted beginning 1.5 hours before sunrise and ending 0.5 hours after sunrise.
#'
#' Audio sampling that did not use a sunrise-based sampling scheme errs on the side of adding an additional ai timestep chunk on each end of the subset.
#'
#' \itemize{
#' \item{Cool list of stuff}
#' \item{More stuff}
#' \item{and more!}
#' }
#'
#' @seealso  \code{\link{NVSPL_To_AI}}, \code{\link{SongMeter_To_NVSPL}}
#' @import AMMonitor maptools svDialogs tuneR
#' @export
#' @examples
#' \dontrun{
#'
#' # examples here
#'
#' }
#'


bcp_phenology_smooth <- function(db.path,
                                 speciesID,
                                 templateID,
                                 locationID,
                                 amml,
                                 ensemble.type,
                                 span,
                                 ai.data,
                                 ai.variable,
                                 eval.data,
                                 keep.all.transition.days,
                                 year,
                                 start.date,
                                 end.date,
                                 lat,
                                 long,
                                 start.rise,
                                 end.rise,
                                 tz.recorder,
                                 tz.local,
                                 w0 = 0.2,
                                 p0 = 0.2,
                                 burnin = 500,
                                 mcmc = 1000,
                                 n.reps = 100) {

  if(missing(eval.data))
    message('Spectrogram evaluation data was not provided in the "eval.data" argument, so no dates will be excluded from analysis.')

  if(missing(db.path) | missing(speciesID) | missing(templateID) | missing(amml) | missing(ensemble.type)) {
    message('\nArgument inputs for db.path, speciesID, templateID, amml, and/or ensemble.type are missing. Therefore, BCP analysis will be conducted on acoustic index data rather than on species automated detection data. If this is not desired, please stop the function and supply these inputs. View help documentation at ?bcp_phenology_smooth.')
    bcp.type <- 'ai'
  } else {
    message('\nBecause db.path, speciesID, templateID, amml, and ensemble.type have been supplied, BCP analysis will be conducted on species automated detection counts. If this is not desired, please stop the function and view help documentation at ?bcp_phenology_smooth.'    )
    bcp.type <- 'counts'
  }

  # Read in acoustic indices data
  ai <- data.table(read.csv(file = ai.data))

  ai[,date := as.Date(gsub('_', '-', Date))]

  # Find all unique timezones used for this project -- likely just GMT and/or local time
  tz.recorders <- unique(ai$timezone) # find unique timezones

  # Can't easily vectorize the timezone conversion, so instead we loop through
  # timezones to recorder, UTC, and local time columns
  for (tzr in 1:length(tz.recorders)) {

    this.tz <- tz.recorders[tzr]

    # Create a UTC time column for computing sunrise-based sampling times that
    # avoid daylight savings issues (ymd_hms wil create this in UTC)
    ai[timezone == this.tz,
       dateTimeUTC := ymd_hms(paste0(date,' ', Hr, ':', Min, ':', Sec), tz = this.tz)]

    # Create a local time column for downstream interpretability
    ai[timezone == this.tz,
       dateTimeLocal :=  with_tz(dateTimeUTC, tzone = tz.local)]

    # Ensure date and year reflect local time
    ai[timezone == this.tz, date := as.Date(dateTimeLocal, tz = this.tz)]
    ai[timezone == this.tz,yr := year(dateTimeLocal)]

  }

  # can't check/view data.tables in debugger for some reason??

  # view(ai[,c('Date', 'Yr', 'Mo', 'Day', 'Hr', 'Min', 'Sec', 'timezone', 'dateTimeUTC', 'dateTimeLocal', 'date', 'yr')])


  # Keep only the year & locationID input by argument
  ai <- ai[yr == year & Site == locationID]

  # Compute sunrise time for this location (using dateTimeUTC to avoid daylight savings issues)
  ai[,sunrise := sunriset(crds = matrix(c(long, lat), nrow = 1),
                          dateTime = dateTimeUTC,
                          direction = 'sunrise',
                          POSIXct = TRUE)$time]
  s.start <- start.rise * 3600
  s.end <- end.rise * 3600 # convert to hours
  ai[,sample.start := sunrise - s.start]
  ai[,sample.end := sunrise + s.end]

  # Subset all data and keep only that which falls within the designated sampling period
  # Below, we are rounding down to the nearest minute for the start, and up to the nearest
  # minute for the end, to avoid accidentally throwing away data
  # For earlier years at GLBA that sampled in 10 minute increments, this will lead
  # to 11-13 10-minute timesteps per day depending on where the minute falls...
  # This is another place where changes in sampling through time need to be carried through systematically
  # For later years based on sun, it will be 12 each day if unit = 1 minutes
  # What if unit = 10 minutes? Nope -- don't do this. It will allow more data
  # to be sampled from the 24 hour data and then lead to unfair comparison.
  # Will just have to to accept that subsetting of 24-hour data occurring on 10min increments
  # will lead to between 11-13 timesteps per day, when trying to compare against
  # sunrise-based sampling on a fixed schedule.
  # The other, more precise way is just to clip the earlier 24-hour data to sunrise schedule and process from there
  # but the conclusions will likely be the same.
  ai <- ai[dateTimeLocal >= floor_date(sample.start, unit = '1 minutes') &
             dateTimeLocal <= ceiling_date(sample.end, unit = '1 minutes')]

  # If start and end dates are specified, subset further
  if (!missing(start.date) & !missing(end.date)) {
    ai <- ai[date >= as.Date(start.date) & date <= as.Date(end.date)]
  }

  # Add locationID based on site name
  ai[,locationID := Site]

  # If spectrogram evaluation & exclusion data are provided, incorporate it into the bcp
  if(!missing(eval.data)) {

    # Bring in the eval data:
    eval <- data.table(read.csv(eval.data))
    eval[,realDate := as.Date(date, '%m/%d/%Y')] # add R-friendly date

    # Figure out which dates to exclude
    exclude.dates <- eval[exclude == 1, realDate]

    # Exclude dates based on labeled spectrograms that have been analyzed
    ai[!(date %in% exclude.dates), exclude := FALSE]
    ai[(date %in% exclude.dates), exclude := TRUE]

    #  ai.exclude <- ai[!(date %in% exclude.dates)]

  } else {
    # if no eval data provided, no dates are excluded
    ai[,exclude := FALSE]
  }

  # Initialize the timestep
  tstep <- unique(ai$Timestep)

  # Prep data if using species counts
  if (bcp.type == 'counts') {

    # Generate automated detection species counts
    sp.ct <- species_counts(db.path = db.path,
                            speciesID = speciesID,
                            templateID = templateID,
                            amml = amml,
                            ensemble.type = ensemble.type,
                            ai.data = ai.data, # want original ai.data input
                            locationID = locationID,
                            tz.recorder = tz.recorder, tz.local = tz.local,
                            tstep = tstep)

    # Join with aito ensure only keeping data within specified timeframe
    ai <- ai[,c('locationID', 'dateTimeLocal', 'exclude')]
    sp.ct <- merge(ai, sp.ct, by = c('locationID', 'dateTimeLocal'), all.x = TRUE)

    # Summarize means of each plotting metric by date
    dat <- sp.ct[, sum(N), by = as.Date(dateTimeLocal)]
    colnames(dat) <- c('date', 'V1')

    # Merge back with ai to get exclude column
    dat <- merge(dat, unique(ai[,c('date', 'exclude')]), by = 'date', all.x = TRUE)

  }

  # Prep data if using Acoustic Indices
  if (bcp.type == 'ai') {

    # Recast to long form for ggplot
    ai.long <- data.table(melt(ai, measure.vars = ai.variable,
                               variable.name = 'metric', value.name = 'value'))

    # Only keep desired rows & useful columns for analysis
    ai.long <- ai.long[metric == ai.variable,
                       c('Site', 'yr', 'date', 'dateTimeUTC', 'dateTimeLocal', 'metric', 'value', 'exclude')]

    # Add locationID based on site name
    ai.long[,locationID := locationID]

    # Summarize means of each plotting metric by date
    dat <- ai.long[ , mean(value), by = c('locationID', 'yr', 'date', 'metric')]

    colnames(dat) <- c('locationID', 'yr', 'date', 'metric', 'V1')

    # Merge back with AI to get exclude column
    dat <- merge(dat, unique(ai[,c('date', 'exclude')]), by = 'date', all.x = TRUE)

  }

  # Generate a smoothed loess model upon which to run the BCP
  dat.exclude <- dat[exclude == FALSE] # only run on the data that isn't excluded
  dat.exclude[,index := 1:length(date)]
  loess.mod <- loess(V1 ~ index, data = dat.exclude, span = span) # enter span given by user
  smoothed <- predict(loess.mod)

  # Capture rep results in lists
  results <- peaks <- tr.days <- list()

  # Run bcp function using function argument inputs from MFM GLBA phenology code
  if (bcp.type == 'counts') {
    message('\nRunning BCP ', n.reps, ' times for ', toupper(speciesID), ' at ', locationID, ' in ', year, ': ')
  }
  if (bcp.type == 'ai') {
    message('\nRunning BCP ', n.reps, ' times for ', ai.variable, ' at ', locationID, ' in ', year, ': ')
  }

  for (j in 1:n.reps){

    message(j)

    # Conduct n.reps of each grid combo
    b <- bcp(y = smoothed, w0 = w0, p0 = p0, burnin = burnin, mcmc = mcmc)

    # Capture full results in a user-friendly way for plotting
    if (bcp.type == 'counts') {
      bcp.results <- data.table(locationID = locationID,
                                speciesID = speciesID,
                                date = dat.exclude$date,
                                value = dat.exclude$V1,
                                smooth = smoothed,
                                posterior.prob = b$posterior.prob,
                                posterior.mean = b$posterior.mean[,1],
                                rep = j)
    }
    if (bcp.type == 'ai') {
      bcp.results <- data.table(locationID = locationID,
                                date = dat.exclude$date,
                                metric = ai.variable,
                                value = dat.exclude$V1,
                                smooth = smoothed,
                                posterior.prob = b$posterior.prob,
                                posterior.mean = b$posterior.mean[,1],
                                rep = j)
    }
    results[[j]] <- bcp.results

    # Calculate peak day based on posterior mean
    peaks[[j]] <- bcp.results[which.max(bcp.results$posterior.mean)]

  } # end n.rep loop

  all.results <- rbindlist(results)
  all.peaks <- rbindlist(peaks)

  # Take the MEAN posterior prob of all n.reps and only store the ones that exceed 0.5 over n.reps
  # That way we don't get a fake transition day in there that randomly went above 0.5 once
  all.tr.days <- all.results[,list(mean.posterior.prob = mean(posterior.prob)),
                             by = c('locationID', 'date')][mean.posterior.prob >= 0.5]

  # All results
  all.peaks[,peak := 1]
  res <- merge(all.results, all.peaks, by = intersect(names(all.results), names(all.peaks)),
               all.x = TRUE)
  res[is.na(peak), peak := 0]
  setkey(res,locationID, date, rep)

  # Transition days over n.reps
  tr <- unique(all.tr.days[,c('locationID', 'date')])

  # Identify transition days based on user input
  ifelse(test = keep.all.transition.days == TRUE,
         yes = final.tr.days <- tr[,date, by = 'locationID'][, transition.day := 1], # keep all
         no = final.tr.days <- tr[,.(date = range(date)), by = 'locationID'][, transition.day := 1] # only keep range
  )

  # Peak.days over n.reps
  final.peak.days <- unique(res[peak == 1][,c('locationID', 'date')])[, peak.day := 1]


  # Clean up dat for merge
  if(bcp.type == 'counts') {

    dat[,locationID := locationID]
    dat[,N := V1]
    dat[,speciesID := speciesID]
    dat[,V1 := NULL]
  }
  if (bcp.type == 'ai') {
    dat[, locationID := locationID]
    dat[, ai.variable := V1]
    dat[,V1 := NULL]
  }

  # Merge info about peak and transition days identified after n.reps
  dat <- merge(dat, final.peak.days, by = c('locationID', 'date'), all.x = TRUE)
  dat <- merge(dat, final.tr.days, by = c('locationID', 'date'), all.x = TRUE)

  # Add if eval data are included, add exclusion.category and characterize

  if(!missing(eval.data)) {

    # Add in a column to show whether data were excluded or not
    # Clean up the eval data for easier plotting downstream
    eval[grep(pattern = 'wind', x = reason, ignore.case = TRUE), exclusion.category := 'Geophony']
    eval[grep(pattern = 'rain', x = reason, ignore.case = TRUE), exclusion.category := 'Geophony']
    eval[grep(pattern = 'anthr', x = reason, ignore.case = TRUE), exclusion.category := 'Anthrophony']
    eval[grep(pattern = 'no data', x = reason, ignore.case = TRUE), exclusion.category := 'No Data']
    eval[,exclusion.subcategory := reason]
    excl.why <- eval[,c('realDate', 'exclusion.category', 'exclusion.subcategory')]
    dat <- merge(dat, excl.why, by.x = 'date', by.y = 'realDate', all.x = TRUE)
  } else {
    dat[,exclusion.category := as.character(NA)]
  }

  dat <- unique(dat) # sometimes things get duplicated in one of these merges??

  # Clean up and eliminate columns that become obsolete after merging
  dat[is.na(exclusion.category), exclusion.category := 'Clean']

  # For cases where early csv data gives an exclusion category, but is labeled as 0 (i.e., not excluded):
  dat[exclude == FALSE, exclusion.category := 'Clean']

  if (bcp.type == 'counts') {
    final.tr.days[,speciesID := speciesID]
    final.peak.days[,speciesID := speciesID]
  }
  final.tr.days[,transition.day := NULL]
  final.peak.days[,peak.day := NULL]

  # Merge in the values from the loess.smooth model to the final dataset
  # (only dates included in the analysis will have index & loess.smooth values)
  dat.exclude[, loess.smooth := smoothed]
  dat <- merge(x = dat,
               y = dat.exclude[, c('date', 'locationID', 'yr', 'index', 'loess.smooth')],
               by = c('date', 'locationID', 'yr'),
               all.x = TRUE)

  # Return excluded dates for this year
  if(!missing(eval.data)) {
    exclude.dates.yr <- exclude.dates[year(exclude.dates) == year ]
  } else {
    exclude.dates.yr <- 'Spectrogram evaluation data was not provided in the "eval.data" argument, so no dates have been excluded from analysis.'
  }

  # Return results
  if (bcp.type == 'counts') {
    results <- list(full.bcp.results = res,
                    peak.days = final.peak.days,
                    transition.days = final.tr.days,
                    species.count.data = dat,
                    excluded.dates = exclude.dates.yr)
  }
  if (bcp.type == 'ai'){
    results <- list(full.bcp.results = res,
                    peak.days = final.peak.days,
                    transition.days = final.tr.days,
                    ai.data = dat,
                    excluded.dates = exclude.dates.yr)

  }
}


# species_counts ===============================================================

#' @name species_counts
#' @title Generate automated species detection counts
#' @description Generate automated species detection counts by timestep based on AMMonitor classifications and acoustic index data
#' @param db.path tbd
#' @param speciesID tbd
#' @param templateID tbd
#' @param amml tbd
#' @param ensemble.type tbd
#' @param ai.data File path to acoustic index csv
#' @param locationID Site name of monitoring location for which to perform analysis
#' @param tz.recorder Olsen names timezone used by the audio recorder during data collection. For example, you may have collected data using a Wildlife Acoustics SM4, and instead of setting a local time, UTC/GMT likely may have been used. Note that 'UTC' and 'GMT' are synonymous and both acceptable for this function argument. This argument accounts for the fact that recordings may have been taken in UTC. The tz.local argument then allows us to convert the times to local times that will make sense for analysis.
#' @param tz.local Olsen names timezone for local time at the monitoring location (e.g., 'America/Anchorage').
#' @return Returns a list object with full results, peak days, and transition days. Give a detailed explanation of each item.
#' @details
#'
#' This function was developed by the National Park Service Natural Sounds and Night Skies Division to perform a Bayesian Changepoint Analysis (BCP) in bioacoustics phenology projects for a selected year, acoustic monitoring site, and acoustic index metric.
#'
#' To account for stochasticity, the BCP is performed n.reps times (default n.reps = 100).
#' Transition days are defined as the earliest and latest season date for which the average posterior probability falls above 0.5
#' Peak days are defined as the most frequently occurring(??) days with the highest posterior mean over n.reps (IS THIS HOW I SHOULD DEFINE IT?)
#'
#' A note on timezones. SM4s do not account for daylight saving changes. The tz.local (local timezone) allows us to account for changes between Standard and Daylight time.
#'
#' Because most bcp_phenology projects are going to be looking at sunrise-based audio sampling, the start.rise and end.rise arguments account for the possibility of varied sampling audio recording schemes through the years. E.g. for GLBA, start.rise = 1.5 and end.rise = 0.5 indicates that data subsetted beginning 1.5 hours before sunrise and ending 0.5 hours after sunrise.
#'
#' Audio sampling that did not use a sunrise-based sampling scheme errs on the side of adding an additional ai timestep chunk on each end of the subset.
#'
#' \itemize{
#' \item{Cool list of stuff}
#' \item{More stuff}
#' \item{and more!}
#' }
#'
#' @seealso  \code{\link{NVSPL_To_AI}}, \code{\link{SongMeter_To_NVSPL}}, \code{\link{bcp_phenology_ai}}, \code{\link{bcp_phenology_smooth}},  \code{\link{bcp_phenology_counts}}
#' @import AMMonitor maptools svDialogs tuneR
#' @export
#' @examples
#' \dontrun{
#'
#' # examples here
#'
#' }

species_counts <- function(db.path,
                           speciesID,
                           templateID,
                           locationID,
                           amml,
                           ensemble.type = 'precision',
                           ai.data,
                           tz.recorder,
                           tz.local,
                           tstep = 10) {

  # Read in acoustic indices data
  ai <- data.table(read.csv(file = ai.data))

  # Create a time column that reflects the time used for the audio recorder (often, but not always, UTC)
  ai[,date := as.Date(gsub('_', '-', Date))]
  ai[,dateTimeRecorder := ymd_hms(paste0(date,' ', Hr, ':', Min, ':', Sec), tz = tz.recorder)]

  # Create a UTC time column for computing sunrise-based sampling times that avoid daylight savings issues
  ai[,dateTimeUTC := with_tz(dateTimeRecorder, tzone = 'UTC')]

  # Create a local time column for downstream interpretability
  ai[,dateTimeLocal :=  with_tz(dateTimeUTC, tzone = tz.local)]

  # Connect to db
  conx <- RSQLite::dbConnect(drv = dbDriver('SQLite'), dbname = db.path)
  RSQLite::dbExecute(conn = conx, statement = "PRAGMA foreign_keys = ON;")

  # Read in template
  tmp <- templatesUnserialize(db.path, templateID)
  dur <- tmp@templates[[1]]@duration

  # Read in entire classifications table for species
  classifs <- qry(db.path = db.path, table = 'classifications')
  classifs <- classifs[grep(pattern = templateID, x = modelName, fixed = TRUE)]

  # Create ensemble preds
  ens <- classifierEnsembleM(db.path = db.path,
                             amml = classifiers,
                             ensemble = ensemble.type,
                             classificationsDT = classifs)
  scrids <- ens[predicted == 1, scoreID]

  # Now join the species scoreIDs with recordingID
  recs <- qry(db.path, statement = 'SELECT scoreID, recordingID, time FROM scores
            WHERE scoreID = $scoreID ',
              params = list(scoreID = scrids))

  # Count N detections by timestep
  recs <- add_time_cols(dt = recs,
                        tz.local = tz.local,
                        tz.recorder = tz.recorder,
                        timestep = tstep)
  recs <- recs[locationID == locationID]

  # Merge ai with the nstep to get 0 counts for the species

  # Figure out whether the ai.data used a start.at.beginning routine or not
  ifelse(test = sum(ai$Min %in% c(0, 10, 20, 30, 40, 50)) == nrow(ai),
         yes = start.at.beginning <- FALSE,
         no = start.at.beginning <- TRUE)

  # This only works if a start.at.beginning routine was NOT used and everything occurred on predictable hourly timesteps
  if(start.at.beginning == FALSE) {
    n.by.timestep <- recs[,.N, by = c('locationID', 'Date', 'Hr', 'Min')]
    ai_merge <- merge(ai, n.by.timestep,
                      by = c('locationID', 'Date', 'Hr', 'Min'), all.x = TRUE)
  }

  if(start.at.beginning == TRUE) {

    # Summarize species detection count by minute to join to acoustic inds
    n.by.timestep <- recs[,.N, by = c('locationID', 'detectionTimeMinute')]

    # The start.at.beginning NVSPLtoAI routine means that timestep bins will change
    # on a near daily basis with the sunrise. Instead of recalculating bins each
    # day this method does a simple rolling join to identify the nearest AI time bin
    # https://www.r-bloggers.com/2016/06/understanding-data-table-rolling-joins/
    n.by.timestep[,join_col := detectionTimeMinute]
    ai[,join_col := dateTimeLocal]
    setkey(ai, join_col)
    setkey(n.by.timestep, join_col)
    merge_ai <- ai[n.by.timestep, roll = TRUE, rollends = c(TRUE,TRUE)]
    #  merge_ai <- merge_ai[,c('dateTime', 'detectionTimeMinute','join_col', 'locationID', 'N')]
    n.by.timestep.2 <- merge_ai[,sum(N), by = 'dateTimeLocal'] # NOW we finally have a close approximation of the N by AI timestep -- may be a little off bc rounds to nearest bin rather than the 10 minutes after, but good enough
    colnames(n.by.timestep.2)[colnames(n.by.timestep.2) == 'V1'] <- 'N'

    # Find dateTimes missing from the merged data
    # (where AI data were collected but no target species detections exist)
    miss_dt <- data.table(dateTimeLocal = ai$dateTimeLocal[!(ai$dateTimeLocal %in% merge_ai$dateTimeLocal)],
                          N = as.numeric(NA))
    ai_merge <- rbind(n.by.timestep.2, miss_dt)

    # Merge back with the AI results to get all the AI data back
    ai_merge <- merge(x = ai_merge, y = ai, by = 'dateTimeLocal')

    setkey(ai_merge, dateTimeLocal)
  }

  ai_merge[,speciesID := speciesID]

  # Add total duration of template-detected singing time within timestep increment
  ai_merge[is.na(N), N := 0] # NAs mean there were no detections - correct to 0
  ai_merge[,duration := N*dur]
  ai_merge[,locationID := Site]
  ai_merge <- ai_merge[,c('locationID', 'dateTimeLocal', 'speciesID', 'N', 'duration')]
}



# phenology_smooth =============================================================

#' @name phenology_smooth
#' @title Use LOESS smoothing to track phenology.
#' @description Create loess smoothing model to capture trends of acoustic indices or species count data. Simple version with no BCP analysis necessary; peak days can be identified directly from peak in loess curve; transition periods can be identified based on thoughtful and biologically meaningful definitions of transitions.
#' @param db.path tbd
#' @param speciesID tbd
#' @param locationID tbd
#' @param templateID tbd
#' @param locationID Site name of monitoring location for which to perform analysis
#' @param amml tbd
#' @param span LOESS smooth span to use (link to relevant details)
#' @param ai.data File path to acoustic index csv
#' @param eval.data File path to eval days csv that includes dates to exclude from analysis
#' @param year YYYY year for which to perform analysis
#' @param lat Latitude in decimal degrees, necessary to compute sunrise times
#' @param long Latitude in decimal degrees, necessary to computer sunrise times
#' @param start.rise Character object indicating how long, in hours, before sunrise to begin data subset (necessary if processing multiple years of data with varied audio recording schedules). See Details.
#' @param end.rise Character object indicating how long, in hours, after sunrise to end data subset (necessary if processing multiple years of data with varied audio recording schedules). See Details
#' @param tz.recorder Olsen names timezone used by the audio recorder during data collection. For example, you may have collected data using a Wildlife Acoustics SM4, and instead of setting a local time, UTC/GMT likely may have been used. Note that 'UTC' and 'GMT' are synonymous and both acceptable for this function argument. This argument accounts for the fact that recordings may have been taken in UTC. The tz.local argument then allows us to convert the times to local times that will make sense for analysis.
#' @param tz.local Olsen names timezone for local time at the monitoring location (e.g., 'America/Anchorage').
#' @return Returns a list object with stuff...
#' @details
#'
#' add deetzs
#'
#' \itemize{
#' \item{Cool list of stuff}
#' \item{More stuff}
#' \item{and more!}
#' }
#'
#' @seealso  \code{\link{NVSPL_To_AI}}, \code{\link{SongMeter_To_NVSPL}}
#' @import AMMonitor maptools svDialogs tuneR
#' @export
#' @examples
#' \dontrun{
#'
#' # examples here
#'
#' }
#'


phenology_smooth <- function(db.path,
                             speciesID,
                             templateID,
                             locationID,
                             amml,
                             ensemble.type,
                             span,
                             ai.data,
                             ai.variable,
                             eval.data,
                             year,
                             start.date,
                             end.date,
                             lat,
                             long,
                             start.rise,
                             end.rise,
                             tz.recorder,
                             tz.local) {

  if(missing(eval.data))
    message('Spectrogram evaluation data was not provided in the "eval.data" argument, so no dates will be excluded from analysis.')

  if(missing(db.path) | missing(speciesID) | missing(templateID) | missing(amml) | missing(ensemble.type)) {
    message('\nArgument inputs for db.path, speciesID, templateID, amml, and/or ensemble.type are missing. Therefore, analysis will be conducted on acoustic index data rather than on species automated detection data. If this is not desired, please stop the function and supply these inputs. View help documentation at ?phenology_smooth.')
    smooth.type <- 'ai'
  } else {
    message('\nBecause db.path, speciesID, templateID, amml, and ensemble.type have been supplied, analysis will be conducted on species automated detection counts. If this is not desired, please stop the function and view help documentation at ?phenology_smooth.'    )
    smooth.type <- 'counts'
  }

  # Read in acoustic indices data
  ai <- data.table(read.csv(file = ai.data))

  # Create a time column that reflects the time used for the audio recorder (often, but not always, UTC)
  ai[,date := as.Date(gsub('_', '-', Date))]
  ai[,dateTimeRecorder := ymd_hms(paste0(date,' ', Hr, ':', Min, ':', Sec), tz = tz.recorder)]

  # Create a UTC time column for computing sunrise-based sampling times that avoid daylight savings issues
  ai[,dateTimeUTC := with_tz(dateTimeRecorder, tzone = 'UTC')]

  # Create a local time column for downstream interpretability
  ai[,dateTimeLocal :=  with_tz(dateTimeUTC, tzone = tz.local)]

  # Ensure date and year reflect local time
  ai[,date := as.Date(dateTimeLocal)]
  ai[,yr := year(dateTimeLocal)]

  # Keep only the year & locationID input by argument
  ai <- ai[yr == year & Site == locationID]

  # Compute sunrise time for this location (using dateTimeUTC to avoid daylight savings issues)
  ai[,sunrise := sunriset(crds = matrix(c(long, lat), nrow = 1),
                          dateTime = dateTimeUTC,
                          direction = 'sunrise',
                          POSIXct = TRUE)$time]
  s.start <- start.rise * 3600
  s.end <- end.rise * 3600 # convert to hours
  ai[,sample.start := sunrise - s.start]
  ai[,sample.end := sunrise + s.end]

  # Subset all data and keep only that which falls within the designated sampling period
  # Below, we are rounding down to the nearest minute for the start, and up to the nearest
  # minute for the end, to avoid accidentally throwing away data
  ai <- ai[dateTimeLocal >= floor_date(sample.start, unit = '1 minutes') &
             dateTimeLocal <= ceiling_date(sample.end, unit = '1 minutes')]

  # If start and end dates are specified, subset further
  if (!missing(start.date) & !missing(end.date)) {
    ai <- ai[date >= as.Date(start.date) & date <= as.Date(end.date)]
  }

  # Add locationID based on site name
  ai[,locationID := Site]

  # If spectrogram evaluation & exclusion data are provided, incorporate it into the bcp
  if(!missing(eval.data)) {

    # Bring in the eval data:
    eval <- data.table(read.csv(eval.data))
    eval[,realDate := as.Date(date, '%m/%d/%Y')] # add R-friendly date

    # Figure out which dates to exclude
    exclude.dates <- eval[exclude == 1, realDate]

    # Exclude dates based on labeled spectrograms that have been analyzed
    ai[!(date %in% exclude.dates), exclude := FALSE]
    ai[(date %in% exclude.dates), exclude := TRUE]

    #  ai.exclude <- ai[!(date %in% exclude.dates)]

  } else {
    # if no eval data provided, no dates are excluded
    ai[,exclude := FALSE]
  }

  # Initialize the timestep
  tstep <- unique(ai$Timestep)

  # Prep data if using species counts
  if (smooth.type == 'counts') {

    # Generate automated detection species counts
    sp.ct <- species_counts(db.path = db.path,
                            speciesID = speciesID,
                            templateID = templateID,
                            amml = amml,
                            ensemble.type = ensemble.type,
                            ai.data = ai.data, # want original ai.data input
                            locationID = locationID,
                            tz.recorder = tz.recorder, tz.local = tz.local,
                            tstep = tstep)

    # Join with aito ensure only keeping data within specified timeframe
    ai <- ai[,c('locationID', 'dateTimeLocal', 'exclude')]
    sp.ct <- merge(ai, sp.ct, by = c('locationID', 'dateTimeLocal'), all.x = TRUE)

    # Summarize means of each plotting metric by date
    dat <- sp.ct[, sum(N), by = as.Date(dateTimeLocal)]
    colnames(dat) <- c('date', 'V1')

    # Merge back with ai to get exclude column
    dat <- merge(dat, unique(ai[,c('date', 'exclude')]), by = 'date', all.x = TRUE)

  }

  # Prep data if using Acoustic Indices
  if (smooth.type == 'ai') {

    # Recast to long form for ggplot
    ai.long <- data.table(melt(ai, measure.vars = ai.variable,
                               variable.name = 'metric', value.name = 'value'))

    # Only keep desired rows & useful columns for analysis
    ai.long <- ai.long[metric == ai.variable,
                       c('Site', 'yr', 'date', 'dateTimeUTC', 'dateTimeLocal', 'metric', 'value', 'exclude')]

    # Add locationID based on site name
    ai.long[,locationID := locationID]

    # Summarize means of each plotting metric by date
    dat <- ai.long[ , mean(value), by = c('locationID', 'yr', 'date', 'metric')]

    colnames(dat) <- c('locationID', 'yr', 'date', 'metric', 'V1')

    # Merge back with AI to get exclude column
    dat <- merge(dat, unique(ai[,c('date', 'exclude')]), by = 'date', all.x = TRUE)

  }

  # Generate a smoothed loess model upon which to run the BCP
  dat.exclude <- dat[exclude == FALSE] # only run on the data that isn't excluded
  dat.exclude[,index := 1:length(date)]
  loess.mod <- loess(V1 ~ index, data = dat.exclude, span = span) # enter span given by user
  smoothed <- predict(loess.mod)

  # Capture full results in a user-friendly way for plotting
  if (smooth.type == 'counts') {
    smooth.results <- data.table(locationID = locationID,
                                 speciesID = speciesID,
                                 date = dat.exclude$date,
                                 value = dat.exclude$V1,
                                 loess.smooth = smoothed)
  }

  if (smooth.type == 'ai') {
    smooth.results <- data.table(locationID = locationID,
                                 date = dat.exclude$date,
                                 metric = ai.variable,
                                 value = dat.exclude$V1,
                                 loess.smooth = smoothed)
  }

  # Add peaks
  smooth.results[loess.smooth == max(loess.smooth), peak.day := 1]
  smooth.results[is.na(peak.day), peak.day := 0]

  # Join smoothed resutls with overall data
  dat <- merge(dat, smooth.results, by = c('date', 'locationID', 'metric'), all.x = TRUE)

  # Clean up dat
  if(smooth.type == 'counts') {
    dat[,N := V1]
    dat[,speciesID := speciesID]
  }
  if (smooth.type == 'ai') {
    dat[, ai.variable := V1]
  }
  dat[,V1 := NULL]

  # Add if eval data are included, add exclusion.category and characterize
  if(!missing(eval.data)) {

    # Add in a column to show whether data were excluded or not
    # Clean up the eval data for easier plotting downstream
    eval[grep(pattern = 'wind', x = reason, ignore.case = TRUE), exclusion.category := 'Geophony']
    eval[grep(pattern = 'rain', x = reason, ignore.case = TRUE), exclusion.category := 'Geophony']
    eval[grep(pattern = 'anthr', x = reason, ignore.case = TRUE), exclusion.category := 'Anthrophony']
    eval[grep(pattern = 'no data', x = reason, ignore.case = TRUE), exclusion.category := 'No Data']
    eval[,exclusion.subcategory := reason]
    excl.why <- eval[,c('realDate', 'exclusion.category', 'exclusion.subcategory')]
    dat <- merge(dat, excl.why, by.x = 'date', by.y = 'realDate', all.x = TRUE)
  } else {
    dat[,exclusion.category := as.character(NA)]
  }

  # Clean up and eliminate columns that become obsolete after merging
  dat[is.na(exclusion.category), exclusion.category := 'Clean']

  # For cases where early csv data gives an exclusion category, but is labeled as 0 (i.e., not excluded):
  dat[exclude == FALSE, exclusion.category := 'Clean']

  # Return excluded dates for this year
  if(!missing(eval.data)) {
    exclude.dates.yr <- exclude.dates[year(exclude.dates) == year ]
  } else {
    exclude.dates.yr <- 'Spectrogram evaluation data was not provided in the "eval.data" argument, so no dates have been excluded from analysis.'
  }

  # Return results
  if (smooth.type == 'counts') {
    results <- list(species.count.data = dat,
                    excluded.dates = exclude.dates.yr)
  }
  if (smooth.type == 'ai'){
    results <- list(ai.data = dat,
                    excluded.dates = exclude.dates.yr)

  }
}
