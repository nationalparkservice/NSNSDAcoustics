
# birdnet_heatmap_time =================================================================

#' @name birdnet_heatmap_time
#' @title Plot heat maps of BirdNET detections by date and time of day
#' @description Plot heat maps of BirdNET results for a selected species and
#' above a selected confidence threshold by date and time of day for multiple or
#' single year data. See Details.
#' @param data Data.table or data.frame of BirdNET results that a user would like
#' to plot. Generally, this data object should be preceded by a call to \code{\link{add_time_cols}};
#'  all data should come from a single site and the object must contain columns
#'  named "locationID" (character), "recordingID" (character), and "dateTimeLocal" (POSIXct).
#'  Multiple years of data are allowed in one dataset to enable easy comparison of
#'  vocal activity by year at a site.
#' @param locationID Character input of the locationID for which data should be plotted.
#' @param common.name Character input of the target species for which data should
#' be plotted.
#' @param conf.threshold Numeric input of the BirdNET confidence threshold above
#' which data should be plotted. Detections below this confidence threshold will
#'  be discarded.
#' @param dates.sampled Date or character vector of all dates sampled that should
#' be visualized on the heat map. This information is required because your data
#'  input may only contain detection data, and not non-detection data (i.e., zeroes).
#'  For example, you might have recorded audio on 2021-03-14, but have no BirdNET
#'  detections in "data". This will result in an inaccurate visual. Since BirdNET
#'  results do not automatically contain non-detection data, it is incumbent on
#'  the user to input which dates were sampled.
#' @param julian.breaks Optional numeric vector of julian date plotting breaks
#' to use on the x axis. If omitted, will be computed automatically. Example
#' inputs: c(140, 160, 180) would graph 3 breaks on the x axis (May 20, June 9,
#' and June 29 for non-leap year data); c(130:160) would graph every single date
#' from May 10 to June 9 on the x axis (for non-leap year data). See also
#' \code{\link{readable_julian_breaks}}. Please start with 1 for the first day
#' of the year rather than 0. \href{https://www.cdfa.ca.gov/ahfss/mpes/pdfs/Julian_Calendar.pdf}{This chart}
#' might be helpful for choosing julian breaks.
#' @param tz.local Character Olsen names timezone for local time at the monitoring
#'  location (e.g., 'America/Los_angeles').
#' @param comparable.color.breaks Logical flag for whether to create heat map
#' color breaks based on all species in the input data set or based only on the
#' species of interest in this plot. TRUE means it will be easier to make straightforward
#'  comparisons between species, FALSE means activity contrasts within a single
#'  species will be easier to see.
#' @param minute.timestep Integer input of how to summarize the data by minute.
#' Any divisor of 60 is allowed in options c(1, 2, 3, 4, 5, 6, 10, 12, 15, 20, 30, 60).
#' @param hours.sampled Either an integer vector declaring which hours were sampled
#'  across the monitoring period (e.g., c(6:8, 18:20)), or a list declaring sun-based
#'  monitoring based on how many hours before and after sunset were recorded, e.g.
#'  list(sunrise = c(1.5, 1.5), sunset = c(1, 1)) means that the schedule recorded
#'  1.5 hours before sunrise until 1.5 hours after, and 1 hour before sunset to
#'  1 hour after. If missing hours.sampled, the function assumes continuous
#'  sampling and will display the plot as such; beware that this may misrepresent
#'  your data: if you did not sample during all hours, the plot will make it
#'  appear as if you did.
#' @param y.axis.limits Length 2 integer vector of hourly limits to show. c(0, 23) is default.
#' @param plot.title User input plot title if desired.
#' @param sun.lines Optional character vector of sun-based lines to plot, from \code{\link[suncalc]{getSunlightTimes}}.
#' Options include: c('sunrise', 'sunriseEnd', 'goldenHourEnd', 'solarNoon',
#' 'goldenHour', 'sunsetStart', 'sunset', 'dusk', 'nauticalDusk', 'night',
#' 'nadir', 'nightEnd', 'nauticalDawn', 'dawn')
#' @param sun.linetypes If using sun.lines, include an accompanying equal-length
#' character vector of linetypes in options: c('solid', 'dashed', 'dotted',
#' dotdash', 'longdash', 'twodash')
#' @param latitude Numeric latitude. Required if using sun.lines argument.
#' @param longitude Numeric longitude. Required if using sun.lines argument.
#' @return Heat map of BirdNET detections across julian date and hour.
#' @details
#'
#' This function was developed by the National Park Service Natural Sounds and
#' Night Skies Division. It is intended to provide exploratory plotting for
#' summarizing and visualizing BirdNET results.
#'
#' Function returns a heatmap of BirdNET detections by julian date, where N is
#' the number of detections in a timestep for common.name above conf.threshold
#' on any given day. If multiple years of data are input to the function, an average
#' is plotted for that timestep and day. The y-axis shows time of day. Optional
#' inputs allow the user to graph sunrise and sunset-based times on the plot for
#' additional context (see help documentation for \code{\link[suncalc]{getSunlightTimes}} for details).
#' For multi-year data, all detections are summed and displayed for a given julian date.
#'
#' For best results, use \code{\link{birdnet_format}} to produce data inputs for
#' this function. Function will also attempt to plot unformatted data, but due
#' to various changes in BirdNET-Analyzer output columns over the years, be
#' aware that results may not be as intended if inputting unformatted data.
#'
#' @seealso  \code{\link{birdnet_heatmap}}
#' @import data.table ggplot2 lubridate monitoR suncalc tuneR viridis
#' @importFrom lubridate month day yday round_date
#' @importFrom suncalc getSunlightTimes
#' @export
#' @examples
#' \dontrun{
#'
#' # Read in example data
#' data(exampleHeatmapData)
#' data(exampleDatesSampled)
#'
#' # Ensure your data has an appropriate recordingID column and time columns
#' dat <- exampleHeatmapData
#' dat[ ,recordingID := basename(filepath)]
#' dat <- add_time_cols(
#'   dt = dat,
#'   tz.recorder = 'America/Los_angeles',
#'   tz.local = 'America/Los_angeles'
#' )
#'
#' # Generate a heatmap at Rivendell for Pacific-slope Flycatcher
#' # Set comparable.color.breaks = FALSE to maximize contrast in a single species map
#' birdnet_heatmap_time(
#'   data = dat,
#'   common.name = 'Pacific-slope Flycatcher',
#'   locationID = 'Rivendell',
#'   conf.threshold = 0.25,
#'   dates.sampled = exampleDatesSampled,
#'   hours.sampled = list(sunrise = c(1.5, 1.5), sunset = c(0, 0)),
#'   y.axis.limits = c(0, 23),
#'   julian.breaks = c(30, 60, 90, 120, 150, 180, 210, 240),
#'   minute.timestep = 5,
#'   comparable.color.breaks = FALSE,
#'   tz.local = 'America/Los_angeles',
#'   latitude = 46.1646,
#'   longitude = -123.77955,
#'   sun.lines = c('dusk', 'dawn', 'sunrise', 'sunset'),
#'   sun.linetypes = c('dotdash', 'longdash', 'dotted', 'solid')
#' )
#'
#' # Generate heatmaps for several species with comparable.color.breaks == TRUE
#' # so that heatmap color scale is conserved for ease of interspecies comparison
#' sp <- c("Pacific Wren",
#'         "Pacific-slope Flycatcher",
#'         "Swainson's Thrush",
#'         "Wilson's Warbler")
#'
#' for (i in 1:length(sp)) {
#'
#'   print(paste0('Working on ', sp[i]))
#'
#'   g <- birdnet_heatmap_time(
#'     data = dat,
#'     common.name = sp[i],
#'     locationID = 'Rivendell',
#'     conf.threshold = 0.1,
#'     dates.sampled = exampleDatesSampled,
#'     hours.sampled = list(sunrise = c(1.5, 1.5), sunset = c(0, 0)),
#'     y.axis.limits = c(3, 10),
#'     julian.breaks = c(30, 60, 90, 120, 150, 180, 210, 240, 270),
#'     minute.timestep = 1,
#'     plot.title = sp[i],
#'     comparable.color.breaks = TRUE,
#'     tz.local = 'America/Los_angeles',
#'     latitude = 46.1646,
#'     longitude = -123.77955,
#'     sun.lines = c('dawn', 'sunrise'),
#'     sun.linetypes = c('longdash', 'solid')
#'   )
#'
#'   print(g)
#'
#' }
#'
#' }
#'

birdnet_heatmap_time <- function(
    data,
    locationID,
    common.name,
    conf.threshold,
    julian.breaks,
    dates.sampled,
    tz.local,
    comparable.color.breaks = FALSE,
    minute.timestep,
    plot.title,
    hours.sampled,
    y.axis.limits = c(0, 23),
    sun.lines = NULL,
    sun.linetypes = NULL,
    latitude,
    longitude
) {

  valid.lty <-c('solid', 'dashed', 'dotted', 'dotdash', 'longdash', 'twodash')
  locid <- locationID

  yrs <- unique(data$year)
  yrs <- yrs[!is.na(yrs)]

  # Check and correct column type from previous birdnet column outputs
  if (any(c('common.name', 'Common name', 'Confidence', 'Start (s)') %in% colnames(data))) {
    colnames(data)[colnames(data) %in% c('common.name', 'Common name')] <- 'common_name'
    colnames(data)[colnames(data) %in% c('Confidence')] <- 'confidence'
    colnames(data)[colnames(data) %in% c('Start (s)')] <- 'start'
    message('\nIt looks like you are using unformatted data. We\'ll try to plot your input data anyway, but please consider using the function birdnet_format() to ensure best data formatting, otherwise you may encounter unexpected results in this function and R package.')
  }

  if(missing(tz.local)) {
    tz.local <- tz(data$dateTimeLocal)
    message('\nYou didn\'t put anything in the `tz.local` argument; using `tz.local = ', tz.local, '` based on your data. If this is wrong, quit function and input your desired value.')
  }

  if (length(yrs) > 1) {
    message(
      '\nMultiple years of data are detected: ',
      paste0(yrs, collapse = ', '),
      '\n',
      'Data will be averaged across all years. Restart function and check your data inputs if this is not desired.\n'
    )
  }

  if (!missing(sun.lines) & (missing(latitude) | missing(longitude))) {
    stop("Please include latitude, longitude, and tz.local if using sun.lines argument. See ?birdnet_heatmap_time for details and examples.\n")
  }

  if (!missing(sun.lines) & missing(sun.linetypes)) {
    message("\nYou used argument `sun.lines` but did not include `sun.linetypes`, so we are randomly choosing them for you. To customize these, use the `sun.linetypes` argument. See ?birdnet_heatmap_time for details and examples.")
    sun.linetypes <- sample(x = valid.lty, size = length(sun.lines), replace = TRUE)
  }

  if (!missing(sun.lines) & length(sun.lines) != length(sun.linetypes)) {
    stop("If using sun.lines argument, include a sun.linetypes argument input of equal length. See ?birdnet_heatmap_time for details and examples.\n")
  }

  if (!missing(sun.lines) & !missing(sun.linetypes)) {

    valid.suns <-  c('sunrise', 'sunriseEnd', 'goldenHourEnd',
                     'solarNoon', 'goldenHour', 'sunsetStart', 'sunset', 'dusk',
                     'nauticalDusk', 'night', 'nadir', 'nightEnd',
                     'nauticalDawn', 'dawn')

    check.suns <- sun.lines[!(sun.lines %in% valid.suns)]

    if (length(check.suns) > 0 ) {
      stop('Your entry ',
           paste(check.suns, collapse = ', '),
           ' is not a valid input to sun.lines. \nValid inputs to sun.lines include: ',
           paste(valid.suns, collapse = ', '), '.')
    }

    check.lty <- sun.linetypes[!(sun.linetypes %in% valid.lty)]

    if (length(check.lty) > 0 ) {
      stop('Your entry ',
           paste(check.lty, collapse = ', '),
           ' is not a valid input to sun.linetypes. \nValid inputs to sun.linetypes are: ',
           paste(valid.lty, collapse = ', '), '.')
    }
  }

  # Create readable julian breaks
  if(missing(julian.breaks)) {
    message('\nMissing `julian.breaks` argument, so we are making an educated guess for you. If you do not like how the x-axis looks, consider customizing the `julian.breaks` argument.')
    brks <- readable_julian_breaks(
      data = data[locationID == locid], # base off largest breadth of input data?
      posix.column = 'date',
      format = c('%d', '%b'),
      sep = '-',
      timestep = 14
    )
  } else {
    # Create a dummy data.table of dates
    rng <- range(as.Date(julian.breaks, origin = '2023-01-01'))
    dummy.brks <- data.table(date = seq.Date(from = rng[1],
                                             to = rng[2],
                                             by = 1))
    dummy.brks[,julian.date := yday(date)]
    dummy.brks[,month := lubridate::month(date, label = TRUE)][
      ,day := lubridate::day(date)]
    dummy.brks[,date.lab := paste0(day, '-', month)]

    # Set reasonable breaks
    brks <- unique(dummy.brks[julian.date %in% julian.breaks,
                              c('julian.date', 'date.lab')])
  }

  # We define the detectionTimeLocal as the date time local + the "start" column in seconds
  data[,detectionTimeLocal := dateTimeLocal + start]
  data[,date := as.Date(detectionTimeLocal, tz = tz.local)]

  # Generate timestep-unit resolution bins for detections and for recs with no detections:
  data[, timebin := round_date(x = detectionTimeLocal, unit = paste(minute.timestep, 'minute'))]

  # For recs with no detections
  data[is.na(timebin), timebin := round_date(x = dateTimeLocal, unit = paste(minute.timestep, 'minute'))]

  # Add julian date and minute-based y.unit
  data[,julian := yday(date)]
  data[, y.unit := (hour(timebin)*60 + minute(timebin))]

  # Need some robust info about which date-times were sampled so that we have "zero data"
  # (timesteps that have recordings but no detections)
  seq.by <- 60*minute.timestep
  if (!(60 %% minute.timestep == 0)) {
    stop("For minute.timestep, only numbers divisible by 60 are supported")
  }

  # If comparable.color.breaks == TRUE
  # Create heatmap color breaks based on the whole dataset, not just this species
  # so that interspecies comparisons are easier visually (i.e., use the same color breaks)
  if (comparable.color.breaks == TRUE) {
    prep <- data[,.N, by = c('common_name', 'julian', 'y.unit', 'year')]
    prep <- prep[,mean(N), by = c('common_name', 'julian', 'y.unit')]

    if(nrow(prep) == 0) {
      stop('No detections for this combination of locationID, common.name, and conf.threshold.')
    }

    color.breaks <- pretty(
      c(0,
        seq(from = min(0, na.rm = TRUE),
            to = max(prep$V1, na.rm = TRUE))))
  }

  # If comparable.color.breaks == FALSE
  # Create heatmap color breaks based only on this species, so that patterns in this species
  # are easier to see
  if (comparable.color.breaks == FALSE) {

    prep <- data[common_name == common.name & confidence >= conf.threshold & locationID == locid,
                 .N,
                 by = c('common_name', 'julian', 'y.unit', 'year')]
    prep <- prep[,mean(N), by = c('common_name', 'julian', 'y.unit')]

    if(nrow(prep) == 0) {
      stop('No detections for this combination of locationID, common.name, and conf.threshold.')
    }

    color.breaks <- pretty(
      c(0,
        seq(from = min(0, na.rm = TRUE),
            to = max(prep$V1, na.rm = TRUE))))
  }

  # If you don't have dates sampled, could use this for a time sequence.
  # But I believe it's a better practice to force the user to declare which dates were sampled
  # forces user to have a higher understanding of their dataset in advance,
  # better opportunities to discern NAs vs 0s
  tseq <- seq(
    from = min(data$timebin, na.rm = TRUE),
    to = max(data$timebin, na.rm = TRUE),
    by = seq.by
  )

  if (missing(hours.sampled)) {
    hours.sampled <- 0:23
    message('\nYou did not use the `hours.sampled` argument. As a result, this plot will imply that all hours from 0:23 were sampled. See ?birdnet_heatmap_time helpfile for details if this is not desired.')
  }

  sampled.times <- data.table(tbin = tseq)
  sampled.times[,date := as.Date(tbin, tz = tz.local)]

  if(inherits(hours.sampled, 'integer')) {
    # Subset according to hours falling within user-specified sampling range
    sampled.times[,hour := hour(tbin)]
    sampled.times <- sampled.times[hour %in% hours.sampled]
  }

  if(inherits(hours.sampled, 'list')) {
    # Compute sun-based times for these sampled tbins
    sun.times <- as.data.table(sampled.times[,getSunlightTimes(
      date = date,
      lat = latitude,
      lon = longitude,
      tz = tz.local)])

    sampled.times <- merge(
      x = sampled.times,
      y = unique(sun.times[,c('date', 'sunrise', 'sunset')]),
      by = 'date',
      all.x = TRUE
    )

    # Subset according to tbins falling within user-specified sampling range
    sampled.times[,riselowerlimit := sunrise - 3600*hours.sampled$sunrise[1]]
    sampled.times[,riseupperlimit := sunrise + 3600*hours.sampled$sunrise[2]]
    sampled.times[,setlowerlimit := sunset - 3600*hours.sampled$sunset[1]]
    sampled.times[,setupperlimit := sunset + 3600*hours.sampled$sunset[2]]
    sampled.times <-
      sampled.times[(tbin >= riselowerlimit & tbin <= riseupperlimit) |
                      (tbin >= setlowerlimit & tbin <= setupperlimit) ]

  }

  # Next, we subset the sampled times according to date as well
  sampled.times <- sampled.times[date %in% dates.sampled]

  # Isolate actual time bins
  sampled.tbins <- sampled.times$tbin

  # Subset to common name, confidence threshold, and locationID
  sp.subset <- data[common_name == common.name
                    & confidence >= conf.threshold
                    & locationID == locid]

  # Give a warning message if some dets are being skipped due to DST change/invalid recording name
  bad.dets <-  sp.subset[is.na(y.unit)]
  if ( nrow(bad.dets) > 0 ) {
    warning(nrow(bad.dets),
            ' detections from the following recordings will not be graphed: ',
            paste0(unique(bad.dets$recordingID), collapse = ', '),
            '; this is likely due to an invalid recordingID associated with daylight savings time change.\n'
    )
  }

  # Eliminate any daylight savings NA rows
  sp.subset <- sp.subset[!is.na(y.unit)]

  # Summarize detection activity by date and timebin for desired species
  # (consider summarizing this by julian instead of date?)
  sp.tbin.day <- sp.subset[,.N, by = c('common_name', 'date', 'timebin')]

  # Add in zeroes for timebins that were sampled but had no detections
  zero.det.tbins <- data.table(
    common_name = common.name,
    #   timebin = tseq[!(tseq %in% unique(sp.tbin.day$timebin))]  # if need to use tseq
    timebin = sampled.tbins[!(sampled.tbins %in% unique(sp.tbin.day$timebin))]
  )
  zero.det.tbins[, N := 0]
  zero.det.tbins[, date := as.Date(timebin, tz = tz.local)]

  # Only keep dates and tbins that were actually sampled
  zero.det.tbins <- zero.det.tbins[timebin %in% sampled.tbins]
  zero.det.tbins <- zero.det.tbins[date %in% dates.sampled]

  # Bind in zero det data if applicable
  sp.tbin.day <- rbind(
    sp.tbin.day[timebin %in% sampled.tbins & date %in% dates.sampled],
    zero.det.tbins[,c('common_name', 'date', 'timebin', 'N')]
  )

  # Add NAs for timebins that were NOT recorded (sampled)
  sp.tbin.day <- sp.tbin.day[order(common_name, timebin)]

  # Add a time col for easier graphing -- data.table::as.ITime()
  sp.tbin.day[ , plot.time := as.ITime(timebin)]

  if (!missing(sun.lines)) {

    # Add user-specified sun times. More info from ?suncalc::getSunlightTimes help doc:
    sun <- data.table(date = unique(sp.tbin.day$date))
    sun <- sun[, suncalc::getSunlightTimes(
      date = date,
      lat = latitude,
      lon = longitude,
      tz = tz.local)
    ]
    sun <- as.data.table(sun)
    sp.tbin.day[,hour := hour(timebin)]
    sp.tbin.day[,partialMinute := minute(timebin)]
    sp.tbin.day[,minute := hour*60 + partialMinute]
  }

  heat.cols <- c('gray84', viridis::magma(n = 5, begin = 0.2)) # begin 0.1 to start black instead of purple

  # Generate minute-based y.unit and y breaks
  sp.tbin.day[, y.unit := (hour(timebin)*60 + minute(timebin))]
  y.brks <- seq(from = 0, to = (1440 - 60), by = 60) #0:23*60 for miin

  if (!missing(sun.lines)) {
    sun.plot <- data.table(variable = sun.lines, sun.lty = sun.linetypes)
    sun <- melt(sun, id.vars = c('date', 'lat', 'lon'), measure.vars = sun.lines)
    sun.dt <- merge(sun, sun.plot, by = 'variable')
    sun.dt[,minute :=  hour(value)*60 + minute(value)]
    sun.dt[,julian := yday(date)]

    # MUST sort this table -- critical to get scale_linetype_manual working downstream
    setkey(sun.dt, julian, variable)

    # Check on inconsistencies in sunlines due to lat/long (e.g., northern latitudes in summer may not get full astro night)
    check.sun <- sun.dt[is.na(minute)]
    if (nrow(check.sun) > 0 ) {
      problem.dates <- sort(check.sun$date)
      problem.vars <- unique(check.sun$variable)
      warning('Missing sun timing information for problem variables: ', paste0(problem.vars, collapse = ', '),
              ' on dates like ', paste0(head(problem.dates), collapse = ', '), '... through ...',
              paste0(tail(problem.dates), collapse = ', '),
              '. It is possible that sun timing information is not available for these variables at this latitude.',
              ' This may result in unexpected plotting behavior for sun.lines. \n')
    }

    # subset sun vars to what we are plotting
    sun.dt <- unique(sun.dt[,c('variable', 'sun.lty', 'minute', 'julian')])

    # To deal with multi-year data and different exact sun times (and different daylight saving switches)
    # we keep only the first occurrence for a julian date
    sun.dt <- sun.dt[,.SD[1], by = c('variable', 'julian'), .SDcols = c('sun.lty', 'minute')]
    setkey(sun.dt, julian, variable)
  }

  # Make sure sp.tbin.day has a julian
  sp.tbin.day[,julian := yday(date)]

  # Take an average by julian date regardless of whether we are graphing 1 year or multiple
  sp.tbin.day <- sp.tbin.day[, mean(N), by = c('common_name', 'julian', 'y.unit')]
  colnames(sp.tbin.day)[colnames(sp.tbin.day) == 'V1'] <- 'N'

  # y.axis.limits argument to control y.axis
  if (length(y.axis.limits) > 2) {
    stop('y.axis.limits argument should only be length 2.')
  }
  y.labs <- c(paste0('0', 0:9, ':00'), paste0(10:23, ':00'))
  hours.plot <- data.table(y.brks, y.labs, hour = 0:23)
  hours.plot <- hours.plot[hour %in% seq(from = y.axis.limits[1], to = y.axis.limits[2], by = 1)]
  y.lims <- range(hours.plot$y.brks)

  # Limits must encompass 1 minute.timestep beyond range to plot correctly in hour 0 & 23
  y.lims[1] <- y.lims[1] - minute.timestep
  y.lims[2] <- y.lims[2] + minute.timestep

  if (missing(plot.title)) plot.title <- paste0('Total BirdNET Detections, ', locid, ': ', common.name)

  # Generate the plot
  g <- ggplot(sp.tbin.day, aes(x = julian, y = y.unit)) +
    geom_tile(#color = 'gray84', # <== add this in if you want a border around each tile
      alpha = 1,width = 1, aes(fill = N)) +
    scale_fill_gradientn(
      limits  = range(color.breaks),
      colors = heat.cols,
      breaks = color.breaks,
      na.value = 'white'
    ) +

    # Plot sun.lines if indicated by user
    {if (!missing(sun.lines))
      geom_line(data = sun.dt, aes(x = julian, y = minute, linetype = variable))
    } +

    # Customize sun.linetypes if indicated by user
    {if (!missing(sun.lines))
      scale_linetype_manual(
        name = 'Sun Times',
        values = sun.dt$sun.lty,
        labels = sun.dt$variable
      )
    } +

    scale_x_continuous(
      expand = c(0, 0),
      breaks = brks$julian.date,
      labels = brks$date.lab,
      limits = range(yday(dates.sampled), na.rm = TRUE)) +

    scale_y_continuous(
      expand = c(0, 0),
      breaks = y.brks,
      labels = y.labs,
      limits = y.lims
    ) +
    labs(title = plot.title,
         x = 'Date',
         y = 'Time') +
    theme(
      axis.line = element_line(color = 'black'),
      plot.title = element_text(hjust = 0.5, size = 12),
      legend.text = element_text(size = 12),
      legend.title = element_blank(),
      panel.background = element_blank(),
      legend.key = element_blank(),
      axis.title = element_text(size = 12),
      axis.text.x = element_text(angle = 45, hjust = 1, size = 12),
      axis.text.y = element_text(angle = 0, hjust = 1, size = 12))

  return(g)

}
