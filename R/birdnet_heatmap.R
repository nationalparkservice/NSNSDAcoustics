# birdnet_heatmap =================================================================

#' @name birdnet_heatmap
#' @title Plot heat maps of BirdNET detections by date
#' @description Plot heat maps of BirdNET results for a selected species and above a selected confidence threshold by date for multiple or single year data. See Details.
#' @param data Data.table or data.frame of BirdNET results that a user would like to plot. Generally, this data object may be preceded by a call to \code{\link{add_time_cols}}; all data should come from a single site and the object must contain columns named "locationID" (character), "recordingID" (character), and "dateTimeLocal" (POSIXct). Multiple years of data are allowed in one dataset to enable easy comparison of vocal activity by year at a site.
#' @param locationID Character input of the locationID for which data should be plotted.
#' @param common.name Character input of the target species for which data should be plotted.
#' @param conf.threshold Numeric input of the BirdNET confidence threshold above which data should be plotted. All detections below this confidence threshold will be discarded.
#' @param dates.sampled Date or character vector of all dates sampled that should be visualized on the heat map. This information is required because your data input may only contain detection data, and not non-detection data (i.e., zeroes). For example, you might have recorded audio on 2021-03-14, but have no BirdNET detections in "data". This will result in an inaccurate visual. Since BirdNET results do not automatically contain non-detection data, it is incumbent on the user to input which dates were sampled.
#' @param julian.breaks Optional numeric vector of julian date plotting breaks to use on the x axis. If omitted, will be computed automatically. Example inputs: c(140, 160, 180) would graph 3 breaks on the x axis (May 20, June 9, and June 29 for non-leap year data); c(130:160) would graph every single date from May 10 to June 9 on the x axis (for non-leap year data). See also \code{\link{readable_julian_breaks}}. Please start with 1 for the first day of the year rather than 0.
#' @param tz.local Character Olsen names timezone for local time at the monitoring location (e.g., 'America/Los_angeles').
#' @param comparable.color.breaks Logical flag for whether to create heat map color breaks based on all species in the input data set or based only on the species of interest in this plot. TRUE means it will be easier to make straightforward comparisons between species, FALSE means activity contrasts within a single species will be easier to see.
#' @return Heat map of BirdNET detections, where N is the number of detections for common.name above conf.threshold on any given day.
#' @details
#' This function was developed by the National Park Service Natural Sounds and Night Skies Division. It is intended to provide exploratory plotting for summarizing and visualizing BirdNET results.
#'
#' Function returns a heatmap of BirdNET detections, where N is the number of detections for common.name above conf.threshold on any given day. Note that if you have different sampling efforts on different days, the heat map "as is" may not accurately visualize your target species' vocalization effort. In this case, you may wish to do some preprocessing to your data input to control for differences in sampling effort on different days.
#'
#'
#' @seealso  \code{\link{birdnet_barchart}} \code{\link{birdnet_heatmap_time}}
#' @import data.table ggplot2 lubridate monitoR tuneR viridis
#' @importFrom lubridate month day yday
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
#'            dt = dat,
#'            tz.recorder = 'America/Los_angeles',
#'            tz.local = 'America/Los_angeles'
#' )
#'
#'
#' # Generate a heatmap at Rivendell for Pacific Wren
#' # Set comparable.color.breaks = FALSE to maximize contrast in a single species map
#' # Add user-input julian.breaks
#' birdnet_heatmap(
#'      data = dat,
#'      locationID = 'Rivendell',
#'      common.name = 'Pacific Wren',
#'      conf.threshold = 0.2,
#'      dates.sampled = exampleDatesSampled,
#'      julian.breaks = seq(from = 70, to = 250, by = 30),
#'      comparable.color.breaks = FALSE
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
#'  print(paste0('Working on ', sp[i]))
#'
#'  g <- birdnet_heatmap(
#'      data = dat,
#'      locationID = 'Rivendell',
#'      common.name = sp[i],
#'      conf.threshold = 0.2,
#'      dates.sampled = exampleDatesSampled,
#'      julian.breaks = seq(from = 70, to = 250, by = 30),
#'      comparable.color.breaks = TRUE
#'  )
#'
#'  print(g)
#'
#' }
#'
#' }
#'

birdnet_heatmap <- function(
    data,
    locationID,
    common.name,
    conf.threshold,
    julian.breaks,
    dates.sampled,
    tz.local,
    comparable.color.breaks = FALSE
) {

  if(missing(dates.sampled)) {
    stop('Missing input dates.sampled. See ?birdnet_heatmap for details. Please input dates.sampled, a Date vector or character vector of all dates sampled that should be visualized on the heatmap. This information is required because your data input may only contain detection data, and not non-detection data (i.e., zeroes). For example, you might have recorded audio on 2021-03-14, but have no BirdNET detections in "data". This will result in an inaccurate visual. Since BirdNET results do not automatically contain non-detection data, it is incumbent on the user to input which dates were sampled.')
  }

  data <- as.data.table(data)

  # Make sure dat has year, date, and julian.date
  data[,year := year(dateTimeLocal)]
  data[,date := as.Date(dateTimeLocal)]
  data[,julian.date := yday(dateTimeLocal)]

  # Ensure date data type
  dates.sampled <- as.Date(dates.sampled)

  # Avoid any strange behavior with data.table by reference
  locid <- locationID

  # Create readable julian breaks
  if(missing(julian.breaks)) {
    brks <- readable_julian_breaks(
      data = data[locationID == locid], # base off largest breadth of input data?
      posix.column = 'date',
      format = c('%B', '%d'),
      sep = ' ',
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

  # If comparable.color.breaks == TRUE
  # Create heatmap color breaks based on the whole dataset, not just this species
  # so that interspecies comparisons are easier visually (i.e., use the same color breaks)
  if (comparable.color.breaks == TRUE) {
    color.breaks <- pretty(
      c(0,
        seq(from = min(0, na.rm = TRUE),
            to = max(data[,.N, by = c('locationID', 'common_name', 'date')]$N, na.rm = TRUE))))
  }

  # If comparable.color.breaks == FALSE
  # Create heatmap color breaks based only on this species, so that patterns in this species
  # are easier to see
  if (comparable.color.breaks == FALSE) {
    color.breaks <- pretty(
      c(0,
        seq(from = min(0, na.rm = TRUE),
            to = max(data[common_name == common.name & confidence >= conf.threshold & locationID == locid,
                          .N,
                          by = c('locationID', 'common_name', 'date')]$N, na.rm = TRUE))))
  }

  # Subset data according to input params
  dts <- data[common_name == common.name
              & confidence >= conf.threshold
              & locationID == locid]

  # Summarize detection counts by date
  dtn <- dts[,.N, by = c('date', 'locationID')]

  # Add in dates that were sampled but which had 0 dets
  zero.dates <- dates.sampled[!(dates.sampled %in% dtn$date)]
  if (length(zero.dates) != 0) {
    dets.0 <- data.table(date = dates.sampled[!(dates.sampled %in% dtn$date)],
                         locationID = locid,
                         N = 0)
    dtn <- rbind(dtn, dets.0)
  }
  dtn <- dtn[order(date)]
  dtn[,year :=  year(date)]
  dtn[,julian.date := yday(date)]

  # Set colors
  heat.cols <- c('gray75', viridis::magma(n = 5, begin = 0.2))
  u.lim <- max(color.breaks)

  dtn <- dtn[!is.na(year)]

  # Plot
  g <- ggplot(dtn,
              aes(julian.date, as.factor(year),
                  fill = N)) +
    geom_tile(#color = 'gray50',   # add lines around each tile
      alpha = 1, width = 1) +
    scale_fill_gradientn(
      limits  = range(color.breaks),
      colors = heat.cols,
      breaks = color.breaks
    ) +
    scale_x_continuous(expand = c(0, 0),
                       breaks = brks$julian.date,
                       labels = brks$date.lab) +
    scale_y_discrete(expand = c(0, 0)) +
    labs(title = paste0(unique(dtn$locationID), ' - ', common.name),
         x = 'Date',
         y = 'Year') +
    theme(
      axis.ticks.y = element_blank(),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      axis.line = element_line(color = 'black'),
      plot.title = element_text(hjust = 0.5, size = 12),
      legend.text = element_text(size = 12),
      strip.background = element_blank(),
      strip.text.x = element_blank(),
      panel.background = element_blank(),
      legend.key = element_blank(),
      axis.title.x = element_text(size = 12),
      axis.text.x = element_text(angle = 45, hjust = 1, size = 12))

  return(g)
}
