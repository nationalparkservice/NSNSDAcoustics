# birdnet_barchart =================================================================

#' @name birdnet_barchart
#' @title Plot stacked barcharts of BirdNET detections
#' @description Plot stacked barcharts of user-selected BirdNET results by date
#' @param data Data.table or data.frame of formatted BirdNET results that a user would like to plot. Generally, this data object may be preceded by a call to \code{\link{add_time_cols}}; all data should come from a single site and the object must contain columns named "locationID" (character), "recordingID" (character), and "dateTimeLocal" (POSIXct).
#' @param julian.breaks Optional numeric vector of julian date plotting breaks to use on the x axis. If omitted, will be computed automatically. Example inputs: c(140, 160, 180) would graph 3 breaks on the x axis (May 20, June 9, and June 29 for non-leap year data); c(130:160) would graph every single date from May 10 to June 9 on the x axis (for non-leap year data). See also \code{\link{readable_julian_breaks}}. Please start with 1 for the first day of the year rather than 0.
#' @param y.limits Optional numeric vector to control y-axis limits (e.g., c(0, 150)). If not entered, will be generated automatically.
#' @param interactive Default = FALSE for a static plot. If true, produces a plotly plot with interactive hover.
#' @param focal.species Optional character vector containing the common names of species to highlight. Any species contained in the data object that are not contained in focal.species will be plotted under one color as "Other".
#' @param focal.colors Optional character vector describing the colors that should be used for species named in the focal.species argument. Non-focal species ("Other") will be plotted in black.
#' @return Stacked barchart of BirdNET results
#' @details
#' This function was developed by the National Park Service Natural Sounds and Night Skies Division. It is intended to provide exploratory plotting for summarizing and visualizing BirdNET results.
#'
#' Generally, interactive = FALSE should be used in conjunction with the focal.species argument. If focal.species argument is provided, a legend will also be plotted.
#'
#' Use of interactive = TRUE is meant strictly for exploratory purposes. If focal.species argument is not used, no legend will be plotted. A typical use case is to omit focal.species when setting interactive = TRUE.
#'
#'
#' @seealso  \code{\link{birdnet_analyzer}}, \code{\link{birdnet_format}}, \code{\link{birdnet_verify}}
#' @import data.table ggplot2 lubridate monitoR tuneR
#' @importFrom plotly ggplotly style
#' @importFrom lubridate month day yday
#' @export
#' @examples
#' \dontrun{
#'
#' # Read in exampleBarchartData
#' data(exampleBarchartData)
#'
#' # Generally, add_time_cols() may be called as part of preprocessing
#' # (if not, please ensure data object has columns that include locationID (character),
#' # recordingID (character), and dateTimeLocal (POSIXct))
#' dat <- add_time_cols(dt = exampleBarchartData,
#'                      tz.recorder = 'America/Los_angeles',
#'                      tz.local = 'America/Los_angeles')
#'
#' # Produce an interactive plotly barchart with interactive = TRUE
#' birdnet_barchart(data = dat, interactive = TRUE)
#'
#' # Produce a static ggplot barchat with interactive = FALSE,
#' # add focal.species with custom colors (any species in the data object
#' # that are not in focal.species will be plotted in black as "Other".)
#' birdnet_barchart(data = dat,
#'                  interactive = FALSE,
#'                  focal.species = c("Pacific Wren", "Swainson's Thrush", "Varied Thrush"),
#'                  focal.colors = c('#00BE67', '#C77CFF', '#c51b8a'))
#'
#' }
#'

birdnet_barchart <- function(data, julian.breaks, y.limits,
                             interactive = FALSE,
                             focal.species, focal.colors)
{
  # For some reason, data.table is having "side-effects" on the data object
  # So saving a backup object here that will be operated on
  dt <- copy(data)

  # Check for only one locationID
  if (length(unique(dt$locationID)) != 1) {
    stop(paste0('The data object contains more than one locationID: ', paste0(unique(dt$locationID), collapse = ', '), '. Please input a data object that contains only one locationID.'))
  }

  # Check for plotly
  if (interactive == TRUE) {
    if (!requireNamespace("plotly", quietly = TRUE)) {
      stop( "Package \"plotly\" must be installed to use interactive = TRUE.",
            call. = FALSE)
    }
  }

  if(!missing(focal.species) & missing(focal.colors)) {
    stop('Please input focal.colors if you are using focal.species argument.')
  }

  # Check and correct column type (common.name from csv data, common_name from r/txt)
  if ('common.name' %in% colnames(dt)) {
    colnames(dt)[colnames(dt) == 'common.name'] <- 'common_name'
  }

  # Add time columns
  dt[,year := year(dateTimeLocal)]
  dt[,date := as.Date(dateTimeLocal)]
  dt[,julian.date := yday(date)]

  # Remove NA data from dt (occurs if recording has no detections)
  # Do now rather than later, to avoid issues with "Other" if using focals
  dt <- dt[!is.na(common_name)]

  # If focal species are indicated, correct for nonfocal species
  if (!missing(focal.species)) {
    # If other species aside from focals occur in the dataset, add "Other" category
    if (nrow(dt[!(common_name %in% focal.species)] ) > 0) {
      dt[!(common_name %in% focal.species), common_name := 'Other']
      focal.colors <- c(focal.colors, 'black') # Add black to species.colors for Other
    }
  }

  # Rearrange dt and add dates
  stacksp <- dt[, .N, by = c('common_name', 'year', 'date')]
  colnames(stacksp)[1] <- 'common_name' # ensure we have this version

  if (!missing(focal.species)) {

    # If including "Other" nonfocals in the plot, set levels to include "Other"
    if (nrow(dt[!(common_name %in% focal.species)] ) > 0) {
      levs <- labs <- c(focal.species, 'Other')
    } else {
      levs <- labs <- focal.species
    }

    # Reorder as factor to get legend to reflect species/color order
    stacksp[, common_name := factor(common_name, levels = levs, labels = labs)]
  }

  # Prep human-readable date labels for julian date
  stacksp[,julian.date := yday(date)]
  stacksp[,month := lubridate::month(date, label = TRUE)][
    ,day := lubridate::day(date)]
  stacksp[,date.lab := paste0(day, '-', month)]

  # Generate julian.breaks and y.limits if missing
  if (missing(julian.breaks)) {
    julian.range <- range(stacksp$julian.date)
    julian.breaks <- seq(from = floor(julian.range[1]/10)*10,
                         to = ceiling(julian.range[2]/10)*10, by = 20)
  }

  if (missing(y.limits)) {
    y.limits <- c(0, max(stacksp[,sum(N), by = c('year', 'julian.date')]$V1))
  }

  # Set reasonable breaks
  brks <- unique(stacksp[julian.date %in% julian.breaks,
                         c('julian.date', 'date.lab')])

  # If any julian.breaks specified by user are not present in brks
  # due to zero detections for that day, add them back in
  missing.jul <- julian.breaks[!(julian.breaks %in% brks$julian.date)]
  if (length(missing.jul) != 0) {
    missing.dates <- data.table(julian.date = missing.jul)
    missing.dates[,date := as.Date(julian.date,
                                   origin = floor_date(stacksp$date[1],
                                                       unit = 'years'))]
    missing.dates[,month := lubridate::month(date, label = TRUE)][
      ,day := lubridate::day(date)]
    missing.dates[,date.lab := paste(day, month, sep = '-')]
    brks <- rbind(brks, missing.dates[,c('julian.date', 'date.lab')])
  }

  # Create a dummy x axis to accommodate the circularity of julian dates
  # (allows for reasonable/pretty x-axis that only looks at winter dates
  # spanning from e.g. julian date 330:365 and 1:20)
  brks.start.x <- data.table(
    julian.date = c(julian.breaks[1]:365, 1:(julian.breaks[1] - 1)),
    x.scale = 1:365
  )

  # Join brks to stacksp
  stacksp <- merge(stacksp, brks.start.x, by = 'julian.date', all.x = TRUE)
  brks <- merge(brks, brks.start.x, by = 'julian.date', all.x = TRUE)
  setkey(brks, x.scale)

  # In case of leap years, keep only the first instance of a julian date
  brks <- brks[match(unique(brks$julian.date), brks$julian.date),]

  # Generate plots that don't use focal species arg
  if (missing(focal.species)) {
    alldets <- ggplot(stacksp, aes(x = x.scale, y = N,
                                   fill = common_name)) +
      # ^^ add color = common_name to aes() to eliminate artefact white space between some bars
      # (although I like these in interactive mode with lots of species)
      geom_bar(position = "stack", stat = "identity") +
      facet_wrap(~year) +
      xlab('Date') +
      ylab('N') +
      scale_x_continuous(expand = c(0, 0),
                         breaks = brks$x.scale,
                         labels = brks$date.lab,
                         limits = range(brks$x.scale)) +
      scale_y_continuous(expand = c(0, 0), limits = y.limits) +
      # scale_color_manual(guide = 'none') + # eliminate additional legend
      ggtitle(paste0('Count by Date - ', unique(dt$locationID))) +
      theme_classic() +
      theme(axis.ticks.y = element_blank(),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            axis.line = element_line(color = 'black'),
            plot.title = element_text(hjust = 0.5, size = 12),
            legend.position = 'none',
            legend.title = element_blank(),
            strip.background = element_blank(),
            axis.title.x = element_text(size = 12),
            axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))
    if (interactive == TRUE) {

      return(

          # Not displaying correctly =/
          # style(
          #   ggplotly(alldets),
          #   hoverinfo = 'text',
          #   hovertext = paste('Date:', stacksp$date.lab, '<br>',
          #                     'Species:', stacksp$common_name, '<br>',
          #                     'N:', stacksp$N, '<br>')
          # )

          style(ggplotly(alldets, tooltip = c('fill', 'y', 'text')), hoverinfo = 'text')
        )

    } else {
      return(alldets)
    }
  } # end no focal species


  # If using focal species
  if(!missing(focal.species)) {

    focs <- ggplot(stacksp, aes(x = x.scale, y = N,
                                fill = common_name, color = common_name)) +
      # ^^ add color = common.name to aes() to eliminate artefact white space between some bars
      geom_bar(position = "stack", stat = "identity") +
      facet_wrap(~year) +
      xlab('Date') +
      ylab('N') +
      scale_color_manual(values = focal.colors, guide = 'none') + # eliminate extra legend
      scale_fill_manual(values = focal.colors) +
      scale_x_continuous(expand = c(0, 0),
                         breaks = brks$x.scale,
                         labels = brks$date.lab,
                         limits = range(brks$x.scale)) +
      scale_y_continuous(expand = c(0, 0), limits = y.limits) +
      ggtitle(paste0('Count by Date - ', unique(dt$locationID))) +
      labs(fill = 'Species') +
      theme_classic() +
      theme(axis.ticks.y = element_blank(),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            axis.line = element_line(color = 'black'),
            plot.title = element_text(hjust = 0.5, size = 12),
            legend.position = 'bottom',
            # ^^ note: legend.position will not be respected by ggplotly style:
            #    https://github.com/plotly/plotly.R/issues/1049
            legend.text = element_text(size = 12),
            strip.background = element_blank(),
            axis.title.x = element_text(size = 12),
            axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

    if (interactive == TRUE) {

      return(

        # Not displaying correctly =/
        # style(
        #   ggplotly(focs),
        #   hoverinfo = 'text',
        #   hovertext = paste('Date:', stacksp$date.lab, '<br>',
        #                     'Species:', stacksp$common_name, '<br>',
        #                     'N:', stacksp$N, '<br>')
        # )

        style(ggplotly(focs, tooltip = c('color', 'y', 'text')), hoverinfo = 'text')
      )

    } else {
      return(focs)
    }
  } # end if using focal species
}
