% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/birdnet_barchart.R
\name{birdnet_barchart}
\alias{birdnet_barchart}
\title{Plot stacked barcharts of BirdNET detections}
\usage{
birdnet_barchart(
  data,
  julian.breaks,
  y.limits,
  interactive = FALSE,
  focal.species,
  focal.colors
)
}
\arguments{
\item{data}{Data.table or data.frame of formatted BirdNET results. Generally,
this data object may be preceded by a call to \code{\link{add_time_cols}};
all data should come from a single site and the object must contain columns
named "locationID" (character), "recordingID" (character), and "dateTimeLocal" (POSIXct).}

\item{julian.breaks}{Optional numeric vector of julian date plotting breaks
to use on the x axis. If omitted, will be computed automatically. Example
inputs: c(140, 160, 180) would graph 3 breaks on the x axis (May 20, June 9,
and June 29 for non-leap year data); c(130:160) would graph every single date
from May 10 to June 9 on the x axis (for non-leap year data). See also
\code{\link{readable_julian_breaks}}. Please start with 1 for the first day
of the year rather than 0. \href{https://www.cdfa.ca.gov/ahfss/mpes/pdfs/Julian_Calendar.pdf}{This chart}
might be helpful for choosing julian breaks.}

\item{y.limits}{Optional numeric vector to control y-axis limits (e.g., c(0, 150)).
If not entered, will be generated automatically.}

\item{interactive}{Default = FALSE for a static plot. If true, produces a
\code{\link{plotly}} plot with interactive hover.}

\item{focal.species}{Optional character vector containing the common names of
species to highlight. Any species contained in the data object that are not
contained in \code{focal.species} will be plotted under one color as "Other". To avoid
this behavior, subset your data input to remove non-focal species.}

\item{focal.colors}{Optional character vector describing the colors that
should be used for species named in the \code{focal.species} argument. Non-focal
species ("Other") will also be plotted, if present.}
}
\value{
Stacked barchart of BirdNET results
}
\description{
Plot stacked barcharts of user-selected BirdNET results by date
}
\details{
This function was developed by the National Park Service Natural Sounds and
Night Skies Division. It is intended to provide exploratory plotting for
summarizing and visualizing BirdNET results.

Generally, \code{interactive = FALSE} should be used in conjunction with the \code{focal.species}
argument. If \code{focal.species} argument is provided, a legend will also be plotted.

Use of \code{interactive = TRUE} is meant strictly for exploratory purposes.
If \code{focal.species} argument is not used, no legend will be plotted.
A typical use case is to omit \code{focal.species} when setting \code{interactive = TRUE}.

For best results, use \code{\link{birdnet_format}} to produce data inputs for
this function. Function will also attempt to plot unformatted data, but due
to various changes in BirdNET Analyzer output columns over the years, be
aware that results may not be as intended if inputting unformatted data.
}
\examples{
\dontrun{

# Read in example data
data(exampleHeatmapData)

# Ensure your data has an appropriate recordingID column and time columns
dat <- exampleHeatmapData
dat[ ,recordingID := basename(filepath)]
dat <- add_time_cols(
 dt = dat,
 tz.recorder = 'America/Los_angeles',
 tz.local = 'America/Los_angeles'
)

# Produce an interactive plotly barchart with interactive = TRUE
birdnet_barchart(data = dat, interactive = TRUE)

# Produce a static ggplot barchat with interactive = FALSE,
# add focal.species with custom colors (any species in the data object
# that are not in focal.species will be plotted in gray as "Other".)
birdnet_barchart(
   data = dat,
   interactive = FALSE,
   focal.species = c("Pacific Wren", "Swainson's Thrush"),
   focal.colors = c('#00BE67', '#C77CFF')
)

}

}
\seealso{
\code{\link{birdnet_heatmap}}, \code{\link{birdnet_heatmap_time}}, \code{\link{birdnet_spectro}}
}
