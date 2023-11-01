
# birdnet_iNEXT ================================================================

#' @name birdnet_iNEXT
#' @title UNDER DEVELOPMENT
#' @description TBD
#' @param data Data.frame / data.table with....
#' @param confidence.threshold Threshold below which BirdNET results should be excluded.
#' @param n.detections Number of detections of a species within a day to conclude presence.
#' @return DESCRIBE...  additional columns:
#' \itemize{
#' \item{\strong{insert col name here}: tbd }
#' \item{\strong{insert col name here}: tbd }
#' \item{\strong{insert col name here}: tbd }
#' }
#'
#' @details
#'
#' This function was developed by the National Park Service Natural Sounds and Night Skies Division to support species biodiversity characterization in bioacoustics projects. This function is under development.
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
#' # Choose "sized-based" vs "coverage-based"
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
#' # Richness values are essentially the same;
#' # it's the confidence values that are different
#' # size.based conf ranges will be much tighter
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
#' # Graph Results
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
  #   (IS THIS TRUE? OR IS IT JUST THE TOTAL NUMBER OF DATES WE DETECTED SOMETHING?
  #   WHAT IF BIRDNET DETECTED NOTHING THAT DAY? Does this workflow account for the possibility of 0 detection data?
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



