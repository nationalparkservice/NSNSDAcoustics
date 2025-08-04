# birdnet_conf_threshold ===============================================================

#' @name birdnet_conf_threshold
#' @title Generate options for BirdNET confidence thresholds
#' @description Generate options for BirdNET confidence thresholds based on
#' (1) logistic regression as in \href{https://link.springer.com/article/10.1007/s10336-024-02144-5}{Wood and Kahl (2024)}
#' and/or (2) common machine learning performance evaluation metrics (i.e., choose
#'  the lowest BirdNET confidence threshold that maximizes the precision score or F1
#'  score, or the highest confidence threshold that maximizes the recall score)
#'  as in \href{https://irma.nps.gov/DataStore/Reference/Profile/2309071}{Balantic (2025)}.
#' @param data Data.table or data.frame of detection data with verifications.
#' Columns must include `common_name` (character), `confidence` (numeric) and a
#'  `verify` column that indicates 1 for true positive and 0 for false positive.
#'  The data type of the `verify` column does not matter as long as it is
#'  composed of 1 and 0.
#' @param probabilities Vector of probability cutoffs to investigate for logistic
#' regression. Default = `c(0.85, 0.90, 0.95)`.
#' @param min.conf Input the minimum BirdNET confidence value (`min_conf`) that you
#' originally used when processing your data through BirdNET Analyzer. If left blank, the
#' function attempts to find a `min_conf` column from your BirdNET results in `data`.
#' @param plot Logical for whether to produce and display plot(s) in R.
#' Default = `TRUE`. Note: if you have many species, see `plot.folder` argument
#' for an option to save plots to a folder instead of viewing them in R.
#' @param plot.folder (Optional) If used, will save plots to a folder of your
#' choice for more organized viewing. Typically a better option than the `plot`
#' argument if you have many species.
#' @return List with two data.tables/data.frames:
#'
#' \strong{logistic_regression_results}: Table of confidence score thresholds to
#'  use for different probability cutoffs.
#'
#' \itemize{
#' \item{\strong{common_name}: Species common name.}
#' \item{\strong{pr_true}: Probability that a detection is a true positive.}
#' \item{\strong{confidence}: BirdNET confidence value to achieve the associated
#'  probability of true positive.}
#' \item{\strong{note}: Notes whether the confidence value for this row is below
#'  the minimum confidence value in your data.}
#' }
#'
#' \strong{machine_learning_performance_results}: Table of confidence score
#' thresholds to use based on maximizing for F1, precision, or recall.
#'
#' \itemize{
#' \item{\strong{common_name}: Species common name.}
#' \item{\strong{maximization_metric}: Machine learning performance evaluation
#' metric.}
#' \item{\strong{confidence}: BirdNET confidence value needed to maximize this
#' performance evaluation metric.}
#' \item{\strong{metric_score}: Machine learning performance evaluation score
#' for this metric (ranges from 0-1).}
#' }
#'
#' Optionally also returns a set of three plots for each species:
#'
#' \itemize{
#' \item{\strong{1:} Plot showing logistic regression BirdNET score thresholds
#' at the cutoffs given by the user in the `probabilities` argument.}
#' \item{\strong{2:} Plot illustrating machine learning performance metric scores
#'  at different BirdNET confidence thresholds.}
#' \item{\strong{3:} Histograms showing BirdNET detection confidence score
#' distributions for verified true positives (1) and false positives (0).}
#' }
#'
#' @details
#'
#' This function was developed by the National Park Service Natural Sounds and
#' Night Skies Division to assist with choosing BirdNET confidence thresholds
#' either based on logistic regression as in \href{https://link.springer.com/article/10.1007/s10336-024-02144-5}{Wood and Kahl (2024)}
#' or based on common machine learning performance evaluation metrics (F1, precision, recall) as in \href{https://irma.nps.gov/DataStore/Reference/Profile/2309071}{Balantic (2025)}.
#' Note that there may be multiple
#' BirdNET confidence thresholds that maximize the F1, precision, or recall score. This
#' function returns the lowest possible BirdNET confidence threshold you can choose
#' that will still maximize the precision score or F1 score. It also returns the
#' highest possible BirdNET confidence threshold you can use while still maximizing
#' recall.
#'
#' @seealso \code{\link{birdnet_verify}}, \code{\link{birdnet_format}}
#' @import data.table ggplot2
#' @importFrom caret confusionMatrix
#' @importFrom viridis magma viridis
#' @importFrom ggplot2 ggplot
#' @export
#' @examples
#' \dontrun{
#'
#' # Your input data.table/data.frame might have a variety of formats,
#' # but at minimum, needs columns 'common_name', 'confidence', and a 'verify'
#' # column with 1 for true positives and 0 for false positives.
#' # Depending on the verification scheme you used, you may need to do
#' # some minor reformatting first, as we illustrate in the example:
#'
#' data(exampleVerified)
#' vers <- exampleVerified
#'
#' # Modify the verification column to become 0s and 1s
#' vers[verify == 'n', verify := 0]
#' vers[verify == 'y', verify := 1]
#'
#' # Generate confidence thresholds without generating plots
#' birdnet_conf_threshold(
#'    data = vers,
#'    probabilities = c(0.85, 0.90, 0.95, 0.99),
#'    plot = FALSE
#' )
#'
#' # Generate confidence thresholds and plots
#' birdnet_conf_threshold(
#'    data = vers,
#'    probabilities = c(0.85, 0.90, 0.95, 0.99),
#'    plot = TRUE
#' )
#'
#' # Generate confidence thresholds; save plots to folder instead of plotting in R
#' dir.create('example-conf-threshold-plots')
#' birdnet_conf_threshold(
#'    data = vers,
#'    probabilities = c(0.75, 0.85, 0.90, 0.95, 0.99),
#'    plot = FALSE,
#'    plot.folder = 'example-conf-threshold-plots'
#' )
#'
#' # Delete all temporary example files when finished
#' unlink(x = 'example-conf-threshold-plots', recursive = TRUE)
#'
#' }

birdnet_conf_threshold <- function(
    data,
    probabilities = c(0.85, 0.90, 0.95),
    min.conf,
    plot = TRUE,
    plot.folder
) {

  data <- data.table(data)
  sp <- sort(unique(data$common_name))
  ht <- 5.5
  wt <- 6.5
  check.cols <- colnames(data)
  data[,verify := as.character(verify)] # coerce to character for modeling
  check.verify <- sort(unique(data$verify))
  req.cols <- c('common_name', 'confidence', 'verify')
  check.cols <- req.cols %in% colnames(data)

  if (sum(check.cols) != 3) {
    stop('Your input to the `data` argument is missing required column name(s): ',
         req.cols[!(req.cols %in% colnames(data))],
         '\nSee `?birdnet_conf_threshold` for details.')
  }

  if(setequal(check.verify, c("0", "1")) == FALSE) {
    stop('Please ensure `verify` column only contains 0 or 1. Your `verify` column has the following invalid values: ',
         paste0(check.verify[!(check.verify %in% c('0', '1'))], collapse = ', '),
         '\nSee `?birdnet_conf_threshold` for details.')
  }

  message('Found ', length(sp), ' species in this dataset. Computing confidence thresholds and machine learning performance metrics... \n')

  if(!missing(plot.folder)) {
    message('Generating plots in your folder "', plot.folder, '"\n')
  }

  sp.results.lr <- sp.results.ml <- warnings.for <- list()

  # Establish minimum confidence value
  if(missing(min.conf)) {
    min.conf <- unique(data$min_conf)
    if (is.null(min.conf)) {
      stop('You did not input a minimum BirdNET confidence value in the `min.conf` argument, and we can\'t find one in your `data` input columns. Please input desired value to `min.conf`. See `?birdnet_conf_threshold` for details.\n')
    } else {
      message('Using `min.conf` = ', min.conf, ' based on your data input. If this behavior is undesired, please exit the function and input desired value to `min.conf`.\n')
    }
  }

  for (i in 1:length(sp)) {

    vers <- data[common_name == sp[i]]
    this.sp <- unique(vers$common_name)
    message('Working on ', this.sp, '.')

    if (plot == FALSE & !missing(plot.folder)) {
      plot <- TRUE
      no.folder <- FALSE
    }

    if (missing(plot.folder)) {no.folder <- TRUE}

    if (plot == TRUE) {

      hist.plot <- ggplot2::ggplot(data = vers, aes(x = confidence)) +
        geom_histogram(bins = 30) +
        facet_grid(~verify) +
        ggtitle(paste0('Score Distribution for ', this.sp)) +
        scale_x_continuous(expand = c(0,0)) +
        scale_y_continuous(expand = c(0,0)) +
        xlab('BirdNET Confidence Value') +
        ylab('Count') +
        theme_bw() +
        theme(panel.grid = element_blank())

      if (no.folder == TRUE) { print(hist.plot) }

      if (!missing(plot.folder)) {
        ggsave(filename = paste0(plot.folder,'/', sp[i], '-scores-histogram.png'),
               plot = hist.plot,
               units = 'in',
               height = ht,
               width = wt)
      }
    }

    # Generate regression curve to see the probability of a true positive
    # for different birdnet confidence thresholds
    # Compute logit of confidence -- make sure it is not bound between 0 & 1
    vers[,logit.confs := log(confidence/(1 - confidence))]

    # 0/1 verify variable becomes a factor data type for the model
    vers[,verify := factor(verify)]

    # Run logistic regression, catch any warnings and print to console for user
    x <- tryCatch({
      mylogit <- glm(verify ~ logit.confs, data = vers, family = 'binomial')
    }, warning = function(w) {
      message('     ==> ', this.sp, ' logistic regression has a warning: ', conditionMessage(w), '\n')
      NA
    })
    suppressWarnings(mylogit <- glm(verify ~ logit.confs, data = vers, family = 'binomial'))

    # Prepare data for visualization
    vers[,graph.var := as.numeric(verify)]
    vers[graph.var == 1, graph.var := 0]
    vers[graph.var == 2, graph.var := 1]

    logit.confs <- confs <- c()
    for (j in 1:length(probabilities)) {
      logit.confs[j] <- (log(probabilities[j]/(1 - probabilities[j])) - mylogit$coefficients[1])/mylogit$coefficients[2]
      confs[j] <- exp(logit.confs[j])/(1 + exp(logit.confs[j]))
    }

    pr <- data.table(common_name = this.sp,
                     logit.conf = logit.confs,
                     pr.true = probabilities,
                     confidence = confs)

    # Notify user if regression confidence thresholds fall below min.conf
    check.mc <- pr[confidence <= min.conf]
    if(nrow(check.mc > 0)) {
      message('     ==> The following logistic regression-based confidence thresholds fall below the `min.conf` value. These lines will still be graphed on the regression plot, but note that your sample data does not contain values below `min.conf` = ', min.conf, ':')
      message(paste0(capture.output(check.mc[,c('common_name', 'pr.true', 'confidence')]), collapse = '\n        '))
      message('\n')
    }

    # Add labels and colors to make graph interpretable
    pr[,cols := viridis::viridis(n = nrow(pr))]

    pr[,`Pr(True) [BirdNET Confidence]:` :=
         paste0('Pr(',
                # use format() to show 2 digits even if ends in 0 like 0.9
                format(round(pr.true, digits = 2), nsmall = 2),
                ') [', format(round(confidence, digits = 2), nsmall = 2), ']')]

    if (plot == TRUE) {

      # Plot probability of true positive at different values
      logreg.plot <- ggplot2::ggplot(vers, aes(logit.confs, mylogit$fitted.values)) +
        geom_line(color = 'gray44', linewidth = 1.5) +
        geom_point(data = vers,
                   mapping = aes(logit.confs,
                                 as.numeric(graph.var)),
                   size = 3,
                   alpha = 0.15) +
        scale_x_continuous(expand = c(0,0)) +
        geom_vline(data = pr, aes(xintercept = logit.confs,
                                  color = `Pr(True) [BirdNET Confidence]:` ,
                                  linetype = `Pr(True) [BirdNET Confidence]:` ),
                   linewidth = 1.25) +
        scale_color_manual(values = rev(viridis::viridis(nrow(pr)))) +

        xlab('Logit(BirdNET Confidence)') +
        ylab('Pr(True Positive)') +
        ggtitle(paste0('Logistic Regression Score Thresholds for ', this.sp)) +
        theme_bw() +
        theme(panel.grid = element_blank(),
              legend.position = 'inside',
              legend.position.inside = c(0.87217, 0.16),

              legend.title = element_text(size = 7),
              legend.background = element_rect(
                fill = 'white', #gray88
                linewidth = 0.25,
                linetype = 'solid',
                colour = 'black'
              )
        )

      if (no.folder == TRUE) { print(logreg.plot) }

      if (!missing(plot.folder)) {
        ggsave(filename = paste0(plot.folder,'/', sp[i], '-logistic-regression.png'),
               plot = logreg.plot,
               units = 'in',
               height = ht,
               width = wt)
      }
    }

    # Look at how the model performs if we set birdnet confidence threshold from min.conf to 1.
    confs <- seq(from = min.conf, to = 1, by = 0.001)
    ml.results <- data.table(
      common_name = this.sp,
      confidence = confs,
      Accuracy = 0,
      F1 = 0,
      Precision = 0,
      Recall = 0
    )

    for (k in 1:length(confs)) {

      threshold.prep <- vers[common_name == this.sp, c('common_name', 'confidence', 'verify')]
      threshold.prep[verify == 0, verify := 'n'] # positive arg to caret::confusionMatrix must be char
      threshold.prep[verify == 1, verify := 'y']

      # For anything that falls below this confidence value, we "miss" it
      # so it becomes a false negative, mark as 'n'
      threshold.prep[confidence <= confs[k], threshold := 'n']

      # If it exceeds this threshold, it becomes a y
      threshold.prep[confidence > confs[k], threshold := 'y']

      cm <- caret::confusionMatrix(
        # These are the results when a confidence threshold is applied
        data = factor(threshold.prep$threshold,levels = c('y', 'n')),

        # These are the true data
        reference = factor(threshold.prep$verify, levels = c('y', 'n')),
        positive = 'y'
      )
      ml.results[k, F1 := cm$byClass[['F1']]]
      ml.results[k, Precision := cm$byClass[['Precision']]]
      ml.results[k, Recall := cm$byClass[['Recall']]]
    }

    thresh.long <- melt(
      data = ml.results,
      id.vars = c('common_name', 'confidence'),
      measure.vars = c('F1', 'Precision', 'Recall')
    )

    # Identify confidence thresholds that maximize f1, precision, recall
    # (There may be multiple confidence thresholds that meet this criteria)
    max.f1.full <- ml.results[F1 == max(F1, na.rm = TRUE)]
    max.prec.full <- ml.results[Precision == max(Precision, na.rm = TRUE)]
    max.recall.full <- ml.results[Recall == max(Recall, na.rm = TRUE)]

    sp.res.ml <- data.table(
      common_name = this.sp,
      maximization_metric = c('F1', 'Precision', 'Recall'),

      # If multiple values maximize it, we choose the lowest
      # birdnet confidence value that maximizes the F1 and the precision,
      # and the highest birdnet confidence value that will still maximize recall...
      confidence = c(min(max.f1.full$confidence), # choose LOWEST conf
                     min(max.prec.full$confidence), # choose LOWEST conf
                     max(max.recall.full$confidence)), # choose HIGHEST conf
      metric_score = c(unique(max.f1.full$F1),
                       unique(max.prec.full$Precision),
                       unique(max.recall.full$Recall))
    )

    colnames(thresh.long)[colnames(thresh.long) == 'variable'] <- 'maximization_metric'
    colnames(thresh.long)[which(colnames(thresh.long) == 'value')] <- 'metric_score'

    if (plot == TRUE) {

      # Drop NA values from plotting
      thresh.long <- thresh.long[!is.na(metric_score)]

      # Visualize how F1, precision, and recall change based on
      # different birdnet confidence thresholds
      cols <- rev(viridis::magma(n = nrow(ml.results[common_name == this.sp])))

      # Merge for annotation labels
      sp.res.ml[,plot_anno := 'Maximized Score']
      thresh.long <- merge(thresh.long,
                           sp.res.ml,
                           by = c('common_name', 'maximization_metric', 'confidence', 'metric_score'),
                           all.x = TRUE)
      thresh.long[is.na(plot_anno), plot_anno := 'Other Scores']
      thresh.long[,plot_anno := factor(plot_anno,
                                       levels = c('Maximized Score', 'Other Scores'))]

      ml.plot <- ggplot2::ggplot(data = thresh.long,
                                 aes(maximization_metric,
                                     metric_score,
                                     color = confidence,
                                     size = plot_anno)) +
        geom_point(data = thresh.long) +
        annotate("text",
                 x = 2,
                 y = 1.15,
                 label = 'BirdNET confidence value that maximizes this metric score: ') +
        geom_text(
          data = thresh.long[plot_anno == 'Maximized Score'],
          aes(x = maximization_metric,
              y = metric_score,
              label = c(sp.res.ml[maximization_metric == 'F1', confidence],
                        sp.res.ml[maximization_metric == 'Precision', confidence],
                        sp.res.ml[maximization_metric == 'Recall', confidence])),
          hjust = -0.2,
          vjust = 0,
          size = 4,
          color = 'black'
        ) +
        scale_color_gradientn('BirdNET Confidence Threshold', colours = cols) +
        ylab('Metric Score') +
        ggtitle(paste0('Machine Learning Performance Metrics for ', this.sp)) +
        theme_bw() +
        theme(legend.title = element_text(size = 10),
              axis.title.x = element_blank(),
              axis.ticks = element_blank(),
              panel.background = element_blank(),
              panel.grid.major = element_blank(),
              panel.grid = element_blank(),
              legend.position = 'bottom') +
        scale_y_continuous(breaks = c(0, 0.25, 0.5, 0.75, 1)) +
        scale_size_manual(values = c('Maximized Score' = 3, 'Other Scores' = 1)) +
        guides(size = 'none')

      if (no.folder == TRUE) { print(ml.plot) }

      if (!missing(plot.folder)) {
        ggsave(filename = paste0(plot.folder,'/', sp[i], '-ml-evaluation-metrics.png'),
               plot = ml.plot,
               units = 'in',
               height = ht,
               width = wt)
      }
    }

    # Find confidence threshold that maximizes F1, precision, recall
    # (There may be many confidence thresholds that meet these criteria)
    max.f1.full <- ml.results[F1 == max(F1, na.rm = TRUE)]
    max.prec.full <- ml.results[Precision == max(Precision, na.rm = TRUE)]
    max.recall.full <- ml.results[Recall == max(Recall, na.rm = TRUE)]

    sp.res.ml <- data.table(common_name = this.sp,
                            maximization_metric = c('F1', 'Precision', 'Recall'),

                            # If multiple values maximize it, we choose the lowest
                            # birdnet confidence value that maximizes prec of F1,
                            # and the highest value that maximizes the recall
                            confidence = c(min(max.f1.full$confidence),
                                           min(max.prec.full$confidence),
                                           max(max.recall.full$confidence)),
                            metric_score = c(unique(max.f1.full$F1),
                                             unique(max.prec.full$Precision),
                                             unique(max.recall.full$Recall))
    )

    sp.res.lr <- pr[,c('common_name', 'pr.true', 'confidence')]
    sp.res.lr[,common_name := this.sp]
    colnames(sp.res.lr)[2] <- 'pr_true'
    sp.results.lr[[i]] <- sp.res.lr
    sp.results.ml[[i]] <- sp.res.ml
  }

  lr.res <- rbindlist(sp.results.lr)
  lr.res[confidence < min.conf, note := 'Confidence value is below min.conf in your data']
  results <- list(logistic_regression_results = lr.res,
                  machine_learning_metric_maximization_results = rbindlist(sp.results.ml))
  message('\nDone!')
  return(results)
}
