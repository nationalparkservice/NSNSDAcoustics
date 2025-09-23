# birdnet_review_verify ===============================================================

#' @name birdnet_review_verify
#' @title Save verifications from BirdNET GUI segment review to formatted CSVs
#' @description If a user has used the BirdNET Analyzer GUI Segments
#' tab to generate segments, and has used the GUI Review tab to provide verifications,
#' this function allows reviewed segment data to be saved with the labels
#' 'Positive' and/or 'Negative' in the 'verify' column of formatted CSVs created
#' from \code{\link{birdnet_format}}. \strong{Use with caution: if verifications already exist in the 'verify'
#' column of the CSV, note that they may be overwritten by this function, or may
#' produce a mix of inconsistent labels.}
#' @param segments.directory Segments directory path
#' @param results.directory Path to CSV result directory
#' @return Updates the 'verify' column of CSVs generated from \code{\link{birdnet_format}}
#' with 'Positive' or 'Negative' for segments verified by a user via the Review tab of BirdNET
#' Analyzer GUI V2.
#' @details
#'
#' This function was developed by the National Park Service Natural Sounds and
#' Night Skies Division to process audio data using BirdNET.
#'
#' Function has only been tested for BirdNET Analyzer GUI version 2+.
#' Unexpected behavior may result if attempting to combine BirdNET Analyzer V2 GUI
#' segment outputs with BirdNET Analyzer V1 GUI segment outputs.
#'
#' @seealso  \code{\link{birdnet_format}}, \code{\link{birdnet_verify}}
#' @import data.table
#' @importFrom stringr str_extract str_match
#' @export
#' @examples
#' \dontrun{
#'
#' # Create a BirdNET results directory for this example
#' # Write examples of formatted BirdNET outputs to example results directory
#' dir.create('example-results-directory')
#' data(exampleFormatted1)
#' write.table(
#'    x = exampleFormatted1,
#'    file = 'example-results-directory/Rivendell_20210623_113602.BirdNET_formatted_results.csv',
#'    row.names = FALSE, quote = FALSE, sep = ',')
#' data(exampleFormatted2)
#' write.table(
#'   x = exampleFormatted2,
#'   file = 'example-results-directory/Rivendell_20210623_114602.BirdNET_formatted_results.csv',
#'   row.names = FALSE, quote = FALSE, sep = ',')
#'
#' # Create directories with reviewed segments for this example
#' dir.create("example-segments-directory/Swainson's Thrush/Negative",
#'            recursive = TRUE)
#' dir.create("example-segments-directory/Swainson's Thrush/Positive",
#'            recursive = TRUE)
#' data(exampleSegments)
#' fi.names <- paste0("example-segments-directory/Swainson's Thrush/",
#'               c("Negative/0.224_67_Rivendell_20210623_113602_411.0s_414.0s.wav",
#'                 "Negative/0.343_35_Rivendell_20210623_113602_588.0s_591.0s.wav",
#'                 "Positive/0.666_7_Rivendell_20210623_113602_129.0s_132.0s.wav",
#'                 "Positive/0.996_6_Rivendell_20210623_114602_345.0s_348.0s.wav"))
#' for (i in 1:length(exampleSegments)) {
#' tuneR::writeWave(
#'    object = exampleSegments[[i]],
#'    filename = fi.names[i]
#'   )
#' }
#'
#' # Add reviewed segment verifications to "verify" column of formatted results
#' birdnet_review_verify(
#'   segments.directory = 'example-segments-directory',
#'   results.directory = 'example-results-directory'
#' )
#'
#' # Delete all temporary example files when finished
#' unlink(x = 'example-results-directory', recursive = TRUE)
#' unlink(x = 'example-segments-directory', recursive = TRUE)
#'
#' }
#'

birdnet_review_verify <- function(
    segments.directory,
    results.directory
)
{

  res <- birdnet_gather(results.directory = results.directory, formatted = TRUE)

  # Check segments dir contents
  sp.seg <- sort(basename(list.dirs(path = segments.directory, full.names = TRUE, recursive = FALSE)))
  sp.res <- sort(unique(res$common_name))

  # Check for any species in segments that are not in res
  sp.not <- sp.seg[which(!(sp.seg %in% sp.res))]
  if(length(sp.not) > 0) {
    warning('Found the following common_names in segments.directory: ', paste0(sp.seg, collapse = ', '), '. The following common_names in segments.directory are not found in results.directory and were not processed: ', paste(sp.not, collapse = ', '))
  }

  sp <- sp.seg[which(sp.seg %in% sp.res)]

  sg.list <- list()
  for (i in 1:length(sp)) {

    seg.dirs <- list.dirs(path = paste0(segments.directory, '/', sp[i]), recursive = FALSE)
    pos.files <- data.table(
      segment = list.files(
        path = seg.dirs[grepl(pattern = 'Positive', x = seg.dirs)],
        full.names = FALSE),
      class = 'Positive'
    )
    neg.files <- data.table(
      segment = list.files(
        path = seg.dirs[grepl(pattern = 'Negative', x = seg.dirs)],
        full.names = FALSE),
      class = 'Negative'
    )

    sg.prep <- rbind(pos.files, neg.files)

    # Extract identifying information from the segment string name:
    # ^([0-9.]+) extracts confidence value
    # _[^_]+_ avoids looking at the number in the second segment of the string
    # ((?:[^_]+_)*?[^_]+) "lazy" match for recordingID (in case of varied underscores or non-conventional naming)
    # _(\\d+\\.\\ds) grabs start
    # (\\d+\\.\\ds)\\ grabs end
    # \\.((?i:wav|mp3|flac))$ case-insensitive capture of recording file extension
    regex.match <- str_match(
      sg.prep$segment,
      "^([0-9.]+)_[^_]+_((?:[^_]+_)*?[^_]+)_(\\d+\\.\\ds)_(\\d+\\.\\ds)\\.((?i:wav|mp3|flac))$"
    )

    sg.prep[, `:=` (common_name = sp[i],
                    confidence = regex.match[,2],
                    recordingID = paste0(regex.match[,3], '.', regex.match[,6]),
                    start = as.numeric(gsub(pattern = 's', replacement = '', x = regex.match[,4])),
                    end = as.numeric(gsub(pattern = 's', replacement = '', x = regex.match[,5])))]
    sg.prep[ , composite.key := paste(recordingID, start, end, common_name, sep = '-')]

    sg.list[[i]] <- sg.prep
  }

  sg.tbl <- rbindlist(l = sg.list)
  recids <- unique(sg.tbl$recordingID)
  results <- birdnet_gather(results.directory = results.directory)

  message('Updating the following files with verifications from GUI segments... \n')

  # For each recordingID, gather results, create composite key for tracking
  for (j in 1:length(recids)) {
    update <- sg.tbl[recordingID == recids[j]]
    res <- results[recordingID == recids[j]]
    res[,composite.key := paste(recordingID, start, end, common_name, sep = '-')]

    # Update by composite key with Positive and Negative as class label for verify
    setkey(res, composite.key)
    setkey(update, composite.key)
    res[composite.key %in% update$composite.key, verify := update$class]
    res[,composite.key := NULL]

    fi <- list.files(path = results.directory,
                     pattern = gsub(pattern = '.wav|.flac|.mp3',
                                    replacement = '', x = recids[j],
                                    ignore.case = TRUE),
                     full.names = TRUE)
    formatted.finame <- fi[grep(pattern = '_formatted_', x = fi)]

    # Reset keys to sort rows consistent with typical birdnet csv
    setkey(res, start, end, common_name)
    write.csv(x = res, file = formatted.finame, row.names = FALSE)
    message(basename(formatted.finame))
  }

  message('\nDone.')
}
