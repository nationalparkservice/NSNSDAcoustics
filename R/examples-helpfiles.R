# Documentation for example data

#' @title exampleAudio
#' @description Sample recording (audio) data stored as a \code{Wave} file. 
#' @format An object of class \code{Wave} containing 6 slots.
#' @details The exampleAudio object can be used to experiment with functions that use audio data, such as \code{\link{SongMeter_To_NVSPL}}
#' @docType data
#' @keywords datasets
#' @name exampleAudio
#' @usage data(exampleAudio)
#' @examples
#' # Load the dataset
#' data(exampleAudio)
#'
#' # View structure
#' str(exampleAudio, max.level = 1)
#'
NULL



#' @title exampleNVSPL
#' @description Sample NVSPL data stored as data.frames within a list object.
#' @format An object of class \code{list} containing 9 data.frames.
#' @details The exampleNVSPL object can be used to experiment with functions that use NVSPL data. See the example code given in \code{\link{NVSPL_To_AI}}.
#' @docType data
#' @keywords datasets
#' @name exampleNVSPL
#' @usage data(exampleNVSPL)
#' @examples
#' 
#' # Create an input directory for this example
#' dir.create('example-input-directory')
#'
#' # Read in example NVSPL data
#' data(exampleNVSPL)
#' 
#' # View structure 
#' str(exampleNVSPL, 1)
#'
#' # Write example NVSPL data to example input directory to mimic NVSPL .txt format
#' for (i in 1:length(exampleNVSPL)) {
#' write.table(x = exampleNVSPL[[i]],
#'             file = paste0('example-input-directory/', names(exampleNVSPL)[i]),
#'             sep = ',',
#'             quote = FALSE)
#' }
#'
#' # Delete all temporary example files when finished
#' unlink(x = 'example-input-directory', recursive = TRUE)
#'
NULL



#' @title exampleScores
#' @description Sample scores data.frame.
#' @format An object of class \code{data.frame} and \code{data.table} containing 4 columns.
#' @details The exampleScores object is used to demonstrate \code{\link{add_time_cols}}.
#' @docType data
#' @keywords datasets
#' @name exampleScores
#' @usage data(exampleScores)
#' @examples
#' 
#' # Read in example data
#' data(exampleScores)
#' 
#' # View structure 
#' str(exampleScores, 1)
#'
NULL
