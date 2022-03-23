# Documentation for example data

#' @title exampleAudio1
#' @description Sample recording (audio) data stored as a \code{Wave} file.
#' @format An object of class \code{Wave} containing 6 slots.
#' @details The exampleAudio1 object can be used to experiment with functions that use audio data, such as \code{\link{Wave_To_NVSPL}}
#' @docType data
#' @keywords datasets
#' @name exampleAudio1
#' @usage data(exampleAudio1)
#' @examples
#' # Load the dataset
#' data(exampleAudio1)
#'
#' # View structure
#' str(exampleAudio1, max.level = 1)
#'
NULL

#' @title exampleAudio2
#' @description Sample recording (audio) data stored as a \code{Wave} file.
#' @format An object of class \code{Wave} containing 6 slots.
#' @details The exampleAudio2 object can be used to experiment with functions that use audio data, such as \code{\link{Wave_To_NVSPL}}
#' @docType data
#' @keywords datasets
#' @name exampleAudio2
#' @usage data(exampleAudio2)
#' @examples
#' # Load the dataset
#' data(exampleAudio2)
#'
#' # View structure
#' str(exampleAudio2, max.level = 1)
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
