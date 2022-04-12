# Documentation for example data

#' @title exampleAudio1
#' @description Sample recording (audio) data stored as a \code{Wave} file.
#' @format An object of class \code{Wave} containing 6 slots.
#' @details The exampleAudio1 object can be used to experiment with functions that use audio data, such as \code{\link{wave_to_nsvpl}}
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
#' @details The exampleAudio2 object can be used to experiment with functions that use audio data, such as \code{\link{wave_to_nvspl}}
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
#' @details The exampleNVSPL object can be used to experiment with functions that use NVSPL data. See the example code given in \code{\link{nvspl_to_ai}}.
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


#' @title exampleBirdNET1
#' @description Sample of raw BirdNET output.
#' @format An object of class \code{data.frame} containing 1 column.
#' @details The exampleBirdNET1 object can be used to experiment with birdnet_* functions, such as \code{\link{birdnet_format_csv}}
#' @docType data
#' @keywords datasets
#' @name exampleBirdNET1
#' @usage data(exampleBirdNET1)
#' @examples
#' # Load the dataset
#' data(exampleBirdNET1)
#'
#' # View structure
#' str(exampleBirdNET1, max.level = 1)
#'
#' # Create a BirdNET results directory for this example
#' dir.create('example-results-directory')
#'
#' # Write examples of raw BirdNET CSV outputs to example results directory to
#' # mimic BirdNET output format
#' write.csv(x = exampleBirdNET1,
#'           file = 'example-results-directory/BirdNET_Rivendell_20210623_113602.csv',
#'           row.names = FALSE, )
#'
#' # Delete all temporary example files when finished
#' unlink(x = 'example-results-directory', recursive = TRUE)
#'
NULL


#' @title exampleBirdNET2
#' @description Sample of raw BirdNET output.
#' @format An object of class \code{data.frame} containing 1 column.
#' @details The exampleBirdNET2 object can be used to experiment with birdnet_* functions, such as \code{\link{birdnet_format_csv}}
#' @docType data
#' @keywords datasets
#' @name exampleBirdNET2
#' @usage data(exampleBirdNET2)
#' @examples
#' # Load the dataset
#' data(exampleBirdNET2)
#'
#' # View structure
#' str(exampleBirdNET2, max.level = 1)
#'
#' # Create a BirdNET results directory for this example
#' dir.create('example-results-directory')
#'
#' # Write examples of raw BirdNET CSV outputs to example results directory to
#' # mimic BirdNET output format
#' write.csv(x = exampleBirdNET2,
#'           file = 'example-results-directory/BirdNET_Rivendell_20210623_114602.csv',
#'           row.names = FALSE, )
#'
#' # Delete all temporary example files when finished
#' unlink(x = 'example-results-directory', recursive = TRUE)
#'
NULL



#' @title exampleFormatted1
#' @description Sample of formatted BirdNET output.
#' @format An object of class \code{data.frame} containing 8 columns.
#' @details The exampleFormatted1 object can be used to experiment with birdnet_* functions, such as \code{\link{birdnet_gather_results}}
#' @docType data
#' @keywords datasets
#' @name exampleFormatted1
#' @usage data(exampleFormatted1)
#' @examples
#' # Load the dataset
#' data(exampleFormatted1)
#'
#' # View structure
#' str(exampleFormatted1, max.level = 1)
#'
#' # Create a BirdNET results directory for this example
#' dir.create('example-results-directory')

#' # Write examples of formatted BirdNET CSV outputs to example results directory
#' # to mimic output format of birdnet_format_csv
#' write.csv(x = exampleFormatted1,
#'           file = 'example-results-directory/BirdNET_formatted_Rivendell_20210623_113602.csv',
#'           row.names = FALSE, )
#'
#' # Delete all temporary example files when finished
#' unlink(x = 'example-results-directory', recursive = TRUE)
#'
NULL


#' @title exampleFormatted2
#' @description Sample of formatted BirdNET output.
#' @format An object of class \code{data.frame} containing 8 columns.
#' @details The exampleFormatted2 object can be used to experiment with birdnet_* functions, such as \code{\link{birdnet_gather_results}}
#' @docType data
#' @keywords datasets
#' @name exampleFormatted2
#' @usage data(exampleFormatted2)
#' @examples
#' # Load the dataset
#' data(exampleFormatted2)
#'
#' # View structure
#' str(exampleFormatted2, max.level = 1)
#'
#' # Create a BirdNET results directory for this example
#' dir.create('example-results-directory')

#' # Write examples of formatted BirdNET CSV outputs to example results directory
#' # to mimic output format of birdnet_format_csv
#' write.csv(x = exampleFormatted2,
#'           file = 'example-results-directory/BirdNET_formatted_Rivendell_20210623_114602.csv',
#'           row.names = FALSE, )
#'
#' # Delete all temporary example files when finished
#' unlink(x = 'example-results-directory', recursive = TRUE)
#'
NULL


#' @title examplePlotData
#' @description Sample data.frame to demonstrate birdnet_plot_detections
#' @format An object of class \code{data.frame} and \code{data.table} containing 8 columns.
#' @details The examplePlotData object is used to demonstrate \code{\link{birdnet_plot_detections}}
#' @docType data
#' @keywords datasets
#' @name examplePlotData
#' @usage data(examplePlotData)
#' @examples
#'
#' # Load the dataset
#' data(examplePlotData)
#'
#' # View structure
#' str(examplePlotData, max.level = 1)
#'
NULL

