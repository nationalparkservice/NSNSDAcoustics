# Documentation for example data

#' @title exampleAudio1
#' @description Sample recording (audio) data stored as a \code{Wave} file.
#' @format An object of class \code{Wave} containing 6 slots.
#' @details The exampleAudio1 object can be used to experiment with functions that use audio data.
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
#' @details The exampleAudio2 object can be used to experiment with functions that use audio data.
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


#' @title exampleBirdNET1
#' @description Sample of raw BirdNET output.
#' @format An object of class \code{data.frame} containing 14 columns.
#' @details The exampleBirdNET1 object can be used to experiment with birdnet_* functions, such as \code{\link{birdnet_format}}
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
#' # Write examples of raw BirdNET txt outputs to example results directory to
#' # mimic BirdNET output format
#' write.table(
#'   x = exampleBirdNET1,
#'   file = 'example-results-directory/Rivendell_20210623_113602.BirdNET.results.r.csv',
#'   row.names = FALSE, quote = FALSE, sep = ','
#' )
#'
#' # Delete all temporary example files when finished
#' unlink(x = 'example-results-directory', recursive = TRUE)
#'
NULL


#' @title exampleBirdNET2
#' @description Sample of raw BirdNET output.
#' @format An object of class \code{data.frame} containing 14 columns.
#' @details The exampleBirdNET2 object can be used to experiment with birdnet_* functions, such as \code{\link{birdnet_format}}
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
#' # Write examples of raw BirdNET txt outputs to example results directory to
#' # mimic BirdNET output format
#' write.table(
#'   x = exampleBirdNET2,
#'   file = 'example-results-directory/Rivendell_20210623_114602.BirdNET.results.r.csv',
#'   row.names = FALSE, quote = FALSE, sep = ','
#' )
#'
#' # Delete all temporary example files when finished
#' unlink(x = 'example-results-directory', recursive = TRUE)
#'
NULL



#' @title exampleFormatted1
#' @description Sample of formatted BirdNET output.
#' @format An object of class \code{data.frame} containing 17 columns.
#' @details The exampleFormatted1 object can be used to experiment with birdnet_* functions, such as \code{\link{birdnet_gather}}
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
#'
#' # Write examples of formatted BirdNET outputs to example results directory
#' # to mimic output format of birdnet_format
#' write.table(x = exampleFormatted1,
#'             file = 'example-results-directory/Rivendell_20210623_113602.BirdNET_formatted_results.csv',
#'             row.names = FALSE, quote = FALSE, sep = ',')
#'
#' # Delete all temporary example files when finished
#' unlink(x = 'example-results-directory', recursive = TRUE)
#'
NULL


#' @title exampleFormatted2
#' @description Sample of formatted BirdNET output.
#' @format An object of class \code{data.frame} containing 17 columns.
#' @details The exampleFormatted2 object can be used to experiment with birdnet_* functions, such as \code{\link{birdnet_gather}}
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
#'
#' # Write examples of formatted BirdNET outputs to example results directory
#' # to mimic output format of birdnet_format
#' write.table(x = exampleFormatted2,
#'             file = 'example-results-directory/Rivendell_20210623_114602.BirdNET_formatted_results.csv',
#'             row.names = FALSE, quote = FALSE, sep = ',')
#'
#' # Delete all temporary example files when finished
#' unlink(x = 'example-results-directory', recursive = TRUE)
#'
NULL


#' @title exampleSpectroData
#' @description Sample data.frame to demonstrate \code{\link{birdnet_spectro}}
#' @format An object of class \code{data.frame} and \code{data.table} containing 17 columns.
#' @details The exampleSpectroData object is used to demonstrate \code{\link{birdnet_spectro}}
#' @docType data
#' @keywords datasets
#' @name exampleSpectroData
#' @usage data(exampleSpectroData)
#' @examples
#'
#' # Load the dataset
#' data(exampleSpectroData)
#'
#' # View structure
#' str(exampleSpectroData, max.level = 1)
#'
NULL


#' @title exampleBarchartData
#' @description Sample data.table / data.frame to demonstrate \code{\link{birdnet_barchart}}
#' @format An object of class \code{data.frame} and \code{data.table} containing 17 columns.
#' @details The exampleBarchartData object is used to demonstrate \code{\link{birdnet_barchart}}
#' @docType data
#' @keywords datasets
#' @name exampleBarchartData
#' @usage data(exampleBarchartData)
#' @examples
#'
#' # Load the dataset
#' data(exampleBarchartData)
#'
#' # View structure
#' str(exampleBarchartData, max.level = 1)
#'
NULL


#' @title exampleAI
#' @description Sample data.frame to demonstrate \code{\link{plot_audio_summary}}
#' @format An object of class \code{data.frame} containing 60 columns.
#' @details The exampleAI object is used to demonstrate \code{\link{plot_audio_summary}}
#' @seealso \code{\link{plot_audio_summary}}, \code{\link{nvspl_to_ai}}
#' @docType data
#' @keywords datasets
#' @name exampleAI
#' @usage data(exampleAI)
#' @examples
#'
#' # Load the dataset
#' data(exampleAI)
#'
#' # View structure
#' str(exampleAI, max.level = 1)
#'
NULL



#' @title exampleHeatmapData
#' @description Example data for \code{\link{birdnet_heatmap}} and \code{\link{birdnet_heatmap_time}}
#' @format An object of class \code{data.table} and \code{data.frame} containing 14 columns.
#' @details The exampleHeatmapData object can be used to experiment with birdnet_heatmap* functions.
#' @docType data
#' @keywords datasets
#' @name exampleHeatmapData
#' @usage data(exampleHeatmapData)
#' @examples
#' # Load the dataset
#' data(exampleHeatmapData)
#'
#' # View structure
#' str(exampleHeatmapData, max.level = 1)
#'
#'
NULL

#' @title exampleDatesSampled
#' @description Example data for \code{\link{birdnet_heatmap}} and \code{\link{birdnet_heatmap_time}}
#' @format An object of class \code{Date} of length 430.
#' @details The exampleDatesSampled object can be used to experiment with birdnet_heatmap* functions.
#' @docType data
#' @keywords datasets
#' @name exampleDatesSampled
#' @usage data(exampleDatesSampled)
#' @examples
#' # Load the dataset
#' data(exampleDatesSampled)
#'
#' # View structure
#' str(exampleDatesSampled, max.level = 1)
#'
#'
NULL

#' @title exampleSpeciesList
#' @description Example data for \code{\link{birdnet_analyzer}}
#' @format An object of class \code{data.table} and \code{data.frame} with 4 rows and 1 column.
#' @details The exampleSpeciesList object can be used to test the \code{\link{birdnet_analyzer}} function.
#' @docType data
#' @keywords datasets
#' @name exampleSpeciesList
#' @usage data(exampleSpeciesList)
#' @examples
#' # Load the dataset
#' data(exampleSpeciesList)
#'
#' # View structure
#' str(exampleSpeciesList, max.level = 1)
#'
#'
NULL
