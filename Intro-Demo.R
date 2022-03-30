# Demo Script
# cbalantic

# Note to self: Install and Restart button on RStudio Build pane to re-run pkg after a change

library(data.table)
library(NSNSDAcoustics)

# Not sure why these aren't importing even though pkg is built?
library(tuneR)
library(ineq)
library(moments)
library(NbClust)
library(svDialogs)
library(vegan)

# Wave_To_NVSPL ================================================================

# Look at helpfile for Wave_To_NVSPL, which uses PAMGuide
?Wave_To_NVSPL

# Create an input directory for this example
dir.create('example-input-directory')

# Read in example wave files
data(exampleAudio1)
data(exampleAudio2)

# Write example waves to example input directory
tuneR::writeWave(object = exampleAudio1,
                 filename = 'example-input-directory/Rivendell_20210715_114502.wav')
tuneR::writeWave(object = exampleAudio2,
                 filename = 'example-input-directory/Rivendell_20210715_115502.wav')

# Perform Wave_To_NVSPL in test mode (test.file = TRUE)
Wave_To_NVSPL(
  input.directory = 'example-input-directory',
  data.directory = FALSE,
  test.file = TRUE,
  project = 'testproject',
  timezone = 'GMT')

# Perform Wave_To_NVSPL in batch mode (test.file = FALSE)
Wave_To_NVSPL(
  input.directory = 'example-input-directory',
  data.directory = FALSE,
  test.file = FALSE,
  project = 'testproject',
  timezone = 'GMT')

# Verify that NVSPL outputs have been created
nvspls <- list.files('example-input-directory/NVSPL', full.names = TRUE)

# View one of the NVSPL outputs
one.nvspl <- read.delim(file = nvspls[1], sep = ',')
data.table(one.nvspl)

# Delete all temporary example files when finished
unlink(x = 'example-input-directory', recursive = TRUE)



# NVSPL_To_AI =================================================================

# Look at helpfile for NVSPL_To_AI
?NVSPL_To_AI

# Create an input and output directory for this example
dir.create('example-input-directory')
dir.create('example-output-directory')

# Read in example NVSPL data
data(exampleNVSPL)

# Write example NVSPL data to example input directory
for (i in 1:length(exampleNVSPL)) {
  write.table(x = exampleNVSPL[[i]],
              file = paste0('example-input-directory/', names(exampleNVSPL)[i]),
              sep = ',',
              quote = FALSE)
}

# Run NVSPL_To_AI to generate acoustic indices csv for example NVSPL files,
# setting start.at.beginning = FALSE
NVSPL_To_AI(input.directory = 'example-input-directory',
            output.directory = 'example-output-directory',
            project = 'example-project',
            start.at.beginning = FALSE)

# Run NVSPL_To_AI to generate acoustic indices csv for example NVSPL files,
# setting start.at.beginning = TRUE
NVSPL_To_AI(input.directory = 'example-input-directory',
            output.directory = 'example-output-directory',
            project = 'example-project',
            start.at.beginning = TRUE)

# Read in both files to compare
start.standard <- read.csv(list.files(path = './example-output-directory/',
                                      pattern = 'Index_Created',
                                      full.names = TRUE))
start.begin <- read.csv(list.files(path = './example-output-directory/',
                                   pattern = 'Begin_Created',
                                   full.names = TRUE))

# View a few rows of each file; note the Hr, Min, Sec differences between both options
start.standard[1:4, ]
start.begin[1:4, ]

# Delete all temporary example files when finished
unlink(x = 'example-input-directory', recursive = TRUE)
unlink(x = 'example-output-directory', recursive = TRUE)




# Other major functions downstream of PAMGuide ================================

# birdnet functions

# Run Bayesian Changepoint Analysis for a single site and year based on acoustic indices
?bcp_phenology_ai # hold off on these for now since maybe they will not be relevant
# I think the best place to focus may be BirdNET for now
