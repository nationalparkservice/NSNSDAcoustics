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

# Look at helpfile for Wave_To_NVSPL, which uses PAMGuide
?Wave_To_NVSPL

# Create an input directory for this example
dir.create('example-input-directory')

# Read in example wave file
data(exampleAudio)

# Write example wave to example input directory
tuneR::writeWave(object = exampleAudio,
                 filename = 'example-input-directory/GLBABART_20200528_104200.wav')

# Write the same example wave file twice, but give it a fake different name
# this time (Wave_To_NVSPL expects to process multiple files, so we're
# generating a second "fake" wave file here to demonstrate the function example)
tuneR::writeWave(object = exampleAudio,
                 filename = 'example-input-directory/GLBABART_20200528_114157.wav')

# Perform Wave_To_NVSPL in test mode (test.file = TRUE)
Wave_To_NVSPL(
  input.directory = 'example-input-directory',
  data.directory = FALSE,
  test.file = TRUE,
  project = 'testproject',
  timezone = 'GMT')

# PLACES TO CHECK in main Wave_To_NVSPL wrapper function
# (it's possible the mistakes are in the math of the NPS script, and not in the pamguide code itself?)
# --> Line 1692 section might be the first place to check?

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


# To step into the function itself and troubleshoot or debug,
# I prefer placing a browser() call near the problem line in the function
# Then re-source the function change with that browser() call, and the next time
# you run the function you'll step into the function environment and be able to debug




# NVSPL_To_AI =================================================================

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

# No need to do anything with these for now

# Convert NVSPL files to acoustic indices
?NVSPL_To_AI

# Run Bayesian Changepoint Analysis for a single site and year based on acoustic indices
?bcp_phenology_ai
