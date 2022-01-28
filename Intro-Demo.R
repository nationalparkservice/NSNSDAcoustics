# Demo Script
# cbalantic

# Note to self: Install and Restart button on RStudio Build pane to re-run pkg after a change

library(data.table)
library(NSNSDAcoustics)

# Look at helpfile for SongMeter_To_NVSPL, which uses PAMGuide
?SongMeter_To_NVSPL

# Create an input directory for this example
dir.create('example-input-directory')

# Read in example wave file
data(exampleAudio)

# Write example wave to example input directory
tuneR::writeWave(object = exampleAudio,
                 filename = 'example-input-directory/GLBABART_20200528_104200.wav')

# Write the same example wave file twice, but give it a fake different name
# this time (SongMeter_To_NVSPL expects to process multiple files, so we're
# generating a second "fake" wave file here to demonstrate the function example)
tuneR::writeWave(object = exampleAudio,
                 filename = 'example-input-directory/GLBABART_20200528_114157.wav')

# Perform SongMeter_To_NVSPL in test mode (test.file = TRUE)
SongMeter_To_NVSPL(
  input.directory = 'example-input-directory',
  data.directory = FALSE,
  test.file = TRUE,
  project = 'testproject',
  timezone = 'GMT',
  PAMvers = 'V12_noChunk')

# see line 1684 + 1729 for example of CB question
# another question - doc w/ metadata for each column in the output file?

# Perform SongMeter_To_NVSPL in batch mode (test.file = FALSE)
SongMeter_To_NVSPL(
  input.directory = 'example-input-directory',
  data.directory = FALSE,
  test.file = FALSE,
  project = 'testproject',
  timezone = 'GMT',
  PAMvers = 'V12_noChunk')

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





# Other major functions downstream of PAMGuide ================================

# No need to do anything with these for now

# Convert NVSPL files to acoustic indices
?NVSPL_To_AI

# Run Bayesian Changepoint Analysis for a single site and year based on acoustic indices
?bcp_phenology_ai
