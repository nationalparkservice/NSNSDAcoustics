# Script for generating and cleaning example data sets for NSNSDAcoustics


# Sample wave file for wave_to_nvspl ======================================

# Split up a larger file into two 1-minute files
# froms <- c(61, 121)
# tos <- c(120, 180)
# hms <- c('115602', '115702')
# for (i in 1:length(tos)) {
#   wav <- tuneR:::readWave(filename = 'data-raw/CCT_20210715_115502.wav',
#                           from = froms[i], to = tos[i], units = 'seconds')
#   tuneR::writeWave(object = wav,
#                    filename = paste0('data-raw/Rivendell_20210715_', hms[i], '.wav'))
#
# }
#
#
# exampleAudio1 <- readWave(filename = './data-raw/Rivendell_20210715_115602.wav')
# save(exampleAudio1, file = 'data/exampleAudio1.RData')
#
# exampleAudio2 <- readWave(filename = './data-raw/Rivendell_20210715_115702.wav')
# save(exampleAudio2, file = 'data/exampleAudio2.RData')


# Nevermind. NVSPL code wants 10 minute timesteps. Have to commit the larger file I guess.


# exampleAudio1 <- readWave(filename = './data-raw/Rivendell_20210715_114502.wav')
# save(exampleAudio1, file = 'data/exampleAudio1.RData')
#
# exampleAudio2 <- readWave(filename = './data-raw/Rivendell_20210715_115502.wav')
# save(exampleAudio2, file = 'data/exampleAudio2.RData')


# 2 x 10 minute sample waves files ========================================

# Need 10 min files for wave_to_nvspl
# want files with good birds on them to demo the birdnet fxns

# Split up a larger file into two 1-minute files
froms <- c(2997, 2396)
tos <- c(3597, 2996)
hms <- c('113602', '114602')
for (i in 1:length(tos)) {
  wav <- tuneR:::readWave(filename = 'data-raw/LEWICLAT_20210623_045502.wav',
                          from = froms[i], to = tos[i], units = 'seconds')
  wav <- downsample(object = wav, samp.rate = 22050) # downsample this
  tuneR::writeWave(object = wav,
                   filename = paste0('data-raw/Rivendell_20210623_', hms[i], '.wav'))
}

exampleAudio1 <- readWave(filename = './data-raw/Rivendell_20210623_113602.wav')
save(exampleAudio1, file = 'data/exampleAudio1.RData')

exampleAudio2 <- readWave(filename = './data-raw/Rivendell_20210623_114602.wav')
save(exampleAudio2, file = 'data/exampleAudio2.RData')



# Sample NVSPL files for nvspl_to_ai ===========================================

# Create an audio directory for this example
dir.create('example-input-directory')

# Read in example wave files
data(exampleAudio1)
data(exampleAudio2)

# Write example waves to example audio directory
tuneR::writeWave(object = exampleAudio1,
                 filename = 'example-input-directory/Rivendell_20210623_113602.wav')
tuneR::writeWave(object = exampleAudio2,
                 filename = 'example-input-directory/Rivendell_20210623_114602.wav')

# Perform wave_to_nvspl in test mode (test.file = TRUE)
wave_to_nvspl(
  input.directory = 'example-input-directory',
  data.directory = FALSE,
  test.file = FALSE,
  project = 'example-data',
  timezone = 'GMT')

nvspl.txt <- list.files('./example-input-directory/NVSPL', full.names = TRUE)
nv <- list()
for (i in 1:length(nvspl.txt)) {
  nv[[i]] <- read.delim(nvspl.txt[1], sep = ',')
}
names(nv) <- basename(nvspl.txt)
exampleNVSPL <- nv
save(exampleNVSPL, file = 'data/exampleNVSPL.RData')

unlink('example-input-directory')


# Sample scores table for add_time_cols timestep demo ==========================


# FIRST, READ IN SPECIES AUTOMATED DETECTION DATA
# Connect to db & turn on key constraints
# (note this will only work for Cathleen locally)
db.path <- 'F:/LEWI-AMMonitor-DELETE-SCORES.sqlite'
conx <- RSQLite::dbConnect(drv = dbDriver('SQLite'), dbname = db.path)
RSQLite::dbExecute(conn = conx, statement = "PRAGMA foreign_keys = ON;")


sc <- qry(db.path, statement = 'select * from scores where templateID = "pawr1" ')

dbDisconnect(conx)

set.seed(10)
s <- sc[sample(x = 1:.N, replace = FALSE, size = 10), c('recordingID', 'templateID', 'time', 'score')]

exampleScores <- s
save(exampleScores, file = 'data/exampleScores.RData')


# birdnet_run birdnet raw results example data =================================


# Set up python/reticulate
Sys.setenv(RETICULATE_PYTHON = "C:/Users/cbalantic/Anaconda3/envs/pybirdnet/python.exe")
library(reticulate) # call AFTER Sys.setenv
use_condaenv(condaenv = "pybirdnet", required = TRUE)

# Create an audio directory for this example
dir.create('example-audio-directory')

# Create a results directory for this example
dir.create('example-results-directory')

# Read in example wave files
data(exampleAudio1)
data(exampleAudio2)

# Write example waves to example audio directory
tuneR::writeWave(object = exampleAudio1,
                 filename = 'example-audio-directory/Rivendell_20210623_113602.wav')
tuneR::writeWave(object = exampleAudio2,
                 filename = 'example-audio-directory/Rivendell_20210623_114602.wav')

# Run audio data through BirdNET
birdnet_run(audio.directory = 'C:/Users/cbalantic/OneDrive - DOI/Code-NPS/NSNSDAcoustics/example-audio-directory', # must use absolute path!
            results.directory = 'C:/Users/cbalantic/OneDrive - DOI/Code-NPS/NSNSDAcoustics/data-raw', # must use absolute path!
            birdnet.directory = 'C:/Users/cbalantic/OneDrive - DOI/CathleenPythonTest/',
            birdnet.script = 'BirdNET-Reticulate.py',
            lat = 46.09924,
            lon = -123.8765)

birdnet.files <- list.files('./data-raw/', pattern = 'BirdNET_Rivendell_', full.names = TRUE)
exampleBirdNET1 <- read.csv(file = birdnet.files[1], header = TRUE)
exampleBirdNET2 <- read.csv(file = birdnet.files[2], header = TRUE)

colnames(exampleBirdNET1) <- colnames(exampleBirdNET2) <-
  c('Start (s);End (s);Scientific name;Common name;Confidence')

#Save as RData
save(exampleBirdNET1, file = 'data/exampleBirdNET1.RData')
save(exampleBirdNET2, file = 'data/exampleBirdNET2.RData')

unlink('example-audio-directory')


# Example formatted results ====================================================

# Create a BirdNET results directory for this example
dir.create('example-results-directory')

# Write examples of raw BirdNET CSV outputs to example results directory
data(exampleBirdNET1)
write.csv(x = exampleBirdNET1,
          file = 'example-results-directory/BirdNET_Rivendell_20210623_113602.csv',
          row.names = FALSE, )

data(exampleBirdNET2)
write.csv(x = exampleBirdNET2,
          file = 'example-results-directory/BirdNET_Rivendell_20210623_114602.csv',
          row.names = FALSE)

# Run birdnet_format_csv:
birdnet_format_csv(results.directory = 'example-results-directory',
                   timezone = 'GMT')

formatted.files <- list.files(path = 'example-results-directory',
                              pattern = '_formatted_',
                              full.names = TRUE)

exampleFormatted1 <- read.csv(file = formatted.files[1], header = TRUE)
exampleFormatted2 <- read.csv(file = formatted.files[2], header = TRUE)

#Save as RData
save(exampleFormatted1, file = 'data/exampleFormatted1.RData')
save(exampleFormatted2, file = 'data/exampleFormatted2.RData')

unlink('example-results-directory')
