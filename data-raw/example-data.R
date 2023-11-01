# Script for generating and cleaning example data sets for NSNSDAcoustics

# This example-data-old.R version reflects example data formatted for BirdNET-Analyzer

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


# birdnet_analyzer =============================================================

library(NSNSDAcoustics)
library(tools)

# Set up python/reticulate
Sys.setenv(RETICULATE_PYTHON = "C:/Users/cbalantic/Anaconda3/envs/pybirdanalyze/python.exe")
library(reticulate) # call AFTER Sys.setenv
use_condaenv(condaenv = "pybirdanalyze", required = TRUE)

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

# Run audio data through BirdNET using week + lat longs
# source("C:/Users/cbalantic/OneDrive - DOI/Code-NPS/NSNSDAcoustics/R/birdnet_analyzer.R")
birdnet_analyzer(audio.directory = 'C:/Users/cbalantic/OneDrive - DOI/Code-NPS/NSNSDAcoustics/example-audio-directory', # must use absolute path!
                 results.directory = 'C:/Users/cbalantic/OneDrive - DOI/Code-NPS/NSNSDAcoustics/example-results-directory', # must use absolute path!
                 birdnet.directory = 'C:/Users/cbalantic/OneDrive - DOI/BirdNET-Analyzer-main/',
                 use.week = TRUE,
                 lat = 46.09924,
                 lon = -123.8765)

# Testing lat longs but use.week = FALSE
birdnet_analyzer(audio.directory = 'C:/Users/cbalantic/OneDrive - DOI/Code-NPS/NSNSDAcoustics/example-audio-directory', # must use absolute path!
                 results.directory = 'C:/Users/cbalantic/OneDrive - DOI/Code-NPS/NSNSDAcoustics/example-results-directory', # must use absolute path!
                 birdnet.directory = 'C:/Users/cbalantic/OneDrive - DOI/BirdNET-Analyzer-main/',
                 use.week = FALSE,
                 lat = 46.09924,
                 lon = -123.8765)

# Testing species list, use.week = FALSE
birdnet_analyzer(audio.directory = 'C:/Users/cbalantic/OneDrive - DOI/Code-NPS/NSNSDAcoustics/example-audio-directory', # must use absolute path!
                 results.directory = 'C:/Users/cbalantic/OneDrive - DOI/Code-NPS/NSNSDAcoustics/example-results-directory', # must use absolute path!
                 birdnet.directory = 'C:/Users/cbalantic/OneDrive - DOI/BirdNET-Analyzer-main/',
                 use.week = FALSE,
                 slist = 'C:/Users/cbalantic/OneDrive - DOI/Code-NPS/NSNSDAcoustics/data-raw/sp-list.txt')


# Testing LL = -1
birdnet_analyzer(audio.directory = 'C:/Users/cbalantic/OneDrive - DOI/Code-NPS/NSNSDAcoustics/example-audio-directory', # must use absolute path!
                 results.directory = 'C:/Users/cbalantic/OneDrive - DOI/Code-NPS/NSNSDAcoustics/example-results-directory', # must use absolute path!
                 birdnet.directory = 'C:/Users/cbalantic/OneDrive - DOI/BirdNET-Analyzer-main/',
                 use.week = FALSE,
                 lat = -1,
                 lon = -1)





birdnet.files <- list.files('./data-raw/', pattern = '.BirdNET.results.csv', full.names = TRUE)
#birdnet.files <- birdnet.files[grep('.txt', birdnet.files)]
exampleBirdNET1 <- fread(file = birdnet.files[1], header = TRUE)
exampleBirdNET2 <- fread(file = birdnet.files[2], header = TRUE)

# Scrub out filepath lead
exampleBirdNET1[,filepath := gsub('C:/Users/cbalantic/OneDrive - DOI/Code-NPS/NSNSDAcoustics', '', filepath)]
exampleBirdNET2[,filepath := gsub('C:/Users/cbalantic/OneDrive - DOI/Code-NPS/NSNSDAcoustics', '', filepath)]

#Save as RData
save(exampleBirdNET1, file = 'data/exampleBirdNET1.RData')
save(exampleBirdNET2, file = 'data/exampleBirdNET2.RData')

unlink('example-audio-directory')


# Example formatted results ====================================================

# Create a BirdNET results directory for this example
dir.create('example-results-directory')

# Write examples of raw BirdNET CSV outputs to example results directory
data(exampleBirdNET1)
write.table(x = exampleBirdNET1,
            file = 'example-results-directory/Rivendell_20210623_114602.BirdNET.results.csv',
            row.names = FALSE, quote = FALSE, sep = ',')

data(exampleBirdNET2)
write.table(x = exampleBirdNET2,
            file = 'example-results-directory/Rivendell_20210623_114602.BirdNET.results.csv',
            row.names = FALSE, quote = FALSE, sep = ',')

# Run birdnet_format:
birdnet_format(results.directory = 'example-results-directory',
               timezone = 'GMT')

formatted.files <- list.files(path = 'example-results-directory',
                              pattern = '_formatted_',
                              full.names = TRUE)

exampleFormatted1 <- fread(file = formatted.files[1], header = TRUE)
exampleFormatted2 <- fread(file = formatted.files[2], header = TRUE)

#Save as RData
save(exampleFormatted1, file = 'data/exampleFormatted1.RData')
save(exampleFormatted2, file = 'data/exampleFormatted2.RData')

unlink('example-results-directory')


# Example plot data ============================================================

# Create an audio directory for this example
dir.create('example-audio-directory')

# Read in example wave files
data(exampleAudio1)
data(exampleAudio2)

# Write example waves to example audio directory
tuneR::writeWave(object = exampleAudio1,
                  filename = 'example-audio-directory/Rivendell_20210623_113602.wav')
tuneR::writeWave(object = exampleAudio2,
                  filename = 'example-audio-directory/Rivendell_20210623_114602.wav')

# Create a BirdNET results directory for this example
dir.create('example-results-directory')

# Write examples of formatted BirdNET outputs to example results directory
data(exampleFormatted1)
write.table(x = exampleFormatted1,
            file = 'example-results-directory/BirdNET_formatted_Rivendell_20210623_113602.txt',
            row.names = FALSE, quote = FALSE, sep = ',')

data(exampleFormatted2)
write.table(x = exampleFormatted2,
            file = 'example-results-directory/BirdNET_formatted_Rivendell_20210623_114602.txt',
            row.names = FALSE, quote = FALSE, sep = ',')


# Gather formatted BirdNET results
dat <- birdnet_gather(results.directory = 'example-results-directory',
                      formatted = TRUE)

# Create a random sample of three detections to verify
to.verify <- rbind(dat[common_name == "Swainson's Thrush" & confidence >= 0.95],
                   dat[common_name == "Swainson's Thrush" & confidence <= 0.2])

# Create a verification library for this species
ver.lib <- c('song', 'call', 'false', 'unsure')

# Verify detections
birdnet_verify(data = to.verify,
               verification.library = ver.lib,
                audio.directory = 'example-audio-directory',
                results.directory = 'example-results-directory',
                overwrite = FALSE)

# Check that underlying data have been updated with user verifications
dat <- birdnet_gather(results.directory = 'example-results-directory',
                      formatted = TRUE)
#dat <- dat[!is.na(verify)] # no! we want all the stuff, even unverified, to demo unverified plotting
examplePlotData <- dat

#Save as RData
save(examplePlotData, file = 'data/examplePlotData.RData')

# Delete all temporary example files when finished
unlink(x = 'example-audio-directory', recursive = TRUE)
unlink(x = 'example-results-directory', recursive = TRUE)

# data for birdnet_barchart ====================================================

results.dir <- 'C:/Users/cbalantic/OneDrive - DOI/BirdNET-guidance/birdnet-sandbox/Results/LEWI/BirdNET-analyzer-results'

# Gather up results
res <- birdnet_gather(results.directory = results.dir, formatted = TRUE)

# Change the column content to be Rivendell
res[,filepath := gsub(pattern = 'CLAT/LEWICLAT', replacement = 'Rivendell', x = filepath)]
res[,recordingID := gsub(pattern = 'LEWICLAT', replacement = 'Rivendell', x = recordingID)]
res[,c('lat', 'lon') := .(NA, NA)]

exampleBarchartData <- res[grep('Rivendell', x = filepath)]

#Save as RData
save(exampleBarchartData, file = 'data/exampleBarchartData.RData')



data <- add_time_cols(dt = data,
                     tz.recorder = 'America/Los_angeles',
                     tz.local = 'America/Los_angeles')


out <- birdnet_barchart(data = test, interactive = TRUE)


# Testing focal species/ focal colors
birdnet_barchart(data = test,
                 interactive = FALSE,
                 focal.species = c("Pacific Wren", "Swainson's Thrush"),
                 focal.colors = c('#00BE67', '#C77CFF'))




# debugging birdnet format BirdNET_ find and replace ===========================

# Fixing the "BirdNET_" find and replace issue -- if a folder also has this name,
# it will be replaced and have the wrong folder path

# Also checking on txt results with no file

# Create a BirdNET results directory for this example
dir.create('BirdNET_results')

# Write examples of raw BirdNET CSV outputs to example results directory
data(exampleBirdNET1)
write.table(x = exampleBirdNET1,
            file = 'BirdNET_results/BirdNET_Rivendell_20210623_113602.txt',
            row.names = FALSE, quote = FALSE, sep = ',')

data(exampleBirdNET2)
write.table(x = exampleBirdNET2,
            file = 'BirdNET_results/BirdNET_Rivendell_20210623_114602.txt',
            row.names = FALSE, quote = FALSE, sep = ',')

# Copy result-free CCC txt file into the folder
file.copy(from = './data-raw/BirdNET_CCC_20181011_225700.txt',
          to = './BirdNET_results/BirdNET_CCC_20181011_225700.txt')

# Run birdnet_format:
birdnet_format(results.directory = 'BirdNET_results',
               timezone = 'GMT')



# Also checking on it for old CSV
unlink('BirdNET_results')
dir.create('BirdNET_results')

# Copy result-free CCC txt file into the folder
file.copy(from = './data-raw/old/rawCSV/BirdNET_UBMC_20191102_190300.csv',
          to = './BirdNET_results/BirdNET_UBMC_20191102_190300.csv')

# Copy regular result
file.copy(from = './data-raw/old/rawCSV/BirdNET_Rivendell_20210623_113602.csv',
          to = './BirdNET_results/BirdNET_Rivendell_20210623_113602.csv')

# Run birdnet_format:
birdnet_format(results.directory = 'BirdNET_results',
               timezone = 'GMT')



# debugging birdnet_format & birdnet_barchart NA issue =========================

# needed to make sure "BirdNET_" was not getting appended into the recordingID when there were no results

# Create a BirdNET results directory for this example
dir.create('BirdNET_results')

# Copy result-free txt file into the folder
file.copy(from = 'C:/Users/cbalantic/OneDrive - DOI/BirdNET-guidance/birdnet-sandbox/Results/ROMO/BirdNET-Analyzer-No-2019-CCT-CCC-HSPC/BirdNET_HSPT_20210606_024004.txt',
          to = './BirdNET_results/BirdNET_HSPT_20210606_024004.txt')
file.copy(from = 'C:/Users/cbalantic/OneDrive - DOI/BirdNET-guidance/birdnet-sandbox/Results/ROMO/BirdNET-Analyzer-No-2019-CCT-CCC-HSPC/BirdNET_HSPT_20210606_025004.txt',
          to = './BirdNET_results/BirdNET_HSPT_20210606_025004.txt')

birdnet_format(results.directory = 'BirdNET_results', timezone = 'UTC')

dat <- birdnet_gather(results.directory = 'BirdNET_results', formatted = TRUE)
dat <- add_time_cols(dt = dat, tz.recorder = 'UTC', tz.local = 'America/Denver')
birdnet_barchart(data = dat, interactive = FALSE,
                 focal.species = c('American Robin', 'Willow Flycatcher', 'Cordilleran Flycatcher'),
                 focal.colors = c('blue', 'red', 'orange'))

birdnet_barchart(data = dat, interactive = FALSE,
                 focal.species = c('American Robin', 'Willow Flycatcher', 'Cordilleran Flycatcher'),
                 focal.colors = c('blue', 'red', 'orange'))

# TESTING BIRDNET v2.2 =========================================================

# August 2022

library(NSNSDAcoustics)

# Set up python/reticulate
Sys.setenv(RETICULATE_PYTHON = "C:/Users/cbalantic/Anaconda3/envs/pybirdanalyze2pt2/python.exe")
library(reticulate) # call AFTER Sys.setenv
use_condaenv(condaenv = "pybirdanalyze2pt2", required = TRUE)

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
source("C:/Users/cbalantic/OneDrive - DOI/Code-NPS/NSNSDAcoustics/R/birdnet_analyzer.R")
library(tools)
birdnet_analyzer(audio.directory = 'C:/Users/cbalantic/OneDrive - DOI/Code-NPS/NSNSDAcoustics/example-audio-directory', # must use absolute path!
                 results.directory = 'C:/Users/cbalantic/OneDrive - DOI/Code-NPS/NSNSDAcoustics/example-results-directory', # must use absolute path!
                 birdnet.directory = 'C:/Users/cbalantic/OneDrive - DOI/BirdNET-Analyzer-main-2pt2/',
                 lat = 46.09924,
                 lon = -123.8765)

v22 <- rbindlist(lapply(list.files('example-results-directory', full.names = TRUE), function(x) fread(x)))

data(exampleBirdNET1)
data(exampleBirdNET2)
v21 <- rbind(exampleBirdNET1, exampleBirdNET2)

v21[,mean(confidence), by = c('common_name')]
v22[,mean(confidence), by = c('common_name')] # wtf... there are NO varied thrush in this at all? Wtf?

v21[,.N, by = c('common_name')]
v22[,.N, by = c('common_name')]

# TESTING BIRDNET v2.3 =========================================================
# April 2023
library(NSNSDAcoustics)
source("C:/Users/cbalantic/OneDrive - DOI/Code-NPS/NSNSDAcoustics/R/birdnet_analyzer.R")

# Set up python/reticulate
Sys.setenv(RETICULATE_PYTHON = "C:/Users/cbalantic/Anaconda3/envs/pybirdanalyze2pt3/python.exe")
library(reticulate) # call AFTER Sys.setenv
use_condaenv(condaenv = "pybirdanalyze2pt3", required = TRUE)

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
source("C:/Users/cbalantic/OneDrive - DOI/Code-NPS/NSNSDAcoustics/R/birdnet_analyzer.R")
library(tools)
birdnet_analyzer(audio.directory = 'C:/Users/cbalantic/OneDrive - DOI/Code-NPS/NSNSDAcoustics/example-audio-directory', # must use absolute path!
                 results.directory = 'C:/Users/cbalantic/OneDrive - DOI/Code-NPS/NSNSDAcoustics/example-results-directory', # must use absolute path!
                 birdnet.directory = 'C:/Users/cbalantic/OneDrive - DOI/BirdNET-Analyzer-main-2pt3/',
                 lat = 46.09924,
                 lon = -123.8765,
                 use.week = FALSE)

v23 <- rbindlist(lapply(list.files('example-results-directory', full.names = TRUE), function(x) fread(x)))

data(exampleBirdNET1)
data(exampleBirdNET2)
v21 <- rbind(exampleBirdNET1, exampleBirdNET2)

v21[,mean(confidence), by = c('common_name')]
v22[,mean(confidence), by = c('common_name')] # wtf... there are NO varied thrush in this at all? Wtf?

v21[,.N, by = c('common_name')]
v22[,.N, by = c('common_name')]

unlink('example-audio-directory')
unlink('example-results-directory')

# Sample data to generate acoustic index file ==================================


exampleAI <- read.csv('data-raw/Rivendell_10mins_NVSPL_AcousticIndexStartatBegin_2012to2021_Created2022-04-21.csv')

#Save as RData
save(exampleAI, file = 'data/exampleAI.RData')
