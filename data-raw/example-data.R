# Script for generating and cleaning example data sets for NSNSDAcoustics


# Sample wave file for Wave_To_NVSPL ======================================

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


exampleAudio1 <- readWave(filename = './data-raw/Rivendell_20210715_114502.wav')
save(exampleAudio1, file = 'data/exampleAudio1.RData')

exampleAudio2 <- readWave(filename = './data-raw/Rivendell_20210715_115502.wav')
save(exampleAudio2, file = 'data/exampleAudio2.RData')


# Sample NVSPL files for NVSPL_To_AI ===========================================

# Perform Wave_To_NVSPL in test mode (test.file = TRUE)
Wave_To_NVSPL(
  input.directory = 'data-raw',
  data.directory = FALSE,
  test.file = FALSE,
  project = 'example-data',
  timezone = 'GMT')

nvspl.txt <- list.files('./data-raw/NVSPL', full.names = TRUE)
nv <- list()
for (i in 1:length(nvspl.txt)) {
  nv[[i]] <- read.delim(nvspl.txt[1], sep = ',')
}
names(nv) <- basename(nvspl.txt)
exampleNVSPL <- nv
save(exampleNVSPL, file = 'data/exampleNVSPL.RData')



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
