# Script for generating and cleaning example data sets for NSNSDAcoustics


# Sample wave file for SongMeter_To_NVSPL ======================================
exampleAudio <- readWave(filename = './data-raw/GLBABART_20200528_104200.wav')
save(exampleAudio, file = 'data/exampleAudio.RData')



# Sample NVSPL files for NVSPL_To_AI ===========================================
nvspl.txt <- list.files('./data-raw/example-NVSPL-GLBA-2020', full.names = TRUE)
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
