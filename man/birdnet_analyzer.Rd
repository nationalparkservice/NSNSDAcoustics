% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/birdnet_analyzer.R
\name{birdnet_analyzer}
\alias{birdnet_analyzer}
\title{Process audio files through BirdNET}
\usage{
birdnet_analyzer(
  birdnet.version,
  birdnet.path,
  i.audio,
  o.results,
  fmin = 0,
  fmax = 15000,
  lat = -1,
  lon = -1,
  week = -1,
  sf.thresh = 0.03,
  slist = NULL,
  sensitivity = 1,
  overlap = 0,
  audio.speed = 1,
  threads = 2,
  min.conf = 0.1,
  locale = "en",
  batchsize = 1,
  rtype = "csv",
  additional.columns = c("lat", "lon", "week", "overlap", "sensitivity", "min_conf",
    "species_list", "model"),
  combine.results = FALSE,
  classifier = NULL,
  skip.existing.results = FALSE,
  top.n = NULL,
  merge.consecutive = 1
)
}
\arguments{
\item{birdnet.version}{Character name of BirdNET Analyzer release you are using.
E.g., \code{"v2.1.1"}, \code{"v1.5.1"}. Support for earlier versions is not prioritized. If this doesn't
work, update to a new version of BirdNET Analyzer or \href{https://github.com/nationalparkservice/NSNSDAcoustics/blob/main/README.md#additional-tips-and-notes}{consider constructing the command statement
by hand} instead of using this function. See releases at \href{https://github.com/birdnet-team/BirdNET-Analyzer/releases}{https://github.com/birdnet-team/BirdNET-Analyzer/releases}.
\strong{CRITICAL NOTE}: The BirdNET "release" version specified in this argument
is different from the BirdNET model version. The only BirdNET model version
supported by this function is model version 2.4.}

\item{birdnet.path}{Absolute path to BirdNET-Analyzer.exe installation on your
machine. e.g., "C:/path/to/BirdNET-Analyzer/BirdNET-Analyzer.exe"}

\item{i.audio}{Absolute path to input audio file or audio folder.}

\item{o.results}{Absolute path to output folder.}

\item{fmin}{Minimum frequency for bandpass filter in Hz. Default = 0.}

\item{fmax}{Maximum frequency for bandpass filter in Hz. Default = 15000.}

\item{lat}{Recording location latitude. Set -1 to ignore. Default = -1.}

\item{lon}{Recording location longitude Set -1 to ignore. Default = -1.}

\item{week}{Week of the year when the recording was made. Values in c(1:48) (4 weeks per month).
Set -1 for year-round species list. Default = -1.}

\item{sf.thresh}{Minimum species occurrence frequency threshold for location
filter. Values from 0.0001 to 0.99. Default = 0.03.}

\item{slist}{Absolute path to species list text file or folder. If
folder is provided, species list needs to be named "species_list.txt". If
lat and lon are provided, BirdNET-Analyzer will ignore this argument. See
\href{https://birdnet-team.github.io/BirdNET-Analyzer/best-practices/species-lists.html}{BirdNET-Analyzer documentation}
for details on formatting and creating a species list.}

\item{sensitivity}{Detection sensitivity. Higher values result in higher
sensitivity. Values from 0.5 to 1.5 for BirdNET Analyzer v1, and values
from 0.75 to 1.25 for BirdNET Analyzer v2. Default = 1.0.
Values other than 1.0 will shift the sigmoid function on the x-axis. Use
complementary to the cut-off threshold.}

\item{overlap}{Overlap of prediction segments in seconds. Values from 0.0 to 2.9. Default = 0.0.}

\item{audio.speed}{\strong{v2 ONLY}. Speed factor for audio playback. Values < 1.0 will slow
down the audio, values > 1.0 will speed it up. At a 10x decrease (audio speed 0.1),
a 384 kHz recording becomes a 38.4 kHz recording. Default = 1.0.}

\item{threads}{Number of CPU threads. Default = 2.}

\item{min.conf}{Minimum confidence threshold. Values from 0.00001 to 0.99.
Default = 0.1.}

\item{locale}{Locale for translated species common names. Values in \code{c("af", "en_UK", "de", "it", ...)}. Default = "en".}

\item{batchsize}{Number of samples to process at the same time. Default = 1.}

\item{rtype}{Specifies output format. \code{Default = "csv"}. For v1, values in \code{c("table", "audacity", "r", "kaleidoscope", "csv")}.
For v2, values in \code{c("table", "audacity", "kaleidoscope", "csv")} -- note that
\code{rtype = "r"} is no longer an option. Users who previously
processed data through BirdNET Analyzer v1 can retain downstream
functionality for BirdNET Analyzer v2 in NSNSDAcoustics by using \code{rtype = "csv"}
and \code{additional.columns = c("lat","lon","week", "overlap","sensitivity","min_conf","species_list","model")}}

\item{additional.columns}{\strong{v2 ONLY}. Additional columns to include in the output, only
available if using \code{rtype = "csv"}. Values in \code{c("lat","lon","week", "overlap","sensitivity","min_conf","species_list","model")}. The default and recommended behavior is to add all columns. Put NULL if no additional columns are desired.}

\item{combine.results}{\strong{v2 ONLY}. In addition to individual files, also outputs a combined file for all the
selected result types. Default = FALSE.}

\item{classifier}{Absolute path to custom trained classifier.
If using \code{classifier}, any inputs to \code{lat}, \code{lon}, and \code{locale} are ignored. Default = NULL.}

\item{skip.existing.results}{Skip files that have already been analyzed. Default = FALSE.}

\item{top.n}{Saves only the top N predictions for each segment independent of
their score. If used, \code{min.conf} threshold will be ignored.}

\item{merge.consecutive}{\strong{v2 ONLY}. Maximum number of consecutive detections above
\code{min.conf} to merge for each detected species. This will result in fewer entries
in the result file with segments longer than 3 seconds. Set to 0 or 1 to
disable merging. BirdNET Analyzer uses the mean of the top 3 scores from all
consecutive detections for merging. If you want to merge all consecutive detections,
try choosing a high numeric value.}
}
\value{
Saves a file of results for each audio file in \code{o.results}.
Depending on your BirdNET release version, if using \code{rtype = "r"} (v1)
or \code{rtype = "csv"} (v2), files are csv with suffix \code{"BirdNET.results.csv"}
or \code{"BirdNET.results.r.csv"}.

\strong{Output from V1:}

\itemize{
\item{\strong{filepath}: Filepath for the processed audio file.}
\item{\strong{start}: Start time of detection in seconds.}
\item{\strong{end}: End time of detection in seconds.}
\item{\strong{scientific_name}: Species scientific name.}
\item{\strong{common_name}: Species common name.}
\item{\strong{confidence}: BirdNET's confidence level in this detection ranging from 0 (least confident) to 1 (most confident).}
\item{\strong{lat}: Latitude input used.}
\item{\strong{lon}: Longitude input used.}
\item{\strong{week}: Week of year input used.}
\item{\strong{overlap}: Overlap input used.}
\item{\strong{sensitivity}: Sensitivity input used.}
\item{\strong{min_conf}: Minimum confidence input used.}
\item{\strong{species_list}: Species list used.}
\item{\strong{model}: BirdNET model used.}
}

\strong{Output from V2:} (optionally also includes lat, lon, week, overlap, sensitivity, min_conf,
species_list, and model, if indicated by \code{additional.columns} argument.)

\itemize{
\item{\strong{Start (s)}: Start time of detection in seconds.}
\item{\strong{End (s)}: End time of detection in seconds.}
\item{\strong{File}: Filepath for the processed audio file.}
\item{\strong{Scientific name}: Species scientific name.}
\item{\strong{Common name}: Species common name.}
\item{\strong{Confidence}: BirdNET's confidence level in this detection ranging from 0 (least confident) to 1 (most confident).}
}
}
\description{
Process audio files through
\href{https://github.com/kahst/BirdNET-Analyzer}{BirdNET-Analyzer} via R on Windows. To use this function, follow the steps
outlined in the \href{https://github.com/nationalparkservice/NSNSDAcoustics/blob/main/README.md#running-birdnet-from-rstudio}{NSNSDAcoustics ReadME}.
See BirdNET Analyzer \href{https://birdnet-team.github.io/BirdNET-Analyzer/usage/cli.html#birdnet-analyzer-analyze}{usage documentation}
for more details. BirdNET Analyzer introduced breaking
changes with an update to v2 in 2025. This function
attempts to retain capacity for both v1.5.1 and v2.1.1. Please
\href{https://github.com/nationalparkservice/NSNSDAcoustics/issues}{submit an issue}
if you discover problems.
}
\details{
This function was developed by the National Park Service Natural Sounds and
Night Skies Division to act as a wrapper to process audio data using BirdNET.

To use this function, follow the steps outlined in the
\href{https://github.com/nationalparkservice/NSNSDAcoustics/blob/main/README.md#running-birdnet-from-rstudio}{NSNSDAcoustics ReadME}.
Breaking changes were introduced in BirdNET Analyzer v2, and this function
attempts to retain capacity both for v1.5.1 and v2. Please
\href{https://github.com/nationalparkservice/NSNSDAcoustics/issues}{submit an issue}
if you discover problems.
}
\examples{
\dontrun{

# Create an audio directory for this example
dir.create('example-audio-directory')

# Read in example wave files
data(exampleAudio1)
data(exampleAudio2)

# Write example waves to example audio directory
tuneR::writeWave(
  object = exampleAudio1,
  filename = 'example-audio-directory/Rivendell_20210623_113602.wav'
)
tuneR::writeWave(
  object = exampleAudio2,
  filename = 'example-audio-directory/Rivendell_20210623_114602.wav'
)

# Create a BirdNET results directory for this example
dir.create('example-results-directory')

# Create a species_list.txt file for this example and write to working directory
data(exampleSpeciesList)
write.table(
   x = exampleSpeciesList,
   file = 'species_list.txt',
   row.names = FALSE,
   col.names = FALSE,
   quote = FALSE
)

##### The following example is pseudocode ######

# Because this function calls an external program (BirdNET-Analyzer.exe),
# the example function below will not be modifiable to run for you unless
# you follow the instructions given in the NSNSDAcoustics documentation here:
# https://github.com/nationalparkservice/NSNSDAcoustics/blob/main/README.md

# Run all audio data in a directory through BirdNET
birdnet_analyzer(
 birdnet.version = 'v2.1.1',
 birdnet.path = 'absolute/path/AppData/Local/Programs/BirdNET-Analyzer/BirdNET-Analyzer.exe',
 i.audio = 'absolute/path/example-audio-directory',
 o.results = 'absolute/path/example-results-directory',
 slist = 'absolute/path/species_list.txt',
)

# Delete all temporary example files when finished
unlink(x = 'example-audio-directory', recursive = TRUE)
unlink(x = 'example-results-directory', recursive = TRUE)

}
}
\seealso{
\code{\link{birdnet_format}}, \code{\link{birdnet_verify}}
}
