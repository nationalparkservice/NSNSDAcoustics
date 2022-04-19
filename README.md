# NSNSDAcoustics

This repository provides a place for NSNSD staff to develop and modernize several bioacoustics workflows. Contributors should clone this repository (set it up as an R project package) and connect it to GitHub (see [Happy Git with R](https://happygitwithr.com/) for tips). While the package is in development, use the Build > Install and Restart buttons in RStudio to install the package locally on your machine.

**All documentation and code is currently under development.**

# Table of Contents

- **[Installing NSNSDAcoustics](#installing-nsnsdacoustics)**
- **[Running BirdNET from RStudio with birdnet_run](#running-birdnet-from-rstudio-with-birdnet_run)**: Go here if you want to use RStudio to process .wav or .mp3 audio files through BirdNET. Warning: not for the fainthearted; requires a substantial amount of setup.
- **[Assessing BirdNET results](#assessing-birdnet-results)**: Go here if you already have raw BirdNET CSV outputs in hand and want to use R to wrangle, visualize, and verify the results.
  * **[Reformat raw BirdNET CSV results](#reformat-raw-birdnet-csv-results)**
  * **[Verify BirdNET results](#verify-birdnet-results)**
  * **[Visualize BirdNET detections](#visualize-birdnet-detections)**

- **[A few other convenience functions for BirdNET](#a-few-other-convenience-functions-for-birdnet)**
  * **[Gather up BirdNET CSV results](#gather-up-birdnet-csv-results)**
  * **[Summarize count data of detected species](#summarize-count-data-of-detected-species)**

- **[Converting wave audio files to NVSPL with wave_to_nvspl](#converting-wave-audio-files-to-nvspl-with-wave_to_nvspl)**: Go here for a user-friendly PAMGuide wrapper function to convert wave files to NVSPL.
- **[Converting NVSPL files to acoustic indices with nvspl_to_ai](#converting-nvspl-files-to-acoustic-indices-with-nvspl_to_ai)**: Go here to convert NVSPL.txt files into a CSV of acoustic indices.


## Installing NSNSDAcoustics

First, ensure that you have installed the latest versions of [R](https://cran.r-project.org/) and [RStudio](https://www.rstudio.com/products/rstudio/download/). 

Next, install NSNSDAcoustics using `install_github()` function from the R package `devtools`.

```r
devtools::install_github('nationalparkservice/NSNSDAcoustics')
```

If `install_github()` doesn't work, you can download the zip or tar.gz file directly using one of the following links. 
* Windows users can download the zip file from NSNSDAcoustics-master.zip [**need to create link**]()
* Mac or Linux users can download the tar.gz file from NSNSDAcoustcs-master.tar.gz [**need to create link**]()

After downloading, open R Studio, click on the Install button on the Packages tab, select Install From Package Archive File, and navigate to the downloaded file.

### A note on data.table syntax
NSNSDAcoustics depends on the R package `data.table`, which allows for fast querying and manipulation of large data.frames. If you are an R user but have never used `data.table` syntax before, some of the example code may look unfamiliar. Don't fret -- `data.table` object types are also  `data.frames`. If you get frustrated trying to work with them, you can always convert to a regular `data.frame` to work with a more familiar object type.


## Running BirdNET from RStudio with birdnet_run

`birdnet_run()` processes files through BirdNET by using the [reticulate](https://rstudio.github.io/reticulate/) package to run Python from RStudio. This function was developed for Windows 10 and has not been tested on other systems.

To use `birdnet_run()`, please first complete the following steps. The function will not work otherwise. 

(1) Install [BirdNET-Lite](https://github.com/kahst/BirdNET-Lite). If you're on a Windows machine, [see here](https://github.com/cbalantic/cbalantic.github.io/blob/master/_posts/2022-03-07-Install-BirdNET-Windows-RStudio.md#part-1-installing-birdnet-on-a-windows-machine).

(2) Set up [a conda environment for BirdNET](https://github.com/cbalantic/cbalantic.github.io/blob/master/_posts/2022-03-07-Install-BirdNET-Windows-RStudio.md#1-set-up-a-conda-environment)

Once you've completed those steps, here are few other tips for using `birdnet_run()`: 
* The function assumes that all files in a folder come from the same site, and that the audio files follow a SITEID_YYYYMMDD_HHMMSS.wav naming convention. If this is not the case for your files, you'll need to do some preprocessing.
* The function can handle either .wav or .mp3 audio file types. The function's current internal behavior for .mp3 files is to convert to a temporary wave file for processing, and then delete the temporary file when finished. This behavior may not be necessary on all platforms and Python / conda installations, but is might be necessary for Windows 10 if you followed the above instructions.
* The function expects absolute paths for all directory arguments in `birdnet_run()`. This is necessary due to the way RStudio is communicating with the underlying Python code. 
* Note that BirdNET's option to input a customized species list has not been implemented.

Below, we'll walk through the documentation and example helpfiles for `birdnet_run()`. Start by pulling up the function helpfile. Everything covered below is located in the "Examples" section of this helpfile. 

```r
?birdnet_run
```

Note: if you don't want to go to the trouble of installing BirdNET, you can still view example data to get an idea of the raw CSV outputs produced by BirdNET-Lite:
```r
# View examples of CSV outputs produced by birdnet_run(): 

data(exampleBirdNET1)
write.csv(x = exampleBirdNET1,
          file = 'BirdNET_Rivendell_20210623_113602.csv',
          row.names = FALSE)

data(exampleBirdNET2)
write.csv(x = exampleBirdNET2,
          file = 'BirdNET_Rivendell_20210623_114602.csv',
          row.names = FALSE)

```

If you **do** want to run BirdNET from R, the following pseudocode provides an outline for how to implement `birdnet_run()`. Because this function uses two external programs (Python and BirdNET-Lite), the examples below will not be modifiable to run for you unless you have installed BirdNET-Lite and set up a conda environment.

First, before even calling in the reticulate package, you need to use `Sys.setenv(RETICULATE_PYTHON = )` to point to your conda python.exe path. If you followed [these instructions](https://github.com/cbalantic/cbalantic.github.io/blob/master/_posts/2022-03-07-Install-BirdNET-Windows-RStudio.md#1-set-up-a-conda-environment), your conda python.exe path may look something like this: "C:/Users/Username/Anaconda3/envs/pybirdnet/python.exe"). You'll then invoke `use_condaenv()` to tell conda to use your pybirdnet conda environment. 
```r
# Must set environment BEFORE calling in the reticulate package
Sys.setenv(RETICULATE_PYTHON = "C:/Your/Python/Path/Here/python.exe")
library(reticulate)

# Set your conda environment
use_condaenv(condaenv = "pybirdnet", required = TRUE)
```

Next, we create a few example directories, one with sample wave audio files that come with the package, and another to collect your BirdNET CSV results. We read in the example audio files and write them to the example audio directory. This is meant to illustrate the file types and folder structure `birdnet_run()` expects to encounter.
```r
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
```

Once the example files are set up, we're ready to process audio files through BirdNET. `birdnet_run()` has several arguments. `audio.directory` takes a character string with the absolute path to audio files that should be processed. Files are expected to have the naming convention SITEID_YYYYMMDD_HHMMSS.wav. `birdnet_run()`'s default behavior is to process every file in the audio.directory through BirdNET. However, if the user only wants to process specific files, they can pass in a character vector to the argument `audio.files`. Next, `results.directory` takes an absolute path to the directory where you would like your BirdNET CSV results to be stored. `birdnet.directory` takes the absolute path to the directory where you have installed BirdNET on your machine. The remaining arguments allow you to customize the processing experience. `lat` and `lon` take numeric values of the latitude and longitude of the recording location, respecitvely. It is highly recommended to input latitude and longitude values, but these can be ignored by setting the argument value to -1. `ovlp` indicates the overlap in seconds between extracted spectrograms (accepts values from 0 to 2.9; default = 0.0). `sens`  is the detection sensitivity, with higher values resulting in higher sensitivity (i.e., more results). Sensitivity values may range from 0.5 to 1.5, and the default is 1.0. Finally, `min.conf` is the minimum confidence threshold to generate a detection, accepting values from 0.01 to 0.99 (default = 0.1). 

In the below example, once you modify the directory paths, `birdnet_run()` will process all example audio files in the folder, using default values for overlap, sensitivity, and minimum confidence threshold:
```r
# Run all audio data in a directory through BirdNET
birdnet_run(audio.directory = 'absolute/path/to/example-audio-directory',
            results.directory = 'absolute/path/to/example-results-directory',
            birdnet.directory = 'absolute/path/to/BirdNET',
            lat = 46.09924,
            lon = -123.8765)
```

For cases in which we only want to process selected audio files, we can use the `audio.files` argument to specify one or more files, as below: 
```r
# Use optional "audio.files" argument to process specific files
birdnet_run(audio.directory = 'absolute/path/to/example-audio-directory',
            audio.files = 'Rivendell_20210623_113602.wav',
            results.directory = 'absolute/path/to/example-results-directory',
            birdnet.directory = 'absolute/path/to/BirdNET',
            lat = 46.09924,
            lon = -123.8765)
```             

Finally, we clean up after ourselves by deleting temporary files that we set up for the example. 
```r
# Delete all temporary example files when finished
unlink(x = 'example-audio-directory', recursive = TRUE)
unlink(x = 'example-results-directory', recursive = TRUE)
```

You may not want to process files through RStudio, or you may already have BirdNET-Lite CSV results in hand that you would like to begin analyzing, in which case you can skip ahead to the next functions. 

## Assessing BirdNET Results

If you have a large number of audio files, and plan to monitor for a long time across many locations, you may quickly find yourself managing thousands of BirdNET CSVs. It's likely that you'll want a systematic way to track and check on these results, and verify whether BirdNET detections are truly from a target species. The `birdnet_format_csv()` --> `birdnet_verify()` workflow offers one way to keep track of your verifications. An alternative way would be to set up a SQLite database (e.g., [as used in the AMMonitor package](https://code.usgs.gov/vtcfwru/ammonitor/-/wikis/home)). Although a database solution may ultimately be the most robust way to track results through time in a long term project, this can come with a lot of start up and might not be easily extensible to your project needs. Instead, the solution below provides a simple way to reformat and work with the CSVs directly (`birdnet_format_csv()`), allowing you to store your verifications there (`birdnet_verify()`). Lastly, `birdnet_plot_detections()` provides a flexible way to visualize a subset of detected data, whether or not your have verifiied the detections. 

### Reformat raw BirdNET CSV results

`birdnet_format_csv()` is a simple function that reformats the raw BirdNET CSV into a new CSV with R-friendly column names, a "recordingID" column for easier data manipulation, a "verify" column to support manual verification of detection results, and a "timezone" column to clarify the timezone setting used by the audio recorder. 

Below, we'll walk through the documentation and example helpfiles for `birdnet_format_csv()`. It's always good practice to start by pulling up the function helpfile. Everything covered below is located in the "Examples" section of this helpfile. 

```r
?birdnet_format_csv
```

To run this example, we first create an example "results" directory, and then we write some raw BirdNET CSV results to this directory. This is meant to illustrate the file types and folder structure `birdnet_format_csv()` expects to encounter.
```r
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
```

Now, we're set up to run the function. `birdnet_format_csv()` takes two arguments. The `results.directory` should point to the folder where you have stored your raw BirdNET CSV outputs. **The `timezone` argument allows you to specify the timezone setting used in the audio recorder (i.e., the timezone reflected in the wave filename). It's important to pay attention to this and get it right!** Recall that the birdnet functions described here expect wave files that follow a SITEID_YYYYMMDD_HHMMSS.wav naming convention. In the package sample audio data, we have a wave named Rivendell_20210623_113602.wav. This means the site ID is Rivendell, and the recording was taken on June 23rd, 2021 at 11:36:02. The `timezone` argument allows us to clarify what 11:36:02 actually means. Was the recording taken in local time at your site, or was it taken in UTC? This point might seem trivial if you're just getting started with collecting data at a few sites for a single season, but if you're collecting data across many sites, over many years, with varying audio recorder equipment and varying recording settings through time, different field technicians, and potentially across timezones, you will want to keep meticulous track of your timezones so that you can make accurate comparisons across time and space. If recordings were taken in local time at your study site, specify an [Olson-names-formatted character timezone](https://en.wikipedia.org/wiki/List_of_tz_database_time_zones#List) for the location (e.g., "America/Los_Angeles"). If recordings were taken in UTC, you can put either "GMT" or "UTC" (both are acceptable in R for downstream date-time formatting). This argument is critical to foster clarity in data analysis through the years and across monitoring locations, because some projects may vary across time and space as to whether the standard operating procedure specifies recordings in UTC vs. local time.

Below, we point to our example results directory and specify 'GMT' (i.e., 'UTC') as the timezone, since the recordings were not taken in local time at this recorder.
```r
# Run birdnet_format_csv:
birdnet_format_csv(results.directory = 'example-results-directory',
                   timezone = 'GMT')
```

After running the function, look in your example results directory folder and check on the results. This function produces new formatted CSVs of BirdNET results with filename prefix "BirdNET_formatted_" (note that it does NOT overwrite your raw BirdNET CSVs). The columns in this formatted CSV can be described as follows.

* recordingID: Recording identifier for the file, as SITE_YYYYMMDD_HHMMSS.wav.
* start.s: Start time of detection in seconds.
* end.s: End time of detection in seconds.
* scientific.name: Species scientific name.
* common.name: Species common name.
* confidence: BirdNET's confidence level in this detection ranging from 0 (least confident) to 1 (most confident).
* verify: A column into which verifications may be populated. When initially created, will be 'NA'.
* timezone: Timezone setting used in the audio recorder.

Finally, we clean up after ourselves by deleting temporary files that we set up for the example. 
```r
# Delete all temporary example files when finished
unlink(x = 'example-results-directory', recursive = TRUE)
```

The point of all this CSV reformatting is to make it easier to keep track of our downstream analyses of BirdNET detections. The "verify" column is what allows us to track whether a detected event actually came from a target species. 

### Verify BirdNET results
`birdnet_verify()` allows the user to manually verify a selected subset of detections based on a user-input library of classification options.

Below, we'll walk through the documentation and example helpfiles for `birdnet_verify()`. As always, we start by pulling up the function helpfile. Everything covered below is located in the "Examples" section of this helpfile.

```r
?birdnet_verify
```

To run this example, we first create an example audio directory, to which we will write the sample audio that comes with the package. We'll also set up an example results directory to which we will write example formatted CSV data. This is meant to illustrate the file types and folder structure `birdnet_verify()` expects to encounter.

```r

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

# Write examples of formatted BirdNET CSV outputs to example results directory
data(exampleFormatted1)
write.csv(x = exampleFormatted1,
          file = 'example-results-directory/BirdNET_formatted_Rivendell_20210623_113602.csv',
          row.names = FALSE)

data(exampleFormatted2)
write.csv(x = exampleFormatted2,
          file = 'example-results-directory/BirdNET_formatted_Rivendell_20210623_114602.csv',
          row.names = FALSE)
```

Next, we can use `birdnet_gather_results()` to grab all the formatted results from the example folder. From here, you can manipulate your results however you want to create a subset of detections that you wish to verify. The key is to subset only to a single species. In case you accidentally include multiple species in your data subset, the `birdnet_verify()` function will remind you that it only accepts one species at a time for verifications. In this example, we'll focus on verifying detections for the [Swainson's Thrush (Catharus ustulatus)](https://www.allaboutbirds.org/guide/Swainsons_Thrush/sounds).

You can create your verification sample however you like. A few options are to take a simple random sample, or take a stratified sample based on detections from different locations or times of day. Depending on your question, you might even want to verify *everything* in your CSVs for the target species. In the below example, we set a seed for reproducibility and take a simple random sample of three Swainson's Thrush detections to be verified.
```r
# Gather formatted BirdNET results
dat <- birdnet_gather_results(results.directory = 'example-results-directory',
                             formatted = TRUE)

# Create a random sample of three detections to verify
set.seed(4)
to.verify <- dat[common.name == "Swainson's Thrush"][sample(.N, 3)]
```

The next step is to create a "verification library" for this species: essentially, a character vector of acceptable options for your verification labels. Verifying BirdNET detections may be tricky depending on your research question, because BirdNET does not distinguish between the different types of vocalizations a bird may produce. This means the burden is on you, the verifier, to label the detection in a way that will best support you in answering your motivating research question. 

The Swainson's Thrush provides a good example of what makes this challenging, because in addition to its recognizable flutelike song, it has a variety of different call types, including a "peep" note, a "whit", a high-pitched "whine", a "bink", and a "peeer" call. 

Thus, the verification library you set up will depend on the level of detail you need to answer your research question. Here are two examples of questions you might have as you verify. 
* **Is this a Swainson's Thrush or not?** For this question, you might just want to label a detection as "Yes, this is my target species!" or "No, this definitely isn't my target species", or "I'm not sure". You might choose simple verification labels like c('y', 'n', 'unsure').
* **What type of Swainson's Thrush vocalization is this?** For this question, you might choose more descriptive verification labels like c('song', 'call', 'unsure', 'false'), or something even more detailed like c('song', 'peep', 'whit', 'whine', 'bink', 'peeer', 'false', 'unsure'). 

This part is left to your discretion. It's one of the challenging aspects of assessing automated detection results, and you may find that you need to iterate through a few options before settling on the verification library that will work best for a given species and research question.

Below, we'll use a simple verification library where 'y' means yes, it's a Swainson's Thrush, 'n' means it's not, and 'unsure' means we aren't certain. 
```r
# Create a verification library for this species
ver.lib <- c('y', 'n', 'unsure')
```

Once we have the data.frame or data.table of detections we want to verify, and once we've set up a verification library, we're ready to use `birdnet_verify()`. This interactive function displays a spectrogram of the detected event and prompts the user to label it with one of the input options defined in the verification library. The function also optionally writes a temporary wave clip to your working directory that you can listen to. (Although there are options for playing a sound clip automatically from R, the behavior of these options varies enough across platforms/operating systems that I decided it would be simpler to write a temporary wave clip file for the user). If you expect to be listening to the clips, you'll want easy access to your working directory so that you can open the wave clips up manually. 

`birdnet_verify()` has several arguments. `data` takes your data.frame or data.table of detections you want to verify. Data must be formatted according to `birdnet_format_csv()` and contain columns named recordingID, start.s, end.s, scientific.name, common.name, confidence, verify, and timezone. `verification.library` takes a character vector of the labeling options you will be using. `audio.directory` should point to the directory where your audio files live, and `results.directory` points to the directory where your formatted BirdNET CSVs are stored. `overwrite` allows you to decide whether or not any previously existing verifications should be overwritten. When FALSE, users will not be prompted to verify any detected events that already have a verification, but when TRUE, you may be overwriting previous labels. The `play` argument specifies whether or not a temporary wave file should be written to the working directory for each detection. The remaining arguments allow the user to customize how detected events should be displayed during the interactive session. We do not review these options here; see `?birdnet_verify` for details.
```r
# Verify detections
birdnet_verify(data = to.verify,
               verification.library = ver.lib,
               audio.directory = 'example-audio-directory',
               results.directory = 'example-results-directory',
               overwrite = FALSE, 
               play = TRUE,
               frq.lim = c(0, 12),
               buffer = 1,
               box.col = 'blue',
               spec.col = monitoR::gray.3())
```

When you run this example, the RStudio console will prompt you to provide a label for the detection. The plot pane will display a spectrogram of the detection. You'll use this spectrogram -- optionally along with the temporary wave file -- to decide how to label the detection. In this example, we've chosen to place a 1 second buffer around the detection to provide additional visual and acoustic context. The detection itself is contained within the blue box (all BirdNET detections occur in three-second chunks). About 23.5 seconds in, a Swainson's Thrush begins singing. The spectrogram title gives us information about where we can find this detection in the CSV file, and tells us that BirdNET had a confidence level of 0.43 for the detection. We can label this as 'y' because the blue detection window does contain a Swainson's Thrush vocalization. 

<img src=https://github.com/dbetchkal/NMSIM-Python/blob/pyproj_1p9/static/2020%2010%2022%20NMSIM%20source%20improvement%20schema.png width=700><br>

Once you've added labels for the remaining detections (in fact, they all contain Swainson's Thrush vocalizations!), `birdnet_verify()` will update your verifications to the underlying formatted CSVs contained in the example results directory. Below, we gather up the results again and check that our three verifications have been added.

```r
# Check that underlying CSVs have been updated with user verifications
dat <- birdnet_gather_results(results.directory = 'example-results-directory',
                              formatted = TRUE)
dat[!is.na(verify)]
```

Finally, we clean up after ourselves by deleting temporary files that we set up for the example.
```r
# Delete all temporary example files when finished
unlink(x = 'example-audio-directory', recursive = TRUE)
unlink(x = 'example-results-directory', recursive = TRUE)

```


### Visualize BirdNET detections
`birdnet_plot_detections()` allows the user to visualize data...


## A few other convenience functions for BirdNET

### Gather up BirdNET CSV results
`birdnet_gather_results()` gathers all BirdNET CSV results from a desired folder into a user-friendly data.table / data.frame

### Summarize count data of detected species
`birdnet_species_counts()` summarizes count data from a data.table of detected species over a selected time unit


## Converting wave audio files to NVSPL with wave_to_nvspl

Define NVSPLs. What are they and why do we convert to them?

First, pull up the documentation file for this function:
```r
?wave_to_nvspl
```

`wave_to_nvspl()` uses PAMGuide code to convert wave files into NVSPL format. PAMGuide was developed by Nathan D. Merchant et al. 2015 (**ADD LINKS**). The suggested workflow for this function is to first set test.file = TRUE to test that your workflow has been accurately parameterize. Next, to batch process NVSPLs for many audio files, run with test.file = FALSE.

```r
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

# Perform wave_to_nvspl in test mode (test.file = TRUE)
wave_to_nvspl(
 input.directory = 'example-input-directory',
 data.directory = FALSE,
 test.file = TRUE,
 project = 'testproject',
 timezone = 'GMT')

# Perform wave_to_nvspl in batch mode (test.file = FALSE)
wave_to_nvspl(
 input.directory = 'example-input-directory',
 data.directory = FALSE,
 test.file = FALSE,
 project = 'testproject',
 timezone = 'GMT')

# Verify that NVSPL outputs have been created
nvspls <- list.files('example-input-directory/NVSPL', full.names = TRUE)

# View one of the NVSPL outputs
one.nvspl <- read.delim(file = nvspls[1], sep = ',')

# Delete all temporary example files when finished
unlink(x = 'example-input-directory', recursive = TRUE)

```

## Converting NVSPL files to acoustic indices with nvspl_to_ai

Explain about 1/3 octave bands.
Why use this wave --> NVSPL --> acoustic indices workflow instead of using existing acoustic index functions that take wave data directly?

First, pull up the documentation file for this function:
```r
?nvspl_to_ai
```

`nvspl_to_ai()` converts NVSPLs (generated by PAMGuide via `wave_to_nvspl`) into a range of acoustic indices. The examples below demonstrate how to use this function. When the start.at.beginning argument is set to TRUE (**DESCRIBE**); when start.at.beginning = FALSE (**DESCRIBE**)

```r

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

# Run nvspl_to_ai to generate acoustic indices csv for example NVSPL files,
# setting start.at.beginning = FALSE
nvspl_to_ai(input.directory = 'example-input-directory',
            output.directory = 'example-output-directory',
            project = 'example-project',
            start.at.beginning = FALSE)

# Run nvspl_to_ai to generate acoustic indices csv for example NVSPL files,
# setting start.at.beginning = TRUE
nvspl_to_ai(input.directory = 'example-input-directory',
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


```
