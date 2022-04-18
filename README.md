# NSNSDAcoustics

This repository provides a place for NSNSD staff to develop and modernize several bioacoustics workflows. Contributors should clone this repository (set it up as an R project package) and connect it to GitHub (see [Happy Git with R](https://happygitwithr.com/) for tips). While the package is in development, use the Build > Install and Restart buttons in RStudio to install the package locally on your machine.

**All documentation and code is currently under development.**

# Table of Contents

- **[Installing NSNSDAcoustics](#installing-nsnsdacoustics)**
- **[Running BirdNET from RStudio with birdnet_run](#running-birdnet-from-rstudio-with-birdnet_run)**: Go here if you want to use RStudio to process .wav or .mp3 audio files through BirdNET. Warning: not for the fainthearted; requires a substantial amount of setup.
- **[Assessing BirdNET results](#assessing-birdnet-results)**: Go here if you already have raw BirdNET CSV outputs in hand and want to use R to wrangle, visualize, and verify the results.
  * **[Reformat raw BirdNET CSV results](#reformat-raw-birdnet-csv-results)**
  * **[Gather up BirdNET CSV results](#gather-up-birdnet-csv-results)**
  * **[Summarize count data of detected species](#summarize-count-data-of-detected-species)**
  * **[Verify BirdNET results](#verify-birdnet-results)**
  * **[Visualize BirdNET detections](#visualize-birdnet-detections)**
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

Once the example files are set up, we're ready to process audio files through BirdNET. `birdnet_run()` has several arguments. `audio.directory` takes a character string with the aboslute path to audio files that should be processed. Files are expected to have the naming convention SITEID_YYYYMMDD_HHMMSS.wav. `birdnet_run()`'s default behavior is to process every file in the audio.directory through BirdNET. However, if the user only wants to process specific files, they can pass in a character vector to the argument `audio.files`. Next, `results.directory` takes an absolute path to the directory where you would like your BirdNET CSV results to be stored. `birdnet.directory` takes the absolute path to the directory where you have installed BirdNET on your machine. The remaining arguments allow you to customize the processing experience. `lat` and `lon` take numeric values of the latitude and longitude of the recording location, respecitvely. It is highly recommended to input latitude and longitude values, but these can be ignored by setting the argument value to -1. `ovlp` indicates the overlap in seconds between extracted spectrograms (accepts values from 0 to 2.9; default = 0.0). `sens`  is the detection sensitivity, with higher values resulting in higher sensitivity (i.e., more results). Sensitivity values may range from 0.5 to 1.5, and the default is 1.0. Finally, `min.conf` is the minimum confidence threshold to generate a detection, accepting values from 0.01 to 0.99 (default = 0.1). 

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

`birdnet_format_csv()` reformats the raw BirdNET CSV into 

### Verify BirdNET results
`birdnet_verify()` allows the user to manually verify a selected subset of detections based on a user-input library of classification options 

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
