# NSNSDAcoustics

This repository provides a place for National Park Service [Natural Sounds and Night Skies Division (NSNSD)](https://www.nps.gov/orgs/1050/index.htm) staff to develop and modernize several bioacoustics workflows. 

**All documentation and code is actively under development. There is currently no official release of this package and code may change. If you encounter a problem, please submit it to [Issues](https://github.com/nationalparkservice/NSNSDAcoustics/issues). If you have a question or need that isn't covered by submitting an issue, please reach out to Cathleen Balantic (`cathleen_balantic` at `nps.gov`).**

# Table of Contents

- **[Installing NSNSDAcoustics](#installing-nsnsdacoustics)**
- **[Running BirdNET from RStudio with birdnet_analyzer](#running-birdnet-from-rstudio-with-birdnet_analyzer)**: Go here if you want to use RStudio to process .wav or .mp3 audio files through [BirdNET](https://birdnet.cornell.edu/). Requires some setup. 
- **[Assessing BirdNET results](#assessing-birdnet-results)**: Go here if you already have raw BirdNET outputs in hand from rtype = 'r', and want to use R to wrangle, visualize, and verify the results.
  * **[Reformat raw BirdNET results](#reformat-raw-birdnet-results)**
  * **[Gather BirdNET results](#gather-birdnet-results)**
  * **[Verify BirdNET results](#verify-birdnet-results)**
  * **[Visualize BirdNET spectrograms](#visualize-birdnet-spectrograms)**
  * **[Create barcharts of BirdNET detections](#create-barcharts-of-birdnet-detections)**

- **[Converting wave audio files to NVSPL tables with wave_to_nvspl](#converting-wave-audio-files-to-nvspl-tables-with-wave_to_nvspl)**: Go here for a PAMGuide wrapper function to convert wave files to NVSPL formatted tables.
- **[Converting NVSPL files to acoustic indices with nvspl_to_ai](#converting-nvspl-files-to-acoustic-indices-with-nvspl_to_ai)**: Go here to convert NVSPL.txt files into a CSV of acoustic indices.


## Installing NSNSDAcoustics

First, ensure that you have installed the latest versions of [R](https://cran.r-project.org/) and [RStudio](https://www.rstudio.com/products/rstudio/download/). 

Next, you can install NSNSDAcoustics using one of two options: 

### (1) Option 1: Use `install_github()`:

You may need to first install the latest version of devtools, and the R console may also prompt you to install Rtools (follow directions given in console message). If you are having trouble installing devtools, make sure to disconnect from VPN. Once you have Rtools and devtools, you can install the latest version of NSNSDAcoustics:

```r
install.packages('devtools')
library(devtools)
devtools::install_github('nationalparkservice/NSNSDAcoustics')
```

### (2) Option 2: Download and install manually
**Note: this option will not be implemented until we have a first stable release of NSNSDAcoustics.**

Eventually, you will be able to download the zip or tar.gz file directly using one of the following links. 
* Windows users can download the zip file from NSNSDAcoustics-master.zip [**need to create link**]()
* Mac or Linux users can download the tar.gz file from NSNSDAcoustcs-master.tar.gz [**need to create link**]()

After downloading, open R Studio, click on the Install button on the Packages tab, select Install From Package Archive File, and navigate to the downloaded file.

Once NSNSDAcoustics is installed, you can call in the package and look at all helpfiles: 

```r
library(NSNSDAcoustics)
help(package = 'NSNSDAcoustics')
```


### A note on data.table syntax
NSNSDAcoustics depends on the R package `data.table`, which enables fast querying and manipulation of large data.frames. If you are an R user but have never used `data.table` syntax before, some of the example code may look unfamiliar. Don't fret -- `data.table` object types are also  `data.frames`. If you get frustrated trying to work with them, you can always convert to a regular `data.frame` to deal with a more familiar object type.


## Running BirdNET from RStudio with birdnet_analyzer

[BirdNET](https://birdnet.cornell.edu/) is a bird sound recognition program developed by the [Cornell Center for Conservation Bioacoustics](https://www.birds.cornell.edu/ccb/). The [BirdNET-Analyzer Github repository](https://github.com/kahst/BirdNET-Analyzer) provides a promising free tool for processing large volumes of audio data relatively quickly and understanding something about which avian species are present.

As an R user, you may prefer to run BirdNET directly from RStudio. To process audio files through BirdNET with RStudio, `birdnet_analyzer()` uses the [reticulate](https://rstudio.github.io/reticulate/) package to run Python from RStudio. **This function was developed for Windows 10 and has not been tested on other systems.** P.S.: As an alternative to `birdnet_analyzer()`, you may find that you prefer to run BirdNET directly from the command line, as described at the [BirdNET-Analyzer Github repository](https://github.com/kahst/BirdNET-Analyzer). This may be faster since you will be able to specify multiple threads, which likely won't work via R due to the way Python and R interact. If you choose to run BirdNET from the command line instead of from `birdnet_analyzer()`, you will still be able to use other functions in this package to gather and visualize BirdNET results, **so long as you have specified --rtype "r"**.

If you prefer to use `birdnet_analyzer()`, please first complete the following steps. The function will not work otherwise. 

### (1) Install BirdNET using the "Install BirdNET from zip" instructions at [BirdNET-Analyzer -- Setup (Windows)](https://github.com/kahst/BirdNET-Analyzer#setup-windows). 

Early on in the [Setup (Windows)](https://github.com/kahst/BirdNET-Analyzer#setup-windows) section, the instructions will encourage you to download a "fully-packaged version that does not require you to install any additional packages and can be run as-is". This is *not* the file you want, so keep scrolling until you hit a section that says "Install BirdNET from zip" and click "Download BirdNET Zip-file". Unzip that file to a desired location on your machine. This folder should have the name **BirdNET-Analyzer-main**.

Check the image below to make sure you're installing the correct version: 

<p align="center">
<img src=https://github.com/nationalparkservice/NSNSDAcoustics/blob/main/images/BirdNET-install-link.png alt="Version of BirdNET you should install if you want to run BirdNET from RStudio."><br>
</p>


### (2) Download and install [Anaconda](https://www.anaconda.com/). 

BirdNET-Analyzer runs on Python, and installing Anaconda will position you for the streamlined setup and dependency management necessary to run BirdNET-Analyzer from R.

### (3) Set up a conda environment for BirdNET-Analyzer

Python setup and package dependency management is a little different from what you might be used to as an R user. In this workflow, you will use something called a conda environment to manage and isolate the package dependencies needed to run BirdNET-Analyzer from R. Learn more about conda environments [here](https://conda.io/projects/conda/en/latest/user-guide/tasks/manage-environments.html#creating-an-environment-with-commands) and [here](https://towardsdatascience.com/a-guide-to-conda-environments-bc6180fc533).

Set up your BirdNET conda environment by opening up an Anaconda prompt. In the Windows search bar, type and click “Anaconda Prompt (Anaconda3)”. Run the following commands to install all the necessary packages you’ll need to run BirdNET within your conda environment. Below, we are creating a conda environment named "pybirdanalyze", and then installing necessary packages to it. 

```python
conda create --name pybirdanalyze
conda install -n pybirdanalyze -c conda-forge librosa numpy=1.20
conda install -n pybirdanalyze tensorflow 
```

Make sure you know where your pybirdanalyze conda environment lives. It might have a path name like: C:\\Users\\Username\\Anaconda3\\envs\\pybirdanalyze

The contents of your conda folder should look something like this: 

<p align="center">
<img src=https://github.com/nationalparkservice/NSNSDAcoustics/blob/main/images/BirdNET-conda-folder.png alt="The contents of your conda folder should look something like this."><br>
</p>


### (4) Copy the "checkpoints" folder and "eBird_taxonomy_codes_2021E.json" file from BirdNET-Analyze-main into your BirdNET conda environment folder.

If you don't do this, `birdnet_analyzer()` will throw errors telling you that it can't find these files. 

### (5) Process audio data using `birdnet_analyzer()`. 

Here are few tips for using this function: 

* The function assumes that all files in a folder come from the same site, and that the audio files follow a SITEID_YYYYMMDD_HHMMSS naming convention. If this is not the case for your files (e.g., AudioMoth files do not come with a SiteID), you'll need to do some preprocessing.
* Cornell's underlying BirdNET-Analyzer software gives several options for file output types, but the only one implemented in this function is 'r'. See `?birdnet_analyzer` for output column details. 
* The function can handle either .wav or .mp3 audio file types. The current internal behavior for .mp3 files is to convert to a temporary wave file for processing, and then delete the temporary file when finished. This behavior may not be necessary on all platforms and Python / conda installations, but might be necessary for Windows 10 if you followed the above instructions.
* The function expects absolute paths for all directory arguments in `birdnet_analyzer()`. This is necessary due to the way RStudio is communicating with the underlying Python code. 

Below, we'll walk through the documentation and example helpfiles for `birdnet_analyzer()`. Start by pulling up the function helpfile. Everything covered below is located in the "Examples" section of this helpfile. 

```r
?birdnet_analyzer
```

Note: if you don't want to go to the trouble of installing and setting up BirdNET, you can still view example data to get an idea of the raw outputs produced by BirdNET-Analyzer
```r
# To view example outputs of raw txt BirdNET results, write to working directory
data(exampleBirdNET1)
write.table(x = exampleBirdNET1,
            file = 'Rivendell_20210623_113602.BirdNET.results.csv',
            row.names = FALSE, quote = FALSE, sep = ',')
data(exampleBirdNET2)
write.table(x = exampleBirdNET2,
            file = 'Rivendell_20210623_114602.BirdNET.results.csv',
            row.names = FALSE, quote = FALSE, sep = ',')
```

If you **do** want to run BirdNET from R, the following pseudocode provides an outline for how to implement `birdnet_analyzer()`. Because this function uses external programs, the examples below will not be modifiable to run for you unless you have followed the setup instructions above.

First, before even calling in the reticulate package, you need to use `Sys.setenv(RETICULATE_PYTHON = )` to point to your conda python.exe path. Your conda python.exe path may look something like this: "C:/Users/Username/Anaconda3/envs/pybirdanalyze/python.exe". You'll then invoke `use_condaenv()` to tell conda to use the pybirdanalyze conda environment. 

```r
# Must set environment BEFORE calling in the reticulate package
Sys.setenv(RETICULATE_PYTHON = "C:/Your/Python/Path/Here/python.exe")
library(reticulate)

# Set your conda environment
use_condaenv(condaenv = "pybirdanalyze", required = TRUE)
```

Next, we create a few example directories, one with sample wave audio files that come with the package, and another to collect your BirdNET results. We read in the example audio files and write them to the example audio directory. This setup illustrates the file types and folder structure `birdnet_analyzer()` expects to encounter.
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

Once the example files are set up, we're ready to process audio files through BirdNET. `birdnet_analyzer()` has several arguments. `audio.directory` takes a character string with the absolute path to audio files that should be processed. Files are expected to have the naming convention SITEID_YYYYMMDD_HHMMSS with a wav or mp3 extension. `birdnet_analyzer()`'s default behavior is to process every file in the audio.directory through BirdNET. However, if the user only wants to process specific files, they can pass in a character vector to the argument `audio.files` **or** use the `start` argument to specify a file number to start with. Next, `results.directory` takes an absolute path to the directory where you would like your BirdNET results to be stored. `birdnet.directory` takes the absolute path to the directory where you have installed BirdNET on your machine. The remaining arguments allow you to customize the processing experience. `use.week`	is a logical flag for whether to use week of year in the prediction. If use.week = TRUE, the behavior of `birdnet_analyzer()` is to parse the week of year from the SITEID_YYYYMMDD_HHMMSS filename using lubridate::week(). If FALSE, birdnet_analyzer() will not consider the week of the year when making predictions. `lat` and `lon` take numeric values of the latitude and longitude of the recording location, respectively. You can ignore latitude and longitude values by setting the argument value to -1. A customized species list can be used with the `slist` argument. Please consult the helpfile or [BirdNET-Analyzer](https://github.com/kahst/BirdNET-Analyzer) documentation for details. 

In the below example, once you modify the directory paths, `birdnet_analyzer()` will process all example audio files in the folder, with user-input values for lat and lon, and default values for remaining arguments:
```r
# Run all audio data in a directory through BirdNET
birdnet_analyzer(audio.directory = 'absolute/path/example-audio-directory',
                 results.directory = 'absolute/path/example-results-directory',
                 birdnet.directory = 'absolute/path/BirdNET-Analyzer-main',
                 use.week = TRUE,
                 lat = 46.09924,
                 lon = -123.8765)
```

For cases in which we only want to process selected audio files, we can use the `audio.files` argument to specify one or more files, as below: 
```r
# Use optional "audio.files" argument to process specific files
birdnet_analyzer(audio.directory = 'absolute/path/example-audio-directory',
                 audio.files = 'Rivendell_20210623_113602.wav',
                 results.directory = 'absolute/path/example-results-directory',
                 birdnet.directory = 'absolute/path/BirdNET-Analyzer-main',
                 use.week = TRUE,
                 lat = 46.09924,
                 lon = -123.8765)
```             

Finally, clean up by deleting temporary files that were set up for the example. 
```r
# Delete all temporary example files when finished
unlink(x = 'example-audio-directory', recursive = TRUE)
unlink(x = 'example-results-directory', recursive = TRUE)
```

You may not want to process files through RStudio, or you may already have BirdNET-Analyzer txt or csv results in hand that you would like to begin analyzing, in which case you can skip ahead to the next functions. 

### (6) Keep BirdNET Updated (...if you wish)

[The BirdNET-Analyzer model is periodically updated](https://github.com/kahst/BirdNET-Analyzer/tree/main/checkpoints). NSNSDAcoustics supports V2.1 and V2.2. If you have previously been running V2.1 through NSNSDAcoustics, and want to update to V2.2, you will need to redo steps 1, 3, and 4 above. Preliminary investigation suggests that V2.1 and V2.2 produce different results, so if you are intending to apply an updated model to new incoming audio and then compare those results to older audio that was processed with an older BirdNET-Analyzer model, beware that this may change results and inference.

## Assessing BirdNET Results

If you have a large number of audio files, and plan to monitor for a long time across many locations, you may very quickly find yourself managing thousands of BirdNET output files. It's likely that you'll want a systematic way to track and check on these results, and verify whether BirdNET detections are truly from a target species. The `birdnet_format()` --> `birdnet_verify()` workflow offers one way to keep track of your verifications. An alternative way would be to set up a SQLite database (e.g., [as used in the AMMonitor package](https://code.usgs.gov/vtcfwru/ammonitor/-/wikis/home)). Although a database solution may ultimately be the most robust way to track results through time in a long term project, this can come with a lot of start up and might not be easily extensible to your project needs. Instead, the worfklow below provides a simple way to reformat and work with BirdNET output files directly, allowing you to store your verifications there. Lastly, `birdnet_plot()` and `birdnet_barchart()` provide plotting options to visualize detected data.

### Reformat raw BirdNET results

`birdnet_format()` is a simple function that reformats the raw BirdNET txt or csv results with a "recordingID" column for easier data manipulation, a "verify" column to support manual verification of detection results, and a "timezone" column to clarify the timezone setting used by the audio recorder. 

Below, we'll walk through the documentation and example helpfiles for `birdnet_format()`. Always start by pulling up the function helpfile. Everything covered below is located in the "Examples" section of this helpfile. 

```r
?birdnet_format
```

To run this example, we first create an example "results" directory, and then we write some raw BirdNET txt results to this directory. This setup illustrates the file types and folder structure `birdnet_format()` expects to encounter.
```r
# Create a BirdNET results directory for this example
dir.create('example-results-directory')

# Write examples of raw BirdNET outputs to example results directory
data(exampleBirdNET1)
write.table(x = exampleBirdNET1,
            file = 'example-results-directory/Rivendell_20210623_113602.BirdNET.results.csv',
            row.names = FALSE, quote = FALSE, sep = ',')
data(exampleBirdNET2)
write.table(x = exampleBirdNET2,
            file = 'example-results-directory/Rivendell_20210623_114602.BirdNET.results.csv',
            row.names = FALSE, quote = FALSE, sep = ',')
```

Now, we're set up to run the function. `birdnet_format()` takes two arguments. First, the `results.directory` should point to the folder where you have stored your raw BirdNET outputs. Second, the `timezone` argument allows you to specify the timezone setting used in the audio recorder (i.e., the timezone reflected in the wave filename). **It's important to pay attention to this!** Recall that the functions described here expect wave files that follow a SITEID_YYYYMMDD_HHMMSS naming convention. In the package sample audio data, we have a wave named Rivendell_20210623_113602.wav. This means the site ID is Rivendell, and the recording was taken on June 23rd, 2021 at 11:36:02. The `timezone` argument allows us to clarify what 11:36:02 actually means. Was the recording taken in local time at your site, or was it taken in UTC? This point might seem trivial if you're just getting started with collecting data at a few sites for a single season, but if you're collecting data across many sites, over many years, with varying audio recorder equipment and varying recording settings through time, different field technicians, and potentially across timezones (as we often do at NPS NSNSD), you will want to keep meticulous track of your timezones so that your analyses will be accurate across time and space. If recordings were taken in local time at your study site, specify an [Olson-names-formatted character timezone](https://en.wikipedia.org/wiki/List_of_tz_database_time_zones#List) for the location (e.g., "America/Los_Angeles"). If recordings were taken in UTC, you can put either 'GMT' or 'UTC' (both are acceptable in R for downstream date-time formatting). 

Below, we point to our example results directory and specify 'GMT' (i.e., 'UTC') as the timezone, since the recordings were not taken in local time at this recorder.
```r
# Run birdnet_format:
birdnet_format(results.directory = 'example-results-directory',
               timezone = 'GMT')
```

After running the function, look in your example results directory folder and check on the results. This function produces new formatted files with filename prefix "BirdNET_formatted_" (note that it does NOT overwrite your raw BirdNET results). The columns in this formatted file are described in the helpfile. 

Finally, clean up by deleting temporary files that were set up for the example. 
```r
# Delete all temporary example files when finished
unlink(x = 'example-results-directory', recursive = TRUE)
```

The point of all this reformatting is to make it easier to keep track of our downstream analyses of BirdNET detections. The "verify" column is what allows us to track whether a detected event actually came from a target species. 

### Gather BirdNET results
`birdnet_gather()` is a simple convenience function that gathers all BirdNET results from a desired folder into one user-friendly data.table / data.frame. View the helpfile for more information and code examples (`?birdnet_gather`). 

The function allows you to gather either unformatted (raw) or formatted data. For this example, we set up an example results directory and write both formatted and unformatted .txt results to it. This illustrates the type of folder and file structure `birdnet_gather()` expects to encounter.

```r
# Create a BirdNET results directory for this example
dir.create('example-results-directory')

# Write examples of formatted BirdNET outputs to example results directory
data(exampleFormatted1)
write.table(x = exampleFormatted1,
            file = 'example-results-directory/Rivendell_20210623_113602.BirdNET_formatted_results.csv',
            row.names = FALSE, quote = FALSE, sep = ',')

data(exampleFormatted2)
write.table(x = exampleFormatted2,
            file = 'example-results-directory/Rivendell_20210623_114602.BirdNET_formatted_results.csv',
            row.names = FALSE, quote = FALSE, sep = ',')

# Write examples of raw BirdNET outputs to example results directory
data(exampleBirdNET1)
write.table(x = exampleBirdNET1,
            file = 'example-results-directory/Rivendell_20210623_113602.BirdNET.results.csv',
            row.names = FALSE, quote = FALSE, sep = ',')
data(exampleBirdNET2)
write.table(x = exampleBirdNET2,
            file = 'example-results-directory/Rivendell_20210623_114602.BirdNET.results.csv',
            row.names = FALSE, quote = FALSE, sep = ',')

```

`birdnet_gather()` takes two arguments: `results.directory` (the file path to the directory where BirdNET results are stored) and `formatted`, a logical indicating whether formatted results should be gathered. If TRUE, formatted results are gathered. If FALSE, unformatted (raw) BirdNET results are gathered. Both options are demonstrated below. 
```r
# Gather formatted BirdNET results
formatted.results <- birdnet_gather(
                             results.directory = 'example-results-directory',
                             formatted = TRUE
                             )

# Gather unformatted (raw) BirdNET results
raw.results <- birdnet_gather(
                       results.directory = 'example-results-directory',
                       formatted = FALSE
                       )
```

Finally, we delete all example files when finished. 
```r
# Delete all temporary example files when finished
unlink(x = 'example-results-directory', recursive = TRUE)
```

### Verify BirdNET results
`birdnet_verify()` allows the user to manually verify a selected subset of detections based on a user-input library of classification options.

Below, we'll walk through the documentation and example helpfiles for `birdnet_verify()`. As always, we start by pulling up the function helpfile. Everything covered below is located in the "Examples" section of this helpfile.

```r
?birdnet_verify
```

To run this example, we first create an example audio directory, to which we will write the sample audio that comes with the package. We'll also set up an example results directory to which we will write example formatted .txt data. This is meant to illustrate the file types and folder structure `birdnet_verify()` expects to encounter.

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

# Write examples of formatted BirdNET outputs to example results directory
data(exampleFormatted1)
write.table(x = exampleFormatted1,
            file = 'example-results-directory/Rivendell_20210623_113602.BirdNET_formatted_results.csv',
            row.names = FALSE, quote = FALSE, sep = ',')
data(exampleFormatted2)
write.table(x = exampleFormatted2,
            file = 'example-results-directory/Rivendell_20210623_114602.BirdNET_formatted_results.csv',
            row.names = FALSE, quote = FALSE, sep = ',')
```

Next, we can use `birdnet_gather()` to grab all the formatted results from the example folder. From here, you can manipulate your results however you want to create a subset of detections that you wish to verify. The key is to subset only to a single species. In case you accidentally include multiple species in your data subset, `birdnet_verify()` will remind you that it only accepts one species at a time for verifications. In this example, we'll focus on verifying detections for the [Swainson's Thrush (Catharus ustulatus)](https://www.allaboutbirds.org/guide/Swainsons_Thrush).

You can create your verification sample however you like. A few options are to take a simple random sample, or take a stratified sample based on detections from different locations or times of day. Depending on your question, you might even want to verify *every* detection for your target species. In the below example, we set a seed for reproducibility and take a simple random sample of three Swainson's Thrush detections.
```r
# Gather formatted BirdNET results
dat <- birdnet_gather(results.directory = 'example-results-directory',
                      formatted = TRUE)

# Create a random sample of three detections to verify
set.seed(4)
to.verify <- dat[common_name == "Swainson's Thrush"][sample(.N, 3)]
```

The next step is to create a "verification library" for this species; essentially, a character vector of acceptable options for your verification labels. Verifying BirdNET detections may be tricky depending on your research question, because BirdNET does not distinguish between the different types of vocalizations a bird may produce. This means the burden is on you, the verifier, to label the detection in a way that will best support you in answering your motivating research question. 

The Swainson's Thrush provides a good example of what makes this challenging, because in addition to its [recognizable flutelike song, it has a variety of different call types](https://www.allaboutbirds.org/guide/Swainsons_Thrush/sounds), including a "peep" note, a "whit", a high-pitched "whine", a "bink", and a "peeer" call. 

Thus, the verification library you set up will depend on the level of detail you need to answer your research question. Here are two examples of questions you might have as you verify:
* **Is this a Swainson's Thrush or not?** For this question, you'll think to yourself, "Yes, this is my target species!" or, "No, this definitely isn't my target species", or, "I'm not sure". You might choose simple verification labels like c('y', 'n', 'unsure').
* **What type of Swainson's Thrush vocalization is this?** For this question, you might choose more descriptive verification labels like c('song', 'call', 'unsure', 'false'), or something even more detailed like c('song', 'peep', 'whit', 'whine', 'bink', 'peeer', 'false', 'unsure'). 

This part is left to your discretion. It's one of the challenging aspects of assessing automated detection results, and you may find that you need to iterate through a few options before settling on the verification library that works best for your circumstances.

Below, we'll use a simple verification library where 'y' means yes, it's a Swainson's Thrush, 'n' means it's not, and 'unsure' means we aren't certain. 
```r
# Create a verification library for this species
ver.lib <- c('y', 'n', 'unsure')
```

Now we're ready to use `birdnet_verify()`. This interactive function displays a spectrogram of the detected event and prompts the user to label it with one of the input options defined in the verification library. The function also optionally writes a temporary wave clip to your working directory. (Although there are options for playing a sound clip automatically from R, the behavior of these options varies across platforms/operating systems; instead of using R to play the clip, we decided it would be simpler to write a temporary wave clip file for the user). If you expect to be listening to the clips, you'll want easy access to your working directory so that you can open the wave clips up manually. 

`birdnet_verify()` has several arguments. `data` takes the data.frame or data.table of detections you want to verify. `verification.library` takes a character vector of the labeling options you will be using. `audio.directory` points to the directory where your audio files live, and `results.directory` points to the directory where your formatted BirdNET files are stored. `overwrite` allows you to decide whether or not any previously existing verifications should be overwritten. When FALSE, users will not be prompted to verify any detected events that already have a verification, but when TRUE, you may be overwriting previous labels. The `play` argument specifies whether or not a temporary wave file should be written to the working directory for each detection. The remaining arguments allow the user to customize how detected events should be displayed during the interactive session (see `?birdnet_verify` for details).
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

Running this example will produce an interactive output like the below image. The RStudio console will prompt you to provide a label for the detection. The plot pane will display a spectrogram of the detection. You'll use this spectrogram -- optionally along with the temporary wave file -- to decide which label to choose. In this example, we've used the `buffer` argument to place a 1 second buffer around the detection to provide additional visual and acoustic context. The detection itself is contained within the blue box (all BirdNET detections occur in three-second chunks). About 23.5 seconds in, a Swainson's Thrush begins singing. The spectrogram title gives us information about where we can find this detection in the txt or csv file, and informs us that BirdNET has a confidence level of 0.9 for the detection. We can label this as 'y' because the blue detection window does contain a Swainson's Thrush vocalization. 

**Click image for a larger version.**

<p align="center">
<img src=https://github.com/nationalparkservice/NSNSDAcoustics/blob/main/images/ver1.png alt="Illustration of outputs when using the function birdnet_verify. Left side of image shows interactive RStudio interface used for verification, right side depicts spectrogram of detected event to be verified, with a blue border showing the time boundaries of the detection."><br>
</p>


Once you've added labels for the remaining detections (in fact, they all contain Swainson's Thrush vocalizations!), `birdnet_verify()` will update the underlying formatted txt or csv files with your verifications. Below, we gather up the results again and check that our three verifications have been added.

```r
# Check that underlying files have been updated with user verifications
dat <- birdnet_gather(results.directory = 'example-results-directory',
                      formatted = TRUE)
dat[!is.na(verify)]
```

Finally, we clean up by deleting temporary files that we set up for the example.
```r
# Delete all temporary example files when finished
unlink(x = 'example-audio-directory', recursive = TRUE)
unlink(x = 'example-results-directory', recursive = TRUE)

```


### Visualize BirdNET spectrograms
`birdnet_plot()` allows the user to visualize spectrograms of BirdNET detections (whether or not the data have been verified). 

Start by pulling up the function helpfile. Everything covered below is located in the "Examples" section of this helpfile. 
```r
?birdnet_plot
```

We start by creating an example audio directory and writing example audio data to this directory.
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
```

Next, we read in example data.table/data.frame for plotting. Checking the structure of this example data reveals that there are 284 rows of data in the example.
```r
# Read in example data.table/data.frame for plotting
data(examplePlotData)

# Check the structure of this example data
str(examplePlotData)
```

`birdnet_plot()` expects a data.table/data.frame that has been formatted with the columns produced by `birdnet_format()`. Beyond that, this data.frame can contain just about anything. A user might choose to plot data by species, song type, verification label, confidence levels, and more. The `audio.directory` argument should point to the folder where your audio are contained. The remaining arguments allow some aesthetic control over plotting, with the option to provide a title, control the frequency limits, and choose spectrogram and box colors (see helpfile for details).  

Below, we subset the `examplePlotData` object to plot detections for Swainson's Thrush that contain the label "song" in the verify column. We give the plot a descriptive title, use frequency limits ranging from 0.5 to 12 kHz, specify a gray color scheme for the spectrogram, and draw gray boxes around each detection. 
```r
# Plot only detections of Swainson's Thrush verified as "song",
# with frequency limits ranging from 0.5 to 12 kHz, gray spectrogram colors,
# a custom title, and a gray box around each detection
plot.songs <- examplePlotData[common_name == "Swainson's Thrush" & verify == "song"]
birdnet_plot(data = plot.songs,
             audio.directory = 'example-audio-directory',
             title = "Swainson's Thrush Songs",
             frq.lim = c(0.5, 12),
             new.window = TRUE,
             spec.col = gray.3(),
             box = TRUE,
             box.lwd = 1,
             box.col = 'gray')
```
**Click image for a larger version.**
<p align="center">
<img src=https://github.com/nationalparkservice/NSNSDAcoustics/blob/main/images/plot1.png
     alt="Illustration of spectrogram output from birdnet_plot for Swainson's Thrush."><br>
</p>

In the next example, we plot detections for Swainson's Thrush that contain the label "call" in the verify column. We give the plot a descriptive title, use frequency limits ranging from 0.5 to 6 kHz and choose not to draw any boxes around detections. Below, we demonstrate that the `spec.col` argument allows for adjustable spectrogram colors, and that users can create their own gradients or use existing ones. A few spectrogram color options are provided with the package (e.g., gray.3()). In the example below, we input a color gradient from the [viridis](https://cran.r-project.org/web/packages/viridis/vignettes/intro-to-viridis.html) R package.
```r
# install.packages('viridis') # install the package first if you do not have it
library(viridis) 

# Plot only detections of Swainson's Thrush verified as "call"
# with frequency limits ranging from 0.5 to 6 kHz,a custom title, no boxes,
# and colors sampled from the viridis color package
plot.calls <- examplePlotData[common_name == "Swainson's Thrush" & verify == "call"]
birdnet_plot(data = plot.calls,
             audio.directory = 'example-audio-directory',
             title = "Swainson's Thrush Calls",
             frq.lim = c(0.5, 6),
             new.window = TRUE,
             spec.col = viridis::viridis(30),
             box = FALSE)
```
**Click image for a larger version.**

<p align="center">
<img src=https://github.com/nationalparkservice/NSNSDAcoustics/blob/main/images/plot2.png
     alt="Alternative illustration of spectrogram output from birdnet_plot for Swainson's Thrush, showing the of use different color schemes and plotting parameters."><br>
</p>

In the final example, we demonstrate that `birdnet_plot()` can also be used to visualize unverified data. Below, we loop through to plot all detections for two selected species -- [Varied Thrush](https://www.allaboutbirds.org/guide/Varied_Thrush/sounds) and [Pacific-slope Flycatcher](https://www.allaboutbirds.org/guide/Pacific-slope_Flycatcher/sounds) -- where the confidence of detection is greater than or equal to 0.25. 
```r
# Loop through to plot detections for selected unverified species
# where confidence of detection >= 0.25
# with frequency limits ranging from 0.5 to 12 kHz, custom titles, gray boxes,
# and gray spectrogram colors
sp <- c('Varied Thrush', 'Pacific-slope Flycatcher')
for (i in 1:length(sp)) {
 plot.sp <- examplePlotData[confidence >= 0.25 & common_name == sp[i]]
 birdnet_plot(data = plot.sp,
              audio.directory = 'example-audio-directory',
              title = paste0(sp[i], ' Detections >= 0.25'),
              frq.lim = c(0.5, 12),
              new.window = TRUE,
              spec.col = gray.3(),
              box = TRUE,
              box.lwd = 0.5,
              box.col = 'gray',
              title.size = 1.5)
}
```

**Click image for a larger version.**

<p align="center">
<img src=https://github.com/nationalparkservice/NSNSDAcoustics/blob/main/images/plot3.png
     alt="Illustration of the spectrogram outputs from birdnet_plot from looping through two focal species."><br>
</p>

Finally, delete all temporary files when finished. 
```r
# Delete all temporary example files when finished
unlink(x = 'example-audio-directory', recursive = TRUE)
```

### Create barcharts of BirdNET detections

`birdnet_barchart()` allows the user to visualize stacked barcharts of user-selected BirdNET results by date. Start by pulling up the function helpfile. Everything covered below is located in the "Examples" section of this helpfile. 
```r
?birdnet_barchart
``` 

First, we read in example data to be used as an input to `birdnet_barchart`. This would typically be a data.table generated by a call to `birdnet_gather`, and typically the data have been formatted using `birdnet_format`. Generally, this data object may be preceded by a call to `add_time_cols`. Regardless, the data object input to `birdnet_barchart` should contain BirdNET detection data that comes from a single site, and the object must contain columns named "locationID" (character), "recordingID" (character), and "dateTimeLocal" (POSIXct).

```r
# Read in exampleBarchartData
data(exampleBarchartData)

# Generally, add_time_cols() may be called as part of preprocessing
# (if not, please ensure data object has columns that include locationID (character),
# recordingID (character), and dateTimeLocal (POSIXct))
dat <- add_time_cols(dt = exampleBarchartData,
                     tz.recorder = 'America/Los_angeles',
                     tz.local = 'America/Los_angeles')
```

In addition to the data object, `birdnet_barchart` has an argument called interactive. When set to TRUE, `birdnet_barchart` will display a plotly-based interactive plot that allows the user to hover over data to see which species detections are being visualized.
```r
# Produce an interactive plotly barchart with interactive = TRUE
birdnet_barchart(data = dat, interactive = TRUE)
```
**Click image for a larger version. The figure is not interactive and merely serves as an illustration.**
<p align="center">
<img src=https://github.com/nationalparkservice/NSNSDAcoustics/blob/main/images/barchart-interactive.png
     alt="Screenshot of an interactive plotly barchart that shows stacked bars of detected species through time at a monitoring location. The x axis shows date, and the y axis shows the total number of detections for that date. Hovering over individual bars shows an instance of 1420 detections of Pacific Wren on julian date 132."><br>
</p>


Meanwhile, when interactive is set to FALSE, `birdnet_barchart` produces a ggplot-based static plot. The user also has the option to highlight certain species with the focal.species argument, which takes a character vector of common names of species to display. The focal.colors argument allows the user to specify which colors to use for which focal species. If the data object contains other species aside from the focals, all non-focal species will be plotted in black as "Other". 

```r
# Produce a static ggplot barchat with interactive = FALSE,
# add focal.species with custom colors (any species in the data object
# that are not in focal.species will be plotted in black as "Other".)
birdnet_barchart(data = dat,
                 interactive = FALSE,
                 focal.species = c("Pacific Wren", "Swainson's Thrush", "Varied Thrush"),
                 focal.colors = c('#00BE67', '#C77CFF', '#c51b8a'))
```
**Click image for a larger version.**
<p align="center">
<img src=https://github.com/nationalparkservice/NSNSDAcoustics/blob/main/images/barchart-static.png 
     alt="Static ggplot barchart highlighting the detections of three focal species through time at a monitoring location. The x axis shows date, and the y axis shows the total number of detections for that date."><br>
</p>


Generally, interactive = FALSE should be used in conjunction with the focal.species argument. If the focal.species argument is being used, a legend will also be plotted. This option provides a static plot output and the opportunity to highlight a small number of focal species and their detection activity through time. 

Meanwhile, use of interactive = TRUE is meant strictly for exploratory purposes. If the focal.species argument is not used, no legend will be plotted. A typical use case is to omit focal.species when setting interactive = TRUE since the pointer can hover over the data interactively to display species. A legend is not plotted in this case because typically interactive mode is only being used when there are dozens of species to display, and the number of colors in the legend will make species indistinguishable. 


## Converting wave audio files to NVSPL tables with wave_to_nvspl

`wave_to_nvspl()` uses PAMGuide code to convert wave files into an NVSPL formatted table. NVSPL stands for NPS-Volpe Sound Pressure Level, and is the standard format used in NSNSD analyses. These are hourly files comprised of 1/3 octave data in 1-sec LEQ increments. PAMGuide was developed by [Nathan D. Merchant et al. 2015](https://besjournals.onlinelibrary.wiley.com/doi/full/10.1111/2041-210X.12330). 

Start by pulling up the function helpfile. Everything covered below is located in the "Examples" section of this helpfile. 

```r
?wave_to_nvspl
```

We start by creating an example audio input directory and writing example audio to this directory. This is meant to illustrate the file types and folder structure `wave_to_nvspl()` expects to encounter.
```r
# Create an input directory for this example
dir.create('example-input-directory')

# Read in example wave files
data(exampleAudio1)
data(exampleAudio2)

# Write example waves to example input directory
tuneR::writeWave(object = exampleAudio1,
                 filename = 'example-input-directory/Rivendell_20210623_113602.wav')
tuneR::writeWave(object = exampleAudio2,
                 filename = 'example-input-directory/Rivendell_20210623_114602.wav')
```

`wave_to_nvspl()` takes several arguments. `input.directory` indicates the top-level input directory path. `data.directory` is a logical flag for whether audio files are housed in 'Data' subdirectories (common when using Songmeter SM4). The next argument, `test.file`, is a logical flag for whether to run the function in testing mode or in batch processing mode. The `project` argument allows the user to input a project name. The project name will be used to create a "params" file that will save parameter inputs in a file for posterity. The `timezone` argument forces the user to specify the timezone for the time reflected in the audio file name. Additional arguments are described in the helpfile; note that there are several default values in this function customized for NSNSD default settings when using a Songmeter SM4 audio recorder. 

The suggested workflow for this function is to first set `test.file = TRUE` to verify that your workflow has been accurately parameterized. When `test.file = TRUE`, `wave_to_nvspl()` will assess one file and encourage the user to check all outputs. Users should ensure there isn't an NA in the "Time stamp start time" output (if so, something is wrong). Lastly, the `test.file = TRUE` argument will create a plot allowing the user to verify that time is continuous. If there are breaks in the plotted line, there is an issue with your parameterization. For additional context and details, NSNSD staff and collaborators should view [this video tutorial](https://doimspp.sharepoint.com/sites/nsnsdallstaff/Shared%20Documents/Science%20and%20Tech/Software/SongMeterToNVSPL/SongMeter4toNVSPL.mp4).

```r
# Perform wave_to_nvspl in test mode (test.file = TRUE)
wave_to_nvspl(
 input.directory = 'example-input-directory',
 data.directory = FALSE,
 test.file = TRUE,
 project = 'testproject',
 timezone = 'GMT')
```

Once you feel confident that you have parameterized accurately, run the function in batch mode by setting `test.file = FALSE`. The example below provides progress feedback and takes a few moments to run. Once complete, we can view the NVSPL table outputs. Column names are described in the helpfile.
```r
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
```

Finally, we clean up by deleting the example input directory.
```r
# Delete all temporary example files when finished
unlink(x = 'example-input-directory', recursive = TRUE)
```


## Converting NVSPL files to acoustic indices with nvspl_to_ai

`nvspl_to_ai()` takes NVSPL table data created by `wave_to_nvspl()` and converts it into a broad range of acoustic index values, including acoustic activity, acoustic complexity index, acoustic diversity index, acoustic richness, spectral persistence, and roughness. 

Start by pulling up the function helpfile. Everything covered below is located in the "Examples" section of this helpfile. 
```r
?nvspl_to_ai
```

First, we create a few example directories: an input directory containing the sample wave audio files that come with the package, and an output directory to collect the resulting CSV of acoustic index values. This is meant to illustrate the file types and folder structure `nvspl_to_ai()` expects to encounter.

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
```

Now we are positioned to run `nvspl_to_ai()`. `input.directory` indicates the top-level input directory path, `output.directory` specifies where csv results should be stored, and  `project` allows the user to input a project name. The project name will be used to create a "params" file that will save parameter inputs in a file for posterity. Additional arguments are described in the helpfile; note that there are several default values in this function customized for NSNSD default settings.

```r
# Run nvspl_to_ai to generate acoustic indices csv for example NVSPL files,
nvspl_to_ai(input.directory = 'example-input-directory',
            output.directory = 'example-output-directory',
            project = 'example-project')
            
# View Results
(ai.results <- read.csv(list.files(path = 'example-output-directory',
                                   pattern = '.csv', full.names = TRUE)))
```

Finally, we clean up by deleting all example files. 
```r
# Delete all temporary example files when finished
unlink(x = 'example-input-directory', recursive = TRUE)
unlink(x = 'example-output-directory', recursive = TRUE)
```
