# NSNSDAcoustics

This repository provides a place for National Park Service [Natural Sounds and Night Skies Division (NSNSD)](https://www.nps.gov/orgs/1050/index.htm) staff to develop and modernize several bioacoustics workflows. 

If you encounter a problem, please submit it to [Issues](https://github.com/nationalparkservice/NSNSDAcoustics/issues).

# Table of Contents

- **[Installing NSNSDAcoustics](#installing-nsnsdacoustics)**
- **[Running BirdNET from RStudio](#running-birdnet-from-rstudio)**: Go here if you want to use your Windows machine to process files through RStudio [BirdNET-Analyzer]([https://birdnet.cornell.edu/](https://github.com/kahst/BirdNET-Analyzer)). Requires some setup. 
- **[Assessing BirdNET results](#assessing-birdnet-results)**: Go here if you already have raw BirdNET outputs in hand from rtype = 'r', and want to use R to wrangle, visualize, and verify the results.
  * **[Reformat raw BirdNET results](#reformat-raw-birdnet-results)**
  * **[Gather BirdNET results](#gather-birdnet-results)**
  * **[Verify BirdNET results](#verify-birdnet-results)**
  * **[Visualize BirdNET spectrograms](#visualize-birdnet-spectrograms)**
  * **[Create barcharts of BirdNET detections](#create-barcharts-of-birdnet-detections)**
  * **[Create heat maps of BirdNET detections by date](#create-heat-maps-of-birdnet-detections-by-date)**
  * **[Create heat maps of BirdNET detections by date and time](#create-heat-maps-of-birdnet-detections-by-date-and-time)**

- **[Converting wave audio files to NVSPL tables with wave_to_nvspl](#converting-wave-audio-files-to-nvspl-tables-with-wave_to_nvspl)**: Go here for a PAMGuide wrapper function to convert wave files to NVSPL formatted tables.
- **[Converting NVSPL files to acoustic indices with nvspl_to_ai](#converting-nvspl-files-to-acoustic-indices-with-nvspl_to_ai)**: Go here to convert NVSPL.txt files into a CSV of acoustic indices.


## Installing NSNSDAcoustics

First, ensure that you have installed the latest versions of [R](https://cran.r-project.org/) and [RStudio](https://www.rstudio.com/products/rstudio/download/). 

Next, you can install NSNSDAcoustics using one of two options: 

### (1) Option 1: Use `install_github()`:

You may first need to install the latest version of devtools (and possibly Rtools). Then you can install the latest version of NSNSDAcoustics:

```r

install.packages('devtools')
library(devtools)
devtools::install_github('nationalparkservice/NSNSDAcoustics')

```

### (2) Option 2: Download and install manually

Download the latest zip or tar.gz file from the [Releases Page](https://github.com/nationalparkservice/NSNSDAcoustics/releases). Note that major releases are also captured on NPS DataStore at this link: [https://irma.nps.gov/DataStore/Reference/Profile/2311171](https://irma.nps.gov/DataStore/Reference/Profile/2311171)

After downloading, open R Studio, click on the Install button on the Packages tab, select Install From Package Archive File, and navigate to the downloaded file.

Once NSNSDAcoustics is installed, you can call in the package and view helpfiles: 

```r

library(NSNSDAcoustics)
help(package = 'NSNSDAcoustics')

```


### A note on data.table syntax
NSNSDAcoustics depends on the R package `data.table`, which enables fast querying and manipulation of large data.frames. If you are an R user but have never used `data.table` syntax before, some of the example code may look unfamiliar. Don't fret -- `data.table` object types are also  `data.frames`. If you get frustrated trying to work with them, you can always convert to a regular `data.frame` to deal with a more familiar object type.


## Running BirdNET from RStudio

**This worfklow was developed for Windows 10, BirdNET-Analyzer releases V1.1.x through 1.5.x, and model version V2.4. It has not been tested on other systems.** 

[BirdNET](https://birdnet.cornell.edu/) is a bird sound recognition program developed by the [Cornell Center for Conservation Bioacoustics](https://www.birds.cornell.edu/ccb/). The [BirdNET-Analyzer Github repository](https://github.com/kahst/BirdNET-Analyzer) provides a promising free tool for quickly processing large volumes of audio data and detecting sounds. 

### (1) Step 1. Download the fully packaged BirdNET Analyzer for Windows. 

As of this writing, [version 1.5.1](https://github.com/kahst/BirdNET-Analyzer/releases/tag/v1.5.1) is the approved version for NPS. For a signed version of this software, NPS staff please reach out to Cathleen. 

### (2) Step 2. Familiarize yourself with the [command line arguments listed in BirdNET-Analyzer's documentation](https://birdnet-team.github.io/BirdNET-Analyzer/usage/cli.html#birdnet_analyzer.cli-analyzer_parser-named-arguments).

These command line arguments are the building blocks needed to construct a statement that tells BirdNET where to find your audio (--i), where to place result files (--o), what detection sensitivity to use (--sensitivity), where to find a species list if you are using one (--slist), how many CPU threads to use (--threads), what type of result to produce (--rtype) and more. **To use the functions in this package, you will need to specify --rtype 'r'.** Once you understand the command line arguments, you are ready to try using BirdNET from the Windows command line. 

### (3) Step 3. (Optional) Test your BirdNET Installation. 

Test that BirdNET is functional by opening up a Windows Command Prompt (see below image). Look to the lower lefthand side of your screen and locate the Windows search bar. Type Command Prompt and click the app.  

<p align="center">
<img src=https://github.com/nationalparkservice/NSNSDAcoustics/blob/main/images/command-prompt.png alt="Image showing how to locate and open the Windows command prompt."><br>
</p>


Next, you can construct a statement for the command prompt. First, you'll need to know the full file path for your BirdNET-Analyzer.exe. This file path should end in "../Programs/BirdNET-Analyzer/BirdNET-Analyzer.exe". Your statement might look something like the following example, or it might include additional command line arguments: 

`"C:/your-path-here/Programs/BirdNET-Analyzer/BirdNET-Analyzer.exe" --i "D:/AUDIO" --o "D:/RESULTS" --lat -1 --lon -1 --week -1 --slist "D:/species_list.txt" --rtype "r" --min_conf 0.1 --sensitivity 1.0 --threads 4`

Please edit this example to reflect file paths and folder names on your machine, and then modify, omit, or include command line arguments as desired. If BirdNET-Analyzer writes files to your results folder, you were successful in getting everything installed. 

### (4) Step 4. Run BirdNET from RStudio.

If you are processing many terabytes, years, and/or locations of data through BirdNET, you might have dozens or hundreds of audio folders. The prospect of constructing command line statements by hand for each folder may sound daunting and tedious. You may find yourself wishing that you could loop through all of your folders and access BirdNET-Analyzer directly from R. The `birdnet_analyzer()` function enables this. 

Below, we'll walk through the documentation and example helpfiles for `birdnet_analyzer()`. Always start by pulling up the function helpfile. Everything covered below is located in the "Examples" section of this helpfile. 

```r

?birdnet_analyzer

```

To set up this example, we first create example audio and results directories. Then we write example audio wave files to the directory. We also create a sample species list to illustrate how you might use this in your workflow. This setup illustrates the file types and folder structure you will emulate when you use `birdnet_analyzer()`.

```r

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
```

Now, we're set up to run the function. `birdnet_analyzer()` takes a large number of arguments. First, `birdnet.version` specifies which release of BirdNET-Analyzer you are using (e.g., 'v1.5.1'). In `birdnet.path`, 	specify the absolute path to the BirdNET-Analyzer.exe installation on your machine. e.g., "C:/your-path-here/Programs/BirdNET-Analyzer/BirdNET-Analyzer.exe". All of the remaining arguments will look very similar to the [command line arguments listed in BirdNET-Analyzer's documentation](https://birdnet-team.github.io/BirdNET-Analyzer/usage/cli.html). For example, `i.audio` requires an absolute path to an input file or folder, and `o.results` requires an absolute path to an output file or folder. See `?birdnet_analyzer()` help documentation or BirdNET-Analyzer's documentation for more details. 

Below, we show pseudocode that you can modify to test the function. Note that many arguments are missing because many use defaults. Be sure to familiarize yourself with each argument. You may wish to test various combinations of parameters and observe how they affect results.

```r

##### The following example is pseudocode ######

# Because this function calls an external program (BirdNET-Analyzer.exe),
# the example function below will not be modifiable to run for you unless
# you have already installed BirdNET-Analyzer V1.1.0 for Windows.

# Run all audio data in a directory through BirdNET
# Modify the birdnet.path, i.audio, o.results, and
# slist arguments with appropriate paths for your machine

birdnet_analyzer(
 birdnet.version = 'v1.1.0',
 birdnet.path = 'absolute/path/AppData/Local/Programs/BirdNET-Analyzer/BirdNET-Analyzer.exe',
 i.audio = 'absolute/path/example-audio-directory',
 o.results = 'absolute/path/example-results-directory',
 slist = 'absolute/path/species_list.txt',
)

```

If you have dozens of audio folders, you can use `birdnet_analyzer()` to loop through each folder. For example, let's say you have several audio folders: D:/AUDIO_1, D:/AUDIO_2, and D:/AUDIO_3. You might have created corresponding results folders: D:/RESULTS_1, D:/RESULTS_2, and D:/RESULTS_3. The following pseudocode illustrates a loop that can be edited for your own purposes: 

```r

# Initialize important variables:
my.birdnet.path <- 'absolute/path/AppData/Local/Programs/BirdNET-Analyzer/BirdNET-Analyzer.exe'
audio.folders <- c(paste0('D:/', 'AUDIO_', 1:3))
result.folders <- c(paste0('D:/', 'RESULTS_', 1:3))
species.list.path <- 'D:/species_list.txt'

for (i in 1:length(audio.folders)) {
 birdnet_analyzer(
  birdnet.version = 'v1.1.0',
  birdnet.path = my.birdnet.path,
  i.audio = audio.folders[i],    
  o.results = result.folders[i], 
  slist = species.list.path
 )
}

```

### Additional Tips and Notes: 

* You can specify any option in `rtype`, but to use other functions in this package, please set `rtype = 'r'`.
* The `threads` argument specifies how many files BirdNET will work on at once, and it depends on the number of logical processors your machine has. On a Windows machine, to figure out how many cores your processor has, press **CTRL + SHIFT + ESC** to open Task Manager. Select the **Performance** tab to see how many cores and logical processors your PC has. A rule of thumb is to never set "threads" to more than the number of logical processors minus 1. For example, your machine might have 8 logical processors, in which case you would set "threads" to no higher than 7. 

If you're having trouble running `birdnet_analyzer()`, note that under the hood, it is simply constructing a statement for the command line and wrapping a `system()` call around the statement to send it to BirdNET-Analyzer.exe. You can construct your own similar commands and loop through them like so: 

```r

# Initialize important variables:
my.birdnet.path <- 'absolute/path/AppData/Local/Programs/BirdNET-Analyzer/BirdNET-Analyzer.exe'
audio.folders <- c(paste0('D:/', 'AUDIO_', 1:3))
result.folders <- c(paste0('D:/', 'RESULTS_', 1:3))
species.list.path <- 'D:/species_list.txt'
num.threads <- 7

# Generate a single command to loop through several folders:
## NOTE: be mindful of your quotations when editing!
all.commands <- paste0(
  '"', my.birdnet.path,
  '" --i "', audio.folders,
  '" --o "', result.folders,
  '" --lat -1 --lon -1 --week -1 --slist ',
  species.list.path, ' --rtype "r" --threads ',
  num.threads, ' --min_conf 0.1 --sensitivity 1.0'
)

# Test that one command runs
# system(all.commands[1])

# Loop through all commands (i.e., all audio folders) and send them to BirdNET-Analyzer
for (i in 1:length(all.commands)) {
  cat('\n \n This is folder', i, 'of', length(all.commands), '\n \n')
  system(all.commands[i])
}

```



## Assessing BirdNET Results

If you have a large number of audio files, and plan to monitor for a long time across many locations, you may soon find yourself managing thousands of BirdNET output files. It's likely that you'll want a systematic way to track and check on these results, and verify whether BirdNET detections are truly from a target species. The `birdnet_format()` --> `birdnet_verify()` workflow offers one way to keep track of your verifications. An alternative way would be to set up a SQLite database (e.g., [as used in the AMMonitor package](https://code.usgs.gov/vtcfwru/ammonitor)). Although a database solution may ultimately be the most robust way to track results through time in a long term project, this can come with a lot of start up and might not be easily extensible to your project needs. Instead, the worfklow below provides a simple way to reformat and work with BirdNET output files directly, allowing you to store your verifications there.

### Reformat raw BirdNET results

`birdnet_format()` is a simple function that reformats the raw BirdNET `rtype = 'r'` results with a "recordingID" column for easier data manipulation, a "verify" column to support manual verification of detection results, and a "timezone" column to clarify the timezone setting used by the audio recorder. 

Below, we'll walk through the documentation and example helpfiles for `birdnet_format()`. Always start by pulling up the function helpfile. Everything covered below is located in the "Examples" section of this helpfile. 

```r

?birdnet_format

```

To run this example, we first create an example "results" directory, and then we write some BirdNET results to this directory. This setup illustrates the file types and folder structure `birdnet_format()` expects to encounter.
```r


# Create a BirdNET results directory for this example
dir.create('example-results-directory')

# Write examples of raw BirdNET outputs to example results directory
data(exampleBirdNET1)
write.table(
  x = exampleBirdNET1,
  file = 'example-results-directory/Rivendell_20210623_113602.BirdNET.results.csv',
  row.names = FALSE, quote = FALSE, sep = ','
)

data(exampleBirdNET2)
write.table(
  x = exampleBirdNET2,
  file = 'example-results-directory/Rivendell_20210623_114602.BirdNET.results.csv',
  row.names = FALSE, quote = FALSE, sep = ','
)

```

Now, we're set up to run the function. `birdnet_format()` takes two arguments. First, the `results.directory` should point to the folder where you have stored your raw BirdNET outputs. Second, the `timezone` argument allows you to specify the timezone setting used in the audio recorder (i.e., the timezone reflected in the wave filename). **It's important to pay attention to this!** Recall that the functions described here expect wave files that follow a SITEID_YYYYMMDD_HHMMSS naming convention. In the package sample audio data, we have a wave named Rivendell_20210623_113602.wav. This means the site ID is Rivendell, and the recording was taken on June 23rd, 2021 at 11:36:02. The `timezone` argument allows us to clarify what 11:36:02 actually means. Was the recording taken in local time at your site, or was it taken in UTC? This point might seem trivial if you're just getting started with collecting data at a few sites for a single season, but if you're collecting data across many sites, over many years, with varying audio recorder equipment and varying recording settings through time, different field technicians, and potentially across timezones (as we often do at NPS NSNSD), you will want to keep meticulous track of your timezones so that your analyses will be accurate across time and space. If recordings were taken in local time at your study site, specify an [Olson-names-formatted character timezone](https://en.wikipedia.org/wiki/List_of_tz_database_time_zones#List) for the location (e.g., "America/Los_Angeles"). If recordings were taken in UTC, you can put either 'GMT' or 'UTC'; both are acceptable in R for downstream date-time formatting. 

Below, we point to our example results directory and specify 'GMT' (i.e., 'UTC') as the timezone, since the recordings were not taken in local time at this recorder.

```r

# Run birdnet_format:
birdnet_format(
  results.directory = 'example-results-directory',
  timezone = 'GMT'
)

```

After running the function, look in your example results directory folder and check on the results. This function produces new formatted files with filename suffix "BirdNET_formatted_results.csv". Note that `birdnet_format()` does NOT overwrite your raw BirdNET results. 

Finally, clean up by deleting temporary files that were set up for the example. 

```r

# Delete all temporary example files when finished
unlink(x = 'example-results-directory', recursive = TRUE)

```

The point of all this reformatting is to make it easier to keep track of downstream analyses of BirdNET detections. The "verify" column allows us to track whether a detected event actually came from a target species. 

### Gather BirdNET results

`birdnet_gather()` is a simple convenience function that gathers all BirdNET results from a desired folder into one user-friendly data.table / data.frame. View the helpfile for more information and code examples (`?birdnet_gather`). 

The function allows you to gather either unformatted (raw) or formatted data. For this example, we set up an example results directory and write both formatted and unformatted csv results to it. This illustrates the folder and file structure `birdnet_gather()` expects to encounter.

```r

# Create a BirdNET results directory for this example
dir.create('example-results-directory')

# Write examples of formatted BirdNET outputs to example results directory
data(exampleFormatted1)
write.table(
   x = exampleFormatted1,
   file = 'example-results-directory/Rivendell_20210623_113602.BirdNET_formatted_results.csv',
   row.names = FALSE, quote = FALSE, sep = ','
)

data(exampleFormatted2)
write.table(
   x = exampleFormatted2,
   file = 'example-results-directory/Rivendell_20210623_114602.BirdNET_formatted_results.csv',
   row.names = FALSE, quote = FALSE, sep = ','
)

# Write examples of raw BirdNET outputs to example results directory
data(exampleBirdNET1)
write.table(
   x = exampleBirdNET1,
   file = 'example-results-directory/Rivendell_20210623_113602.BirdNET.results.csv',
   row.names = FALSE, quote = FALSE, sep = ','
)

data(exampleBirdNET2)
write.table(
   x = exampleBirdNET2,
   file = 'example-results-directory/Rivendell_20210623_114602.BirdNET.results.csv',
   row.names = FALSE, quote = FALSE, sep = ','
)

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

To run this example, we first create an example audio directory, to which we write the sample audio that comes with the package. We also set up an example results directory, to which we write example formatted .csv data. This is meant to illustrate the file types and folder structure `birdnet_verify()` expects to encounter.

```r

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

# Write examples of formatted BirdNET outputs to example results directory
data(exampleFormatted1)
write.table(
   x = exampleFormatted1,
   file = 'example-results-directory/Rivendell_20210623_113602.BirdNET_formatted_results.csv',
   row.names = FALSE, quote = FALSE, sep = ','
 )

data(exampleFormatted2)
write.table(
  x = exampleFormatted2,
  file = 'example-results-directory/Rivendell_20210623_114602.BirdNET_formatted_results.csv',
  row.names = FALSE, quote = FALSE, sep = ','
 )

```

Next, we can use `birdnet_gather()` to grab all the formatted results from the example folder. From here, you can manipulate your results however you want to create a subset of detections that you wish to verify. The key is to subset only to a single species. In case you accidentally include multiple species in your data subset, `birdnet_verify()` will remind you that it only accepts one species at a time for verifications. In this example, we'll focus on verifying detections for the [Swainson's Thrush (Catharus ustulatus)](https://www.allaboutbirds.org/guide/Swainsons_Thrush).

You can create your verification sample however you like. A few options are to take a simple random sample, or take a stratified sample based on detections from different locations or times of day. Depending on your question, you might even want to verify *every* detection for your target species. In the below example, we set a seed for reproducibility and take a simple random sample of three Swainson's Thrush detections.

```r

# Gather formatted BirdNET results
dat <- birdnet_gather(
  results.directory = 'example-results-directory',
  formatted = TRUE
)

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
birdnet_verify(
  data = to.verify,
  verification.library = ver.lib,
  audio.directory = 'example-audio-directory',
  results.directory = 'example-results-directory',
  overwrite = FALSE,
  play = TRUE,
  frq.lim = c(0, 12),
  buffer = 1,
  box.col = 'blue',
  spec.col = monitoR::gray.3()
)

```

Running this example will produce an interactive output like the below image. The RStudio console will prompt you to provide a label for the detection. The plot pane will display a spectrogram of the detection. You'll use this spectrogram -- optionally along with the temporary wave file -- to decide which label to choose. In this example, we've used the `buffer` argument to place a 1 second buffer around the detection to provide additional visual and acoustic context. The detection itself is contained within the blue box (all BirdNET detections occur in three-second chunks). About 23.5 seconds in, a Swainson's Thrush begins singing. The spectrogram title gives us information about where we can find this detection in the csv file, and informs us that BirdNET has a confidence level of 0.19 for the detection. We can label this as 'y' because the blue detection window does contain a Swainson's Thrush vocalization. 

**Click image for a larger version.**

<p align="center">
<img src=https://github.com/nationalparkservice/NSNSDAcoustics/blob/main/images/ver1.png alt="Illustration of outputs when using the function birdnet_verify. Left side of image shows interactive RStudio interface used for verification, right side depicts spectrogram of detected event to be verified, with a blue border showing the time boundaries of the detection."><br>
</p>


Once you've added labels for the remaining detections, `birdnet_verify()` will update the underlying formatted csv files with your verifications. Below, we gather up the results again and check that our three verifications have been added.

```r

# Check that underlying files have been updated with user verifications
dat <- birdnet_gather(
  results.directory = 'example-results-directory',
  formatted = TRUE
)

dat[!is.na(verify)]

```

Finally, we clean up by deleting temporary files that we set up for the example.

```r

# Delete all temporary example files when finished
unlink(x = 'example-audio-directory', recursive = TRUE)
unlink(x = 'example-results-directory', recursive = TRUE)

```


### Visualize BirdNET spectrograms

`birdnet_spectro()` allows the user to visualize spectrograms of BirdNET detections (whether or not the data have been verified). 

Start by pulling up the function helpfile. Everything covered below is located in the "Examples" section of this helpfile. 

```r

?birdnet_spectro

```

We start by creating an example audio directory and writing example audio data to this directory.

```r

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

```

Next, we read in example data.table/data.frame for plotting. This example data.table has several hundred BirdNET results, some of which have been verified. 

```r

# Read in example data.table/data.frame for plotting
data(exampleSpectroData)

```

`birdnet_spectro()` expects a data.table/data.frame that has been formatted with the columns produced by `birdnet_format()`. Beyond that, this data.frame can contain just about anything. A user might choose to plot data by species, song type, verification label, confidence levels, and more. The `audio.directory` argument should point to the folder where your audio are contained. The remaining arguments allow some aesthetic control over plotting, with the option to provide a title, control the frequency limits, and choose spectrogram and box colors.  

Below, we subset the `exampleSpectroData` object to plot detections for Swainson's Thrush that contain songs. In this case, the user had a verification library of c('song', 'call', 'both', 'n') -- a detection was labeled 'both' if it contained both a Swainson's thrush song and call. Below, we give the plot a descriptive title, use frequency limits ranging from 0.5 to 12 kHz, specify a gray color scheme for the spectrogram, and draw gray boxes around each detection. 

```r

# Plot detections of Swainson's Thrush that contain songs
# with frequency limits ranging from 0.5 to 12 kHz, gray spectrogram colors,
# a custom title, and a gray box around each detection
plot.songs <- exampleSpectroData[common_name == "Swainson's Thrush" & verify %in% c("song", "both")]
birdnet_spectro(
  data = plot.songs,
  audio.directory = 'example-audio-directory',
  title = "Swainson's Thrush Songs",
  frq.lim = c(0.5, 12),
  new.window = TRUE,
  spec.col = gray.3(),
  box = TRUE,
  box.lwd = 1,
  box.col = 'gray'
)

```

**Click image for a larger version.**
<p align="center">
<img src=https://github.com/nationalparkservice/NSNSDAcoustics/blob/main/images/plot1.png
     alt="Illustration of spectrogram output from birdnet_spectro for Swainson's Thrush."><br>
</p>

In the next example, we plot detections for Swainson's Thrush that were verified to contain calls. We give the plot a descriptive title, use frequency limits ranging from 0.5 to 6 kHz and choose not to draw any boxes around detections. Below, we demonstrate that the `spec.col` argument allows for adjustable spectrogram colors, and that users can create their own gradients or use existing ones. A few spectrogram color options are provided with the package (e.g., gray.3()). In the example below, we input a color gradient from the [viridis](https://cran.r-project.org/web/packages/viridis/vignettes/intro-to-viridis.html) R package.

```r

# install.packages('viridis') # install the package first if you do not have it
library(viridis) 

# Plot only detections of Swainson's Thrush that contain calls
# with frequency limits ranging from 0.5 to 6 kHz, a custom title, no boxes,
# and colors sampled from the viridis color package
plot.calls <- exampleSpectroData[common_name == "Swainson's Thrush" & verify %in% c("call", "both")]
birdnet_spectro(
  data = plot.calls,
  audio.directory = 'example-audio-directory',
  title = "Swainson's Thrush Calls",
  frq.lim = c(0.5, 12),
  new.window = TRUE,
  spec.col = viridis::viridis(30),
  box = FALSE,
)

```
**Click image for a larger version.**

<p align="center">
<img src=https://github.com/nationalparkservice/NSNSDAcoustics/blob/main/images/plot2.png
     alt="Alternative illustration of spectrogram output from birdnet_spectro for Swainson's Thrush, showing the of use different color schemes and plotting parameters."><br>
</p>

In the final example, we demonstrate that `birdnet_spectro()` can also be used to visualize unverified data. Below, we loop through to plot all detections for two selected species -- [Pacific Wren](https://www.allaboutbirds.org/guide/Pacific_Wren/sounds) and [Pacific-slope Flycatcher](https://www.allaboutbirds.org/guide/Pacific-slope_Flycatcher/sounds) -- where the confidence of detection is greater than or equal to 0.25. 

```r

# Loop through to plot detections for selected unverified species
# where confidence of detection >= 0.25
# with frequency limits ranging from 0.5 to 12 kHz, custom titles, gray boxes,
# and gray spectrogram colors
sp <- c('Pacific Wren', 'Pacific-slope Flycatcher')
for (i in 1:length(sp)) {
 plot.sp <- exampleSpectroData[common_name == sp[i] & confidence >= 0.25]
 birdnet_spectro(data = plot.sp,
    audio.directory = 'example-audio-directory',
    title = paste0(sp[i], ' Detections >= 0.25'),
    frq.lim = c(0.5, 12),
    new.window = TRUE,
    spec.col = gray.3(),
    box = TRUE,
    box.lwd = 0.5,
    box.col = 'gray',
    title.size = 1.5
 )
}

```

**Click image for a larger version.**

<p align="center">
<img src=https://github.com/nationalparkservice/NSNSDAcoustics/blob/main/images/plot3.png
     alt="Illustration of the spectrogram outputs from birdnet_spectro from looping through two focal species."><br>
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

First, we read in example data to be used as an input to `birdnet_barchart()`. This would typically be a data.table generated by a call to `birdnet_gather()`, and typically the data have been formatted using `birdnet_format()`. Generally, this data object may be preceded by a call to `add_time_cols()`. Regardless, the data object input to `birdnet_barchart()` should contain BirdNET detection data that comes from a single site, and the object must contain columns named "locationID" (character), "recordingID" (character), and "dateTimeLocal" (POSIXct).

```r

# Read in example data
data(exampleHeatmapData)

# Ensure your data has an appropriate recordingID column and time columns
dat <- exampleHeatmapData
dat[ ,recordingID := basename(filepath)]
dat <- add_time_cols(
 dt = dat,
 tz.recorder = 'America/Los_angeles',
 tz.local = 'America/Los_angeles'
)

```

In addition to the `data` argument, `birdnet_barchart()` has an argument called `interactive`. When set to TRUE, `birdnet_barchart()` will display a plotly-based interactive plot that allows the user to hover over data to see which species detections are being visualized.

```r

# Produce an interactive plotly barchart with interactive = TRUE
birdnet_barchart(data = dat, interactive = TRUE)

```

**Click image for a larger version. The figure is not interactive and merely serves as an illustration.**
<p align="center">
<img src=https://github.com/nationalparkservice/NSNSDAcoustics/blob/main/images/barchart-interactive.png
     alt="Screenshot of an interactive plotly barchart that shows stacked bars of detected species through time at a monitoring location. The x axis shows date, and the y axis shows the total number of detections for that date. Hovering over individual bars shows an instance of 852 detections of Pacific Wren in mid-April, 2023."><br>
</p>


Meanwhile, when `interactive = FALSE`, `birdnet_barchart()` produces a ggplot-based static plot. The user also has the option to highlight certain species with the `focal.species` argument, which takes a character vector of common names of species to display. The `focal.colors` argument allows the user to specify which colors to use for which focal species. If `data` contains other species aside from the focals, all non-focal species will be plotted in black as "Other". 

```r

# Produce a static ggplot barchat with interactive = FALSE,
# add focal.species with custom colors (any species in the data object
# that are not in focal.species will be plotted in black as "Other".)
birdnet_barchart(
   data = dat,
   interactive = FALSE,
   focal.species = c("Pacific Wren", "Swainson's Thrush"),
   focal.colors = c('#00BE67', '#C77CFF', '#c51b8a')
)

```

**Click image for a larger version.**
<p align="center">
<img src=https://github.com/nationalparkservice/NSNSDAcoustics/blob/main/images/barchart-static.png 
     alt="Static ggplot barchart highlighting the detections of focal species through time at a monitoring location. The x axis shows date, and the y axis shows the total number of detections for that date."><br>
</p>


Generally, `interactive = FALSE` should be used in conjunction with the `focal.species` argument. If using `focal.species`, a legend will also be plotted. This option provides a static plot output and the opportunity to highlight a small number of focal species and their detection activity through time. 

Meanwhile, use of `interactive = TRUE` is meant strictly for exploratory purposes. If `focal.species` is not used, no legend will be plotted. A typical use case is to omit `focal.species` when setting `interactive = TRUE` since the pointer can hover over the data interactively to display species. A legend is not plotted in this case because typically interactive mode is only being used when there are dozens of species to display, and the number of colors in the legend will make species indistinguishable. 


### Create heat maps of BirdNET detections by date

`birdnet_heatmap()` allows the user to visualize heat maps of user-selected BirdNET results by date. Start by pulling up the function helpfile. Everything covered below is located in the "Examples" section of this helpfile. 

```r

?birdnet_heatmap

``` 

First, we read in example data to be used as an input to `birdnet_heatmap()`. This would typically be a data.table generated by a call to `birdnet_gather()`, and the data may or may not been formatted using `birdnet_format()`. Generally, this data object may be preceded by a call to `add_time_cols()`. Regardless, the data object input to `birdnet_heatmap()` should contain BirdNET detection data that comes from a single site, and the object must contain columns named "locationID" (character), "recordingID" (character), and "dateTimeLocal" (POSIXct).  Below, we are also reading in `exampleDatesSampled`, which is a dates object.

```r

# Read in example data
data(exampleHeatmapData)
data(exampleDatesSampled)

# Ensure your data has an appropriate recordingID column and time columns
dat <- exampleHeatmapData
dat[ ,recordingID := basename(filepath)]
dat <- add_time_cols(
 dt = dat,
 tz.recorder = 'America/Los_angeles',
 tz.local = 'America/Los_angeles'
)

```

In the `data` argument, `birdnet_heatmap()` expects a data.frame or data.table of BirdNET results. In `locationID` and `common.name`, specify a valid character locationID and common.name for your location and species of interest. In `conf.threshold`, specify a numeric input for a BirdNET confidence threshold below which detections will be discarded. The optional argument `julian.breaks` allows you to input a numeric vector of julian date plotting breaks to use on the x-axis of the heat map (if omitted, these breaks will be computed automatically by the function). ([Here's a useful chart for choosing julian breaks](https://landweb.modaps.eosdis.nasa.gov/ltdr/browse/calendar.html)). Setting `comparable.color.breaks = TRUE` allows you to generate heatmap color breaks based on every species in the input data to enable easier inter-species comparisons. Setting `comparable.color.breaks = FALSE` means the function will simply generate heatmap color breaks based only on the species of interest you specified in `common.name`. Finally, the `dates.sampled` argument requires either a Date vector or character vector of dates that were sampled and should be visualized on the heat map. This information is required because your data input may only contain detection data, and not non-detection data (i.e., zeroes). For example, you might have recorded audio on 2021-03-14, but have no BirdNET detections in your input to the `data` argument. This will result in an inaccurate visualization. Since your results may not automatically contain non-detection data, it is incumbent on the user to input which dates were sampled.

```r

# Generate a heatmap at Rivendell for Pacific Wren
# Set comparable.color.breaks = FALSE to maximize contrast in a single species map
# Add user-input julian.breaks
birdnet_heatmap(
  data = dat,
  locationID = 'Rivendell',
  common.name = 'Pacific Wren',
  conf.threshold = 0.2,
  dates.sampled = exampleDatesSampled,
  julian.breaks = seq(from = 70, to = 250, by = 30),
  comparable.color.breaks = FALSE
)

```

**Click image for a larger version.**
<p align="center">
<img src=https://github.com/nationalparkservice/NSNSDAcoustics/blob/main/images/heatmap-date-pawr.png 
     alt="Heatmap of detections of Pacific Wren at a monitoring location. The x axis shows date, the y axis shows year, and daily detection value is visualized from low (purple) to high (yellow)."><br>
</p>


In the second example, we loop through multiple species and set `comparable.color.breaks = TRUE` to plot according to a consistent color ramp. This option enables easier comparison between species. 

```r

# Generate heatmaps for several species with comparable.color.breaks == TRUE
# so that heatmap color scale is conserved for ease of interspecies comparison
sp <- c("Pacific Wren",
        "Pacific-slope Flycatcher",
        "Swainson's Thrush",
        "Wilson's Warbler")

for (i in 1:length(sp)) {

 print(paste0('Working on ', sp[i]))

 g <- birdnet_heatmap(
   data = dat,
   locationID = 'Rivendell',
   common.name = sp[i],
   conf.threshold = 0.2,
   dates.sampled = exampleDatesSampled,
   julian.breaks = seq(from = 70, to = 250, by = 30),
   comparable.color.breaks = TRUE
 )

 print(g)

}

```
**Click image for a larger version.**
<p align="center">
<img src=https://github.com/nationalparkservice/NSNSDAcoustics/blob/main/images/heatmap-date-multispecies.png 
     alt="Heatmaps of focal species detections by date at a monitoring location. The x axis shows date, the y axis shows year, and daily detection value is visualized from low (purple) to high (yellow)."><br>
</p>



### Create heat maps of BirdNET detections by date and time

`birdnet_heatmap_time()` allows the user to visualize heat maps of user-selected BirdNET results by date and time. Start by pulling up the function helpfile. Everything covered below is located in the "Examples" section of this helpfile. 

```r

?birdnet_heatmap_time

``` 

First, we read in example data to be used as an input to `birdnet_heatmap_time()`. This would typically be a data.table generated by a call to `birdnet_gather()`, and the data may or may not been formatted using `birdnet_format()`. Generally, this data object may be preceded by a call to `add_time_cols()`. Regardless, the data object input to `birdnet_heatmap_time()` should contain BirdNET detection data that comes from a single site, and the object must contain columns named "locationID" (character), "recordingID" (character), and "dateTimeLocal" (POSIXct). Below, we are also reading in `exampleDatesSampled`, which is a dates object.

```r
# Read in example data
data(exampleHeatmapData)
data(exampleDatesSampled)

# Ensure your data has an appropriate recordingID column and time columns
dat <- exampleHeatmapData
dat[ ,recordingID := basename(filepath)]
dat <- add_time_cols(
  dt = dat,
  tz.recorder = 'America/Los_angeles',
  tz.local = 'America/Los_angeles'
)

```

The arguments to `birdnet_heatmap_time()` are similar to `birdnet_heatmap()`, but with a few additions. The `hours.sampled` argument allows the user to clearly display which hours were actually acoustically monitored; the argument takes either an integer vector declaring which hours were sampled across the monitoring period (e.g., c(6:8, 18:20)), or a list declaring sun-based monitoring based on how many hours before and after sunset were recorded. For example, `list(sunrise = c(1.5, 1.5), sunset = c(1, 1))` means that the schedule recorded 1.5 hours before sunrise until 1.5 hours after, and 1 hour before sunset to 1 hour after. If missing `hours.sampled`, the function assumes continuous sampling and will display the plot as such; beware that this may misrepresent your data and if you did not sample during all hours, the plot will make it appear as if you did. The `y.axis.limits` argument lets the user control how much of the 24-hour day to display, (for example, `y.axis.limits = c(2, 12)` would display data from 2am to 12pm). The `minute.timestep` argument allows the user to specify how finely to bin the data; any divisor of 60 is allowed in options c(1, 2, 3, 4, 5, 6, 10, 12, 15, 20, 30, 60). The `sun.lines` and `sun.linetypes` arguments allow the user to customize and graph lines indicating sunrise, sunset, dawn, dusk, and several other options detailed in the helpfile. If using these arguments, the `latitude` and `longitude` arguments are also required. 

In the below example, we graph Pacific-slope Flycatcher detections at Rivendell above a confidence threshold of 0.25, in 5 minute increments, under a sampling regime where acoustic monitoring was conducted from 1.5 hours before sunrise to 1.5 hours after, with 0 sampling hours around sunset. We also graph lines for dusk, dawn, sunrise, and sunset. 

```r

# Generate a heatmap at Rivendell for Pacific-slope Flycatcher
# Set comparable.color.breaks = FALSE to maximize contrast in a single species map
birdnet_heatmap_time(
  data = dat,
  common.name = 'Pacific-slope Flycatcher',
  locationID = 'Rivendell',
  conf.threshold = 0.25,
  dates.sampled = exampleDatesSampled,
  hours.sampled = list(sunrise = c(1.5, 1.5), sunset = c(0, 0)),
  y.axis.limits = c(0, 23),
  julian.breaks = c(30, 60, 90, 120, 150, 180, 210, 240, 270),
  minute.timestep = 5,
  comparable.color.breaks = FALSE,
  tz.local = 'America/Los_angeles',
  latitude = 46.1646,
  longitude = -123.77955,
  sun.lines = c('dusk', 'dawn', 'sunrise', 'sunset'),
  sun.linetypes = c('dotdash', 'longdash', 'dotted', 'solid')
)

```

**Click image for a larger version.**
<p align="center">
<img src=https://github.com/nationalparkservice/NSNSDAcoustics/blob/main/images/heatmap-date-time.png 
     alt="Heatmap of detections of Pacific-slope Flycatcher at a monitoring location. The x axis shows date, the y axis shows time of day, and daily detection value is visualized from low (purple) to high (yellow)."><br>
</p>

In the second example, we loop through multiple species and set `comparable.color.breaks = TRUE` to plot according to a consistent color ramp. This option enables easier comparison between species. 

```r

# Generate heatmaps for several species with comparable.color.breaks == TRUE
# so that heatmap color scale is conserved for ease of interspecies comparison
sp <- c("Pacific Wren",
        "Pacific-slope Flycatcher",
        "Swainson's Thrush",
        "Wilson's Warbler")

for (i in 1:length(sp)) {

  print(paste0('Working on ', sp[i]))

  g <- birdnet_heatmap_time(
    data = dat,
    common.name = sp[i],
    locationID = 'Rivendell',
    conf.threshold = 0.1,
    dates.sampled = exampleDatesSampled,
    hours.sampled = list(sunrise = c(1.5, 1.5), sunset = c(0, 0)),
    y.axis.limits = c(3, 10),
    julian.breaks = c(30, 60, 90, 120, 150, 180, 210, 240, 270),
    minute.timestep = 1,
    plot.title = sp[i],
    comparable.color.breaks = TRUE,
    tz.local = 'America/Los_angeles',
    latitude = 46.1646,
    longitude = -123.77955,
    sun.lines = c('dawn', 'sunrise'),
    sun.linetypes = c('longdash', 'solid')
  )

  print(g)

}

```

**Click image for a larger version.**
<p align="center">
<img src=https://github.com/nationalparkservice/NSNSDAcoustics/blob/main/images/heatmap-date-time-loop.png 
     alt="Heatmaps of detections of focal species at a monitoring location. The x axis shows date, the y axis shows time of day, and daily detection value is visualized from low (purple) to high (yellow)."><br>
</p>


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
tuneR::writeWave(
  object = exampleAudio1,
  filename = 'example-input-directory/Rivendell_20210623_113602.wav'
)

tuneR::writeWave(
  object = exampleAudio2,
  filename = 'example-input-directory/Rivendell_20210623_114602.wav'
)

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
 timezone = 'GMT'
)

```

Once you feel confident that you have parameterized accurately, run the function in batch mode by setting `test.file = FALSE`. The example below provides progress feedback and takes a few moments to run. Once complete, we can view the NVSPL table outputs. Column names are described in the helpfile.

```r

# Perform wave_to_nvspl in batch mode (test.file = FALSE)
wave_to_nvspl(
 input.directory = 'example-input-directory',
 data.directory = FALSE,
 test.file = FALSE,
 project = 'testproject',
 timezone = 'GMT'
)

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
  write.table(
    x = exampleNVSPL[[i]],
    file = paste0('example-input-directory/', names(exampleNVSPL)[i]),
    sep = ',',
    quote = FALSE
  )
}

```

Now we are positioned to run `nvspl_to_ai()`. `input.directory` indicates the top-level input directory path, `output.directory` specifies where csv results should be stored, and  `project` allows the user to input a project name. The project name will be used to create a "params" file that will save parameter inputs in a file for posterity. Additional arguments are described in the helpfile; note that there are several default values in this function customized for NSNSD default settings.

```r

# Run nvspl_to_ai to generate acoustic indices csv for example NVSPL files
nvspl_to_ai(
  input.directory = 'example-input-directory',
  output.directory = 'example-output-directory',
  project = 'example-project'
)

# View Results
(ai.results <- read.csv(
  list.files(path = 'example-output-directory',
             pattern = '.csv', full.names = TRUE))
)

```

Finally, we clean up by deleting all example files. 

```r

# Delete all temporary example files when finished
unlink(x = 'example-input-directory', recursive = TRUE)
unlink(x = 'example-output-directory', recursive = TRUE)

```
