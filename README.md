# NSNSDAcoustics

This repository provides a place for NSNSD staff to develop and modernize several bioacoustics workflows, including,

1) `wave_to_nvspl`: a wrapper function for PAMGuide code that converts wave audio to NVSPLs.
2) `nvspl_to_ai`: a function for converting NVSPL files into acoustic indices
3) The BirdNET function family: several functions for manipulation of [BirdNET data](https://birdnet.cornell.edu/).

Contributors should clone this repository (set it up as an R project package) and connect it to GitHub (see [Happy Git with R](https://happygitwithr.com/) for tips). While the package is in development, use the Build > Install and Restart buttons in RStudio to install the package locally on your machine.

The R script file `Intro-Demo.R` walks through a few basic workflow examples. **MORE INFO TO COME.**

**All documentation and code is currently under development.**

## Converting wave audio files to NVSPL with `wave_to_nvspl()`

Define NVSPLs. What are they and why do we convert to them?

## Converting NVSPL files to acoustic indices with `nvspl_to_ai()`

Explain about 1/3 octave bands.
Why use this wave --> NVSPL --> acoustic indices workflow instead of using existing acoustic index functions that take wave data directly?

## Using `birdnet_run()` functions to process audio data through [BirdNET](https://birdnet.cornell.edu/)

`birdnet_run()` will not work unless you have BirdNET installed and have made necessary modifications. [Here is a method for installing BirdNET onto a Windows machine and running it from RStudio.](https://cbalantic.github.io/Install-BirdNET-Windows-RStudio)

You may not want to process files through RStudio, or you may already have BirdNET-Lite CSV results in hand, in which case you can skip ahead to the next functions. 

## Using `birdnet_()` functions to assess BirdNET results

* `birdnet_format_csv()` reformats the raw BirdNET results...
* `birdnet_gather_results()` gathers all BirdNET CSV results from a desired folder into a user-friendly data.table / data.frame
* `birdnet_species_counts()` summarizes count data from a data.table of detected species over a selected time unit
* `birdnet_verify()` allows the user to manually verify a selected subset of detections based on a user-input library of classification options 
