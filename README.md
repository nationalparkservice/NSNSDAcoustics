# NSNSDAcoustics

This repository provides a place for NSNSD staff to work on a few typical acoustics workflows. Primarily, it will serve as a place to modernize existing PAMGuide code for converting audio to NVSPLs.

Users should clone this repository (set it up as an R project package) and connect it to Github. While the package is in development, use the Build > Install and Restart buttons in RStudio to install the package locally on your machine.

The R script file `Intro-Demo.R` walks through a basic example to get started on stepping into the `SongMeter_To_NVSPL` wrapper function, which uses other internal PAMGuide functions as mapped out below: 

<img src=https://raw.githubusercontent.com/nationalparkservice/NSNSDAcoustics/main/PAMGuide-map.drawio.png width=700><br> Figure 1. Basic draft map of current PAMGuide code contained within NSNSDAcoustics.
