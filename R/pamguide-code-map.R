# PAMGuide Function Code Map
# 2021-12-15

# This document shows a skeleton of the functions used in pamguide. It is meant only to show the function arguments (inputs), outputs, and linkages between functions.

# These functions are using V12_noChunk version of PAMGuide available under NSNSD Teams

# There are 5 functions in this document:
# 4 are internal:
#   META
#   PAMGuide_Meta
#   Viewer
#   PAMGuide

# 1 meant for external use:
#  SongMeter_To_NVSPL

# library depends: svDialogs, tuneR


# WRAPPER FXN FOR USER-FRIENDLY PROCESSING
SongMeter_To_NVSPL <- function(input.directory,
                               data.directory = TRUE,
                               test.file = FALSE,
                               project,
                               PAMvers = 'V12_noChunk',
                               instrum = "SM4",
                               filext = "_%Y%m%d_%H%M%S.wav",
                               filpat = ".+\\d{8}_\\d{6}.wav",
                               mhset = -35,
                               Gset = 16,
                               vADCset = 1,
                               enviset = "Air",
                               envir = 2,
                               rescWat = 0,
                               timezone = "GMT"
)
{

  # If test.file = TRUE
  # Runs PAMGuide.R


  # If test.file = FALSE
  # Runs Meta, which runs PAMGuide_Meta.R


}


# Function for batch processing of passive acoustic data.
Meta <- function(...,atype='TOL',plottype='Both',envi='Air',
                 calib=0,stype = 'MF',Si=-159,Mh=-36,G=0,vADC=1.414,r=50,
                 N=Fs,winname='Hann',lcut=Fs/N,hcut=Fs/2,timestring="",outwrite=1,
                 welch="",chunksize="",linlog = "Log",
                 filenms = filenms) # cb - had to add filnms arg in to get it to work
{


  A <- PAMGuide_Meta(nowfile,atype=atype,plottype='None',envi=envi,calib=calib,
                     stype=stype,Si=Si,Mh=Mh,G=G,vADC=vADC,r=r,N=N,winname=winname,
                     lcut=lcut,hcut=hcut,timestring=timestring,outdir=newpath,
                     outwrite=outwrite,disppar=disppar,welch=welch,
                     chunksize=chunksize,linlog=linlog)

  # CALL TO VIEWER IS COMMENTED OUT
  #Viewer(fullfile=conc,ifile=basename(ofile),linlog=linlog)

  # Output of this fxn is concatenated array
  # 'ofile' object is written based on 'conc' which is an rbind of the 'A' output from PAMGuide_Meta

}

# Function called by Meta.R for batch processing of passive acoustic data.
PAMGuide_Meta <- function(fullfile,...,atype='TOL',plottype='Both',envi='Air',
                          calib=0,ctype = 'TS',Si=-159,Mh=-36,G=0,vADC=1.414,r=50,
                          N=Fs,winname='Hann',lcut=Fs/N,hcut=Fs/2,timestring="",
                          outdir=dirname(fullfile),outwrite=0,disppar=1,welch="",
                          chunksize="",linlog = "Log"){

  # Creates 'A' output array written to CSV file if outwrite = 1
  # If outwrite = 0, returns 'A'

}

# Computes calibrated or relative acoustic metrics from WAV audio files.
PAMGuide <- function(...,atype='PSD',plottype='Both',envi='Air',calib=0,
                     ctype = 'TS',Si=-159,Mh=-36,G=0,vADC=1.414,r=50,N=Fs,
                     winname='Hann',lcut=Fs/N,hcut=Fs/2,timestring="",
                     outdir=dirname(fullfile),outwrite=0,disppar=1,welch="",
                     chunksize="",linlog = "Log", WAVFiles = WAVFiles){

  # CALL TO VIEWER IS COMMENTED OUT
  # Viewer(fullfile=A,plottype=plottype,ifile=ifile,linlog=linlog)

  # Creates 'A' output array written to CSV file if outwrite = 1
  # If outwrite = 0, returns 'A'

  # Basically this function is only used if wrapper function indicates "test.file" mode
  # For actual processing of files, PAMGuide_Meta is used
}

# Function to plot data analysed in PAMGuide.R and Meta.R.
Viewer <- function(...,plottype='Both',fullfile="",ifile="",linlog="Log"){

  # needs atype arg
}

# So basically, none of these is even using the viewer function.
