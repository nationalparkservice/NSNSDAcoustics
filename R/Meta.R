# FUNCTION META ================================================================

# Internal function called by Wave_To_NVSPL

# Function for batch processing of passive acoustic data.

# Computes calibrated acoustic spectra from WAV audio files.

# This code accompanies the manuscript:

#   Merchant et al. (2015). Measuring Acoustic Habitats. Methods
#   in Ecology and Evolution

# and follows the equations presented in Appendix S1. It is not necessarily
# optimised for efficiency or concision.

# See Appendix S1 of the above manuscript for detailed instructions


# Copyright (c) 2014 The Authors.

# Author: Nathan D. Merchant. Last modified 22 Sep 2014

# INITIALISE

#rm(list = setdiff(ls(), lsf.str()))
#clear workspace variables

#' @name Meta
#' @title Internal function from PAMGuide code.
#' @description Internal function from PAMGuide code.
#' @import svDialogs tuneR
#' @include Meta.R PAMGuide.R PAMGuide_Meta.R
#' @export
#' @keywords internal
#'

Meta <- function(...,atype='TOL',plottype='Both',envi='Air',
                 calib=0,stype = 'MF',Si=-159,Mh=-36,G=0,vADC=1.414,r=50,
                 N=Fs,winname='Hann',lcut=Fs/N,hcut=Fs/2,timestring="",outwrite=1,
                 welch="",chunksize="",linlog = "Log",
                 filenms = filenms) # cb - had to add filenms arg in to get it to work
{
  #define Meta function
  # source('PAMGuide_Meta.R')	#load PAMGuide_Meta function
  # source('Viewer.R')				#load Viewer function

  # SELECT TARGET FOLDER CONTAINING WAV FILES

  #fullfile <- new.folder #file.choose()		#select a file in the target folder

  tglo = proc.time()		 #start global timer

  #files <- dir(path = dirname(fullfile), pattern = ".*\\.wav$",full.names = TRUE)
  #files = list.files(fullfile, pattern = '.*.wav', recursive=T, full.names=T)
  files = filenms
  fullfile = files[1]

  #get names of WAV files in target folder
  fIN <- readWave(fullfile,header = TRUE)
  Fs = fIN[[1]]					#get sampling frequency of files in from selected file
  nf = length(files)		#number of files
  cat('No. of WAV files in selected directory: ',nf,'\n')


  # CREATE NEW FOLDER FOR ANALYSIS FILES

  if (calib == 1) {calstring <- 'Abs'} else {calstring <- 'Rel'}

  newdir <- paste('Meta_',basename(dirname(fullfile)),'_',atype,'_',calstring,'_',N,'pt',winname,'Window','_',r*100,'pcOlap',sep="")

  newpath <- file.path(dirname(fullfile),newdir)

  suppressWarnings(dir.create(newpath))


  # ANALYSE FILES IN TARGET FOLDER USING PAMGUIDE_META

  conkcomp <- 1					#initialise concatenation compatibility of arrays

  for (i in 1:nf) #loop through files and analyse
  {
    tana = proc.time()			#start analysis timer for current file
    nowfile <- files[i]			#current file name

    # set PAMGuide to display parameters for first iteration only
    if (i == 1){disppar <- 1} else {disppar <- 1}

    # execute PAMGuide_Meta
    A <- PAMGuide_Meta(
      nowfile,atype=atype,plottype='None',envi=envi,calib=calib,
      stype=stype,Si=Si,Mh=Mh,G=G,vADC=vADC,r=r,N=N,winname=winname,
      lcut=lcut,hcut=hcut,timestring=timestring,outdir=newpath,
      outwrite=outwrite,disppar=disppar,welch=welch,
      chunksize=chunksize,linlog=linlog)

    if (i == 1 && conkcomp == 1){conc <- A}
    #initialise concatenated array on first iteration
    else if (i > 1 && length(A[1,]) == length(conc[1,]))
    {
      dimA <- dim(A) #concatenate newly analysed file on each subsequent iteration

      if (timestring == "")
      {	#if not time-stamped, ensure time vector is cumulative
        tdiff <- conc[3,1] - conc[2,1]
        A[2:dimA[1],1] <- A[2:dimA[1],1] + conc[length(conc[,1]),1] + tdiff
      }
      conc <- rbind(conc,A[2:dimA[1],])
    }
    # concatenate arrays
    else {conkcomp == 0;cat('Sample rates of WAV files in selected folder not equal. Cannot concatenate.')}

    cat('File ',i,'/',nf,': ',basename(nowfile),' analysed in ',(proc.time()-tana)[3],' s\n',sep="")
  }

  ofile <- paste(gsub(".csv","",file.path(dirname(fullfile),'Conk')),'_',
                 basename(dirname(fullfile)),'_',calstring,'_',atype,'_',N,'pt',
                 winname,'Window_',round(r*100),'pcOlap.csv',sep = "")
  cat('Analysis complete in ',(proc.time()-tglo)[3],' s\n',sep="")

  # PLOT OUTPUT
  #Viewer(fullfile=conc,ifile=basename(ofile),linlog=linlog)
  # ^^ CB: why is this commented out? Is Viewer function meant to never be used?

  # WRITE OUTPUT FILE FOR CONCATENATED ARRAY

  cat('Writing concatenated output array...')
  twri = proc.time()
  write.table(conc,file = ofile,row.names=FALSE,col.names=FALSE,quote=FALSE,sep=",",dec=".")

  cat('done in ',(proc.time()-twri)[3],' s\n',sep="")
}



