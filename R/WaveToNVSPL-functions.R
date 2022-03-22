# WaveToNVSPL_functions

# These functions are using V12_noChunk version of PAMGuide available under NSNSD Teams

# There are 5 functions in this document:
# 4 are internal:
#   META
#   PAMGuide_Meta   ==> could rename to batch
#   Viewer
#   PAMGuide

# 1 meant for external use:
#  Wave_To_NVSPL

# depends: svDialogs, tuneR


# hopefully everything except the main function can just be internal fxns that don't require doc.



# FUNCTION META ================================================================

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
    A <- PAMGuide_Meta(nowfile,atype=atype,plottype='None',envi=envi,calib=calib,
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



# FUNCTION PAMGuide_Meta.R =====================================================

# Function called by Meta.R for batch processing of passive acoustic data.

# Computes calibrated acoustic spectra from WAV audio files.

# This code accompanies the manuscript:

#   Merchant et al. (2015). Measuring Acoustic Habitats. Methods
#   in Ecology and Evolution

# and follows the equations presented in Appendix S1. It is not necessarily
# optimised for efficiency or concision.

# See Appendix S1 of the above manuscript for detailed instructions

# Copyright (c) 2014 The Authors.

# Author: Nathan D. Merchant. Last modified 22 Sep 2014

## Begin PAMGuide_Meta

PAMGuide_Meta <- function(fullfile,...,atype='TOL',plottype='Both',envi='Air',
                          calib=0,ctype = 'TS',Si=-159,Mh=-36,G=0,vADC=1.414,r=50,
                          N=Fs,winname='Hann',lcut=Fs/N,hcut=Fs/2,timestring="",
                          outdir=dirname(fullfile),outwrite=0,disppar=1,welch="",
                          chunksize="",linlog = "Log"){

  graphics.off()							#close plot windows
  aid <- 0										#reset metadata code
  if (calib == 0) {aid <- aid + 20				#add calibration element to metadata code
  } else {aid <- aid + 10}
  if (timestring != ""){aid <- aid + 1000} else {aid <- aid + 2000}	#add time stamp element to metadata code


  ## Get file info

  ifile <- basename(fullfile)						      #file name
  fIN <- readWave(fullfile,header = TRUE)			#read file header
  Fs <- fIN[[1]]								            	#sampling frequency
  Nbit <- fIN[[3]]								            #bit depth
  xl <- fIN[[4]]									            #length of file in samples
  xlglo <- xl										              #back-up file length


  ## Read time stamp data if provided-----------------------------------------
  if (timestring != "")
  {
    tstamp <- as.POSIXct(strptime(ifile,timestring) ,origin="1970-01-01")
    if (disppar == 1)
    {
      cat('Time stamp start time: ',format(tstamp),'\n')
    }
  }
  if (timestring == "") tstamp <- NULL		#compute time stamp in R POSIXct format

  #   if (timestring != "") {
  #     tstamp <- as.POSIXct(strptime(ifile,timestring) ,origin="1970-01-01")
  #     if (disppar == 1){
  #       cat('Time stamp start time: ',format(tstamp),'\n')
  #     }
  #   }
  #   else{ tstamp <- NULL }		#compute time stamp in R POSIXct format


  ## Display user-defined settings------------------------------------------

  if (disppar == 1){
    cat('Analysis type:',atype,'\n')
    cat('Plot type:',plottype,'\n')
    if (calib == 1){
      if (ctype == 'EE'){
        cat('End-to-end system sensitivity =',sprintf('%.01f',Si),'dB\n')
        if (envi == 'Wat') {cat('In-air measurement\n')}
        if (envi == 'Wat') {cat('Underwater measurement\n')}}
      if (ctype == 'RC'){
        cat('System sensitivity of recorder (excluding transducer) =',sprintf('%.01f',Si),'dB\n')}
      if (ctype == 'TS' || ctype == 'RC'){
        if (envi == 'Air') {cat('In-air measurement\n')
          cat('Microphone sensitivity:',Mh,'dB re 1 V/Pa\n')
          Mh <- Mh - 120}		#convert to dB re 1 V/uPa
        if (envi == 'Wat') {cat('Underwater measurement\n')
          cat('Hydrophone sensitivity:',Mh,'dB re 1 V/uPa\n')}}
      if (ctype == 'TS'){
        cat('Preamplifier gain:',G,'dB\n')
        cat('ADC peak voltage:',vADC,'V\n')}
    } else {cat('Uncalibrated analysis. Output in relative units.\n')
    }
    cat('Time segment length:',N,'samples =',N/Fs,'s\n')
    cat('Window function:',winname,'\n')
    cat('Window overlap:',r,'%\n')
  }
  r<-r/100

  ## Read input file

  if (chunksize == ""){nchunks = 1
  } else if (chunksize != "") {
    nchunks <- ceiling(xl/(Fs*as.numeric(chunksize)))	#number of chunks of length chunksize in file
  }

  for (q in 1:nchunks){
    if (nchunks == 1){
      #t1=proc.time()									#start timer
      #cat('Loading input file... ')
      xbit <- readWave(fullfile)						#read file
      #cat('done in',(proc.time()-t1)[3],'s.\n')
      xbit <- xbit@left/(2^(Nbit-1))					#convert to full scale (+/- 1) via bit depth
    } else if (nchunks > 1) {
      if (q == nchunks){
        xbit <- readWave(fullfile,from=((q-1)*as.numeric(chunksize)*Fs+1),to=xlglo,units="samples")
        xbit <- xbit@left/(2^(Nbit-1))					#convert to full scale (+/- 1) via bit depth
        xl <- length(xbit)
      } else {
        xbit <- readWave(fullfile,from=((q-1)*as.numeric(chunksize)*Fs+1),to=(q*as.numeric(chunksize)*Fs),units="samples")
        xbit <- xbit@left/(2^(Nbit-1))					#convert to full scale (+/- 1) via bit depth
        xl <- length(xbit)
      }
    }

    if (envi == 'Air'){pref<-20; aid <- aid+100}	#set reference pressure depending for in-air or underwater
    if (envi == 'Wat'){pref<-1; aid <- aid+200}


    ## Compute system sensitivity if provided

    if (calib == 1){
      if (ctype == 'EE') {		#'EE' = end-to-end calibration
        S <- Si}
      if (ctype == 'RC') {		#'RC' = recorder calibration with separate transducer sensitivity defined by Mh
        S <- Si + Mh}
      if (ctype == 'TS') {		#'TS' = manufacturer's specifications
        S <- Mh + G + 20*log10(1/vADC);		#EQUATION 4
      }

      if (disppar == 1){cat('System sensitivity correction factor, S = ',sprintf('%.01f',S),' dB\n')}
    }
    else {S <- 0}

    ## Compute waveform if selected

    if (atype == 'Waveform') {
      if (calib == 1){
        a <- xbit/(10^(S/20)) 				#EQUATION 21
      } else {a <- xbit/(max(xbit))}
      t <- seq(1/Fs,length(a)/Fs,1/Fs)	#time vector
    }

    ## Compute DFT-based metrics if selected

    if (atype != 'Waveform') {

      # Divide signal into data segments (corresponds to EQUATION 5)
      N = round(N)
      nsam = ceiling((xl)-r*N)/((1-r)*N)
      xgrid <- matrix(nrow = N,ncol = nsam)
      for (i in 1:nsam) {
        loind <- (i-1)*(1-r)*N+1
        hiind <- (i-1)*(1-r)*N+N
        xgrid[,i] = xbit[loind:hiind]
      }

      M <- length(xgrid[1,])

      # Apply window function (corresponds to EQUATION 6)
      if (winname == 'Rectangular') {			#rectangular (Dirichlet) window
        w <- matrix(1,1,N)
        alpha <- 1 }					#scaling factor
      if (winname == 'Hann') {			#Hann window
        w <- (0.5 - 0.5*cos(2*pi*(1:N)/N))
        alpha <- 0.5 }					#scaling factor
      if (winname == 'Hamming') {			#Hamming window
        w <- (0.54 - 0.46*cos(2*pi*(1:N)/N))
        alpha <- 0.54 }					#scaling factor
      if (winname == 'Blackman') {		#Blackman window
        w <- (0.42 - 0.5*cos(2*pi*(1:N)/N) + 0.08*cos(4*pi*(1:N)/N))
        alpha <- 0.42 }					#scaling factor

      xgrid <- xgrid*w/alpha


      #Compute DFT (corresponds to EQUATION 7)
      X <- abs(mvfft(xgrid))

      #Compute power spectrum (EQUATION 8)
      P <- (X/N)^2

      #Compute single-side power spectrum (EQUATION 9)
      Pss <- 2*P[0:round(N/2)+1,]

      #Compute frequencies of DFT bins
      f <- floor(Fs/2)*seq(1/(N/2),1,len=N/2)
      flow <- which(f >= lcut)[1]
      fhigh <- max(which(f <= hcut))
      nf <- length(f)
      f <- f[flow:fhigh]

      #Compute PSD in dB if selected
      if (atype == 'PSD') {
        B <- (1/N)*(sum((w/alpha)^2))		#noise power bandwidth (EQUATION 12)
        delf <- Fs/N;						#frequency bin width
        a <- 10*log10((1/(delf*B))*Pss[flow:fhigh,]/(pref^2))-S
      }									#PSD (EQUATION 11)

      #Compute power spectrum in dB if selected
      if (atype == 'PowerSpec') {
        a <- 10*log10(Pss[flow:fhigh,]/(pref^2))-S
      }									#EQUATION 10

      #Compute broadband level if selected
      if (atype == 'Broadband') {
        a <- 10*log10(colSums(Pss[flow:fhigh,])/(pref^2))-S
      }									#EQUATION 17

      #Compute 1/3-octave band levels if selected
      if (atype == 'TOL') {
        if (lcut <25){
          lcut <- 25}

        #Generate 1/3-octave freqeuncies
        lobandf <- floor(log10(lcut))	#lowest power of 10 for TOL computation
        hibandf <- ceiling(log10(hcut))	#highest power of 10 for TOL computation
        nband <- 10*(hibandf-lobandf)+1	#number of 1/3-octave bands
        fc <- matrix(0,nband)			#initialise 1/3-octave frequency vector
        fc[1] <- 10^lobandf;

        #Calculate centre frequencies (corresponds to EQUATION 13)
        for (i in 2:nband) {
          fc[i] <- fc[i-1]*10^0.1}
        fc <- fc[which(fc >= lcut)[1]:max(which(fc <= hcut))]

        nfc <- length(fc)				#number of 1/3-octave bands

        #Calculate boundary frequencies of each band (EQUATIONS 14-15)
        fb <- fc*10^-0.05				#lower bounds of 1/3-octave bands
        fb[nfc+1] <- fc[nfc]*10^0.05	#upper bound of highest band
        if (max(fb) > hcut) {			#if upper bound exceeds highest
          nfc <- nfc-1				# frequency in DFT, remove
          fc <- fc[1:nfc]}

        #Calculate TOLs (corresponds to EQUATION 16)
        P13 <- matrix(nrow = M,ncol = nfc)
        for (i in 1:nfc) {
          fli <- which(f >= fb[i])[1]
          fui <- max(which(f < fb[i+1]))
          for (k in 1:M) {
            fcl <- sum(Pss[fli:fui,k])
            P13[k,i] <- fcl
          }
        }
        a <- t(10*log10(P13/(pref^2))) - S
      }

      #cat('done in',(proc.time()-tana)[3],'s.\n')

      # Compute time vector
      tint <- (1-r)*N/Fs
      ttot <- M*tint-tint
      t <- seq(0,ttot,tint)

      if (nchunks>1){
        if (q == 1){
          newa <- a
          newt <- t
          cat('Analysing in',nchunks,'chunks. Analysing chunk 1')
        } else if (q > 1){
          dima <- dim(a)
          newa <- cbind(newa,a)
          if (timestring != ""){
            #             newt <- c(newt,t)
            #           } else {
            newt <- c(newt,t+(q-1)*as.numeric(chunksize))
          }
          cat('',q)
        }
      }
    }
  }
  if (nchunks>1){a <- newa
  t <- newt
  cat('\n')
  }

  # If not calibrated, scale relative dB to zero
  if (calib == 0) {a <- a-max(a)}

  if (!is.null(tstamp)){t <- t+tstamp

  tdiff <- max(t)-min(t)					#define time format for x-axis of time plot
  if (tdiff < 10){
    tform <- "%H:%M:%S:%OS3"}
  else if (tdiff > 10 & tdiff < 86400){
    tform <- "%H:%M:%S"}
  else if (tdiff > 86400 & tdiff < 86400*7){
    tform <- "%H:%M \n %d %b"}
  else if (tdiff > 86400*7){tform <- "%d %b %y"}
  }

  ## Construct output array

  if (atype == 'PSD' | atype == 'PowerSpec') {
    A <- cbind(t,t(a))
    A <- rbind(c(0,f),A)
    if (atype == 'PSD'){aid <- aid + 1}
    if (atype == 'PowerSpec'){aid <- aid + 2}
    A[1,1] <- aid
  }

  if (atype == 'TOL') {
    A <- cbind(t,t(a))
    A <- rbind(c(0,fc),A)
    aid <- aid + 3
    A[1,1] <- aid
    f <- fc
  }

  if (atype == 'Broadband') {
    A <- t(rbind(t,a))
    aid <- aid + 4
    A[1,1] <- aid
  }

  if (atype == 'Waveform') {
    A <- t(rbind(t,a))		#define output array
    A <- rbind(c(0,0),A)	#add zero top row for metadata
    aid <- aid + 5			#add index to metadata for Waveform
    A[1,1] <- aid			#encode output array with metadata
  }

  ## Reduce time resolution if selected

  dimA <- dim(A)

  if (welch != "" && atype != "Waveform"){
    lout <- ceiling(dimA[1]/welch)+1
    AWelch <- matrix(, nrow = lout, ncol = dimA[2])
    AWelch[1,] <- A[1,]
    tint <- A[3,1] - A[2,1]
    if (disppar == 1){cat('Welch factor =',welch,'x\nNew time resolution =',welch,'(Welch factor) x',N/Fs,'s (time segment length) x',r*100,'% (overlap) =',welch*tint,'s\n')}
    if (lout == 2){
      AWelch[2,] <- 10*log10(mean(10^A[2:dimA[1]]/10))
    }	else {
      for (i in 2:lout) {
        stt <- A[2,1] + (i-2)*tint*welch
        ett <- stt + welch*tint
        stiv <- which(A[2:dimA[1]]>=stt)
        sti <- min(stiv)+1
        etiv <- which(A[2:dimA[1]]<ett)
        eti <- max(etiv)+1
        nowA <- 10^(A[sti:eti,]/10)
        AWelch[i,] <- 10*log10(rowMeans(t(nowA)))
        AWelch[i,1] <- stt+tint*welch/2
      }
    }
    A <- AWelch
    A[1,1] <- aid
    dimA <- dim(A)
    t <- A[2:dimA[1],1]
    f <- A[1,2:dimA[2]]
    a <- t(A[2:dimA[1],2:dimA[2]])

  }


  ## Plot time-domain analyses if selected

  jet.colors <- colorRampPalette(c("#00007F","blue","#007FFF","cyan","#7FFF7F","yellow","#FF7F00","red","#7F0000"))
  if (plottype == 'Time' | plottype == 'Both'){
    cat('Plotting...')
    tplot=proc.time()
    options(scipen = 7)


    if (atype == 'PSD') {	 						#spectrogram
      if (!is.null(tstamp)){
        image(t,f,t(a),ylim = c(lcut,hcut),yaxt="n",log = "y",col = jet.colors(512),main=paste('PSD spectrogram of ',ifile),xlab="Time",ylab="Frequency [ Hz ]",xaxt="n")}
      else {image(t,f,t(a),ylim = c(lcut,hcut),yaxt="n",log = "y",col = jet.colors(512),main=paste('PSD spectrogram of ',ifile),xlab="Time [ s ]",ylab="Frequency [ Hz ]")}
      y1 <- floor(log10(range(f)))
      pow <- seq(y1[1], y1[2]+1)
      ticksat <- as.vector(sapply(pow, function(p) (1:10)*10^p))
      labsat <- as.vector(sapply(pow, function(p) 10^p))
      axis(2, 10^pow,labels = NA)
      axis(2, ticksat,labels = NA, tcl=-0.25, lwd=0, lwd.ticks=1)
      axis(2, labsat, tcl=-0.25, lwd=0, lwd.ticks=1)
    }

    if (atype == 'PowerSpec') {				#spectrogram
      if (!is.null(tstamp)){image(t,f,t(a),ylim = c(lcut,hcut),yaxt="n",log = "y",col = jet.colors(512),main=paste('Power spectrum spectrogram of ',ifile),xlab="Time",ylab="Frequency [ Hz ]",xaxt="n")}
      else {image(t,f,t(a),ylim = c(lcut,hcut),yaxt="n",log = "y",col = jet.colors(512),main=paste('Power spectrum spectrogram of ',ifile),xlab="Time [ s ]",ylab="Frequency [ Hz ]")}
      y1 <- floor(log10(range(f)))
      pow <- seq(y1[1], y1[2]+1)
      ticksat <- as.vector(sapply(pow, function(p) (1:10)*10^p))
      labsat <- as.vector(sapply(pow, function(p) 10^p))
      axis(2, 10^pow,labels = NA)
      axis(2, ticksat,labels = NA, tcl=-0.25, lwd=0, lwd.ticks=1)
      axis(2, labsat, tcl=-0.25, lwd=0, lwd.ticks=1)
    }


    if (atype == 'TOL') {
      if (!is.null(tstamp)){image(t,fc,t(a),ylim = c(min(fb),fb[nfc+1]),yaxt="n",log = "y",col = jet.colors(512),main=paste('TOL spectrogram of ',ifile),xlab="Time",ylab="Frequency [ Hz ]",xaxt="n")}
      else {image(t,fc,t(a),ylim = c(min(fb),fb[nfc+1]),yaxt="n",log = "y",col = jet.colors(512),main=paste('TOL spectrogram of ',ifile),xlab="Time [ s ]",ylab="Frequency [ Hz ]")}
      y1 <- floor(log10(range(fc)))
      pow <- seq(y1[1], y1[2]+1)
      ticksat <- as.vector(sapply(pow, function(p) (1:10)*10^p))
      labsat <- as.vector(sapply(pow, function(p) 10^p))
      axis(2, 10^pow,labels = NA)
      axis(2, ticksat,labels = NA, tcl=-0.25, lwd=0, lwd.ticks=1)
      axis(2, labsat, tcl=-0.25, lwd=0, lwd.ticks=1)
    }

    if (atype == 'Broadband') {
      if (calib == 1) {if (envi == 'Air'){YLAB <- expression(paste("Broadband SPL [ dB re 20 ",mu,"Pa ]"))
      } else {YLAB <- expression(paste("Broadband SPL [ dB re 1 ",mu,"Pa ]"))}
      } else {YLAB <- expression(paste("Relative SPL [ dB ]"))}
      if (!is.null(tstamp)){plot(t,a,type = "l",xlab="Time",ylab="",xaxt="n")
      } else {plot(t,a,type = "l",xlab="Time [ s ]",ylab="")}
      if (envi == 'Air'){mtext(text=YLAB, side=2, line=2)}
      if (envi == 'Wat'){mtext(text=YLAB, side=2, line=2)}
    }

    if (atype == 'Waveform') {
      if (calib == 1) {YLAB <- "Pressure [ Pa ]"
      } else {YLAB <- "Relative pressure";a <- a*1e6}
      if (!is.null(tstamp)) {
        plot(t,a/1e6,xlab="Time",ylab=YLAB,type = "l",ylim = c(-max(a/1e6),max(a/1e6)),xaxt="n")
        tck <- axis(1, labels=FALSE)
        axis.POSIXct(1,as.POSIXct.numeric(tck,origin="1970-01-01"),format=tform,label=TRUE)
      } else {plot(t,a/1e6,xlab="Time [ s ]",ylab=YLAB,type = "l",ylim = c(-max(a/1e6),max(a/1e6)))}
    }

    #format x axis if time stamp is provided
    if (!is.null(tstamp)){tck <- axis(1, labels=FALSE)
    axis.POSIXct(1,as.POSIXct.numeric(tck,origin="1970-01-01"),format=tform,label=TRUE)}
    cat('done in',(proc.time()-tplot)[3],'s.\n')
  }



  ## Write output array to CSV file if selected

  A <- data.matrix(A, rownames.force = NA)
  if (outwrite == 1){
    #if (disppar == 1){cat('Writing output file...')
    #twrite <- proc.time()}
    if (atype == 'Waveform') {
      ofile <- paste(gsub(".wav","",file.path(outdir,basename(fullfile))),'_',atype,'.csv',sep = "")
      write.table(A,file = ofile,row.names=FALSE,quote=FALSE,col.names=FALSE,sep=",")
    }
    if (atype != 'Waveform') {
      ofile <- paste(gsub(".wav","",file.path(outdir,basename(fullfile))),'_',atype,'_',N,'samples',winname,'Window_',round(r*100),'PercentOverlap.csv',sep = "")
      write.table(A,file = ofile,row.names=FALSE,quote=FALSE,col.names=FALSE,sep=",")
    }
    #if (disppar == 1){cat('done in',(proc.time()-twrite)[3],'s.\n')}
  }

  ## Statistical analysis

  if (atype != 'Waveform'){
    if (plottype == 'Stats' | plottype == 'Both'){
      if (atype != 'Broadband'){
        M <- length(A[,1])-1

        cat('Computing noise level statistics...')
        tstat <- proc.time()
        dev.new()							#open new plot window

        # Compute mean level and percentiles
        RMSlevel <- 10*log10(rowMeans(10^(a/10)))
        #calculate RMS mean (EQUATION 18)

        p <- apply(a, 1, quantile, probs = c(0.01,0.05,0.5,0.9,0.95),  na.rm = TRUE)
        #percentile levels
        mindB <- floor(min(a)/10)*10		#minimum dB level rounded down to nearest 10
        maxdB <- ceiling(max(a)/10)*10		#maximum dB level rounded up to nearest 10

        # Compute SPD if more than 1000 data points in time domain
        if (M >= 1000) {
          hind <- 0.1							#histogram bin width for probability densities (PD)
          dbvec = seq(mindB,maxdB,hind)		#dB values at which to calculate empirical PD

          #Compute SPD array (corresponds to EQUATION 19)
          d <- t(apply(a,1,function(x) x<- hist(x,breaks = dbvec,plot = FALSE)$counts))
          d <- d/(hind*M)

          #Plot data
          d[which(d>0.05)] <- 0.05			#saturate colour scale at PD = 0.05
          image(f,dbvec[1:length(dbvec)-1]-hind/2,d,zlim <- c(min(d[which(d>0)]),0.05),log = "x",xaxt="n",col = jet.colors(2^12),xlim<-c(min(f),max(f)),main=paste('Noise level statistics for ',ifile,'\nWindow length = ',N/Fs,' s = ',N,' samples'),xlab="Frequency [ Hz ]",ylab="")
        }
        if (M<1000){plot(f,RMSlevel,type="n",log = "x",xaxt="n",main=paste('Noise level statistics for ',ifile,'\nWindow length = ',N/Fs,' s = ',N,' samples'),xlab="Frequency [ Hz ]",ylab="",ylim=c(mindB,maxdB))}
        lines(f,p[5,])
        lines(f,p[4,])
        lines(f,p[3,])
        lines(f,p[2,])
        lines(f,p[1,])
        lines(f,RMSlevel,col = 'magenta')
        if (M<1000){legend('bottomleft',c("99%","95%","50%","5%","1%","RMS level"),lty=c(1),lwd=c(1),col=c("black","black","black","black","black","magenta"))}
        if (M>=1000){legend('bottomleft',c("SPD","99%","95%","50%","5%","1%","RMS level"),lty=c(1),lwd=c(1),col=c("blue","black","black","black","black","black","magenta"))}
        y1 <- floor(log10(range(f)))
        pow <- seq(y1[1], y1[2]+1)
        ticksat <- as.vector(sapply(pow, function(p) (1:10)*10^p))
        labsat <- as.vector(sapply(pow, function(p) 10^p))
        axis(1, 10^pow,label = NA)
        axis(1, ticksat,labels = NA, tcl=-0.25, lwd=0, lwd.ticks=1)
        axis(1, labsat, tcl=-0.25, lwd=0, lwd.ticks=1)
        if (atype == 'PSD'){
          if (calib == 1) {if (envi == 'Air'){YLAB <- expression(paste("PSD [ dB re 20 ",mu,"Pa"^2,"Hz"^{-1},"]"))} else {YLAB <- expression(paste("PSD [ dB re 1 ",mu,"Pa"^2,"Hz"^{-1},"]"))}
          } else {YLAB <- expression(paste("Relative PSD [ dB ]"))}
          mtext(text=YLAB, side=2, line=2)
        }
        if (atype == 'PowerSpec'){
          if (calib == 1) {if (envi == 'Air'){YLAB <- expression(paste("Power spectrum [ dB re 20 ",mu,"Pa"^2,"Hz"^{-1},"]"))} else {YLAB <- expression(paste("Power spectrum [ dB re 1 ",mu,"Pa"^2,"Hz"^{-1},"]"))}
          } else {YLAB <- expression(paste("Relative power spectrum [ dB ]"))}
          mtext(text=YLAB, side=2, line=2)
        }
        if (atype == 'TOL'){
          if (calib == 1) {if (envi == 'Air'){YLAB <- expression(paste("SPL [ dB re 20 ",mu,"Pa ]"))
          } else {YLAB <- expression(paste("SPL [ dB re 1 ",mu,"Pa ]"))}
          } else {YLAB <- expression(paste("Relative SPL [ dB ]"))}
          mtext(text=YLAB, side=2, line=2)
          cat('done in',(proc.time()-tstat)[3],'s.\n')
        }

        if (atype == 'Broadband'){
          RMSlevel <- 10*log10(mean(10^(a/10)))
          p <- apply(t(a), 1, quantile, probs = c(0:100)/100,  na.rm = TRUE)
          dev.new()							#open new plot window
          plot(p,c(0:100)/100,type="n",xlab="",ylab="Cumulative Distribution Function")
          lines(p,c(0:100)/100)
          if (calib == 1) {if (envi == 'Air'){XLAB <- expression(paste("Broadband SPL [ dB re 20 ",mu,"Pa ]"))
          } else {XLAB <- expression(paste("Broadband SPL [ dB re 1 ",mu,"Pa ]"))}
          } else {XLAB <- expression(paste("Relative SPL [ dB ]"))}
          mtext(text=XLAB, side=1, line=2)
        }
        if (M < 1000 & atype != 'Broadband') {
          cat('Too few time segments (M = ',M,', i.e. <1000) for SPD analysis: for SPD, use a longer file or shorter time segment length (N).\n')}
      }
    }
  }
  return(A)			#return output array
}


# FUNCTION Viewer.R ============================================================

# Function to plot data analysed in PAMGuide.R and Meta.R.

# This code accompanies the manuscript:

#   Merchant et al. (2015). Measuring Acoustic Habitats. Methods
#   in Ecology and Evolution

# and follows the equations presented in Appendix S1. It is not necessarily
# optimised for efficiency or concision.

#	See Appendix S1 of the above manuscript for detailed instructions

# Copyright (c) 2014 The Authors.

# Author: Nathan D. Merchant. Last modified 22 Sep 2014

Viewer <- function(...,plottype='Both',fullfile="",ifile="",linlog="Log"){

  graphics.off()

  ## Select and read input file

  nff <- max(nchar(fullfile))
  if (nff > 0){conk <- fullfile	#option for use by Meta.R
  } else if (nff == 0) {
    fullfile <- file.choose()					#choose file to view
    ifile <- basename(fullfile)					#file name

    cat('Reading selected file...')
    tglo <- proc.time()							#start file read timer

    conk <- as.matrix(read.csv(fullfile,colClasses="numeric",header=FALSE))

    #read selected file
    cat('file read in ',(proc.time()-tglo)[3],' s\n',sep="")
    #display time to read file
  }


  ## Interpret metadata code in selected file

  aid <- conk[1,1]							#get metadata code from file
  tstampid <- substr(aid,1,1)					#extract time stamp identifier
  enviid <- substr(aid,2,2)					#extract in-air/underwater identifier
  calibid <- substr(aid,3,3)					#extract calibrated/uncalibrated identifier
  atypeid <- substr(aid,4,4)					#extract analysis type identifier

  if (tstampid == 1){tstamp = 1} else {tstamp = ""}
  if (enviid == 1){envi = 'Air'	; pref <- 20			#assign PAMGuide variables envi, calib, atype from metadata
  } else {envi = 'Wat' ; pref <- 1}
  if (calibid == 1){calib = 1
  } else {calib = 0}
  if (atypeid == 1){atype = 'PSD'
  } else if (atypeid == 2) {atype = 'PowerSpec'
  } else if (atypeid == 3) {atype = 'TOLf'
  } else if (atypeid == 4) {atype = 'Broadband'
  } else if (atypeid == 5) {atype = 'Waveform'}


  ## Extract data

  dimc <- dim(conk)							#dimensions of input array
  t <- conk[2:dimc[1],1]						#time vector
  if (atype != 'Waveform'){					#if Waveform, format is different
    f <- conk[1,2:dimc[2]]						#frequency vector
    a <- conk[2:dimc[1],2:dimc[2]]				#data array
  } else { a <- conk[2:dimc[1],2]}

  if (tstamp == 1 & t[1]<1e7){t <- (t-719529)*86400	#convert from MATLAB time if so formatted
  t <- as.POSIXct(t,origin="1970-01-01", tz="UTC")
  }


  ## Format time vector if data is time-stamped

  if (tstamp == 1) {							#if data is time stamped
    tdiff <- max(t)-min(t)				#define time format for x-axis of time plot depending on scale
    if (tdiff < 10){
      tform <- "%H:%M:%S:%OS3"}
    else if (tdiff > 10 & tdiff < 86400){
      tform <- "%H:%M:%S"}
    else if (tdiff > 86400 & tdiff < 86400*7){
      tform <- "%H:%M \n %d %b"}
    else if (tdiff > 86400*7){tform <- "%d %b %y"}
  }


  ## Plot time-domain data
  #define color map for spectrograms/SPD
  jet.colors <- colorRampPalette(c("#00007F","blue","#007FFF","cyan","#7FFF7F","yellow","#FF7F00","red","#7F0000"))

  if (plottype == 'Time' | plottype == 'Both'){
    cat('Plotting...')							#if time plot selected
    tplot=proc.time()							#start plot timer
    options(scipen = 7)

    if (atype == 'PSD') {	 					#PSD spectrogram
      if (linlog == "Log"){
        if (tstamp != ""){
          #jet.colors <- colorRampPalette(c("#00007F","blue","#007FFF","cyan","#7FFF7F","yellow","#FF7F00","red","#7F0000"))
          ##define color map for spectrograms/SPD
          image(t,f,a,ylim = c(min(f),max(f)),yaxt="n",log = "y",col = jet.colors(512),main=paste('PSD spectrogram of ',ifile),xlab="Time",ylab="Frequency [ Hz ]",xaxt="n")
        }
        else {image(t,f,a,ylim = c(min(f),max(f)),yaxt="n",log = "y",col = jet.colors(512),main=paste('PSD spectrogram of ',ifile),xlab="Time [ s ]",ylab="Frequency [ Hz ]")}
        y1 <- floor(log10(range(f)))
        pow <- seq(y1[1], y1[2]+1)
        ticksat <- as.vector(sapply(pow, function(p) (1:10)*10^p))
        labsat <- as.vector(sapply(pow, function(p) 10^p))
        axis(2, 10^pow,labels = NA)
        axis(2, ticksat,labels = NA, tcl=-0.25, lwd=0, lwd.ticks=1)
        axis(2, labsat, tcl=-0.25, lwd=0, lwd.ticks=1)
      } else if (linlog == "Lin"){
        if (tstamp != ""){
          image(t,f,a,ylim = c(min(f),max(f)),col = jet.colors(512),main=paste('PSD spectrogram of ',ifile),xlab="Time",ylab="Frequency [ Hz ]",xaxt="n")
        }	else {image(t,f,a,ylim = c(min(f),max(f)),col = jet.colors(512),main=paste('PSD spectrogram of ',ifile),xlab="Time [ s ]",ylab="Frequency [ Hz ]")}}
    }

    if (atype == 'PowerSpec') {					#Power spectrum spectrogram
      if (linlog == "Log"){
        if (tstamp != ""){image(t,f,a,ylim = c(min(f),max(f)),yaxt="n",log = "y",col = jet.colors(512),main=paste('Power spectrum spectrogram of ',ifile),xlab="Time",ylab="Frequency [ Hz ]",xaxt="n")}
        else {image(t,f,a,ylim = c(min(f),max(f)),yaxt="n",log = "y",col = jet.colors(512),main=paste('Power spectrum spectrogram of ',ifile),xlab="Time [ s ]",ylab="Frequency [ Hz ]")}
        y1 <- floor(log10(range(f)))
        pow <- seq(y1[1], y1[2]+1)
        ticksat <- as.vector(sapply(pow, function(p) (1:10)*10^p))
        labsat <- as.vector(sapply(pow, function(p) 10^p))
        axis(2, 10^pow,labels = NA)
        axis(2, ticksat,labels = NA, tcl=-0.25, lwd=0, lwd.ticks=1)
        axis(2, labsat, tcl=-0.25, lwd=0, lwd.ticks=1)
      } else if (linlog == "Lin"){
        if (tstamp != ""){image(t,f,a,ylim = c(min(f),max(f)),col = jet.colors(512),main=paste('Power spectrum spectrogram of ',ifile),xlab="Time",ylab="Frequency [ Hz ]",xaxt="n")}
        else {image(t,f,a,ylim = c(min(f),max(f)),col = jet.colors(512),main=paste('Power spectrum spectrogram of ',ifile),xlab="Time [ s ]",ylab="Frequency [ Hz ]")}}
    }


    if (atype == 'TOLf') {						#Third octave level spectrogram
      if (tstamp != ""){image(t,f,a,ylim = c(min(f)*10^-0.05,max(f)*10^0.05),yaxt="n",log = "y",col = jet.colors(512),main=paste('TOL spectrogram of ',ifile),xlab="Time",ylab="Frequency [ Hz ]",xaxt="n")}
      else {image(t,f,a,ylim = c(min(f)*10^-0.05,max(f)*10^0.05),yaxt="n",log = "y",col = jet.colors(512),main=paste('TOL spectrogram of ',ifile),xlab="Time [ s ]",ylab="Frequency [ Hz ]")}
      y1 <- floor(log10(range(f)))
      pow <- seq(y1[1], y1[2]+1)
      ticksat <- as.vector(sapply(pow, function(p) (1:10)*10^p))
      labsat <- as.vector(sapply(pow, function(p) 10^p))
      axis(2, 10^pow,labels = NA)
      axis(2, ticksat,labels = NA, tcl=-0.25, lwd=0, lwd.ticks=1)
      axis(2, labsat, tcl=-0.25, lwd=0, lwd.ticks=1)
    }

    if (atype == 'Broadband') {					#Broadband level
      if (tstamp != ""){plot(t,a,type = "l",xlab="Time",ylab="",xaxt="n")}
      else{plot(t,a,type = "l",xlab="Time [ s ]",ylab="",main=paste('Broadband SPL of ',ifile))}
      if (envi == 'Air'){mtext(text=expression(paste("Broadband SPL [ dB re 20 ",mu,"Pa ]")), side=2, line=2)}
      if (envi == 'Wat'){mtext(text=expression(paste("Broadband SPL [ dB re 1 ",mu,"Pa ]")), side=2, line=2)}
    }

    if (atype == 'Waveform') {					#Waveform
      x <- a
      if (tstamp != "") {t <- t+tstamp		#if time stamp provided
      tdiff <- max(t)-min(t)					#define time format for x-axis of time plot
      if (tdiff < 10){
        tform <- "%H:%M:%S:%OS3"}
      else if (tdiff > 10 & tdiff < 86400){
        tform <- "%H:%M:%S"}
      else if (tdiff > 86400 & tdiff < 86400*7){
        tform <- "%H:%M \n %d %b"}
      else if (tdiff > 86400*7){tform <- "%d %b %y"}
      plot(t,a/1e6,xlab="Time",ylab="Pressure [ Pa ]",type = "l",ylim = c(-max(x/1e6),max(x/1e6)),xaxt="n")
      tck <- axis(1, labels=FALSE)
      axis.POSIXct(1,as.POSIXct.numeric(tck,origin="1970-01-01",tz="UTC"),format=tform,label=TRUE)
      }
      else {plot(t,x/1e6,xlab="Time [ s ]",ylab="Pressure [ Pa ]",type = "l",ylim = c(-max(x/1e6),max(x/1e6)))}
    }

    if (tstamp != ""){tck <- axis(1, labels=FALSE)
    #format x axis if time stamp is provided
    axis.POSIXct(1,as.POSIXct.numeric(tck,origin="1970-01-01",tz="UTC"),format=tform,label=TRUE)}
    cat('done in',(proc.time()-tplot)[3],'s.\n')
  }

  ## STATISTICAL ANALYSIS

  if (plottype == 'Stats' | plottype == 'Both'){
    tstat <- proc.time()				#start timer
    a <- t(a)								#if Stats plot selected
    if (atype != 'Waveform'){
      if (plottype == 'Stats' | plottype == 'Both'){
        if (atype != 'Broadband'){
          M <- length(conk[,1])-1

          cat('Computing noise level statistics...')

          dev.new()							#open new plot window

          # Compute mean level and percentiles
          RMSlevel <- 10*log10(rowMeans(10^(a/10)))
          #calculate RMS mean (EQUATION 18)
          p <- apply(a, 1, quantile, probs = c(0.01,0.05,0.5,0.9,0.95),  na.rm = TRUE)
          #percentile levels
          mindB <- floor(min(a[is.finite(a)])/10)*10		#minimum dB level rounded down to nearest 10
          maxdB <- ceiling(max(a[is.finite(a)])/10)*10		#maximum dB level rounded up to nearest 10
          # Compute SPD if more than 1000 data points in time domain
          if (M >= 1000) {
            hind <- 0.1							#histogram bin width for probability densities (PD)
            dbvec = seq(mindB,maxdB,hind)		#dB values at which to calculate empirical PD

            #Compute SPD array (corresponds to EQUATION 19)
            d <- t(apply(a,1,function(x) x<- hist(x,breaks = dbvec,plot = FALSE)$counts))
            d <- d/(hind*M)

            #Plot data
            d[which(d>0.05)] <- 0.05			#saturate colour scale at PD = 0.05
            if (linlog == "Log"){
              image(f,dbvec[1:length(dbvec)-1]-hind/2,d,zlim <- c(min(d[which(d>0)]),0.05),log = "x",xaxt="n",col = jet.colors(2^12),xlim<-c(min(f),max(f)),main=paste('Noise level statistics for ',ifile),xlab="Frequency [ Hz ]",ylab="")
            } else if (linlog == "Lin") {
              image(f,dbvec[1:length(dbvec)-1]-hind/2,d,zlim <- c(min(d[which(d>0)]),0.05),col = jet.colors(2^12),xlim<-c(min(f),max(f)),main=paste('Noise level statistics for ',ifile),xlab="Frequency [ Hz ]",ylab="")
            }
          }
          if (M<1000){
            if (linlog == "Log"){
              plot(f,RMSlevel,type="n",log = "x",xaxt="n",main=paste('Noise level statistics for ',ifile),xlab="Frequency [ Hz ]",ylab="",ylim=c(mindB,maxdB))
            } else if (linlog == "Lin"){
              plot(f,RMSlevel,type="n",main=paste('Noise level statistics for ',ifile),xlab="Frequency [ Hz ]",ylab="",ylim=c(mindB,maxdB))
            }
          }
          lines(f,p[5,],lwd = 2)
          lines(f,p[4,],col="gray15",lwd = 2)
          lines(f,p[3,],col="gray30",lwd = 2)
          lines(f,p[2,],col="gray45",lwd = 2)
          lines(f,p[1,],col="gray60",lwd = 2)
          lines(f,RMSlevel,col = 'magenta',lwd = 2)
          if (M<1000){legend('topright',c("99%","95%","50%","5%","1%","RMS level"),lty=c(1),lwd=2,col=c("black","gray20","gray40","gray60","gray80","magenta"))}
          if (M>=1000){legend('topright',c("SPD","99%","95%","50%","5%","1%","RMS level"),lty=c(1),lwd=2,col=c("blue","black","gray20","gray40","gray60","gray80","magenta"))}
          if (linlog == "Log"){

            y1 <- floor(log10(range(f)))
            pow <- seq(y1[1], y1[2]+1)
            ticksat <- as.vector(sapply(pow, function(p) (1:10)*10^p))
            labsat <- as.vector(sapply(pow, function(p) 10^p))
            axis(1, 10^pow,label = NA)
            axis(1, ticksat,labels = NA, tcl=-0.25, lwd=0, lwd.ticks=1)
            axis(1, labsat, tcl=-0.25, lwd=0, lwd.ticks=1)}
          if (atype == 'PSD'){
            if (calib == 1) {if (envi == 'Air'){YLAB <- expression(paste("PSD [ dB re 20 ",mu,"Pa"^2,"Hz"^{-1},"]"))} else {YLAB <- expression(paste("PSD [ dB re 1 ",mu,"Pa"^2,"Hz"^{-1},"]"))}
            } else {YLAB <- expression(paste("Relative PSD [ dB ]"))}
            mtext(text=YLAB, side=2, line=2)
          }
          if (atype == 'PowerSpec'){
            if (calib == 1) {if (envi == 'Air'){YLAB <- expression(paste("Power spectrum [ dB re 20 ",mu,"Pa"^2,"Hz"^{-1},"]"))} else {YLAB <- expression(paste("Power spectrum [ dB re 1 ",mu,"Pa"^2,"Hz"^{-1},"]"))}
            } else {YLAB <- expression(paste("Relative power spectrum [ dB ]"))}
            mtext(text=YLAB, side=2, line=2)
          }
          if (atype == 'TOLf'){
            if (calib == 1) {if (envi == 'Air'){YLAB <- expression(paste("SPL [ dB re 20 ",mu,"Pa ]"))
            } else {YLAB <- expression(paste("SPL [ dB re 1 ",mu,"Pa ]"))}
            } else {YLAB <- expression(paste("Relative SPL [ dB ]"))}
            mtext(text=YLAB, side=2, line=2)
          }
          cat('done in',(proc.time()-tstat)[3],'s.\n')
          if (M < 1000 & atype != 'Broadband') {
            cat('Too few time segments (M = ',M,', i.e. <1000) for SPD analysis: for SPD, use a longer file or shorter time segment length (N).\n')}
        }



      }

    }
  }
  if (atype == 'Broadband'){
    if (plottype == 'Stats' | plottype == 'Both'){
      tstat <- proc.time()
      cat('Computing noise level statistics...')
      p <- apply(a, 1, quantile, probs = c(0:100)/100,  na.rm = TRUE)
      dev.new()  						#open new plot window
      plot(p,c(0:100)/100,type="n",xlab="",ylab="Cumulative Distribution Function",main=paste('CDF of broadband SPL for ',ifile))
      lines(p,c(0:100)/100)
      if (calib == 1) {if (envi == 'Air'){XLAB <- expression(paste("Broadband SPL [ dB re 20 ",mu,"Pa ]"))
      } else {XLAB <- expression(paste("Broadband SPL [ dB re 1 ",mu,"Pa ]"))}
      } else {XLAB <- expression(paste("Relative SPL [ dB ]"))}
      mtext(text=XLAB, side=1, line=2)
      cat('done in',(proc.time()-tstat)[3],'s.\n')
    }
    RMSlev <- 10*log10(mean(10^(a/10)))
    medlev <- 10*log10(median(10^(a/10)))
    Mode <- function(x) {
      ux <- unique(x)
      ux[which.max(tabulate(match(x, ux)))]
    }
    modelev <- Mode(round(a*10)/10)

    tind <- t(3)-t(2)
    SEL <- 10*log10(tind*sum(10^(a/10)))

    if (calib == 1){
      cat('\nRMS level (mean SPL) = ',sprintf('%.1f',RMSlev),'dB re',pref,'uPa\n')
      cat('Median SPL = ',sprintf('%.1f',medlev),'dB re',pref,'uPa\n')
      cat('Mode SPL = ',sprintf('%.1f',modelev),'dB re',pref,'uPa\n')
      cat('SEL = ',sprintf('%.1f',SEL),'dB re',pref,'uPa^2 s. Note: for SEL measurements, set r = 0 (window overlap) and use default N.\n\n')
    } else {
      cat('\nRelative normalised RMS level (mean SPL) = ',sprintf('%.1f',RMSlev),'dB\n')
      cat('Relative normalised median SPL = ',sprintf('%.1f',medlev),'dB\n')
      cat('Relative normalised mode SPL = ',sprintf('%.1f',modelev),'dB\n\n')
      cat('Relative normalised SEL = ',sprintf('%.1f',SEL),'dB. Note: for SEL measurements, set r = 0 (window overlap) and use default N.\n\n')
    }
    if (plottype == 'Stats' | plottype == 'Both'){
      lines(c(RMSlev,RMSlev),c(0,1),col='magenta')
      lines(c(medlev,medlev),c(0,1),col='green')
      lines(c(modelev,modelev),c(0,1),col='blue')
      legend('bottomright',c("CDF","RMS level","Median","Mode"),lty=c(1),lwd=2,col=c("black","magenta","green","blue"))}
  }
} # end Viewer.R


# FUNCTION PAMGuide.R ===========================================================

# Computes calibrated or relative acoustic metrics from WAV audio files.

# This code accompanies the manuscript:

#   Merchant et al. (2015). Measuring Acoustic Habitats. Methods
#   in Ecology and Evolution

# and follows the equations presented in Appendix S1. It is not necessarily
# optimised for efficiency or concision.

# See Appendix S1 of the above manuscript for detailed instructions

# Copyright (c) 2014 The Authors.

# Author: Nathan D. Merchant. Last modified 22 Sep 2014

PAMGuide <- function(...,atype='PSD',plottype='Both',envi='Air',calib=0,
                     ctype = 'TS',Si=-159,Mh=-36,G=0,vADC=1.414,r=50,N=Fs,
                     winname='Hann',lcut=Fs/N,hcut=Fs/2,timestring="",
                     outdir=dirname(fullfile),outwrite=0,disppar=1,welch="",
                     chunksize="",linlog = "Log", WAVFiles = WAVFiles){

  graphics.off()									#close plot windows
  aid <- 0										#reset metadata code
  if (calib == 0) {aid <- aid + 20				#add calibration element to metadata code
  } else {aid <- aid + 10}
  if (timestring != ""){aid <- aid + 1000} else {aid <- aid + 2000}	#add time stamp element to metadata code

  ## Select input file and get file info
  fullfile <- WAVFiles[1]
  #fullfile <- INfile #choose.files() #
  ifile <- basename(fullfile)	#file name
  fIN <- readWave(fullfile,header = TRUE) #read file header
  Fs <- fIN[[1]]									#sampling frequency
  Nbit <- fIN[[3]]								#bit depth
  xl <- fIN[[4]]									#length of file in samples
  xlglo <- xl										  #back-up file length


  ## Read time stamp data if provided
  #if (timestring != "") {tstamp <- as.POSIXct(ifile, tz="America/Los_Angeles", format=timestring)
  if (timestring != "")
  {
    tstamp <- as.POSIXct(strptime(ifile,timestring) ,origin="1970-01-01")
    if (disppar == 1)
    {
      cat('Time stamp start time: ',format(tstamp),'\n')
    }
  }
  if (timestring == "") tstamp <- NULL		#compute time stamp in R POSIXct format


  ## Display user-defined settings

  if (disppar == 1){
    cat('File name:',ifile,'\n')
    cat('File length:',xl,'samples =',xl/Fs,'s\n')
    cat('Analysis type:',atype,'\n')
    cat('Plot type:',plottype,'\n')
    if (calib == 1){
      if (ctype == 'EE'){
        cat('End-to-end system sensitivity =',sprintf('%.01f',Si),'dB\n')
        if (envi == 'Wat') {cat('In-air measurement\n')}
        if (envi == 'Wat') {cat('Underwater measurement\n')}}
      if (ctype == 'RC'){
        cat('System sensitivity of recorder (excluding transducer) =',sprintf('%.01f',Si),'dB\n')}
      if (ctype == 'TS' || ctype == 'RC'){
        if (envi == 'Air') {cat('In-air measurement\n')
          cat('Microphone sensitivity:',Mh,'dB re 1 V/Pa\n')
          Mh <- Mh - 120}		#convert to dB re 1 V/uPa
        if (envi == 'Wat') {cat('Underwater measurement\n')
          cat('Hydrophone sensitivity:',Mh,'dB re 1 V/uPa\n')}}
      if (ctype == 'TS'){
        cat('Preamplifier gain:',G,'dB\n')
        cat('ADC peak voltage:',vADC,'V\n')}
    } else {cat('Uncalibrated analysis. Output in relative units.\n')
    }
    cat('Time segment length:',N,'samples =',N/Fs,'s\n')
    cat('Window function:',winname,'\n')
    cat('Window overlap:',r,'%\n')
  }
  r<-r/100

  ## Read input file

  if (chunksize == ""){nchunks = 1				#if loading whole file, nchunks = 1
  } else if (chunksize != "") {				#if loading file in stages
    nchunks <- ceiling(xl/(Fs*as.numeric(chunksize)))	#number of chunks of length chunksize in file
  }

  for (q in 1:nchunks){							#loop through file chunks
    if (nchunks == 1){							#if loading whole file at once
      t1=proc.time()									#start timer
      cat('Loading input file... ')
      xbit <- readWave(fullfile)						#read file
      cat('done in',(proc.time()-t1)[3],'s.\n')
      xbit <- xbit@left/(2^(Nbit-1))					#convert to full scale (+/- 1) via bit depth
    } else if (nchunks > 1) {						#if loading file in stages
      if (q == nchunks){							#load qth chunk
        xbit <- readWave(fullfile,from=((q-1)*as.numeric(chunksize)*Fs+1),to=xlglo,units="samples")
        xbit <- xbit@left/(2^(Nbit-1))			#convert to full scale (+/- 1) via bit depth
        xl <- length(xbit)						#final chunk length
      } else {
        xbit <- readWave(fullfile,from=((q-1)*as.numeric(chunksize)*Fs+1),to=(q*as.numeric(chunksize)*Fs),units="samples")
        xbit <- xbit@left/(2^(Nbit-1))			#convert to full scale (+/- 1) via bit depth
        xl <- length(xbit)						#chunk length
      }
    }

    if (envi == 'Air'){pref<-20; aid <- aid+100}	#set reference pressure depending for in-air or underwater
    if (envi == 'Wat'){pref<-1; aid <- aid+200}


    ## Compute system sensitivity if provided

    if (calib == 1){
      if (ctype == 'EE') {		#'EE' = end-to-end calibration
        S <- Si}
      if (ctype == 'RC') {		#'RC' = recorder calibration with separate transducer sensitivity defined by Mh
        S <- Si + Mh}
      if (ctype == 'TS') {		#'TS' = manufacturer's specifications
        S <- Mh + G + 20*log10(1/vADC);		#EQUATION 4
        # insert option to calculate across a frequencies???
      }

      if (disppar == 1){cat('System sensitivity correction factor, S = ',sprintf('%.01f',S),' dB\n')}
    } else {S <- 0}				#if not calibrated (calib == 0), S is zero

    ## Compute waveform if selected

    if (atype == 'Waveform') {
      if (calib == 1){
        a <- xbit/(10^(S/20)) 				#EQUATION 21
      } else {a <- xbit/(max(xbit))}
      t <- seq(1/Fs,length(a)/Fs,1/Fs)	#time vector
      tana = proc.time()
    }

    ## Compute DFT-based metrics if selected

    if (atype != 'Waveform') {
      if (nchunks == 1){
        if (atype == 'PSD'){cat('Computing PSD...')}
        if (atype == 'PowerSpec'){cat('Computing power spectrum...')}
        if (atype == 'TOL'){cat('Computing TOLs by DFT method...')}
        if (atype == 'Broadband'){cat('Computing broadband level...')}
        tana = proc.time()}

      # Divide signal into data segments (corresponds to EQUATION 5)
      N = round(N)						#ensure N is an integer
      nsam = ceiling((xl)-r*N)/((1-r)*N)	#number of segments of length N with overlap r
      xgrid <- matrix(nrow = N,ncol = nsam)	#initialise grid of segmented data for analysis
      for (i in 1:nsam) {					#make grid of segmented data for analysis
        loind <- (i-1)*(1-r)*N+1
        hiind <- (i-1)*(1-r)*N+N
        xgrid[,i] = xbit[loind:hiind]
      }

      M <- length(xgrid[1,])

      # Apply window function (corresponds to EQUATION 6)
      if (winname == 'Rectangular') {			#rectangular (Dirichlet) window
        w <- matrix(1,1,N)
        alpha <- 1 }					#scaling factor
      if (winname == 'Hann') {			#Hann window
        w <- (0.5 - 0.5*cos(2*pi*(1:N)/N))
        alpha <- 0.5 }					#scaling factor
      if (winname == 'Hamming') {			#Hamming window
        w <- (0.54 - 0.46*cos(2*pi*(1:N)/N))
        alpha <- 0.54 }					#scaling factor
      if (winname == 'Blackman') {		#Blackman window
        w <- (0.42 - 0.5*cos(2*pi*(1:N)/N) + 0.08*cos(4*pi*(1:N)/N))
        alpha <- 0.42 }					#scaling factor

      xgrid <- xgrid*w/alpha 				#apply window function

      #Compute DFT (corresponds to EQUATION 7)
      X <- abs(mvfft(xgrid))

      #Compute power spectrum (EQUATION 8)
      P <- (X/N)^2

      #Compute single-side power spectrum (EQUATION 9)
      Pss <- 2*P[0:round(N/2)+1,]

      #Compute frequencies of DFT bins
      f <- floor(Fs/2)*seq(1/(N/2),1,len=N/2)
      flow <- which(f >= lcut)[1]			#find index of lcut frequency
      fhigh <- max(which(f <= hcut))		#find index of hcut frequency
      f <- f[flow:fhigh]					#limit frequencies to user-defined range
      nf <- length(f)						#number of frequency bins


      #Compute PSD in dB if selected
      if (atype == 'PSD') {
        B <- (1/N)*(sum((w/alpha)^2))		#noise power bandwidth (EQUATION 12)
        delf <- Fs/N;						#frequency bin width
        a <- 10*log10((1/(delf*B))*Pss[flow:fhigh,]/(pref^2))-S
      }									#PSD (EQUATION 11)

      #Compute power spectrum in dB if selected
      if (atype == 'PowerSpec') {
        a <- 10*log10(Pss[flow:fhigh,]/(pref^2))-S
      }									#EQUATION 10

      #Compute broadband level if selected
      if (atype == 'Broadband') {
        a <- 10*log10(colSums(Pss[flow:fhigh,])/(pref^2))-S
      }									#EQUATION 17

      #Compute 1/3-octave band levels if selected
      if (atype == 'TOL') {
        if (lcut <25){					#limit TOL analysis to > 25 Hz
          lcut <- 25}

        #Generate 1/3-octave freqeuncies
        lobandf <- floor(log10(lcut))	#lowest power of 10 for TOL computation
        hibandf <- ceiling(log10(hcut))	#highest power of 10 for TOL computation
        nband <- 10*(hibandf-lobandf)+1	#number of 1/3-octave bands
        fc <- matrix(0,nband)			#initialise 1/3-octave frequency vector
        fc[1] <- 10^lobandf;

        #Calculate centre frequencies (corresponds to EQUATION 13)
        for (i in 2:nband) {
          fc[i] <- fc[i-1]*10^0.1}
        fc <- fc[which(fc >= lcut)[1]:max(which(fc <= hcut))]

        nfc <- length(fc)				#number of 1/3-octave bands

        #Calculate boundary frequencies of each band (EQUATIONS 14-15)
        fb <- fc*10^-0.05				#lower bounds of 1/3-octave bands
        fb[nfc+1] <- fc[nfc]*10^0.05	#upper bound of highest band
        if (max(fb) > hcut) {			#if upper bound exceeds highest
          nfc <- nfc-1				# frequency in DFT, remove
          fc <- fc[1:nfc]}

        #Calculate TOLs (corresponds to EQUATION 16)
        P13 <- matrix(nrow = M,ncol = nfc)
        for (i in 1:nfc) {
          fli <- which(f >= fb[i])[1]
          fui <- max(which(f < fb[i+1]))
          for (k in 1:M) {
            fcl <- sum(Pss[fli:fui,k])
            P13[k,i] <- fcl
          }
        }
        a <- t(10*log10(P13/(pref^2)))-S
      }

      # Compute time vector
      tint <- (1-r)*N/Fs
      ttot <- M*tint-tint
      t <- seq(0,ttot,tint)
    }

    if (nchunks>1){								#if loading in stages, concatenate analyses in each loop iteration
      if (q == 1){
        newa <- a
        newt <- t
        cat('Chunk size:',chunksize,'s\nAnalysing in',nchunks,'chunks.\n  1: ')

      } else if (q > 1){

        dimA <- dim(a)
        newa <- cbind(newa,a)

        if (timestring != ""){
          #newt <- c(newt,t)
          #} else {
          newt <- c(newt,t+(q-1)*as.numeric(chunksize))
        }
        cat(' ',q,': ')
      }
    } else {cat('done in',(proc.time()-tana)[3],'s.\n')}


    if (nchunks>1){a <- newa					#reassign output array
    t <- newt
    cat('\n')}

    # If not calibrated, scale relative dB to zero
    if (calib == 0 & atype != 'Waveform') {a <- a-max(a)}

    if (!is.null(tstamp)){t <- t + tstamp}

    #  tdiff <- difftime(max(t),min(t),units="secs")					#define time format for x-axis of time plot
    #  if (tdiff < 10){ 		                        tform <- "%H:%M:%S"}
    #  else if (tdiff > 10 & tdiff < 86400){ 		  tform <- "%H:%M:%S"}
    #  else if (tdiff > 86400 & tdiff < 86400*7){ 	tform <- "%H:%M \n %d %b"}
    #  else if (tdiff > 86400*7){                  tform <- "%d %b %y"}
    #  }

    # if (tstamp != ""){t <- t + tstamp
    # tdiff <- max(t)-min(t)					#define time format for x-axis of time plot
    # if (tdiff < 10){ 		                        tform <- "%H:%M:%S:%OS3"}
    # else if (tdiff > 10 & tdiff < 86400){ 		  tform <- "%H:%M:%S"}
    # else if (tdiff > 86400 & tdiff < 86400*7){ 	tform <- "%H:%M \n %d %b"}
    # else if (tdiff > 86400*7){                  tform <- "%d %b %y"}
    # }


    ## Construct output array

    if (atype == 'PSD' | atype == 'PowerSpec') {
      A <- cbind(t,t(a))
      A <- rbind(c(0,f),A)
      if (atype == 'PSD'){aid <- aid + 1}
      if (atype == 'PowerSpec'){aid <- aid + 2}
      A[1,1] <- aid
    }

    if (atype == 'TOL') {
      A <- cbind(t,t(a))
      A <- rbind(c(0,fc),A)
      aid <- aid + 3
      A[1,1] <- aid
      f <- fc
    }

    if (atype == 'Broadband') {
      A <- t(rbind(t,a))
      aid <- aid + 4
      A[1,1] <- aid
    }

    if (atype == 'Waveform') {
      A <- t(rbind(t,a))		#define output array
      A <- rbind(c(0,0),A)	#add zero top row for metadata
      aid <- aid + 5			#add index to metadata for Waveform
      A[1,1] <- aid			#encode output array with metadata
    }

    ## Reduce time resolution if selected

    dimA <- dim(A)				#dimensions of output array

    if (welch != "" && atype != "Waveform"){
      lout <- ceiling(dimA[1]/welch)+1	#length of new, Welch-averaged, output array
      AWelch <- matrix(, nrow = lout, ncol = dimA[2])	#initialise Welch array
      AWelch[1,] <- A[1,]					#assign frequency vector
      tint <- A[3,1] - A[2,1]				#time interval between segments
      cat('Welch factor =',welch,'x\nNew time resolution =',welch,'(Welch factor) x',N/Fs,'s (time segment length) x',r*100,'% (overlap) =',welch*tint,'s\n')
      if (lout == 2){						#if Welch factor too large for number of time data points, abort averaging
        stop(paste('Welch factor is greater than half the number of samples. Set welch <=',dimA[1]/2,', or reduce N.',sep=""),call.=FALSE)
      }	else {
        for (i in 2:lout) {						#loop through Welch segments for averaging
          stt <- A[2,1] + (i-2)*tint*welch	#start time
          ett <- stt + welch*tint				#end time
          stiv <- which(A[2:dimA[1]]>=stt)
          sti <- min(stiv)+1					#start index
          etiv <- which(A[2:dimA[1]]<ett)
          eti <- max(etiv)+1					#end index
          nowA <- 10^(A[sti:eti,]/10)			#take RMS level of segments as per Welch method
          AWelch[i,] <- 10*log10(rowMeans(t(nowA)))	#convert to dB
          AWelch[i,1] <- stt+tint*welch/2		#assign time index
        }
      }
      A <- AWelch										#reassign output array
      dimA <- dim(A)
      t <- A[2:dimA[1],1]
      f <- A[1,2:dimA[2]]
      a <- t(A[2:dimA[1],2:dimA[2]])
    }
    dimA <- dim(A)


    ## Plot data

    #source('Viewer.R')								#initialise Viewer
    #Viewer(fullfile=A,plottype=plottype,ifile=ifile,linlog=linlog)


    ## Write output array to CSV file if selected


    A <- data.matrix(A, rownames.force = NA)
    if (outwrite == 1){
      #if (disppar == 1){cat('Writing output file...')
      #twrite <- proc.time()}
      if (atype == 'Waveform') {
        ofile <- paste(gsub(".wav","",file.path(outdir,basename(fullfile))),'_',atype,'.csv',sep = "")
        write.table(A,file = ofile,row.names=FALSE,quote=FALSE,col.names=FALSE,sep=",")
      }
      if (atype != 'Waveform') {
        ofile <- paste(gsub(".wav","",file.path(outdir,basename(fullfile))),'_',atype,'_',N,'samples',winname,'Window_',round(r*100),'PercentOverlap.csv',sep = "")
        write.table(A,file = ofile,row.names=FALSE,quote=FALSE,col.names=FALSE,sep=",")
      }
      #if (disppar == 1){cat('done in',(proc.time()-twrite)[3],'s.\n')}
    }
  }
}




# FUNCTION Wave_To_NVSPL ====================================================

#' @name Wave_To_NVSPL
#' @title Calibrate and convert wave files into NVSPL format
#' @description This function uses PAMGuide code to convert wave files into NVSPL format. PAMGuide was developed by Nathan D. Merchant et al. 2015 (see \strong{Details}). The suggested workflow for this function is to first set test.file = TRUE to test that your workflow has been accurately parameterized. Next, to batch process NVSPLs, run with test.file = FALSE.
#' @param input.directory Top-level input directory path to audio files to be processed. e.g. E:/AUDIO. \strong{Audio files are expected to have the naming convention SITEID_YYYYMMDD_HHMMSS.wav. Use the filext argument for other file extension patterns (NOT TESTED BY CB).}
#' @param data.directory Logical flag to specify whether audio files are housed in 'Data' subdirectories
#' @param test.file Logical flag for whether to test a file. If TRUE, tests a single file and produces plots and diagnostic outputs. If FALSE, processes entire audio dataset indicated by input.directory.
#' @param project File name for your project (e.g., 'GLBAPhenology2019')
#' @param instrum Audio recorder used (e.g., 'SM4')
#' @param filext File extension pattern (e.g, '_%Y%m%d_%H%M%S.wav'). If using split files, '_0_%Y%m%d_%H%M%S_000.wav'.
#' @param filpat File pattern (can more info be given here? Or could we just constrain the expected file pattern so minimize unexpected user behavior?)
#' @param mhset Microphone sensitivity dBV/Pa (see for more info https://doimspp.sharepoint.com/sites/nsnsdallstaff/Shared%20Documents/Science%20and%20Tech/Software/SongMeterToNVSPL/SongMeter4toNVSPL.mp4)
#' @param Gset Gain settings
#' @param vADCset Zero-peak   (more info? what does this mean)
#' @param enviset Use 'Air' or 'Wat' to indicate whether audio recordings occurred in a terrestrial or underwater environment, respectively.
#' @param rescWat Use 1 if you want to re-scale underwater values to be able to plot using AMT, 0 if not
#' @param timezone Specify timezone setting used in the audio recorder (e.g, 'GMT'). If recordings were taken in local time at your study site, specify an \href{https://en.wikipedia.org/wiki/List_of_tz_database_time_zones#List}{Olson-names-formatted character timezone} for the location (e.g., 'America/Los_Angeles'). This is extremely important to foster clarity in data analysis through the years, as some projects have varied year to year in whether recordings were taken in GMT vs. local time.
#' @return If test.file = TRUE, returns diagnostics. If test.file = FALSE, returns NVSPL txt files in NVSPL folder generated by the function. \strong{CB: we need to go through and document every column in the output of this --i.e. what does INVID and GChar1 etc etc mean -- where is metadata located for this?}
#'
#'
#'
#' Output NVSPL txt file contains the following N columns (need to go through and doc these):
#'
#' \itemize{
#' \item{\strong{SiteID}: Site name.}
#' \item{\strong{STime}: tbd.}
#' \item{\strong{H12p5}: tbd}
#' \item{\strong{all H columns}: tbd.}
#' \item{\strong{H20000}: tbd.}
#' \item{\strong{dbA}: ....}
#' \item{\strong{dbC}: ....}
#' \item{\strong{dbZ}: ...}
#' \item{\strong{etc}: ...}
#' \item{\strong{GPSTimeAdjustment}: Indicates the timezone adjustment made. 'GMT' indicates that audio recordings were taking with no timezone adjustment. If recordings were taken in local time at a study site, an \href{https://en.wikipedia.org/wiki/List_of_tz_database_time_zones#List}{Olson-names-formatted character timezone} for the location (e.g., 'America/Los_Angeles') should have been specified using the 'timezone' input argument to the function. This is extremely important to foster clarity in data analysis through the years, as some projects have varied year to year in whether recordings were taken in GMT vs. local time. setting used in the audio recorder}
#' }
#'
#'
#' @details
#'
#' This function was developed by the National Park Service Natural Sounds and Night Skies Division to act as a wrapper to PAMGuide that would support NSNSD data processing workflows. PAMGuide was published as supplementary material to the following Open Access journal article:
#'
#' \href{https://besjournals.onlinelibrary.wiley.com/doi/full/10.1111/2041-210X.12330}{Merchant, N.D., Fristrup, K.M., Johnson, M.P., Tyack, P.L., Witt, M.J., Blondel, P., Parks, S.E. (2015). Measuring acoustic habitats. Methods in Ecology and Evolution.}
#'
#' \itemize{
#' \item{\href{https://besjournals.onlinelibrary.wiley.com/action/downloadSupplement?doi=10.1111%2F2041-210X.12330&file=mee312330-sup-0001-AppendixS1.pdf}{View accompanying PAMGuide tutorial}}
#' \item{\href{https://sourceforge.net/projects/pamguide/}{Download original PAMGuide code}}
#' }
#'
#' @seealso  \code{\link{NVSPL_To_AI}}
#' @import svDialogs tuneR
#' @export
#' @examples
#' \dontrun{
#'
#' # Create an input directory for this example
#' dir.create('example-input-directory')
#'
#' # Read in example wave files
#' data(exampleAudio1)
#' data(exampleAudio2)
#'
#' # Write example waves to example input directory
#' tuneR::writeWave(object = exampleAudio,
#'                  filename = 'example-input-directory/Rivendell_20210715_114502.wav')
#' tuneR::writeWave(object = exampleAudio,
#'                  filename = 'example-input-directory/Rivendell_20210715_115502.wav')
#'
#' # Perform Wave_To_NVSPL in test mode (test.file = TRUE)
#' Wave_To_NVSPL(
#'  input.directory = 'example-input-directory',
#'  data.directory = FALSE,
#'  test.file = TRUE,
#'  project = 'testproject',
#'  timezone = 'GMT')
#'
#' # Perform Wave_To_NVSPL in batch mode (test.file = FALSE)
#' Wave_To_NVSPL(
#'  input.directory = 'example-input-directory',
#'  data.directory = FALSE,
#'  test.file = FALSE,
#'  project = 'testproject',
#'  timezone = 'GMT')
#'
#' # Verify that NVSPL outputs have been created
#' nvspls <- list.files('example-input-directory/NVSPL', full.names = TRUE)
#'
#' # View one of the NVSPL outputs
#' one.nvspl <- read.delim(file = nvspls[1], sep = ',')
#'
#' # Delete all temporary example files when finished
#' unlink(x = 'example-input-directory', recursive = TRUE)
#'
#' }
#'


# More ideas:
#  Instead of test.file, could do a "diagnostics" argument instead, to plot a time continuous, whether time stamp is right etc. (Good idea but a lot more effort
# since it's essentially eliminating the pamguide function and then would have to
# knit those two functions together to get the same effect. Not a priority for
# CB yet)
# A way to get rid of the pamguide function?
#  Better variable names and/or commenting. Highly desirable but CB not knowledgeable
#  enough to do this expediently and know what variable names / math should be, etc. Not a top priority.
# Params file (like a run log)- use future params file as an input? Good idea
# need to overhaul NVSPL file in general, but that's a task more for Damon


Wave_To_NVSPL <- function(input.directory,
                          data.directory = TRUE,
                          test.file = FALSE,
                          project,
                          instrum = "SM4",
                          filext = "_%Y%m%d_%H%M%S.wav",
                          filpat = ".+\\d{8}_\\d{6}.wav",
                          mhset = -35,
                          Gset = 16,
                          vADCset = 1,
                          enviset = "Air",
                          rescWat = 0,
                          timezone = "GMT"
)
{

  # Ensure a forward slash at end ($) of input.directory
  if (grepl("\\/$", input.directory) == FALSE) {
    input.directory <- paste0(input.directory, '/')
  }

  ## INITIALIZE PARAMETERS

  ## (1) SET DIRECTORY WHERE ALL AUDIO FILES TO PROCESS ARE
  #if you want to process multiple directories (sites) with same recording parameters, choose the highest directory
  WAVDirsDIR <- input.directory

  ## (2) LIST OF DIRECTORIES TO PROCESS
  WAVDirs <- list.dirs(WAVDirsDIR)

  # If using highest directory, that one may not have wavfiles in it, so only find the ones with wav files
  check.folders <- sapply(WAVDirs, function(x) length(list.files(path = x, pattern = '.wav')))
  WAVDirs <- WAVDirs[check.folders > 0]

  # ARE YOUR FILES IN "Data" subdirectories? (standard format from song meters)
  # If data in subdirs:
  if (data.directory == TRUE) {WAVDirs = WAVDirs[grep("Data", WAVDirs)]}

  ## (3) VERSION OF PAMGUIDE, see the most recent code
  vers <- 'V12noChunk' # CB: to be updated with any future tweaks/releases of this Github code
  # this way it updates automatically in the function instead of user having to input


  if(enviset == 'Air') envir <- 2
  if(enviset == 'Wat') envir <- 1

  # Save params for posterity once all necessary objects have been initialized
  params.name <- paste0("paramsFileNVSPL_", project, "_", instrum)
  save(file = paste0(input.directory, params.name), list = ls(all.name = TRUE))
  message('\nOutput "params" file memorializing your inputs to this function has been saved in: \n ', input.directory, ' as ', params.name, '\n')

  # IF TESTING A SINGLE FILE - PROCESS ONE FILE AND CHECK OUTPUT
  if (test.file == TRUE) {
    # get a test file
    WAVFiles = list.files(WAVDirs[1], pattern = filpat, full.names = TRUE)

    # find unique days
    dys = unique(gsub(".+_(\\d{8})_(.+).wav","\\1",WAVFiles) )

    # sets the filenames
    s1 = unlist (strsplit( basename(WAVFiles[1]), '_') ) [1]
    site = unlist (strsplit( s1, '_') )[1]
    filename2 = paste(site, filext, sep="")

    # run the calibration
    message('\n Check all outputs. Make sure there isn\'t an NA in "Time stamp start time".\n')
    PAMGuide(chunksize = 500, atype = 'TOL', timestring = filename2,
             r=0, outwrite=1, plottype = "None",
             calib=1, envi=enviset, ctype="TS", Mh=mhset, G=Gset, vADC=vADCset,
             WAVFiles = WAVFiles)
    message('Reminder: Check all outputs. Make sure there isn\'t an NA in "Time stamp start time".\n')
    # In the outputs here, make sure there isn't an NA in your start time

    # Evaluate the file created
    testFile <- list.files(WAVDirs[1], pattern = 'TOL', recursive=T, full.names=T)
    basename(testFile)

    # combine data
    conk <- as.matrix( read.csv(testFile[1], colClasses="numeric", header=FALSE) )
    dimc <- dim(conk)

    # what is the dB range?
    a <- conk[2:dimc[1],2:dimc[2]]
    hist(a,main = "Check to see if calibration is accurate", xlab="SPL dB")
    message('dB range: ', round(min(a),2), ' to ', round(max(a), 2), '. If these numbers are negative, go back and check your gain settings, environment, and other inputs.' )
    # [ want to see numbers like 0 to 80, if you're getting negative numbers go
    # back and check your gain settings, environment, inputs etc.]
    # 10 to 89 is totally reasonable for this envt

    # time stamp- start and continuous
    t <- conk[2:dimc[1],1]
    t <- as.POSIXct(t,origin="1970-01-01")
    if ( is.na(as.character(t[1])) ) {
      stop('Time is not correct format. Check your inputs to this function.')
    }else {
      plot(conk[2:dimc[1],1],main = 'Check this plot. Is time continuous?') }
    message('Check the plot to ensure time is continuous (no breaks).')
    # delete files, if okay, re-run if not
    file.remove(testFile)
    rm(conk,a,dimc,t, testFile, site, filename2)

  } # end test.file

  # IF NOT TESTING A FILE, BATCH PROCESS ALL AUDIO FILES IN DIRECTORIES SELECTION
  if (test.file == FALSE) {
    # can process multiple sites as long as they have the same calibration parameters

    for (ff in 1:length(WAVDirs))  # ff = 1
    {
      ## NEED to LOOP through wav files of the same day to make NVSPLs

      ## find unique days
      WAVfiles = list.files(WAVDirs[ff], pattern = filpat )
      dys = unique(gsub(".+_(\\d{8})_(.+).wav","\\1",WAVfiles) )

      ## NOTE: uncomment and edit next line if code breaks partway through, use this to start loop on next file
      #dys = dys[74:90]  # CB: THIS IS NOT ROBUST. Add a trycatch & warning messaging instead.

      ## sets the file names for the directory- new one for each direcory
      s1 = unlist(strsplit( WAVfiles[1], '_') ) [1]
      site = unlist(strsplit( s1, '_') )[1]
      filename = paste(site, filext, sep="")

      ## create NVSPL OUTPUT directory
      NVSPLdir = paste(WAVDirs[ff], "NVSPL",sep="\\")
      dir.create(NVSPLdir,showWarnings=F,recursive=T)
      message('\nNVSPL directory created in: ', NVSPLdir, '\n')

      ## LOOP through the unique days- calibrate then convert to NVSPL format
      cnt = 0
      for(d in (dys) )  # d = dys[1] # for testing
      {

        cnt = cnt + 1
        cat('##################################################')
        cat('Processing DIRECTORY ', ff, " of ", length(WAVDirs), " for DAY", cnt, ' of ', length(dys), '\n' )
        cat('##################################################')

        ## (1) GET a list files for each day-------------------------------------------
        udaylist = grep(d, WAVfiles, value=T)
        filenms = paste(WAVDirs[ff], "\\", udaylist, sep="")

        ## (2) RUN PAMGUIDE-------------------------------------------------------------
        Meta(atype = 'TOL', timestring = filename,
             r=0, outwrite=1, plottype = "None", calib=1,
             envi=enviset, ctype="TS", Mh=mhset, G=Gset, vADC=vADCset,
             filenms = filenms)

        ## (3) READ IN file created by PAMGUIDE------------------------------------------
        PAMfiles = list.files(WAVDirs[ff], pattern = "Conk.*.csv",
                              recursive=T, full.names=T)
        PAMfiles2 = list.files(WAVDirs[ff], pattern = ".csv", # CB: originally this said "*.csv". Had to change to '.csv' to work
                               recursive=T, full.names=T)
        PAMdirFiles = dirname(PAMfiles2[2])

        # read in 1st PAM file
        conk <- as.matrix( read.csv(PAMfiles[1], colClasses="numeric",header=FALSE) )
        # conk <- read.csv(PAMfiles[1], header = FALSE)

        # remove the folder with files for each WAV file
        unlink(PAMfiles2)
        unlink(PAMdirFiles, recursive = TRUE)

        ## (4) EXTRACT PARAMS--------------------------------------------------------------
        aid <- conk[1,1]
        tstampid <- substr(aid,1,1)		#extract time stamp identifier
        enviid <- substr(aid,2,2)			#extract in-air/underwater identifier
        calibid <- substr(aid,3,3)		#extract calibrated/uncalibrated identifier
        atypeid <- substr(aid,4,4)

        # assign PAMGuide variables envi, calib, atype from metadata
        #CB: IN THEORY THE ATYPE is never going to change since atype = 'TOL'
        # is a default arg in the meta and pamguide_meta fxn
        if (tstampid == 1){tstamp = 1} else {tstamp = ""}
        if (enviid == 1){
          envi = 'Air'  ; pref <- 20
        } else {envi = 'Wat' ; pref <- 1}
        if (calibid == 1){calib = 1
        } else {calib = 0}
        if (atypeid == 1){atype = 'PSD'
        } else if (atypeid == 2) {atype = 'PowerSpec'
        } else if (atypeid == 3) {atype = 'TOLf'
        } else if (atypeid == 4) {atype = 'Broadband'
        } else if (atypeid == 5) {atype = 'Waveform'}

        # extract DATA SPL DATA and TIMESTAMP.....
        dimc <- dim(conk)
        t <- conk[2:dimc[1],1]
        t <- as.POSIXct(t,origin="1970-01-01")
        tString <- as.character(t) # tString[length(tString)]
        a <- conk[2:dimc[1],2:dimc[2]]
        f <- conk[1,2:dimc[2]]
        # hist(a)   max(a)   min(a)
        rm(conk)

        ## (5) FORMAT myOutput as NVSPL-----------------------------------------------------
        # (note: PAMguide starts at 25 Hz, so lower bands (12.5, 15.8, and 20 are always NaNs)
        NVSPLhead = c("SiteID","STime", "H12p5", "H15p8", "H20", "H25", "H31p5","H40","H50","H63","H80","H100","H125","H160","H200","H250","H315","H400","H500",
                      "H630","H800","H1000","H1250","H1600","H2000","H2500","H3150","H4000","H5000","H6300","H8000","H10000","H12500","H16000","H20000",
                      "dbA","dbC","dbZ","Voltage","WindSpeed","WindDir","TempIns","TempOut","Humidity",
                      "INVID","INSID","GChar1","PAMGuideVersion","GChar3", "AdjustmentsApplied","CalibrationAdjustment","GPSTimeAdjustment","GainAdjustment","Status")

        # check to see of more 1/3 OCB than 33, if so truncate data
        if(dim(a)[2] > 30) a <- a[,1:30]

        # check to see if less than 33 octave
        endA = ((33-4)-dim(a)[2])+1  # cb: what is the actual check?
        # CB: add status message that if less than 33 octaves, we do something

        # calculate a dBA
        # CB: ideally data.frame this with named cols
        aweight <- c(-63.4,-56.7,-50.5,-44.7, -39.4, -34.6, -30.2, -26.2, -22.5, - 19.1, -16.1,
                     -13.4, -10.9, -8.6, -6.6, -4.8, -3.2, -1.9, -0.8, 0, 0.6, 1, 1.2,
                     1.3, 1.2, 1.0, 0.5, -0.1, -1.1, -2.5, -4.3, -6.6, -9.3)
        # only use a-weights for the available data
        #aA <- a + aweight[4:(33-endA)]
        #a[1,] + aweight[4:(33-endA)]
        aA = t( t(a) + aweight[4:(33-endA)] )

        # convert to pressure
        press <- rowMeans(10^(aA/10))   # cb: ie pressure mean
        dBA = 10*log10(press) #hist(dBA)  # cb: mean of the pressures
        pressS <- rowSums(10^(aA/10))   # cb: ie pressure sum
        zweight <- rowSums(10^(a/10))   # cb: if want to do a zweighting fxn
        dBAsum = 10*log10(pressS) #hist(dBA) #cb: sum of the pressures
        # if underwater, rescales the values to the AMT scale, using a normalization formula
        if (rescWat == 1) {
          if (envir == 1)
          {
            cat("rescaling db levels to work with AMT")
            a2 = ((a -  (-8)) / (87 - (-8))) * a
            # hist(a2) a2 = a - 62 # accounts for offset of water/ air
            a=a2
            rm(a2)
          }
        }

        ## determine how many blank columns in NVSPL, assumes you add the first 5 columns
        nBlankCols <- length(NVSPLhead) - (dim(a)[2] + 5)

        ## find unique day hours
        unqHrs <- substr(tString,1,13)

        ## create matrix with all the data combined and add headers
        # CB - I don't love this, seems too easy to make mistakes.
        #      More robust is to set this up as a data.frame or data.table and directly name the columns
        tempOutput <- cbind(site, tString, 0, 0, 0, round(a, 1),
                            matrix(rep(0,dim(a)[1] * nBlankCols),
                                   nrow = dim(a)[1], ncol = nBlankCols))
        colnames(tempOutput) <- NVSPLhead
        tempOutput[,'dbA'] <- dBAsum
        tempOutput[,'dbZ'] <- zweight
        tempOutput[,'PAMGuideVersion'] <- vers

        # CB: I'll keep the GPSTimeAdjustment as the timezone since I'm not
        # sure the purpose/assumptions of that column
        tempOutput[,'GPSTimeAdjustment'] <- timezone

        # CATHLEEN note to self- -
        # want to redo this so that column contents are assigned by name rather
        # than by number. Bc I don't know that these are going in the right place
        # just do colnames(tempout) <- nvspl head and then rename
        # tempOutput[,36] = dBAsum     # CB: this should be dbaSUM not just dba. But column name will be dbA
        #tempOutput[,37] = dBAsum  # CB: this is actually the dbC column!! not dbasum. We are not applying cweight. We can elim this colum
       # tempOutput[,38] =  zweight  # CB: colname is dbF really it should be dbZ is the nomenclature
        # tempOutput[,48] = vers
        # tempOutput[,52] = timezone # CB: so apparently GPSTimeAdjustment column header is the timezone?
        #
        # colnames(tempOutput) <- NVSPLhead

        ## separate tempOutput by unique day hours
        tempOutput <- cbind(unqHrs, tempOutput) #add a column to sort by
        unqHrData <- split(tempOutput, tempOutput[,1]) #find where to split the data

        ## write out data to separate files, breaks into hours
        for(hr in 1:length(unqHrData))
        {
          dataToWrite <- matrix(unqHrData[[hr]],ncol=dim(tempOutput)[2])[,-1]
          colnames(dataToWrite) <- NVSPLhead
          outFileName <- paste(NVSPLdir,"\\", "NVSPL_", site, "_",
                               gsub(" ","_",gsub("-","_",names(unqHrData[hr]))),
                               ".txt", sep="")

          if ( file.exists(outFileName) == T) {write.table(dataToWrite, file=outFileName, append = TRUE, na="", quote=F, row.names=F, col.names = F, sep = ",") }
          if (!file.exists(outFileName) == T) {write.table(dataToWrite, file=outFileName, na="", quote=F, row.names=F,  sep = ",") }

        }

      } ## END DAY LOOP (d)
    }  ## END DIRECTORY LOOP (ff)
  } # end if test.file == FALSE
}  # end Wave_To_NVSPL function

