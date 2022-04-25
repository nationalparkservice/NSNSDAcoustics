# FUNCTION PAMGuide.R

# Internal function called by Wave_To_NVSPL

# Computes calibrated or relative acoustic metrics from WAV audio files.

# This code accompanies the manuscript:

#   Merchant et al. (2015). Measuring Acoustic Habitats. Methods
#   in Ecology and Evolution

# and follows the equations presented in Appendix S1. It is not necessarily
# optimised for efficiency or concision.

# See Appendix S1 of the above manuscript for detailed instructions

# Copyright (c) 2014 The Authors.

# Author: Nathan D. Merchant. Last modified 22 Sep 2014

#' @name PAMGuide
#' @title Internal function from PAMGuide code.
#' @description Internal function from PAMGuide code.
#' @import tuneR
#' @importFrom grDevices graphics.off
#' @importFrom stats mvfft
#' @include Meta.R PAMGuide.R PAMGuide_Meta.R
#' @export
#' @keywords internal
#'

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




