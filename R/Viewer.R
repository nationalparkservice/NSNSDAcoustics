# FUNCTION Viewer.R ============================================================

# Internal function called by Wave_To_NVSPL

# Function to plot data analysed in PAMGuide.R and Meta.R.

# This code accompanies the manuscript:

#   Merchant et al. (2015). Measuring Acoustic Habitats. Methods
#   in Ecology and Evolution

# and follows the equations presented in Appendix S1. It is not necessarily
# optimised for efficiency or concision.

#	See Appendix S1 of the above manuscript for detailed instructions

# Copyright (c) 2014 The Authors.

# Author: Nathan D. Merchant. Last modified 22 Sep 2014

#' @name Viewer
#' @title Internal function from PAMGuide code.
#' @description Internal function from PAMGuide code.
#' @import tuneR
#' @importFrom graphics axis axis.POSIXct lines legend mtext
#' @importFrom grDevices dev.new graphics.off
#' @include Meta.R PAMGuide.R PAMGuide_Meta.R
#' @export
#' @keywords internal
#'

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


