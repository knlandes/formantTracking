
##
## uncomment to debug
##

#library (phonTools)
#sound = loadsound ('../14_22050.wav');speaker = 14;
#cutoffs = c(4000,7000);i=1;j=1;windowlength=30;rows = 1:nrow(csv);makepdf = FALSE;size=c(1200,600);


################################################################
#### PARAMETERS
#sound - the sound being analyzed
#csv - the data read in from a csv file
#rows - whichs rows will be tracked? 
#cutoffs - upper and lower bounds for cutoffs
#color - color scheme for spectrograms. FALSE for black/white, TRUE for regular
#windowlength - window length in milliseconds for lpc analysis
#makepng - TRUE/FALSE makes png figures instead of plots
#size - size of plots in pixels
#speaker - speaker number needed for making plots
################################################################

selectCutoff = function (sound, csv, rows = NULL, cutoffs = c(4000,7000),color='alternate',
                         windowlength=25, makepng = FALSE, size=c(1200,600), speaker = 0){
  
  source ('tracking/formanttrack.R')
  if (is.null(rows)) rows = 1:nrow(csv) 
  fs = sound$fs
  cutoffs = seq(cutoffs[1],cutoffs[2],length.out=6)
  
  if (makepng){
    for (j in rows){
      span = (csv[j,5]*fs):(csv[j,6]*fs)
      tmp = sound$sound[span]
      png (file = paste(speaker,'-',j,'.png',sep=''), width=size[1],height=size[2])
      par (mfrow = c(2,3), mar = c(4,4,1,1))
      for (i in 1:6){  
        formanttrack (tmp, timestep = 3, windowlength=windowlength,cutoff=cutoffs[i],color=color)
      }
      dev.off() # this one is tricky
    }
  }
  if (!makepng){
    best = NULL
    ffs = list()
    count = 1
    for (j in rows){
      span = (csv[j,5]*fs):(csv[j,6]*fs)
      tmp = sound$sound[span]
      par (mfrow = c(2,3), mar = c(4,4,1,1))
      tffs = list()
      for (i in 1:6){  
        tffs[[i]] = formanttrack (tmp, timestep = 5, windowlength=windowlength,cutoff=cutoffs[i],color=color)
      }
      tmp = scan (what=character(), n=1,quiet = TRUE)
      if (!(tmp %in% c('1','2','3','4','5','6'))) stop ('User Exit')
      tmp = as.numeric (tmp)
      best[count] = tmp    
      ffs[[count]] = tffs[[tmp]]
      count = count+1
    }
    output = list (ffs = ffs, cutoff = cutoffs[best])
    output
  }
}  



