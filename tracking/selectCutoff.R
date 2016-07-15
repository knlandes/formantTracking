
##
## uncomment to debug
##

#library (phonTools)
sound = loadsound ('../14_22050.wav');speaker = 14;
cutoffs = c(4000,7000);i=1;j=1;windowlength=30;rows = 1:nrow(dat);makepdf = FALSE;size=c(1200,600);

q = selectCutoff (14, sound, rows = 2, color = 'alternate')



selectCutoff = function (speaker, sound, rows = NULL, cutoffs = c(4000,7000),color='alternate',
                         windowlength=30, f0=NULL, makepng = FALSE, size=c(1200,600)){
  
  source ('tracking/formanttrack.R')
  vowels = c('AA1','AE1','AH1','AO1','AW1','AY1','EH1',
             'ER1','EY1','IH1','IY1','OW1','OY1','UH1','UW1')
  womit = c('THE','WORD','IS')
  
  if (!is.null(f0)) windowlength = ceiling (3*1000/f0)

  fname = paste ('csvs/',speaker,'.csv',sep='')
  dat = read.csv (fname)[,-1]
  dat = dat[dat$phon %in% vowels & !(dat$word %in% womit),]     # select only those rows with a phon in the set of vowels
  colnames(dat)[1]='vowel' 
  if (is.null(rows)) rows = 1:nrow(dat) 
  fs = sound$fs
  cutoffs = seq(cutoffs[1],cutoffs[2],length.out=6)
  
  if (makepng){
    for (j in rows){
      span = (dat[j,5]*fs):(dat[j,6]*fs)
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
      span = (dat[j,5]*fs):(dat[j,6]*fs)
      tmp = sound$sound[span]
      par (mfrow = c(2,3), mar = c(4,4,1,1))
      tffs = list()
      for (i in 1:6){  
        tffs[[i]] = formanttrack (tmp, timestep = 5, windowlength=windowlength,cutoff=cutoffs[i],color=color)
      }
      tmp = scan (what=character(), n=1,quiet = TRUE)
      if (!(tmp %in% c('1','2','3','4','5','6'))) stop ('User Exit')
      tmp = as.numeric (tmp)
      best[j] = tmp    
      ffs[[count]] = tffs[[tmp]]
      count = count+1
    }
    output = list (ffs = ffs, cutoff = cutoffs[best])
    output
  }
}  



