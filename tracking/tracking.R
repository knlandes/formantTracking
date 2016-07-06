
library (phonTools)
source ('tracking/formanttrack.R')
vowels = c('AA1','AE1','AH1','AO1','AW1','AY1','EH1',
           'ER1','EY1','IH1','IY1','OW1','OY1','UH1','UW1')
womit = c('THE','WORD','IS')

# paths
files = list.files('csvs', full.name = TRUE)
wav = loadsound ('../14_22050.wav')
dat = read.csv (files[1])[,-1]       # read file
dat = dat[dat$phon %in% vowels & !(dat$word %in% womit),]     # select only those rows with a phon in the set of vowels
colnames(dat)[1]='vowel'
head (dat)
nrow (dat)

fs=22050


i = 11
span = (dat[i,2]*fs):(dat[i,3]*fs)
tmp = wav$sound[span]


cutoffs = seq(5000,8000,length.out=6)
par (mfrow = c(2,3), mar = c(4,4,1,1))
for (i in 1:6){
  formanttrack (tmp,fs=fs, timestep = 5, windowlength=30,cutoff=cutoffs[i])
}
  

ffs = formanttrack (tmp,fs=fs, timestep = 5, windowlength=30,cutoff=cutoffs[6])


fixffs = function (ffs){

  ### delete, replace, reassign formant. 
   
  times = rep(ffs[,1],5)
  freqs = unlist(ffs[,2:6])
  nf = rep(1:5,each = nrow (ffs))
  
  
  par (mfrow = c(1,1), mar = c(4,4,1,1))
  plot (ffs[,1],ffs[,1],ylim=c(50,cutoffs[5]),pch=16,col=1)
  points (ffs[,1],ffs[,3],pch=16,col=2)
  points (ffs[,1],ffs[,4],pch=16,col=3)
  points (ffs[,1],ffs[,5],pch=16,col=4)
  points (ffs[,1],ffs[,6],pch=16,col=5)

  identify(times,freqs, n=1,plot=FALSE)
  
  

}



