
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


