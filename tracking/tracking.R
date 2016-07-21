
library (phonTools)
source ('tracking/formanttrack.R')
source ('tracking/selectCutoff.R')
source ('tracking/editFormants.R')


# paths
files = list.files('csvs', full.name = TRUE)
sound = loadsound ('../14_22050.wav')   #change to local sound file path
csv = read.csv ('csvs/14.csv')


ffs = selectCutoff (sound, csv, rows = 1:2, color = 'alternate', makepng = TRUE, speaker = 14)

ffs = selectCutoff (sound, csv, rows = 2, color = 'alternate')

ffs = q

editFormants (ffs$ffs[[1]])

editFormants (ffs$ffs[[1]], sound, csv, row=2)



load ('fixffsarc.rda')

par (mfrow = c(3,1), mar = c(4,4,1,1))
plotffs (ffsarc[[1]])
plotffs (ffsarc[[2]])
plotffs (ffsarc[[3]])


