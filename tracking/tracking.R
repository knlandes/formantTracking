
library (phonTools)
source ('tracking/formanttrack.R')
source ('tracking/selectCutoff.R')


# paths
files = list.files('csvs', full.name = TRUE)
sound = loadsound ('../14_22050.wav')   #change to local sound file path
csv = read.csv ('csvs/14.csv')



ffs = selectCutoff (sound, csv, rows = 2, color = 'alternate')
tffs = ffs$ffs[[1]]

editFormants (tffs, sound, csv, row=2)



