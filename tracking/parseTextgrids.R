
##
## code below will take all segments from a text grid and record the content and the start and end times. 
##

# paths
files = list.files('textgrids', full.name = TRUE)

# filename for writing out
outfile = list.files('textgrids', full.name = FALSE)
outfile = paste(strsplit(outfile,split='\\.')[[1]][1],'.csv',sep='')

# text line by line
txt = readLines (files[1])
str (txt) ## 2788 length character vector

# number of segments (including silence and so on)
n = as.numeric (strsplit (txt[14], split = '=')[[1]][2])
steps = seq (16,4*n+15,4)

starts=NULL
for (i in 1:length(steps)){
  tmp = txt[steps[i]]   # get line
  tmp = as.numeric (strsplit (tmp,split='=')[[1]][2])  # split after =
  starts[i] = tmp
}
ends=NULL
for (i in 1:length(steps)){
  tmp = txt[steps[i]+1]   # get line
  tmp = as.numeric (strsplit (tmp,split='=')[[1]][2])  # split after =
  ends[i] = tmp
}
phon=NULL
for (i in 1:length(steps)){
  tmp = txt[steps[i]+2]   # get line
  tmp = strsplit (tmp,split='=')[[1]][2]  # split after =
  tmp = strsplit (tmp,split='\"')[[1]][2]  # split after "
  phon[i] = tmp
}
dat = data.frame (phon = phon, start = starts, end = ends)

n = as.numeric (strsplit (txt[max(steps)+8], split = '=')[[1]][2])
steps = seq (max(steps+10),4*n+max(steps+9),4)

wstarts=NULL
for (i in 1:length(steps)){
  tmp = txt[steps[i]]   # get line
  tmp = as.numeric (strsplit (tmp,split='=')[[1]][2])  # split after =
  wstarts[i] = tmp
}
wends=NULL
for (i in 1:length(steps)){
  tmp = txt[steps[i]+1]   # get line
  tmp = as.numeric (strsplit (tmp,split='=')[[1]][2])  # split after =
  wends[i] = tmp
}
wphon=NULL
for (i in 1:length(steps)){
  tmp = txt[steps[i]+2]   # get line
  tmp = strsplit (tmp,split='=')[[1]][2]  # split after =
  tmp = strsplit (tmp,split='\"')[[1]][2]  # split after "
  wphon[i] = tmp
}
wdat = data.frame (wphon = wphon, wstart = wstarts, wend = wends)

head (dat)
head (wdat)

dat$word='-'
for (i in 1:nrow(wdat)){
  mids = (dat$start+dat$end)/2
  use = wdat$wstart[i]<mids & wdat$wend[i]>mids
  dat$word[use] = as.character(wdat$wphon[i])
}


# save as csv file

write.csv (dat, 'csvs/14.csv')


