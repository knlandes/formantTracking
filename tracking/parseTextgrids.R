
##
## code below will take all segments from a text grid and record the content and the start and end times. 
## has been modified to run for all textgrids present in folder

# paths
files = list.files('textgrids', full.name = TRUE)

for (j in 1:length (files)){

  # filename for writing out
  outfile = list.files('textgrids', full.name = FALSE)
  tmp = strsplit(outfile,split='\\.')[[j]][1]
  outfile = paste(substr(tmp,1,2),'.csv',sep='')
  
  # text line by line
  txt = readLines (files[j])
  #str (txt) ## 2988 length character vector
  
  # number of segments (including silence and so on)
  n = as.numeric (strsplit (txt[14], split = '=')[[1]][2]) #n = 500
  steps = seq (16,4*n+15,4) #sequence starting at 16, ending at 2015, counting in steps of 4, (meaning it ends at 2012)
  
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
  
  #number of words
  n = as.numeric (strsplit (txt[max(steps)+8], split = '=')[[1]][2]) #line 2020, max() finds the peak value, in this case 2012 (plus an additional 8). n is now 242
  steps = seq (max(steps+10),4*n+max(steps+9),4) #redefines steps as sequence starting from 2022, ending at 2989, counting by 4 (the start times of words)
  
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
  dat$wstart=0;dat$wend=0
  for (i in 1:nrow(wdat)){
    mids = (dat$start+dat$end)/2
    use = wdat$wstart[i]<mids & wdat$wend[i]>mids
    dat$word[use] = as.character(wdat$wphon[i])
    dat$wend[use] = wdat$wend[i]
    dat$wstart[use] = wdat$wstart[i]
  }
  
  # save as csv file
  fname = paste ('csvs/',outfile,sep='')
  write.csv (dat, fname)
}
