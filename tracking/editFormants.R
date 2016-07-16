



editFormants = function (ffs, sound = NULL, csv = NULL, row = 1){
  ### delete, reassign formant. 
  selection = 'd'
  spectrogram = FALSE; add = FALSE;
  par (mfrow = c(1,1), mar = c(4,4,1,1))
  if (!is.null(sound) & !is.null(ffs) & !is.null(csv)){
    par(mfrow = c(2,1), oma = c(4,4,1,1) + 0.1, mar = c(.2,.2,.2,.2) + 0.1)
    layout (mat = (c(1,2)),heights = c(.5,1))
    
    span = (csv[row,5]*sound$fs):(csv[row,6]*sound$fs)
    tmp = sound$sound[span]
    spect = spectrogram (tmp,fs=sound$fs,show=FALSE,maxfreq=max(ffs+300), color = 'alternate')
    add = TRUE
    pitch = pitchtrack (tmp,fs=sound$fs,show=FALSE)
    power = powertrack (tmp,fs=sound$fs,show=FALSE)
  }
  while (selection%in%c('d','r')){

    plot (pitch[,1],pitch[,2], type='b',col=4,pch=16,xlim=c(0,1000*length(tmp)/sound$fs), 
          ylim = c(0,300),xaxt='n',yaxt='n')
    pow = (power[,2] - min(power[,2])) / abs(min(power[,2])) * 290
    points (power[,1], pow, type='b',col=2,pch=16,xlim=c(0,1000*length(tmp)/sound$fs))
    plot (spect)
    mtext (side=1,text = 'Time (ms)',line=2.2)
    plotffs (ffs,add=add)
      
    cat ('\n      Options:    (d)elete    (q)uit   (r)eassign    \n\n')
    selection = scan (n=1, what=character(),quiet=TRUE)
    cat ('\nSelection: ', selection)
    
    if (selection == 'd'){
      cat ('\n    How many?  \n\n')
      num = scan (n=1, what=numeric(),quiet=TRUE)
      for (i in 1:num){
        coord = locator (n=1)
        rw = which.min(abs(ffs[,1]-coord$x))
        cl = which.min(abs(ffs[rw,]-coord$y))
        ffs[rw,cl] = 0
        plotffs (ffs,add=add)
        save(ffs,file='fixffs_tmp.rda')
      }
    }
    if (selection == 'r'){
      cat ('\n    How many?  \n\n')
      num = scan (n=1, what=numeric(),quiet=TRUE)
      cat ('\n    Too which formant?  \n\n')
      fn = scan (n=1, what=numeric(),quiet=TRUE)      
      for (i in 1:num){
        coord = locator (n=1)
        dists = log((matrix(rep(abs(ffs[,1]-coord$x),5),ncol=5) * abs(ffs[,-1]-coord$y)))
        cl = order(apply (dists, 2, min))[1]
        rw = which.min(dists[,cl])
        
        ffs[rw,fn+1] = ffs[rw,cl+1]
        ffs[rw,cl+1] = 0
        
        plotffs (ffs,add=add)
        save(ffs,file='fixffs_tmp.rda')
      }
    }
  }
  save(ffs,file='fixffs_tmp.rda')
  ffs
}





plotffs = function (ffs, add = FALSE){
  if (!add) plot (ffs[,1],ffs[,1],ylim=c(0,max(ffs)+100),pch=16,type='n',yaxs='i')
    points (ffs[ffs[,2]!=0,1],ffs[ffs[,2]!=0,2],pch=16,cex=2,col='white')
    points (ffs[ffs[,3]!=0,1],ffs[ffs[,3]!=0,3],pch=16,cex=2,col='white')
    points (ffs[ffs[,4]!=0,1],ffs[ffs[,4]!=0,4],pch=16,cex=2,col='white')
    points (ffs[ffs[,5]!=0,1],ffs[ffs[,5]!=0,5],pch=16,cex=2,col='white')
    points (ffs[ffs[,6]!=0,1],ffs[ffs[,6]!=0,6],pch=16,cex=2,col='white')
    
    points (ffs[ffs[,2]!=0,1],ffs[ffs[,2]!=0,2],pch=16,col=1,cex=1.3)
    points (ffs[ffs[,3]!=0,1],ffs[ffs[,3]!=0,3],pch=16,col=2,cex=1.3)
    points (ffs[ffs[,4]!=0,1],ffs[ffs[,4]!=0,4],pch=16,col=3,cex=1.3)
    points (ffs[ffs[,5]!=0,1],ffs[ffs[,5]!=0,5],pch=16,col=4,cex=1.3)
    points (ffs[ffs[,6]!=0,1],ffs[ffs[,6]!=0,6],pch=16,col=5,cex=1.3)
    
}


