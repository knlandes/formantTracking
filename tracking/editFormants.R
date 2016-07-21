



################################################################
#### PARAMETERS
#ffs - dataframe containing formant tracks
#sound - sound file if you want a spectrogram
#csv - csv data if you want a spectrogram
#row - row number(s) interested in editing if you want a spectrogram
################################################################



editFormants = function (ffs, sound = NULL, csv = NULL, row = 1){
  ### delete, reassign formant. 
  selection = 'd'
  usespect = FALSE; add = FALSE;
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
    usespect = TRUE
    
  }
  ffsarc = list(original=ffs)
  while (selection%in%c('d','r','z')){

    if (usespect){
      plot (pitch[,1],pitch[,2], type='b',col=4,pch=16,xlim=c(0,1000*length(tmp)/sound$fs), 
          ylim = c(0,300),xaxt='n',yaxt='n',xaxs='i')
      pow = (power[,2] - min(power[,2])) / abs(min(power[,2])) * 290
      points (power[,1], pow, type='b',col=2,pch=16,xlim=c(0,1000*length(tmp)/sound$fs))
      plot (spect)
      mtext (side=1,text = 'Time (ms)',line=2.2)
    }
    plotffs (ffs,add=add)
      
    cat ('\n      Options:    (d)elete    (r)eassign    (z)oom     (q)uit  \n\n')
    selection = scan (n=1, what=character(),quiet=TRUE)
    cat ('\nSelection: ', selection, ' \n\n')
    
    if (selection == 'd'){
      cat ('\n    How many?   (0 for box)  \n\n')
      num = scan (n=1, what=numeric(),quiet=TRUE)
      if(num>0)for (i in 1:num){
        coord = locator (n=1)
        if (coord$y<0) break
        
        dists = log((matrix(rep(abs(ffs[,1]-coord$x),5),ncol=5) * abs(ffs[,-1]-coord$y)))
        cl = order(apply (dists, 2, min))[1]
        rw = which.min(dists[,cl])
        #rw = which.min(abs(ffs[,1]-coord$x))
        #cl = which.min(abs(ffs[rw,]-coord$y))
        ffs[rw,cl] = 0
        ll = length(ffsarc)
        ffsarc[[ll+1]] = ffs
        plotffs (ffs,add=add)
        save(ffsarc,file='fixffsarc.rda')
      }
      if (num==0){
        coord = locator (n=2)
        if (coord$y[1]<0 | coord$y[2]<0) break
        rect (coord$x[1],coord$y[1],coord$x[2],coord$y[2],lwd=5)
        rect (coord$x[1],coord$y[1],coord$x[2],coord$y[2],lwd=1,border='white')
        
        cat ('\n  Look good?   (y)es    (n)o    \n\n')
        choice = scan (n=1, what=character(),quiet=TRUE)
        
        if (choice=='y'){
          rw = (ffs[,1]>coord$x[1]) & (ffs[,1]<coord$x[2])
          cl = (ffs[,-1]>coord$y[1]) & (ffs[,-1]<coord$y[2])
          for (i in 1:nrow(ffs)) cl[i,] = cl[i,]*rw[i]
          cl = cbind (0, cl)
          ffs[cl==1] = 0
          ll = length(ffsarc)
          ffsarc[[ll+1]] = ffs
          plotffs (ffs,add=add)
          save(ffsarc,file='fixffsarc.rda')
        }
      }
    }
    if (selection == 'r'){
      cat ('\n    How many?     (0 for box)   \n\n')
      num = scan (n=1, what=numeric(),quiet=TRUE)
      cat ('\n    Too which formant?  \n\n')
      fn = scan (n=1, what=numeric(),quiet=TRUE)      
      if(num>0)for (i in 1:num){
        coord = locator (n=1)
        if (coord$y<0) break
        dists = log((matrix(rep(abs(ffs[,1]-coord$x),5),ncol=5) * abs(ffs[,-1]-coord$y)))
        cl = order(apply (dists, 2, min))[1]
        rw = which.min(dists[,cl])
        
        ffs[rw,fn+1] = ffs[rw,cl+1]
        ffs[rw,cl+1] = 0
        
        ll = length(ffsarc)
        ffsarc[[ll+1]] = ffs
        plotffs (ffs,add=add)
        save(ffsarc,file='fixffsarc.rda')
      }
      
      if (num==0){
        coord = locator (n=2)
        if (coord$y[1]<0 | coord$y[2]<0) break
        rect (coord$x[1],coord$y[1],coord$x[2],coord$y[2],lwd=5)
        rect (coord$x[1],coord$y[1],coord$x[2],coord$y[2],lwd=1,border='white')
        
        cat ('\n  Look good?   (y)es    (n)o    \n\n')
        choice = scan (n=1, what=character(),quiet=TRUE)
        
        if (choice=='y'){
          rw = (ffs[,1]>coord$x[1]) & (ffs[,1]<coord$x[2])
          cl = (ffs[,-1]>coord$y[1]) & (ffs[,-1]<coord$y[2])
          for (i in 1:nrow(ffs)) cl[i,] = cl[i,]*rw[i]
          cl = cbind (0, cl)

          for (i in 1:nrow(cl))
            if (sum(cl[i,])>0) ffs[i,fn+1] = ffs[i,cl[i,]==1]
          
          ffs[cl==1] = 0
  
          ll = length(ffsarc)
          ffsarc[[ll+1]] = ffs
          plotffs (ffs,add=add)
          save(ffsarc,file='fixffsarc.rda')
        }
      }
    }
    
    if (selection == 'z'){
      cat ('\n    Click new start and end:  \n\n')
      coord = locator (n=2)
      if (coord$y[1]<0 | coord$y[2]<0) break
      abline (v = coord$y,lwd=5)
      abline (v = coord$y,lwd=1,col='white')
            
      cat ('\n  Look good?   (y)es    (n)o    \n\n')
      choice = scan (n=1, what=character(),quiet=TRUE)
      
      if (choice=='y'){
        use = coord$x[1]<ffs[,1] & coord$x[2]>ffs[,1]
        ffs = ffs[use,]
        ll = length(ffsarc)
        ffsarc[[ll+1]] = ffs
        
        plotffs (ffs,add=add)
        save(ffsarc,file='fixffsarc.rda')
      }
    }
  }
  save(ffsarc,file='fixffsarc.rda')
  ffs
}





plotffs = function (ffs, add = FALSE){
  if (!add) plot (ffs[,1],ffs[,1],ylim=c(0,max(ffs)+500),pch=16,type='n',yaxs='i')
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


