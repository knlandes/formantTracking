


ffs = q$ffs[[1]]


fixffs = function (ffs,add = FALSE){
  
  ### delete, replace, reassign formant. 
  
  selection = '-'
  
  while (selection!='q'){
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
        #rw = which.min(abs(ffs[,1]-coord$x))
        
        dists = log((matrix(rep(abs(ffs[,1]-coord$x),5),ncol=5) * abs(ffs[,-1]-coord$y)))
        cl = order(apply (dists, 2, min))[1]
        rw = which.min(dists[,cl])
        
        ffs[rw,fn+1] = ffs[rw,cl+1]
        ffs[rw,cl+1] = 0
        
        plotffs (ffs,add=add)
        save(ffs,file='fixffs_tmp.rda')
      }
    }
    
    #save incrementally
  }
  ffs
  
  
}



plotffs = function (ffs, add = FALSE){
par (mfrow = c(1,1), mar = c(4,4,1,1))
if (!add) plot (ffs[,1],ffs[,1],ylim=c(0,max(ffs)+100),pch=16,type='n',yaxs='i')
points (ffs[ffs[,2]!=0,1],ffs[ffs[,2]!=0,2],pch=16,col=1)
points (ffs[ffs[,3]!=0,1],ffs[ffs[,3]!=0,3],pch=16,col=2)
points (ffs[ffs[,4]!=0,1],ffs[ffs[,4]!=0,4],pch=16,col=3)
points (ffs[ffs[,5]!=0,1],ffs[ffs[,5]!=0,5],pch=16,col=4)
points (ffs[ffs[,6]!=0,1],ffs[ffs[,6]!=0,6],pch=16,col=5)
}


