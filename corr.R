corr <- function (directory, threshold=0) {
        currentDirectory=getwd()
        currDirPiece=substr(currentDirectory, nchar(currentDirectory)-7, nchar(currentDirectory))
        if (currDirPiece!=directory) {
                setwd(directory)
        }
        

        lastFrame=complete(directory, 1:332)
        idx = lastFrame$nobs>threshold
        lastFrame=lastFrame[idx,]
        noofvalidobs=nrow(lastFrame)
        if (noofvalidobs>0) {
                ncorr=vector(,length=noofvalidobs)
                for (i in 1:noofvalidobs) {
                        fileno=lastFrame$id[i]
                        if (fileno<10) {
                                filename=paste('00',fileno,'.csv',sep='')     
                        } else if (fileno<100) {
                                filename=paste('0',fileno,'.csv',sep='')  
                        } else {
                                filename=paste(fileno,'.csv',sep='')
                        }
                        datapoints=read.csv(filename)
                        
                        ncorr[i]=cor(datapoints$sulfate, datapoints$nitrate, use="complete.obs")
                }
        }
        ncorr
        
}