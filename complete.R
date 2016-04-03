complete <- function (directory, id=1:232){
        currentDirectory=getwd()
        currDirPiece=substr(currentDirectory, nchar(currentDirectory)-7, nchar(currentDirectory))
        if (currDirPiece!=directory) {
                setwd(directory)
        }
        
        nooffiles=length(id)
        lastFrame=data.frame(id=integer(0),nobs=integer(0))
        
        for (i in 1:nooffiles) {
                fileno=id[i]
                if (fileno<10) {
                        filename=paste('00',fileno,'.csv',sep='')     
                } else if (fileno<100) {
                        filename=paste('0',fileno,'.csv',sep='')  
                } else {
                        filename=paste(fileno,'.csv',sep='')
                }
                datapoints=read.csv(filename)
                for (j in 1:ncol(datapoints)) {
                        idx=is.na(datapoints[,j])==FALSE
                        datapoints=datapoints[idx,]
                }
                lastFrame[i,1]=fileno
                lastFrame[i,2]=nrow(datapoints)
        }
        lastFrame
}