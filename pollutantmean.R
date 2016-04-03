pollutantmean <- function (directory, pollutant, id=1:332) {
        currentDirectory=getwd()
        currDirPiece=substr(currentDirectory, nchar(currentDirectory)-7, nchar(currentDirectory))
        if (currDirPiece!=directory) {
                setwd(directory)
        }
        control=0
        for (i in id) {
                if (control==0) {
                        control=1
                        if (i<10) {
                                filename=paste('00',i,'.csv',sep='')     
                        } else if (i<100) {
                                filename=paste('0',i,'.csv',sep='')  
                        } else {
                                filename=paste(i,'.csv',sep='')
                        }
                        datapoints=read.csv(filename)
                } else {
                        if (i<10) {
                                filename=paste('00',i,'.csv',sep='')     
                        } else if (i<100) {
                                filename=paste('0',i,'.csv',sep='')  
                        } else {
                                filename=paste(i,'.csv',sep='')
                        }
                        datapoints1=read.csv(filename)
                        datapoints=rbind(datapoints1,datapoints)
                }
        }
        print(str(datapoints))
        
        if (pollutant==c('sulfate')) {
                meanx=mean(datapoints$sulfate, na.rm=TRUE)
        } else if (pollutant==c('nitrate')) {
                meanx=mean(datapoints$nitrate, na.rm=TRUE)
        } else {
                meanx=0
                print('Error in pollutant name!')
        }
        
        meanx
}