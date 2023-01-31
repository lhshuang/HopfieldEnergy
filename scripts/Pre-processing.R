# Input data pre-processing
# Z-score normalization
# HVGs selection
preprocess <- function(filename){
  data <- read.table(filename,sep='\t',header=T,row.names = 1) # input data
  
  me.data <- apply(data, 1, mean)
  sd.data <- apply(data,1,sd)
  Zscore <- (data - me.data)/sd.data
  
  Zscore1 <- Zscore[rowSums(Zscore) > 0,]
  Zscore1 <- na.omit(Zscore1)
  
  variation <- as.data.frame(apply(Zscore1, 1, var))
  variation[2] <- rownames(variation)
  variation <- variation[order(variation[,1],decreasing = T),]
  variation1 <- variation[1:(0.05*length(variation[,1])),] 
  
  HVGs_data <- Zscore1[rownames(Zscore1) %in% variation1[,2]==T,]
  
  return(HVGs_data)
}


# Data import and preprocessing
inputfile <- "test"
filename <- paste(inputfile,".txt",sep='')
result <- preprocess(filename = filename)

# result save as txt file
outputfile <- "predata"
file_HVGs <- paste(outputfile,".txt",sep='')
write.table(result,file=file_HVGs,row.names=T,sep='\t',quote=F)







