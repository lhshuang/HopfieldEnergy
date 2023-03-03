# Construction of Hopfield network at stable fixed point

E <- function(filename,ann){
  ex <- read.table(filename,sep='\t',header=T,row.names = 1) # input data
  anndata <- read.table(ann,sep='\t',header=T) # input cell types annotation data
  cell <- as.data.frame(colnames(ex))
  ex[ex < 0] <- 0
  ex[ex > 0] <- 1
  result <- c()
  for (n in 1:ncol(ex)) {
    w <- matrix(data = 0, nrow = length(ex[,1]), ncol = length(ex[,1]))
    for (i in 1:length(ex[,n])) {
      for (j in i:length(ex[,n])) {
        if (i == j) 
        {w[i,j] = 0}else{
          w[i,j] = ex[i,n]*ex[j,n]
          w[j,i]= w[i,j]}
      } 
    }
    E1 <- -1/2* t(ex[,n]) %*% w %*% ex[,n] 
    result <- c(result,E1)
  }
  result <- as.data.frame(result)
  energy <- cbind(cell,result)
  energy <- energy[order(energy[,1]),]
  anndata <- anndata[order(anndata[,1]),]
  enregy_result <- cbind(energy[,1],anndata[,2],energy[,2])
  colnames(enregy_result) <- c("Barcode","CellType","Energy")
  return(enregy_result)

}






# Data import
inputfile <- "predata"
filename <- paste(inputfile,".txt",sep='')

inputfile1 <- "barcode"
ann <- paste(inputfile1,".txt",sep='')

result <- E(filename = filename,ann=ann)


# result save as txt file
outputfile <- "En"
file_En <- paste(outputfile,".txt",sep='')
write.table(result,file=file_En,row.names=T,sep='\t',quote=F)
