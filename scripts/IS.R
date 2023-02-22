IS <- function(filename1,filename2){
  node <- read.table(filename1,sep='\t',header=T,row.names = 1)
  HVG <- read.table(filename2,sep='\t',header=T,row.names = 1)
 
  diff <- data.frame(row.names = rownames(node))
  for (i in 2:ncol(node)){
    tmp.1 <- as.data.frame(node[,i]-node[,i-1])
    diff <- cbind(diff,tmp.1)
  }
  
  diff.1 <- abs(diff)
  diff.2 <- diff.1[rowSums(diff.1)>0,]
 
  result <- matrix(nrow = nrow(diff.2),ncol=2)
  rownames(result) <- rownames(diff.2)
  colnames(result) <- c("importance score (IS)","type")
  
  for (j in 1:nrow(diff.2)){
    
    po.gn <- which(rownames(node) == rownames(diff.2)[j])
    po.updata <- which(diff.2[j,] == 1)
   
    pre <- node[po.gn,po.updata]-node[,po.updata]
    aft <- node[po.gn,po.updata+1]-node[,po.updata+1]
    stable <- node[po.gn,ncol(node)]-node[,ncol(node)]
    
    
    s1 <- length(which(pre-stable==0))/length(stable)
    s2 <- length(which(aft-stable==0))/length(stable)
    im <- s2-s1
    
    tp <- node[po.gn,po.updata+1] - node[po.gn,po.updata]
    
    result[j,1] <- im
    result[j,2] <- tp
    
  }
  result[which(result[,2] == 1),2] = "active"
  result[which(result[,2] == (-1)),2] = "inactive"
  re.1 = result[rownames(result) %in% HVG[,1] ,]
  re.2 = re.1[order(re.1[,1],decreasing = T),]
  re.3 = re.2[order(re.2[,2],decreasing = F),]
  return(re.3)    
}

# Data import
inputfile1 <- "node"
filename1 <- paste(inputfile1,".txt",sep='')
inputfile2 <- "HVG"
filename2 <- paste(inputfile2,".txt",sep='')

result <- IS(filename1,filename2)

# result save as txt file
outputfile <- "IS_result"
file.output <- paste(outputfile,".txt",sep='')

write.table(result,file=file.output,row.names=T,sep='\t',quote=F)
