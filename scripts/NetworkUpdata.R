# updated asymmetrical parameter 
W_updata <- function(ex1,ex2){ #ex1_new,ex2_old
  
  ex1_1 <- ex1
  ex1_2 <- ex2
  
  w <- matrix(data = 0, nrow = length(ex1_1), ncol = length(ex1_1))
  for (i in 1:length(ex1_1)) {
    for (j in i:length(ex1_1)) {
      if (i == j) 
      {w[i,j] = 0}else{
        w[i,j] = ex1_1[i]*ex1_2[j]
        w[j,i] = ex1_1[j]*ex1_2[i]}
    }
  }
  
  return(w)
}


# Hopfield energy 
En_updata <- function(w,ex){
  
  result <- -1/2* t(ex) %*% w %*% ex 
  
  return(result)
}


# Hopfield energy model the Waddington's landscape
HNN_updata <- function(filename,indexfile,i,j){
  
  newgene <- read.table(filename,sep='\t',header=T,row.names = 1) # input data
  index <- read.table(indexfile,sep='\t',header=T) # input index
  random <- seq(1,0,-0.01)
  result <- matrix(nrow = length(random))
  pre_data <- index[index[,2]==i,]
  late_data <- index[index[,2]==j,]
  
  a <- newgene[,colnames(newgene) %in% pre_data[,1]==T]
  b <- newgene[,colnames(newgene) %in% late_data[,1]==T]
  
  a <- a[order(rownames(a)),]
  b <- b[order(rownames(b)),]
  
  for (cell in 1:ncol(a)) {
    
    num <- seq(1,0,-0.01)
    
    energy <- c()
    
    for (n in 1:length(num)) {
      
      a0 <- apply(a, 1, mean)
      a0_1 <- a0[sort(names(a0),decreasing = FALSE)]
      
      old0 <- a0_1
      
      set.seed(123)
      
      a1 <- a[sample(1:nrow(a), num[n]*nrow(a), replace=F),]
      
      a2 <- b[which(rownames(b) %in% rownames(a1)==F),]
      
      a1_1 <- apply(a1,1,mean)
      a2_1 <- apply(a2,1,mean)
      
      a3 <- append(a1_1,a2_1)
      
      a3_1 <- a3[sort(names(a3),decreasing = FALSE)]
      
      no1 <- append(a1[,cell],a2_1)
      names(no1) <- c(rownames(a1),names(a2_1))
      
      no1 <- no1[sort(names(no1),decreasing = FALSE)]
      
      assign(paste("node",n,sep=""),no1)
      
      node <- get(paste("node",n,sep=""))
      
      assign(paste("new",n,sep=""),a3_1)
      new <- get(paste("new",n,sep=""))
      old <- get(paste("old",n-1,sep=""))
      
      assign(paste("old",n,sep=""),new)
      
      w1 <- W_updata (new,old)
      
      E1 <- En_updata(w1,node)
      
      energy <- c(energy,E1)
    }
    
    energy <- as.data.frame(energy)
    assign(paste("energy",cell,sep=""),energy)
    en0 <- get(paste("energy",cell,sep=""))
    result <- cbind(result,en0)
  }
  
  result <- result[,-1]
  
  return(result)
  
}

# Data import
inputfile <- "updata_test"
filename <- paste(inputfile,".txt",sep='')

inputfile1 <- "index_test"
indexfile <- paste(inputfile1,".txt",sep='')

result <- HNN_updata(filename=filename,indexfile=indexfile,i=1,j=2)

# result save as txt file
outputfile <- "update_En"
file_update <- paste(outputfile,".txt",sep='')
write.table(result,file=file_update,row.names=T,sep='\t',quote=F)