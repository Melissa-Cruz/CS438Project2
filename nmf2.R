library(cluster)
library(stringi)
library(NMF)
#package installation 
#install.packages("NMF")



##Input: noisePath-> the path of noise matrix; goodPath -> the path of nmf ran matrix; rank-> the number of clusters
##Output: accuracy -> the accuracy using NMF


getaccuracy <-function(noisePath, goodPath,rank){
  
  print(noisePath)
  print(goodPath)
  x <- read.csv(noisePath,header=FALSE) 
  
  
  N = nrow(x)
 
  ##rank =
  rank <- strtoi(rank) 
  print(rank)
  results <- nmf(as.matrix(x),rank,nrun=2,method="Frobenius") ## runs nmf
  
  clusterMatrix = basis(results) ## finds N by K matrix
  
  
  clusterMatrix = apply(clusterMatrix[],1, function(x){x==max(x)})
  
  ## turns boolean into one hot
  print(clusterMatrix)
  for(i in nrow(clusterMatrix))
  {
    for (j in ncol(clusterMatrix)) {
      
      if (clusterMatrix[i,j] == TRUE)
      {
        clusterMatrix[i,j] = 1
      }
      else
      {  
        clusterMatrix[i,j] = 0
      }
      
      
    }
  }
  # print(clusterMatrix)
  goodDate = read.csv(goodPath, header=FALSE) ##read no noise matrix
  groundTruthMatrix <- as.matrix(goodDate,rownames.force=NA)
  
  consensusMatrix = t(clusterMatrix)%*%clusterMatrix ## multiplying the matrix by its transpose 
  
  groundTruthMatrix = groundTruthMatrix%*%t(groundTruthMatrix) ## multiply the groundtruth matrix by its transpose
  
  #calculate accuracy
  accuracy =0
  print("------")
  print(dim(consensusMatrix))
  print(dim(groundTruthMatrix))
  print("------")
  accuracy = sum(consensusMatrix==groundTruthMatrix) / (dim(groundTruthMatrix)[1] * dim(groundTruthMatrix)[2])
  

  return(accuracy)
} 



##the path of the input folder
path = "C:\\Users\\melicruz\\Desktop\\cs438_assignment2\\matrices\\"
writePath = "c:\\Users\\melicruz\\Desktop\\cs438_assignment2\\results.csv"
M = 1 ##nrun of NMF is 1
allDataName = list.files(path) 
resMatrix = matrix(nrow=length(allDataName), ncol=4) ## create a matrix to save the result
colnames(resMatrix) = c("matrix","classes","noise","accuracy")

for(i in 1:length(allDataName))
{ 
  print(allDataName[i]) 
  noisePath = paste(path, allDataName[i]) ## connects relative and absolute paths 
  noisePath = stri_replace_all_charclass(noisePath, "\\p{WHITE_SPACE}", "") #remove the space in front of the relative path 
  ##print(noisePath)
  splitPath = unlist(strsplit(allDataName[i], "[.]"))
  
  splitPath2 = unlist(strsplit(splitPath[1],"_"))
  
  matrixID = splitPath2[1]
  rank = splitPath2[2]
  
  
  noiseRate = 0
  if(length(splitPath)==2)
  {
    goodPath = allDataName[i]
    noiseRate = 0
  }
  else
  {
    goodPath = paste(splitPath[1], ".", splitPath[3])
    noiseRate = paste("0.", splitPath[2]) 
    noiseRate = stri_replace_all_charclass(noiseRate, "\\p{WHITE_SPACE}","") ##removing the empty space
  }  
  goodPath = paste(path,goodPath) ## connect absolute path and relative path
  goodPath = stri_replace_all_charclass(goodPath, "\\p{WHITE_SPACE}","")
  
  ##print(noiseRate)
  ##read.csv(file1,header=FALSE)
  ##perhaps purity would be a better way of determing accuracy
  accuracy = 0
  
  for(j in 1:M)
  {
    num2 = getaccuracy(noisePath,goodPath,rank)
    print(num2)
    accuracy = accuracy + num2
  }
  
  accuracy = accuracy/M
  print(accuracy)
  
  
  resMatrix[i,1]=matrixID
  resMatrix[i,2]=rank
  resMatrix[i,3]=noiseRate
  resMatrix[i,4]=accuracy
}
write.csv(resMatrix,file=writePath, row.names=FALSE)
