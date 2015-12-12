testModel <- function(classifier, testData){
  h<-matrix(data=0, nrow=1, ncol=10);
  answer<-matrix(data=0, nrow=nrow(testData), ncol=1);
  
  for (i in 1:nrow(testData)){
    
    for (j in 1:ncol(classifier)){
      x<-testData[i,]%*%classifier[,j];
      h[j]<-1/(1+exp(-x));
    }
    
    max<-0;
    maxInd<-1;
    for (k in 1:10){
      if (h[k]>max){
        max<-h[k];
        maxInd<-k
      }
    }
    answer[i]<-maxInd-1;
  }  
  
  return(answer);
}