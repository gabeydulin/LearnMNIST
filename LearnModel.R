learnModel <- function(label, trainData, trainLabels){
  #learn model 
  #relabel trainlabels to '0' and '1' based on input parameter "label", save to vector Y
  y <- matrix(data = 0, nrow = nrow(trainLabels), ncol = ncol(trainLabels))# 60000*1
  for (k in 1:nrow(trainLabels))
  {
    if (trainLabels[k]==label) {
      y[k]<-1 }
    else
    {
      y[k]<-0 
    }
  }
  #initialization of parameters
  epsilon<-0.01;#Termination of the learning
  mu<-0.0001;# learning rate
  lambda <-0.1;# learning rate
  J<-epsilon+1;
  prevJ<-0;
  g<-matrix(data = 0, nrow = nrow(trainData), ncol = 1)# vector for sigmoid function, 60000*1
  
  theta <- matrix((runif(ncol(trainData), min = -0.001, max = 0.001)), ncol=1, nrow=ncol(trainData)); #vector 785*1
  f <- function(x) 1/(1+exp(-x)); #sigmoid
  
  while (abs(prevJ-J)>epsilon){
    g<-apply((trainData)%*%theta,2, f);# g(X*Theta)
    delta<-t(trainData)%*%(g-y);
    delta[-1]<-delta[-1]-2*lambda*mu*theta[-1]; #Add up regularization term without the bias.
    theta<-theta - mu*delta;
    # add 0.000001 to avoid log(0)
    error.matrix <- (-y*log(g+0.000001) - (1-y)*log(1-g+0.000001))
    error.sum<-sum(error.matrix);
    error=((1/nrow(trainData))*error.sum) +lambda*sum(theta[-1]^2); 
    print(error);
    print(label);
    prevJ<-J;
    J<-error;
    #trying to adjust mu
    if (J>prevJ){
      mu<-mu/2
    };
    
  }
  return(theta);
}