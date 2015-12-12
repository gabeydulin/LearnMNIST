# 
 source('LearnModel.R');
 source('load_data.R');
 source('testModel.R');

# load training data from files
data <- loadMNISTData("D:\\Ramis\\ML\\DigitDataSet\\train-images-idx3-ubyte\\train-images.idx3-ubyte", "D:\\Ramis\\ML\\DigitDataSet\\train-labels-idx1-ubyte\\train-labels.idx1-ubyte")
trainLabels <- data$labels
trainData <- data$data

print(dim(trainData))
print(dim(trainLabels))
# trainingData should be 60000x786,  60000 data and 784 features (28x28), tha matrix trainData has 60000 rows and 784 columns
# trainingLabels should have 60000x1, one class label \in {0,1,...9} for each data.

 #uncomment the following 3 lines to see the nth training example and its class label.
#  n <- 9986;
#  image( t(matrix(trainData[n, ], ncol=28, nrow=28)), Rowv=28, Colv=28, col = heat.colors(256),  margins=c(5,10))
#  print("Class label:"); 
#  print(trainLabels[n])

 #normalize the input data
 trainData<-trainData/255;
 
 #add 1 for bias
 trainData <- cbind(1, trainData);
 
 #logistic regression implementation 
 
 #thetha.Matrix -10 learned vectors theta for each label(0,1,2,3,4,5,6,7,8,9)
 theta.Matrix<- matrix(data=0,nrow = ncol(trainData), ncol = 10)
 # train a model
 #learn theta for each label
  for (i in 0:9){
   theta.Matrix[,i+1]<-learnModel(i, trainData, trainLabels)  
 }


 predictedLabels <- matrix(data=0, nrow = nrow(trainLabels), ncol=1);
 predictedLabels <- testModel(theta.Matrix,trainData); 
 #calculate accuracy on training data
 print("accuracy on training data:\t");
 print(sum(predictedLabels == trainLabels)/length(trainLabels));
 
 
 # test the model
  data <- loadMNISTData("D:\\Ramis\\ML\\DigitDataSet\\t10k-images-idx3-ubyte\\t10k-images.idx3-ubyte", "D:\\Ramis\\ML\\DigitDataSet\\t10k-labels-idx1-ubyte\\t10k-labels.idx1-ubyte")
  testLabels <- data$labels
  testData <- data$data
 
 
 #normalize the input data
   testData<-testData/255;
  
  #add 1 for bias
  testData <- cbind(1, testData);
  
  predictedLabels <- matrix(data=0, nrow = nrow(testData), ncol=1);
  predictedLabels <- testModel(theta.Matrix,testData); 

  #calculate accuracy
  print("accuracy on test data:\t")
  print(sum(predictedLabels == testLabels)/length(testLabels))
 