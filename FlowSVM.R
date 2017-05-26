## Draft 0.2.0

# install and loads required dependencies
install.packages(c("e1071", "doParallel", "foreach"));
library("e1071");
library("doParallel");

###########################################################################################################
###########################  LOADS, CLEAN UP AND PREPARE ORIGINAL DATA   ##################################

# Let's load both classes of data AND CLEAN UNDESIRED INFO
sanitizedPositives <- read.csv("../data/positive.csv");
sanitizedNegatives <- read.csv("../data/negative.csv");
sanitizedPositives$Time <- NULL;
sanitizedNegatives$Time <- NULL;
sanitizedPositives$label <- NULL;
sanitizedNegatives$label <- NULL;
sanitizedPositives$X <- NULL;
sanitizedNegatives$X <- NULL;

# get original data transforming in log10, so we get a reasonable range to work
sanitizedPositives <- log10(sanitizedPositives);
sanitizedNegatives <- log10(sanitizedNegatives);
sanitizedPositives$label = 1; # working with numeric classes appears to be the `R`ight way
sanitizedNegatives$label = -1; # working with numeric classes appears to be the `R`ight way

# Building the final dataset and using randomized order to improve the qualit of train datasets and resultant tests
orderedData <- rbind.data.frame(sanitizedNegatives, sanitizedPositives); # get all data united
unorderedData <- orderedData[sample(1:nrow(orderedData)), ]; # randomizing the order of data to avoid bad behaviours

n <- 575900; # number of rows of each resulting dataset
dataSetsList <- split(unorderedData, rep(1:ceiling(nrow(unorderedData)/n), each=n, length.out=nrow(unorderedData)));
#####################################################################################################################
############################### at this point we have smallest dataset samples ######################################


# Creates a RBF prediction model based on a sample dataset with default values to cost function, gamma and epsylon
system.time(predictionModel <- e1071::svm(label ~ ., data = dataSet01, type = "C-classification"));
# Creates a RBF prediction model based on a sample dataset with default values to cost function, gamma and epsylon
system.time(predictionModel3Channel <- e1071::svm(label ~ ., data = dataSet01[, c("FITC.H", "SSC.H", "PerCP.Cy5.5.H", "label")], type = "C-classification"));
save(predictionModel3Channel, file = "~/Projetos/FlowSVM/data/predictionModel3ChannelBasedOnDataset01.Rda");
# saves prediction model in a file to later analisys
write.svm(predictionModel,  svm.file = "~/Projetos/FlowSVM/data/predictionModelBasedOnDataset01.svm");
save(predictionModel, file = "~/Projetos/FlowSVM/data/predictionModelBasedOnDataset01.Rda");
#teste <- load("~/Projetos/FlowSVM/predictionModelBasedOnDataset01.Rda");

# mount a dataset list to iteractivity for save time
resultList <- datasetList <- list(dataSet02, dataSet03, dataSet04, dataSet05, dataSet06, dataSet07, dataSet08, dataSet09, dataSet10);
resultList3Channel <- list(dataSet02[, c("FITC.H", "SSC.H", "PerCP.Cy5.5.H", "label")], dataSet03[, c("FITC.H", "SSC.H", "PerCP.Cy5.5.H", "label")], dataSet04[, c("FITC.H", "SSC.H", "PerCP.Cy5.5.H", "label")], dataSet05[, c("FITC.H", "SSC.H", "PerCP.Cy5.5.H", "label")], dataSet06[, c("FITC.H", "SSC.H", "PerCP.Cy5.5.H", "label")], dataSet07[, c("FITC.H", "SSC.H", "PerCP.Cy5.5.H", "label")], dataSet08[, c("FITC.H", "SSC.H", "PerCP.Cy5.5.H", "label")], dataSet09[, c("FITC.H", "SSC.H", "PerCP.Cy5.5.H", "label")], dataSet10[, c("FITC.H", "SSC.H", "PerCP.Cy5.5.H", "label")])
# testing predition with all chanels
for(i in 1:9) {
  print(paste("Tempo de execução do dataset ", i+1));
  print(system.time(resultList[[i]]$predictedAs <- predict(predictionModel, datasetList[[i]][,-9])));
}

#testing predition using only 3 chanel in test datasets 
for(i in 1:9) {
  print(paste("Tempo de execução do dataset", i+1));
  print(system.time(resultList3Channel[[i]]$predictedAs <- predict(predictionModel3Channel, resultList3Channel[[i]][,-4])));
  dft <- resultList3Channel[[i]];
  save(dft, file=paste("~/Projetos/FlowSVM/data/", i,"result3channel01.RData"));
  print(paste("Total errors of dataset", i+1));
  print(sum(ifelse(resultList3Channel[[i]]$label == resultList3Channel[[i]]$predictedAs,0,1)));
}

for(i in 1:9) {
  #dft <- resultList[[i]];
  #save(dft, file=paste("~/Projetos/FlowSVM/data/", i,"result01.RData"));
  print(paste("Total errors of dataset", i+1));
  print(sum(ifelse(resultList[[i]]$label == resultList[[i]]$predictedAs,0,1)));
}
View(resultList[[1]])


sum(ifelse(resultList[[1]]$label == resultList[[1]]$predictedAs,0,1));

# bigger new train dataset using 90% of original data
trainDataSet <- rbind.data.frame(datasetList[[1]], datasetList[[2]], datasetList[[3]], datasetList[[4]], datasetList[[5]], datasetList[[6]], datasetList[[7]], datasetList[[8]], datasetList[[9]]);
trainDataSet$predictedAs <- NULL;
system.time(biggerPredictionModel <- e1071::svm(label ~ ., data = trainDataSet, type = "C-classification"));

# it was takein more than 130 hours to execute with no hope in make something useful... we need improve it
print(system.time(dataset10predictedAs <- predict(biggerPredictionModel, datasetList[[10]][,-9])));

# testing prediction with other datasets against the generated model
result <- predict(predictionModel, dataSetsList[[2]][,-9]); # uses all testSet dataset minus the `label` column
# Building a new dataset with testSet dataset and add the prediction info as a column
firstComparedSet <- dataSetsList[[2]];
firstComparedSet$predictedAS <- result;
# now just compare label column with predict column and count how much is diferent
# if we get 0, it is same as 100% successful prediction, other value is the number of errors
sum(ifelse(firstComparedSet$label==firstComparedSet$predictedAS,0,1));

# testing prediction with other datasets against the generated model
result2 <- predict(predictionModel, dataSetsList[[3]][,-9]); # uses all testSet dataset minus the `label` column

# Building a new dataset with testSet dataset and add the prediction info as a column
secondComparedSet <- dataSetsList[[3]];
secondComparedSet$predictedAS <- result2;
# now just compare label column with predict column and count how much is diferent
# if we get 0, it is same as 100% successful prediction, other value is the number of errors
sum(ifelse(secondComparedSet$label==secondComparedSet$predictedAS,0,1));

# for the next part, we use the tune function to find best C and gamma parameters
# so we will re-run previous tests to compare results
tune = tune.svm(predictionModel,dataSetsList[[11]],cost=1:100,gamma=seq(0,0.125,0.5,1))

