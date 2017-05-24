## Draft 0.1.0

install.packages("e1071");
library("e1071");

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


# Creates a RBF prediction model based on a sample dataset with default values to cost function, gamma and epsylon
system.time(predictionModel <- e1071::svm(label ~ ., data = dataSet01, type = "C-classification"));
# saves prediction model in a file to assurance
##e1071::write.svm(predictionModel, svm.file = "predictionModelBasedOnDataset01.svm", scale.file = "predictionModelBasedOnDataset01.scale");
save(predictionModel, file = "~/Projetos/FlowSVM/predictionModelBasedOnDataset01.Rda");
#teste <- load("~/Projetos/FlowSVM/predictionModelBasedOnDataset01.Rda");

datasetList <- list(dataSet02, dataSet03, dataSet04, dataSet05, dataSet06, dataSet07, dataSet08, dataSet09, dataSet10);
resultList <- list();

for(i in 1:9) {
  print(paste("Tempo de execução do dataset ", i+1));
  print(system.time(datasetList[[i]]$predictedAs <- predict(predictionModel, datasetList[[i]][,-9])));
}

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

