## Draft 0.1.0

install.packages("e1071");
library("e1071");

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

# Creates five samples with size equals 10% of original data each to train and one to test
# this is not the best method because some data can go in more than one resulting dataset but is ok to test
first_sample <- unorderedData[sample(length(unorderedData),round(0.1 *nrow(unorderedData)),replace = TRUE),];
second_sample <- unorderedData[sample(length(unorderedData),round(0.1 *nrow(unorderedData)),replace = TRUE),];
third_sample <- unorderedData[sample(length(unorderedData),round(0.1 *nrow(unorderedData)),replace = TRUE),];
fourth_sample <- unorderedData[sample(length(unorderedData),round(0.1 *nrow(unorderedData)),replace = TRUE),];
fifth_sample <- unorderedData[sample(length(unorderedData),round(0.1 *nrow(unorderedData)),replace = TRUE),];
testSet <- unorderedData[sample(length(unorderedData),round(0.1 *nrow(unorderedData)),replace = TRUE),];

# Creates a RBF prediction model based on a sample dataset with default values to cost function, gamma and epsylon
predictionModel <- svm(label ~ ., data = first_sample, type = "C-classification");
primaryTest <- predict(predictionModel, testSet[,-9]); # uses all testSet dataset minus the `label` column

# Building a new dataset with testSet dataset and add the prediction info as a column
firstComparedSet <- testSet;
firstComparedSet$predictedAS <- primaryTest;
# now just compare label column with predict column and count how much is diferent
# if we get 0, it is same as 100% successful prediction, other value is the number of errors
sum(ifelse(firstComparedSet$label==firstComparedSet$predictedAS,0,1));

#######################################################################################################
############################## JUST SAVE DATA TO FILES SECTION #####################################

save(sanitizedPositives, file=paste("sanitizedPositives.RData"));
write.csv(sanitizedPositives, file = "sanitizedPositives.csv");

save(sanitizedNegatives, file=paste("sanitizedNegatives.RData"));
write.csv(sanitizedNegatives, file = "sanitizedNegatives.csv");

save(unorderedData, file=paste("unorderedData.RData"));
write.csv(unorderedData, file = "unorderedData.csv");

save(testSet, file=paste("randomized_sample_testSet.RData"));
write.csv(testSet, file = "randomized_sample_testSet.csv");

save(first_sample, file=paste("randomized_first_sample.RData"));
write.csv(first_sample, file = "randomized_first_sample.csv");

save(second_sample, file=paste("randomized_second_sample.RData"));
write.csv(second_sample, file = "randomized_second_sample.csv");

save(third_sample, file=paste("randomized_third_sample.RData"));
write.csv(third_sample, file = "randomized_third_sample.csv");

save(fourth_sample, file=paste("randomized_fourth_sample.RData"));
write.csv(fourth_sample, file = "randomized_fourth_sample.csv");

save(fifth_sample, file=paste("randomized_fifth_sample.RData"));
write.csv(fifth_sample, file = "randomized_fifth_sample.csv");

#######################################################################################################
