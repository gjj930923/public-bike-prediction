library(class)
library(rpart)
library(e1071) # for NB and SVM
library(ada) # for adaboost
library(ggplot2)

dataset = read.csv("e:/pghbike/wholedata_with_zeroes.csv", header = T)

dataset$daytype = as.factor(dataset$daytype)
dataset$stationid = as.factor(dataset$stationid)
dataset$description = as.factor(dataset$description)

demand_class = c(rep(0, dim(dataset)[1]))
for(i in 1:dim(dataset)[1]){
  if(dataset[i, c("demand")] > 0){
    demand_class[i] = 1
  }
}
istrainset = c(rep(1, dim(dataset)[1]))
for(i in 1:dim(dataset)[1]){
  if(as.integer(unlist(strsplit(as.character(dataset[i, c("time")]), "/"))[2]) > 20){
    istrainset[i] = 0
  }
}
dataset = data.frame(dataset, demand_class = demand_class)
dataset.matrix = model.matrix(demand_class~hour+month+daytype+stationid+temperature+wind+rel_humidity+pressure+description ,dataset)

dataset.matrix.training = dataset.matrix[which(istrainset == 1, arr.ind = T), ]
dataset.matrix.testing = dataset.matrix[which(istrainset == 0, arr.ind = T), ]
dataset.matrix.scale = scale(dataset.matrix)
dataset.matrix.scale.training = dataset.matrix.scale[which(istrainset == 1, arr.ind = T), c(-1)]
dataset.matrix.scale.testing = dataset.matrix.scale[which(istrainset == 0, arr.ind = T), c(-1)]
label.training = demand_class[which(istrainset == 1, arr.ind = T)]
label.testing = demand_class[which(istrainset == 0, arr.ind = T)]

label.training.posindex = which(label.training == 1, arr.ind = T)
label.training.negindex = which(label.training == 0, arr.ind = T)
label.training.negindex.sample = sample(label.training.negindex, length(label.training.posindex), replace = F)
dataset.matrix.scale.training.subset = dataset.matrix.scale.training[c(label.training.posindex, label.training.negindex.sample),]
label.training.subset = label.training[c(label.training.posindex, label.training.negindex.sample)]

#dataset.training = dataset[which(istrainset == 1, arr.ind = T), c(-6,-8)]
#dataset.testing = dataset[which(istrainset == 0, arr.ind = T), c(-6,-8)]
########## Binary Classification
#knn
#sampleindex = runif(10000, 1, length(label.training.subset))
prediction = knn(dataset.matrix.scale.training.subset, dataset.matrix.scale.testing, cl=label.training.subset, 100)
prediction_numeric = as.numeric(prediction)
demand_class_numberic = as.numeric(as.factor(label.testing))
accuracylist.knn = prediction_numeric - demand_class_numberic
length(accuracylist.knn[accuracylist.knn == 0]) / length(prediction)
confusion_matrix = table(prediction_numeric, demand_class_numberic)
#write.csv(prediction, "e:/pghbike/binary_test_result.csv")
