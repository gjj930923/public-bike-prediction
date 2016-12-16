library(ggplot2)
library(class)
library(rpart)
library(e1071) # for NB and SVM
library(ada) # for adaboost
dataset = read.csv("E:/pghbike/wholedata.csv")
dataset_with_zeroes = read.csv("E:/pghbike/wholedata_with_zeroes.csv")
index_of_testset_from_dataset_with_zeroes = read.csv("E:/pghbike/binary_test_result.csv")[,1]
index_of_testset_from_dataset_with_zeroes = which(index_of_testset_from_dataset_with_zeroes == 1, arr.ind = T)


dataset.trainset.matrix = matrix()
dataset.trainset.matrix.scale = matrix()
dataset.testset.matrix = matrix()
dataset.testset.matrix.scale = matrix()
nacolumns = c(-1)

#stationid.demand.mean = c(rep(0, 51))
#for(i in 1:51){
#  stationid.demand.mean[i] = mean(dataset[which(dataset$stationid == (i + 999), arr.ind = T), c("demand")])
#}
#stationid.order.index = order(stationid.demand.mean, decreasing = T)
#stationid.demand.mean = stationid.demand.mean[stationid.order.index]
#df = data.frame(index = stationid.order.index + 999, demand = stationid.demand.mean)
#ggplot(df, aes(x = index, y = demand)) + geom_bar(stat = "identity")
#Analytics result:
#Daytype: The frequencies of holidays are greater than weekdays and weekends.
#Description: 1,2,3,10 are good weather. 4 is fair (a little bad). 7,26,94 are bad.
#Stations: 1045 1000 1001 1012 1013 1017 1010 are obviously more frequent than others (>3.8 & <3 per 4 hrs, >2 per 1 hr)
#Stations: 20 stations are in 2-3 and 24 in 1-2 per 4 hrs.
#Stations: 20 stations are in 1.5-1.9 and 24 in 1-1.5 per 1 hr.
#Hourlevels: 13 and 17 are highest (around 3), 9 and 21 are medium (~2), 1 and 5 are lowest (1.6 & 1.3)

getIthDate <- function(datestr){
  diff = as.integer(as.Date(datestr, '%m/%d/%y') - as.Date('6/30/2015', '%m/%d/%y'))
  diff
}

# split dataset into training and testing sets according to the day number of date(<=20: training set and >20: testing set)
#setDataset <- function(){
#  dataset.time = dataset[,1]
#  is.trainset = c(rep(1, length(dataset.time)))
#  for(i in 1:length(dataset.time)){
#    if(as.integer(unlist(strsplit(as.character(dataset.time[i]), "/"))[2]) > 20){
#      is.trainset[i] = 0
#    }
#  }
#  is.trainset
#}
setDataset <- function(){
  is.trainset = c(rep(1, dim(dataset_with_zeroes)[1]))
  for(i in 1:dim(dataset_with_zeroes)[1]){
    if(as.integer(unlist(strsplit(as.character(dataset_with_zeroes[i,c("time")]), "/"))[2]) > 20){
      is.trainset[i] = 0
    }
  }
  dataset_with_zeroes.testset <<- dataset_with_zeroes[which(is.trainset == 0, arr.ind = T),]
  dataset_with_zeroes.testset <<- data.frame(dataset_with_zeroes.testset[index_of_testset_from_dataset_with_zeroes,], is.trainset = 0)
  
  is.trainset = c(rep(1, dim(dataset)[1]))
  for(i in 1:dim(dataset)[1]){
    if(as.integer(unlist(strsplit(as.character(dataset[i,c("time")]), "/"))[2]) > 20){
      is.trainset[i] = 0
    }
  }
  dataset.trainset <<- dataset[which(is.trainset == 1, arr.ind = T),]
  dataset.trainset <<- data.frame(dataset.trainset, is.trainset = 1)
}

combineDataset <- function(){
  dataset <<- rbind(dataset.trainset, dataset_with_zeroes.testset)
  dataset$daytype = as.factor(dataset$daytype)
  #dataset$stationid = as.factor(dataset$stationid)
  dataset$description = as.factor(dataset$description)
}

# transfer (weather) descriptions into weathers
transDescriptions <- function(){
  weather = c(rep("good", dim(dataset)[1]))
  description.list = dataset[, c("description")]
  weather[which(description.list == 4 | description.list == 7 | description.list == 26 | description.list == 94)] = "bad"
  weather = as.factor(weather)
  weather
}


# transfer daytype into holiday
transDaytype <- function(){
  holiday = c(rep(0, dim(dataset)[1]))
  daytype.list = dataset[, c("daytype")]
  holiday[which(daytype.list == 3, arr.ind = T)] = 1
  holiday
}

# transfer hour into hourlevel
transHour <- function(){
  hourlevel = c(rep(0, dim(dataset)[1]))
  hour.list = dataset[, c("hour")]
  hourlevel[which(hour.list >= 11 & hour.list <= 18, arr.ind = T)] = 3
  hourlevel[which(hour.list >= 8 & hour.list <= 10 | hour.list >= 19 & hour.list <= 21, arr.ind = T)] = 2
  hourlevel[which(hour.list <= 7 | hour.list >= 22, arr.ind = T)] = 1
  hourlevel
}

# transfer demands into levels
transDemands <- function(){
  levels = c(rep("A", dim(dataset)[1]))
  demand.list = dataset[, c("demand")]
  levels[which(demand.list == 2, arr.ind = T)] = "B"
  levels[which(demand.list == 3, arr.ind = T)] = "C"
  levels[which(demand.list == 4, arr.ind = T)] = "D"
  levels[which(demand.list > 4, arr.ind = T)] = "E"
  levels
}



# transfer stationid into stationtype
transStationid <- function(){
  stationtype = c(rep("C", dim(dataset)[1]))
  stationid = dataset$stationid
  idToType <<- c(rep("C", 51))
  for(i in 1000:1050){
    if(mean(dataset[which(dataset$stationid == i, arr.ind = T), c("demand")]) > 2){
      idToType[i - 999] <<- "A"
    }
    else if(mean(dataset[which(dataset$stationid == i, arr.ind = T), c("demand")]) <= 2.0 & mean(dataset[which(dataset$stationid == i, arr.ind = T), c("demand")]) > 1.5){
      idToType[i - 999] <<- "B"
    }
  }
  stationtype = idToType[stationid - 999]
  stationtype = as.factor(stationtype)
  stationtype
}

divideDataset <- function(isPCA = F, isMDS = F){
  weather = transDescriptions()
  holiday = transDaytype()
  stationtype = transStationid()
  levels = transDemands()
  #is.trainset = setDataset()
  hourlevel = transHour()
  dataset = na.omit(data.frame(dataset, hourlevel, weather, holiday, stationtype, levels))
  dataset.trainlabel <<- dataset[dataset$is.trainset == 1, c("levels")]
  dataset.testlabel <<- dataset[dataset$is.trainset == 0, c("levels")]
  dataset.matrix = model.matrix(levels~
                                  #hourlevel
                                  #+stationid
                                  #+daytype
                                  #+temperature
                                  +wind
                                +rel_humidity
                                #+description
                                #Below are transfered attributes from ones above
                                +weather
                                +holiday
                                +stationtype
                                +hourlevel
                                , dataset)
  dataset.matrix.scale = scale(dataset.matrix)
  dataset.trainset.matrix <<- dataset.matrix[dataset$is.trainset == 1,]
  dataset.testset.matrix <<- dataset.matrix[dataset$is.trainset == 0,]
  dataset.trainset.matrix.scale <<- dataset.matrix.scale[dataset$is.trainset == 1,]
  dataset.testset.matrix.scale <<- dataset.matrix.scale[dataset$is.trainset == 0,]
  
  
  # PCA plot
  if(isPCA){
    data.matrix.pca = prcomp(scaledMatrix[,nacolumns], scale=F) 
    data.matrix.pc = predict(data.matrix.pca)
    plot(data.matrix.pca, main='screeplot for PCA')
    plot(data.matrix.pc[,1:2], type="n")
    text(x=data.matrix.pc[,1], y=data.matrix.pc[,2], labels=levels)
  }
  # MDS points
  if(isMDS){
    data.matrix.dist = dist(scaledMatrix[,nacolumns])
    data.matrix.mds = cmdscale(data.matrix.dist)
    labels = levels
    #labels[which(levels != 'E', arr.ind = T)] = " "
    #labels[which(levels == 'E', arr.ind = T)] = "."
    plot(data.matrix.mds, type = 'n')
    text(data.matrix.mds,labels=labels)
  }
}




################## Method I: prediction with cross validation
predictionWithCV <- function(isKnn = F, isSVM = F, isDtree = F, isNaiveBayes = F, cv.k = 10, knn.k = 30){
  # k-fold cross-validation
  timestart = Sys.time()
  rownum = dim(dataset.trainset.matrix)[1]
  cv.accuracy.list = c(rep(0, 4))
  accuracy.list.old <<- accuracy.list
  accuracy.list <<- c(rep(0, 4))
  for(i in 1:cv.k){
    testindex = cv.k * c(0:(floor(rownum / cv.k)-1)) + i
    #knn
    if(isKnn){
      prediction = knn(dataset.trainset.matrix.scale[-testindex,nacolumns], dataset.trainset.matrix.scale[testindex,nacolumns], cl=dataset.trainlabel[-testindex], k = knn.k)
      #evaluation
      accuracy = 0
      for(j in 1:length(prediction)){
        if(as.integer(dataset.trainlabel[testindex[j]]) == as.integer(prediction[j])){
          accuracy = accuracy + 1
        }
      }
      accuracy = accuracy / length(prediction)
      if(accuracy > cv.accuracy.list[1]){
        model.knn = knn(dataset.trainset.matrix.scale[-testindex,nacolumns], dataset.testset.matrix.scale[,nacolumns], cl=dataset.trainlabel[-testindex], k = knn.k)
        cv.accuracy.list[1] = accuracy
      }
    }
    
    #svm
    if(isSVM){
      svmdata = data.frame(dataset.trainset.matrix[-testindex,nacolumns], Class = dataset.trainlabel[-testindex])
      tuned <- tune.svm(Class~., data = svmdata,kernel="linear",cost = 10^(-1:0))
      #gamma = 10^(-2:0), cost = 10^(-1:0))
      #gamma = tuned[['best.parameters']]$gamma
      cost = tuned[['best.parameters']]$cost
      model = svm(Class~., data = svmdata
                  ,kernel="linear"
                  #, gamma=gamma
                  , cost=cost
      )
      probclass = predict(model, newdata=dataset.trainset.matrix[testindex,nacolumns], type = "class", probability = F)
      prediction = c(probclass)
      #evaluation
      accuracy = 0
      for(j in 1:length(prediction)){
        if(as.integer(dataset.trainlabel[testindex[j]]) == as.integer(prediction[j])){
          accuracy = accuracy + 1
        }
      }
      accuracy = accuracy / length(prediction)
      if(cv.accuracy.list[2] < accuracy){
        model.svm = model
        cv.accuracy.list[2] = accuracy
      }
    }
    
    #dtree
    if(isDtree){
      dtreedata = data.frame(dataset.trainset.matrix[-testindex,nacolumns], Class = dataset.trainlabel[-testindex])
      model = rpart(Class~., data = dtreedata)
      pfit<- prune(model, cp=model$cptable[which.min(model$cptable[,"xerror"]),"CP"])
      prediction = predict(pfit, newdata=data.frame(dataset.trainset.matrix[testindex,nacolumns]), type = "vector")
      #evaluation
      accuracy = 0
      for(j in 1:length(prediction)){
        if(as.integer(dataset.trainlabel[testindex[j]]) == as.integer(prediction[j])){
          accuracy = accuracy + 1
        }
      }
      accuracy = accuracy / length(prediction)
      if(cv.accuracy.list[3] < accuracy){
        model.dtree = model
        cv.accuracy.list[3] = accuracy
      }
    }
    
    #naive bayes
    if(isNaiveBayes){
      model = naiveBayes(Class~., data = dtreedata)
      prediction = predict(model, newdata=data.frame(dataset.trainset.matrix[testindex,nacolumns]), type = "class")
      #evaluation
      accuracy = 0
      for(j in 1:length(prediction)){
        if(as.integer(dataset.trainlabel[testindex[j]]) == as.integer(prediction[j])){
          accuracy = accuracy + 1
        }
      }
      accuracy = accuracy / length(prediction)
      if(cv.accuracy.list[4] < accuracy){
        model.nb = model
        cv.accuracy.list[4] = accuracy
      }
    }
  }
  
  ## prediction
  #knn
  if(isKnn){
    prediction = model.knn
    #evaluation
    accuracy = 0
    mae = 0
    for(i in 1:length(prediction)){
      if(as.integer(dataset.testlabel[i]) == as.integer(prediction[i])){
        accuracy = accuracy + 1
      }
      else{
        mae = mae + abs(as.integer(dataset.testlabel[i]) - as.integer(prediction[i]))
      }
    }
    accuracy.list[1] <<- accuracy / length(prediction)
    mae.list[1] <<- mae / length(prediction)
  }
  
  #svm
  if(isSVM){
    probclass = predict(model.svm, newdata=dataset.testset.matrix[c(1:100),nacolumns], type = "class", probability = F)
    prediction = c(probclass)
    #evaluation
    accuracy = 0
    mae = 0
    for(i in 1:length(prediction)){
      if(as.integer(dataset.testlabel[i]) == as.integer(prediction[i])){
        accuracy = accuracy + 1
      }
      else{
        mae = mae + abs(as.integer(dataset.testlabel[i]) - as.integer(prediction[i]))
      }
    }
    accuracy.list[2] <<- accuracy / length(prediction)
    mae.list[2] <<- mae / length(prediction)
  }
  
  #dtree
  if(isDtree){
    prediction = predict(model.dtree, newdata=data.frame(dataset.testset.matrix[,nacolumns]), type = "vector")
    #evaluation
    accuracy = 0
    mae = 0
    for(i in 1:length(prediction)){
      if(as.integer(dataset.testlabel[i]) == as.integer(prediction[i])){
        accuracy = accuracy + 1
      }
      else{
        mae = mae + abs(as.integer(dataset.testlabel[i]) - as.integer(prediction[i]))
      }
    }
    accuracy.list[3] <<- accuracy / length(prediction)
    mae.list[3] <<- mae / length(prediction)
  }
  
  #naive bayes
  if(isNaiveBayes){
    prediction = predict(model.nb, newdata=data.frame(dataset.testset.matrix[,nacolumns]), type = "class")
    #evaluation
    accuracy = 0
    mae = 0
    for(i in 1:length(prediction)){
      if(as.integer(dataset.testlabel[i]) == as.integer(prediction[i])){
        accuracy = accuracy + 1
      }
      else{
        mae = mae + abs(as.integer(dataset.testlabel[i]) - as.integer(prediction[i]))
      }
    }
    accuracy.list[4] <<- accuracy / length(prediction)
    mae.list[4] <<- mae / length(prediction)
  }
  timeend = Sys.time()
  runningtime = timeend - timestart
  print(runningtime)
}


################## Method II: prediction without cross validation
prediction <- function(isKnn = F, isSVM = F, isDtree = F, isNaiveBayes = F, knn.k = 30){
  accuracy.list.old <<- accuracy.list
  accuracy.list <<- c(rep(0, 4))
  mae.list.old <<- mae.list
  mae.list <<- c(rep(0, 4))
  timestart = Sys.time()
  #knn
  if(isKnn){
    prediction = knn(dataset.trainset.matrix.scale[,nacolumns], dataset.testset.matrix.scale[,nacolumns], cl=dataset.trainlabel, k = knn.k)
    #evaluation
    accuracy = 0
    mae = 0
    for(i in 1:length(prediction)){
      if(as.integer(dataset.testlabel[i]) == as.integer(prediction[i])){
        accuracy = accuracy + 1
      }
      else{
        mae = mae + abs(as.integer(dataset.testlabel[i]) - as.integer(prediction[i]))
      }
    }
    accuracy.list[1] <<- accuracy / length(prediction)
    mae.list[1] <<- mae / length(prediction)
  }
  
  #svm
  if(isSVM){
    svmdata = data.frame(dataset.trainset.matrix[,nacolumns], Class = dataset.trainlabel)
    #model = svm(Class~TmV, data = data)
    tuned <- tune.svm(Class~., data = svmdata,kernel="sigmoid", degree = c(3,9), coef0 = 0, gamma = 10^(-2:0),cost = 10^(-1:0))
    #gamma = 10^(-2:0), cost = 10^(-1:0))
    #gamma = 0.05*(2:18), cost = 0.05*(4:14))
    gamma = tuned[['best.parameters']]$gamma
    cost = tuned[['best.parameters']]$cost
    #degree = tuned[['best.parameters']]$degree
    coef0 = tuned[['best.parameters']]$coef0
    model = svm(Class~., data = svmdata
                ,kernel="radial"
                , gamma=gamma
                , cost=cost
                #, degree = degree
                , coef0 = coef0
    )
    probclass = predict(model, newdata=dataset.testset.matrix[,nacolumns], type = "class", probability = F)
    prediction = c(probclass)
    #evaluation
    accuracy = 0
    mae = 0
    for(i in 1:length(prediction)){
      if(as.integer(dataset.testlabel[i]) == as.integer(prediction[i])){
        accuracy = accuracy + 1
      }
      else{
        mae = mae + abs(as.integer(dataset.testlabel[i]) - as.integer(prediction[i]))
      }
    }
    accuracy.list[2] <<- accuracy / length(prediction)
    mae.list[2] <<- mae / length(prediction)
  }
  
  #dtree
  if(isDtree){
    dtreedata = data.frame(dataset.trainset.matrix[,nacolumns], Class = dataset.trainlabel)
    model = rpart(Class~., data = dtreedata)
    pfit<- prune(model, cp=model$cptable[which.min(model$cptable[,"xerror"]),"CP"])
    prediction = predict(pfit, newdata=data.frame(dataset.testset.matrix[,nacolumns]), type = "vector")
    #evaluation
    accuracy = 0
    mae = 0
    for(i in 1:length(prediction)){
      if(as.integer(dataset.testlabel[i]) == as.integer(prediction[i])){
        accuracy = accuracy + 1
      }
      else{
        mae = mae + abs(as.integer(dataset.testlabel[i]) - as.integer(prediction[i]))
      }
    }
    accuracy.list[3] <<- accuracy / length(prediction)
    mae.list[3] <<- mae / length(prediction)
  }
  
  #naive bayes
  if(isNaiveBayes){
    model = naiveBayes(Class~., data = dtreedata)
    prediction = predict(model, newdata=data.frame(dataset.testset.matrix[,nacolumns]), type = "class")
    #evaluation
    accuracy = 0
    mae = 0
    for(i in 1:length(prediction)){
      if(as.integer(dataset.testlabel[i]) == as.integer(prediction[i])){
        accuracy = accuracy + 1
      }
      else{
        mae = mae + abs(as.integer(dataset.testlabel[i]) - as.integer(prediction[i]))
      }
    }
    accuracy.list[4] <<- accuracy / length(prediction)
    mae.list[4] <<- mae / length(prediction)
  }
  timeend = Sys.time()
  runningtime = timeend - timestart
  print(runningtime)
  
  #ada
  #adadata = data.frame(dataset.trainset.matrix[,c(-1)], Class = dataset.trainlabel)
  #model = ada(Class~., data = adadata)
  #prediction = predict(model, newdata=data.frame(dataset.testset.matrix[,c(-1,-54,-55,-56)]))
}

accuracy.list = c(rep(0, 4))
accuracy.list.old = c(rep(0, 4))
mae.list = c(rep(0, 4))
mae.list.old = c(rep(0, 4))
setDataset()
combineDataset()
divideDataset()
#predictionWithCV(knn.k = 220)#Method I
prediction(isKnn = T, knn.k = 220)#Method II
prediction(isKnn = T, isDtree = T, isNaiveBayes = T, knn.k = 220)#Method II
#find the best k for knn
#klist.accuracy.100 = c(rep(0, 100))
#for(i in 1:100){
#  prediction(isKnn = T, knn.k = i)
#  klist.accuracy[i] = accuracy.list[1]
#}
#klist.accuracy.250 = c(rep(0, 100))
#for(i in 151:250){
#  prediction(isKnn = T, knn.k = i)
#  klist.accuracy[i-150] = accuracy.list[1]
#}

#find the best para for SVM
prediction(isSVM = T)
