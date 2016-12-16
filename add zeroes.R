addZeroes <- function(month, year, day_max = 31){
  orig.dataset = read.csv("e:/pghbike/healthyride.csv", header = T)
  subsetindex = c(rep(0, dim(orig.dataset)[1]))
  for(i in 1:dim(orig.dataset)[1]){
    if(as.integer(unlist(strsplit(as.character(orig.dataset[i, c("time")]), "/"))[1]) == month
       & as.integer(unlist(strsplit(as.character(orig.dataset[i, c("time")]), "/"))[3]) == year){
      subsetindex[i] = 1
    }
  }
  
  orig.dataset.subset = subset(orig.dataset, stationid <= 1050)
  orig.dataset.subset = orig.dataset.subset[which(subsetindex == 1, arr.ind = T),]
  demand.matrix = array(0, dim = c(day_max, 24, 51))
  for(i in 1:dim(orig.dataset.subset)[1]){
    day = as.integer(unlist(strsplit(as.character(orig.dataset.subset[i, c("time")]), "/"))[2])
    hour = orig.dataset.subset[i, c("hour")] + 1
    stationid = orig.dataset.subset[i, c("stationid")] - 999
    demand.matrix[day, hour, stationid] = demand.matrix[day, hour, stationid] + 1
  }
  output.time = c()
  output.hour = c()
  output.stationid = c()
  output.demand = c()
  output.month = month
  output.daytype = c()
  
  for(i in 1:dim(demand.matrix)[1]){
    for(j in 1:dim(demand.matrix)[2]){
      for(k in 1:dim(demand.matrix)[3]){
        timestr = paste(month, "/", i, "/", year, sep = "")
        output.time = c(output.time, timestr)
        output.hour = c(output.hour, j - 1)
        output.stationid = c(output.stationid, k + 999)
        output.demand = c(output.demand, demand.matrix[i,j,k])
        output.daytype = c(output.daytype, orig.dataset.subset[orig.dataset.subset$time == timestr, c("daytype")][1])
      }
    }
  }
  output.data = data.frame(time = output.time, hour = output.hour, month = output.month, daytype = output.daytype, stationid = output.stationid, demand = output.demand)
  write.csv(output.data, paste("e:/pghbike/healthyride_with_zeroes_",year, "-", month, ".csv", sep = ""))
}

addZeroes(8, 2015)
addZeroes(9, 2015, 30)
addZeroes(10, 2015)
addZeroes(11, 2015, 30)
addZeroes(12, 2015)
addZeroes(1, 2016)
addZeroes(2, 2016, 29)
addZeroes(3, 2016)
addZeroes(4, 2016, 30)
addZeroes(5, 2016)
addZeroes(6, 2016, 30)
addZeroes(7, 2016)
addZeroes(8, 2016)
addZeroes(9, 2016, 30)