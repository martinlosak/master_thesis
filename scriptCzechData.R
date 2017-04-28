createSummary <- function(path = "processed/cz/load_01_2017") {
  summary <- data.frame()
  files = list.files(path)
  
  for (f in files){
    print(f)
    data <- read.csv(sprintf("%s/%s", path, f))
    
    active_avg = round(mean(data$active, na.rm = TRUE), digits = 2)
    reactive_avg = round(mean(data$reactive, na.rm = TRUE), digits = 2)
    active_max = round(as.numeric(max(data$active, na.rm = TRUE)), digits = 2)
    reactive_max = round(as.numeric(max(data$reactive, na.rm = TRUE)), digits = 2)
    active_min = round(as.numeric(min(data$active, na.rm = TRUE)), digits = 2)
    reactive_min = round(as.numeric(min(data$reactive, na.rm = TRUE)), digits = 2)
    ratio = round(reactive_avg / active_avg, digits = 2)
    
    newRow = data.frame(Ratio = ratio, Active_avg = active_avg, Reactive_avg = reactive_avg, Active_max = active_max, Reactive_max = reactive_max, Active_min = active_min, Reactive_min = reactive_min, Meter = f)
    summary = rbind(summary, newRow)
  }

  return(summary)
}

# created: 8 clusters by average active power (january 2017)
createAvgActiveClusters <- function(path = "processed/cz/load_01_2017"){
  files = list.files(path)
  
  cluster1 = data.frame()
  cluster2 = data.frame()
  cluster3 = data.frame()
  cluster4 = data.frame()
  cluster5 = data.frame()
  cluster6 = data.frame()
  cluster7 = data.frame()
  cluster8 = data.frame()
  
  for (f in files){
    print(f)
    data = read.csv(sprintf("%s/%s", path, f))
    
    active_avg = mean(data$active, na.rm = TRUE)
    
    if (0 < active_avg & active_avg < 0.25){
      cluster1 = rbind(cluster1, data)
    }
    
    if (0.25 < active_avg & active_avg < 0.5){
      cluster2 = rbind(cluster2, data)
    }
    
    if (0.5 < active_avg & active_avg < 1.5){
      cluster3 = rbind(cluster3, data)
    }
    
    if (1.5 < active_avg & active_avg < 5){
      cluster4 = rbind(cluster4, data)
    }
    
    if (5 < active_avg & active_avg < 40){
      cluster5 = rbind(cluster5, data)
    }
    
    if (40 < active_avg & active_avg < 100){
      cluster6 = rbind(cluster6, data)
    }
    
    if (100 < active_avg & active_avg < 250){
      cluster7 = rbind(cluster7, data)
    }
    
    if (250 < active_avg){
      cluster8 = rbind(cluster8, data)
    }
  }
  
  write.csv(x = cluster1, file = "processed/cz/clusters/cluster1.csv", quote = FALSE, row.names = FALSE)
  write.csv(x = cluster2, file = "processed/cz/clusters/cluster2.csv", quote = FALSE, row.names = FALSE)
  write.csv(x = cluster3, file = "processed/cz/clusters/cluster3.csv", quote = FALSE, row.names = FALSE)
  write.csv(x = cluster4, file = "processed/cz/clusters/cluster4.csv", quote = FALSE, row.names = FALSE)
  write.csv(x = cluster5, file = "processed/cz/clusters/cluster5.csv", quote = FALSE, row.names = FALSE)
  write.csv(x = cluster6, file = "processed/cz/clusters/cluster6.csv", quote = FALSE, row.names = FALSE)
  write.csv(x = cluster7, file = "processed/cz/clusters/cluster7.csv", quote = FALSE, row.names = FALSE)
  write.csv(x = cluster8, file = "processed/cz/clusters/cluster8.csv", quote = FALSE, row.names = FALSE)
}

#created: kmeans clusters by hourly average active consumption
kmeanshourlyAvgSummary <- function(path = "processed/cz/load_01_2017"){
  path = "processed/cz/load_01_2017"
  summary = data.frame()

  fullNameFiles = list.files(path, full.names = TRUE)
  files = list.files(path)

  datasets = lapply(fullNameFiles, read.csv)
  i = 1
  for (data in datasets) {
    message("Aggregating file by hour: ", files[i])
    aggregate = aggregate(data[,c("active")], by = list(hour = data$hour), FUN = mean, na.rm = TRUE)

    newRow = as.data.frame(t(aggregate$x))
    newRow[is.na(newRow)] = 0
    newRow = (newRow - min(newRow))/(max(newRow) - min(newRow)) # scaling
    colnames(newRow) = as.character(aggregate$hour)

    newRow = cbind(newRow, file = files[i])

    summary = rbind(summary, newRow)

    i = i + 1
  }

  # clustering
  clusterCount = 10 
  message("Creating clusters: ", clusterCount)

  kmeans = kmeans(summary[, 1:24], clusterCount, nstart= 20)
  result = data.frame(summary$file, kmeans$cluster)
  colnames(result) = c("file", "cluster")

  # initialize cluster data.frames
  clusters = list()
  for(c in 1:clusterCount) clusters = c(clusters, list(data.frame()))

  for(f in result$file){
    message("Binding file to cluster: ", f)
  	data <- read.csv(sprintf("%s/%s", path, f))

  	c = result$cluster[result$file == f]
    clusters[[c]] = rbind(clusters[[c]], data)
  }

  i = 1
  for(cluster in clusters){
    message("Writing cluster: ", i)
    write.csv(cluster, file = sprintf("processed/cz/clusters/cluster%d.csv", i), quote = FALSE, row.names = FALSE)
    i = i + 1
  }
}

consumptionBy <- function(type, path){
  summary = data.frame()
  clusters = list.files(path)

  for (cluster in clusters) {
    message("Calculating cluster: ", cluster)
    data = read.csv(sprintf("%s/%s", path, cluster))

    sum = sum(data$active, na.rm = TRUE)
    active_avg = mean(data$active, na.rm = TRUE)
    size = nrow(data) / 2976

    if (type == "hour")
      newRow = consumptionByHour(data, sum, active_avg, size, cluster)

    if (type == "day")
      newRow = consumptionByTypeOfDay(data, sum, active_avg, size, cluster)

    if (type == "shift")
      newRow = consumptionByShift(data, sum, active_avg, size, cluster)

    summary = rbind(summary, newRow)
  }

  return(summary)
}

#created: percentage consumption of active power by: hour (january 2017)
consumptionByHour <- function(data, sum, Active_avg, MeterCount, Cluster){
  for (hour in 0:23) {
    subset = data[data$hour == hour,]
    assign(sprintf("h%d", hour), round(sum(subset$active, na.rm = TRUE) / sum * 100, digits = 2))
  }

  return(data.frame(h0, h1, h2, h3, h4, h5, h6, h7, h8, h9, h10, h11, h12, h13, h14, h15, h16, h17, h18, h19, h20, h21, h22, h23, Active_avg, MeterCount, Cluster))
}

# created: percentage consumption of active power by: type of day (january 2017)
consumptionByTypeOfDay <- function(data, sum, Active_avg, MeterCount, Cluster){
    Monday = round(sum(subset(data, data$day == 1)$active, na.rm = TRUE) / sum * 100, digits = 2)
    Tuesday = round(sum(subset(data, data$day == 2)$active, na.rm = TRUE) / sum * 100, digits = 2)
    Wednesday = round(sum(subset(data, data$day == 3)$active, na.rm = TRUE) / sum * 100, digits = 2)
    Thursday = round(sum(subset(data, data$day == 4)$active, na.rm = TRUE) / sum * 100, digits = 2)
    Friday = round(sum(subset(data, data$day == 5)$active, na.rm = TRUE) / sum * 100, digits = 2)
    Saturday = round(sum(subset(data, data$day == 6)$active, na.rm = TRUE) / sum * 100, digits = 2)
    Sunday = round(sum(subset(data, data$day == 7)$active, na.rm = TRUE) / sum * 100, digits = 2)

    return(data.frame(Monday, Tuesday, Wednesday, Thursday, Friday, Saturday, Sunday, Active_avg, MeterCount, Cluster))
}

# created: percentage consumption of active power by: shift
consumptionByShift <- function(data, sum, Active_avg, MeterCount, Cluster){
  Morning = round(sum(subset(data, data$shift == "R")$active, na.rm = TRUE) / sum * 100, digits = 2)
  Aftenoon = round(sum(subset(data, data$shift == "O")$active, na.rm = TRUE) / sum * 100, digits = 2)
  Night = round(sum(subset(data, data$shift == "N")$active, na.rm = TRUE) / sum * 100, digits = 2)

  return(data.frame(Morning, Aftenoon, Night, Active_avg, MeterCount, Cluster))
}

reactivePlot <- function(file, path = "processed/cz/load_01_2017"){
  if(length(file)){
    data = read.csv(sprintf("%s/%s", path, file))
    active = data$active
    reactive = data$reactive

    plot = plot(active, type="l", ylim=range(c(active, reactive), na.rm=TRUE), xlab="Time", ylab="Energy [W / VAr]", col="red", main="Active vs Reactive energy")
    lines(reactive,col="green")
    legend('topright', legend = c("Active", "Reactive"), col = c("red", "green"), lty = 1, bty = 'n')
    
    return(plot)
  }
}