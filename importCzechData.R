createSummary <- function() {
  summary <- data.frame()
  
  path = "processed/cz/load"
  files <- list.files(path)
  
  for (f in files){
    print(f)
    data <- read.csv(sprintf("%s/%s", path, f))
    
    active_avg = mean(data$active, na.rm = TRUE)
    reactive_avg = mean(data$reactive, na.rm = TRUE)
    active_max = as.numeric(max(data$active, na.rm = TRUE))
    reactive_max = as.numeric(max(data$reactive, na.rm = TRUE))
    active_min = as.numeric(min(data$active, na.rm = TRUE))
    reactive_min = as.numeric(min(data$reactive, na.rm = TRUE))
    ratio = reactive_avg / active_avg
    
    newRow <- data.frame(active_avg, reactive_avg, active_max, reactive_max, active_min, reactive_min, ratio, f)
    summary <- rbind(summary, newRow)
  }
}

# processing czech energy data
# removed: smart-meters with all active and reactive energy is 0 or NA
# changed: data with three columns: datetime, active, reactive
processingCzechEnergy <- function() {
  path = "data/cz/load_edited"
  files = list.files(path)
  
  for (f in files){
    print(f)
    
    data = read.csv(sprintf("%s/%s", path, f), sep = "\t", skip = 3)
    data = setNames(data, c("date","time","active", "active_out", "reactive", "reactive_out"))
    
    isActive = !all(data$active == 0 | is.na(data$active))
    isReactive = !all(data$reactive == 0 | is.na(data$reactive))
    
    if(isActive && isReactive){
      header = read.csv(sprintf("%s/%s", path, f), sep = "\t", header = FALSE, nrows = 2)
      electrometer = as.numeric(gsub("[^0-9]", "", header[1, 1]))
      postcode = gsub("[^0-9]", "", header[2, 2])
      
      data$datetime = as.POSIXct(paste(data$date, data$time), format="%Y%m%d %H:%M:%S")
      
      energy = data[, c("datetime", "active", "reactive")]
      
      write.csv(x = energy, file = sprintf("processed/cz/load/%d_%s.csv", electrometer, postcode), quote = FALSE, row.names = FALSE)
    }
  }  
}

# created: subset of january 2017
# removed: smart-meters with all active and reactive energy is 0 or NA
# removed: smart-meters with active 10% and less filled
# added: day type, hour, shift
createSubset <- function(){
  path = "processed/cz/load"
  files = list.files(path)
  
  from = as.POSIXct("2016-12-31 23:45:00")
  to = as.POSIXct("2017-02-01 00:00:00")
  
  for (f in files){
    print(f)
    
    data <- read.csv(sprintf("%s/%s", path, f))
    
    startDate = as.POSIXct(data[1,1])
    endDate = as.POSIXct(data[nrow(data),1])
    
    if (startDate < from & to < endDate){
      print("creating subset")
      subset = subset(data, from < as.POSIXct(data$datetime) & as.POSIXct(data$datetime) < to)
      
      rows = nrow(subset)
      isActive = !all(subset$active == 0 | is.na(subset$active))
      # isReactive = !all(subset$reactive == 0 | is.na(subset$reactive))
      activeNa = sum(is.na(subset$active)) / rows * 100
      # reactiveNa = sum(is.na(subset$reactive)) / rows * 100
      if(isActive & activeNa < 10){
        
        # added: day type, hour, shift
        library(lubridate)
        subset$day = ifelse(wday(subset$datetime)==1,7,wday(subset$datetime)-1)
        subset$hour = as.numeric(format(as.POSIXct(subset$datetime, format="%Y-%m-%d %H:%M:%S"), format = "%H"))
        subset$shift[subset$hour < 6] = "N"
        subset$shift[subset$hour > 21] = "N"
        subset$shift[subset$hour < 14 & subset$hour > 5] = "R"
        subset$shift[subset$hour < 22 & subset$hour > 13] = "O"
        
        write.csv(x = subset, file = sprintf("processed/cz/load_01_2017/%s", f), quote = FALSE, row.names = FALSE)
      }
    }
  }
  
}

# created: 8 clusters by average active power (january 2017)
createAvgActiveClusters <- function(){
  path = "processed/cz/load_01_2017"
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

# created: percentage consumption of active power by: type of day (january 2017)
consumptionByTypeOfDay <- function(){
  summary <- data.frame()
  
  path = "processed/cz/clusters"
  files = list.files(path)
  
  for(f in files){
    print(f)
    data = read.csv(sprintf("%s/%s", path, f))
    
    sum = sum(data$active, na.rm = TRUE)
    active_avg = mean(data$active, na.rm = TRUE)
    size = nrow(data) / 2976
    
    mon = subset(data, data$day == 1)
    tue = subset(data, data$day == 2)
    wed = subset(data, data$day == 3)
    thu = subset(data, data$day == 4)
    fri = subset(data, data$day == 5)
    sat = subset(data, data$day == 6)
    sun = subset(data, data$day == 7)
       
    Monday = round(sum(mon$active, na.rm = TRUE) / sum * 100, digits = 2)
    Tuesday = round(sum(tue$active, na.rm = TRUE) / sum * 100, digits = 2)
    Wednesday = round(sum(wed$active, na.rm = TRUE) / sum * 100, digits = 2)
    Thursday = round(sum(thu$active, na.rm = TRUE) / sum * 100, digits = 2)
    Friday = round(sum(fri$active, na.rm = TRUE) / sum * 100, digits = 2)
    Saturday = round(sum(sat$active, na.rm = TRUE) / sum * 100, digits = 2)
    Sunday = round(sum(sun$active, na.rm = TRUE) / sum * 100, digits = 2)
    
    newRow <- data.frame(Monday, Tuesday, Wednesday, Thursday, Friday, Saturday, Sunday, active_avg, size, f)
    summary <- rbind(summary, newRow)
  }
}

# created: percentage consumption of active power by: shift
consumptionByShift <- function(){
  summary <- data.frame()
  
  path = "processed/cz/clusters"
  files = list.files(path)
  
  for(f in files){
    print(f)
    data = read.csv(sprintf("%s/%s", path, f))
    
    sum = sum(data$active, na.rm = TRUE)
    active_avg = mean(data$active, na.rm = TRUE)
    size = nrow(data) / 2976
    
    r = subset(data, data$shift == "R")
    o = subset(data, data$shift == "O")
    n = subset(data, data$shift == "N")
    
    Morning = round(sum(r$active, na.rm = TRUE) / sum * 100, digits = 2)
    Aftenoon = round(sum(o$active, na.rm = TRUE) / sum * 100, digits = 2)
    Night = round(sum(n$active, na.rm = TRUE) / sum * 100, digits = 2)
    
    newRow = data.frame(Morning, Aftenoon, Night, active_avg, size, f)
    summary = rbind(summary, newRow)
  }
}

#created: kmeans clusters by hourly average active consumption
kmeanshourlyAvgSummary <- function(){
  summary <- data.frame()

  files = list.files("processed/cz/load_01_2017", full.names = TRUE)
  fullFiles = list.files("processed/cz/load_01_2017")

  datasets = lapply(files, read.csv)
  i = 1
  for (data in datasets) {
    print(fullFiles[i])
    aggregate = aggregate(data[,c("active")], by = list(hour = data$hour), FUN = mean, na.rm = TRUE)

    newRow = as.data.frame(t(aggregate$x))
    newRow[is.na(newRow)] = 0
    newRow = (newRow - min(newRow))/(max(newRow) - min(newRow)) # scaling
    # newRow = as.data.frame(scale(newRow, center = mins, scale = maxs - mins))
    colnames(newRow) = as.character(aggregate$hour)

    newRow = cbind(newRow, file = fullFiles[i])

    summary = rbind(summary, newRow)

    i = i + 1
  }

  # clustering
  print("Creating clusters")
  kmeans = kmeans(summary[ ,1:24],10, nstart= 20)
  result = data.frame(summary$file, kmeans$cluster)
  colnames(result) = c("file", "cluster")
  write.csv(result, "processed/cz/clusters.csv", quote = FALSE, row.names = FALSE)

path = "processed/cz/load_01_2017"

cluster1 = data.frame()
  cluster2 = data.frame()
  cluster3 = data.frame()
  cluster4 = data.frame()
  cluster5 = data.frame()
  cluster6 = data.frame()
  cluster7 = data.frame()
  cluster8 = data.frame()
  cluster9 = data.frame()
  cluster10 = data.frame()

  for(f in result$file){
  	print(f)
  	data <- read.csv(sprintf("%s/%s", path, f))

  	c = result$cluster[result$file == f]
  	if (c == 1) cluster1 = rbind(cluster1, data)
  	if (c == 2) cluster2 = rbind(cluster2, data)
  	if (c == 3) cluster3 = rbind(cluster3, data)
  	if (c == 4) cluster4 = rbind(cluster4, data)
  	if (c == 5) cluster5 = rbind(cluster5, data)
  	if (c == 6) cluster6 = rbind(cluster6, data)
  	if (c == 7) cluster7 = rbind(cluster7, data)
  	if (c == 8) cluster8 = rbind(cluster8, data)
  	if (c == 9) cluster9 = rbind(cluster9, data)
  	if (c == 10) cluster10 = rbind(cluster10, data)
  }

  write.csv(x = cluster1, file = "processed/cz/clusters/cluster1.csv", quote = FALSE, row.names = FALSE)
  write.csv(x = cluster2, file = "processed/cz/clusters/cluster2.csv", quote = FALSE, row.names = FALSE)
  write.csv(x = cluster3, file = "processed/cz/clusters/cluster3.csv", quote = FALSE, row.names = FALSE)
  write.csv(x = cluster4, file = "processed/cz/clusters/cluster4.csv", quote = FALSE, row.names = FALSE)
  write.csv(x = cluster5, file = "processed/cz/clusters/cluster5.csv", quote = FALSE, row.names = FALSE)
  write.csv(x = cluster6, file = "processed/cz/clusters/cluster6.csv", quote = FALSE, row.names = FALSE)
  write.csv(x = cluster7, file = "processed/cz/clusters/cluster7.csv", quote = FALSE, row.names = FALSE)
  write.csv(x = cluster8, file = "processed/cz/clusters/cluster8.csv", quote = FALSE, row.names = FALSE)
  write.csv(x = cluster9, file = "processed/cz/clusters/cluster9.csv", quote = FALSE, row.names = FALSE)
  write.csv(x = cluster10, file = "processed/cz/clusters/cluster10.csv", quote = FALSE, row.names = FALSE)
}


plot <- function(){
  path = "processed/cz/load_01_2017"
  files = list.files(path, full.names = TRUE)
  for (f in files){
    print(f)
    data <- read.csv(sprintf("%s/%s", path, f))
    active = data$active
    reactive = data$reactive
    
    png(sprintf("processed/cz/plot_01_2017/%s.png",f))
    plot(active, type="l", xlab="Time", ylim=range(c(active, reactive), na.rm=TRUE), ylab="Energy [W / VAr]", col="red", main='Active vs Reactive energy')
    lines(reactive,col="green")
    dev.off()
  }
  
  
  # legend('topright',legend=c("Active","Reactive"),col=c("red","green"), lty=1, bty='n')
}