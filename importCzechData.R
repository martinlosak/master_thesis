createSummary <- function() {
  summary <- data.frame(
    active_avg = numeric(), 
    reactive_avg = numeric(),
    
    active_max = numeric(),
    reactive_max = numeric(),
    
    active_min = numeric(),
    reactive_min = numeric(),
    
    ratio = numeric(),
    file = character()
  )

  path = "processed/cz_1month"
  files <- list.files(path)

  for (f in files){
    print(f)
    data <- read.csv(sprintf("%s/%s", path, f))
    
    active_avg = mean(data$active, na.rm = TRUE)
    reactive_avg = mean(data$reactive, na.rm = TRUE)
    
    ratio = reactive_avg / active_avg
    
    active_max = as.numeric(max(data$active, na.rm = TRUE))
    reactive_max = as.numeric(max(data$reactive, na.rm = TRUE))
    
    active_min = as.numeric(min(data$active, na.rm = TRUE))
    reactive_min = as.numeric(min(data$reactive, na.rm = TRUE))
    
    newRow <- data.frame(active_avg, reactive_avg, active_max, reactive_max, active_min, reactive_min, ratio, f)
    summary <- rbind(summary, newRow)
  }
}

# vytvorenie dat s active a reactive zlozkou, ktore neobsahuju iba 0 alebo NA
processingCzechEnergy <- function() {
  path = "data/cz"
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
      
      write.csv(x = energy, file = sprintf("processed/cz/%d_%s.csv", electrometer, postcode), quote = FALSE, row.names = FALSE)
    }
  }  
}

# vytvorenie subsetu 2 mesiacov zo vsetkych dat a s reactive datami viac ako 90%
createSubset <- function(){
  
  files = list.files(path = "processed/cz", full.names = TRUE)
  
  from = as.POSIXct("2016-11-30 23:45:00")
  to = as.POSIXct("2017-02-01 00:00:00")
  
  for (f in files){
    print(f)
    
    data <- read.csv(f)
    
    startDate = as.POSIXct(data[1,1])
    endDate = as.POSIXct(data[nrow(data),1])
    
    if (startDate < from & to < endDate){
      print("subset")
      # spravim subset dvoch mesiacov
      subset = subset(data, from < as.POSIXct(data$datetime) & as.POSIXct(data$datetime) < to)
      
      # ak percento reactive NA < 10
      rows = nrow(subset)
      reactiveNa = sum(is.na(subset$reactive)) / rows * 100
      
      if(reactiveNa < 10){
        write.csv(x = subset, file = sprintf("processed/%s", f), quote = FALSE, row.names = FALSE)
      }
    }
  }
}

# vytvorenie subsetu januar 2017 zo vsetkych dat s active datami viac ako 90%, ktore neobsahuju iba 0 alebo NA
# + dopocitanie typu dna
createSubset2 <- function(){
  files = list.files(path = "processed/cz")
  
  from = as.POSIXct("2016-12-31 23:45:00")
  to = as.POSIXct("2017-02-01 00:00:00")
  
  for (f in files){
    print(f)
    
    data <- read.csv(sprintf("processed/cz/%s", f))
    
    startDate = as.POSIXct(data[1,1])
    endDate = as.POSIXct(data[nrow(data),1])
    
    if (startDate < from & to < endDate){
      subset = subset(data, from < as.POSIXct(data$datetime) & as.POSIXct(data$datetime) < to)
      
      # ak percento active NA < 10
      rows = nrow(subset)
      isActive = !all(subset$active == 0 | is.na(subset$active))
      activeNa = sum(is.na(subset$active)) / rows * 100
      if(isActive & activeNa < 10){
        
        #vypocet typu dna
        library(lubridate)
        subset$day = ifelse(wday(subset$datetime)==1,7,wday(subset$datetime)-1)
        write.csv(x = subset, file = sprintf("processed/cz_1month/%s", f), quote = FALSE, row.names = FALSE)
      }
      
    }
  }
  
}

# create 7 clusters by average active power
createClusters <- function(){
	path = "processed/cz_1month"
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

	write.csv(x = cluster1, file = "processed/clusters/cluster1.csv", quote = FALSE, row.names = FALSE)
	write.csv(x = cluster2, file = "processed/clusters/cluster2.csv", quote = FALSE, row.names = FALSE)
	write.csv(x = cluster3, file = "processed/clusters/cluster3.csv", quote = FALSE, row.names = FALSE)
	write.csv(x = cluster4, file = "processed/clusters/cluster4.csv", quote = FALSE, row.names = FALSE)
	write.csv(x = cluster5, file = "processed/clusters/cluster5.csv", quote = FALSE, row.names = FALSE)
	write.csv(x = cluster6, file = "processed/clusters/cluster6.csv", quote = FALSE, row.names = FALSE)
	write.csv(x = cluster7, file = "processed/clusters/cluster7.csv", quote = FALSE, row.names = FALSE)
	write.csv(x = cluster8, file = "processed/clusters/cluster8.csv", quote = FALSE, row.names = FALSE)
}

countClustersMean <- function(){
	summary <- data.frame(
	    Mon = numeric(), 
	    Tue = numeric(),
	    Wed = numeric(),
	    Thu = numeric(),
	    Fri = numeric(),
	    Sat = numeric(),
	    Sun = numeric(),

	    active_avg = numeric(),
	    size = numeric(),
	    file = character()
	  )

	path = "processed/clusters"
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

		sum_mon = sum(mon$active, na.rm = TRUE)
		sum_tue = sum(tue$active, na.rm = TRUE)
		sum_wed = sum(wed$active, na.rm = TRUE)
		sum_thu = sum(thu$active, na.rm = TRUE)
		sum_fri = sum(fri$active, na.rm = TRUE)
		sum_sat = sum(sat$active, na.rm = TRUE)
		sum_sun = sum(sun$active, na.rm = TRUE)

		ratio_mon = sum_mon / sum * 100
		ratio_tue = sum_tue / sum * 100
		ratio_wed = sum_wed / sum * 100
		ratio_thu = sum_thu / sum * 100
		ratio_fri = sum_fri / sum * 100
		ratio_sat = sum_sat / sum * 100
		ratio_sun = sum_sun / sum * 100

		newRow <- data.frame(ratio_mon, ratio_tue, ratio_wed, ratio_thu, ratio_fri, ratio_sat, ratio_sun, active_avg, f)
    	summary <- rbind(summary, newRow)
	}
}


plot <- function(){
  files = list.files(path = "processed/cz_new", full.names = TRUE)
  for (f in files){
    print(f)
    data <- read.csv(f)
    active = data$active[1:1000]
    reactive = data$reactive[1:1000]
    
    png(sprintf("processed/%s.png",f))
    plot(active, type="l", xlab="Time", ylim=range(c(active, reactive), na.rm=TRUE), ylab="Energy [W / VAr]", col="red", main='Active vs Reactive energy')
    lines(reactive,col="green")
    dev.off()
  }
  
  
  # legend('topright',legend=c("Active","Reactive"),col=c("red","green"), lty=1, bty='n')
}