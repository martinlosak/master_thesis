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