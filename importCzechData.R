createSummary <- function() {
  summary <- data.frame(electrometer = integer(), postcode = character(), location = character(), rows = integer(), file = character())
  
  files <- list.files(path = "data/cz", full.names = TRUE)
  for (f in files){
    print(f)
    t <-read.csv(f, header = FALSE, sep = "\t", stringsAsFactors = FALSE)
    electrometer <- as.integer(gsub("[^0-9]", "", t[1, 1]))
    location <- t[2, 1]
    postcode <- t[2, 2]
    rows <- nrow(t) -3
    newRow <- data.frame(electrometer, postcode, location, rows, f)
    summary <- rbind(summary, newRow)
  }
}

processingCzechEnergy <- function() {
  
  files = list.files(path = "data/cz", full.names = TRUE)
  
  for (f in files){
    print(f)
    
    data = read.csv(f, sep = "\t", skip = 3)
    data = setNames(data, c("date","time","active", "active_out", "reactive", "reactive_out"))
    
    isActive = !all(data$active == 0 | is.na(data$active))
    isReactive = !all(data$reactive == 0 | is.na(data$reactive))
    
    if(isActive && isReactive){
      header = read.csv(f, sep = "\t", header = FALSE, nrows = 2)
      electrometer = as.numeric(gsub("[^0-9]", "", header[1, 1]))
      postcode = gsub("[^0-9]", "", header[2, 2])
      
      data$datetime = as.POSIXct(paste(data$date, data$time), format="%Y%m%d %H:%M:%S")
      
      energy = data[, c("datetime", "active", "reactive")]
      
      write.csv(x = energy, file = sprintf("processed/cz/%d_%s.csv", electrometer, postcode), quote = FALSE, row.names = FALSE)
    }
  }  
}