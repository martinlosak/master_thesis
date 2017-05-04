processingTexasWeather <- function(){
  weather = read.csv("data/texas/texas-weather.csv")
  weather = setNames(weather, c("datetime","temperature","dewpoint","humidity","pressure","wind","rainfall","cloud"))
  weather$datetime = as.POSIXct(weather$datetime, tz = "CST6CDT")
  write.csv(weather, file = "processed/texas/texas_weather.csv", quote = FALSE, row.names = FALSE)
}

processingTexasEnergy <- function(){
  export = read.csv("data/texas/dataport-export.csv")
  metadata = read.csv("data/texas/dataport-metadata.csv")
  
  # odstranenie 2016 navyse
  export = export[!(export$localminute=="2016-01-01 00:00:00-06"),]
  # spojenie s metadatami a odstranenie zaznamov mimo mesto Austin
  energy = merge(x = export[,c("dataid", "localminute","use")], y = metadata[,c("dataid", "city")], by = "dataid", all.x=TRUE)
  energy = energy[(energy$city=="Austin"),]
  
  library(data.table)
  energy.dt = data.table(energy)
  # SUM(load) podla datetime a usporiadanie podla datetime
  energy = energy.dt[,list(load=sum(use)), by='localminute']
  energy = energy[order(energy$localminute),]
  energy$datetime = as.POSIXct(energy$localminute, tz = "CST6CDT")
  energy$localminute = NULL
  energy = energy[,c(2,1)]

  # vypocet oneskorenej spotreby
  start = 745 # 1.2.2014
  end = nrow(energy)
  energy$load_h1[start:end] = energy$load[(start-1):(end-1)]
  energy$load_h2[start:end] = energy$load[(start-2):(end-2)]
  energy$load_h3[start:end] = energy$load[(start-3):(end-3)]
  energy$load_d1[start:end] = energy$load[(start-24):(end-24)]
  energy$load_d1h1[start:end] = energy$load[(start-24-1):(end-24-1)]
  energy$load_d1h2[start:end] = energy$load[(start-24-2):(end-24-2)]
  energy$load_d1h3[start:end] = energy$load[(start-24-3):(end-24-3)]
  energy$load_w1[start:end] = energy$load[(start-168):(end-168)]
  
  # odstranenie 1. mesiaca
  energy = energy[-c(1:744),]
  
  library(lubridate)
  energy$day = ifelse(wday(energy$datetime)==1,7,wday(energy$datetime)-1)
  energy$date = as.Date(energy$datetime, tz = "CST6CDT")
  energy$time = hour(energy$datetime)
  
  # vypocet prazdnin
  holidays = dbSelect("SELECT * FROM texas_holiday")
  holidays$date = as.Date(holidays$date)
  energy$holiday = ifelse(energy$date %in% holidays$date, 1, 0)
  
  energy$season[is.element(month(energy$datetime), c(3,4,5))] = 1
  energy$season[is.element(month(energy$datetime), c(6,7,8))] = 2
  energy$season[is.element(month(energy$datetime), c(9,10,11))] = 3
  energy$season[is.element(month(energy$datetime), c(12,1,2))] = 4
  
  write.csv(energy, file = "processed/texas/texas_load.csv", quote = FALSE, row.names = FALSE)
}

insertTexasIntoDB <- function(){
  load <- read.csv("processed/texas/texas_load.csv")
  weather <- read.csv("processed/texas/texas_weather.csv")
  merged <- merge(x = load, y = weather, by = "datetime", all.x = TRUE)
  
  dbSelect("DELETE FROM texas")
  dbSelect("ALTER TABLE texas AUTO_INCREMENT = 1")
  dbWrite(data = merged, tableName = "texas")
}