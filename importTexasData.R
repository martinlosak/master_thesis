processingTexasWeather <- function(){
  weather <- read.csv("data/texas/texas-weather.csv")
  weather <- setNames(weather, c("datetime","temperature","dewpoint","humidity","pressure","wind","rainfall","cloud"))
  weather$datetime <- as.POSIXct(weather$datetime, format="%Y-%m-%d %H:%M:%S")
  weather <- na.omit(weather)
  write.csv(x = weather, file = "processed/texas/texas_weather.csv", quote = FALSE, row.names = FALSE)
}

processingTexasEnergy <- function(){
  export <- read.csv("data/texas/dataport-export.csv")
  metadata <- read.csv("data/texas/dataport-metadata.csv")
  
  # odstranenie 2016 navyse
  export <- export[!(export$localminute=="2016-01-01 00:00:00-06"),]
  # spojenie s metadatami a odstranenie zaznamov mimo mesto Austin
  texas <- merge(x = export[,c("dataid", "localminute","use")], y = metadata[,c("dataid", "city")], by = "dataid", all.x=TRUE)
  texas <- texas[(texas$city=="Austin"),]
  
  library(data.table)
  texas.dt <- data.table(texas)
  # SUM(load) podla datetime a usporiadanie podla datetime
  texas <- texas.dt[,list(load=sum(use)), by='localminute']
  texas <- texas[order(texas$localminute),]
  texas$datetime <- as.POSIXct(texas$localminute, format="%Y-%m-%d %H:%M")
  texas$localminute <- NULL
  texas <- texas[,c(2,1)]
  
  library(lubridate)
  texas$day = ifelse(wday(texas$datetime)==1,7,wday(texas$datetime)-1)
  
  holidays <- read.csv("data/texas/texas-holidays.csv")
  holidays$date <- as.Date(holidays$date)
  texas$holiday <- ifelse(as.Date(texas$datetime) %in% holidays$date, 1, 0)
  
  texas$season[is.element(month(texas$datetime), c(3,4,5))] <- 1
  texas$season[is.element(month(texas$datetime), c(6,7,8))] <- 2
  texas$season[is.element(month(texas$datetime), c(9,10,11))] <- 3
  texas$season[is.element(month(texas$datetime), c(12,1,2))] <- 4
  
  texas <- na.omit(texas)
  
  write.csv(x = texas, file = "processed/texas/texas_load.csv", quote = FALSE, row.names = FALSE)
}

insertTexasIntoDB <- function(){
  load <- read.csv("processed/texas/texas_load.csv")
  weather <- read.csv("processed/texas/texas_weather.csv")
  merged <- merge(x = load, y = weather, by = "datetime", all.x = TRUE)
  
  dbSendQuery(con, "DELETE FROM texas")
  dbSendQuery(con, "ALTER TABLE texas AUTO_INCREMENT = 1")
  dbWriteTable(con, name="texas", value=merged, append=TRUE, row.names=FALSE)
}