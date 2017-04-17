processingSlovakEnergy <- function() {
  energy <- read.csv(paste("data/sr/load/",".csv", sep= mapper$energy))
  energy <- setNames(energy, c("date","time","load"))
  
  # nastavenie casu na interval [0 - 1425] minut
  energy$time[1:11428] <- energy$time[1:11428] + 105
  energy$time[11429:26208] <- energy$time[11429:26208] + 45
  energy$time[26209:46372] <- energy$time[26209:46372] + 105
  energy$time[46373:57220] <- energy$time[46373:57220] + 45
  # odstranenie zaznamov navyse pri zmene casu
  energy <- energy[-c(11425, 11426, 11427, 11428, 46369, 46370, 46371, 46372), ]
  
  # vypocet oneskorenej spotreby
  start <- 2977 # 1.8.2013
  end <- nrow(energy)
  energy$load_h1[start:end] <- energy$load[(start-1):(end-1)]
  energy$load_h2[start:end] <- energy$load[(start-2):(end-2)]
  energy$load_h3[start:end] <- energy$load[(start-3):(end-3)]
  energy$load_d1[start:end] <- energy$load[(start-96):(end-96)]
  energy$load_d1h1[start:end] <- energy$load[(start-96-1):(end-96-1)]
  energy$load_d1h2[start:end] <- energy$load[(start-96-2):(end-96-2)]
  energy$load_d1h3[start:end] <- energy$load[(start-96-3):(end-96-3)]
  energy$load_w1[start:end] <- energy$load[(start-672):(end-672)]
  
  # odstranenie 1. mesiaca
  energy <- energy[-c(1:2976),]
  
  # vypocet casu z minutovej hodnoty a spojenie s datumom do DATETIME
  energy$date <- as.Date(energy$date, "%d/%m/%Y")
  energy$datetime <- paste(energy$date, paste(energy$time %/% 60, energy$time %% 60, sep = ":"), sep = " ")
  energy$datetime <- as.POSIXct(energy$datetime, format="%Y-%m-%d %H:%M")
  
  #vypocet typu dna
  library(lubridate)
  energy$day = ifelse(wday(energy$datetime)==1,7,wday(energy$datetime)-1)
  
  # vypocer prazdnin
  holidays <- dbSelect("SELECT * FROM slovakia_holiday")
  holidays$date <- as.Date(holidays$date)
  energy$holiday <- ifelse(energy$date %in% holidays$date, 1, 0)
  
  # vypocet sezony
  energy$season[is.element(month(energy$datetime), c(3,4,5))] <- 1
  energy$season[is.element(month(energy$datetime), c(6,7,8))] <- 2
  energy$season[is.element(month(energy$datetime), c(9,10,11))] <- 3
  energy$season[is.element(month(energy$datetime), c(12,1,2))] <- 4
  
  energy <- na.omit(energy)
  
  write.csv(x = energy, file = sprintf("processed/sr/load/%s.csv", mapper$energy), quote = FALSE, row.names = FALSE)
}

processingSlovakWeather <- function(){
  shmu <- read.csv(paste("data/sr/meteo_edit/",".csv", sep= mapper$meteo))
  shmu <- setNames(shmu, c("date","time","sun","temperature","pressure","wind","humidity","rainfall"))
  i <- 1
  end <- nrow(shmu)
  while(i < end) {
    shmu$newtime[i] <- as.character(shmu$time[i])
    shmu$newtime[i+1] <- sapply(strsplit(as.character(shmu$time[i+1]), ":"),
                                function(x) {
                                  x <- as.numeric(x)
                                  paste(x[1], "15", "00", sep = ":")
                                })
    shmu$newtime[i+2] <- sapply(strsplit(as.character(shmu$time[i+2]), ":"),
                                function(x) {
                                  x <- as.numeric(x)
                                  paste(x[1], "30", "00", sep = ":")
                                })
    shmu$newtime[i+3] <- sapply(strsplit(as.character(shmu$time[i+3]), ":"),
                                function(x) {
                                  x <- as.numeric(x)
                                  paste(x[1], "45", "00", sep = ":")
                                })
    i <- i+4
  }
  # vypocet casu z minutovej hodnoty a spojenie s datumom do DATETIME
  shmu$datetime <- paste(as.Date(shmu$date, "%d/%m/%Y"), shmu$newtime, sep = " ")
  shmu$datetime <- as.POSIXct(shmu$datetime, format="%Y-%m-%d %H:%M:%S")
  # ak je wind NA, je 0
  shmu$wind[is.na(shmu$wind)] <- 0
  shmu$date <- NULL
  shmu$time <- NULL
  shmu$newtime <- NULL
  
  write.csv(x = shmu, file = paste("processed/sr/meteo/",".csv", sep= mapper$meteo), quote = FALSE, row.names = FALSE)
}

insertIntoDB <- function(){
  load <- read.csv(paste("processed/sr/load/",".csv", sep= mapper$energy))
  weather <- read.csv(paste("processed/sr/meteo/",".csv", sep= mapper$meteo))
  merged <- merge(x = load, y = weather, by = "datetime", all.x = TRUE)
  
  dbSelect(sprintf("DELETE FROM %s", mapper$table_name))
  dbSelect(sprintf("ALTER TABLE %s AUTO_INCREMENT = 1", mapper$table_name))
  con = getConnection()
  dbWriteTable(con, name=mapper$table_name, value=merged, append=TRUE, row.names=FALSE)
  dbDisconnect(con)
}