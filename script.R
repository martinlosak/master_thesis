processingOriginalEnergy <- function(file) {
  energy <- read.csv("csv/8_ba_suma.csv")
  energy <- setNames(energy, c("date","time","load"))
  
  # nastavenie casu na interval [0 - 1425] minut
  energy$time[1:11428] <- energy$time[1:11428] + 105
  energy$time[11429:26208] <- energy$time[11429:26208] + 45
  energy$time[26209:46372] <- energy$time[26209:46372] + 105
  energy$time[46373:57220] <- energy$time[46373:57220] + 45
  # odstranenie zaznamov navyse pri zmene casu
  energy <- energy[-c(11425, 11426, 11427, 11428, 46369, 46370, 46371, 46372), ]
  
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
  
  # odstranenie 1 mesiaca
  energy <- energy[-c(1:2976),]
  # vypocet casu z minutovej hodnoty a spojenie s datumom do DATETIME
  energy$date <- as.Date(energy$date, "%d/%m/%Y")
  energy$datetime <- paste(energy$date, paste(energy$time %/% 60, energy$time %% 60, sep = ":"), sep = " ")
  energy$datetime <- as.POSIXct(energy$datetime, format="%Y-%m-%d %H:%M")
  
  library(lubridate)
  energy$day = ifelse(wday(energy$datetime)==1,7,wday(energy$datetime)-1)
  
  holidays <- c(as.Date("2013-08-29"), # snp
                as.Date("2013-09-01"), # ustava
                as.Date("2013-09-15"), # maria
                as.Date("2013-11-01"), # svati
                as.Date("2013-11-17"), # sloboda
                as.Date("2013-12-24"), # vianoce
                as.Date("2013-12-25"), # vianoce
                as.Date("2013-12-26"), # vianoce
                as.Date("2014-01-01"), # sr
                as.Date("2014-01-06"), # krali
                as.Date("2014-04-18"), # velkanoc
                as.Date("2014-04-21"), # velkanoc
                as.Date("2014-05-01"), # praca
                as.Date("2014-05-08"), # fasizmus
                as.Date("2014-07-05"), # cyril
                as.Date("2014-08-29"), # snp
                as.Date("2014-09-01"), # ustava
                as.Date("2014-09-15"), # maria
                as.Date("2014-11-01"), # svati
                as.Date("2014-11-17"), # sloboda
                as.Date("2014-12-24"), # vianoce
                as.Date("2014-12-25"), # vianoce
                as.Date("2014-12-26"), # vianoce
                as.Date("2015-01-01"), # sr
                as.Date("2015-01-06")) # krali
  energy$holiday <- ifelse(energy$date %in% holidays, 1, 0)

  energy <- na.omit(energy)
  
  write.csv(x = energy, file = "csv/ba_load.csv", quote = FALSE, row.names = FALSE)
}

processingSHMU <- function(){
  shmu <- read.csv("csv/8_BRATISLAVA.csv")
  shmu <- setNames(shmu, c("date","time","sun","temperature","pressure","wind","humidity","rainfall"))
  i <- 1
  while(i < 54240) {
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
  shmu$date <- NULL
  shmu$time <- NULL
  shmu$newtime <- NULL

  write.csv(x = shmu, file = "csv/ba_weather.csv", quote = FALSE, row.names = FALSE)
}

insertIntoDB <- function(){
  load <- read.csv("csv/ba_load.csv")
  weather <- read.csv("csv/ba_weather.csv")
  bratislava <- merge(x = load, y = weather, by = "datetime", all.x = TRUE)
  
  library(RMySQL)
  con <- dbConnect(MySQL(),user="root", password="852456",dbname="dp", host="localhost")
  dbSendQuery(con, "DELETE FROM bratislava")
  dbSendQuery(con, "ALTER TABLE bratislava AUTO_INCREMENT = 1")
  dbWriteTable(con, name="bratislava", value=bratislava, append=TRUE, row.names=FALSE)
  dbDisconnect(con)
}

getValues <- function(){
  library(RMySQL)
  con <- dbConnect(MySQL(),user="root", password="852456",dbname="dp", host="localhost")
  bratislava <- dbGetQuery(con,"SELECT time,`load`,load_h1,day,holiday,sun,temperature,pressure,wind,humidity,rainfall FROM bratislava WHERE YEAR(datetime) = 2014 AND MONTH(datetime) = 7")
  return(bratislava)
}

neural <- function() {
  library(RMySQL)
  con <- dbConnect(MySQL(),user="root", password="852456",dbname="dp", host="localhost")
  data <- dbGetQuery(con,"SELECT time,`load`,load_h1,load_h2,load_h3,day,holiday,sun,temperature,pressure,wind,humidity,rainfall FROM bratislava WHERE YEAR(datetime) = 2014 AND MONTH(datetime) = 7")

  params = c("time","day","holiday")
  
  maxs <- apply(data, 2, max)
  mins <- apply(data, 2, min)
  
  data_scaled <- as.data.frame(scale(data, center = mins, scale = maxs - mins))  # skalujem na interval [0,1]
  
  train_scaled <- data_scaled[1:2880, ]   # trenovacia mnozina 30 dni
  test_scaled <- data_scaled[2881:2976, ] # testovacia mnozina 31 den
  
  library(neuralnet)
  nn <- neuralnet(paste("load ~", paste(params, collapse = " + ")), data = train_scaled, hidden = c(5), threshold = 0.01, stepmax = 1e+6, algorithm = 'rprop+', learningrate.limit = c(10^(-6), 50), learningrate.factor = list(minus = 0.5, plus = 1.2), err.fct = 'sse', act.fct = "logistic", linear.output = TRUE, lifesign = 'full')
  
  plot(nn)
  
  
  pr.nn_scaled <- compute(nn, test_scaled[, params])
  pr.nn <- pr.nn_scaled$net.result * (max(data$load) - min(data$load)) + min(data$load)
  test <- (test_scaled$load) * (max(data$load) - min(data$load)) + min(data$load)
  MAPE.nn <- 100 * sum(abs((test - pr.nn) / test)) / nrow(test_scaled)
  
  plot(pr.nn,type="l", xlab="Hodina", ylab="Spotreba", col="red")
  lines(test,col="green")
}
neural()