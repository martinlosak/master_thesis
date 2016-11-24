processingEnergy <- function(file) {
  energy <- read.csv(file)
  energy$TimeUTC[1:5714] <- energy$TimeUTC[1:5714] + 90
  energy$TimeUTC[5715:13104] <- energy$TimeUTC[5715:13104] + 30
  energy$TimeUTC[13105:23186] <- energy$TimeUTC[13105:23186] + 90
  energy$TimeUTC[23187:28610] <- energy$TimeUTC[23187:28610] + 30
  energy$DateUTC <- as.Date(energy$DateUTC, "%d/%m/%Y")
  write.csv(
    x = energy,
    file = "bratislava/energy2.csv",
    quote = FALSE,
    row.names = FALSE
  )
}
processingEnergy("bratislava/energy.csv")

processingWeather <- function(file) {
  weather <- read.csv(file)
  weather <- weather[-c(5, 6, 8, 9, 10, 11, 12)]
  weather$TimeUTC <- substr(weather$DateUTC, 12, 16)
  weather$TimeUTC <- sapply(strsplit(weather$TimeUTC, ":"),
                            function(x) {
                              x <- as.numeric(x)
                              x[1] * 60 + x[2]
                            })
  weather$DateUTC <- as.Date(weather$DateUTC)
  weather <- weather[!duplicated(weather[, 6:7]) , ]
  write.csv(
    x = weather,
    file = "bratislava/weather2.csv",
    quote = FALSE,
    row.names = FALSE
  )
}
processingWeather("bratislava/weather.csv")

mergeFunc <- function(ene_file, wea_file) {
  energy <- read.csv(ene_file)
  weather <- read.csv(wea_file)
  merged <- merge(energy, weather, c("DateUTC", "TimeUTC"), sort = FALSE)
  write.csv(x = merged, file = "bratislava/merged.csv", quote = FALSE, row.names = FALSE)
}
mergeFunc("bratislava/energy2.csv", "bratislava/weather2.csv")

decomposeDemand <- function(file) {
  merged <- read.csv(file)
  
  ts <- ts(merged$SUM_of_MNOZSTVO,
           frequency = 96,
           start = 0)
  decom <- stl(ts, s.window = "periodic", robust = TRUE)
  
  ts_temp = data.frame(merged$DateUTC, merged$Temperature_F, decom$time.series[, "trend"])
  
  write.csv(
    x = ts_temp,
    file = "bratislava/ts_temp.csv",
    quote = FALSE,
    row.names = FALSE
  )
  
}
decomposeDemand("bratislava/merged.csv")

drawGraph <- function(file) {
  ts_temp <- read.csv(file)
  #   library(corrplot)
  #   m <- cor(merged[,2:6])
  #   corrplot(m, method = "circle")
  
  #   pairs(merged, panel = panel.smooth)
  #   plot(merged$Temperature_F, merged$SUM_of_MNOZSTVO, pch = 1, col = "red", cex = 0.5,
  #        xlab = "Temperature [°F]", ylab = "Consumption [MWh]")
  #   plot(x = ts_temp$DateUTC, y = ts_temp$decom)
  #   lines(x = ts_temp$DateUTC, y = ts_temp$Temperature_F)
  
  x <- ts_temp$DateUTC
  y1 <- ts_temp$decom
  y2 <- ts_temp$Temperature_F
  
  
  library(ggplot2)
  temp_plot = ggplot(ts_temp, aes(x = DateUTC, y = Temperature_F)) + geom_line(colour = "red") + xlab("Čas") + ylab("Teplota") + theme_classic()
  temp_plot
  #   demd_plot = ggplot(ts_temp, aes(x = DateUTC, y = decom)) + geom_line(colour = "blue") + xlab("Čas") + ylab("Spotreba") + theme_classic()
  #   demd_plot
  
  #   plot(x, y1, type = "l", col = "blue", xlab = "Čas", ylab = "Spotreba")
  #   plot(x, y2, type = "l", col = "red", xlab = "Čas", ylab = "Teplota")
}
#drawGraph("bratislava/ts_temp.csv")

neural <- function() {
  data <- read.csv("bratislava/energy2.csv")  # nacitam si energ. data
  data <- data[17521:19008, ]                  # iba vezmem jul/2014
  data <- data[-c(1)]                         # odstranim datum
  maxs <- apply(data, 2, max)
  mins <- apply(data, 2, min)
  
  data_scaled <- as.data.frame(scale(data, center = mins, scale = maxs - mins))  # skalujem na interval [0,1]
  
  train_scaled <- data_scaled[1:1440, ]   # trenovacia mnozina 30 dni
  test_scaled <- data_scaled[1441:1488, ] # testovacia mnozina 31 den
  
  library(neuralnet)
  f <- as.formula(paste("SUM_of_MNOZSTVO ~ TimeUTC"))
  nn <- neuralnet(f, data = train_scaled, hidden = c(5, 3), linear.output = T)
  plot(nn)
  pr.nn_scaled <- compute(nn, test_scaled[, 1:1])
  pr.nn <- pr.nn_scaled$net.result * (max(data$SUM_of_MNOZSTVO) - min(data$SUM_of_MNOZSTVO)) + min(data$SUM_of_MNOZSTVO)
  test <- (test_scaled$SUM_of_MNOZSTVO) * (max(data$SUM_of_MNOZSTVO) - min(data$SUM_of_MNOZSTVO)) + min(data$SUM_of_MNOZSTVO)
  MSE.nn <- sum((test - pr.nn) ^ 2) / nrow(test_scaled)
  MAPE.nn <- 100 * sum(abs((test - pr.nn) / test)) / nrow(test_scaled)
}
neural()