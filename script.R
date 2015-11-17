processingEnergy <- function(file) {
  energy <- read.csv(file)
  energy$TimeUTC[1:5714] <- energy$TimeUTC[1:5714] + 90
  energy$TimeUTC[5715:13104] <- energy$TimeUTC[5715:13104] + 30
  energy$TimeUTC[13105:23186] <- energy$TimeUTC[13105:23186] + 90
  energy$TimeUTC[23187:28610] <- energy$TimeUTC[23187:28610] + 30
  energy$DateUTC <- as.Date(energy$DateUTC,"%d/%m/%Y")
  write.csv(
    x = energy,file = "bratislava/energy2.csv", quote = FALSE, row.names = FALSE
  )
}
processingEnergy("bratislava/energy.csv")

processingWeather <- function(file) {
  weather <- read.csv(file)
  weather <- weather[-c(5,6,8,9,10,11,12)]
  weather$TimeUTC <- substr(weather$DateUTC,12,16)
  weather$TimeUTC <- sapply(strsplit(weather$TimeUTC,":"),
                            function(x) {
                              x <- as.numeric(x)
                              x[1] * 78 + x[2]
                            })
  weather$DateUTC <- as.Date(weather$DateUTC)
  weather <- weather[!duplicated(weather[,6:7]) ,]
  write.csv(
    x = weather, file = "bratislava/weather2.csv", quote = FALSE, row.names = FALSE
  )
}
processingWeather("bratislava/weather.csv")

mergeFunc <- function(ene_file, wea_file) {
  energy <- read.csv(ene_file)
  weather <- read.csv(wea_file)
  merged <- merge(energy,weather,c("DateUTC","TimeUTC"),sort = FALSE)
  write.csv(
    x = merged,file = "bratislava/merged.csv", quote = FALSE, row.names = FALSE
  )
}
mergeFunc("bratislava/energy2.csv", "bratislava/weather2.csv")

drawGraph <- function(file){
  merged <- read.csv(file)
#   library(corrplot)
#   m <- cor(merged[,2:6])
#   corrplot(m, method = "circle")
  
  # pairs(merged, panel = panel.smooth)
#   plot(merged$Temperature_F, merged$SUM_of_MNOZSTVO, pch = 1, col = "red", cex = 0.5,
#        xlab = "Temperature [°F]", ylab = "Consumption [MWh]")
  ts <- ts(merged$SUM_of_MNOZSTVO, frequency = 96, start = 0)
  stl.dekom <- stl(ts, s.window="periodic", robust = TRUE)
  plot(stl.dekom)
  
}
drawGraph("bratislava/merged.csv")