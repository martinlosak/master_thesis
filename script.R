getValues <- function(from, to, inputs, dbTable){
  query = paste("SELECT `datetime`,`load`,",paste("`",paste(inputs, collapse = "`,`"),"`", sep = "")," FROM ",dbTable," WHERE DATE(datetime) between '",from,"' AND '",to,"'", sep="")
  result = dbSelect(query)
  result$datetime = as.POSIXct(result$datetime, format="%Y-%m-%d %H:%M")
  return(result)
}

correlate <- function(data, method){
  correlation = cor(data[sapply(data, is.numeric)], method = method)
  return (correlation)
}

createNeuralModel <- function(data, inputs, split) {
  maxs = apply(data[sapply(data, is.numeric)], 2, max)
  mins = apply(data[sapply(data, is.numeric)], 2, min)
  
  data_scaled = as.data.frame(scale(data[sapply(data, is.numeric)], center = mins, scale = maxs - mins))  # skalujem na interval [0,1]
  
  train_scaled = data_scaled[1:split, ]   # trenovacia mnozina

  library(neuralnet)
  nn = neuralnet(paste("load ~", paste(inputs, collapse = " + ")), data = train_scaled, hidden = c(5), threshold = 0.01, stepmax = 1e+6, algorithm = 'rprop+', learningrate.limit = c(10^(-6), 50), learningrate.factor = list(minus = 0.5, plus = 1.2), err.fct = 'sse', act.fct = "logistic", linear.output = TRUE, lifesign = 'full')
  return(nn)
}

computeANN <- function(nn, data, inputs, split){
  maxs = apply(data[sapply(data, is.numeric)], 2, max)
  mins = apply(data[sapply(data, is.numeric)], 2, min)
  
  data_scaled = as.data.frame(scale(data[sapply(data, is.numeric)], center = mins, scale = maxs - mins))  # skalujem na interval [0,1]
  
  test_scaled = data_scaled[(split+1):nrow(data_scaled), ] # testovacia mnozina
  
  library(neuralnet)
  predicted_scaled = compute(nn, test_scaled[, inputs])
  predicted = predicted_scaled$net.result * (max(data$load) - min(data$load)) + min(data$load)
  test = (test_scaled$load) * (max(data$load) - min(data$load)) + min(data$load)
  mape = 100 * sum(abs((test - predicted) / test)) / nrow(test_scaled)
  
  output = data.frame(cbind(data[(split+1):nrow(data_scaled),],predicted))
  
  computed = list("mape" = round(mape, digits = 4), "data" = output)
  return(computed)
}

createGamModel <- function(data){
  library(mgcv)
  model = gamm(load ~ s(day, bs = "cc", k = 7) + s(temperature), data = data)
  layout(matrix(1:2, ncol = 2))
  plot(model$gam, shade=TRUE, shade.col="lightblue", pch=19, cex=0.75)
  plot = layout(1)
  return(plot)
}

regionCorrelationChart <- function(data){
	library(ggplot2)
	library(reshape2)

	mdata = melt(data, id.vars = "region")

	plot = ggplot(mdata, aes(variable, value, fill=region)) + geom_bar(stat="identity", position="dodge") + theme(axis.text = element_text(size=14))
	return(plot)
}

drawPlot <- function(data){
  library(ggplot2)
  plot = ggplot(data, aes(datetime, load)) + geom_line(colour="steelblue",size=1) + labs(x="Time",y="Load")
  return(plot)
}

drawNNPlot <- function(nn){
  plot = plot(nn, intercept = FALSE, show.weights = FALSE, col.entry = "red", col.hidden = "blue", col.out = "green")
  return(plot)
}

computedPlot <- function(computed){
  plot = plot(computed$data$load,computed$data$predicted, xlab="Load [kWh]", ylab="Load [kWh]",col='red',main='Real vs fitted load',pch=18,cex=0.7)
  abline(0,1,lwd=2)
  legend('bottomright',legend='ANN',pch=18,col='red', bty='n')
  
  return(plot)
}

computedLinePlot <- function(computed){
  plot = plot(computed$data$predicted, type="l", xlab="Time", ylab="Load [kWh]", col="red",main='Real vs fitted load')
  lines(computed$data$load,col="green")
  legend('topright',legend=c("ANN","Real"),col=c("red","green"), lty=1, bty='n')
  return(plot)
}

computedResidualsPlot <- function(computed){
  library(ggplot2)
  library(data.table)
  computed$data$residuals = computed$data$predicted - computed$data$load
  p = ggplot(data = data.table(Fitted_values = computed$data$predicted, Residuals = computed$data$residuals),
   aes(Fitted_values, Residuals)) +
  geom_point(size = 1.7) +
  geom_smooth() +
  geom_hline(yintercept = 0, color = "red") +
  labs(title = "Fitted values vs Residuals")
  plot = print(p)
  return(plot)
}