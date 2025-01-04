setwd("~/r-scripts/intesa")
# Install dependencies from CRAN
# 
# install.packages("yahoofinancer")
# install.packages("ggplot2")
# install.packages("ggpmisc")
# install.packages("lubridate")

library(yahoofinancer)
library(ggplot2)
library(ggpmisc)
library(lubridate)

download_history <- function(stock) {
  obj <- Ticker$new(stock)$get_history(period='2y')
  # covert data format
  obj$date <- as.Date(obj$date)
  # calculate value
  obj$value <- (obj$high + obj$low) / 2
  # remove 
  obj$high <- NULL
  obj$low <- NULL
  obj$open <- NULL
  obj$close <- NULL
  obj$adj_close <- NULL
  
  obj
}

convert_history  <- function(stock, btc_history) {
  obj <- download_history(stock)
  btc_history$value_btc <- btc_history$value
  btc_history$value <- NULL
  btc_history$volume <- NULL
  data <- merge(btc_history, obj, by = "date")
  data$value <- data$value / data$value_btc
  data$volume <- data$volume / data$value_btc
  data$value_btc <- NULL

  data
}

btc <- download_history('btc-usd')

full <- btc
full$stock <- 'btcusd'
                   
stocks <- list('aapl', 'amzn', 'ba', 'ge', 'intc', 'isp.mi', 'mcd', 'nvda', 'tsla')

for (x in stocks) {
  print(x)
  obj <- convert_history(x, btc)
  obj$stock <- x
  full <- rbind(full, obj)
  
  g = ggplot(obj, aes(x=date, y=value)) + 
    geom_line() + 
    geom_smooth(se = FALSE) +
    stat_peaks(geom = "point", span = 15, color = "steelblue3", size = 1) +
    stat_peaks(geom = "label", span = 15, color = "steelblue3", angle = 0, hjust = -0.1, x.label.fmt = "%Y-%m-%d") +
    stat_peaks(geom = "rug", span = 15, color = "blue", sides = "b") +
    stat_valleys(geom = "point", span = 11, color = "red", size = 1) +
    stat_valleys(geom = "label", span = 11, color = "red", angle = 0, hjust = -0.1, x.label.fmt = "%Y-%m-%d") +
    stat_valleys(geom = "rug", span = 11, color = "red", sides = "b")
  
  filename = paste(x, '_value.png', sep = '')
  ggsave(filename = filename, g, width = 10, height = 6, dpi = 150, units = "in", device='png')
  
  g = ggplot(obj, aes(x=date, y=volume)) + 
    geom_area() 
  
  filename = paste(x, '_volume.png', sep = '')
  ggsave(filename = filename, g, width = 10, height = 6, dpi = 150, units = "in", device='png')
}

g = ggplot(full[full$stock!='btcusd',], aes(x=date, y=value, color=stock)) + 
  geom_line() + 
  geom_smooth(se = FALSE) 

ggsave(filename = 'value.png', g, width = 10, height = 6, dpi = 150, units = "in", device='png')

g = ggplot(full[full$stock!='btcusd',], aes(x=date, y=volume, color=stock)) + 
  geom_line() + 
  geom_smooth(se = FALSE) 

ggsave(filename = 'volume.png', g, width = 10, height = 6, dpi = 150, units = "in", device='png')
