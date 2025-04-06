# Install and load required packages
if (!requireNamespace("readr", quietly = TRUE)) install.packages("readr")
if (!requireNamespace("dplyr", quietly = TRUE)) install.packages("dplyr")
if (!requireNamespace("ggplot2", quietly = TRUE)) install.packages("ggplot2")
if (!requireNamespace("reshape2", quietly = TRUE)) install.packages("reshape2")

library(readr)
library(dplyr)
library(ggplot2)
library(reshape2)
# wget https://opreturn.org/raw/price.csv
# cat price.csv | tr '[]' '  ' > price2.csv

price <- read_csv("price2.csv", col_names = FALSE)
names(price) <- seq(1, 1120)
price$block <- (seq.int(nrow(price)) - 1) * 288

heatmap_data_long <- melt(price, id.vars = "block", 
                          variable.name = "Measurement", 
                          value.name = "Value")

df <- heatmap_data_long[heatmap_data_long$block < 2000000, ]
df$Measurement <- as.integer(df$Measurement)
df <- df[df$Value > 0, ]
g <- ggplot(df, aes(x = block, y = Measurement, colour = log(Value))) +
  geom_point(shape = ".") +
  scale_color_gradient(low = "white", high = "black") +
  
  geom_hline(yintercept=log(0.00000001*100000000)*50, linetype="dashed", color = "blue") +
  geom_text(aes( 0, log(0.00000001*100000000)*50, label = "0.00000001 BTC", vjust = -1), size = 3, color = "blue") +
  
  geom_hline(yintercept=log(0.0000001*100000000)*50, linetype="dashed", color = "blue") +
  geom_text(aes( 0, log(0.0000001*100000000)*50, label = "0.0000001 BTC", vjust = -1), size = 3, color = "blue") +
  
  geom_hline(yintercept=log(0.000001*100000000)*50, linetype="dashed", color = "blue") +
  geom_text(aes( 0, log(0.000001*100000000)*50, label = "0.000001 BTC", vjust = -1), size = 3, color = "blue") +
  
  geom_hline(yintercept=log(0.00001*100000000)*50, linetype="dashed", color = "blue") +
  geom_text(aes( 0, log(0.00001*100000000)*50, label = "0.00001 BTC", vjust = -1), size = 3, color = "blue") +
  
  geom_hline(yintercept=log(0.0001*100000000)*50, linetype="dashed", color = "blue") +
  geom_text(aes( 0, log(0.0001*100000000)*50, label = "0.0001 BTC", vjust = -1), size = 3, color = "blue") +
    
  geom_hline(yintercept=log(0.001*100000000)*50, linetype="dashed", color = "blue") +
  geom_text(aes( 0, log(0.001*100000000)*50, label = "0.001 BTC", vjust = -1), size = 3, color = "blue") +
    
  geom_hline(yintercept=log(0.01*100000000)*50, linetype="dashed", color = "blue") +
  geom_text(aes( 0, log(0.01*100000000)*50, label = "0.01 BTC", vjust = -1), size = 3, color = "blue") +
  
  geom_hline(yintercept=log(0.1*100000000)*50, linetype="dashed", color = "blue") +
  geom_text(aes( 0, log(0.1*100000000)*50, label = "0.1 BTC", vjust = -1), size = 3, color = "blue") +
  
  geom_hline(yintercept=log(1*100000000)*50, linetype="dashed", color = "yellow") +
  geom_text(aes( 0, log(1*100000000)*50, label = "1 BTC", vjust = -1), size = 3, color = "yellow") +
  
  geom_hline(yintercept=log(10*100000000)*50, linetype="dashed", color = "red") +
  geom_text(aes( 0, log(10*100000000)*50, label = "10 BTC", vjust = -1), size = 3, color = "red") +

  geom_hline(yintercept=log(100*100000000)*50, linetype="dashed", color = "red") +
  geom_text(aes( 0, log(100*100000000)*50, label = "100 BTC", vjust = -1), size = 3, color = "red") +

  ggtitle("Output size distribution") +
  xlab("Height") + 
  ylab("Amount distribution")
ggsave(filename = "output_size_full.png", g, width = 10000, height = 2000,  units = "px", device="png")

