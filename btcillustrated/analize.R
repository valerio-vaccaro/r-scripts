setwd("~/r-scripts/btcillustrated")

library(dplyr)
library(ggplot2)
library(lubridate)
library(reshape2)

years <- list(2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019, 2020, 2021, 2022, 2023, 2024, 2025)

blocks <- read.csv("scriptpubkeys_per_block.csv")
blocks$Timestamp <- as.POSIXct(blocks$Timestamp, origin="1970-01-01")
blocks <- blocks %>%
  mutate(Delta = Timestamp - lag(Timestamp))

# plot delta 
blocks_filtered <- blocks[blocks$Delta<3600*4,]

g <- ggplot(blocks_filtered, aes(Height, Delta)) +
  geom_point(alpha=0.1) +
  geom_hline(yintercept=0, linetype="dashed", color = "yellow") +
  geom_hline(yintercept=600, linetype="dashed", color = "red") +
  geom_hline(yintercept=3600, linetype="dashed", color = "blue") +
  geom_hline(yintercept=3600*2, linetype="dashed", color = "blue") +
  geom_hline(yintercept=3600*3, linetype="dashed", color = "blue") +
  geom_hline(yintercept=3600*4, linetype="dashed", color = "blue") +
  ggtitle("Delay between two blocks\nFull dataset") +
  xlab("Height") + ylab("Delay (s)")
ggsave(filename = "delta_full.png", g, width = 10000, height = 2000,  units = "px", device="png")

for (x in years) {
  g <- ggplot(blocks_filtered[year(blocks_filtered$Timestamp) == x,], aes(Timestamp, Delta)) +
    geom_point(alpha=0.3) +
    geom_hline(yintercept=0, linetype="dashed", color = "yellow") +
    geom_hline(yintercept=600, linetype="dashed", color = "red") +
    geom_hline(yintercept=3600, linetype="dashed", color = "blue") +
    geom_hline(yintercept=3600*2, linetype="dashed", color = "blue") +
    geom_hline(yintercept=3600*3, linetype="dashed", color = "blue") +
    geom_hline(yintercept=3600*4, linetype="dashed", color = "blue") +
    ggtitle(paste("Delay between two blocks\n", x, sep="")) +
    xlab("Timestamp") + ylab("Delay (s)")
  filename = paste("delta_", x, sep = "")
  filename = paste(filename, ".png", sep = "")
  ggsave(filename = filename, g, width = 10000, height = 2000,  units = "px", device="png")
}

# plot nonce
g = ggplot(blocks, aes(Height, Nonce)) +
  geom_point(alpha=0.1) +
  ggtitle("Nonce distribution\nFull dataset") +
  xlab("Height") + ylab("Nonce")
ggsave(filename = "nonce_full.png", g, width = 10000, height = 2000,  units = "px", device="png")

for (x in years) {
  g <- ggplot(blocks[year(blocks$Timestamp) == x,], aes(Timestamp, Nonce)) +
    geom_point(alpha=0.3) +
    ggtitle(paste("Nonce distribution\n", x, sep="")) +
    xlab("Timestamp") + ylab("Nonce")
  filename = paste("nonce_", x, sep = "")
  filename = paste(filename, ".png", sep = "")
  ggsave(filename = filename, g, width = 10000, height = 2000,  units = "px", device="png")
}

df <- blocks
df$Tx <- NULL
df$Timestamp <- NULL
df$Nonce <- NULL
df$Delta <- NULL
df <- melt(df ,  id.vars = 'Height', variable.name = 'Series')

g <- ggplot(df[df$value > 0,], aes(Height, value, alpha=0.001, color=Series)) +
  geom_point() +
  facet_grid(Series ~ .) +
  ggtitle("Scrptpubkeys distribution\nFull dataset") +
  xlab("Height") + ylab("Occorrences in a block")
ggsave(filename = "scriptpubkeys_full.png", g, width = 10000, height = 4000,  units = "px", device="png")

df <- blocks
df$Tx <- NULL
df$Height <- NULL
df$Nonce <- NULL
df$Delta <- NULL
df <- melt(df ,  id.vars = 'Timestamp', variable.name = 'Series')

df_filtered <- df[df$value > 0,]

for (x in years) {
  g <- ggplot(df_filtered[year(df_filtered$Timestamp) == x,], aes(Timestamp, value, alpha=0.001, color=Series)) +
    geom_point() +
    facet_grid(Series ~ .) +
    ggtitle(paste("Scrptpubkeys distribution\n", x, sep="")) +
    xlab("Timestamp") + ylab("Occorrences in a block")
  filename = paste("scriptpubkeys_", x, sep = "")
  filename = paste(filename, ".png", sep = "")
  ggsave(filename = filename, g, width = 10000, height = 4000,  units = "px", device="png")
}

