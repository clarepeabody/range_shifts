rm(list=ls())

#install libraries
library(tidyverse)
library(devtools)
#devtools::install_github("dkahle/ggmap", ref = "tidyup", force=TRUE)
library(ggmap)
options(stringsAsFactors = F)

#get API key for ggmap:
#API_key = insert api key

#see data_prep.R for full explanation of starting_point
calcofi <- read_csv("starting_point.csv")

#move data into long format
calcofi <-
  calcofi %>%
  group_by(year, season) %>%
  gather(key = "species", value = "value", 11:21) 

#order season factors
calcofi$season <- as.factor(calcofi$season)
levels(calcofi$season) <- c("winter", "spring", "summer", "fall")

unique(calcofi$species)

#calculate the abundance weighted lat/long of each species, as the sum across all lat long, multiplied by the abundance/total abundance for that year
mean_loc <-
  calcofi[complete.cases(calcofi),] %>%
  ungroup() %>%
  group_by(year, season, species) %>%
  mutate(total = sum(value)) %>%
  summarize(mean_long = sum(longitude*(value/total)), mean_lat = sum(latitude*(value/total)))
  
#create plots for each species mean lat
for (i in unique(mean_loc$species)) {
  nickname = substr(i, 1,4)
  filename = paste(nickname, "_mean_lat", ".png", sep = "")
  p = ggplot(data = mean_loc[which(mean_loc$species == i),]) +
    geom_point(aes(x = year, y = mean_lat)) +
    ggtitle(label = i) +
    geom_smooth(aes(x = year, y = mean_lat), method = "lm")+
    facet_wrap(~season)+
    theme_bw()+
  png(filename = filename)
  print(p)
  dev.off()
}

#create plots for mean centroid
for (i in unique(mean_loc$species)) {
  nickname = substr(i, 1,4)
  filename = paste(nickname, "_mean_centroid", ".png", sep = "")
  p = ggplot(data = mean_loc[which(mean_loc$species == i),]) +
    # geom_text(aes(label = year, x = mean_long, y = mean_lat, color = year)) +
    ggtitle(label = i) +
    geom_path(aes(x = mean_long, y = mean_lat, color = year))+
    scale_color_continuous()+
    facet_wrap(~season)+
    theme_bw()+
  png(filename = filename)
  print(p)
  dev.off()
}

class(calcofi$line)
calcofi$line <- asr(calcofi$line)
class(calcofi$line)
calcofi$line

#create heatmap of abundance at each unique station latitude
calcofi_hm <-
  calcofi %>%
  ungroup() %>%
  mutate(line = as.character(line)) %>%
  select(year, season, line, species, value) %>%
  group_by(year, season, line, species) %>%
  mutate(mean_line_v_log = log(mean(value))+1)

for (i in unique(calcofi_hm$species)) {
  nickname = substr(i, 1,4)
  filename = paste(nickname, "_heatmap", ".png", sep = "")
  p = ggplot(calcofi_hm[which(calcofi_hm$species == i),], aes(year, line)) +
    ggtitle(label = i) +
    geom_tile(aes(fill = mean_line_v_log))+
    scale_fill_gradient()+
    facet_wrap(~season)+
    theme_bw()+
    )
    png(filename = filename)
  print(p)
  dev.off()
}
