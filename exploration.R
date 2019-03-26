rm(list=ls())

#install libraries
library(tidyverse)
library(devtools)
#devtools::install_github("dkahle/ggmap", ref = "tidyup", force=TRUE)
library(ggmap)
options(stringsAsFactors = F)

#get API key for ggmap:
#API_key = insert api key

#see starting_point_names and data.prep.R for full explanation of starting_point
calcofi <- read_csv("starting_point.csv")
  

#look at spatial and temporal coverage
calcofi_yr <-
  calcofi %>%
  select(year, season) %>%
  distinct() %>%
  mutate(yesno = 1)

  spread(season, yesno, fill = 0) %>%
  gather(key = season, value = "yesno", 2:5)

png("temporal_coverage.png")
ggplot(calcofi_yr, aes(x = year, y = season, color = yesno)) +
  geom_point()+
  guides(fill = F)
dev.off()
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
    scale_color_viridis_c()+
    facet_wrap(~season)+
    theme_bw()+
  png(filename = filename)
  print(p)
  dev.off()
}
?scale_fill_viridis_c
class(calcofi$line)
calcofi$line <- asr(calcofi$line)
class(calcofi$line)
calcofi$line

#create heatmap of abundance at each unique station latitude
calcofi_hm <-
  calcofi %>%
  ungroup() %>%
  mutate(line = as.factor(line)) %>%
  select(year, season, line, species, value) %>%
  group_by(year, season, line, species) %>%
  mutate(mean_line_v_log = log(mean(value))+1)

lev <- levels(calcofi_hm$line) 
levels(calcofi_hm$line) <- rev(lev)
levels(calcofi_hm$line) 

for (i in unique(calcofi_hm$species)) {
  nickname = substr(i, 1,4)
  filename = paste(nickname, "_heatmap", ".png", sep = "")
  p = ggplot(calcofi_hm[which(calcofi_hm$species == i),], aes(year, line)) +
    ggtitle(label = i) +
    geom_tile(aes(fill = mean_line_v_log))+
    scale_fill_gradient()+
    facet_wrap(~season)+
    theme_bw()+
    theme(legend.position = "bottom")
  png(filename = filename)
  print(p)
  dev.off()
}
unique(calcofi_hm$species)
