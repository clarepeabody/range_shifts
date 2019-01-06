rm(list=ls())
#setwd
setwd("/Users/Clare/Documents/R/calcofi_ranges_19")

library(tidyverse)
options(stringsAsFactors = F)




#Read in merged datasets

calcofi <- read.csv("mex us culled stations.csv", stringsAsFactors = F)
str(calcofi)


#I. Clean data / georeference stations

###Note: For CalCOFI, the complete line name is used (e.g., 106.7).  For IMECOCAL they round (106.7 becomes 107). Also, single diget stations have been changed to double digets (e.g., 5 to 50)

#New variables
calcofi2 <-
  calcofi %>%
  mutate(S_L2 = as.character(round(as.numeric(S_L)))) %>%
  mutate(., lower = round(as.numeric(word(sampling_extent, 1)))) %>%
  mutate(., upper = round(as.numeric(word(sampling_extent, 5)))) %>%
  mutate(., line_station2 = paste(as.character(S_L2), as.character(S_S))) %>%
  mutate(., S_S2 = ifelse(nchar(as.character(calcofi$S_S)) == 1, paste(calcofi$S_S, 0, sep = ""), as.character(calcofi$S_S))) %>%
  select(., line_station2, S_S2, S_L2, latitude, longitude, season, month, year, lower, upper, Bathylagoides.wesethi, Ceratoscopelus.townsendi, Chauliodus.macouni, Diogenichthys.atlanticus, Leuroglossus.stilbius, Lipolagus.ochotensis, Protomyctophum.crockeri, Stenobrachius.leucopsarus, Symbolophorus.californiensis, Tarletonbeania.crenularis, Triphoturus.mexicanus)

#Calculate mean coordinates for unique stations
calcofi3 <-
  calcofi2 %>%
  mutate(ls = paste(S_L2, S_S2)) %>%
  group_by(ls) %>%
  mutate(lat = mean(latitude), long = mean(longitude)) %>%
  ungroup()

#Quick peek
stations <- 
  calcofi3 %>%
  select(lat, long, ls) %>%
  distinct()
ggplot(data = stations, aes(x = long, y = lat, label = ls))+
  geom_text(size = 2)




#II. Determine sampling frame for analysis

#Determine upper and lower sampling extent for each cruise-year
extent <-
  calcofi3 %>%
  select(year, season, lower, upper) %>%
  distinct(.) %>%
  gather(., "limit", "extent", 3:4)

#Quick peek
ggplot(extent)+
  geom_point(aes(year, extent, color = limit))+
  facet_wrap(~season)+
  scale_y_reverse()+
  ggtitle("N-S sampling extent of combined CalCOFI-IMECOCAL dataset, 1951-2017")+
  theme_bw()

#The "box" for analysis is determined as the modes of the upper and lower limits for cruises after 1976

#This is just a mode function for character objects
Mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

limits <-
  calcofi3 %>%
  group_by(season) %>%
  filter(year >= 1976) %>%
  select(season, upper, lower) %>%
  summarise(up = Mode(upper), lo = Mode(lower))

calcofi4 <-
  left_join(calcofi3, limits, by = "season")

#FILTERING RULES
###Remove *stations* north of (less than line #) cruise-season upper limit or south of (greater than line #) cruise-season lower limit
###Remove *cruise-years* with upper limit south of (greater than) cruise-season upper limit or lower limit north of (less than) cruise-season lower limit
str(calcofi4)
calcofi5 <-
  calcofi4 %>%
  mutate(line = as.numeric(S_L2)) %>%
  mutate(station = as.numeric(S_S2)) %>%
  filter(line >= up & line <= lo) %>%
  filter(upper <= up & lower >= lo) %>%
  select(line_station2, line, station, long, lat, longitude, latitude, season, month, year, Bathylagoides.wesethi, Ceratoscopelus.townsendi, Chauliodus.macouni, Diogenichthys.atlanticus, Leuroglossus.stilbius, Lipolagus.ochotensis, Protomyctophum.crockeri, Stenobrachius.leucopsarus, Symbolophorus.californiensis, Tarletonbeania.crenularis, Triphoturus.mexicanus)


colnames(calcofi5) <- c("line_station", "line", "station", "long", "lat", "longitude", "latitude", "season", "month", "year", "Bathylagoides wesethi", "Ceratoscopelus townsendi", "Chauliodus macouni", "Diogenichthys atlanticus", "Leuroglossus stilbius", "Lipolagus ochotensis", "Protomyctophum crockeri", "Stenobrachius leucopsarus", "Symbolophorus californiensis", "Tarletonbeania crenularis", "Triphoturus mexicanus")

write_csv(calcofi, "starting_point.csv")