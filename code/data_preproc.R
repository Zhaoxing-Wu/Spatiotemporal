library(data.table)
library(dplyr)
library(lubridate)
library(geosphere) # For distance calculation
library(sp)
library(sf)
library(geodata)
library(raster)
library(ggplot2)
library(stringr)
library(tidyr)

df = readRDS("data/iintid_prev.rds")
df_demo=readRDS("data/iintid_demographics.rds")
df_age = readRDS("data/iintid_age.rds")

##----get a summary table, include some stats before preprocessing--------------
df_ret = df %>%
  mutate(Date = as.Date(TimeUTC)) %>%
  group_by(IIntID) %>%
  summarise(n_rows_before = n(),# number of days before preprocessing (after remove na and keep only gps)
            n_days_before = n_distinct(Date))%>% # not related to time intervals recorded
  right_join(df_age, by="IIntID") %>%
  group_by(IIntID) %>%
  slice_max(total_time_age, n = 1, with_ties = FALSE)%>%
  ungroup()%>%
  mutate(Age_group = case_when(Age >= 20 & Age <= 26 ~ "Age 20-26",
                               Age >= 27 & Age <= 34 ~ "Age 27-34"))%>%
  right_join(df_demo, by="IIntID")%>%
  dplyr::select(IIntID,Age.x, Age_group, n_rows_before, n_days_before, Sex)
rm(df_age, df_demo)

##--------preprocessing---------------------------------------------------------
# Step 1: remove outliers
df <- df %>%
  filter(!is.na(prev_esti)) %>% #1) remove pts outside of south africa
  group_by(IIntID) %>%
  mutate(
    isDaytime = hour(TimeUTC) > 6 & hour(TimeUTC) < 19, 
    Distance = distHaversine(cbind(Longitude, Latitude), cbind(lag(Longitude), lag(Latitude))), #unit: m
    TimeInterval = as.numeric(difftime(TimeUTC, lag(TimeUTC), units = "secs")),
    TimeInterval = ifelse(is.na(TimeInterval), 0, TimeInterval), # unit: secs
    Speed = Distance / TimeInterval, #unit: m/s, dist/0 = inf
    Speed = ifelse(is.na(Speed), 0, Speed),
    Date = as.Date(TimeUTC),
    isWeekday = !(weekdays(TimeUTC) %in% c("Saturday", "Sunday")),
    blockID = cumsum(TimeInterval > 2*60),#gap 2 minutes for outlier removal
  ) %>%
  ungroup() %>%
  group_by(blockID, IIntID) %>%
  filter(n() != 1, #2) remove blk (2min) with only 1 obs
         row_number() != 1, #3) remove 1st obs in a blk
         is.finite(Speed) & Speed<=15, #4) remove speed>15
         !(TimeInterval > 3 & Speed > 7),
         !(duplicated(TimeUTC))) #5) remove pts at duplicated timestamps
         #TimeInterval > 0.8) #6) remove pts too close in time

#Step 2: handle gaps, recalculate speed, distance & time intervals
df <- df %>% 
  group_by(IIntID) %>% #recalculate timeinterval after remove outliers
  mutate(TimeInterval = as.numeric(difftime(TimeUTC, lag(TimeUTC), units = "secs")),
         TimeInterval = ifelse(is.na(TimeInterval), 0, TimeInterval)
         )%>%
  ungroup() %>%
  mutate(blockID = cumsum(TimeInterval > 30*60)) %>%#gap 30 minutes 
  group_by(blockID, IIntID) %>%
  mutate(# first timeinterval in each blk: 0
    TimeInterval = as.numeric(difftime(TimeUTC, lag(TimeUTC), units = "secs")),
    TimeInterval = ifelse(is.na(TimeInterval), 0, TimeInterval),
    Distance = distHaversine(cbind(Longitude, Latitude), cbind(lag(Longitude), lag(Latitude))), #unit: m
    Distance = ifelse(is.na(Distance), 0, Distance),
    Speed = Distance / TimeInterval, #unit: m/s, dist/0 = inf
    Speed = ifelse(is.na(Speed), 0, Speed),) %>%
  ungroup()

#Step 3: handle grids outside contour (assign negative grid_id based on district)
# pts outside SA: is.na(df$prev_esti)
# pts outside contour bbox: is.na(grid_id)
# pts outside contour: !df$if_inside_contour
unique_prev_esti <- unique(df[!df$if_inside_contour, "prev_esti"])$prev_esti
for (i in 1:length(unique_prev_esti)){
  df$grid_id[df$prev_esti==unique_prev_esti[i]& !df$if_inside_contour] = -i
}
anyNA(df)

##----table: outside contour (grid id & district name)--------------------------
crs = CRS("+init=epsg:4326")
load("before estimating prevalance 100 meters corrected/RSAPrevalenceShapefile.Rdata")
df_outside_contour = df %>%
  filter(!if_inside_contour) %>%
  st_as_sf(coords = c("Longitude", "Latitude"), crs = crs) %>%
  st_join(st_transform(south_africa_shape, crs = crs), 
          join = st_within) %>%
  dplyr::select("grid_id", "NAME_2")%>%
  st_drop_geometry()%>%
  distinct()
rm(crs, south_africa_shape, i, unique_prev_esti)

##-----------table: prevalence for each IIntID & grid_id------------------------
# for calculating contextual exposure: only need time spent in each grid, don't need distance
df_prev = df %>%
  group_by(IIntID) %>%
  mutate(
    # ith grid != (i-1)th grid: 0 for ith grid
    # first in each block is na
    # time and distance traveled between grids are ignored
    # different from the gpssummary table: only ignore between contour
    TimeInterval_grid = ifelse(grid_id != lag(grid_id), 0, TimeInterval),
    TimeInterval_grid = ifelse(is.na(TimeInterval_grid), 0, TimeInterval_grid), 
    Distance_grid = ifelse(grid_id != lag(grid_id), 0, Distance), 
    Distance_grid = ifelse(is.na(Distance_grid), 0, Distance_grid))%>%
  ungroup()%>%
  group_by(IIntID, grid_id)%>%
  mutate(time_inside_grid = sum(TimeInterval_grid),
         distance_inside_grid = sum(Distance_grid))%>%
  filter(time_inside_grid>0)%>%
  slice_head()%>%
  ungroup() %>%
  group_by(IIntID) %>%
  arrange(desc(time_inside_grid))%>%
  mutate(time = time_inside_grid/sum(time_inside_grid), #standardize time
         time_cum = cumsum(time))

thresholds <- seq(0.01, 1, by = 0.01)
