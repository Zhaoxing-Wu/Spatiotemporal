library(data.table)
library(dplyr)
library(lubridate)
library(geosphere) # For distance calculation
library(sp)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
library(geodata)
library(raster)
library(ggplot2)
library(stringr)

#################files##########################################################
folder_path1 <- "data/iintid_output"  # Phase 1, 2
folder_path2 <- "data/iintid_output_0512" # 2025 phase3
if (!dir.exists(folder_path1)||!dir.exists(folder_path2)) {stop("The folder path does not exist. Please check the path and try again.")}
files = union(sort(list.files(path = folder_path1)),
              sort(list.files(path = folder_path2)))# Sort the file names alphabetically
sorted_files = setdiff(files, c("51381.csv", "70880.csv", "71803.csv")) # should be of length 204, remove people with no records

#########################functions##############################################
data_obtain = function(file_no, folder_path1, col_drop = c("GPSUid", "StudyId", "UserId", "DeviceId", "Altitude", "Accuracy", 
                                                          "Bearing", "Sex", "IsUrbanOrRuralName", "Speed")){
  df = fread(paste0(folder_path1,"/", file_no), drop = col_drop)
  df <- as.data.table(df) # (Much Faster than dplyr)
  df =df[df$Provider %in% c('gps')]
  df = na.omit(df) 
  return(df)
}

data_sort = function(file_no, folder_path1, folder_path2){
  df1 = data_obtain(file_no, folder_path1)
  df2 = data_obtain(file_no, folder_path2)
  df <- unique(rbind(df1, df2))
  df[, TimeUTC := as.POSIXct(RecordTimeLocal, format = "%Y/%m/%d %H:%M:%OS", tz = "UTC")]
  df[, isSatelliteRecordTime := SatelliteTimeLocal == as.Date(RecordTimeLocal)]
  setorder(df, TimeUTC)
  df[, c("RecordTimeLocal", "Provider", "SatelliteTimeLocal", "Age") := NULL]
  return(df)
}
#########################data combined##########################################
dt_combined <- data.table(IIntID = integer(), Latitude= numeric(), Longitude = numeric(),         
                          TimeUTC = as.POSIXct(character()), isSatelliteRecordTime = logical())
for (file_no in sorted_files){
  ##file_no = "85517.csv"
  print(file_no)
  dt = data_sort(file_no, folder_path1, folder_path2)
  dt_combined <- rbind(dt_combined, dt[,Age := NULL])
}
#saveRDS(dt_combined, file = "data/iintid_NAremoved_GPS_sortedT_LatLonSate.rds")
#iintid_NAremoved42998067_GPS_sortedT_LatLonSate.rds
#########################data demo##############################################
df_demo2 <- data.frame(IIntID = numeric(), Age = numeric(), Sex = character(), 
                       IsUrbanOrRuralName = character(), GPSUid = character(), 
                       StudyId = numeric(), UserId = numeric(), DeviceId = character(), 
                       RecordTimeLocal = character(), Accuracy = numeric(), 
                       Altitude = numeric(), Bearing = numeric(), 
                       Latitude = numeric(), Longitude = numeric(), Provider = character(),
                       SatelliteTimeLocal = character(), Speed = numeric())
for (file_no in sorted_files){
  df_demo2 <- rbind(df_demo2, read.csv(paste0(folder_path1,"/", file_no), nrows = 1, header = TRUE, sep=";"))
}
df_demo2 = df_demo2 %>%
  dplyr::select(-c("Age", "StudyId", "RecordTimeLocal", "Accuracy", "Altitude", "Bearing", 
            "Latitude", "Longitude", "Provider", "SatelliteTimeLocal", "Speed"))%>%
  left_join(as.data.frame(dt_demo), by="IIntID")
#saveRDS(df_demo2, file = "raw_data/iintid_demographics.rds")

######################data age##################################################
dt_age<- data.table(IIntID = integer(), Age= integer(), total_time_age = numeric())
dt_range <- data.table(IIntID = integer(), Source = character(), StartTime = as.POSIXct(character()), EndTime = as.POSIXct(character()))
for (file_no in sorted_files){
  print(file_no)
  col_drop = c("GPSUid", "StudyId", "UserId", "DeviceId", "Altitude", "Accuracy","SatelliteTimeLocal", 
                             "Bearing", "Sex", "IsUrbanOrRuralName", "Speed", "Longitude", "Latitude")
  df1 = data_obtain(file_no, folder_path1, col_drop)
  df2 = data_obtain(file_no, folder_path2, col_drop)
  
  df1[, TimeUTC := as.POSIXct(RecordTimeLocal, format = "%Y/%m/%d %H:%M:%OS", tz = "UTC")]
  dt_range <- rbind(dt_range, data.table(
      IIntID = df1$IIntID[1],
      Source = "df1",
      StartTime = min(df1$TimeUTC, na.rm = TRUE),
      EndTime = max(df1$TimeUTC, na.rm = TRUE)
  ))
  df2[, TimeUTC := as.POSIXct(RecordTimeLocal, format = "%Y/%m/%d %H:%M:%OS", tz = "UTC")]
  dt_range <- rbind(dt_range, data.table(
      IIntID = df2$IIntID[1],
      Source = "df2",
      StartTime = min(df2$TimeUTC, na.rm = TRUE),
      EndTime = max(df2$TimeUTC, na.rm = TRUE)
  ))
  df <- unique(rbind(df1, df2))
  setorder(df, TimeUTC)
  
  df = df%>%
    mutate(TimeInterval = as.numeric(difftime(TimeUTC, lag(TimeUTC), units = "secs"))) %>%
    filter(TimeInterval <= 60*30) %>% #remove intervals >30min
    group_by(Age)%>%
    mutate(total_time_age = sum(TimeInterval, na.rm=TRUE)) %>%
    slice(1) %>%
    dplyr::select(c("IIntID", "Age", "total_time_age"))
  df$total_time_age = df$total_time_age/sum(df$total_time_age)
  dt_age <- rbind(dt_age, df)
}
#saveRDS(dt_age, file = "data/age.rds") #iintid_age
#saveRDS(dt_range, file = "data/range.rds") #iintid_date_range

#####################grid cell matching#########################################
sa_border <- ne_countries(country = "South Africa", returnclass = "sf")$geometry
lat_min <- min(sa_border[[1]][[1]][[1]][,2])
lat_max <- max(sa_border[[1]][[1]][[1]][,2])
lon_min <- min(sa_border[[1]][[1]][[1]][,1])
lon_max <- max(sa_border[[1]][[1]][[1]][,1])

load("before estimating prevalance 100 meters corrected/RSAPrevalenceShapefile.Rdata")
load("before estimating prevalance 100 meters corrected/LocalPrevalenceEstimates2022.RData")
wkbl_path <- "45 WKBLs_DSA/DSA_DKD_region.shp"
wkbl_shape <- st_read(wkbl_path)
wkbl_shape <- st_zm(wkbl_shape, drop = TRUE)  # Remove Z dimension
wkbl_shape = st_transform(wkbl_shape, crs = st_crs(south_africa_shape))
crs = CRS("+init=epsg:4326")#which ggplot use by default

##--------------Grid of calculating prevalance----------------------------------
source('auxi_funcs.R')
border = 0.02
meter_set = 100
grid_sp <- grid_in_meters(wkbl_shape, border, meter_set, crs)
grid_sf <- st_as_sf(grid_sp)
grid_sf$grid_id = 1:nrow(grid_sf)
grid_centroids <- coordinates(grid_sp)  # matrix: [n_cells, 2], columns: Lon, Lat
colnames(grid_centroids) <- c("x_center", "y_center")
grid_center_need <- as.matrix(grid_centroids) 
rownames(grid_center_need) = c(1:nrow(grid_center_need))

order_grid_for_rec = order(grid_centroids[,2], grid_centroids[,1])
ordered_centroids <- grid_centroids[order_grid_for_rec, ]
ordered_centroids = round(ordered_centroids,5)
grid_center_need_ordered <- as.matrix(ordered_centroids)
rownames(grid_center_need_ordered) = c(1:nrow(grid_center_need))

##--------------join data with grid cells---------------------------------------
df_combined = readRDS("data/iintid_NAremoved42998067_GPS_sortedT_LatLonSate.rds")
xy <- df_combined[, c("Longitude", "Latitude")] 
geodf_sf <- st_as_sf(xy, coords = c("Longitude", "Latitude"), crs = crs)
geodf_sp <- SpatialPointsDataFrame(coords= xy,data=xy)
proj4string(geodf_sp) <-crs
res <- st_join(geodf_sf, grid_sf)$grid_id
##---------------add if inside contour------------------------------------------
crs_use <- st_crs(wkbl_shape)
grid_sf  <- st_transform(grid_sf, crs_use)
grid_sf$grid_id <- seq_len(nrow(grid_sf))

center_sf <- st_as_sf(data.frame(grid_id  = grid_sf$grid_id,
                                 x_center = grid_center_need[, 1],
                                 y_center = grid_center_need[, 2]),
                      coords  = c("x_center", "y_center"),
                      crs     = crs_use, remove  = FALSE)
center_sf$inside_contour <-lengths(st_within(center_sf, wkbl_shape)) > 0  
pts_sf <- st_as_sf(df_combined, coords  = c("Longitude", "Latitude"),
                   crs = crs_use, remove  = FALSE)              
pts_sf <- st_join(pts_sf, grid_sf["grid_id"], left = TRUE)
pts_sf <- pts_sf %>%
  left_join(st_drop_geometry(center_sf), by = "grid_id") %>%
  rename(center_longitude  = x_center,
         center_latitude   = y_center,
         if_inside_contour = inside_contour)
  # if_inside_contour
  #             false: the point is inside the wkbl boundary box but outside the contour
  #             na: the point is outside the wkbl boundary box

#saveRDS(pts_sf, file = "data/iintid_grid.rds")
#data/iintid_grid_sf.rds

pts_df <- pts_sf %>%
  st_drop_geometry() %>%  # removes the geometry column
  dplyr::select(IIntID, Latitude, Longitude, TimeUTC, isSatelliteRecordTime, grid_id, if_inside_contour)
#saveRDS(pts_df, file = "data/iintid_grid_df.rds")

######################grid with prevalence######################################
pts_sf = readRDS("data/iintid_grid_sf.rds")
dat <- pts_sf
dat$if_inside_contour = dat$grid_id %in% insideGridCells$grid_id    # make FALSE instead of NA
pts_sf <- dat          # if pts_sf is an sf object, this keeps geometry intact
result_df <- st_drop_geometry(pts_sf)  

crs_use <- st_crs(wkbl_shape)
south_africa_shape <- st_transform(south_africa_shape, crs_use)
pts_with_prev <- st_join(
  pts_sf,                              # geometry + all original cols
  south_africa_shape["prevEstimate"],  # just this attribute
  left = TRUE                          # keep all points
)
index_more = rep(0, nrow(grid_sf))
for (i in 1:length(insideGridCells$grid_id)){
  index_more[insideGridCells$grid_id[i]] = i
}

pts_with_prev$prev_esti = -1
index_inside = which(pts_with_prev$if_inside_contour==TRUE)

pts_with_prev$prev_esti[index_inside] = insideGridCells$prev_esti[index_more[pts_with_prev$grid_id[index_inside]]]

prev_esti <- ifelse(
  pts_with_prev$if_inside_contour,     # TRUE → inside contour
  pts_with_prev$prev_esti,                                  # assign -1
  pts_with_prev$prevEstimate           # otherwise use SA estimate (may be NA)
)
pts_with_prev$prev_esti=prev_esti
pts_prev = pts_with_prev%>%
  st_drop_geometry() %>%
  dplyr::select(IIntID, Latitude, Longitude, TimeUTC, isSatelliteRecordTime, 
                grid_id, if_inside_contour, prev_esti)
saveRDS(pts_prev, file = "data/iintid_prev.rds")
