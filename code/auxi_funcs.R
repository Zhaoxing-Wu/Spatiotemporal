### create grid on wkbl_shape by input grid size
grid_old <- function(wkbl_shape, border, size, output="sf"){
  bbox <- st_bbox(wkbl_shape)
  x <- seq(from = bbox["xmin"] - border, 
           to = bbox["xmax"] + border, by = size)
  y <- seq(from = bbox["ymin"] - border, 
           to = bbox["ymax"] + border, by = size)
  xy <- expand.grid(x = x, y = y)
  
  if (output=="sf"){
    grid_sf_points <- st_as_sf(xy, coords = c("x", "y"),
                               crs = crs)
    grid_polygons <- st_make_grid(grid_sf_points, 
                                  cellsize = c(size, size), 
                                  what = "polygons")
    grid_sf <- st_sf(geometry = grid_polygons,  crs = crs)
    return (grid_sf)
  }else{
    grid.pts <- SpatialPointsDataFrame(coords= xy, data=xy, 
                                       proj4string = crs)
    gridded(grid.pts) <- TRUE
    grid_sp <- as(grid.pts,"SpatialPolygons")
    return (grid_sp)
  }
}

### create grid on wkbl_shape by input grid size
grid <- function(wkbl_shape, border, size){
  bbox <- st_bbox(wkbl_shape)
  x <- seq(from = bbox["xmin"] - border, 
           to = bbox["xmax"] + border, by = size)
  y <- seq(from = bbox["ymin"] - border, 
           to = bbox["ymax"] + border, by = size)
  xy <- expand.grid(x = x, y = y)
  
  
  grid.pts <- SpatialPointsDataFrame(coords= xy, data=xy, 
                                     proj4string = crs)
  gridded(grid.pts) <- TRUE
  grid_sp <- as(grid.pts,"SpatialPolygons")
  return (grid_sp)
  
}

grid_in_meters <- function(wkbl_shape, border, meter_set, crs) {
  # 1) Get bounding box
  bbox <- st_bbox(wkbl_shape)
  
  # 2) Approximate degree-per-meter at the bounding box's center latitude
  lat_center <- (bbox["ymin"] + bbox["ymax"]) / 2
  
  # ~111,319.9 m per degree of latitude
  deg_per_meter_lat <- 1 / 111319.9 
  
  # ~111,319.9 * cos(latitude) m per degree of longitude
  deg_per_meter_lon <- 1 / (cos(lat_center * pi / 180) * 111319.9)
  
  # 3) Convert meter_set to "degree" increments
  dx <- meter_set * deg_per_meter_lon  # step size in longitude degrees
  dy <- meter_set * deg_per_meter_lat  # step size in latitude degrees
  
  # 4) Build sequences of grid lines in lon/lat
  x_seq <- seq(from = bbox["xmin"] - border,
               to   = bbox["xmax"] + border,
               by   = dx)
  y_seq <- seq(from = bbox["ymin"] - border,
               to   = bbox["ymax"] + border,
               by   = dy)
  
  # 5) Expand into full grid points
  xy <- expand.grid(x = x_seq, y = y_seq)
  
  # 6) Create SpatialPointsDataFrame, then coerce to SpatialPolygons
  grid.pts <- SpatialPointsDataFrame(coords = xy, data = xy, proj4string = crs)
  gridded(grid.pts) <- TRUE
  grid_sp <- as(grid.pts, "SpatialPolygons")
  
  return(grid_sp)
}

library(stringr)

filter_by_date_and_time <- function(df2, date_select, time_select) {
  df2 <- df2 %>%
    mutate(
      RecordTimeParsed = ymd_hms(str_sub(RecordTimeLocal, 1, 19)), # cut extra decimals
      Weekday = (wday(RecordTimeParsed) + 5) %% 7 + 1,             # 1 = Monday ... 7 = Sunday
      TimeOfDay = hour(RecordTimeParsed)/24 + minute(RecordTimeParsed)/(24*60) +
        second(RecordTimeParsed)/(24*3600)
    ) %>%
    filter(
      Weekday %in% date_select,
      TimeOfDay >= time_select[1],
      TimeOfDay <= time_select[2]
    )
  
  return(df2)
}


## Anchor points
find_local_maxima_discrete <- function(grid_center_need, density_values) {
  #### 1) Check input consistency
  if (nrow(grid_center_need) != length(density_values)) {
    stop("The number of density values must match the number of grid points.")
  }
  
  # Extract unique sorted x and y
  x_unique <- sort(unique(grid_center_need[, 1]))
  y_unique <- sort(unique(grid_center_need[, 2]))
  nx <- length(x_unique)
  ny <- length(y_unique)
  
  if (nx * ny != nrow(grid_center_need)) {
    stop("grid_center_need does not form a complete rectangular grid. 
Please check or consider an interpolation-based approach.")
  }
  
  #### 2) Sort by (x,y) and reshape
  ord <- order(grid_center_need[,1], grid_center_need[,2])
  grid_sorted <- grid_center_need[ord, ]
  dens_sorted <- density_values[ord]
  
  # Reshape into a matrix fmat[i,j] where
  #   i indexes x_unique (length nx)
  #   j indexes y_unique (length ny)
  fmat <- matrix(dens_sorted, nrow = nx, ncol = ny, byrow = TRUE)
  indice_mat = matrix(ord, nrow = nx, ncol = ny, byrow = TRUE)
  
  ## --- 1.  prepare ----------------------------------------------------------------
  ## dimensions
  nx <- nrow(fmat); ny <- ncol(fmat)
  
  ## helper that returns a full -Inf matrix
  blank <- function() matrix(-Inf, nx, ny)
  
  ## build the eight neighbour matrices ---------------------------------
  nbr <- list(
    ul = blank(),  u  = blank(),  ur = blank(),
    l  = blank(),                 r  = blank(),
    dl = blank(),  d  = blank(),  dr = blank()
  )
  
  # fill each with the appropriate shift
  nbr$ul[2:nx,   2:ny   ] <- fmat[1:(nx-1), 1:(ny-1)]          # up-left
  nbr$u [2:nx,          ] <- fmat[1:(nx-1),           ]        # up
  nbr$ur[2:nx, 1:(ny-1) ] <- fmat[1:(nx-1), 2:ny     ]         # up-right
  nbr$l [     , 2:ny    ] <- fmat[        , 1:(ny-1) ]         # left
  nbr$r [     , 1:(ny-1)] <- fmat[        , 2:ny     ]         # right
  nbr$dl[1:(nx-1), 2:ny ] <- fmat[2:nx    , 1:(ny-1)]          # down-left
  nbr$d [1:(nx-1),       ]<- fmat[2:nx    ,          ]         # down
  nbr$dr[1:(nx-1),1:(ny-1)]<- fmat[2:nx  , 2:ny     ]          # down-right
  ## --- 3.  element-wise comparison ------------------------------------------------
  # TRUE where centre ≥ every neighbour
  is_local_max <- Reduce(`&`, lapply(nbr, function(m) fmat >= m))
  
  # drop the outer border because it has incomplete neighbourhoods
  is_local_max[c(1, nx), ] <- FALSE
  is_local_max[, c(1, ny)] <- FALSE
  
  ## --- 4.  collect results --------------------------------------------------------
  loc <- which(is_local_max, arr.ind = TRUE)
  
  local_maxima <- data.frame(
    ind     = indice_mat[loc],
    ix      = loc[,1],
    iy      = loc[,2],
    x_coord = x_unique[loc[,1]],
    y_coord = y_unique[loc[,2]],
    f_value = fmat[loc]
  )
  
  # local_maxima now contains exactly the same information the loop produced
  
  # Optionally, sort results by descending density
  local_maxima <- local_maxima[order(-local_maxima$f_value), ]
  
  #### 4) Return local maxima
  return(local_maxima)
}

## Anchor points
find_local_maxima_discrete2 <- function(grid_center_need, density_values) {
  #### 1) Check input consistency
  if (nrow(grid_center_need) != length(density_values)) {
    stop("The number of density values must match the number of grid points.")
  }
  
  # Extract unique sorted x and y
  x_unique <- sort(unique(grid_center_need[, 1]))
  y_unique <- sort(unique(grid_center_need[, 2]))
  nx <- length(x_unique)
  ny <- length(y_unique)
  
  if (nx * ny != nrow(grid_center_need)) {
    stop("grid_center_need does not form a complete rectangular grid. 
Please check or consider an interpolation-based approach.")
  }
  
  #### 2) Sort by (x,y) and reshape
  ord <- order(grid_center_need[,1], grid_center_need[,2])
  grid_sorted <- grid_center_need[ord, ]
  dens_sorted <- density_values[ord]
  
  # Reshape into a matrix fmat[i,j] where
  #   i indexes x_unique (length nx)
  #   j indexes y_unique (length ny)
  fmat <- matrix(dens_sorted, nrow = nx, ncol = ny, byrow = TRUE)
  indice_mat = matrix(ord, nrow = nx, ncol = ny, byrow = TRUE)
  
  #### 3) Find discrete local maxima by neighbor comparison
  local_maxima <- data.frame(
    ix      = integer(),
    iy      = integer(),
    x_coord = numeric(),
    y_coord = numeric(),
    f_value = numeric(),
    stringsAsFactors = FALSE
  )
  
  # We check only "interior" points [2..nx-1, 2..ny-1]
  # so each point has 8 neighbors. 
  # (You can adapt if you want to consider boundaries too.)
  
  for (i in 2:(nx-1)) {
    for (j in 2:(ny-1)) {
      center_val <- fmat[i, j]
      
      # Collect the 8 neighbors
      neighbors <- c(
        fmat[i-1, j-1], fmat[i-1, j], fmat[i-1, j+1],
        fmat[i,   j-1],                fmat[i,   j+1],
        fmat[i+1, j-1], fmat[i+1, j], fmat[i+1, j+1]
      )
      
      # Check if center_val >= all neighbors
      if (center_val >= max(neighbors)) {
        # This is a local max (in the discrete sense)
        local_maxima <- rbind(local_maxima, data.frame(
          ind = indice_mat[i,j],
          ix      = i,
          iy      = j,
          x_coord = x_unique[i],
          y_coord = y_unique[j],
          f_value = center_val
        ))
      }
    }
  }
  
  # Optionally, sort results by descending density
  local_maxima <- local_maxima[order(-local_maxima$f_value), ]
  
  #### 4) Return local maxima
  return(local_maxima)
}



data_obtain = function(file_no,folder_name){
  
  df_ahri <- fread(paste0(folder_name,file_no))
  
  df_ahri = na.omit(df_ahri) 
  df_ahri <- as.data.table(df_ahri) # (Much Faster than dplyr)
  df_ahri[, Outlier := !(Latitude >= lat_min & Latitude <= lat_max & 
                           Longitude >= lon_min & Longitude <= lon_max)]
  df = df_ahri[!df_ahri$Outlier,]
  
  df =df[df$Provider %in% c('gps')]
  return(df)
}

data_sort = function(file_no){
  df1 = data_obtain(file_no,'iintid_output/')
  df2 = data_obtain(file_no,'iintid_output_0512/')
  df_combined <- unique(rbind(df1, df2))
  df_combined$RecordTimeLocalUTC <- as.POSIXct(
    df_combined$RecordTimeLocal, 
    format = "%Y/%m/%d %H:%M:%OS", 
    tz = "UTC"
  )
  
  df_sorted <- df_combined[order(df_combined$RecordTimeLocalUTC), ]
  return(df_sorted)
}

data_sort3 = function(file_no){
  df1 = data_obtain(file_no,'iintid_output/')
  df2 = data_obtain(file_no,'iintid_output_0512/')
  if (nrow(df1)==0){
    df1 <- df2[0, ]
  }else if (nrow(df2)==0){
df2 <- df1[0, ]
  }
  df_combined <- unique(rbind(df1, df2))
  df_combined$RecordTimeLocalUTC <- as.POSIXct(
    df_combined$RecordTimeLocal, 
    format = "%Y/%m/%d %H:%M:%OS", 
    tz = "UTC"
  )
  
  df_sorted <- df_combined[order(df_combined$RecordTimeLocalUTC), ]
  return(df_sorted)
}


