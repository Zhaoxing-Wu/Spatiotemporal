library(data.table)
library(dplyr)
library(lubridate)
library(broom)
library(dplyr)
library(purrr)
library(ggplot2)
library(ggcorrplot)
#source('auxi_funcs.R')

############find time interval on raw data for gap identification###############
############output time interval file###########################################
t_int_list_exist = TRUE
if (!t_int_list_exist){
  folder_path1 <- "iintid_output"  # Phase 1
  folder_path2 <- "iintid_output_0512"
  files1 <- list.files(path = folder_path1)
  files2 <- list.files(path = folder_path2)
  sorted_files1 <- sort(files1) # Sort the file names alphabetically
  sorted_files2 = sort(files2)
  sorted_files = union(sorted_files1,sorted_files2)
  t_int_day = c()
  t_int_night = c()
  t_int_wkday = c()
  t_int_wkend = c()
  for (file_no in sorted_files) {
    #file_no = "4277.csv"
    df = data_sort(file_no)
    df <- df %>%
      mutate(
        RecordTimeParsed = ymd_hms(str_sub(RecordTimeLocal, 1, 19)), # cut extra decimals
        Weekday = (wday(RecordTimeParsed) + 5) %% 7 + 1,             # 1 = Monday ... 7 = Sunday
        TimeOfDay = hour(RecordTimeParsed)/24 + 
          minute(RecordTimeParsed)/(24*60) + 
          second(RecordTimeParsed)/(24*3600),
        WeekdayTF = Weekday >= 1 & Weekday <= 5,                     # Monday–Friday
        Daytime = hour(RecordTimeParsed) >= 7 & hour(RecordTimeParsed) < 19, # 7AM–7PM
        Date = as.Date(RecordTimeParsed))%>%
      arrange(RecordTimeParsed)%>%
      mutate(TimeInterval_btw_obs = as.numeric(difftime(RecordTimeParsed, lag(RecordTimeParsed), units = "secs")))
  
    t_int_day   <- c(t_int_day, df$TimeInterval_btw_obs[df$Daytime])
    t_int_night <- c(t_int_night, df$TimeInterval_btw_obs[!df$Daytime])
    t_int_wkday <- c(t_int_wkday, df$TimeInterval_btw_obs[df$WeekdayTF])
    t_int_wkend <- c(t_int_wkend, df$TimeInterval_btw_obs[!df$WeekdayTF])
    print(file_no)
  }

  # Save as .rds file
  t_int_list <- list(day   = t_int_day, night = t_int_night, wkday = t_int_wkday, wkend = t_int_wkend)
  saveRDS(t_int_list, file = "time_interval_data.rds")
}
t_day   <- t_int_day[!is.na(t_int_day) & t_int_day <= 60*60*12 & t_int_day>=0]
t_night <- t_int_night[!is.na(t_int_night) & t_int_night <= 60*60*12& t_int_night>=0]
t_wkday <- t_int_wkday[!is.na(t_int_wkday) & t_int_wkday <= 60*60*12& t_int_wkday>=0]
t_wkend <- t_int_wkend[!is.na(t_int_wkend) & t_int_wkend <= 60*60*12& t_int_wkend>=0]

###############plot histogram###################################################
t_int_list = readRDS("time_interval_data.rds")
t = c(t_int_list$day, t_int_list$night, t_int_list$wday, t_int_list$wkend)
t = t[!is.na(t) & t >= 60*10 & t <= 60*60*2] # remove na, <10min, >2h
df <- data.frame(x = t)

# Plot histogram with theme_bw
ggplot(df, aes(x = x)) +
  geom_density(fill = "grey70", color = "black") +
  scale_x_continuous(
    breaks = c(600, 1800, 3600,5400, 7200),  # 10 min, 30 min, 1 hr, 2 hr
    labels = c("10min", "30min", "1hr","1.5hr", "2hr")
  ) +
  labs(
    x = "Gaps",       # Change x-axis title
    y = "Density"     # Change y-axis title
  )+
  theme_bw()

################plot histogram for different time ranges########################
t_int_cleaned <- lapply(t_int_list, function(t) {
  t = t[!is.na(t) & t >= 0 & t <= 60*60*12] # remove NA, negative, >12h values
  n <- length(t)
  range <- c(0, 6, 5*60, 10*60, 20*60, 40*60, 5*60*60)
  labels <- c("[0sec,6sec)", "[6sec,5min)", "[5min,10min)", "[10min,20min)", "[20min,40min)", "[40min,5h)")
  bins <- cut(t, breaks = range, right = FALSE, labels = labels)# Cut the data into bins
  percentage <- round(prop.table(table(bins)) * 100, 2)
  
  par(mfrow = c(3, 2))
  for (i in 1:(length(range) - 1)) {
    t_sub <- t[t >= range[i] & t < range[i + 1]]
    h <- hist(t_sub, plot = FALSE)
    x_ticks <- h$mids
    if (i==1||i==2){
      x_labels <- x_ticks  # from 5min offset
      x_lab = "secs"
    }else if (i==3||i==4||i==5){
      x_labels <- round((x_ticks) / 60, 1)  # from 5min offset
      x_lab = "min"
    }else if (i==6){
      x_labels <- round((x_ticks) / 60/60, 1)  # from 5min offset
      x_lab = "hr"
    }
    plot(h, main = paste(labels[i], "(", percentage[i], "%)"), 
        ylab = "Frequency", ylim = c(0, max(h$counts) * 1.2), xaxt = "n", xlab = x_lab)
    axis(1, at = x_ticks, labels = x_labels)
    # Add labels on top of each bar
    nonzero_idx <- h$counts > 0
    text(x = h$mids[nonzero_idx],
         y = h$counts[nonzero_idx],
         labels = h$counts[nonzero_idx],
         pos = 3, cex = 0.8, col = "blue")
  }
  
})

#####################distances within gaps######################################
# Define function to compute proportions for a given df
compute_gap_proportions <- function(df2, gap_labels) {
  gap_filters <- list(
    gap1s   = which(df2$TimeInterval >= 0     & df2$TimeInterval < 60),
    gap1min = which(df2$TimeInterval >= 60*1  & df2$TimeInterval < 60*5),
    gap5min = which(df2$TimeInterval >= 60*5  & df2$TimeInterval < 60*10),
    gap10min= which(df2$TimeInterval >= 60*10 & df2$TimeInterval < 60*20),
    gap20min= which(df2$TimeInterval >= 60*20 & df2$TimeInterval < 60*40),
    gap40min= which(df2$TimeInterval >= 60*40 & df2$TimeInterval < 60*60),
    gap60min= which(df2$TimeInterval >= 60*60 & df2$TimeInterval < 60*60*5)
  )
  
  gap_distances <- list()
  
  for (gap_name in names(gap_filters)) {
    #gap_name = "gap60min"
    indices <- gap_filters[[gap_name]]
    
    gap_distances[[gap_name]] <- data.frame(
      time = df2$TimeUTC[indices],
      time_interval = df2$TimeInterval[indices],
      distance_m = df2$Distance[indices]
    )
  }
  
  # Calculate proportions
  lapply(names(gap_distances), function(name) {
    n_large <- sum(gap_distances[[name]]$distance_m > sqrt(2)*500)
    n_total <- nrow(gap_distances[[name]])
    c(n_large, n_total)
  })
}


# Define gap labels
gap_labels <- c(
  gap1s="1s–60s", gap1min="1min–5min", gap5min="5min–10min",
  gap10min="10min–20min", gap20min="20min–40min",
  gap40min="40min–1hr", gap60min="1hr–5hr"
)

# Run calculations
#id = unique(df2$IIntID)
#i=2
df_day <- df2[df2$isDaytime & df2$WeekdayTF, ]# & df2$IIntID ==id[i]
df_night <- df2[!df2$isDaytime & df2$WeekdayTF, ]#& df2$IIntID ==id[i]
ret_day   <- compute_gap_proportions(df_day, gap_labels)
ret_night <- compute_gap_proportions(df_night, gap_labels)

# Print results
for (i in 1:3) {
  cat(paste0(gap_labels[i], "\n"))
  cat("Daytime:   ", round(ret_day[[i]][1]/ ret_day[[i]][2] * 100 , 5), "%\n")#
  cat("Nighttime: ", round(ret_night[[i]][1]/ ret_night[[i]][2] * 100, 5), "%\n\n")# 
}

# Combined >10min
for (label in c("Daytime", "Nighttime")) {
  ret <- if (label == "Daytime") ret_day else ret_night
  perc <- sum(sapply(ret[4:7], function(x) x[1])/sum(sapply(ret[4:7], function(x) x[2])))#
  cat(paste0(">10min ", label, ": ", round(perc * 100, 5), "%\n"))
}

##########
gap_filters <- list(
  gap1s  = which(df2$TimeInterval >= 0   & df2$TimeInterval < 60),
  gap1min  = which(df2$TimeInterval >= 60*1   & df2$TimeInterval < 60*5),
  gap5min  = which(df2$TimeInterval >= 60*5   & df2$TimeInterval < 60*10),
  gap10min = which(df2$TimeInterval >= 60*10  & df2$TimeInterval < 60*20),
  gap20min = which(df2$TimeInterval >= 60*20  & df2$TimeInterval < 60*40),
  gap40min = which(df2$TimeInterval >= 60*40  & df2$TimeInterval < 60*60),
  gap60min = which(df2$TimeInterval >= 60*40  & df2$TimeInterval < 60*60*5)
)

# Store distances per group
gap_distances <- list()

# Loop over each group
for (gap_name in names(gap_filters)) {
  indices <- gap_filters[[gap_name]]
  
  # Remove first-row indices per IIntID
  valid_indices <- setdiff(indices, index_IIntID)
  
  # Remove index 1 (to safely access previous row)
  valid_indices <- valid_indices[valid_indices > 1]
  
  # Coordinates before and after gap
  lat1 <- df2$Latitude[valid_indices - 1]
  lon1 <- df2$Longitude[valid_indices - 1]
  lat2 <- df2$Latitude[valid_indices]
  lon2 <- df2$Longitude[valid_indices]
  
  # Calculate Haversine distances
  distances <- distHaversine(cbind(lon1, lat1), cbind(lon2, lat2))
  speed = distances/df2$TimeInterval[valid_indices]
  isSpeedNormal = speed<=40
  isDistance = distances>0
  # Store result
  gap_distances[[gap_name]] <- data.frame(index = valid_indices[isSpeedNormal&isDistance], 
                                          distance_m = distances[isSpeedNormal&isDistance])
}
gap_labels <- c(
  gap1s="1s-60s",
  gap1min  = "1min–5 min",
  gap5min  = "5min–10 min",
  gap10min = "10min–20 min",
  gap20min = "20min–40 min",
  gap40min = "40min–1hr",
  gap60min = "1hr–5hr"
)

ret = lapply(names(gap_distances), function(name) {
  c(sum(gap_distances[[name]]$distance_m>sqrt(2)*200), nrow(gap_distances[[name]]))
})

print(paste0(gap_labels[1],": ", round(ret[[1]][1]/ret[[1]][2]*100, 5), "%"))
print(paste0(gap_labels[2],": ", round(ret[[2]][1]/ret[[2]][2]*100, 5), "%"))
print(paste0(gap_labels[3],": ", round(ret[[3]][1]/ret[[3]][2]*100, 5), "%"))
print(paste0(">10min: ", round((ret[[4]][1]/ret[[4]][2]+ ret[[5]][1]/ret[[5]][2]+ 
                                  ret[[6]][1]/ret[[6]][2]+ ret[[7]][1]/ret[[7]][2])
                               *100, 5), "%"))


###plot boxplot

df_distances_all <- bind_rows(
  lapply(names(gap_distances), function(name) {
    df <- gap_distances[[name]]
    
    if (nrow(df) > 0) {
      q3 <- quantile(df$distance_m, 0.75, na.rm = TRUE) #filter by quantile
      df_filtered <- df %>%
        filter(distance_m > q3) %>%
        mutate(gap_type = name)
      return(df_filtered)
    } else {
      return(NULL)  # Skip empty groups
    }
  })
)

# Make a boxplot
ggplot(df_distances_all, aes(x = gap_type, y = distance_m)) +
  geom_boxplot(fill = "skyblue", color = "darkblue") +
  labs(title = "Distance Distributions by Time Gaps",
       x = "Time Gap Type",
       y = "Distance (meters)") +
  ylim(0, 100000)+
  scale_x_discrete(labels = gap_labels) +
  theme_minimal()




