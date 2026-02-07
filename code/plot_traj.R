library(patchwork)

#df2 = df[df$isTimeUnique & df$isMultiInBlock & !df$isFirstInBlock & df$isSA&!df$isSpeedCommonLoc, ]
####################Custom plotting function####################################
plot_id_path <- function(data, id) {
  #id = "83334"#"83334", "99456", "82203"
  #speed_threshold <- 7
  df_plot <- data %>%
    filter(#TimeInterval >= 10,
      IIntID == id) %>%
    mutate(
      isDaytime = hour(TimeUTC) >7  & hour(TimeUTC) < 19, 
      lead_lat = lead(Latitude),
      lead_lon = lead(Longitude),
      lag_lat  = lag(Latitude),
      lag_lon  = lag(Longitude),
      dist_forward = distHaversine(cbind(Longitude, Latitude), cbind(lead_lon, lead_lat)),
      dist_backward = distHaversine(cbind(Longitude, Latitude), cbind(lag_lon, lag_lat)),
      time_forward = as.numeric(difftime(lead(TimeUTC), TimeUTC, units = "secs")),
      time_backward = as.numeric(difftime(TimeUTC, lag(TimeUTC), units = "secs")),
      speed_forward = dist_forward / time_forward,
      speed_backward = dist_backward / time_backward,
      Date = as.Date(TimeUTC)
    )
  #quantile90=quantile(df_plot$speed_backward[df_plot$speed_backward!=Inf], 0.9, na.rm=TRUE)
  #if (quantile90<=5){
  #  speed_threshold = quantile(df_plot$speed_backward[df_plot$speed_backward!=Inf], 0.95, na.rm=TRUE)
  #}else{
  #  speed_threshold = quantile(df_plot$speed_backward[df_plot$speed_backward!=Inf], 0.90, na.rm=TRUE)
  #}
  #speed_threshold = 10
  df_plot = df_plot%>%                # Ensure a Date column exists
    filter(Date %in% sort(unique(Date))[1:2])%>%
    #  mutate(
    #   is_fast_forward = speed_forward > speed_threshold,
    #   is_fast_backward = speed_backward > speed_threshold,
    ##   is_outlier = is_fast_forward | is_fast_backward | 
    #     lead(is_fast_backward, default = FALSE) | 
    #     lag(is_fast_forward, default = FALSE)
    # )%>%
    # filter(!is_outlier)%>%
    #filter(distance<10) %>%
    mutate(
      x = Longitude, y = Latitude,
      xend = lag(Longitude), yend = lag(Latitude), #previous element
    )%>%
    #filter(time_forward<600,
    #      time_backward<600)%>%
    filter(!is.na(xend) & !is.na(yend)&!isDaytime ) %>%
    mutate(
      x    = (x    - min(x))  / (max(x)    - min(x))-0.5,
      y    = (y    - min(y))  / (max(y)    - min(y))-0.5,
      xend = lag(x),
      yend = lag(y)
    ) #%>%
  #slice_head(n = 1000)
  
  #label_indices <- which(df_plot$distance>20)
  #label_points <- df_plot[label_indices, ]
  ggplot(df_plot) +
    geom_segment(aes(x = x, y = y, xend = xend, yend = yend),
                 color = "grey", linewidth = 0.3) +
    geom_point(aes(x = x, y = y), color = "black", size = 0.1) +
    geom_point(aes(x = xend, y = yend), color = "black", size = 1) +#o/w the last point is missing
    #geom_point(data = label_points, aes(x = xend, y = yend), color = "blue", size = 2)+
    theme_bw() +
    labs(
      #title = paste0("# days: ", length(unique(df_plot$Date))),
      x = "Centered Longitude", y = "Centered Latitude"
    )
}
#################plot trajectory for 3 unique individuals#######################
ids <- c("85478", "88063", "52546")
p1 <- plot_id_path(df, ids[1])
p2 <- plot_id_path(df, ids[2])
p3 <- plot_id_path(df, ids[3])
(p1 | p2 | p3)

##################plot trajectory for all#######################################
ids = unique(df$IIntID)
for (i in ids){
  ggsave(
    filename = paste0("plot/traj_", i,"_2days.png"),
    plot = plot_id_path(df, i),           # optional if it's the last plot
    width = 8,                # in inches
    height = 6,
    dpi = 300                 # resolution
  )
}