source('data_preproc.R')
library(knitr)
library(kableExtra)

##########################table in the paper####################################
df_dist <- df %>%
  group_by(IIntID) %>%
  mutate(# ignore time interval/distance if move between contours
    TimeInterval_grid = ifelse(if_inside_contour != lag(if_inside_contour), 0, TimeInterval),
    TimeInterval_grid = ifelse(is.na(TimeInterval_grid), 0, TimeInterval_grid),
    Distance_grid = Distance,
    Distance_grid = ifelse(if_inside_contour != lag(if_inside_contour), 0, Distance_grid), 
    Distance_grid = ifelse(is.na(Distance_grid), 0, Distance_grid)) %>%
  ungroup()%>%
  group_by(IIntID, if_inside_contour)%>%
  mutate(distance_total = sum(Distance_grid),
         #time = n_distinct(Date),
         time = sum(TimeInterval_grid)/60/60/24,
         distance_avg = sum(Distance_grid)/1000/time)%>% # divided by sum of recorded time in days/ total number of observed days
  slice_head()

#handle the individual with few time observed
df_dist[df_dist$IIntID==54946 & !df_dist$if_inside_contour, "distance_avg"] = 0
df_dist[df_dist$IIntID==54946 & !df_dist$if_inside_contour, "time"] = 0

df_paper = df_ret%>%
  right_join(df_dist, by="IIntID") %>%
  group_by(Sex, Age_group)%>%
  mutate(n=n_distinct(IIntID))%>%
  ungroup()%>%
  group_by(IIntID)%>%
  mutate(time_sum = sum(time))%>%
  group_by(Sex, Age_group, if_inside_contour,n) %>%
  summarise(time_mean = mean(time), time_sd = sd(time), 
            distance_mean = mean(distance_avg), distance_sd = sd(distance_avg))%>%
  mutate(if_inside_contour = if_else(if_inside_contour, "inside", "outside")) %>%
  pivot_wider(names_from = if_inside_contour,
              values_from = c(time_mean, time_sd, distance_mean, distance_sd),
              names_sep = "_") %>%
  mutate(Time_Inside   = sprintf("%.2f ± %.2f", time_mean_inside, time_sd_inside),
         Dist_Inside   = sprintf("%.2f ± %.2f", distance_mean_inside, distance_sd_inside),
         Time_Outside  = sprintf("%.2f ± %.2f", time_mean_outside, time_sd_outside),
         Dist_Outside  = sprintf("%.2f ± %.2f", distance_mean_outside, distance_sd_outside),
         Sex = ifelse(Sex == "FEM", "Women", "Men"), 
         Age_group = ifelse(Age_group == "Age 20-26", "20-26", "27-34")) %>%
  dplyr::select(Sex, Age_group, n, Time_Inside, Dist_Inside, Time_Outside, Dist_Outside)

# Now create the LaTeX table
kable(df_paper, format = "latex", booktabs = TRUE,
      caption = "Summary mobility measures for the Sesikhona GPS Study.") %>%
  add_header_above(c(" " = 3, "Time Spent" = 2, "Distance Traveled" = 2)) %>%
  add_header_above(c(" " = 2, "N\\textsuperscript{a}" = 1,
                     "Inside\\textsuperscript{b}" = 1,
                     "Inside\\textsuperscript{c}" = 1,
                     "Outside\\textsuperscript{d}" = 1,
                     "Outside\\textsuperscript{e}" = 1))

##########
num_fem = nrow(df_ret[df_ret$Sex=="FEM",])
num_mal = nrow(df_ret[df_ret$Sex=="MAL",])
df_paper = df_ret%>%
  right_join(df_dist, by="IIntID") %>%
  group_by(Sex, anchor_inside) %>%
  mutate(n=n_distinct(IIntID),
         n_prop=round(ifelse(Sex=="FEM", n/num_fem*100, n/num_mal*100), 2)) %>%
  ungroup() %>%
  #group_by(IIntID)%>%
  #mutate(time_sum = sum(time))%>%
  #ungroup()%>%
  group_by(Sex, anchor_inside, if_inside_contour, n, n_prop) %>%
  summarise(time_mean = mean(time), time_sd = sd(time), 
            distance_mean = mean(distance_avg), distance_sd = sd(distance_avg))%>%
  mutate(if_inside_contour = if_else(if_inside_contour, "inside", "outside")) %>%
  pivot_wider(names_from = if_inside_contour,
              values_from = c(time_mean, time_sd, distance_mean, distance_sd),
              names_sep = "_") %>%
  mutate(n = sprintf("%.0f(%.2f)", n, n_prop),
         Time_Inside   = sprintf("%.2f ± %.2f", time_mean_inside, time_sd_inside),
         Dist_Inside   = sprintf("%.2f ± %.2f", distance_mean_inside, distance_sd_inside),
         Time_Outside  = sprintf("%.2f ± %.2f", time_mean_outside, time_sd_outside),
         Dist_Outside  = sprintf("%.2f ± %.2f", distance_mean_outside, distance_sd_outside),
         Sex = ifelse(Sex == "FEM", "Women", "Men")) %>%
  dplyr::select(Sex,anchor_inside,n, Time_Inside, Dist_Inside, Time_Outside, Dist_Outside)%>%
  arrange(desc(Sex), desc(anchor_inside))

# Now create the LaTeX table
kable(df_paper, format = "latex", booktabs = TRUE,
      caption = "Summary mobility measures for the Sesikhona GPS Study.") %>%
  add_header_above(c(" " = 3, "Time Spent" = 2, "Distance Traveled" = 2)) %>%
  add_header_above(c(" " = 2, "N\\textsuperscript{a}" = 1,
                     "Inside\\textsuperscript{b}" = 1,
                     "Inside\\textsuperscript{c}" = 1,
                     "Outside\\textsuperscript{d}" = 1,
                     "Outside\\textsuperscript{e}" = 1))

#####check individuals with large distance
df_check = df_ret%>%
  right_join(df_dist, by="IIntID") %>%
  group_by(IIntID) %>%
  mutate(total_time = sum(time))%>%
  dplyr::select(IIntID, Sex, Age_group,if_inside_contour, time, distance_avg, 
                total_time, distance_total, n_day)%>%
  filter(!if_inside_contour) %>%
  mutate(time_prop_outside = time/total_time,
         distance_total = distance_total/1000)%>%
  rename(time_outside = time)
#saveRDS(df_check, "outsideContour_distance.rds")

## compare with haoyang's result
output = read.csv("output_all0803-corrected (1).csv")
output$IIntID = as.numeric(gsub("\\.csv$", "", output$Individual))
output$Travel_distance_outside = output$Travel_distance_outside/1000
output$speed = output$Travel_distance_outside/output$Time_recorded_outside
temp = df_check%>%
  dplyr::select(IIntID, distance_total, time_outside, distance_avg) %>%
  right_join(output[,c("IIntID", "Travel_distance_outside", "Time_recorded_outside", "speed")])
temp$time_outside <- format(temp$time_outside , scientific = FALSE)
temp$distance_total <- format(temp$distance_total , scientific = FALSE)
temp$distance_avg <- format(temp$distance_avg , scientific = FALSE)
