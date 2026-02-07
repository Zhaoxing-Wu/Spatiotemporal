source('data_preproc.R')
library(knitr)
library(kableExtra)

##--------------prevalence------------------------------------------------------
df_inout_day_grid = df_prev %>%
  group_by(IIntID, if_inside_contour) %>%
  summarise(n_day = n_distinct(Date),
            n_grid = n_distinct(grid_id)) %>%
  dplyr::select(IIntID, if_inside_contour, n_day, n_grid) %>%
  pivot_wider(names_from = if_inside_contour,
              values_from = c(n_day, n_grid)) %>%
  rename(n_day_inside = n_day_TRUE, n_day_outside = n_day_FALSE,
         n_grid_inside = n_grid_TRUE, n_grid_outside = n_grid_FALSE)

df_wk_anchor = df_prev %>%
  group_by(IIntID, isWeekday)%>%
  arrange(desc(time_inside_grid)) %>%
  summarise(anchor_inside = first(if_inside_contour)) %>%
  pivot_wider(names_from = isWeekday, values_from = anchor_inside)%>%
  rename(anchor_inside_wkday = `TRUE`, anchor_inside_wkend = `FALSE`)

df_day_anchor = df_prev %>%
  group_by(IIntID, isDaytime)%>%
  arrange(desc(time_inside_grid)) %>%
  summarise(anchor_inside = first(if_inside_contour)) %>%
  pivot_wider(names_from = isDaytime, values_from = anchor_inside)%>%
  rename(anchor_inside_day = `TRUE`, anchor_inside_night = `FALSE`)


df_as_prev <- df_prev%>%
  group_by(IIntID) %>%
  arrange(desc(time_inside_grid)) %>%
  mutate(timeweight_inside_grid = time_inside_grid/sum(time_inside_grid),
         time_cum = cumsum(timeweight_inside_grid),
         is_anchor = row_number() ==1,
         is_as99 = time_cum<=max(0.99, first(time_cum)),
         is_as95 = time_cum<=max(0.95, first(time_cum)),
         is_as90 = time_cum<=max(0.90, first(time_cum)),
         is_as70 = time_cum<=max(0.70, first(time_cum)),
         is_as50 = time_cum<=max(0.50, first(time_cum)))%>%
  summarise(n_grid = n(),
            n_days = n_distinct(Date),
            prev_anchor = first(prev_esti),
            prev_as99 = sum(prev_esti[is_as99] * (timeweight_inside_grid[is_as99]/sum(timeweight_inside_grid[is_as99])) ),
            prev_as95 = sum(prev_esti[is_as95] * (timeweight_inside_grid[is_as95]/sum(timeweight_inside_grid[is_as95])) ),
            prev_as90 = sum(prev_esti[is_as90] * (timeweight_inside_grid[is_as90]/sum(timeweight_inside_grid[is_as90])) ),
            prev_as70 = sum(prev_esti[is_as70] * (timeweight_inside_grid[is_as70]/sum(timeweight_inside_grid[is_as70])) ),
            prev_as50 = sum(prev_esti[is_as50] * (timeweight_inside_grid[is_as50]/sum(timeweight_inside_grid[is_as50])) ),
            prev_as90_xanchor = sum(prev_esti[is_as90 &!is_anchor] * (timeweight_inside_grid[is_as90&!is_anchor]/sum(timeweight_inside_grid[is_as90&!is_anchor])) ),
            prev_as95_xanchor = sum(prev_esti[is_as95 &!is_anchor] * (timeweight_inside_grid[is_as95&!is_anchor]/sum(timeweight_inside_grid[is_as95&!is_anchor])) ),
            prev_as99_xanchor = sum(prev_esti[is_as99 &!is_anchor] * (timeweight_inside_grid[is_as99&!is_anchor]/sum(timeweight_inside_grid[is_as99&!is_anchor])) ),
            n_grid_as99 = sum(is_as99),
            n_grid_as95 = sum(is_as95),
            n_grid_as90 = sum(is_as90),
            n_grid_as70 = sum(is_as70),
            n_grid_as50 = sum(is_as50),
            timeprop_anchor = first(time_cum),
            timeprop_inside = sum(timeweight_inside_grid[if_inside_contour]),
            time_inside = sum(time_inside_grid[if_inside_contour]),
            anchor_inside = first(if_inside_contour))

prev = function(df_prev, filter_col, name_col){
  ret <- df_prev[filter_col, ] %>%
    group_by(IIntID) %>%
    mutate(time_stand = time_inside_grid / sum(time_inside_grid),
           {{ name_col }} := sum(prev_esti * time_stand)) %>%
    slice_head()%>%
    dplyr::select(IIntID, {{ name_col }})
  return(ret)
}
df_prev_day = prev(df_prev, df_prev$isDaytime, `prev_day`)
df_prev_night = prev(df_prev, !df_prev$isDaytime, `prev_night`)
df_prev_inside = prev(df_prev, df_prev$if_inside_contour, `prev_inside`)
df_prev_outside = prev(df_prev, !df_prev$if_inside_contour, `prev_outside`)
df_prev_wkday = prev(df_prev, df_prev$isWeekday, `prev_wkday`)
df_prev_wkend = prev(df_prev, !df_prev$isWeekday, `prev_wkend`)
df_prev_overall = prev(df_prev, rep(TRUE, nrow(df_prev)), `prev_overall`)

df_inout_dist = df %>%
  group_by(IIntID) %>%
  mutate(# ignore time interval/distance if move between contours
    TimeInterval_grid = ifelse(if_inside_contour != lag(if_inside_contour), 0, TimeInterval),
    TimeInterval_grid = ifelse(is.na(TimeInterval_grid), 0, TimeInterval_grid),
    Distance_grid = Distance,
    Distance_grid = ifelse(if_inside_contour != lag(if_inside_contour), 0, Distance_grid), 
    Distance_grid = ifelse(is.na(Distance_grid), 0, Distance_grid)) %>%
  ungroup()%>%
  group_by(IIntID, if_inside_contour)%>%
  mutate(distance_total = sum(Distance_grid))%>% # divided by sum of recorded time in days/ total number of observed days
  slice_head() %>%
  dplyr::select(IIntID, if_inside_contour, distance_total) %>%
  pivot_wider(names_from = if_inside_contour, 
              values_from = distance_total) %>%
  rename(distance_inside = `TRUE`,
         distance_outside = `FALSE`)

df_timeprop = df %>%
  group_by(IIntID) %>%
  summarise(timeprop_day = sum(TimeInterval[isDaytime])/sum(TimeInterval),
            timeprop_wkday = sum(TimeInterval[isWeekday])/sum(TimeInterval))

df_rst = df_ret %>%
  left_join(df_inout_day_grid, by = "IIntID")%>%
  left_join(df_as_prev, by = "IIntID")%>%
  left_join(df_wk_anchor, by = "IIntID") %>%
  left_join(df_day_anchor, by = "IIntID") %>%
  left_join(df_prev_day, by = "IIntID")%>%
  left_join(df_prev_night, by = "IIntID")%>%
  left_join(df_prev_inside, by = "IIntID")%>%
  left_join(df_prev_outside, by = "IIntID")%>%
  left_join(df_prev_wkday, by = "IIntID")%>%
  left_join(df_prev_wkend, by = "IIntID")%>%
  left_join(df_prev_overall, by = "IIntID")%>%
  left_join(df_inout_dist, by = "IIntID")%>%
  left_join(df_timeprop, by = "IIntID") 

df_rst$Sex = as.factor(df_rst$Sex)
df_rst_inside = df_rst[df_rst$anchor_inside,]
df_rst_outside = df_rst[!df_rst$anchor_inside,]
rm(df_inout_day_grid, df_as_prev, df_wk_anchor, df_day_anchor, df_prev_day, 
   df_prev_night, df_prev_inside, df_prev_outside, df_prev_wkday, df_prev_wkend, 
   df_prev_overall, df_inout_dist, df_timeprop, df_ret, prev) 

#saveRDS(df_rst$IIntID[df_rst$anchor_inside==FALSE],"data/anchor_outside.rds")
