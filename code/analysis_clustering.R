source('data_prev.R')
set.seed(123)
##---------------------clustering on num. grid-----------------------------------------------
df_as_num_grid = readRDS("data/df_as_num_grid.RDS")

#x cluster on people with inside home location
df_as_num_grid_inside = df_as_num_grid
#df_as_num_grid_inside = df_as_num_grid[df_as_num_grid$IIntID %in% df_rst_inside$IIntID,]
km <- kmeans(log1p(df_as_num_grid_inside[,51:96]), centers = 2, nstart = 25) #skip IIntID#use as 50-95
df_as_num_grid_inside = df_as_num_grid_inside %>%
  mutate(cluster = as.factor(km$cluster),
         cluster = ifelse(cluster==2, "High Mobility", "Low Mobility"))
iintid_cls_ngrid_h = df_as_num_grid_inside[df_as_num_grid_inside$cluster == "High Mobility",]$IIntID
iintid_cls_ngrid_l = df_as_num_grid_inside[df_as_num_grid_inside$cluster == "Low Mobility",]$IIntID

# check cluster on log transformed data
df_as_num_grid_inside %>%
  pivot_longer(cols = 2:101) %>%
  rename(as_level = name, num_grid = value) %>%
  mutate(as_level = as.integer(sub("num_grid_as_", "", as_level)),
         IIntID = as.factor(IIntID),
         num_grid = log1p(num_grid)) %>%
  ggplot()+
  geom_line(aes(x = as_level, y = num_grid,group = IIntID, col = cluster), linewidth = 0.2)+
  scale_color_manual(values = c("High Mobility" = "red", "Low Mobility" = "grey50"))+
  coord_cartesian(xlim = c(25, 100), ylim = c(0.5, 6))+ 
  labs(x = "Activity Space Level", y = "log(1+#Grids)", color = "")+
  theme_bw()+
  theme(legend.position = "top")
#ggsave("plot/cluster_ngrid_log.png")

#check cluster on original data
df_as_num_grid_inside_long = df_as_num_grid_inside %>%
  pivot_longer(cols = 2:101) %>%
  rename(as_level = name, num_grid = value) %>%
  mutate(as_level = as.integer(sub("num_grid_as_", "", as_level)),
         IIntID = as.factor(IIntID))
ggplot(data = df_as_num_grid_inside_long)+
  geom_line(aes(x = as_level, y = num_grid,group = IIntID, col = cluster), linewidth = 0.2)+
  scale_color_manual(values = c("High Mobility" = "red", "Low Mobility" = "grey50"))+
  coord_cartesian(xlim = c(25, 100), ylim = c(0, 100))+ 
  labs(x = "Activity Space Level", y = "Number of Grids", color = "")+
  theme_bw()+
  theme(legend.position = "top")
#ggsave("plot/cluster_ngrid.png")

id_list1 = df_as_num_grid_inside[df_as_num_grid_inside$cluster=="High Mobility",]$IIntID
id_list2 = df_as_num_grid_inside[df_as_num_grid_inside$cluster=="Low Mobility",]$IIntID
df_select1 = df_rst[df_rst$IIntID %in% id_list1,]
df_select2 = df_rst[df_rst$IIntID %in% id_list2,]
t.test(df_select1$prev_as50, df_select1$prev_overall, paired=TRUE)
t.test(df_select2$prev_as50, df_select2$prev_overall, paired=TRUE)
t.test(df_select1$prev_as50, df_select2$prev_as50)
t.test(df_select1$prev_overall, df_select2$prev_overall)
summary(df_select1$Sex)
summary(df_select2$Sex)
mean(df_select1$n_grid_as95)
sd(df_select1$n_grid_as95)
mean(df_select2$n_grid_as95)
sd(df_select2$n_grid_as95)

t.test(df_rst[df_rst$IIntID %in% id_list1,]$prev_as50,
       df_rst[df_rst$IIntID %in% id_list1,]$prev_inside, paired = TRUE)
t.test(df_rst[df_rst$IIntID %in% id_list,"prev_overall"],
       df_rst[df_rst$IIntID %notin% id_list,"prev_overall"])

mean(df_select$prev_as50)
mean(df_select$prev_overall)

##-------------clustering on prev-----------------------------------------------
if (FALSE){
  # only for inside contour grid id
  df_as_grid_id = readRDS("data/df_as_grid_id.RDS")
  
  df_grid_prev <- df_as_grid_id %>%# Select all columns except the IIntID identifier
    pivot_longer(cols = -IIntID, names_to = "as_level", 
                 values_to = "grid_id", names_transform = list(as_level = as.numeric)) %>%
    filter(!is.na(grid_id))%>%
    left_join(df_prev, by = c("IIntID" = "IIntID", "grid_id" = "grid_id")) %>%
    filter(!is.na(time_inside_grid)) %>%
    group_by(IIntID, as_level) %>%
    mutate(timeweight_inside_grid = time_inside_grid / sum(time_inside_grid, na.rm = TRUE)) %>%
    summarise(prev = sum(prev_esti * timeweight_inside_grid, na.rm = TRUE), 
              .groups = 'drop') %>%# Ungroup the data
    pivot_wider(names_from = as_level, values_from = prev, id_cols = IIntID)
    
  saveRDS(df_grid_prev, "data/df_grid_prev.rds")
}

df_grid_prev = readRDS("data/df_grid_prev.rds")
df_grid_prev_inside = df_grid_prev[df_grid_prev$IIntID %in% df_rst_inside$IIntID,]
set.seed(123)
km <- kmeans(df_grid_prev_inside[,51:96], centers = 3, nstart = 25) #skip IIntID#use as 50-95
df_grid_prev_inside$cluster <- as.factor(km$cluster)
df_grid_prev_inside_long = df_grid_prev_inside %>%
  pivot_longer(cols = 2:101) %>%
  rename(as_level = name, num_grid = value) %>%
  mutate(as_level = as.integer(sub("num_grid_as_", "", as_level)),
         IIntID = as.factor(IIntID),
         cluster = recode_factor(cluster, `2` = "High", `3` = "Medium", `1` = "Low"))
iintid_cls_prev_h = unique(df_grid_prev_inside_long[df_grid_prev_inside_long$cluster == "High",]$IIntID)
iintid_cls_prev_m = unique(df_grid_prev_inside_long[df_grid_prev_inside_long$cluster == "Medium",]$IIntID)
iintid_cls_prev_l = unique(df_grid_prev_inside_long[df_grid_prev_inside_long$cluster == "Low",]$IIntID)

ggplot(data = df_grid_prev_inside_long)+
  geom_line(aes(x = as_level, y = num_grid,group = IIntID, col = cluster), linewidth = 0.2)+
  scale_color_manual(values = c("High" = "red", "Medium" = "orange", "Low" = "blue"))+
  labs(x = "Activity Space Level", y = "Prevalence Inside", color = "Risk")+
  theme_bw()+
  theme(legend.position = "top")
#ggsave("plot/cluster_prev.png", width = 6, height = 5)

df_grid_prev_inside = df_grid_prev[df_grid_prev$IIntID %in% df_rst_inside$IIntID,]
df_grid_prev_inside[,2:101] = df_grid_prev_inside[,2:101] - as.data.frame(rep(df_grid_prev_inside[,2], 100))
df_grid_prev_inside=df_grid_prev_inside[df_grid_prev_inside$IIntID %notin% c(83860, 83334),]
set.seed(123)
km <- kmeans(df_grid_prev_inside[,51:96], centers = 3, nstart = 25) #skip IIntID#use as 50-95
df_grid_prev_inside$cluster <- as.factor(km$cluster)
df_grid_prev_inside_long = df_grid_prev_inside %>%
  pivot_longer(cols = 2:101) %>%
  rename(as_level = name, num_grid = value) %>%
  mutate(as_level = as.integer(sub("num_grid_as_", "", as_level)),
         IIntID = as.factor(IIntID),
         cluster = recode_factor(cluster, `2` = "Increase", `1` = "Stable", `3` = "Decrease"))
iintid_cls_prev_stand_h = unique(df_grid_prev_inside_long[df_grid_prev_inside_long$cluster == "Increase",]$IIntID)
iintid_cls_prev_stand_m = unique(df_grid_prev_inside_long[df_grid_prev_inside_long$cluster == "Stable",]$IIntID)
iintid_cls_prev_stand_l = unique(df_grid_prev_inside_long[df_grid_prev_inside_long$cluster == "Decrease",]$IIntID)

ggplot(data = df_grid_prev_inside_long)+
  geom_line(aes(x = as_level, y = num_grid,group = IIntID, col = cluster), linewidth = 0.2)+
  scale_color_manual(values = c("Increase" = "red", "Stable" = "orange", "Decrease" = "blue"))+
  labs(x = expression(gamma), y = expression(Deviation~from~epsilon["home"]), color = NULL)+
  theme_bw() +
  theme(
    legend.position      = c(0.05, 0.05),      # lower-left inside plot
    legend.justification = c(0, 0),            # anchor legend's bottom-left corner
    legend.background    = element_rect(fill = "white", color = "black")
  )+
  guides(color = guide_legend(override.aes = list(linewidth = 0.8, alpha = 1)))
ggsave("plot/cluster_prev_stand.png", width = 6, height = 3)

ind_clh = unlist(df_grid_prev_inside[df_grid_prev_inside$cluster==3,"IIntID"])
ind_cll = unlist(df_grid_prev_inside[df_grid_prev_inside$cluster==2,"IIntID"])
summary(df_rst[df_rst$IIntID %in% ind_clh,"Sex"])
summary(df_rst[df_rst$IIntID %in% ind_cll,"Sex"])
summary(df_rst[df_rst$IIntID %in% ind_clh,"Age.x"])
summary(df_rst[df_rst$IIntID %in% ind_cll,"Age.x"])
summary(df_rst[df_rst$IIntID %in% ind_clh,"timeprop_anchor"])
summary(df_rst[df_rst$IIntID %in% ind_cll,"timeprop_anchor"])

