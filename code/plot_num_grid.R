source('data_preproc.R')

##------functions for generating grid id & n grids------------------------------
## grid_id of each individual at different activity space levels
gen_df_as_grid_id = function(df){
  IIntID_list = unique(df$IIntID)
  df_as_grid_id <- as.data.frame(matrix(ncol = 101, nrow = 0))
  colnames(df_as_grid_id) <- c(1:100, "IIntID")
  for (iintid in IIntID_list){
    temp = df[df$IIntID==iintid, c("grid_id","time_cum")]
    temp$time_cum = ceiling(temp$time_cum*100) 
    output = temp[0,] #initialize the long output for each individual
    # sort by time_cum
    # grid 1, time_cum n_1 -> as 1:100
    # grid i, time_cum n_i -> as n_(i-1):100
    for (i in 1:nrow(temp)){
      if (i == 1){
        output = rbind(output, data.frame(grid_id = rep(temp[1,]$grid_id, 100), time_cum = 1:100))
      }else{
        output = rbind(output, data.frame(grid_id = rep(temp[i,]$grid_id, 100-temp[i-1,]$time_cum+1), time_cum = temp[i-1,]$time_cum:100))
      }
    }
    output_wide <- output %>%
      mutate(value = 1) %>%   # without creating another column, output has nrow=1, each cell stores a list of grid_id
      pivot_wider(names_from = time_cum, values_from = value, values_fill = NA) %>%
      mutate(across(-grid_id, ~ ifelse(.x == 1, grid_id, NA)), #for all columns except the grid_id, replace 1 by grid_id
             IIntID = iintid) %>%
      dplyr::select(-grid_id)
    df_as_grid_id= rbind(df_as_grid_id, output_wide)
  }
  return (df_as_grid_id)
}

##num. grids of each individual at different activity space levels
gen_df_as_num_grid = function(df_as_grid_id){
  df_as_num_grid <- df_as_grid_id %>%
    group_by(IIntID) %>%
    summarise(across(1:100, ~ sum(!is.na(.x))), .groups = "drop")
  return (df_as_num_grid)
}

##---------output RDS-----------------------------------------------------------
if (FALSE){
  df_prev_inside = df_prev%>% 
    filter(if_inside_contour)%>% # get inside contour
    group_by(IIntID)%>%
    arrange(desc(time_inside_grid))%>% # sort from largest to smallest
    mutate(time = time_inside_grid/sum(time_inside_grid), #standardize time
           time_cum = cumsum(time))
  
  df_as_grid_id = gen_df_as_grid_id(df_prev)
  df_as_num_grid = gen_df_as_num_grid(df_as_grid_id)
  saveRDS(df_as_num_grid, file = "data/df_as_num_grid_inout.rds") #contain both inside and outside grids
  saveRDS(df_as_grid_id, file = "data/df_as_grid_id_inout.rds")
  
  df_as_grid_id = gen_df_as_grid_id(df_prev_inside)
  df_as_num_grid = gen_df_as_num_grid(df_as_grid_id)
  saveRDS(df_as_num_grid, file = "data/df_as_num_grid.rds") # only contain inside grids
  saveRDS(df_as_grid_id, file = "data/df_as_grid_id.rds")
}

##-------------table (inside contour): grids for diff as level------------------
df_as_grid_id = readRDS("data/df_as_grid_id.rds")
df_as_num_grid = readRDS("data/df_as_num_grid.rds")
iintid_female = unique(df_ret$IIntID[df_ret$Sex=="FEM"])
iintid_male = unique(df_ret$IIntID[df_ret$Sex=="MAL"])

# for a prevalence dataset of selected individuals
# return grid_id, num_grid for the union of selected individual activity spaces.
gen_grid = function(df){
  temp = df%>%
    filter(if_inside_contour)%>%
    group_by(grid_id)%>%
    summarise(time = sum(time_inside_grid))%>%
    arrange(desc(time))%>%
    mutate(time = time/sum(time), time_cum = cumsum(time), IIntID = 1)
  df_as_grid = gen_df_as_grid_id(temp)
  num_grid_all = gen_df_as_num_grid(df_as_grid)
  return (list(df_as_grid[,1:100], num_grid_all[1,2:101]))
}
##-------------smallest level where grid count > 1------------------------------
first_increase <- function(x) {
  idx <- which(x > 1)[1]
  if (is.na(idx)) return(NA)  # if no increase
  return(idx)
}
first_level <- apply(df_as_num_grid[2:101], 1, first_increase)

# Plot the CDF
ggplot() +
  stat_ecdf(aes(x = first_level), geom = "step", linewidth = 1) +
  labs(x = expression(gamma), y = "Proportion") +
  scale_x_continuous(limits = c(0, 100)) +
  scale_y_continuous(limits = c(0, 1)) +
  theme_minimal() 
ggsave("plot/num_grid_gt1_cdf.png", width = 6, height = 3)

mean(df_as_num_grid$`50`)

##----------plot num_grids (for all individuals (balanced) in a group)----------
iintid_fem_selected = sample(iintid_female, length(iintid_male))
df_prev_fem_selected = df_prev[df_prev$IIntID%in%iintid_fem_selected, ]
fem_selected_grid = gen_grid(df_prev_fem_selected)
num_grid_fem_selected_all = unlist(fem_selected_grid[[2]])

fem_selected_grid <- replicate(100, {
  # sample same number of females as in male group
  iintid_female_selected <- sample(iintid_female, length(iintid_male), replace = FALSE)
  df_prev_selected <- df_prev[df_prev$IIntID %in% iintid_female_selected, ]
  fem_selected_grid <- gen_grid(df_prev_selected)
  num_grid_fem_selected_all <- unlist(fem_selected_grid[[2]])
  num_grid_fem_selected_all # return the 100-length vector
}, simplify = "matrix")  # result: 100 rows × 10 columns

df_plot <- data.frame(threshold = thresholds*100, mal = num_grid_mal_all,
                      fem = rowMeans(fem_selected_grid, na.rm = TRUE), 
                      q1 = apply(fem_selected_grid, 1, quantile, probs = 0.25, na.rm = TRUE),
                      q3 = apply(fem_selected_grid, 1, quantile, probs = 0.75, na.rm = TRUE))
ggplot(df_plot, aes(x = threshold)) +
  geom_ribbon(aes(ymin = q1, ymax = q3, fill = "Female"), alpha = 0.1) +
  geom_line(aes(y = fem, color = "Female"), alpha = 0.7) +
  geom_line(aes(y = mal, color = "Male"), alpha = 0.7) +
  coord_cartesian(ylim = c(0, 120)) +
  labs(title = "", x = expression(gamma), y = "Number of grid cells", color = "Sex", fill = NULL) +
  scale_color_manual(values = c("Female" = "red", "Male" = "blue"))+
  scale_fill_manual(values = c("Female" = "red", "Male" = "blue"))+
  theme_minimal() +
  guides(fill = "none")+
  theme(panel.background = element_rect(fill = "white", colour = NA), plot.background  = element_rect(fill = "white", colour = NA))
ggsave("plot/num_grid_all.png", width = 10, height = 3)

##----------plot num_grids (for all individuals in a group)---------------------
df_prev_fem = df_prev[df_prev$IIntID%in%iintid_female, ]
df_prev_mal = df_prev[df_prev$IIntID%in%iintid_male, ]
fem_grid = gen_grid(df_prev_fem)
mal_grid = gen_grid(df_prev_mal)
num_grid_fem_all = unlist(fem_grid[[2]])
num_grid_mal_all = unlist(mal_grid[[2]])

df_plot <- data.frame(threshold = thresholds*100, fem = num_grid_fem_all, mal = num_grid_mal_all)
ggplot(df_plot) +
  geom_line(aes(x = threshold, y = mal, color = "Men"), alpha = 0.7) +
  geom_line(aes(x = threshold, y = fem, color = "Women"), alpha = 0.7) +
  labs(title = "", x = "Activity Space Level (%)", y = "Number of grid cells", color = "Sex") +
  theme_minimal() +
  coord_cartesian(ylim = c(0, 120)) +
  scale_color_manual(values = c("Women" = "red", "Men" = "blue"))+
  theme(panel.background = element_rect(fill = "white", colour = NA), plot.background  = element_rect(fill = "white", colour = NA))
#ggsave("plot/num_grid_all.png", width = 10, height = 3)

##--------plot num_grid (at individual level)-----------------------------------
mat <- as.matrix(df_as_num_grid[df_as_num_grid$IIntID%in%iintid_female,-1])
num_grid_fem_ind_mean <- colMeans(mat, na.rm = TRUE)
num_grid_fem_ind_sd <- apply(mat, 2, quantile, probs = c(0.25, 0.75), na.rm = TRUE)
mat <- as.matrix(df_as_num_grid[df_as_num_grid$IIntID%in%iintid_male,-1])
num_grid_mal_ind_mean <- colMeans(mat, na.rm = TRUE)
num_grid_mal_ind_sd <- apply(mat, 2, quantile, probs = c(0.25, 0.75), na.rm = TRUE)
df_plot <- data.frame(threshold = thresholds*100, 
                      fem_mean = num_grid_fem_ind_mean, mal_mean = num_grid_mal_ind_mean, 
                      fem_q1 = num_grid_fem_ind_sd[1,], mal_q1 = num_grid_mal_ind_sd[1,], 
                      fem_q2 = num_grid_fem_ind_sd[2,], mal_q2 = num_grid_mal_ind_sd[2,])
ggplot(df_plot[-100,], aes(x = threshold)) +
  geom_ribbon(aes(ymin = fem_q1, ymax = fem_q2, fill = "Female"), alpha = 0.1) +
  geom_line(aes(y = fem_mean, color = "Female"), alpha = 0.7) +
  geom_ribbon(aes(ymin = mal_q1, ymax = mal_q2, fill = "Male"), alpha = 0.1) +
  geom_line(aes(y = mal_mean, color = "Male"), alpha = 0.7) +
  coord_cartesian(ylim = c(0, 20)) +
  labs(title = "", x = expression(gamma), y = "Number of grid cells", color = "Sex", fill = NULL) +
  scale_color_manual(values = c("Female" = "red", "Male" = "blue"))+
  scale_fill_manual(values = c("Female" = "red", "Male" = "blue"))+
  theme_minimal() +
  guides(fill = "none")+
  theme(panel.background = element_rect(fill = "white", colour = NA), plot.background  = element_rect(fill = "white", colour = NA))
ggsave("plot/num_grid_ind.png", width = 10, height = 3)

##--------plot diff grid cells (for all individuals in a group)-----------------
df_as_grid_fem = fem_grid[[1]]
df_as_grid_mal = mal_grid[[1]]
num_grid_diff <-sapply(1:100, function(t) {
  fem_vals <- unlist(df_as_grid_fem[,t])
  fem_vals <- fem_vals[!is.na(fem_vals)] 
  mal_vals <- unlist(df_as_grid_mal[,t])
  mal_vals <- mal_vals[!is.na(mal_vals)] 
  c((length(fem_vals)-sum(fem_vals %in% mal_vals))/length(fem_vals),
    (length(mal_vals)-sum(mal_vals %in% fem_vals))/length(mal_vals))
})
df_plot <- data.frame(threshold = thresholds*100, 
                      num_grid_diff_fem = num_grid_diff[1,], num_grid_diff_mal = num_grid_diff[2,])
ggplot(data = df_plot) +
  geom_line(aes(x = threshold, y = num_grid_diff_mal, color = "Men"), alpha = 0.7) +
  geom_line(aes(x = threshold, y = num_grid_diff_fem, color = "Women"), alpha = 0.7) +
  labs(title = "", x = "Activity Space Level (%)", y = "Percentage Share of Unique Grid Cells", color = "Sex") +
  scale_color_manual(values = c("Women" = "red", "Men" = "blue"))+
  theme_minimal() +
  coord_cartesian(xlim = c(0, 100)) +
  theme(panel.background = element_rect(fill = "white", colour = NA), plot.background  = element_rect(fill = "white", colour = NA))
#ggsave("plot/num_diff_grid_all.png", width = 10, height = 3)

##--------plot diff grid cells (at individual level)-----------------------------------
num_grid_diff <- sapply(1:100, function(i) {
  as_grid_fem = df_as_grid_id[df_as_grid_id$IIntID %in% iintid_female,]
  as_grid_mal = df_as_grid_id[df_as_grid_id$IIntID %in% iintid_male,]
  mal_vals <- unlist(unique(na.omit(as_grid_mal[, i])))
  fem_vals <- unlist(unique(na.omit(as_grid_fem[, i])))
  c((length(fem_vals)-sum(fem_vals %in% mal_vals))/length(fem_vals),   # num of grids in fem but not in mal
    (length(mal_vals)-sum(mal_vals %in% fem_vals))/length(mal_vals))
})
df_plot <- data.frame(threshold = thresholds*100, 
                      num_grid_diff_fem = num_grid_diff[1,], num_grid_diff_mal = num_grid_diff[2,])
ggplot() +
  geom_line(data = df_plot,
            aes(x = threshold, y = num_grid_diff_mal, color = "Men only"), alpha = 0.7) +
  geom_line(data = df_plot,
            aes(x = threshold, y = num_grid_diff_fem, color = "Women only"), alpha = 0.7) +
  labs(title = "", x = "Activity Space Level (%)", y = "Percentage Share of Unique Grid Cells", color = "") +
  scale_color_manual(values = c("Women only" = "red", "Men only" = "blue"))+
  theme_minimal() +
  coord_cartesian(xlim = c(0, 100)) +
  theme(panel.background = element_rect(fill = "white", colour = NA), plot.background  = element_rect(fill = "white", colour = NA))
#ggsave("plot/num_diff_grid_ind.png", width = 10, height = 3)

##--------plot # overlapping grid cells (balanced) (for all individuals in a group)--------
num_grid_diff <- replicate(100, { # do sampling of females multiple times
  x = sapply(1:100, function(t) {#at individual level, iterate through all activity space levels
    iintid_female_selected <- sample(iintid_female, length(iintid_male), replace = FALSE)
    as_grid_fem = df_as_grid_id[df_as_grid_id$IIntID %in% iintid_female_selected,]
    #as_grid_fem = df_as_grid_id[df_as_grid_id$IIntID %in% iintid_female,]
    as_grid_mal = df_as_grid_id[df_as_grid_id$IIntID %in% iintid_male,]
    mal_vals <- unlist(unique(na.omit(as_grid_mal[, t])))
    fem_vals <- unlist(unique(na.omit(as_grid_fem[, t])))
    sum(fem_vals %in% mal_vals)
  })
}, simplify = "matrix") 

mean = rowMeans(num_grid_diff, na.rm = TRUE)
q1 <- apply(num_grid_diff, 1, quantile, probs = 0.25, na.rm = TRUE)
q3 <- apply(num_grid_diff, 1, quantile, probs = 0.75, na.rm = TRUE)
df_plot <- data.frame(threshold = thresholds*100, mean, q1, q3)
ggplot(data = df_plot) +
  geom_line(aes(x = threshold, y = mean)) +
  geom_ribbon(data = df_plot, aes(x = threshold, ymin = q1, ymax = q3), alpha = 0.1) 

##----------------
#num_grid_diff_mal <-sapply(thresholds, function(t) {
#  fem_vals <- df_as_grid_fem$grid_id[df_as_grid_fem$time_cum <= t]
#  mal_vals <- df_as_grid_mal$grid_id[df_as_grid_mal$time_cum <= t]
#  (length(mal_vals)-sum(mal_vals %in% fem_vals))/length(mal_vals) 
#})
num_grid_diff_fem <- sapply(seq(2, 101), function(i) {
  mal_vals <- unique(na.omit(as_grid_mal[, i]))
  fem_vals <- unique(na.omit(as_grid_fem[, i]))
  (length(fem_vals)-sum(fem_vals %in% mal_vals))/length(fem_vals)   # num of grids in fem but not in mal
})

num_grid_overlap <-sapply(1:100, function(t) {#collective
  fem_vals <- unlist(df_as_grid_fem[,t])
  fem_vals <- fem_vals[!is.na(fem_vals)] 
  mal_vals <- unlist(df_as_grid_mal[,t])
  mal_vals <- mal_vals[!is.na(mal_vals)] 
  c(sum(fem_vals %in% mal_vals)/length(fem_vals),sum(fem_vals %in% mal_vals)/length(mal_vals))
})


