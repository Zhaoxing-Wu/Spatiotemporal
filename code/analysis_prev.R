source('data_prev.R')

##-------------pairwise t-test between prev_as50 and prev_as50+-----------------
#for all participants
t.test(df_rst$prev_as50, df_rst$prev_overall, paired = TRUE) # no
t.test(df_rst$prev_as50, df_rst$prev_inside, paired = TRUE) #yes
#for participants whose home location is inside
t.test(df_rst_inside$prev_as50, df_rst_inside$prev_overall, paired = TRUE) #yes
t.test(df_rst_inside$prev_as50, df_rst_inside$prev_inside, paired = TRUE) # no
#for participants whose home location is outside
t.test(df_rst_outside$prev_as50, df_rst_outside$prev_overall, paired = TRUE) #yes
t.test(df_rst_outside$prev_as50, df_rst_outside$prev_inside, paired = TRUE) #yes
t.test(df_rst_outside$prev_as50, df_rst_outside$prev_outside, paired = TRUE)  #no

##--------------divide into low/high risk groups based on time/prev-------------
vals_time <- 1 - df_rst_inside$timeprop_inside
vals_prev = df_rst_inside$prev_inside
qs_time <- quantile(vals_time, c(0.4, 0.6))
qs_prev <- quantile(vals_prev, c(0.4, 0.6))
ind2 <- which(vals_time > qs_time[2] & vals_prev > qs_prev[2])  # top right
ind1 <- which(vals_time < qs_time[1] & vals_prev < qs_prev[1])  # bottom left
ind3 <- which(vals_time > qs_time[2] & vals_prev < qs_prev[2])  # bottom right
ind4 <- which(vals_time < qs_time[2] & vals_prev > qs_prev[2])  # top left
df_rst_inside$group_flag <- "Other"
df_rst_inside$group_flag[ind1] <- "Low Risk"
df_rst_inside$group_flag[ind2] <- "High Risk"
df_rst_inside$group_flag[c(ind3, ind4)] <- "High Risk (one)"
df_rst_inside$group_flag = as.factor(df_rst_inside$group_flag)

ggplot(df_rst_inside, aes(x = 1 - timeprop_inside, y = prev_inside)) +
  geom_rect(aes(ymin = qs_prev[2], ymax = Inf, xmin = -Inf, xmax = Inf, fill = "High Risk (Local)"),
            alpha = 0.06, inherit.aes = FALSE) +
  geom_rect(aes(xmin = qs_time[2], xmax = Inf, ymin = -Inf, ymax = Inf, fill = "High Risk (External)"),
            alpha = 0.02, inherit.aes = FALSE) +  # Prevents geom_rect from inheriting 'color' and 'group' aesthetics
  scale_fill_manual(name = "", values = c("High Risk (External)" = "lightsalmon2", 
                                          "High Risk (Local)" = "plum3")) +
  geom_vline(xintercept = qs_time[1], linetype = "dotted", color = "grey50", linewidth = 0.5)+
  geom_vline(xintercept = qs_time[2], linetype = "dashed", color = "lightsalmon3", linewidth = 0.8)+
  geom_hline(yintercept = qs_prev[1], linetype = "dotted", color = "grey50", linewidth = 0.5)+
  geom_hline(yintercept = qs_prev[2], linetype = "dashed", color = "plum4", linewidth = 0.8)+
  geom_point(aes(color = group_flag), size = 0.8) +
  scale_color_manual(values = c("High Risk" = "#B22222", 
                                "High Risk (one)" = "black",    
                                "Low Risk" = "blue", "Other" = "grey70"),
                     breaks = c("High Risk", "Low Risk")) +
  guides(color = guide_legend(title = "", breaks = c("High Risk", "Low Risk"), override.aes = list(size = 2)),
         fill = guide_legend(name = "", override.aes = list(alpha = 0.8, size = 5, shape = 15 )))+
  labs(x = "Proportion of Time Outside AHRI Study Area",
       y = expression(epsilon["in"]))+
  theme_minimal()+
  theme(legend.position = "top", legend.title = element_text(face = "bold"),
        plot.background = element_rect(fill = "white", color = NA),
        legend.spacing.x = unit(0.1, "cm"), legend.margin = margin(t = 0, b = 0, unit = "cm"))
ggsave("plot/risk_groups.png",width = 6,height = 4)

#
t.test(df_rst_inside$prev_as50[ind1], df_rst_inside$prev_overall[ind1], paired = TRUE) # no
t.test(df_rst_inside$prev_as50[ind1], df_rst_inside$prev_inside[ind1], paired = TRUE) # no
t.test(df_rst_inside$prev_as50[ind2], df_rst_inside$prev_overall[ind2], paired = TRUE) # yes
t.test(df_rst_inside$prev_as50[ind2], df_rst_inside$prev_inside[ind2], paired = TRUE) # yes

t.test(df_rst_inside$prev_as50[ind3], df_rst_inside$prev_overall[ind3], paired = TRUE) # no
t.test(df_rst_inside$prev_as50[ind3], df_rst_inside$prev_inside[ind3], paired = TRUE) # no
t.test(df_rst_inside$prev_as50[ind4], df_rst_inside$prev_overall[ind4], paired = TRUE) # yes
t.test(df_rst_inside$prev_as50[ind4], df_rst_inside$prev_inside[ind4], paired = TRUE) # yes

df_rst_inside$group = "Low risk"
df_rst_inside$group[ind2] = "High risk"
df_rst_inside$group[ind3] = "High prev inside"
df_rst_inside$group[ind4] = "High time outside"


df_rst_inside$group = "Low risk"
df_rst_inside$group[df_rst_inside$IIntID %in% iintid_cls_prev_stand_h] = "High risk"
df_rst_inside$group[df_rst_inside$IIntID %in% iintid_cls_prev_stand_m] = "Medium"
df_rst_inside$group[ind4] = "High time outside"
temp = df_rst_inside %>%
  group_by(group) %>%   # replace with your actual group variable name
  summarise(
    n = n(),  # number of participants in the group
    mean_age = mean(Age.x, na.rm = TRUE),
    sd_age = sd(Age.x, na.rm = TRUE),
    mean_timeprop = mean(timeprop_anchor, na.rm = TRUE),
    sd_timeprop = sd(timeprop_anchor, na.rm = TRUE),
    mean_ngrid = mean(n_grid_inside, na.rm = TRUE),
    sd_ngrid = sd(n_grid_inside, na.rm = TRUE),
    mean_ngrid_as95 = mean(n_grid_as95, na.rm = TRUE),
    sd_ngrid_as95 = sd(n_grid_as95, na.rm = TRUE),
    sex_male = sum(Sex == "MAL", na.rm = TRUE),
    sex_female = sum(Sex == "FEM", na.rm = TRUE)
  )

# within the high/low risk groups
t.test(df_rst_inside$prev_anchor[ind1], df_rst_inside$prev_as99_xanchor[ind1]) # no 
t.test(df_rst_inside$prev_anchor[ind2], df_rst_inside$prev_as99_xanchor[ind2]) # yes, anchor much higher
t.test(df_rst_inside$prev_as50[ind1], df_rst_inside$prev_as99[ind1], paired = TRUE, alternative = "less") # no
t.test(df_rst_inside$prev_as50[ind2], df_rst_inside$prev_as99[ind2], paired = TRUE, alternative = "greater") # yes
t.test(df_rst_inside$prev_as50[ind1], df_rst_inside$prev_inside[ind1], paired = TRUE, alternative = "less") # no
t.test(df_rst_inside$prev_as50[ind2], df_rst_inside$prev_inside[ind2], paired = TRUE, alternative = "greater") # yes
# there is no demographic differences
t.test(prev_inside ~ Sex, data = df_rst_inside[c(ind1, ind2),]) #no 
t.test(timeprop_inside ~ Sex, data = df_rst_inside[c(ind1, ind2),]) #no
t.test(df_rst_inside$Age.x[ind1], df_rst_inside$Age.x[ind2]) #no
# prev_as50, timeprop_anchor different as expected
t.test(df_rst_inside$prev_as50[ind1], df_rst_inside$prev_as50[ind2]) #yes, ind1<ind2
t.test(df_rst_inside$timeprop_anchor[ind1], df_rst_inside$timeprop_anchor[ind2])#yes, ind1mean>ind2
# there is no large mobility differences
t.test(df_rst_inside$n_grid_inside[ind1], df_rst_inside$n_grid_inside[ind2])#ind1mean>ind2
# for two groups, the non-anchor location activity space prevalence are the same
t.test(subset(df_rst_inside, prev_as90_xanchor != 0)[ind1, "prev_as90_xanchor"], 
       subset(df_rst_inside, prev_as90_xanchor != 0)[ind2, "prev_as90_xanchor"])
t.test(subset(df_rst_inside, prev_as95_xanchor != 0)[ind1, "prev_as95_xanchor"],
       subset(df_rst_inside, prev_as95_xanchor != 0)[ind2, "prev_as95_xanchor"])
t.test(subset(df_rst_inside, prev_as99_xanchor != 0)[ind1, "prev_as99_xanchor"],
       subset(df_rst_inside, prev_as99_xanchor != 0)[ind2, "prev_as99_xanchor"])


####################

#auc -> mobility 
# need to quantify participant's mobility/ dispersion around the area
# auc is essentially sum of t_g_i * w_i, if replace w_i with log t => entropy

#need mobility exposure
# use 50% to replace the home location: might move around the home location
# use gps instead of just the home location: able to get xx%as_prev

# spagetti

#activity space for 70, 80, 90 for 3 representative (difference) participant
#colored by prevalence

# get indices of top 3 largest differences




#################
df_rst_inside = df_rst[df_rst$anchor_inside,]
df_rst_inside$distance_outside <- replace_na(df_rst_inside$distance_outside, 0)
vals_dist <- df_rst_inside$distance_inside+df_rst_inside$distance_outside
vals_prev = df_rst_inside$prev_inside
qs_prev <- quantile(vals_prev, c(0.4, 0.6))
qs_dist <- quantile(vals_dist, c(0.4, 0.6))
ind2 <- which(vals_prev > qs_prev[2] & vals_dist > qs_dist[2])  # top quartile
ind1 <- which(vals_prev < qs_prev[1] & vals_dist < qs_dist[1])  # bottom quartilesummary(df_rst$Sex[ind1])
df_rst_inside$group_flag <- "other"
df_rst_inside$group_flag[ind1] <- "ind1"
df_rst_inside$group_flag[ind2] <- "ind2"
ggplot(df_rst_inside, aes(x = distance_inside+distance_outside, y = prev_inside, color = group_flag)) +
  geom_point() +
  scale_color_manual(values = c("ind1" = "blue", "ind2" = "red", "other" = "grey70")) +
  theme_minimal()
t.test(prev_inside ~ Sex, data = df_rst_inside[c(ind1, ind2),]) #no diff
t.test(timeprop_inside ~ Sex, data = df_rst_inside[c(ind1, ind2),]) #no diff
t.test(df_rst_inside$Age.x[ind1], df_rst_inside$Age.x[ind2])#no diff
t.test(df_rst_inside$prev_anchor[ind1], df_rst_inside$prev_anchor[ind2]) #ind1<ind2
t.test(df_rst_inside$timeprop_anchor[ind1], df_rst_inside$timeprop_anchor[ind2])#no difference
t.test(subset(df_rst_inside, prev_as90_xanchor != 0)[ind1, "prev_as90_xanchor"], # no difference
       subset(df_rst_inside, prev_as90_xanchor != 0)[ind2, "prev_as90_xanchor"])
t.test(subset(df_rst_inside, prev_as95_xanchor != 0)[ind1, "prev_as95_xanchor"],
       subset(df_rst_inside, prev_as95_xanchor != 0)[ind2, "prev_as95_xanchor"])
t.test(subset(df_rst_inside, prev_as99_xanchor != 0)[ind1, "prev_as99_xanchor"],
       subset(df_rst_inside, prev_as99_xanchor != 0)[ind2, "prev_as99_xanchor"])
t.test(df_rst_inside$prev_anchor[ind1], df_rst_inside$prev_as99_xanchor[ind1]) # no difference
t.test(df_rst_inside$prev_anchor[ind2], df_rst_inside$prev_as99_xanchor[ind2]) #anchor much higher
t.test(df_rst_inside$prev_as50[ind2], df_rst_inside$prev_as99[ind2]) #not as significant

df_rst_inside = df_rst[df_rst$anchor_inside,]
vals_grid <- df_rst_inside$n_grid_inside
vals_prev = df_rst_inside$prev_inside
qs_prev <- quantile(vals_prev, c(0.4, 0.6))
qs_grid <- quantile(vals_grid, c(0.4, 0.6))
ind2 <- which(vals_prev > qs_prev[2] & vals_grid > qs_grid[2])  # top quartile
ind1 <- which(vals_prev < qs_prev[1] & vals_grid < qs_grid[1])  # bottom quartilesummary(df_rst$Sex[ind1])
df_rst_inside$group_flag <- "other"
df_rst_inside$group_flag[ind1] <- "ind1"
df_rst_inside$group_flag[ind2] <- "ind2"
ggplot(df_rst_inside, aes(x = n_grid_inside, y = prev_inside, color = group_flag)) +
  geom_point() +
  scale_color_manual(values = c("ind1" = "blue", "ind2" = "red", "other" = "grey70")) +
  theme_minimal()
t.test(prev_inside ~ Sex, data = df_rst_inside[c(ind1, ind2),])
t.test(timeprop_inside ~ Sex, data = df_rst_inside[c(ind1, ind2),]) #no diff
t.test(df_rst_inside$Age.x[ind1], df_rst_inside$Age.x[ind2])#no diff
t.test(df_rst_inside$prev_anchor[ind1], df_rst_inside$prev_anchor[ind2]) #ind1<ind2
t.test(df_rst_inside$timeprop_anchor[ind1], df_rst_inside$timeprop_anchor[ind2])#no difference
t.test(subset(df_rst_inside, prev_as90_xanchor != 0)[ind1, "prev_as90_xanchor"], # no difference
       subset(df_rst_inside, prev_as90_xanchor != 0)[ind2, "prev_as90_xanchor"])
t.test(subset(df_rst_inside, prev_as95_xanchor != 0)[ind1, "prev_as95_xanchor"],
       subset(df_rst_inside, prev_as95_xanchor != 0)[ind2, "prev_as95_xanchor"])
t.test(subset(df_rst_inside, prev_as99_xanchor != 0)[ind1, "prev_as99_xanchor"],
       subset(df_rst_inside, prev_as99_xanchor != 0)[ind2, "prev_as99_xanchor"])
t.test(df_rst_inside$prev_anchor[ind1], df_rst_inside$prev_as99_xanchor[ind1]) # no difference
t.test(df_rst_inside$prev_anchor[ind2], df_rst_inside$prev_as99_xanchor[ind2]) #anchor much higher

t.test(df_rst$prev_as50, df_rst$prev_overall) 
t.test(df_rst$prev_as50, df_rst$prev_as99_xanchor) 
t.test(df_rst$prev_as50[df_rst$anchor_inside], df_rst$prev_overall[df_rst$anchor_inside])
t.test(df_rst$prev_as50[df_rst$timeprop_anchor < quantile(df_rst$timeprop_anchor, 0.2) & df_rst$anchor_inside], 
       df_rst$prev_overall[df_rst$timeprop_anchor < quantile(df_rst$timeprop_anchor, 0.2)& df_rst$anchor_inside]) 
t.test(df_rst$prev_as50[df_rst$n_grid_inside > quantile(df_rst$n_grid_inside, 0.5) & df_rst$anchor_inside], 
       df_rst$prev_overall[df_rst$n_grid_inside > quantile(df_rst$n_grid_inside, 0.5)& df_rst$anchor_inside])  

t.test(df_rst$prev_as50, df_rst$prev_inside) #as50<inside, not useful
t.test(df_rst$prev_as50[df_rst$anchor_inside], df_rst$prev_inside[df_rst$anchor_inside])
t.test(df_rst$prev_as50[!df_rst$anchor_inside], df_rst$prev_inside[!df_rst$anchor_inside])

t.test(df_rst$prev_as50[df_rst$timeprop_anchor > quantile(df_rst$timeprop_anchor, 0.5) & df_rst$anchor_inside], 
       df_rst$prev_inside[df_rst$timeprop_anchor > quantile(df_rst$timeprop_anchor, 0.5)& df_rst$anchor_inside])

m1 = lm(prev_inside ~ n_grid_inside + Sex+ Age.x + timeprop_inside + timeprop_anchor, data = df_rst[ind2,])
summary(m1)
m2 = lm(prev_inside ~ log(n_grid_inside) + Sex+ Age.x + timeprop_inside + timeprop_anchor, data = df_rst[df_rst$anchor_inside,])
summary(m2)
cor(df_rst$n_grid_inside, df_rst$prev_inside)


df_rst %>%
  dplyr::select(IIntID, prev_inside, prev_outside, prev_overall) %>%
  pivot_longer(cols = starts_with("prev_"),
               names_to = "prev_type",
               values_to = "prev_value")%>%
  mutate(IIntID = factor(IIntID, 
                         levels = df_rst %>% 
                           arrange(prev_inside) %>%
                           pull(IIntID))) %>%# Reorder IIntID factor based on prev_overall
  ggplot(aes(x = factor(IIntID), y = prev_value, color = prev_type, group = prev_type)) +
  #geom_line() +
  geom_point() +
  labs(x = "IIntID", y = "Prev Value", color = "Type of Prev") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

par(mfrow = c(2, 2))
hist(df_rst$prev_anchor)
hist(df_rst$prev_inside)
hist(df_rst$prev_outside)
hist(df_rst$prev_overall)

ggplot(df_rst, aes(x=timeprop_inside, y = prev_overall)) +
  geom_point()

cor(df_rst$timeprop_inside, df_rst$prev_overall)
cor(df_rst$timeprop_inside, df_rst$prev_inside)

summary_day <- df_rst %>%
  group_by(anchor_inside, anchor_inside_day, anchor_inside_night)%>%
  summarise(n = n())
summary_wk <- df_rst %>%
  group_by(anchor_inside, anchor_inside_wkday, anchor_inside_wkend)%>%
  summarise(n = n())
summary_tbl <- df_rst %>%
  group_by(anchor_inside, anchor_inside_wkday, anchor_inside_wkend, anchor_inside_day, anchor_inside_night)%>%
  summarise(n = n(),
            prev_inside_avg = mean(prev_inside, na.rm = TRUE),
            prev_inside_sd = sd(prev_inside, na.rm = TRUE),
            prev_outside_avg = mean(prev_outside, na.rm = TRUE),
            prev_outside_sd = sd(prev_outside, na.rm = TRUE),
            prev_overall_avg = mean(prev_overall, na.rm = TRUE),
            prev_overall_sd = sd(prev_overall, na.rm = TRUE)) %>%
  filter(n>=5)


# 1. outside prev > overall/inside: 4273 (anchor loc at night is outside)
# 2. spend most time in anchor (inside contour), most have high prev >30
#     -> inside prev <30 (anchor low prevalence): 75876 , 87727
# 3. if anchor loc is inside, |prev_inside - prev_anchor| <1
#     except 83860: prev_anchor (36), prev_inside (33), n_grid (55th), high prev during daytime, low at night

temp = df_rst %>%
  mutate(prev_day_diff = abs(prev_day-prev_night)) %>%
  dplyr::select(IIntID, Sex, prev_day, prev_night, prev_day_diff, anchor_inside, 
                anchor_inside_day, anchor_inside_night, prev_overall, timeprop_day, timeprop_inside)%>%
  filter(anchor_inside& anchor_inside_day & anchor_inside_night,
         prev_day_diff>=2) 
# for large prev day/night difference
# male: night > day
# female: day > night

temp = df_rst %>%
  mutate(prev_wk_diff = abs(prev_wkday-prev_wkend)) %>%
  dplyr::select(IIntID, Sex, prev_wkday, prev_wkend, prev_wk_diff, anchor_inside, 
                anchor_inside_wkday, anchor_inside_wkend, prev_overall, timeprop_wkday, timeprop_inside) %>%
  filter(anchor_inside& anchor_inside_wkend & anchor_inside_wkday,
         prev_wk_diff>=2) %>%
  arrange(prev_wk_diff)

temp = df_rst %>%
  #dplyr::select(num_grid, prev_inside, prev_outside, prev_overall, IIntID)
  filter(prev_overall-prev_inside < -5) # if spend a lot of time outside, then overall prevalence tend to be lower than inside prevalence

ggplot(df_rst, aes(x = distance_inside/time_inside, y = prev_inside)) +
  geom_point()+
  geom_line()
cor(df_rst$distance_inside/df_rst$time_inside, df_rst$prev_inside)
cor(df_rst$distance_inside, df_rst$prev_inside)
cor(df_rst$n_day_inside, df_rst$prev_inside)

ggplot(df_rst, aes(x = n_grid_inside, y = prev_inside)) +
  geom_point()+
  geom_line()
cor(df_rst$n_grid_inside, df_rst$prev_inside)

df_anchor_outside = df_rst[!df_rst$anchor_inside,]
ggplot(df_rst[!df_rst$anchor_inside,], aes(x=timeprop_inside, y = prev_overall)) +
  geom_point()+
  geom_line()
cor(df_rst[!df_rst$anchor_inside,]$timeprop_inside, df_rst[!df_rst$anchor_inside,]$prev_overall)
df_rst[!df_rst$anchor_inside,]

summary(df_rst[!df_rst$anchor_inside,]$prev_wkday-df_rst[!df_rst$anchor_inside,]$prev_wkend)
summary(df_rst[df_rst$anchor_inside,]$prev_wkday-df_rst[df_rst$anchor_inside,]$prev_wkend)

summary(df_rst[!df_rst$anchor_inside,c("anchor_inside_wkend", "anchor_inside_wkday", "anchor_inside_night", "anchor_inside_day")])
with(df_rst[!df_rst$anchor_inside, ],
     table(anchor_inside_wkend, anchor_inside_wkday))
with(df_rst[!df_rst$anchor_inside, ],
     table(anchor_inside_night, anchor_inside_day))


cor(df_rst[,7:40], use = "pairwise.complete.obs")
library(corrplot)
corr_matrix <- cor(df_rst[,7:40], use = "pairwise.complete.obs")
corrplot(corr_matrix, method = "color", type = "upper", tl.col = "black")


