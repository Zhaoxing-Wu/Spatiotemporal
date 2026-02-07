source('data_prev.R')
library(paletteer)
niceColors = paletteer_c("ggthemes::Classic Orange-Blue", 30)[30:1] #color for the prevalence level

e <- new.env()# Copy only one function to global environment
source("auxi_funcs.R", local = e)
grid_in_meters <- e$grid_in_meters
rm(e)

iintid_female = unique(df_ret$IIntID[df_ret$Sex=="FEM"])
iintid_male = unique(df_ret$IIntID[df_ret$Sex=="MAL"])

##--------------map-------------------------------------------------------------
crs = CRS("+init=epsg:4326")
load("before estimating prevalance 100 meters corrected/RSAPrevalenceShapefile.Rdata")
wkbl_shape <- st_zm(st_read("45 WKBLs_DSA/DSA_DKD_region.shp"), drop = TRUE)  # Remove Z dimension
wkbl_shape = st_transform(wkbl_shape, crs = st_crs(south_africa_shape))

sa_sf = st_sf(geometry = st_sfc(st_union(south_africa_shape)), crs = crs)

grid_sf <- st_as_sf(grid_in_meters(wkbl_shape, 0.02, 100, crs))
grid_sf$grid_id = 1:nrow(grid_sf)

##-----------plot activity space colored by the prevalence level of each grid---
prev_max = max(df_prev[df_prev$grid_id>0,]$prev_esti)
prev_min = min(df_prev[df_prev$grid_id>0,]$prev_esti)

plot_prev_map_inside = function(df, iintid, filename){
  temp = df[df$IIntID%in%iintid, ] %>%
    filter(if_inside_contour)%>%
    group_by(grid_id)%>%
    slice(1) %>%
    left_join(grid_sf, by = "grid_id")
  ggplot(st_as_sf(temp)) +
    geom_sf(aes(fill = prev_esti), color = NA) +      
    scale_fill_gradientn(colours = niceColors, name = "Exposure",
                         limits = c(prev_min, prev_max)) + 
    geom_sf(data = wkbl_shape, color ="black", fill = NA)+
    theme_minimal() +
    theme(panel.background = element_rect(fill = "white", colour = NA),
          plot.background  = element_rect(fill = "white", colour = NA))
  ggsave(filename,width = 5,height = 5)
}

df_rst_inside = df_rst[df_rst$anchor_inside,]
idx_anchorin_high <- order(df_rst_inside$prev_as50 - df_rst_inside$prev_inside, 
                           decreasing = TRUE)[1:3]
idx_anchorin_low <- order(df_rst_inside$prev_as50 - df_rst_inside$prev_inside,
                          decreasing = FALSE)[1:3]
summary(df_rst_inside[idx_anchorin_high,]$Age.x)
summary(df_rst_inside[idx_anchorin_low,]$Age.x)
summary(df_rst_inside[idx_anchorin_high,]$Sex)
summary(df_rst_inside[idx_anchorin_low,]$Sex)
summary(df_rst_inside[idx_anchorin_high,]$timeprop_anchor)
summary(df_rst_inside[idx_anchorin_low,]$timeprop_anchor)
summary(df_rst_inside[idx_anchorin_high,]$prev_as50)
summary(df_rst_inside[idx_anchorin_low,]$prev_as50)
summary(df_rst_inside[idx_anchorin_high,]$prev_inside)
summary(df_rst_inside[idx_anchorin_low,]$prev_inside)

plot_prev_map_inside(df, df_rst_inside[idx_anchorin_high,]$IIntID,
                     paste("plot/map_prev_anchorin_high",".png",sep=''))
plot_prev_map_inside(df, df_rst_inside[idx_anchorin_low,]$IIntID,
                     paste("plot/map_prev_anchorin_low",".png",sep=''))
##-----------
plot_prev_map_inside(df, iintid_cls_ngrid_h[iintid_cls_ngrid_h%in%iintid_female],
                     "plot/map_prev_cls_ngrid_h_fem.png")
plot_prev_map_inside(df, iintid_cls_ngrid_l[iintid_cls_ngrid_l%in%iintid_female],
                     paste("plot/map_prev_cls_ngrid_l_fem",".png",sep=''))
plot_prev_map_inside(df, iintid_cls_ngrid_h[iintid_cls_ngrid_h%in%iintid_male],
                     "plot/map_prev_cls_ngrid_h_mal.png")
plot_prev_map_inside(df, iintid_cls_ngrid_l[iintid_cls_ngrid_l%in%iintid_male],
                     "plot/map_prev_cls_ngrid_l_mal.png")

plot_prev_map_inside(df, iintid_cls_ngrid_h,
                     "plot/map_prev_cls_ngrid_h.png")
plot_prev_map_inside(df, iintid_cls_ngrid_l,
                     "plot/map_prev_cls_ngrid_l.png")

plot_prev_map_inside(df, iintid_cls_prev_h,
                     "plot/map_prev_cls_prev_h.png")
plot_prev_map_inside(df, iintid_cls_prev_m,
                     "plot/map_prev_cls_prev_m.png")
plot_prev_map_inside(df, iintid_cls_prev_l,
                     "plot/map_prev_cls_prev_l.png")
plot_prev_map_inside(df, iintid_cls_prev_stand_h, 
                     "plot/map_prev_cls_prev_stand_h.png")
plot_prev_map_inside(df, iintid_cls_prev_stand_m,
                     "plot/map_prev_cls_prev_stand_m.png")
plot_prev_map_inside(df, iintid_cls_prev_stand_l,
                     "plot/map_prev_cls_prev_stand_l.png")
