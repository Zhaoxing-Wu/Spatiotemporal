source('data_preproc.R')

e <- new.env()# Copy only one function to global environment
source("auxi_funcs.R", local = e)
grid_in_meters <- e$grid_in_meters
rm(e)
#df_ret = df_rst
niceColors <- c("#F0DB94","#FDAE61", "#F46D43","#E54D35", "#D73027")
iintid_female = unique(df_ret$IIntID[df_ret$Sex=="FEM"])
iintid_male = unique(df_ret$IIntID[df_ret$Sex=="MAL"])

##--------------map-------------------------------------------------------------
crs = CRS("+init=epsg:4326")
load("before estimating prevalance 100 meters corrected/RSAPrevalenceShapefile.Rdata")
wkbl_shape <- st_zm(st_read("45 WKBLs_DSA/DSA_DKD_region.shp"), drop = TRUE)  # Remove Z dimension
wkbl_shape = st_transform(wkbl_shape, crs = st_crs(south_africa_shape))
grid_sf <- st_as_sf(grid_in_meters(wkbl_shape, 0.02, 100, crs))
grid_sf$grid_id = 1:nrow(grid_sf)

sa_sf = st_sf(geometry = st_sfc(st_union(south_africa_shape)), crs = crs)

# use rnaturalearth to get lesotho -> not fit the map
lesotho_around = c("Thabo Mofutsanyane", "Xhariep", "Joe Gqabi", "Mangaung", 
                        "Alfred Nzo", "Sisonke", "Umgungundlovu", "Uthukela")
lesotho_around_df = south_africa_shape[south_africa_shape$NAME_2 %in% lesotho_around,] #get all districts surrounding lesotho
lesotho_around_df <- st_union(lesotho_around_df)[[1]] #get the union of those districts
lesotho_around_df <- st_geometry(st_sf(geometry = st_sfc(st_polygon(lesotho_around_df[[1]])), crs = crs))#districts around lesotho, hollow circle
lesotho_sf <- st_sf(geometry = st_sfc(st_polygon(list(lesotho_around_df[[1]][[2]]))), crs = crs)#get the inner circle->lesotho
##----------plot functions-----------------
plot_time_map_inside = function(df, filename){
  ggplot(df) +
    geom_sf(aes(fill = log(time+exp(-15))), color = NA) +      
    scale_fill_gradientn(colours = niceColors, name = "Time",
                         limits = c(-15, 0),  breaks = c(-15, 0),
                         labels = c("Low", "High")) + 
    geom_sf(data = wkbl_shape, color ="black", fill = NA)+
    theme_minimal() +
    theme(panel.background = element_rect(fill = "white", colour = NA),
          plot.background  = element_rect(fill = "white", colour = NA))
  ggsave(filename,width = 5,height = 5)
}

plot_time_map_outside = function(df, filename){
  ggplot(df) +
    geom_sf(aes(fill = log(time+exp(-15))), color = "lightgrey") +
    scale_fill_gradientn(colours = niceColors, name = "Time Scale",
                         limits = c(-15, 0), breaks = c(-15, 0), 
                         labels = c("Low", "High"),na.value = "white") +
    geom_sf(data = wkbl_shape, fill = "dimgrey", color = NA)+
    geom_sf(data = lesotho_sf, fill = "dimgrey", color = "dimgrey") +
    geom_sf(data = sa_sf, color = "black", fill=NA) +
    theme_minimal()+
    theme(panel.background = element_rect(fill = "white", colour = NA),
          plot.background  = element_rect(fill = "white", colour = NA))
  ggsave(filename, width = 5, height = 3.5)
}

##------------plot time inside contour for gender groups------------------------
for (i in c(1, 2, 3)){ # 1:all, 2:f, 3:m
  if (i==1){
    filename = "all"
    df_filtered = df_prev
  }else if(i==2){
    filename = "fem"
    df_filtered = df_prev[df_prev$IIntID%in%iintid_female, ]
    grid_id_fem = unique(df_filtered$grid_id) #for plotting overlapping as between female and male
  }else if(i==3){
    filename = "mal"
    df_filtered = df_prev[df_prev$IIntID%in%iintid_male, ]
    grid_id_mal = unique(df_filtered$grid_id)
  }
  
  #----plot inside contour
  temp = df_filtered%>%
    filter(if_inside_contour)%>%
    group_by(grid_id)%>%
    summarise(time = sum(time_inside_grid))%>%
    arrange(desc(time))%>%
    mutate(time = time/sum(time),
           time_cum = cumsum(time),
           inside_as95 = time_cum<=max(0.95, first(time_cum)),
           inside_as65 = time_cum<=max(0.65, first(time_cum)))%>%
    left_join(grid_sf, by = "grid_id")
  plot_time_map_inside(st_as_sf(temp), paste("plot/time_inside_",filename,".png",sep=''))

  if (i==2){ #for plotting 65%/90% as later
    df_as_grid_fem = temp
    grid_id_fem65 = unique(temp[temp$inside_as65,]$grid_id)
    grid_id_fem95 = unique(temp[temp$inside_as95,]$grid_id)
  }else if(i==3){ 
    df_as_grid_mal = temp
    grid_id_mal65 = unique(temp[temp$inside_as65,]$grid_id)
    grid_id_mal95 = unique(temp[temp$inside_as95,]$grid_id)
  }
  
  #---plot outside contour
  if (i == 1 | i==2 | i==3){
    temp = df_filtered%>%
      filter(!if_inside_contour)%>%
      left_join(df_outside_contour, by = "grid_id")%>%
      group_by(NAME_2)%>%
      summarise(time =sum(time_inside_grid))%>%
      mutate(time = time/sum(time))%>%
      right_join(south_africa_shape)
    plot_time_map_outside(st_as_sf(temp), paste("plot/time_outside_",filename,".png",sep=''))
  }
}

##---plot overlapping as between males and females at as level 65, 90, 100------
for (i in c(1, 2, 3)){
  if (i == 1){
    df_fem = grid_sf[grid_sf$grid_id %in% grid_id_fem & !(grid_sf$grid_id %in% grid_id_mal),]
    df_mal = grid_sf[grid_sf$grid_id %in% grid_id_mal & !(grid_sf$grid_id %in% grid_id_fem),]
    df_both = grid_sf[grid_sf$grid_id %in% grid_id_fem & grid_sf$grid_id %in% grid_id_mal,]
    filename = "100"
  }else if (i == 2){
    df_fem = grid_sf[grid_sf$grid_id %in% grid_id_fem95 & !(grid_sf$grid_id %in% grid_id_mal95),]
    df_mal = grid_sf[grid_sf$grid_id %in% grid_id_mal95 & !(grid_sf$grid_id %in% grid_id_fem95),]
    df_both = grid_sf[grid_sf$grid_id %in% grid_id_fem95 & grid_sf$grid_id %in% grid_id_mal95,]
    filename = "95"
  }else{
    df_fem = grid_sf[grid_sf$grid_id %in% grid_id_fem65 & !(grid_sf$grid_id %in% grid_id_mal65),]
    df_mal = grid_sf[grid_sf$grid_id %in% grid_id_mal65 & !(grid_sf$grid_id %in% grid_id_fem65),]
    df_both = grid_sf[grid_sf$grid_id %in% grid_id_fem65 & grid_sf$grid_id %in% grid_id_mal65,]
    filename = "65"
  }
  
  df_all <- bind_rows(
    df_fem  %>% mutate(Sex= "Female"),
    df_mal  %>% mutate(Sex = "Male"),
    df_both %>% mutate(Sex = "Both")
  )
    
  if (i==1){ # for 100% as, the boundary of each grid is white
  ggplot() +
    geom_sf(data = df_all, aes(fill = Sex), color = "white", size = 0.1) +  # white borders
    geom_sf(data = wkbl_shape, fill = NA, color = "black") +      # outer outline
    scale_fill_manual(values = c("Female" = "#e41a1c", "Male" = "#377eb8", "Both" = "grey")) +
    theme_minimal() +
    theme(
      panel.background = element_rect(fill = "white", colour = NA),
      plot.background  = element_rect(fill = "white", colour = NA)
    )
  ggsave(paste("plot/grid_as",filename,".png",sep=''),width = 5,height = 5)
  }else{
  ggplot() +
    geom_sf(data = df_all, aes(fill = Sex, color = Sex)) +  # white borders
  geom_sf(data = wkbl_shape, fill = NA, color = "black") +      # outer outline
  scale_fill_manual(values = c("Female" = "#e41a1c", "Male" = "#377eb8", "Both" = "grey")) +
  scale_color_manual(values = c("Female" = "#e41a1c", "Male" = "#377eb8", "Both" = "grey")) +
  theme_minimal() +
  theme(
    panel.background = element_rect(fill = "white", colour = NA),
    plot.background  = element_rect(fill = "white", colour = NA)
  )
  ggsave(paste("plot/grid_as",filename,".png",sep=''),width = 5,height = 5)
  }
}

##------plot individual as------------------------------------------------------
for (i in c(1, 2)){
  if(i==1){
    filename = "fem"
    df_filtered = df_prev[df_prev$IIntID%in%iintid_female, ]
    iintid_list = iintid_female
  }else{
    filename = "mal"
    df_filtered = df_prev[df_prev$IIntID%in%iintid_male, ]
    iintid_list = iintid_male
  }
  
  for (iintid in iintid_list){
    #----plot inside contour
    temp = df_filtered%>%
      filter(if_inside_contour,
             IIntID == iintid)%>%
      group_by(grid_id)%>%
      summarise(time = sum(time_inside_grid))%>%
      arrange(desc(time))%>%
      mutate(time = time/sum(time))%>%
      left_join(grid_sf, by = "grid_id")
    plot_time_map_inside(st_as_sf(temp), paste("plot/as/time_inside_",filename,"_", iintid, ".png",sep=''))
  }
}

##-------------
plot_time_map_inside = function(iintid, filename){
  temp = df_prev%>%
    filter(if_inside_contour,
           IIntID %in% iintid)%>%
    group_by(grid_id)%>%
    summarise(time = sum(time_inside_grid))%>%
    arrange(desc(time))%>%
    mutate(time = time/sum(time),
           time_cum = cumsum(time),
           inside_as95 = time_cum<=max(0.95, first(time_cum)),
           inside_as65 = time_cum<=max(0.65, first(time_cum)))%>%
    #filter(inside_as65) %>%
    left_join(grid_sf, by = "grid_id")
  ggplot(st_as_sf(temp)) +
    geom_sf(aes(fill = log(time+exp(-15))), color = NA) +      
    scale_fill_gradientn(colours = niceColors, name = "Time",
                         limits = c(-15, 0),  breaks = c(-15, 0),
                         labels = c("Low", "High")) + 
    geom_sf(data = wkbl_shape, color ="black", fill = NA)+
    theme_minimal() +
    theme(panel.background = element_rect(fill = "white", colour = NA),
          plot.background  = element_rect(fill = "white", colour = NA))
  ggsave(filename,width = 5,height = 5)
}

plot_time_map_inside(iintid_cls_prev_h, "plot/map_time_cls_prev_h.png")
plot_time_map_inside(iintid_cls_prev_m, "plot/map_time_cls_prev_m.png")
plot_time_map_inside(iintid_cls_prev_l, "plot/map_time_cls_prev_l.png")

plot_time_map_inside(iintid_cls_prev_stand_h, "plot/map_time_cls_prev_stand_h.png")
plot_time_map_inside(iintid_cls_prev_stand_m, "plot/map_time_cls_prev_stand_m.png")
plot_time_map_inside(iintid_cls_prev_stand_l, "plot/map_time_cls_prev_stand_l.png")

plot_time_map_inside(iintid_cls_ngrid_h, "plot/map_time_cls_ngrid_h.png")
plot_time_map_inside(iintid_cls_ngrid_l, "plot/map_time_cls_ngrid_l.png")
plot_time_map_inside(iintid_cls_ngrid_h[iintid_cls_ngrid_h%in%iintid_female], 
                     "plot/map_time_cls_ngrid_h_fem.png")
plot_time_map_inside(iintid_cls_ngrid_l[iintid_cls_ngrid_l%in%iintid_female], 
                     "plot/map_time_cls_ngrid_l_fem.png")
plot_time_map_inside(iintid_cls_ngrid_h[iintid_cls_ngrid_h%in%iintid_male],
                     "plot/map_time_cls_ngrid_h_mal.png")
plot_time_map_inside(iintid_cls_ngrid_l[iintid_cls_ngrid_l%in%iintid_male],
                     "plot/map_time_cls_ngrid_l_mal.png")
sum(iintid_cls_ngrid_h%in%iintid_female)/length(iintid_female)
sum(iintid_cls_ngrid_h%in%iintid_male)/length(iintid_male)
