melt_formatter <- function(area_df, crop_name) {
  area_df <- area_df[1:125,]
  
  LAI_df <- select(area_df, DVS, LAI)
  if ("WRoots" %in% colnames(area_df)) {
    select(area_df, c(DVS, WRoots, WLeaves, WStems, WStorageOrgans,LAI))
    
    area_df$DVS <- as.numeric(area_df$DVS)
    area_df$WRoots <- as.numeric(area_df$WRoots)
    area_df$WLeaves <- as.numeric(area_df$WLeaves)
    area_df$WStems <- as.numeric(area_df$WStems)
    area_df$WStorageOrgans <- as.numeric(area_df$WStorageOrgans)
    
    dvsStorageOrgans <- select(area_df, DVS, WStorageOrgans) %>% 
      mutate(newcol="Storage Organs") %>% 
      rename(Weight=WStorageOrgans)
    dvsStems <- select(area_df, DVS, WStems) %>% 
      mutate(newcol="Stems") %>% 
      rename(Weight=WStems)
    dvsLeaves <- select(area_df, DVS, WLeaves) %>% 
      mutate(newcol="Leaves") %>% 
      rename(Weight=WLeaves)
    dvsRoots <- select(area_df, DVS, WRoots) %>% 
      mutate(newcol="Roots") %>% 
      rename(Weight=WRoots)
    
    area_df <- rbind(dvsRoots,dvsLeaves, dvsStems, dvsStorageOrgans) %>% 
      rename(Organ = newcol) %>% 
      mutate(Organ = factor(Organ, levels = c("Storage Organs", "Stems", "Leaves", "Roots")))
  } else {
    area_df <- area_df %>%  select(DVS, W) %>% 
      mutate(newcol="W") %>% 
      rename(Weight=W) %>% 
      rename(Organ = newcol) %>% 
      mutate(Organ = factor(Organ, levels = c("W")))
  }
  area_df <- area_df[order(area_df$DVS),]
  LAI_df <- mutate(LAI_df,crop=crop_name)
  area_df <- mutate(area_df,crop=crop_name)

  
  return(list(area_df,LAI_df,crop_name))
}
  
area_plotter <- function(data,data2, crop_list) {
  if (length(unique(data$Organ)) > 1) {
    color_list = c("goldenrod1","green4","yellowgreen","red4")
  } else {
    color_list = "lightblue"
  }
  
  scaleFactor <- 6/2300
  area_plot <- ggplot() + 
    geom_area(data, mapping = aes(x=DVS, y=Weight, fill=Organ)) +
    geom_line(data2, mapping = aes(x=DVS, y=LAI/scaleFactor, color="LAI"),size=4, linetype=6) +
    scale_fill_manual(values = color_list) +
    facet_wrap(nrow=1,ncol=3, ~factor(crop, levels=crop_list)) +
    scale_color_manual(name = "Group", values = c( "LAI" = "darkblue"), labels = c("LAI")) +
    scale_y_continuous(name = "Biomass [g/m²]", sec.axis = sec_axis( trans=~.*scaleFactor, name="LAI")) +
    theme_bw() +
    theme(
      #axis.title.x = element_text(size = 30), 
      #axis.text.x = element_text(size = 20),
      #axis.title.y.left=element_text(color="black", size= 30),
      #axis.text.y.left=element_text(color="black", size= 20),
      #axis.title.y.right=element_text(color="black", size= 30, angle = 90, 'vjust = -0.05'),
      #axis.text.y.right=element_text(color="black", size= 20),
      legend.key = element_rect(colour = "transparent", fill = "white"),
      legend.title = element_blank(),
      #legend.text = element_text(size=30),
      #legend.title = element_text(size = 25),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      panel.border = element_rect(colour = "black", fill = NA),
      #panel.border = element_blank(),
      strip.background = element_rect(fill="white"),
      #strip.text = element_text(size=30),
      legend.position = "right",
      panel.background = element_blank(),
      axis.line = element_line(colour = "black"))

  
  # if (length(unique(data$crop)) > 1 & length(unique(data$crop)) < 5) {
  #   ncol = length(unique(data$crop))
  #   final_area_plot <- area_plot +
  #   facet_wrap(nrow=1, ~factor(crop, levels=crop_list))
  #   
  # } else if (length(unique(data$crop)) > 5 & length(unique(data$crop)) < 9) {
  #   ncol = length(unique(data$crop))
  #   final_area_plot <- area_plot +
  #     facet_wrap(nrow=2, ~factor(crop, levels=crop_list))
  #   
  # } else {
  #   final_area_plot <- area_plot +
  #       facet_wrap(nrow=3, ~factor(crop, levels=crop_list))
  # }
  #ggsave(paste(plotfilename,".png"),area_plot, width = 35, height = 40, units = "cm", dpi = 500)
  #ggsave(paste(plotfilename,".svg"),area_plot, width = 35, height = 35, units = "cm", dpi = 500)
  return(area_plot)
}
