csv_melt_formatter <- function(input_path, crop_name) {
  data_second <- read.csv2(input_path)%>% 
    select(c(DVS, WRoots, WLeaves, WStems, WStorageOrgans,LAI))
  data_second <- data_second[1:127,]
  data_second2 <- select(data_second, DVS, LAI)
  data_second$DVS <- as.numeric(data_second$DVS)
  data_second$WRoots <- as.numeric(data_second$WRoots)
  data_second$WLeaves <- as.numeric(data_second$WLeaves)
  data_second$WStems <- as.numeric(data_second$WStems)
  data_second$WStorageOrgans <- as.numeric(data_second$WStorageOrgans)
  
  dvsStorageOrgans <- select(data_second, DVS, WStorageOrgans) %>% 
    mutate(newcol="Storage Organs") %>% 
    rename(Weight=WStorageOrgans)
  dvsStems <- select(data_second, DVS, WStems) %>% 
    mutate(newcol="Stems") %>% 
    rename(Weight=WStems)
  dvsLeaves <- select(data_second, DVS, WLeaves) %>% 
    mutate(newcol="Leaves") %>% 
    rename(Weight=WLeaves)
  dvsRoots <- select(data_second, DVS, WRoots) %>% 
    mutate(newcol="Roots") %>% 
    rename(Weight=WRoots)
  
  data_second <- rbind(dvsRoots,dvsLeaves, dvsStems, dvsStorageOrgans) %>% 
    rename(Organ = newcol) %>% 
    mutate(Organ = factor(Organ, levels = c("Storage Organs", "Stems", "Leaves", "Roots")))
  
  data_second <- data_second[order(data_second$DVS),]
  
  data_second2 <- mutate(data_second2,crop=crop_name)
  data_second <- mutate(data_second,crop=crop_name)
  
  return(list(data_second,data_second2,crop_name))
}



area_plotter <- function(data,data2) {
  scaleFactor <- 6/2300
  area_plot <- ggplot() + 
    geom_area(data, mapping = aes(x=DVS, y=Weight, fill=Organ)) +
    geom_line(data2, mapping = aes(x=DVS, y=LAI/scaleFactor, color="LAI"),size=4, linetype=6) +
    scale_fill_manual(values = c("goldenrod1","green4","yellowgreen","red4")) +
    scale_color_manual(name = "Group", values = c( "LAI" = "darkblue"), labels = c("LAI")) +
    scale_y_continuous(name = "Biomass [g/m²]", sec.axis = sec_axis( trans=~.*scaleFactor, name="LAI")) +
    #facet_wrap(nrow=2,ncol=4, ~factor(crop, levels=crop_list)) +
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
  plotfilename = "comparison"
  
  #ggsave(paste(plotfilename,".png"),area_plot, width = 35, height = 40, units = "cm", dpi = 500)
  #ggsave(paste(plotfilename,".svg"),area_plot, width = 35, height = 35, units = "cm", dpi = 500)
  return(area_plot)
}