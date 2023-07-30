source("scripts/no_partitioning.R")
source("scripts/partitioning.R")
source("scripts/partitioning_TRANRF.R")
source("scripts/utils.R")


R_no_partitioning <- no_partitioning()
#df_no_partitioning
R_partitioning <- partitioning()
#df_partitioning
R_partitioning_TRANRF <- partitioning_TRANRF_model()
#df_partitioning_TRANRF

setwd(dir = "results")
Simile_no_partitioning <- read.csv("Simile_no_partitioning.csv")
Simile_partitioning <- read.csv("Simile_partitioning_wheat.csv")
Simile_partitioning_TRANRF <- read.csv("Simile_partitioning_TRANRF_wheat.csv")
setwd("..")


Simile_no_partitioning == R_no_partitioning


comp <- df_comp(Simile_partitioning_TRANRF,R_partitioning_TRANRF)
comp

df1 <- read.csv2("results/R_partitioning_TRANRF_wheat.csv")
df2 <- read.csv2("results/R_partitioning_wheat.csv")

len_df <- min(nrow(df1),nrow(df2))
df1 <- df1[1:len_df,]
df2 <- df2[1:len_df,]

Root_Diff <- df1$FRoot - df2$FRoot
Leaves_Diff <- df1$FLeaves - df2$FLeaves
Stem_Diff <- df1$FStem - df2$FStem
Organ_Diff <- df1$FStorageOrgans - df2$FStorageOrgans
df <- data.frame(DVS=df1$DVS,
                 Root_Diff=Root_Diff,
                 Leaves_Diff=Leaves_Diff,
                 Stem_Diff=Stem_Diff,
                 Organ_Diff=Organ_Diff)

dataO <- df[1:125,]
# dataO$DVS <- as.numeric(dataO$DVS)
# dataO$FRoot <- as.numeric(dataO$FRoot)
# dataO$FLeaves <- as.numeric(dataO$FLeaves)
# dataO$FStems <- as.numeric(dataO$FStems)
# dataO$FStorageOrgans <- as.numeric(dataO$FStorageOrgans)

dvsStorageOrgans <- select(dataO, DVS, Organ_Diff) %>% 
  mutate(newcol="Storage Organs") %>% 
  rename(Fraction=Organ_Diff)
dvsStems <- select(dataO, DVS, Stem_Diff) %>% 
  mutate(newcol="Stems") %>% 
  rename(Fraction=Stem_Diff)
dvsLeaves <- select(dataO, DVS, Leaves_Diff) %>% 
  mutate(newcol="Leaves") %>% 
  rename(Fraction=Leaves_Diff)
dvsRoots <- select(dataO, DVS, Root_Diff) %>% 
  mutate(newcol="Roots") %>% 
  rename(Fraction=Root_Diff)

dataO <- rbind(dvsRoots,dvsLeaves, dvsStems, dvsStorageOrgans) %>% 
  rename(Organ=newcol) %>% 
  mutate(Organ = factor(Organ, levels = c("Storage Organs", "Stems", "Leaves", "Roots")))

dataO <- dataO[order(dataO$DVS),]


fractions<-ggplot(data= dataO, aes(DVS, Fraction, group=Organ, color = Organ)) + #geom_line(data2, mapping = aes(x=DVS, y=LAI/scaleFactor, color="LAI"),size=4, linetype=6) +
  geom_line(linewidth=4) +
  labs(y="Fractions")+
  #facet_wrap(vars(crop))+
  theme(#axis.title.x = element_text(size = 30), 
    #axis.text.x = element_text(size = 20),
    #axis.title.y.left=element_text(color="black", size= 30),
    #axis.text.y.left=element_text(color="black", size= 20),
    #axis.title.y.right=element_text(color="black", size= 30, angle = 90, 'vjust = -0.05'),
    #axis.text.y.right=element_text(color="black", size= 20),
    legend.key = element_rect(colour = "transparent", fill = "white"),
    legend.title = element_blank(),
    #legend.text = element_text(size=20),
    #legend.title = element_text(size = 25),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_rect(color = "black", fill = NA),
    #panel.border = element_blank(),
    strip.background = element_rect(color="black",fill="white"),
    #strip.text = element_text(size=20),
    legend.position = "bottom",
    panel.background = element_blank(),
    axis.line = element_line(colour = "black"))+
  scale_color_manual(values= c("goldenrod1","green4","yellowgreen","red4"),
                     labels=c("Storage Organs","Stems","Leaves","Roots"))

fractions
