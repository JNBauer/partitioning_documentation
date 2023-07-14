source("scripts/partitioning_TRANRF.R")
library(dplyr)



# crop_name <- "wheat"
# setwd(dir = "data")
# fractions <- read.csv2("all_fractions_.csv") %>%
#   filter(Crop==crop_name)
# 
# setwd("..")
# fractions

x = 1 * 1

df_partitioning_TRANRF <- partitioning_TRANRF_model(TRANRF_section = c(0.7,1.1))
df_partitioning_TRANRF

setwd(dir = "data")
partitioning_sml <- read.csv("partitioning_gk.csv")
setwd("..")
partitioning_sml



df_comp <- function(df1,df2) {
  
  len_df <- min(nrow(df1),nrow(df2))
  df1 <- df1[1:len_df,]
  df2 <- df2[1:len_df,]
  
  comp <- df1$LAI-df2$LAI
  df <- data.frame(DVS=df1$DVS,
                   LAI1=df1$LAI,
                   LAI2=df2$LAI,
                   Diff=comp)
  ggplot(df, aes(x=DVS,y=Diff)) +
    geom_line()
  
  return(df)
}

comp <- df_comp(df_partitioning,partitioning_sml)
comp

ggplot(comp, aes(x=DVS,y=Diff)) +
  geom_point()
