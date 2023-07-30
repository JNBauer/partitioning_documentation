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
df


