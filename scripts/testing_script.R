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


