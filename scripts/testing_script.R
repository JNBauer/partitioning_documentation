# Accessing the models and util-functions
source("scripts/no_partitioning.R")
source("scripts/partitioning.R")
source("scripts/partitioning_TRANRF.R")
source("scripts/utils.R")

# Loading our results as saved in the results directory
Simile_no_partitioning <- read.csv("results/Simile_no_partitioning.csv")
Simile_partitioning <- read.csv("results/Simile_partitioning_wheat.csv")
Simile_partitioning_TRANRF <- read.csv("results/Simile_partitioning_TRANRF_wheat.csv")
R_no_partitioning <- read.csv2("results/R_no_partitioning.csv")
R_partitioning <- read.csv("results/R_partitioning_wheat.csv")
R_partitioning_TRANRF <- read.csv("results/R_partitioning_TRANRF_wheat.csv")

# New results can be created, by running the model functions
new_R_no_partitioning <- no_partitioning(save_csv = FALSE)
#R_no_partitioning
new_R_partitioning_wheat <- partitioning(save_csv = FALSE)
#R_partitioning_wheat
new_R_partitioning_grain_maize <- partitioning(save_csv = FALSE, crop_name = "grain_maize")
#R_partitioning_grain_maize
new_R_partitioning_TRANRF <- partitioning_TRANRF(save_csv = FALSE)
#R_partitioning_TRANRF

# The df_comp function can be used to compare two result data frames
# and calculate the difference between LAI and W
comp <- df_comp(R_no_partitioning,new_R_no_partitioning)
comp
