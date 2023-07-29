no_partitioning <- function(weather_data = "weather_dikopshof_2007.csv",
                            save_csv = FALSE,
                            ET = 125) {
  
  require(dplyr)
  setwd(dir = "data")
  weather <- read.csv(weather_data)
  setwd("..")
  
  ETmod <- ET
  ET <- ET + 1
  
  # constant factors
  T_b <- 2
  r_l <- 0.009
  k <- 0.7
  RUE <- 3
  p_l <-0.325 # or 0.5
  s_new <- 0.02
  T_b1 <- 2
  T_b2 <- 5
  P_T1 <- 800
  P_T2 <- 850
  SeedingDate = 101
  
  # simile factors
  Temperature <- weather$Tmean..C.
  Temperature <- Temperature[SeedingDate:(ETmod + SeedingDate)]
  Irradiation <- weather$Radiation..MJ.m2.
  Irradiation <- Irradiation[SeedingDate:(ETmod + SeedingDate)]
  T_e <- numeric(ET)
  r_g <- numeric(ET)
  l_par <- numeric(ET)
  
  # simile flows
  dLAI <- numeric(ET)
  dW <- numeric(ET)
  dD <- numeric(ET)
  
  # simile rectangle
  LAI <- numeric(ET)
  W <- numeric(ET)
  DVS <- numeric(ET)
  
  # rectangle start values
  LAI[1] <- 0.12
  W[1] <- 10
  DVS[1] <- 0
  
  Timestep <- seq(0,ETmod)
  
  for (n in 2:ET) {
    # DVS 
    if (DVS[n-1] < 1) {
      dD[n-1] <- max(0,(Temperature[n-1]-T_b1)/P_T1) }
    else {
      dD[n-1] <- max(0,(Temperature[n-1]-T_b2)/P_T2) }
    
    # dW and dW prepoc
    T_e[n-1] <- Temperature[n-1] - T_b
    r_g[n-1] <- T_e[n-1] * r_l
    
    l_par[n-1] <- Irradiation[n-1] * 0.5 * (1-exp(-k*LAI[n-1]))
    # dW
    if (DVS[n-1] <= 2) {
      dW[n-1] <- l_par[n-1] * RUE
    }
    else {
      dW[n-1] <- 0
    }
    
    # LAI calculation
    if (LAI[n-1] < 0.7 && DVS[n-1]< 0.3) {
      dLAI[n-1] <- r_g[n-1] * LAI[n-1] }
    else if(DVS[n-1] < 1){
      dLAI[n-1]<- s_new*p_l*dW[n-1]
    }
    else {
      dLAI[n-1] <- s_new * p_l * dW[n-1]  - 0.03 * LAI[n-1]
    }
    
    # Compartment calculation
    W[n] <- dW[n-1] + W[n-1]
    DVS[n] <- dD[n-1] + DVS[n-1]
    LAI[n] <- dLAI[n-1] + LAI[n-1]
  }
  
  df <- data.frame(Timestep, DVS, W, LAI, dW)
  
  if (save_csv) {
    setwd("results")
    write.csv2(df, "R_no_partitioning.csv")
    setwd("..")
  }
  return(df)
}
