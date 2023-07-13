partitioning_model <- function(weather_data = "weather_dikopshof_2007.csv",
                               crop_name = "wheat",
                               save_csv = TRUE,
                               ET = 126) {
  setwd(dir = "data")
  fractions <- read.csv2("all_fractions_.csv")
  fractions <- filter(fractions,Crop==crop_name)

  weather <- read.csv(weather_data)
  setwd("..")

  Root_fractions <- filter(fractions,Organ=="Roots")
  Stems_fractions <- filter(fractions,Organ=="Stems")
  Leaves_fractions <- filter(fractions,Organ=="Leaves")
  StorageOrgans_fractions <- filter(fractions,Organ=="StorageOrgans")
  
  # constant factors
  T_b <- 2
  r_l <- 0.009
  k <- 0.7
  RUE <- 3
  p_l <- 0.5
  s_new <- 0.02
  
  T_b1 <- 2
  T_b2 <- 5
  P_T1 <- 800
  P_T2 <- 850
  SeedingDate = 101
  
  # simile factors
  Temperature <- weather$Tmean..C.
  Temperature <- Temperature[SeedingDate:(ET+SeedingDate-1)]
  Irradiation <- weather$Radiation..MJ.m2.
  Irradiation <- Irradiation[SeedingDate:(ET+SeedingDate-1)]
  
  T_e <- numeric(ET)
  r_g <- numeric(ET)
  l_par <- numeric(ET)
  
  FRoot <- numeric(ET)
  fshoot <- numeric(ET)
  FLeaves <- numeric(ET)
  FStems <- numeric(ET)
  FStorageOrgans <- numeric(ET)
  FAll <- numeric(ET)
  
  # simile flows
  dLAI <- numeric(ET)
  dW <- numeric(ET)
  dD <- numeric(ET)
  dWRoots <- numeric(ET)
  dWLeaves <- numeric(ET)
  dWStems <- numeric(ET)
  dWStorageOrgans <- numeric(ET)
  
  # simile rectangle
  LAI <- numeric(ET)
  W <- numeric(ET)
  DVS <- numeric(ET)
  
  WRoots <- numeric(ET)
  WLeaves <- numeric(ET)
  WStems <- numeric(ET)
  WStorageOrgans <- numeric(ET)
  
  # rectangle start values
  LAI[1] <- 0.12
  W[1] <- 10
  DVS[1] <- 0
  
  WRoots[1] <- 5
  WLeaves[1] <- 3.25
  WStems[1] <- 1.25
  WStorageOrgans[1] <- 0
  
  for (n in 2:ET)
  {
    # DVS 
    if (DVS[n-1] < 1) {
      dD[n-1] <- max(0,(Temperature[n-1]-T_b1)/P_T1) }
    else {
      dD[n-1] <- max(0,(Temperature[n-1]-T_b2)/P_T2) }
    
    # dW and dW prepoc
    T_e[n-1] <- Temperature[n-1] - T_b
    r_g[n-1] <- T_e[n-1] * r_l
    
    l_par[n-1] <- Irradiation[n-1] * 0.5 * (1-exp(-k*LAI[n-1]))
    
    if (DVS[n-1] <= 2) {
      dW[n-1] <- l_par[n-1] * RUE
    }
    else {
      dW[n-1] <- 0
    }
    
    # Fraction partitioning
    Root_frac <- approx(Root_fractions$DVS, Root_fractions$Fraction, xout=DVS[n-1])
    
    #FRoot[n-1] <- Root_frac$y[1] * froot[n-1]
    FRoot[n-1] <- Root_frac$y[1]
    fshoot[n-1] <- 1-FRoot[n-1]
    Stems_frac <- approx(Stems_fractions$DVS, Stems_fractions$Fraction, xout=DVS[n-1])
    FStems[n-1] <- fshoot[n-1] * Stems_frac$y[1]
    Leaves_frac <- approx(Leaves_fractions$DVS, Leaves_fractions$Fraction, xout=DVS[n-1])
    FLeaves[n-1] <- fshoot[n-1] * Leaves_frac$y[1]
    StorageOrgans_frac <- approx(StorageOrgans_fractions$DVS, StorageOrgans_fractions$Fraction, xout=DVS[n-1])
    FStorageOrgans[n-1] <- fshoot[n-1] * StorageOrgans_frac$y[1]
    
    FAll[n-1] <- FRoot[n-1] + FStems[n-1] + FLeaves[n-1] + FStorageOrgans[n-1]
    
    # Organ growth
    dWRoots[n-1] <- FRoot[n-1] * dW[n-1]
    dWStems[n-1] <- FStems[n-1] * dW[n-1]
    dWStorageOrgans[n-1] <- FStorageOrgans[n-1] * dW[n-1]
    dWLeaves[n-1] <- FLeaves[n-1] * dW[n-1]
    
    #LAI calculation
    if (LAI[n-1] < 0.7 & DVS[n-1]< 0.3) {
      dLAI[n-1] <- r_g[n-1] * LAI[n-1] }
    else if(DVS[n-1] < 1){
      dLAI[n-1] <- s_new * dWLeaves[n-1]
    }  else {
      dLAI[n-1] <- s_new * dWLeaves[n-1]  - 0.03 * LAI[n-1]
    }
    
    # Compartment calculation
    W[n] <- dW[n-1] + W[n-1]
    DVS[n] <- dD[n-1] + DVS[n-1]
    LAI[n] <- dLAI[n-1] + LAI[n-1]
    
    WRoots[n] <- dWRoots[n-1] + WRoots[n-1]
    WLeaves[n] <- dWLeaves[n-1] + WLeaves[n-1]
    WStems[n] <- dWStems[n-1] + WStems[n-1]
    WStorageOrgans[n] <- dWStorageOrgans[n-1] + WStorageOrgans[n-1]
  }
  
  #df <- data.frame(DVS,LAI, W, WRoots, WLeaves, WStems, WStorageOrgans, dWRoots, dWLeaves, dWStems, dWStorageOrgans, FRoot, FLeaves, FStems, FStorageOrgans, fshoot, FAll, Temperature, Irradiation)
  df <- data.frame(DVS, W, dW, LAI, WRoots,WLeaves, WStems, WStorageOrgans, FRoot, FLeaves, FStems, FStorageOrgans)
  filename <- paste("partitioning_",crop_name,".csv", sep = "")
  if (save_csv == TRUE) {
    setwd("data")
    write.csv2(df,filename)
    setwd("..")}
  return(df)
}
