df_comp <- function(df1,df2) {
  len_df <- min(nrow(df1),nrow(df2))
  df1 <- df1[1:len_df,]
  df2 <- df2[1:len_df,]
  
  compLAI <- df1$LAI-df2$LAI
  compW <- df1$W-df2$W
  df <- data.frame(DVS=df1$DVS,
                   LAI1=df1$LAI,
                   LAI2=df2$LAI,
                   Diff_LAI=compLAI,
                   W1=df1$W,
                   W2=df2$W,
                   Diff_W=compW)
  return(df)
}

