df = read.csv('marketing_campaign.csv', sep = "\t", header = T)
dfp = read.csv('marketing_campaign.csv', sep = "\t", header = T)
df <- na.omit(df)

df$Age = 2023 - df$Year_Birth
df$Year_Birth = NULL

columns <- c("MntWines", "MntFruits", "MntMeatProducts", 
             "MntFishProducts", "MntSweetProducts", "MntGoldProds", 
             "NumDealsPurchases", "NumWebPurchases", "NumCatalogPurchases", 
             "NumStorePurchases",  "Age", "Income")

outlier_thresholds <- function(dataframe, variable) {
  quartile1 <- quantile(dataframe[[variable]], 0.15,na.rm=TRUE)
  quartile3 <- quantile(dataframe[[variable]], 0.85,na.rm=TRUE)
  interquantile_range <- quartile3 - quartile1
  up_limit <- quartile3 + 1.5 * interquantile_range
  low_limit <- quartile1 - 1.5 * interquantile_range
  return(c(low_limit, up_limit))
}
replace_with_thresholds <- function(df, variable) {
  thresholds <- outlier_thresholds(df, variable)
  low_limit <- thresholds[1]
  up_limit <- thresholds[2]
  df <- df[!(df[[variable]] < low_limit | df[[variable]] > up_limit), ]
  return(df)
}

for (col in columns) {
  df <-replace_with_thresholds(df, col)
}
summary(df)
write.csv(df, "df_cleaned.csv", row.names = FALSE)
