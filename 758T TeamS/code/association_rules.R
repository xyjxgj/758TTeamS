df = read.csv('df_cleaned.csv')

quantiles <- quantile(df$Income, probs = c(0.33, 0.66))
##
df$Income <- ifelse(df$Income <= 38200, 'low', 
                          ifelse(df$Income >= 58679, 'high', 'median'))
# Kidhome
df$Kidhome <- ifelse(df$Kidhome == 0 , 'no', 'yes')
# Teenhome
df$Teenhome <- ifelse(df$Teenhome == 0 , 'no', 'yes')
# Complain
df$Complain <- ifelse(df$Complain == 0 , 'no', 'yes')
# Age
quantiles.age <- quantile(df$Age, probs = c(0.33, 0.66))
df$Age <- ifelse(df$Age <= 48 , 'young',
                 ifelse(df$Age >= 59, 'old', 'middle'))

library(data.table)
setnames(df, old = c('MntFruits','MntWines', 'MntMeatProducts', 'MntFishProducts',
                     'MntSweetProducts', 'MntGoldProds'), 
         new = c('Fruits','Wines', 'MeatProducts', 
                 'FishProducts',
                 'SweetProducts', 'GoldProds'))

df
# max amount spent
cnames <-c('Fruits','Wines', 'MeatProducts', 
           'FishProducts',
           'SweetProducts', 'GoldProds')
df$max_spent <- cnames[apply(df[cnames],1,which.max)]
#purchase media
setnames(df, old = c('NumWebPurchases', 'NumCatalogPurchases', 'NumStorePurchases'), 
         new = c('Web', 'Catalog', 'Store'))
cnames1 <- c('Web', 'Catalog', 'Store')
df$purchase_media <- cnames1[apply(df[cnames1],1,which.max)]


# Response
df$last_campaign <- ifelse(df$Response == 0 , 'no', 'yes')

# deals
df$deals <- ifelse(df$NumDealsPurchases >=median(df$NumDealsPurchases), 'prefer','notprefer')

# web visit
df$web_visits <- ifelse(df$NumWebVisitsMonth >=median(df$NumWebVisitsMonth), 'more-likely','less-likely') 
#education
df$Dt_Customer = NULL
df$Recency = NULL
df$Z_CostContact = NULL
df$Z_Revenue = NULL
df$AcceptedCmp1= NULL
df$AcceptedCmp2= NULL
df$AcceptedCmp3= NULL
df$AcceptedCmp4= NULL
df$AcceptedCmp5= NULL

cnames_drop <- c('NumDealsPurchases','Response','NumWebVisitsMonth')
df[cnames_drop] <- NULL
df[cnames] <- NULL
df[cnames1] <- NULL


library(arules)
library(arulesViz)

# make all columns into items
df[sapply(df, is.character)] <- lapply(df[sapply(df, is.character)], 
                                       as.factor)

trans <-transactions(df[-1])
inspect(trans)
itemFrequencyPlot(trans,topN=25,type="relative")

rules<-apriori(data=trans, parameter=list(supp=0.005,conf = 0.05),
               appearance = list(default="lhs",rhs="Age=old"),
               control = list(verbose=F))
rules<-sort(rules, decreasing=TRUE,by="confidence")
inspect(rules[1:10])
rules.norhs <- apriori(data=trans, parameter = list(supp = 0.001, conf = 0.05),control = list(verbose=F))
inspect(rules.norhs[1:5])
