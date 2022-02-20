# Clustering-Credit-Card-User-Behavior

CC = read.csv("pathway", head = TRUE)
table(is.na(CC$CUST_ID))
table(is.na(CC$BALANCE))
table(is.na(CC$BALANCE_FREQUENCY))
table(is.na(CC$PURCHASES))
table(is.na(CC$ONEOFF_PURCHASES))
table(is.na(CC$INSTALLMENTS_PURCHASES))
table(is.na(CC$CASH_ADVANCE))
table(is.na(CC$PURCHASES_FREQUENCY))
table(is.na(CC$ONEOFF_PURCHASES_FREQUENCY))
table(is.na(CC$PURCHASES_INSTALLMENTS_FREQUENCY))
table(is.na(CC$CASH_ADVANCE_FREQUENCY))
table(is.na(CC$CASH_ADVANCE_FREQUENCY))
table(is.na(CC$CASH_ADVANCE_TRX))
table(is.na(CC$PURCHASES_TRX))
table(is.na(CC$CREDIT_LIMIT)) #Only 1 NA, should remove it
table(is.na(CC$PAYMENTS))
table(is.na(CC$MINIMUM_PAYMENTS)) # Make into 0s
table(is.na(CC$PRC_FULL_PAYMENT))
table(is.na(CC$TENURE))

#Data Wrangling and Cleaning

#Remove NA
which(is.na(CC$CREDIT_LIMIT))
subset here

#Recode Variables

CC_2 <- CC_1 %>%
  select(CUST_ID, BALANCE, BALANCE_FREQUENCY, PURCHASES, ONEOFF_PURCHASES, INSTALLMENTS_PURCHASES, CASH_ADVANCE, PURCHASES_FREQUENCY, PURCHASES_INSTALLMENTS_FREQUENCY, ONEOFF_PURCHASES_FREQUENCY,
         CASH_ADVANCE_FREQUENCY, CASH_ADVANCE_TRX, PURCHASES_TRX, CREDIT_LIMIT, PAYMENTS, PRC_FULL_PAYMENT, TENURE) %>%
  rename(id = CUST_ID, bal = BALANCE, bal_FREQ = BALANCE_FREQUENCY, install_PURCHASES = INSTALLMENTS_PURCHASES, cash_adv = CASH_ADVANCE, PURCHASES_FREQ = PURCHASES_FREQUENCY, PURCHASES_install_FREQ = PURCHASES_INSTALLMENTS_FREQUENCY,
         cash_adv_FREQ = CASH_ADVANCE_FREQUENCY, cash_adv_TRX = CASH_ADVANCE_TRX, credit_limit = CREDIT_LIMIT, pmts = PAYMENTS, prc_full_pmts = PRC_FULL_PAYMENT, tenure = TENURE, oneoff_PURCHASES = ONEOFF_PURCHASES, oneoff_PURCHASES_FREQ = ONEOFF_PURCHASES_FREQUENCY)


#Scale

CC_2_scaled = scale(CC_2[,2:17]) 
d = dist(x = CC_2_scaled,method = 'euclidean') 


#K-means Clustering
library(ggplot2)

set.seed(617)
km = kmeans(x = CC_2_scaled,centers = 4,iter.max=10000,nstart=25)
k_segments = km$cluster
table(km$cluster) #Cluster 1(405), Cluster 2(3370), Cluster 3(1192), Cluster 4(3982)





