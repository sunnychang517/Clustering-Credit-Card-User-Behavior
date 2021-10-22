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
table(is.na(CC$CREDIT_LIMIT))

#Remove NA
library(dplyr)
which(is.na(CC$CREDIT_LIMIT))
CC <- CC[-c(5204), ]

#Subset 
CC_1 <- subset(CC,select = -c(16))

#Recode Variables
library(tidyverse)
library(dplyr)

CC_2 <- CC_1 %>%
  select(CUST_ID, BALANCE, BALANCE_FREQUENCY, PURCHASES, ONEOFF_PURCHASES, INSTALLMENTS_PURCHASES, CASH_ADVANCE, PURCHASES_FREQUENCY, PURCHASES_INSTALLMENTS_FREQUENCY, ONEOFF_PURCHASES_FREQUENCY,
         CASH_ADVANCE_FREQUENCY, CASH_ADVANCE_TRX, PURCHASES_TRX, CREDIT_LIMIT, PAYMENTS, PRC_FULL_PAYMENT, TENURE) %>%
  rename(id = CUST_ID, bal = BALANCE, bal_FREQ = BALANCE_FREQUENCY, install_PURCHASES = INSTALLMENTS_PURCHASES, cash_adv = CASH_ADVANCE, PURCHASES_FREQ = PURCHASES_FREQUENCY, PURCHASES_install_FREQ = PURCHASES_INSTALLMENTS_FREQUENCY,
         cash_adv_FREQ = CASH_ADVANCE_FREQUENCY, cash_adv_TRX = CASH_ADVANCE_TRX, credit_limit = CREDIT_LIMIT, pmts = PAYMENTS, prc_full_pmts = PRC_FULL_PAYMENT, tenure = TENURE, oneoff_PURCHASES = ONEOFF_PURCHASES, oneoff_PURCHASES_FREQ = ONEOFF_PURCHASES_FREQUENCY)


#Scale

CC_2_scaled = scale(CC_2[,2:17]) #Our data cluster
d = dist(x = CC_2_scaled,method = 'euclidean') 
clusters = hclust(d = d,method='ward.D2')
plot(clusters)
cor(cophenetic(clusters),d) #[1] 0.3531409, this is our goodness of fit

#K-means Clustering
library(ggplot2)

set.seed(617)
km = kmeans(x = CC_2_scaled,centers = 4,iter.max=10000,nstart=25)

table(km$cluster) #Cluster 1(405), Cluster 2(3370), Cluster 3(1192), Cluster 4(3982)

#Total within sum of squares Plot supports 3 to 4 clusters

within_ss = sapply(1:5,FUN = function(x){
  set.seed(617)
  kmeans(x = CC_2_scaled,centers = x,iter.max = 1000,nstart = 25)$tot.withinss})

ggplot(data=data.frame(cluster = 1:5,within_ss),aes(x=cluster,y=within_ss))+
  geom_line(col='steelblue',size=1.2)+
  geom_point()+
  scale_x_continuous(breaks=seq(1,10,1))

#Ratio Plot supports 4 clusters

ratio_ss = sapply(1:5,FUN = function(x) {
  set.seed(617)
  km = kmeans(x = CC_2_scaled,centers = x,iter.max = 1000,nstart = 25)
  km$betweenss/km$totss} )

ggplot(data=data.frame(cluster = 1:5,ratio_ss),aes(x=cluster,y=ratio_ss))+
  geom_line(col='steelblue',size=1.2)+
  geom_point()+
  scale_x_continuous(breaks=seq(1,10,1))

#Silhouette Plot supports 2 to 4

library(cluster)
silhoette_width = sapply(2:10,
                         FUN = function(x) pam(x = CC_2_scaled,k = x)$silinfo$avg.width)
ggplot(data=data.frame(cluster = 2:10,silhoette_width),aes(x=cluster,y=silhoette_width))+
  geom_line(col='steelblue',size=1.2)+
  geom_point()+
  scale_x_continuous(breaks=seq(2,10,1))

#check cluster sizes to find out niche segments
#3 clusters
#set.seed(617)
#km = kmeans(x = CC_2_scaled,centers = 3,iter.max=10000,nstart=25)
#k_segments = km$cluster
#table(k_segments)

#4 clusters
set.seed(617)
km = kmeans(x = CC_2_scaled,centers = 4,iter.max=10000,nstart=25)
k_segments = km$cluster
table(k_segments) #Cluster 1(405), Cluster 2(3370), Cluster 3(1192), Cluster 4(3982)

#4 clusters has a niche market

##m_segment
#M clustering
library(mclust)
set.seed(617)
clusters_mclust = Mclust(CC_2_scaled)
summary(clusters_mclust) ##Cluster 1(2093), Cluster 2(3800), Cluster 3(1980), Cluster 4(1076)

mclust_bic = sapply(1:10,FUN = function(x) -Mclust(CC_2_scaled,G=x)$bic)
mclust_bic 

ggplot(data=data.frame(cluster = 1:10,bic = mclust_bic),aes(x=cluster,y=bic))+
  geom_line(col='steelblue',size=1.2)+
  geom_point()+
  scale_x_continuous(breaks=seq(1,10,1)) 

m_clusters = Mclust(data = CC_2_scaled,G = 4) #4 clusters
m_segments = m_clusters$classification
table(m_segments) #Cluster 1(1445), Cluster 2(1007), Cluster 3(2951), Cluster 4(3546)

##h_segment
set.seed(617)
rect.hclust(tree=clusters,k = 4,border = 'tomato')

cluster_4 = cutree(tree = clusters,k=4) ## Cluster 1(4415), Cluster 2(1276), Cluster 3(3235), Cluster 4(23)
table(cluster_4)

#Describe each clusters

CC_2comb = cbind(CC_2,k_segments,cluster_4,m_segments)
CC_2comb = as.data.frame(CC_2comb)

library(dplyr)
CC_2comb %>%
  group_by(k_segments)%>%
  summarize_all(function(x) round(mean(x,na.rm=T),2))%>%
  data.frame()

library(dplyr); library(ggplot2); library(tidyr)
CC_2comb %>%
  group_by(k_segments)%>%
  summarize_all(function(x) round(mean(x,na.rm=T),2))%>%
  gather(key = var,value = value, bal:tenure)%>%
  ggplot(aes(x=var,y=value,fill=factor(k_segments)))+
  geom_col(position='dodge')+
  coord_flip()

#Proportion Tables

round(prop.table(table(CC_2comb$k_segments,CC_2comb$credit_limit),1),2)*100
round(prop.table(table(CC_2comb$k_segments,CC_2comb$bal_FREQ),1),2)*100
round(prop.table(table(CC_2comb$k_segments,CC_2comb$bal),1),2)*100

round(prop.table(table(CC_2comb$k_segments,CC_2comb$cash_adv_FREQ),1),2)*100
round(prop.table(table(CC_2comb$k_segments,CC_2comb$oneoff_PURCHASES_FREQ),1),2)*100
round(prop.table(table(CC_2comb$k_segments,CC_2comb$PURCHASES_install_FREQ),1),2)*100
round(prop.table(table(CC_2comb$k_segments,CC_2comb$prc_full_pmts),1),2)*100
round(prop.table(table(CC_2comb$k_segments,CC_2comb$tenure),1),2)*100

CC_2comb %>%
  group_by(cluster_4)%>%
  summarize_all(function(x) round(mean(x,na.rm=T),2))%>%
  gather(key = var,value = value, bal:tenure)%>%
  ggplot(aes(x=var,y=value,fill=factor(cluster_4)))+
  geom_col(position='dodge')+
  coord_flip()

CC_2comb %>%
  group_by(m_segments)%>%
  summarize_all(function(x) round(mean(x,na.rm=T),2))%>%
  gather(key = var,value = value, bal:tenure)%>%
  ggplot(aes(x=var,y=value,fill=factor(m_segments)))+
  geom_col(position='dodge')+
  coord_flip()



