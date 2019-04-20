library(ggplot2)
library(magrittr)
library(Metrics)
library(plotly)
library(corrplot)
library(PerformanceAnalytics)
library(Hmisc)
library(knitr)
library(mctest)
library(caret)
library(ROCR)
library(scales)
library(dummies)
library(neuralnet)
library(class)
library(dplyr)
library(e1071)
library(tree)
library(ISLR)
library(data.table)

df <- fread("Credit.csv")

df$AMOUNT_REQUESTED <- as.numeric(gsub(",","",df$AMOUNT_REQUESTED))
df$CREDIT_EXTENDED <- NULL
df$`OBS#` <- NULL

#1
#Creating Dummy Varaibles
df$CHK0 <- factor(ifelse(df$CHK_ACCT == 0, 1, 0))
df$CHK1 <- factor(ifelse(df$CHK_ACCT == 1, 1, 0))
df$CHK2 <- factor(ifelse(df$CHK_ACCT == 2, 1, 0))
df$CHK3 <- factor(ifelse(df$CHK_ACCT == 3, 1, 0))

df$SAV0 <- factor(ifelse(df$SAV_ACCT == 0, 1, 0))
df$SAV1 <- factor(ifelse(df$SAV_ACCT == 1, 1, 0))
df$SAV2 <- factor(ifelse(df$SAV_ACCT == 2, 1, 0))
df$SAV3 <- factor(ifelse(df$SAV_ACCT == 3, 1, 0))
df$SAV4 <- factor(ifelse(df$SAV_ACCT == 4, 1, 0))

df$HISTORY0 <- factor(ifelse(df$HISTORY == 0, 1, 0))
df$HISTORY1 <- factor(ifelse(df$HISTORY == 1, 1, 0))
df$HISTORY2 <- factor(ifelse(df$HISTORY == 2, 1, 0))
df$HISTORY3 <- factor(ifelse(df$HISTORY == 3, 1, 0))
df$HISTORY4 <- factor(ifelse(df$HISTORY == 4, 1, 0))

df$JOB0 <- factor(ifelse(df$JOB == 0, 1, 0))
df$JOB1 <- factor(ifelse(df$JOB == 1, 1, 0))
df$JOB2 <- factor(ifelse(df$JOB == 2, 1, 0))
df$JOB3 <- factor(ifelse(df$JOB == 3, 1, 0))

df$TYPE0 <- factor(ifelse(df$TYPE == 0, 1, 0))
df$TYPE1 <- factor(ifelse(df$TYPE == 1, 1, 0))
df$TYPE2 <- factor(ifelse(df$TYPE == 2, 1, 0))
df$TYPE3 <- factor(ifelse(df$TYPE == 3, 1, 0))
df$TYPE4 <- factor(ifelse(df$TYPE == 4, 1, 0))
df$TYPE5 <- factor(ifelse(df$TYPE == 5, 1, 0))
df$TYPE6 <- factor(ifelse(df$TYPE == 6, 1, 0))

##
#2
set.seed(12345)
df$PROFITABLE <- ifelse(df$NPV > 0, 1, 0)
df$PROFITABLE <- factor(df$PROFITABLE)
df1 <- df[, -21]
df1 <- df1[, -46]
df1 <- scale(df1)

kmeansCluster = kmeans(df1, 5, nstart=20)
kmeansCluster
dist(kmeansCluster$centers)


##
#4
newdf <- data.frame(df$NPV, kmeansCluster$cluster)



##
#6
kmeansCluster_4 = kmeans(df1, 4, nstart=20)
kmeansCluster_4
dist(kmeansCluster_4$centers)

kmeansCluster_6 = kmeans(df1, 9, nstart=20)
kmeansCluster_6
dist(kmeansCluster_6$centers)


