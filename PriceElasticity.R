library(DBI)
library(rJava)
library(RJDBC)
library(RPostgreSQL)
library(dplyr)

drv <- dbDriver("PostgreSQL")

conn <- dbConnect(drv, host="XXXXXXXX.redshift.amazonaws.com", 
                  port="5439",
                  dbname="XXXXXXoddb01", 
                  user="XXXXXXnly", 
                  password="XXXXXX01")

Item_7995002 <- dbGetQuery(conn, "select * from analytics.daily_lot_level where lot = '7995022' and ordered_qty >=10")

head(Item_7995002)
summary(Item_7995002)
str(Item_7995002)

par(mfrow = c(1,2))

boxplot(Item_7995002$ordered_qty,horizontal = TRUE, xlab="qty")
hist(Item_7995002$ordered_qty,main="",xlab="qty",prob=T)
lines(density(Item_7995002$ordered_qty),lty="dashed",lwd=2.5,col="red")

qty_nprom = subset(Item_7995002,is_promotion==0)
qty_prom = subset(Item_7995002,is_promotion==1)

mean(qty_nprom$ordered_qty)
mean(qty_prom$ordered_qty)

par(mfrow = c(1,2))

# histogram to explore the data distribution shapes
hist(qty_nprom$ordered_qty,main="",xlab="sales with nature production theme ad",prob=T)
lines(density(qty_nprom$ordered_qty),lty="dashed",lwd=2.5,col="red")

hist(qty_prom$ordered_qty,main="",xlab="sales with family health caring theme ad",prob=T)
lines(density(qty_prom$ordered_qty),lty="dashed",lwd=2.5,col="red")

shapiro.test(qty_nprom$ordered_qty)
shapiro.test(qty_prom$ordered_qty)

t.test(qty_nprom$ordered_qty,qty_prom$ordered_qty)

library(dplyr)

corr <- select(Item_7995002, ordered_qty, unit_price, list_price)
pairs(corr,col="blue",pch=20)
pairs20x(corr)

#Level-Level Regression
ordered.reg<-lm(ordered_qty ~ unit_price, data = Item_7995002)
summary(ordered.reg)

install.packages("e1071")
library(e1071)

ordered.svm <- svm(ordered_qty ~ unit_price, Item_7995002)
summary(ordered.svm)
predictedY <- predict(ordered.svm, Item_7995002)
error <- Item_7995002$ordered_qty - predictedY
svrPredictionRMSE <- rmse(error)

install.packages("randomForest")
library(randomForest)

set.seed(1341)
ordered_rf <- randomForest(ordered_qty ~ unit_price + unit_cost, Item_7995002, n_tree = 200)
print(ordered_rf)

install.packages("gnm")
library(gnm)

gnm <- gnm(ordered_qty ~ unit_price, data = Item_7995002, family = gaussian)

library(proto)
library(nls2)

non_lm <- nls2(ordered_qty ~ unit_price, data = Item_7995002, algorithm = "plinear")
cor(ordered_qty,predict(non_lm))

#Level-Log Regression
ordered.llog<-lm(ordered_qty ~ log(unit_price), data = Item_7995002)
summary(ordered.llog)

#Log-Level Regression
ordered.levell<-lm(log(ordered_qty) ~ unit_price, data = Item_7995002)
summary(ordered.levell)

#Log-Log Regression
ordered.ll<-lm(log(ordered_qty) ~ log(unit_price), data = Item_7995002)
summary(ordered.ll)
#=====================================================

Item_7995002_V <- dbGetQuery(conn, "select date, AVG(unit_price) As unit_price, AVG(list_price) As list_price, SUM(ordered_qty) As Ordered_Qty from analytics.daily_lot_level where lot = '7995022'
group by date")

library(dplyr)

str(Item_7995002_V)

corr <- select(Item_7995002_V, ordered_qty, unit_price, list_price)
pairs(corr,col="blue",pch=20)
pairs20x(corr)

#Level-Level Regression
ordered.reg<-lm(ordered_qty ~ unit_price + list_price, data = Item_7995002_V)
summary(ordered.reg)

#Level-Log Regression
ordered.llog<-lm(ordered_qty ~ log(unit_price) + log(list_price), data = Item_7995002_V)
summary(ordered.llog)

#Log-Level Regression
ordered.levell<-lm(log(ordered_qty) ~ unit_price, data = Item_7995002_V)
summary(ordered.levell)

#Log-Log Regression
ordered.ll<-lm(log(ordered_qty) ~ log(unit_price) + log(list_price), data = Item_7995002_V)
summary(ordered.ll)
