#k means clustering
getwd()
setwd("C:/Users/aaroh/Documents/BA with R")
dungaree.df <- read.csv("dungaree.csv")
row.names(dungaree.df) <- dungaree.df[,1]
dungaree.df <- dungaree.df[,-1]
boxplot(dungaree.df, main="Number of Pairs of Four Different Types of Dungarees", xlab="Types of Dungarees", ylab="Number of Pairs of Jeans Sold") 
sum(is.na(dungaree.df$FASHION)) 
sum(is.na(dungaree.df$LEISURE))
sum(is.na(dungaree.df$STRETCH)) 
sum(is.na(dungaree.df$ORIGINAL))
library("VIM") 
aggr(dungaree.df) 
summary(dungaree.df)
boxplot(dungaree.df$FASHION)
boxplot(dungaree.df$LEISURE)
boxplot(dungaree.df$STRETCH)
boxplot(dungaree.df$ORIGINAL)
dungaree.df$FASHION[dungaree.df$FASHION > 112+1.5*IQR(dungaree.df$FASHION)] <- 112+1.5*IQR(dungaree.df$FASHION)
dungaree.df$LEISURE[dungaree.df$LEISURE > 2148+1.5*IQR(dungaree.df$LEISURE)] <- 2148+1.5*IQR(dungaree.df$LEISURE)
dungaree.df$STRETCH[dungaree.df$STRETCH > 568+1.5*IQR(dungaree.df$STRETCH)] <- 568+1.5*IQR(dungaree.df$STRETCH)
dungaree.df$ORIGINAL[dungaree.df$ORIGINAL > 2053+1.5*IQR(dungaree.df$ORIGINAL)] <- 2053+1.5*IQR(dungaree.df$ORIGINAL)
dungaree.df$FASHION[dungaree.df$FASHION < 70-1.5*IQR(dungaree.df$FASHION)] <- 70-1.5*IQR(dungaree.df$FASHION)
dungaree.df$LEISURE[dungaree.df$LEISURE < 1695-1.5*IQR(dungaree.df$LEISURE)] <- 1695-1.5*IQR(dungaree.df$LEISURE)
dungaree.df$STRETCH[dungaree.df$STRETCH < 312-1.5*IQR(dungaree.df$STRETCH)] <- 312-1.5*IQR(dungaree.df$STRETCH)
dungaree.df$ORIGINAL[dungaree.df$ORIGINAL < 1658-1.5*IQR(dungaree.df$ORIGINAL)] <- 1658-1.5*IQR(dungaree.df$ORIGINAL)
boxplot(dungaree.df$FASHION)
boxplot(dungaree.df$LEISURE)
boxplot(dungaree.df$STRETCH)
boxplot(dungaree.df$ORIGINAL)
nrow(dungaree.df)
dungaree.df.norm <- sapply(dungaree.df, scale) 
boxplot(dungaree.df.norm,main="Scaled: four different types of dungarees sold at stores", xlab="Jeans Type", ylab="Number of pairs of jeans sold") 
install.packages("NbClust")
library("NbClust")
set.seed(42)
devAskNewPage(ask=TRUE) 
nc <- NbClust(dungaree.df.norm, min.nc=2, max.nc=10, method="kmeans") 
table(nc$Best.n[1,]) 
barplot(table(nc$Best.n[1,]), xlab="Number of Clusters", ylab="Number of criteria", main="Number of clusters chosen by criteria") 
fit.km <- kmeans(dungaree.df.norm, 6, nstart=25) 
fit.km$size 
fit.km$centers 
wssplot <- function(data, nc=10, seed=42){ 
  wss <- (nrow(data)-1)*sum(apply(data,2,var)) 
  for (i in 2:nc){ 
    set.seed(42) 
    wss[i] <- sum(kmeans(data, centers=i)$withinss)} 
  plot(1:nc, wss, type="b", xlab="Number of Clusters", 
       ylab="Within groups sum of squares")} 
wssplot(dungaree.df.norm,nc=10,seed=42) 
set.seed(42)
devAskNewPage(ask=TRUE)
nc <- NbClust(dungaree.df.norm, min.nc=2, max.nc=6, method="kmeans") 
table(nc$Best.n[1,]) 
barplot(table(nc$Best.n[1,]), xlab="Number of Clusters", ylab="Number of criteria", main="Number of clusters chosen by criteria") 
fit.km <- kmeans(dungaree.df.norm, 3 , nstart=25) 
fit.km$size
fit.km$centers
wssplot(dungaree.df.norm,nc=6,seed=42) 






