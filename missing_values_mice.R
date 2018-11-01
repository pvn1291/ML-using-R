library(lubridate)
library(dplyr)
library(tidyverse)
library(psych)
library(viridis)
library(zoo)
library(cluster) # for daisy 
library(factoextra) # plots 
library(gmodels)
library(NbClust)# determine number of clusters
library(pvclust) # probability measure for clusters
library(ggplot2)
library(VIM)
library(mice)
library(missForest)

#---------------------------- Fina NA Values ---------------------------------

findNAs <- function (df){
  tmp <- NULL
  for (i in colnames(df)){
    tmp[i] <- sum(is.na(df[, i]))
  }
  
  naInfo <- data.frame(colnames(df), tmp, round(((tmp / nrow(df)) * 100), 2))
  colnames(naInfo) <- c("Attribute", "NA Count", "NA Percentage")
  naInfo <- naInfo[order(naInfo$`NA Percentage`, decreasing = T),]
  
  nn <- data.frame("Total", sum(naInfo$`NA Count`), sum(naInfo$`NA Percentage`))
  colnames(nn) <- c("Attribute", "NA Count", "NA Percentage")
  
  naInfo <- rbind(naInfo, nn)
  rownames(naInfo) <- naInfo$Attribute
  
  rm(nn)
  rm(tmp)
  return(naInfo)
}        

#---------------------------- Load IRIS ---------------------------------

data("iris")
iris <- data.frame(iris, stringsAsFactors = TRUE)
iris.mis <- prodNA(iris, noNA = 0.1)

View(iris.mis)

#---------------------------- Check NA Values ---------------------------------

naInfo <- findNAs(iris.mis)
View(naInfo)

md.pattern(iris.mis)

missing_value_plot <- aggr(iris.mis, col = c('yellow', 'skyblue'), bars = T,
                          numbers = T, labels = names(iris.mis), 
                          ylabs = c('Missing Data', 'Heatmap'), prop = T)

#---------------------------- Subset Numeric ---------------------------------

iris.mis.num <- subset(iris.mis, select = -c(Species))

glimpse(iris.mis.num)

iris.num.imputed.pmm <- mice(data = iris.mis.num, m = 5, method = 'pmm', maxit = 5, seed = 101)
iris.num.imputed.midastouch <- mice(data = iris.mis.num, m = 5, method = 'midastouch', maxit = 5, seed = 101)

summary(iris.num.imputed.pmm)
iris.num.imputed.midastouch

#---------------------------- Check Imputed Data ---------------------------------
iris.num.imputed.pmm$imp$Sepal.Width

iris.imputed.1 <- complete(iris.num.imputed.pmm, action = 1)

iris.imputed.1

#------------------------- Fit models on imputed data -----------------------------

fit1 <- with(data = iris.imputed.1, expr = lm(Sepal.Length ~ Sepal.Width + Petal.Length))
summary(fit1)

fit5 <- with(data = iris.num.imputed.pmm, expr = lm(Sepal.Length ~ Sepal.Width + Petal.Length))
summary(fit5)

combined_fit <- pool(fit)
summary(combined_fit)

#---------------------------- Subset Factor Variable ------------------------------

levels(iris.mis$Species)

iris.mis$Species_Coded[iris.mis$Species == 'setosa'] <- 0
iris.mis$Species_Coded[iris.mis$Species == 'versicolor'] <- 1
iris.mis$Species_Coded[iris.mis$Species == 'virginica'] <- 2

iris.tmp <- subset(iris.mis, select = -c(Species_Coded))

iris.imputed.all <- mice(data = iris.tmp, m = 5, seed = 101, method = c('pmm', 'pmm', 'pmm', 'pmm', 'polr'), maxit = 5)

summary(iris.imputed.all)

iris.imputed.all.1 <- complete(iris.imputed.all, action = 1)


