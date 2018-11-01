#--------------------------------------- Linear Regression ----------------------------------------
# Data: Reatiler data
  # Features: catalog, agegroup, gender, homeownership, maritalstatus, location, income, expense
  # Goal: Predict the expense given the profile of customer.

#-------------------------- Libraries -----------------------------
  install.packages("effects")
  install.packages("carData")
  install.packages("corrplot")
  install.packages('DAAG')
  library(tidyverse)
  library(psych)
  library(effects)
  library(dplyr)
  library(corrplot)
  library(DAAG)

#-------------------------- Utility Functions -----------------------------
  outliers <- function(column) {
    lowerq <- as.vector(quantile(column)[2]) # returns 1st quartile
    upperq <- as.vector(quantile(column)[4]) # returns 1st quartile
    iqr <- upperq-lowerq  
    
    # Moderate outliers
    
    mod.outliers.upper <- (iqr * 1.5) + upperq
    mod.outliers.lower <- lowerq - (iqr * 1.5)
    mod.outliers <- which(column > mod.outliers.upper | 
                            column < mod.outliers.lower)
    print(paste("Moderate outlier:", mod.outliers))
    #Extreme outliers
    
    extreme.outliers.upper <- (iqr * 3) + upperq
    extreme.outliers.lower <- lowerq - (iqr * 3)
    extreme.outliers<-which(column > extreme.outliers.upper 
                            | column < extreme.outliers.lower)
    print(paste("Extreme outlier:", extreme.outliers))
    
    if (length(mod.outliers) > 0) {
      cnames <- c('Outlier Index', 'Outlier Type')
      outlier_info <- data.frame(mod.outliers, 'Moderate')
    
      if (length(extreme.outliers) > 0) {
        colnames(outlier_info) <- cnames
        ext <- data.frame(extreme.outliers, 'Extreme')
        colnames(ext) <- cnames
        
        outlier_info <- rbind(outlier_info, ext)
      }
      return(outlier_info)  
    } 
  }
  
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
  
#-------------------------- Data Cleaning -----------------------------
  glimpse(retailer)
  
  View(head(retailer))
  
  colSums(is.na(retailer))
  naInfo = findNAs(retailer)
  naInfo
  
  retailer$catalog <- as.factor(retailer$catalog)
  
#-------------------------- Univariate - Bivariate -----------------------------
  #--------------- Univariate ----------------
  str(retailer)
  
  psych::describe(retailer[, c('income', 'expense')])
  
  par(mfrow = c(1,2))
  hist(retailer$income, main = "Distribution of Income", xlab = "Income", col = "steelblue", las = 2, freq = F)
  rug(jitter(retailer$income), col = "red")
  lines(density(retailer$income), col = "green", lwd = 2)
  boxplot(retailer$income, main="Distribution of Income", col=c("orange"), xlab = "Income")
  
  par(mfrow = c(1,2))
  hist(retailer$expense, main = "Distribution of expense", xlab = "expense", col = "steelblue", las = 2, freq = F)
  rug(jitter(retailer$expense), col = "red")
  lines(density(retailer$expense), col = "green", lwd = 2)
  boxplot(retailer$expense, main="Distribution of expense", col=c("orange"), xlab = "expense")
  
  dev.off()
  
  # Income and expense are left skewed
  
  #--------------- Outliers ----------------
  boxplot(retailer$income)$out
  boxplot(retailer$expense)$out
  
  income_outliers <- outliers(retailer$income)
  expense_outliers <- outliers(retailer$expense)
  
  retailer <- retailer[-c(expense_outliers[, 1], income_outliers[, 1]), ]
  
  #--------------- Transformations ----------------
  par(mfrow=c(2,2))
  plot(density(retailer$expense), xlab = "Expense", main = "Without transformation", col = 'blue') 
  plot(density(log(retailer$expense)), xlab = "Expense", main = "Log transformation", col = 'blue')
  plot(density(sqrt(retailer$expense)), xlab = "Expense", main = "Sqrt transformation", col = 'blue')
  plot(density(retailer$expense^0.5), xlab = "Expense", main = "Power transformation", col = 'blue') # box cox transformation
  dev.off()
  
  par(mfrow=c(2,2))
  plot(density(retailer$income), xlab = "income", main = "Without transformation", col = 'blue') 
  plot(density(log(retailer$income)), xlab = "income", main = "Log transformation", col = 'blue')
  plot(density(sqrt(retailer$income)), xlab = "income", main = "Sqrt transformation", col = 'blue')
  plot(density(retailer$income^0.5), xlab = "income", main = "Power transformation", col = 'blue') # box cox transformation
  dev.off()
  
  #--------------- Bivariate ----------------
  plot(retailer$expense ~ retailer$income)
  abline(lm(retailer$expense ~ retailer$income), col = "limegreen", lwd = 2.5)
  
  corSummary <- cor(retailer$expense, retailer$income, use = "complete.obs", method = "pearson")
  corSummary
  
  corMatrix <- cor(retailer[, c("expense", "income")])
  corrplot::corrplot(corMatrix, method = "circle", addCoef.col = "black")
  
  cor.test(retailer$expense, retailer$income)
  
  chisq.test(retailer$catalog, retailer$agegroup) #***
  chisq.test(retailer$catalog, retailer$gender) # (NS) 
  chisq.test(retailer$catalog, retailer$homeownership) #***
  chisq.test(retailer$catalog, retailer$maritalstatus) #. 
  chisq.test(retailer$catalog, retailer$location) #.
  chisq.test(retailer$agegroup, retailer$gender) #***
  chisq.test(retailer$agegroup, retailer$homeownership) #***
  chisq.test(retailer$agegroup, retailer$maritalstatus) #***
  chisq.test(retailer$agegroup, retailer$location) #*
  chisq.test(retailer$agegroup, retailer$gender) #***
  
  #---------------- Bivariate DV ~ IVs ---------------
  summary(aov(expense ~ agegroup, data=retailer))
  summary(aov(expense ~ gender, data=retailer))
  summary(aov(expense ~ homeownership, data=retailer))
  summary(aov(expense ~ maritalstatus, data=retailer))
  summary(aov(expense ~ location, data=retailer))
  
#-------------------------- Train Test Split --------------------------
  
  set.seed(101)
  train_row_index <- sample(x = 1:nrow(retailer), size = 0.7 * nrow(retailer))
  # choose 70% indexes at random from retailer dataset indices
  train_data <- retailer[train_row_index, ]
  test_data <- retailer[-train_row_index, ] 
  
#-------------------------- Model -----------------------------  
  mod1 <- lm(expense~income+agegroup+gender+homeownership+maritalstatus+location, data=train_data)
  summary(mod1)
  
  mod_full <- lm(expense ~ ., data = train_data)
  summary(mod_full)
  
  #---------------- Stepwise regression for parameter selection ------------
  #-------- Choose model with minimum AIC
  step(mod_full, direction = 'both')
  
  model <- lm(expense ~ catalog + agegroup + location + income, data = train_data)
  summary(model)
  
  #------- Predictions ---------
  predictions <- predict(model, test_data)
  predictions_table <- data.frame(cbind(YTrue = test_data$expense, YPred = predictions))
  
  correlation_accuracy <- cor(predictions_table)
  correlation_accuracy[2] * 100
  
  mse <- mean((predictions_table$YTrue - predictions_table$YPred) ^ 2)
  mse  
  
  #--------- 10 Fold crossvalidation ----------
  cv_model <- cv.lm(data = train_data, expense ~ catalog + agegroup + location + income, m =10)
  summary(cv_model)
  
  
  
  
  
  
  
  