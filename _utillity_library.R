#----------------------- Descriptive Stats for Numeric Variable ------------------
# Statistics
mean(NUM_VAR_NAME)
median(NUM_VAR_NAME)
mean() - median(NUM_VAR_NAME)
sd(NUM_VAR_NAME)
quantile(NUM_VAR_NAME)
psych::describe(NUM_VAR_NAME)

# Plots
par(mfrow = c(1,2))
hist(NUM_VAR_NAME, main = "Distribution of VARNAME", xlab = "VARNAME", col = "steelblue", las = 2, freq = F)
rug(jitter(NUM_VAR_NAME), col = "red")
lines(density(NUM_VAR_NAME), col = "green", lwd = 2)
boxplot(NUM_VAR_NAME, main="Distribution of VARNAME", col=c("orange"), xlab = "VARNAME")

#----------------------- Transformation of numeric Variable ------------------
par(mfrow=c(2,2))
plot(density(NUM_VAR_NAME), xlab = "VARNAME", main = "Without transformation", col = 'blue') 
plot(density(log(NUM_VAR_NAME)), xlab = "VARNAME", main = "Log transformation", col = 'blue')
plot(density(sqrt(NUM_VAR_NAME)), xlab = "VARNAME", main = "Sqrt transformation", col = 'blue')
plot(density(NUM_VAR_NAME ^ 0.5), xlab = "VARNAME", main = "Power transformation", col = 'blue')

#----------------------- Distribution of Factor Variable ------------------
summary()
par(mfrow = c(1,1))
prpTbl <- prop.table(table())
barplot(pcatTbl * 100, main = "VARNAME Categories", xlab = "VARNAME Category (Levels)", 
        col = c("darkgrey", "darkgoldenrod1"), las = 1)

#----------------------- Relation between Numeric & Numeric -----------------

# Write H0 (Null) and H1 (Alternative) hypothesis
# H0 (Null) = NUM_VAR_NAME1 and NUM_VAR_NAME2 are independent of each other
# H1 (Alternative) = NUM_VAR_NAME1 and NUM_VAR_NAME2 are dependent on each other
# Accept H0 if pValue > 5%
# Reject H0 if pValue < 5%

plot(NUM_VAR_NAME1 ~ NUM_VAR_NAME2)
abline(lm(NUM_VAR_NAME1 ~ NUM_VAR_NAME2), col = "limegreen", lwd = 2.5)

corSummary <- cor(NUM_VAR_NAME1, NUM_VAR_NAME2, use = "complete.obs", method = "pearson")
corSummary

corMatrix <- cor(DATAFRAMENAME[, c("NUM_VAR_NAME1", "NUM_VAR_NAME2")])
corrplot::corrplot(corMatrix, method = "circle", addCoef.col = "black")

cor.test(NUM_VAR_NAME1, NUM_VAR_NAME2)
# Accept H0 if pValue > 5%
# Reject H0 if pValue < 5%

# ---------------------- Relationship between Numeric & Numeric by factor ----------------
# span is the lowess smoothing parameter. try a different value to see how it works. 
scatterplot(Y ~ X | league, data=baseball, lwd=2, span=0.8, 
            main="Relationship between Payroll and PCT by League", 
            xlab="Payroll in $million", 
            ylab="% wins",
            legend.plot=TRUE,
            id.method="identify", 
            labels=baseball$team,
            boxplots="xy")

#Conclusion:

#----------------------- Relation between Factor & Factor -----------------

# Write H0 (Null) and H1 (Alternative) hypothesis
# H0 (Null) = FACTOR_VAR_NAME1 and FACTOR_VAR_NAME2 are independent of each other
# H1 (Alternative) = FACTOR_VAR_NAME1 and FACTOR_VAR_NAME2 are dependent of each other
# Accept H0 if pValue > 5%
# Reject H0 if pValue < 5%

table <- table(table(FACTORVAR1, FACTORVAR2))
propTable <- prop.table(table)
barplot(propTable, main = "VARNAME Categories", xlab = "VARNAME Category (Levels)", 
        col = c("darkgrey", "darkgoldenrod1"), las = 1)
legend("bottomright", legend = c("LIST OF LEVELS"), fill = c("COLORS"))
chisq.test(table)

# Conclusion: 

# OR

xTable <- xtabs(~ FACTORVAR1 + FACTORVAR2)
propXTable <- prop.table(xTable)
barplot(propXTable, main = "VARNAME Categories", xlab = "VARNAME Category (Levels)", 
        col = c("darkgrey", "darkgoldenrod1"), las = 1)
legend("bottomright", legend = c("LIST OF LEVELS"), fill = c("COLORS"))
chisq.test(table)

# Conclusion: 

#----------------------- Relation between Numeric & Factor -----------------

# Write H0 (Null) and H1 (Alternative) hypothesis
# boxplot(y ~ x)
boxplot(NUMVAR ~ FACTORVAR, data = DATAFRAMENAME, main="Distribution of NUM_VAR_NAME for FACTOR_VAR_NAME", 
        xlab = "FACTOR_VAR_NAME Categories (levels)", ylab = "MPG", col=c("orange", "lightblue4"))
plotmeans(NUMVAR ~ FACTORVAR, lwd=3, col="red", p=0.99)

testAOV <- aov(NUMVAR ~ FACTORVAR, data = DATAFRAMENAME)
summary(testAOV) 
# H0 Null rejected (pValue < 5%). That mean is different for all the categories.
# F value -> Variation among sample means

# We use Tukey pairwise comparisons
testAOV.tk <-TukeyHSD(testAOV)
testAOV.tk
round(testAOV.tk$spent,2)

# F value is ----, and p-value is -----. 
# The variation of NUMERIC_VAR_NAME means among different FACTOR_VAR_NAME is much larger.
# p-value is ----- than 0.05. 
# Hence we can conclude that for our confidence interval we accept the alternative hypothesis H1. 
# that there is a significant relationship between FACTOR_VAR_NAME and NUMERIC_VAR_NAME.

#----------------------- Linear Regression -----------------

# Step 1: Analyze vaiables: 
  # 1. Factors should have considerable observations
    # If not, either 1. Club multiple categories or 2. Drop categories
      DATAFRAME_NAME$NEW_COL_NAME["CONDITION"] <- "VALUE_TO_BE_ASSIGNED" # Vehicle categories based on CC
  
  # 2. Numeric should be normally distributed
    # If not either remove outliers or transform variable using log() and sqrt()
    plot(density(log(retailer$expense)))
    plot(density(sqrt(retailer$expense)))
    plot(density(retailer$expense^0.2)) # box cox transformation
    summary(powerTransform(retailer$catalog))

# Step 2: Create Model -> lm (DV ~ IV1 + IV2 + ... + IVn)
  options(scipen = 999)
  linearModel <- lm (NUM_VAR_NAME ~ OTHER_VARIABLES, data = DATAFRAME_NAME)
  summary(linearModel)

# Check for pValues of each independent variable to consider their significance on model
# Check R Square Value for model accuracy
# (Keeping other variables constant)Check Estimate column for impact / influence of independent variable on depedent variable
  # if indepedent variable is numeric = Estimate column value represents contribution to dependent variable
  # if indepedent variable is factor,
    # first level is considered as reference level for comparison of contributions by other levels
    # To change the reference category, 
      DATAFRAME_NAME$COL_NAME <- relevel(DATAFRAME_NAME$COL_NAME, ref = NUM)

# Step 3: Diagnose Regression Model
  # 1: Plot the model
      dev.off()
      par(mfrow = c(2,2)) # 4 plots on canvas
      plot(linearModel)
      # Read the plots for outliers and leverage of outliers
  
  # 2: Find outliers          
    # Method 1: hatvalues
      plot(hatvalues(linearModel))
      outliers <- identify(hatvalues(linearModel), col = "red")
      NEW_DATA_FRAME <- DATA_FRAME[- outliers,] # removing entire rows containing outlier values
      
    # Method 2: cook's distance
      round(sort(cooks.distance(linearModel)), 2)
      # Remove outliers
      outliers <- c("comma seperated list of outliers at the end")
      NEW_DATA_FRAME <- DATA_FRAME[- outliers,] # removing entire rows containing outlier values
  
  # 3: Create new model  
      linearModel.new <- lm (NUM_VAR_NAME ~ OTHER_VARIABLES, data = DATAFRAME_NAME)
      summary(linearModel.new)
  
  # 4: Compare old and new models
      #----------------- Summary of old model ----------------
      summary(linearModel)
      #----------------- Summary of new model ----------------
      summary(linearModel.new)
      
      #----------------- Improvement in accuracy ----------------
      summary(linearModel.new)$adj.r.squared - summary(linearModel)$adj.r.squared
      
#----------------------- Hypothesis Testing -----------------
  # For comparing mean of sample vs population or means of two variables
      
  # H0 = Sample Mean and True mean are same
  # H1 = Sample Mean and True mean are diffrent and not equals to 0 (can be > or <)     
      
  # T Tests
      # 1: One sample test on Numeric variables only
        test <- t.test(DATAFRAME$COLNAME, alternative = "two.sided", mu = TRUEMEAN, conf.level = 0.95)
        test
        # if pValue < 0.05 reject H0 else accept H0 (CI = 95%)
        # if pValue < 0.01 reject H0 else accept H0 (CI = 99%)
      
      # 2: For two variables  
        test <- t.test(DATAFRAME$COLNAME["Condition"], DATAFRAME$COLNAME["Condition"])
        test
        # Example: price_null <- t.test(cars$price[cars$agecat=="newies"], alternative="greater", mu=11196.07, conf.level=0.95)
        # plot histograms to prove hypothesis
          hist(cars$price[cars$agecat=="oldies"], main="Testing price differences")
          hist(cars$price[cars$agecat=="newies"], col="steelblue", add=TRUE) # add = TRUE, overlap graphs
          legend("topleft", c("oldies", "newies"), col=c("white", "steelblue"), pch=c(19,19), cex=0.8)
        
  # Calculate confidence interval at 95%
      sd_error <- sd(sample_var_name) / sqrt(sample_size) # sample_size is # of observations in sample
      lower_bound <- mean(sample_var_name) - 1.96 * sd_error
      upper_bound <- mean(sample_var_name) + 1.96 * sd_error
      c(lower_bound, upper_bound)
      
  # Calculate confidence interval at 99%
      sd_error <- sd(sample_var_name) / sqrt(sample_size) # sample_size is # of observations in sample
      lower_bound <- mean(sample_var_name) - 2.578 * sd_error
      upper_bound <- mean(sample_var_name) + 2.578 * sd_error
      c(lower_bound, upper_bound)      
      
      
      
#----------------------- Utilities -----------------------
# ----------- Libraries ------------
      library(readr)
      library(tibble)
      library(dplyr)
      library(zoo)
      library(tidyverse)
      library(ggplot2)
      library(gridExtra)
      
# ----------- Recode and filter ----------------
      # recode factor###########
      Salaries$discipline1 <-  recode_factor(Salaries$discipline, "A"="Theoetical", "B"="Applied")
      # if else #####
      Salaries$workExp<-ifelse(Salaries$yrs.service <=5, "low", 
                               ifelse(Salaries$yrs.service > 6 & Salaries$yrs.service <= 10, "mid", "high"))
      # group by function ## tidyverse #
      Salaries %>% group_by(sex) %>% summarise(Average = mean(salary), Maximum = max(salary), Minimum = min(salary))
      
      # tidyverse filter 
      salMoreThanMean <- forbes %>% filter(Salary > mean(Salary))
      mean(salMoreThanMean$FiveYrReturn) #13.53041
      
      
      ifelse(Salaries$yrs.service > 6 & Salaries$yrs.service <= 10, "mid", "high")
# ------------- Rowwise Iteration ---------------
      # iterate over row
      
      for (row in 1:nrow(citiesWithPopulationOver2M)) {
          citiesWithPopulationOver2M$percentageOfPoliceStaff[row] <- citiesWithPopulationOver2M$Total_officers[row] / citiesWithPopulationOver2M$Total_law_enforcement_employees[row]
      }
# ----------- Date Wrangling --------------
      date1 <- as.Date(issue, format="%m/%d/%Y") 
      sometimes <- c("1/1/2017 7:00:01", "1/2/2017 7:00:20", "1/3/2017 7:02:15")
      
      library(lubridate)
      mdy(issue) %>% print() %>% class() 
      times <- mdy_hms(sometimes, tz="Africa/Abidjan") %>% print()  # time zone
      
      as.POSIXct(sometimes, format="%m/%d/%Y %H:%M:%S")
      year(times)
      yday(times)
      minute(times)
      second(times)
      
      
      # Time zone 
      # Default is local machine. 
        Sys.timezone()
      # To input a specific time zone: 
        as.POSIXct(sometimes, format="%m/%d/%Y %H:%M:%S", tz="Africa/Abidjan")
      
      
      # Extracting time components (year, month, etc.)
          format(times, "%Y")
          format(times, "%y")
          format(times, "%B")
          weekdays(times)
          months(times)
          quarters(times)
          format(times, "%S")
      
# ----------- Combine Multiple Columns ------------
          paste(colName1, colName2, ....., colNamen, sep = "sep_char")
          
# ---------- Load Packages ---------
      library(car)
      library(dplyr)
      library(gmodels)
      library(gplots)
      library(psych)
      library(corrplot)
      library(readr)
      library(psych)
      
# ----------- Order datafram by column --------
      
      df[order(df$colName1, df$ColName2),]
      empCSV %>% arrange(desc(colName1), colName2) -> newDf

# ---------- Remove Outliers -----------------
      outliers_m <- function(column) 
      {
        lowerq <- as.vector(quantile(column)[2]) # returns 1st quartile
        upperq <- as.vector(quantile(column)[4]) # returns 1st quartile
        iqr <- upperq-lowerq  
        
        # Moderate outliers
        
        mod.outliers.upper <- (iqr * 1.5) + upperq
        mod.outliers.lower <- lowerq - (iqr * 1.5)
        mod.outliers <- which(column > mod.outliers.upper | 
                                column < mod.outliers.lower)
        return(mod.outliers)
      }      

# ------------- Find Probablity ----------
      #a: 
      probAt54 <- pnorm(54, mean = 52, sd = 10.5)
      probAt60 <- pnorm(60, mean = 52, sd = 10.5)
      probBet54to60 <- probAt60 - probAt54
      probBet54to60
      #b:
      probAtLeast45 <- 1 - pnorm(45, mean = 52, sd = 10.5)
      probAtLeast45
      #c:
      probLessThan35 <- pnorm(35, mean = 52, sd = 10.5)
      probLessThan35

# ------------- Sampling of data -------------
      n <- REQD_NO_OF_SAMPLES
      sampleVar <- sample(DATAFRAME$COL_NAME, n)
      mean(DATAFRAME$COL_NAME) - mean(sampleVar)
      median(DATAFRAME$COL_NAME) - median(sampleVar)
      sd(DATAFRAME$COL_NAME) - sd(sampleVar)
      
      # For loop:
      # Initialize a vector for each parameter 
      n <- 30 
      sample_means <- rep(NA, 100)
      sample_medians <- rep(NA, 100)
      sample_sds <- rep(NA, 100)
      
      # Run the for loop
      for (i in 1:100) {
        s<-sample(DATAFRAME$COL_NAME, n)
        sample_means[i]<-mean(s)
        sample_medians[i]<-median(s)
        sample_sds[i]<-sd(s)
      }
      
      mean(DATAFRAME$COL_NAME)-mean(sample_means) 
      median(DATAFRAME$COL_NAME)-mean(sample_medians) 
      sd(DATAFRAME$COL_NAME)-mean(sample_sds) 
      
      par(mfrow=c(1,3))
      hist(sample_means); hist(sample_medians); hist(sample_sds)
      # If Not a normal distribution. This is because we have extreme values. 
      # Increasing n will take care of this
      
# ----------- Remove NAs from data --------     
      na.omit(DATAFRAME_NAME) # from entire data frame
      na.omit(DATAFRAME_NAME, cols = c('COLNAME1', 'COLNAME2')) # from specific columns in dataframe
      
# ----------- Read.CSV -------------
      NEW_DATA_FRAME <- read.csv(file.choose(), header = T, stringsAsFactors = F)
      
# ----------- Load RData -----------
      setwd('Paths')
      load('RData_Filename')
      
# ------------- Regular Expressions ---------------
      grep("$", cost) # row output # row no where $ is found in row values
      grep("$", cost, value=T) # value output 
      grepl("$", cost) # logical output  # grepl = grep logical
      grep("$50", cost)
      grep("\\$50", cost) # escape special symbols, such as $,.() etc. 
      grep("\\$50|\\$20", cost) # the | stands for "or" 
      grep("\\b500\\b", cost, value=T) # \\b indicates boundaries
      
      # Replace -----
          digitpattern <- "\\$|\\.00"
          gsub(digitpattern, "", cost) # remember to reassign the processed output and convert to 
          # the correct data type if needed 
          
          nums <- "\\d{1,}" # Extract only digits that follow one another 
          regmatches(cost, regexpr(nums, cost))
          
# ------------- Correlation Matrix ---------------
    pairs(~price+mileage+year, data=cars, 
                main="Correlation Plot Matrix", 
                pch=20, 
                lower.panel=NULL) # shows only the upper panel 
          
    scatterplotMatrix(~price+mileage+year, data=cars, 
                            spread=F, smoother.args=list(lty=1, lwd=2.6), 
                            main="Better Correlation Plot")			
          
# ---------------- ggplot commands ------------------
    # Histograms for Index Variables    
        qplot(EEI_SCORE, data=finalFevsData, geom="histogram", col=I("steelblue"), fill=I("blue"), alpha=I(.4)) +
            geom_vline(xintercept = median(finalFevsData$EEI_SCORE), color="red", lwd=2) 
        
        qplot(EPI_SCORE, data=finalFevsData, geom="density")
    
    # Boxplot
        finalFevsData %>% ggplot(aes(x=EPI_SCORE_SCALED, y=EEI_SCORE, color = EPI_SCORE_SCALED)) + geom_boxplot()
    
    # Barplot
        df %>% count(unlist(df[, colName])) %>% mutate(perc = n / nrow(df)) -> prpTable
        
        ggplot(prpTable, aes(x = levels(unlist(prpTable[, 1])), y = perc, 
                             fill = levels(unlist(prpTable[, 1])))) +
            geom_bar(stat = "identity") +
            labs(x = paste("Response levels to ", colName),
                 y = "Percent of Responses", title = "Proportion of Responses",
                 fill = levels(unlist(prpTable[, 1])))
        
    # Bivariate plots,
        # Factor Numeric
        qplot(league,pct, data=baseball, ylab="%wins", xlab="League", main="PCT by League", geom="boxplot") 
        
        # Numeric Numeric
        qplot(y=pct,x=pay, data=baseball, main="PCT and Pay")
        
        # facets is a group variale.
        # two numeric and a factor
        qplot(x=pay,y=pct, data=baseball, ylab="%wins", xlab="Pay", 
              main="Relationship between PCT and Pay, by League", 
              facets=~division)
    # Arranging ggplots on canvas 
        par(mfrow=c(1,2))
        ggplot(data=baseball, aes(x=pay_mil)) + geom_histogram() 
        ggplot(baseball, aes(x=pay_mil, y=pct, color=league, shape=division)) +
            geom_point() + geom_jitter() # nope 
        
        p1 <- ggplot(data=baseball, aes(x=pay)) + geom_histogram() 
        
        p2 <- ggplot(baseball, aes(x=pay_mil, y=pct, color=league, shape=division)) +
            geom_point() + geom_jitter() 
        
        grid.arrange(p1, p2, ncol=2)
        
# ------------------ COmpare Linear Models -------------------------
        # We can only compare models with the same DV and same data
        anova(mod2,mod7, test="Chisq") # Yes, the two are significantly different (last one is much better)
        # But really, we want to know to find the extent to which our models have improved, after "cleaning"
        summary(mod7.1)$adj.r.squared-summary(mod2)$adj.r.squared # only slight improvement 

# ------------------ Find NA Values -------------------------
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
      
      