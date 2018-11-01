# CPD Data Analysis

# load libraries
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

# Functions
outliers_m <- function(column) 
{
  lowerq <- as.vector(quantile(column)[2]) # returns 1st quartile
  upperq <- as.vector(quantile(column)[4]) # returns 1st quartile
  iqr <- upperq-lowerq  
  
  # Moderate outliers
  
  mod.outliers.upper <- (iqr * 3) + upperq
  mod.outliers.lower <- lowerq - (iqr * 3)
  mod.outliers <- which(column > mod.outliers.upper | 
                          column < mod.outliers.lower)
  return(mod.outliers)
}    

# Set File Path
# filepath <- "~/Documents/Spring 2018/IDS 462/final project data/hr"
# setwd(filepath)

# load R datae
load(file.choose())

# Datatype conversion of cpd_dataset1
cpd_dataset1$year <- as.factor(cpd_dataset1$year) # 2002 to 2017
cpd_dataset1$gender <- as.factor(cpd_dataset1$gender)
cpd_dataset1$rank <- as.factor(cpd_dataset1$rank)
cpd_dataset1$pay_grade <- as.factor(cpd_dataset1$pay_grade)
cpd_dataset1$employee_status <- as.factor(cpd_dataset1$employee_status)

cpd_dataset1$start_date <- mdy(cpd_dataset1$start_date)
cpd_dataset1$spp_date <- mdy(cpd_dataset1$spp_date)
cpd_dataset1$org_hire_date <- mdy(cpd_dataset1$org_hire_date)

# Data cleaning

cpd_dataset1$middle_initial <- NULL
cpd_dataset1$middle_initial2 <- NULL
cpd_dataset1$first_name_NS <- NULL
cpd_dataset1$last_name_NS <- NULL
cpd_dataset1$suffix_name <- NULL
cpd_dataset1$spp_date <- NULL

sum(is.na(cpd_dataset1)) # Neary 6380 NA. 3% of data

for (i in 1:nrow(cpd_dataset1)) {
  if(is.na(cpd_dataset1[i, "start_date"])) {
    cpd_dataset1[i, "start_date"] <- cpd_dataset1[i, "org_hire_date"]
  }
}

cpd_dataset1 <- na.omit(cpd_dataset1)

# Preparing dataset 1
# Creating new factor by grouping paygrades
# Recoing factor

D <- c("D|1", "D|2", "D|2A", "D|3")
E <- c("E|3", "E|4", "E|5", "E|6")
EX <- c("EX|9011", "EX|9701", "EX|9756", "EX|9781", "EX|9782")
SR <- c("SR|9742", "SR|9752", "SR|9759", "SR|9762", "SR|9780", "SR|9785", "SR|9786", "SR|9796")

cpd_dataset1$pay_group <- ifelse(cpd_dataset1$pay_grade %in% D, "D", 
                           ifelse(cpd_dataset1$pay_grade %in% E, "E",
                           ifelse(cpd_dataset1$pay_grade %in% EX, "EX", "SR")))


# Deriving variable start year; to calculate years in service.
cpd_dataset1$start_year <-year(cpd_dataset1$start_date)
cpd_dataset1$year_of_service <- (as.numeric(cpd_dataset1$year) + 2001) - cpd_dataset1$start_year  + 1

# Recoing factor - Political Event - New Mayor Selection
cpd_dataset1$event_new_mayor <- ifelse((as.numeric(cpd_dataset1$year) + 2001)
                                 >= 2011, "After Mayor R. Emmanuel", "Before Mayor R. Emmanuel")

# Preparing Dataset 2.
# Taking cpd employees with last unique record
cpd_dataset2 <- cpd_dataset1[order(cpd_dataset1$year, decreasing=T),]   
cpd_dataset2 <- cpd_dataset2[!duplicated(cpd_dataset2$fullname),]
str(cpd_dataset2)

# Removing columns which are not recquired for this analysis
cpd_dataset2$start_date <- NULL
cpd_dataset2$org_hire_date <- NULL
cpd_dataset2 <- data.frame(cpd_dataset2)
cpd_dataset2$row_id <- NULL
cpd_dataset2$fullname <- NULL
rownames(cpd_dataset2) <- cpd_dataset2$fullname
cpd_dataset2$first_name <- NULL  
cpd_dataset2$last_name <- NULL
cpd_dataset2$event_new_mayor <- as.factor(cpd_dataset2$event_new_mayor)

# Merging data of power of dollar or buying capacity for year 2002-2017. Taking reference year as 2018

# Below is the source of dolllar buying capacity indicator by year
# https://www.measuringworth.com/calculators/ppowerus/
years<-seq(from = 2002, to = 2017, by = 1)
multiplier <- c(1.36,1.33,1.30,1.26,1.22,1.18,1.14,1.14,1.12,1.09,1.07,1.05,1.04,1.03,1.02,1.0)

purchase_power_dollar <- data.frame(years, rep(2018, length(years)), multiplier)
colnames(purchase_power_dollar) <- c("Reference_Year", "Present_Year", "Dollar_Value_Multiplier")
purchase_power_dollar$Reference_Year<-as.factor(purchase_power_dollar$Reference_Year)

# Merging data x - dataset2 with purchase price of dollar
cpd_dataset2 <- merge(cpd_dataset2, purchase_power_dollar, by.x = "year", by.y = "Reference_Year", all.x = T)

# Data conversion and cleaning
cpd_dataset2$Present_Year <- NULL
cpd_dataset2$normalised_salary <- cpd_dataset2$salary * cpd_dataset2$Dollar_Value_Multiplier
cpd_dataset2$pay_group <- as.factor(cpd_dataset2$pay_group)
cpd_dataset2$employee_status <- droplevels(cpd_dataset2$employee_status)

# Dataset1 
# Merging crimedata with Dataset1
# Read more: http://www.city-data.com/crime/crime-Chicago-Illinois.html#ixzz5DWno6efF

# 2017 - Not given in above link =, but given 1.3% increase as compared to 2016

years <- seq(from = 2002, to = 2017, by = 1)
crimIndex <- c(746.1, 738.1, 650.2,	610.1,	604.2,	592.2,	615.5,	568.7
               ,540.4,	547.9,	536.3,	473.0,	425.4,	426.4,	510.7, 576.19)
crime_rate <- data.frame(years, crimIndex)

sal1<-merge(cpd_dataset1, crime_rate, by.x = "year", by.y = "years", all.x = T)
cpd_dataset1 <- sal1
remove(sal1)

######### 
# Linear Model - OLS
########

# Checking distribution od dependent var - salary
plot(density(cpd_dataset2$normalised_salary))
# Checking and removing outliers
outliers<-outliers_m(cpd_dataset2$normalised_salary)
cpdWithoutOutlier <- cpd_dataset2[-outliers,]

# Checking distribution od dependent var - salary. After removing outliers
plot(density(cpdWithoutOutlier$normalised_salary))

# 1st model
lm1<-lm(normalised_salary~ year_of_service + gender +pay_group + employee_status, data = cpdWithoutOutlier)
summary(lm1)

# Gender insignificant
# employee_status needs recoding or refactoring or releveling

dev.off()
par(mfrow = c(2,2)) # 4 plots on canvas
plot(lm1)

# Relevelling Employee Status
cpdWithoutOutlier$employee_status <- droplevels(cpdWithoutOutlier$employee_status)
cpdWithoutOutlier$employmentType <- "Regular"

for (i in 1: nrow(cpdWithoutOutlier)) {
  if(cpdWithoutOutlier$employee_status[i] == "PROBATIONARY CAREER SERVICE") {
    cpdWithoutOutlier$employmentType[i] <- "Probationary"
  }
}
table(cpdWithoutOutlier$employmentType)

# Method 2: cook's distance
tail(round(sort(cooks.distance(lm1)), 2))

outliers <- c(11515, 11693, 14995,  3463,  3842)
cpdWithoutOutlier_1 <- cpdWithoutOutlier[-outliers,]

lm2<-lm(normalised_salary~ year_of_service + pay_group + employmentType, data = cpdWithoutOutlier_1)
summary(lm2)

ggplot(lm2$model, aes_string(x = names(lm2$model)[2], y = names(lm2$model)[1])) + 
  geom_point() +
  stat_smooth(method = "lm", col = "red") +
  labs(title = paste("Adj R2 = ",signif(summary(lm2)$adj.r.squared, 5),
                     "Intercept =",signif(lm2$coef[[1]],5 ),
                     " Slope =",signif(lm2$coef[[2]], 5),
                     " P =",signif(summary(lm2)$coef[2,4], 5)))

dev.off()
par(mfrow = c(2,2)) # 4 plots on canvas
plot(lm2)

# Adjusted R-squared:  0.7885


#####
# Factor Analysis
# Clustering
####

# Taking a 10% sample. Taking a lot of time to run with all observations.

numrows <- nrow(cpd_dataset1)
sampsize <- round(numrows*0.10, 0)

set.seed(462) 
samp <- sample(numrows, sampsize)

cpd_dataset1_sample <- cpd_dataset1[samp, ]

cpd_dataset1_sample$employmentType <- "Regular"

for (i in 1: nrow(cpd_dataset1_sample)) {
  if(cpd_dataset1_sample$employee_status[i] == "PROBATIONARY CAREER SERVICE") {
    cpd_dataset1_sample$employmentType[i] <- "Probationary"
  }
}

cpd_dataset1_sample_scaled <- data.frame(scale(cpd_dataset1_sample[,c(4,8,15,16,18)]))

fa.parallel(cpd_dataset1_sample_scaled, fa="both", n.iter=100, show.legend=F)
cpd_s_fa <- factanal(cpd_dataset1_sample_scaled, 2) 
cpd_s_fa$loadings

cpd_dataset1_sample_scaled$year <- cpd_dataset1_sample$year
cpd_dataset1_sample_scaled$gender <- cpd_dataset1_sample$gender
cpd_dataset1_sample_scaled$rank <- cpd_dataset1_sample$rank
cpd_dataset1_sample_scaled$employee_status <- cpd_dataset1_sample$employee_status
cpd_dataset1_sample_scaled$pay_group <- as.factor(cpd_dataset1_sample$pay_group)
cpd_dataset1_sample_scaled$event_new_mayor <- as.factor(cpd_dataset1_sample$event_new_mayor)
cpd_dataset1_sample_scaled$employeeType <- as.factor(cpd_dataset1_sample$employmentType)

cpd_ds2_gowers <- daisy(cpd_dataset1_sample_scaled, metric = "gower")
cpd_ds2_gowers_hc <- hclust(cpd_ds2_gowers, method="ward.D2")

# Plot
plot(cpd_ds2_gowers_hc, hang=-1)
rect.hclust(cpd_ds2_gowers_hc, k=9, border="red") 
rect.hclust(cpd_ds2_gowers_hc, k=11, border="blue")# Good one!

hc_cut <- cutree(cpd_ds2_gowers_hc, k=11)
cpd_dataset1_sample$cluster <- hc_cut

# Analyzing Numeric
cpd_dataset1_sample[,c(4,8,18,16,15,20)] %>% group_by(cluster) %>% summarise_all(funs(mean)) 

# Analyzing Factors
ftable(cpd_dataset1_sample$cluster, cpd_dataset1_sample$year) 
ftable(cpd_dataset1_sample$cluster, cpd_dataset1_sample$gender) 
ftable(cpd_dataset1_sample$cluster, cpd_dataset1_sample$pay_group)
ftable(cpd_dataset1_sample$cluster, cpd_dataset1_sample$employee_status) 
ftable(cpd_dataset1_sample$cluster, cpd_dataset1_sample$event_new_mayor) 
ftable(cpd_dataset1_sample$cluster, cpd_dataset1_sample$employmentType) 

# 2d plot of clusters
clusplot(cpd_dataset1_sample[,c(4,8,18,16,15,20)], cpd_dataset1_sample$cluster, main='2D representation of the Cluster solution',
         color=TRUE, shade=TRUE)

# Conclusion:
# Cluster 9, 10 below to probationalry officer with experience 1 to 2 years.
# CLuster 7 belongs to pay group E
# High ranking officials belong to age group EX and SX.

# Effect of Mayor Selection
# Bivariate

# crime rate mayor
# After Mayor R. Emmanuel      500
# Before Mayor R. Emmanuel     630

cpd_dataset1_sample %>% group_by(event_new_mayor) %>% summarise(Average = mean(crimIndex))
qplot(x=crimIndex, y=event_new_mayor, data=cpd_dataset1_sample, ylab="Event- New Mayor", xlab="CrimeIndex")

boxplot(crimIndex ~ event_new_mayor, data = cpd_dataset1_sample, main="Distribution of crimIndex for event_new_mayor", 
        xlab = "event_new_mayor Categories (levels)", ylab = "crimIndex", col=c("orange", "lightblue4"))

# mayor and salaries

cpd_dataset1_sample %>% group_by(event_new_mayor) %>% summarise(Average = mean(salary))

boxplot(salary ~ event_new_mayor, data = cpd_dataset1_sample, main="Distribution of salary for event_new_mayor", 
        xlab = "event_new_mayor Categories (levels)", ylab = "salary", col=c("orange", "lightblue4"))

# Creating New Data frame to find the,
# Relation b/w workforce and avg.crime wrt year, ref = 2011 (Elections)

crimeHire_Year <- cpd_dataset1_sample[cpd_dataset1_sample$start_year >= 2002, ] %>% group_by(start_year) %>% summarise(hireCount = length(start_year), crimeIndex = mean(crimIndex))


crimeHire_Year$event <- "Before"
for (i in 1: nrow(crimeHire_Year)) {
  if(crimeHire_Year$start_year[i] >= 2011) {
    crimeHire_Year$event[i] <- "After"
  }
}

ggplot(crimeHire_Year, aes(x=start_year, y=crimeIndex, color=event, size=hireCount)) + geom_point(shape = 20) +
  ggtitle("Relation b/w workforce and avg.crime wrt year, ref = 2011 (Elections)") +
  labs(x = "Year", y = "Crime Index") +
  scale_x_continuous(breaks = seq(2002,2017,1))

###################################

# Bivariate and Univariate plots

qplot(gender, data=cpd_dataset2, geom="bar", fill=gender)
qplot(pay_group, data=cpd_dataset2, geom="bar", fill=pay_group)
qplot(employee_status, data=cpd_dataset2, geom="bar", fill=employee_status)
qplot(rank, data=cpd_dataset2, geom="bar", fill=rank)

qplot(normalised_salary, data=cpd_dataset2, geom="histogram", col=I("steelblue"), fill=I("blue"), alpha=I(.4))

qplot(x=normalised_salary, y=pay_group, data=cpd_dataset2, ylab="Pay group", xlab="Salary", facets=~gender)

ggplot(cpd_dataset2, aes(x=pay_group, y=normalised_salary, color=gender)) +
  geom_point() + geom_jitter() 

# salary and paygroup
qplot(x=pay_group, y=normalised_salary, data=cpd_dataset2, ylab="salary", xlab="pay_group")
aov_sal_pay_group <- aov(normalised_salary ~ pay_group, data = cpd_dataset2)
summary(aov_sal_pay_group) 
# H0 Null rejected (pValue < 5%). That mean is different in mean for all Categories

# salary and employee_status
qplot(x=employee_status, y=normalised_salary, data=cpd_dataset2, ylab="salary", xlab="employee_status")
aov_sal_employee_status <- aov(normalised_salary ~ employee_status, data = cpd_dataset2)
summary(aov_sal_employee_status) 
# H0 Null rejected (pValue < 5%). That mean is different in mean for all Categories

# salary and year
qplot(x=year, y=normalised_salary, data=cpd_dataset2, ylab="salary", xlab="year")
aov_sal_year <- aov(normalised_salary ~ year, data = cpd_dataset2)
summary(aov_sal_year) 
# H0 Null rejected (pValue < 5%). That mean is different in mean for all Categories

# salary by gender= 
# gender may impact
qplot(x=gender, y=normalised_salary, data=cpd_dataset2, ylab="salary", xlab="Gender")
# H0 Null rejected (pValue < 5%). That mean is different for Male and Female Categories
# F value -> Variation among sample means - 52.65.

aov_sal_gender <- aov(normalised_salary ~ gender, data = cpd_dataset2)
summary(aov_sal_gender) 

# salary and gender by paygroup
qplot(x=pay_group, y=normalised_salary, data=cpd_dataset2, ylab="salary", xlab="Gender", facets=~gender)

# salary and age at hire
qplot(y=normalised_salary, x=age_at_hire, data=cpd_dataset2, main="Salary and Age and at hire")

# Not significant
corSummary <- cor(cpd_dataset2$normalised_salary, cpd_dataset2$age_at_hire, use = "complete.obs", method = "pearson")
corSummary

corMatrix <- cor(cpd_dataset2[, c("normalised_salary", "age_at_hire")])
corrplot::corrplot(corMatrix, method = "circle", addCoef.col = "black")

# salary and year_of_service
qplot(y=normalised_salary, x=year_of_service, data=cpd_dataset2, main="Salary and Years in Service")

# Significant - 70.4%

corSummary <- cor(cpd_dataset2$normalised_salary, cpd_dataset2$year_of_service, use = "complete.obs", method = "pearson")
corSummary

corMatrix <- cor(cpd_dataset2[, c("normalised_salary", "year_of_service")])
corrplot::corrplot(corMatrix, method = "circle", addCoef.col = "black")


###################################################################

save.image(file ="final_term_project.Rdata")
