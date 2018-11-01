mod <- lm(Revenue ~ Age + Gender, data = ins)
summary(mod)

ins$revDummy <- abs(min(ins$Revenue)) + ins$Revenue + 1
ins$revDummy
mod <- lm(revDummy ~ Age + Gender, data = ins)
summary(mod)
View(ins)

accidents <- ins[ins$ClaimsInd == 1, ]
noAccidents <- ins[ins$ClaimsInd == 0, ]

mod1 <- lm(Revenue[Year == '2005'] ~ Age[Year == '2005'] + Gender[Year == '2005'],data = ins)
summary(mod1)

outliers_m <- function(column) {
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

rev_out_mod <- outliers_m(ins$Revenue)

new_ins <- ins[-rev_out_mod,]
new_ins

options(scipen = 999)


mod <- lm(revDummy[Year == '2008'] ~ Zone[Year == '2008'] + Age[Year == '2008'] + Gender[Year == '2008'] + IDV[Year == '2008'], data = new_ins)
summary(mod)

#--------------- 2005 ------------------
modNrth2005 <- lm(revDummy[Zone == 'North' &  Year == '2005'] ~ Age[Zone == 'North' & Year == '2005'] + Gender[Zone == 'North' & Year == '2005'] + IDV[Zone == 'North' & Year == '2005'], data = new_ins)
summary(modNrth2005)

#--------------- 2006 ------------------
modNrth2006 <- lm(revDummy[Zone == 'North' &  Year == '2006'] ~ Age[Zone == 'North' & Year == '2006'] + Gender[Zone == 'North' & Year == '2006'] + IDV[Zone == 'North' & Year == '2006'], data = new_ins)
summary(modNrth2006)

#--------------- 2007 ------------------
modNrth2007 <- lm(revDummy[Zone == 'North' &  Year == '2007'] ~ Age[Zone == 'North' & Year == '2007'] + Gender[Zone == 'North' & Year == '2007'] + IDV[Zone == 'North' & Year == '2007'], data = new_ins)
summary(modNrth2007)

#--------------- 2008 ------------------
modNrth2008 <- lm(revDummy[Zone == 'North' &  Year == '2008'] ~ Age[Zone == 'North' & Year == '2008'] + Gender[Zone == 'North' & Year == '2008'] + IDV[Zone == 'North' & Year == '2008'], data = new_ins)
summary(modNrth2008)

#--------------- 2009 ------------------
modNrth2009 <- lm(revDummy[Zone == 'North' &  Year == '2009'] ~ Age[Zone == 'North' & Year == '2009'] + Gender[Zone == 'North' & Year == '2009'] + IDV[Zone == 'North' & Year == '2009'], data = new_ins)
summary(modNrth2009)

#--------------- 2010 ------------------
modNrth2010 <- lm(revDummy[Zone == 'North' &  Year == '2010'] ~ Age[Zone == 'North' & Year == '2010'] + Gender[Zone == 'North' & Year == '2010'] + IDV[Zone == 'North' & Year == '2010'], data = new_ins)
summary(modNrth2010)

#--------------- 2011 ------------------
modNrth2011 <- lm(revDummy[Zone == 'North' &  Year == '2011'] ~ Age[Zone == 'North' & Year == '2011'] + Gender[Zone == 'North' & Year == '2011'] + IDV[Zone == 'North' & Year == '2011'], data = new_ins)
summary(modNrth2011)


#---------------- Year Control Variable ------------

#--------------- 2005 ------------------
modNrth2005 <- lm(revDummy[Year == '2005'] ~ Zone[Year == '2005'] + Age[Year == '2005'] + Gender[Year == '2005'] + IDV[Year == '2005'], data = new_ins)
summary(modNrth2005)

#--------------- 2006 ------------------
modNrth2006 <- lm(revDummy[Year == '2006'] ~ Zone[Year == '2006'] + Age[Year == '2006'] + Gender[Year == '2006'] + IDV[Year == '2006'], data = new_ins)
summary(modNrth2006)

#--------------- 2007 ------------------
modNrth2007 <- lm(revDummy[Year == '2007'] ~ Zone[Year == '2007'] + Age[Year == '2007'] + Gender[Year == '2007'] + IDV[Year == '2007'], data = new_ins)
summary(modNrth2007)

#--------------- 2008 ------------------
modNrth2008 <- lm(revDummy[Year == '2008'] ~ Zone[Year == '2008'] + Age[Year == '2008'] + Gender[Year == '2008'] + IDV[Year == '2008'], data = new_ins)
summary(modNrth2008)

#--------------- 2009 ------------------
modNrth2009 <- lm(revDummy[Year == '2009'] ~ Zone[Year == '2009'] + Age[Year == '2009'] + Gender[Year == '2009'] + IDV[Year == '2009'], data = new_ins)
summary(modNrth2009)

#--------------- 2010 ------------------
modNrth2010 <- lm(revDummy[Year == '2010'] ~ Zone[Year == '2010'] + Age[Year == '2010'] + Gender[Year == '2010'] + IDV[Year == '2010'], data = new_ins)
summary(modNrth2010)

#--------------- 2011 ------------------
modNrth2011 <- lm(revDummy[Year == '2011'] ~ Zone[Year == '2011'] + Age[Year == '2011'] + Gender[Year == '2011'] + IDV[Year == '2011'], data = new_ins)
summary(modNrth2011)



mod <- lm(revDummy[Zone == 'North' & Gender == 'Male'] ~ Age[Zone == 'North' & Gender == 'Male'] + IDV[Zone == 'North' & Gender == 'Male'], data = new_ins)
summary(mod)

mod <- lm(revDummy[Zone == 'North' & Gender == 'Female'] ~ Age[Zone == 'North' & Gender == 'Female'] + IDV[Zone == 'North' & Gender == 'Female'], data = new_ins)
summary(mod)



new_ins

table(ins$Year)

sum(ins$Revenue[ins$Year == '2005'])
sum(ins$Revenue[ins$Year == '2006'])
sum(ins$Revenue[ins$Year == '2007'])
sum(ins$Revenue[ins$Year == '2008'])
sum(ins$Revenue[ins$Year == '2009'])
sum(ins$Revenue[ins$Year == '2010'])
sum(ins$Revenue[ins$Year == '2011'])

