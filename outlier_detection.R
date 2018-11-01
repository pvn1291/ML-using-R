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
  
}