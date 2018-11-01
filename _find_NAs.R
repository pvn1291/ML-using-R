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