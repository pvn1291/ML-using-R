# Student : Pavan Dinkar Palve | 670397922 | ppalve2@uic.edu
# Midterm project
# Data: Federal Employee Viewpoint Survey 2017
# Objective: Identify the factors impacting the employee satisfaction
# ----------------------------------------------------------------------------------------------------

# --------------------------------------- Step 0: Load data in ---------------------------------------
library(readr)
library(tibble)
library(dplyr)
library(zoo)
library(tidyverse)
library(ggplot2)
library(gridExtra)

fevsSurvey <- read_csv(file.choose())

# --------------------------------------- Step 1: Data Wrangling -------------------------------------
    # Structure of the data:
        str(fevsSurvey)
        class(fevsSurvey <- read_csv(file.choose()))
        View(fevsSurvey)
    
    # Converting variables to factors
        for(i in 9:ncol(fevsSurvey)) {
            fevsSurvey[, i]<-as.factor(as.numeric(unlist(fevsSurvey[, i])))
        }
        
        for(i in 1:8) {
            fevsSurvey[, i]<-as.factor(unlist(fevsSurvey[, i]))
        }
        
        glimpse(fevsSurvey)
        
    # Checking for NAs
         
        View(naInfo)
        
    # Removing NAs
        fevsSurvey<-na.locf(fevsSurvey)

    # Removing Unnecessary variable- POSTWT
        fevsSurvey <- fevsSurvey[, -80] # Removing POSTWT variable

    # Removing records with 'X' values
        for(i in 9:(ncol(fevsSurvey) - 1)) {
            indexes <- which((fevsSurvey[,i] == "X") == TRUE);
            if (length(indexes) > 0) {
                fevsSurvey[indexes, i] <- 0
            }
        }
        
    # Recoding agency factor as small, medium and large
        tab<-table(fevsSurvey$AGENCY)
        which((tab > 40000) == TRUE) # AG HE HS TR VA
        which((tab <= 40000 & tab > 5000) == TRUE) # AF AR CM DD DJ DL DN EP GS IN NN NV SZ TD 
        which((tab <= 5000) == TRUE) # AM BG BO CT CU DR ED EE FC FQ FT HU IB NF NL NQ NU OM RR SB SE SN ST XX 
        
        large <- c("AG","HE","HS","TR","VA")
        medium <- c("AF","AR","CM","DD","DJ","DL","DN","EP","GS","IN","NN","NV","SZ","TD")
        small <- c("AM","BG","BO","CT","CU","DR","ED","EE","FC","FQ","FT","HU","IB","NF","NL","NQ","NU",
                   "OM","RR","SB","SE","SN","ST","XX")
        count <- 1;
        agencyDef <- NULL
        for(i in fevsSurvey$AGENCY) {
            print(i)
            if (i %in% large) {
                agencyDef[count] <- "Large"
            } else if(i %in% medium) {
                agencyDef[count] <- "Medium"
            } else {
                agencyDef[count] <- "Small"
            }
            count <- count + 1
        }
        fevsSurvey$agencyDef <- agencyDef
        table(fevsSurvey$agencyDef)
        fevsSurvey$agencyDef <- as.factor(fevsSurvey$agencyDef)
        table(fevsSurvey$agencyDef)
        
    # Selection of survey question relevant to research question
        
        # Input question reference csv file
        questionReferences <- read.csv(file.choose(), header = T)
        rownames(questionReferences) <- questionReferences[,1]
        questionReferences$Question.. <- NULL
        
        questionNumbersForEEI <- rownames(questionReferences[questionReferences$EEI != "N/A",]);questionNumbersForEEI
        
        questionNumbersForEPI <- rownames(questionReferences[questionReferences$EPI != "N/A",]);questionNumbersForEPI
        
        questionNumbersForIQ <- rownames(questionReferences[questionReferences$IQ != "N/A",]);questionNumbersForIQ
        
        unique_questions <- unique(c(questionNumbersForEEI, questionNumbersForEPI, questionNumbersForIQ));unique_questions
        unique_questions <- c(colnames(fevsSurvey[,c(1:8)]), unique_questions)
        unique_questions <- c(unique_questions, "agencyDef")
        unique_questions
        
        fevsSurvey <- fevsSurvey[,unique_questions]
        glimpse(fevsSurvey)
        
# --------------------------------------- Step 2: Univariate & Bivariate Analysis --------------------
    # Barplots for all of the factor variables    
        univariate_barplots <- function(df){
            filename <- NULL
            
            for (colIndex in 1:ncol(df)){
                prpTable <- NULL
                colName <- colnames(df)[colIndex]
                if (class(unlist(df[, colName])) == "factor"){
                    filename <- paste("/Users/pvn1291/Desktop/Plots/", colName, ".pdf")
                    print(filename)
                    
                    df %>% count(unlist(df[, colName])) %>% mutate(perc = n / nrow(df)) -> prpTable
                    
                    ggplot(prpTable, aes(x = levels(unlist(prpTable[, 1])), y = perc, 
                                         fill = levels(unlist(prpTable[, 1])))) +
                        geom_bar(stat = "identity") +
                        labs(x = paste("Response levels to ", colName),
                             y = "Percent of Responses", title = "Proportion of Responses",
                             fill = levels(unlist(prpTable[, 1])))
                    ggsave(filename)
                }
            }
        }
        
        univariate_barplots(fevsSurvey[,questionNumbersForEPI]) 
        univariate_barplots(fevsSurvey[,questionNumbersForIQ])
        univariate_barplots(fevsSurvey[,questionNumbersForEEI])
        
    # Chi-Square Tests
        chiTest <- function (v1, df2) {
            df_test <-data.frame()
            for (i in v1) {
                for(j in v1) {
                    table <- table(as.factor(unlist(df2[, i])), as.factor(unlist(df2[, j])))
                    val <- chisq.test(table)$p.value
                    if(i != j) {
                        if(val <= 0.05) {
                            df_test[i, j] <- "Dependent"
                        } else {
                            df_test[i, j] <- "Independent"
                        }
                    } else {
                        df_test[i, j] <- "1"
                    }
                }
            }
            return(df_test)
        }
        
        eei_chi <- chiTest(questionNumbersForEEI, fevsSurvey)
        epi_chi <- chiTest(questionNumbersForEPI, fevsSurvey)
        iq_chi <- chiTest(questionNumbersForIQ, fevsSurvey)
        wqi_chi <- chiTest(questionNumbersForWQI, fevsSurvey)
        
    # Creation of Index Columns    
        eei_df <- fevsSurvey[,c(colnames(fevsSurvey[,c(1:8)]), questionNumbersForEEI, "agencyDef")]
        epi_df <- fevsSurvey[,c(colnames(fevsSurvey[,c(1:8)]), questionNumbersForEPI, "agencyDef")]
        iq_df <- fevsSurvey[,c(colnames(fevsSurvey[,c(1:8)]), questionNumbersForIQ, "agencyDef")]
        wqi_df <- fevsSurvey[,c(colnames(fevsSurvey[,c(1:8)]), questionNumbersForWQI, "agencyDef")]
        
        subsetEEI <- eei_df[,questionNumbersForEEI];subsetEEI
        sum <- apply(subsetEEI, 1, function(x) sum(as.numeric(x)))
        eei_df$scaled <- sum / length(questionNumbersForEEI)
        
        sum <- NULL
        subsetEPI <- epi_df[,questionNumbersForEPI];subsetEPI
        sum <- apply(subsetEPI, 1, function(x) sum(as.numeric(x)))
        epi_df$scaled <- sum / length(questionNumbersForEPI)
        
        sum <- NULL
        subsetIQ <- iq_df[,questionNumbersForIQ];subsetIQ
        sum <- apply(subsetIQ, 1, function(x) sum(as.numeric(x)))
        iq_df$scaled <- sum / length(questionNumbersForIQ)
        
        eei_df$scaledFactor[eei_df$scaled >= 3.5] <- 1
        eei_df$scaledFactor[eei_df$scaled < 3.5] <- 0
        eei_df$scaledFactor<-as.factor(eei_df$scaledFactor)
        
        epi_df$scaledFactor[epi_df$scaled >= 3.5] <- 1
        epi_df$scaledFactor[epi_df$scaled < 3.5] <- 0
        epi_df$scaledFactor<-as.factor(epi_df$scaledFactor)
        
        iq_df$scaledFactor[iq_df$scaled >= 3.5] <- 1
        iq_df$scaledFactor[iq_df$scaled < 3.5] <- 0
        iq_df$scaledFactor<-as.factor(iq_df$scaledFactor)
        
        finalFevsData <- data.frame(eei_df$scaled, eei_df$scaledFactor, epi_df$scaled, epi_df$scaledFactor,
                                    iq_df$scaled, iq_df$scaledFactor)
        finalFevsData <- data.frame(finalFevsData, fevsSurvey$agencyDef)
        colnames(finalFevsData) <- c("EEI_SCORE", "EEI_SCORE_SCALED", "EPI_SCORE", "EPI_SCORE_SCALED",
                                     "IQ_SCORE", "IQ_SCORE_SCALED", "AGENCY")
        str(finalFevsData)
        
    # Histograms for Index Variables    
        qplot(EEI_SCORE, data=finalFevsData, geom="histogram", col=I("steelblue"), fill=I("blue"), alpha=I(.4)) +
            geom_vline(xintercept = median(finalFevsData$EEI_SCORE), color="red", lwd=2) 
        
        qplot(EPI_SCORE, data=finalFevsData, geom="density")
        
        qplot(EPI_SCORE, data=finalFevsData, geom="histogram", col=I("steelblue"), fill=I("blue"), alpha=I(.4)) +
            geom_vline(xintercept = median(finalFevsData$EPI_SCORE), color="red", lwd=2) 
        
        qplot(EPI_SCORE, data=finalFevsData, geom="density")
        
        qplot(IQ_SCORE, data=finalFevsData, geom="histogram", col=I("steelblue"), fill=I("blue"), alpha=I(.4)) +
            geom_vline(xintercept = median(finalFevsData$IQ_SCORE), color="red", lwd=2) 
        
        qplot(IQ_SCORE, data=finalFevsData, geom="density")
        
    # Bi Variate Analysis
        dev.off()
        finalFevsData %>% ggplot(aes(x=EPI_SCORE_SCALED, y=EEI_SCORE, color = EPI_SCORE_SCALED)) + geom_boxplot()
        ggsave("EEI_EPI.pdf")
        
        finalFevsData %>% ggplot(aes(x=IQ_SCORE_SCALED, y=EEI_SCORE, color = IQ_SCORE_SCALED)) + 
            geom_boxplot()
        ggsave("EEI_IQ.pdf")
        
        
        finalFevsData %>% ggplot(aes(x=AGENCY, y=EEI_SCORE, color = AGENCY)) + 
            geom_boxplot()
        ggsave("EEI_AGENCY.pdf")
        
        finalFevsData %>% ggplot(aes(x=AGENCY, y=EPI_SCORE, color = AGENCY)) + 
            geom_boxplot()
        ggsave("EPI_AGENCY.pdf")
        
        finalFevsData %>% ggplot(aes(x=AGENCY, y=IQ_SCORE, color = AGENCY)) + 
            geom_boxplot()
        ggsave("IQ_AGENCY.pdf")
        
# --------------------------------------- Step 3: Data Modelling -------------------------------------
    # Linear Model
        mod6 <- lm(EEI_SCORE ~ EPI_SCORE + IQ_SCORE, data = finalFevsData)
        summary(mod6)
        
        plot(mod6)
        
# ---------------------------------------------------------------------------------------------------
