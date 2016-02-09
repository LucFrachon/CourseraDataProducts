# System set up

library(caret); library(rpart); library(rattle); library(rpart.plot)
library(randomForest); library(dplyr); library(parallel); library(doParallel)
library(ipred); library(RANN)

cluster <- makeCluster(detectCores())
registerDoParallel(cluster)
fitControl <- trainControl(allowParallel = TRUE)
set.seed(2077)



# Data acquisition and munging

prepFullDataset <- function(filePath) {
    if (!file.exists(filePath)) {
        print("Using file from Repo")
    }
    rawData <- read.csv(file = filePath, stringsAsFactors = FALSE,
                        na.strings = c("", "NA", "#DIV/0!"), nrows = 1000)
    ## Manually fix ambiguous cabin numbers:
    rawData$Cabin[76] <- "G73"
    rawData$Cabin[129] <- "E69"
    rawData$Cabin[700] <- "G63"
    rawData$Cabin[716] <- "G73"
    ## Extract surnames and titles from Name:
    rawData$title <- ""
    rawData$surname <- ""
    for (r in 1:nrow(rawData)) {
        rawData$surname[r] <- unlist(strsplit(rawData$Name[r], ","))[1]
        rawData$title[r] <- unlist(strsplit(unlist(strsplit(
            rawData$Name[r], ","))[2], "[.]"))[1]
    }
    ## Filter only numeric values for ticket number:
    for (r in 1:nrow(rawData)) {
        pos=regexpr("[0123456789]*$",rawData$Ticket[r])
        rawData$ticketNum[r] <- substring(rawData$Ticket[r], pos)
    }
    ## 4 passengers with missing ticket numbers: Assign random values:
    rawData$ticketNum[180] <- runif(1, min = min(as.numeric(rawData$ticketNum)), 
                                    max = max(as.numeric(rawData$ticketNum)))
    rawData$ticketNum[272] <- runif(1, min = min(as.numeric(rawData$ticketNum)), 
                                    max = max(as.numeric(rawData$ticketNum)))
    rawData$ticketNum[303] <- runif(1, min = min(as.numeric(rawData$ticketNum)), 
                                    max = max(as.numeric(rawData$ticketNum)))
    rawData$ticketNum[598] <- runif(1, min = min(as.numeric(rawData$ticketNum)), 
                                    max = max(as.numeric(rawData$ticketNum)))
    
}

cleanData <- prepFullDataset("data/train.csv")