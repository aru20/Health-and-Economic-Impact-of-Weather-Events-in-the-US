getwd()
setwd("C:\\Users\\panig\\OneDrive\\Documents\\Coursera project\\Data_Science_Project\\Reproducible Research Assignment 2")
path = getwd()
url1 = "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2FStormData.csv.bz2"
destfile <-"C:\\Users\\panig\\OneDrive\\Documents\\Coursera project\\Data_Science_Project\\Reproducible Research Assignment 2\\StormData.csv.bz"

library(plyr)
library(RCurl)
install.packages ( "cluster" )
library(cluster)
install.packages("stringr")
library("stringr")
install.packages("stringdist")
library("stringdist")
library(ggplot2)
##Download the file
if(!file.exists("StormData.csv.bz2")){
download.file(url1,destfile)
}
## Read the Data
stormData1 <- read.csv("StormData.csv.bz",header = TRUE)
str(stormData1)
head(stormData1)
colnames(stormData1)
head(stormData1)


stormData1$year <- as.numeric(format(as.Date(stormData1$BGN_DATE, format = "%m/%d/%Y %H:%M:%S"), "%Y"))
hist(stormData1$year, breaks = 30)
##Based on the above histogram, we see that the number of events tracked starts to significantly increase around 1995. So, we use the subset of the data from 1990 to 2011 to get most out of good records.

stormData <- stormData1[stormData1$year >= 1995, ]
dim(stormData)
colnames(stormData)
##Now, there are 681500 rows and 38 columns in total.


# # Only use data where fatalities or injuries occurred.  
# stormData<- stormData[stormData$EVTYPE!= "?" & (stormData$INJURIES > 0 | stormData$FATALITIES > 0 | stormData$PROPDMG > 0 | stormData$CROPDMG > 0),]
# str(stormData)
# translate all letters to lowercase
StormData$EVTYPE <- tolower(StormData$EVTYPE)
unique(StormData$EVTYPE)
length(unique(StormData$EVTYPE))


# replace all punct. characters with a space
# event_types <- gsub("[[:blank:]]","",event_types)
# event_types <- gsub("[[:punct:]]",  "" , event_types)
StormData$EVTYPE <- gsub("[[:punct:][:blank:]]+", " ", StormData$EVTYPE)
length(unique(StormData$EVTYPE))
# update the data frame
StormData$EVTYPE <- str_replace_all(StormData$EVTYPE, "early","") 
StormData$EVTYPE <- str_replace_all(StormData$EVTYPE, "extreme","") 
StormData$EVTYPE <- str_replace_all(StormData$EVTYPE, "excessive","")
StormData$EVTYPE <- str_replace_all(StormData$EVTYPE, "gusty","")
StormData$EVTYPE <- str_replace_all(StormData$EVTYPE, "monthly","")
StormData$EVTYPE <- str_replace_all(StormData$EVTYPE, "record","")
StormData$EVTYPE <- str_replace_all(StormData$EVTYPE, "severe","")
StormData$EVTYPE <- str_replace_all(StormData$EVTYPE, "unseasonably","")
StormData$EVTYPE <- str_replace_all(StormData$EVTYPE, "urban","")
StormData$EVTYPE <- str_replace_all(StormData$EVTYPE, " ","") 
StormData$EVTYPE <- str_replace_all(StormData$EVTYPE, "/","")
StormData$EVTYPE[which(grepl("coldairtornado",  StormData$EVTYPE))] <- "tornado"
StormData$EVTYPE[which(grepl("frost\freeze", StormData$EVTYPE))] <- "frost"
length(unique(StormData$EVTYPE))
uniqueEvt <-(unique(StormData$EVTYPE))


distEvt <-stringdistmatrix(uniqueEvt,uniqueEvt,method="jw")
rownames(distEvt)<-uniqueEvt
hc <-hclust(as.dist(distEvt))
plot(hc)
dfClust <- data.frame(uniqueEvt,cutree(hc,k=100))
names(dfClust) <-c('Evtype','cluster')
plot(table(dfClust$cluster))
print(paste('Average no of event per cluster',mean(table(dfClust$cluster))))
t <-table(dfClust$cluster)
t <- cbind(t,t/length(dfClust$cluster))
t <-t[order(t[,2],decreasing = TRUE),]

p <- data.frame(factorName=row.names(t), binCount = t[,1], percentFound=t[,2])


dfClust <-merge(x= dfClust,y = p, by.x = "cluster", by.y = "factorName" ,all.x = T)


dfClust <-dfClust[rev(order(dfClust$binCount)),]

names(dfClust) <-c('cluster','Evttype')
head(dfClust[c('cluster','Evttype')],100)

#processedstormdata$EVTYPE <- str_replace_all(processedstormdata$EVTYPE, "early","") 


# hail <- eventType$EVTYPE[grep("HAIL",eventType$EVTYPE,ignore.case = TRUE)]
# 
# tornado <- eventType$EVTYPE[grep("TORNADO",eventType$EVTYPE,ignore.case = TRUE)]
# tornado
#  