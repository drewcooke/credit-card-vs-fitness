

# Use pacman to check if packages are installed, then load
if (!require("pacman")) install.packages("pacman")
pacman::p_load(pdftools, stringr)

setwd("D:\\Documents\\DatSci\\TellAll\\Chase")
getwd()

chaseStatement <- function(x,y) { 
  pdfName <- x
  pdfGrab <- pdf_text(pdfName,"","")
  pdfRead <- pdfGrab[y]

  strCheck <- data.frame(str_locate(pdfRead,"PURCHASE\r\n"))
  if (is.na(strCheck$start)) strText <- " Amount\r\n" else strText <- "PURCHASE\r\n"
  endCheck <- data.frame(str_locate(pdfRead,"\r\n  INTEREST CHARGED\r\n"))
  if (is.na(endCheck$start)) endText <- "\r\n0000001" else endText <- "\r\n  INTEREST CHARGED\r\n" 
  
  strLocate <- data.frame(str_locate(pdfRead,strText))
  endLocate <- data.frame(str_locate(pdfRead,endText))
  purStart <- strLocate[!is.na(strLocate$end),2]+1
  endStart <- endLocate[!is.na(endLocate$start),1]

  #print(purStart)
  #print(endStart)
  
  if (length(endStart) == 0) endStart <- str_length(pdfRead)
  
  #print(endStart)
  
  if (length(purStart) > 0) {
    charges <- substr(pdfRead,purStart,endStart)
    chargeRows <- strsplit(charges,"\r\n")
    chargeRows <- unlist(chargeRows[1])
    chargeRows <- gsub("\r","",chargeRows)
    cDate <- as.Date(gsub("/","-",paste('2018/',str_squish(substr(chargeRows,1,15)),sep='')))
    cPlace <- str_squish(substr(chargeRows,22,71))
    #print(typeof(cPlace))
    
    clnStr <- function(x) {
      if (substr(x,1,2) == "& ") 
        x <- substr(x,3,50) 
      return(x)
    }
    cPlace <- sapply(cPlace, clnStr, simplify = TRUE, USE.NAMES = FALSE)
    
    cAmount <- as.numeric(substr(chargeRows,120,142)) 
    chargeData <- data.frame(cDate,cPlace,cAmount)
    chargeData <- chargeData[!is.na(chargeData$cDate),]
    chargeData <- chargeData[!is.na(chargeData$cAmount),]
    return(chargeData)
  }
}

data <- NULL

for (i in 1:11) {
  if (i<10) i<-paste("0",i,sep="")  
  setFile <- paste("2018",i,"04-statements-4105-.pdf", sep ="")
  
  stat <- chaseStatement(setFile,1)
  data <- rbind(data,stat)
  
  stat <- chaseStatement(setFile,3)
  data <- rbind(data,stat)
}

#data
str(data)

write.csv(data,file = "chase_data.csv", row.names = FALSE)