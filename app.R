
library(shiny)
library(golem)
library(ggplot2)
library(ggthemes)
library(ggpubr)
library(tidyverse)
library(data.table)
library(tidyr)
library(mwshiny)
library(curl)
library(readxl)
library(rsconnect)
library(sp)
library(ggplot2)
library(ggthemes)
library(ggpubr)
library(sp)

tmp <- tempfile()
file_URL <- "https://data.humdata.org/hxlproxy/api/data-preview.csv?url=https%3A%2F%2Fraw.githubusercontent.com%2FCSSEGISandData%2FCOVID-19%2Fmaster%2Fcsse_covid_19_data%2Fcsse_covid_19_time_series%2Ftime_series_covid19_confirmed_global.csv&filename=time_series_covid19_confirmed_global.csv"
curl_download(file_URL, tmp)
allcovcases <- read.csv(tmp)
file_URL <- "https://data.humdata.org/hxlproxy/api/data-preview.csv?url=https%3A%2F%2Fraw.githubusercontent.com%2FCSSEGISandData%2FCOVID-19%2Fmaster%2Fcsse_covid_19_data%2Fcsse_covid_19_time_series%2Ftime_series_covid19_deaths_global.csv&filename=time_series_covid19_deaths_global.csv"
curl_download(file_URL, tmp)
allcovdeaths <- read.csv(tmp)
#view(allcovdeaths)
covcases<-allcovcases %>% filter(Province.State =="Faroe Islands" 
                                 | Province.State=="Greenland" | Country.Region == "Sweden" 
                                 | Country.Region == "Finland" | Country.Region == "Norway"
                                 | Country.Region == "Ireland" | Country.Region == "Iceland")
covdeaths<-allcovdeaths %>% filter(Province.State =="Faroe Islands" 
                                   | Province.State=="Greenland" | Country.Region == "Sweden" 
                                   | Country.Region == "Finland" | Country.Region == "Norway"
                                   | Country.Region == "Ireland" | Country.Region == "Iceland")

#SORT CASES DATA
#drop Lat/Long columns
covcases2 = subset(covcases, select = -c(Lat,Long)) 
#cascade country.region into province.state where there's space
covcases3 <- mutate(covcases2, Province.State = ifelse(covcases2$Province.State == "", as.character(covcases2$Country.Region), as.character(covcases2$Province.State)))
#drop Country.Region column
covcases3 = subset(covcases3, select = -c(Country.Region)) 
#transpose it but keep names of 1st column (places) as headers
flipcases = setNames(data.frame(t(covcases3[,-1])), covcases3[,1])
#now make rownames (dates) into 1st column
setDT(flipcases, keep.rownames = "cDate")
#now populate cDate with ACTUAL dates - will make graphing easier - are in MM/DD/YY formate in base CSV
flipcases$cDate = seq(as.Date("2020-01-22"),by = "days", length.out = nrow(flipcases))
#rename column Faroe Islands
names(flipcases)[names(flipcases) == "Faroe Islands"] <- "FaroeIslands"


#SORT DEATHS DATA
#drop Lat/Long columns
covdeaths2 = subset(covdeaths, select = -c(Lat,Long)) 
#cascade country.region into province.state where there's space
covdeaths3 <- mutate(covdeaths2, Province.State = ifelse(covdeaths2$Province.State == "", as.character(covdeaths2$Country.Region), as.character(covdeaths2$Province.State)))
#drop Country.Region column
covdeaths3 = subset(covdeaths3, select = -c(Country.Region)) 
#transpose it but keep names of 1st column (places) as headers
flipdeaths = setNames(data.frame(t(covdeaths3[,-1])), covdeaths3[,1])
#now make rownames (dates) into 1st column
setDT(flipdeaths, keep.rownames = "cDate")
#now populate cDate with ACTUAL dates - will make graphing easier - are in MM/DD/YY formate in base CSV
flipdeaths$cDate = seq(as.Date("2020-01-22"),by = "days", length.out = nrow(flipdeaths))
#rename column Faroe Islands
names(flipdeaths)[names(flipdeaths) == "Faroe Islands"] <- "FaroeIslands"

# so CSV data from John Hopkins only has Total i.e. Cumulative cases 
# the following calculates the delta i.e. daily new cases/deaths
flipcases$deltaFar <- flipcases$FaroeIslands - lag(flipcases$FaroeIslands)
flipcases$deltaGrl <- flipcases$Greenland - lag(flipcases$Greenland)
flipcases$deltaFin <- flipcases$Finland - lag(flipcases$Finland)
flipcases$deltaIsl <- flipcases$Iceland - lag(flipcases$Iceland)
flipcases$deltaIre <- flipcases$Ireland - lag(flipcases$Ireland)
flipcases$deltaNor <- flipcases$Norway - lag(flipcases$Norway)
flipcases$deltaSwe <- flipcases$Sweden - lag(flipcases$Sweden)

flipdeaths$deltaFar <- flipdeaths$FaroeIslands - lag(flipdeaths$FaroeIslands)
flipdeaths$deltaGrl <- flipdeaths$Greenland - lag(flipdeaths$Greenland)
flipdeaths$deltaFin <- flipdeaths$Finland - lag(flipdeaths$Finland)
flipdeaths$deltaIsl <- flipdeaths$Iceland - lag(flipdeaths$Iceland)
flipdeaths$deltaIre <- flipdeaths$Ireland - lag(flipdeaths$Ireland)
flipdeaths$deltaNor <- flipdeaths$Norway - lag(flipdeaths$Norway)
flipdeaths$deltaSwe <- flipdeaths$Sweden - lag(flipdeaths$Sweden)


#set populations 
#faroe islands #greenland  #Finland # Iceland # Ireland #Norway #sweden '10.23m #NI # Scotland
popns <- c(52123, 56992, 5518000, 339031, 4904000, 5515736, 10230000, 1822000, 5454000)

#NOW GO GET SCO & NI

# new code June 2024
file_URL <- "https://raw.githubusercontent.com/mikeocLimerick/COVIDWATCH-NPA/main/UKdata"
UKNIdata <- read_delim(file_URL, delim = "\t", col_names = TRUE)
UKNIdata$cDate<-as.Date(UKNIdata$cDate,"%d/%m/%Y")

#Old code
#file_URL <- "https://api.coronavirus.data.gov.uk/v1/data?filters=areaType=nation&structure=%7B%22areaType%22:%22areaType%22,%22areaName%22:%22areaName%22,%22areaCode%22:%22areaCode%22,%22date%22:%22date%22,%22newCasesByPublishDate%22:%22newCasesByPublishDate%22,%22cumCasesByPublishDate%22:%22cumCasesByPublishDate%22%7D&format=csv"
#curl_download(file_URL, tmp)
#UKNIcases <- read.csv(tmp)
#view(UKNIcases)
### get deaths UK (incl NI)
#file_URL <- "https://api.coronavirus.data.gov.uk/v1/data?filters=areaType=nation&structure=%7B%22areaType%22:%22areaType%22,%22areaName%22:%22areaName%22,%22areaCode%22:%22areaCode%22,%22date%22:%22date%22,%22newDeaths28DaysByPublishDate%22:%22newDeaths28DaysByPublishDate%22,%22cumDeaths28DaysByPublishDate%22:%22cumDeaths28DaysByPublishDate%22%7D&format=csv"
#curl_download(file_URL, tmp)
#UKNIdeaths <- read.csv(tmp)
#mergethetwo
#UKNIdata <- merge(UKNIcases,UKNIdeaths,by=c('date','areaName'),all.x=T)
#UKNIdata$date<-as.Date(UKNIdata$date)
#drop "areaType" & areaCode columns
#UKNIdata = UKNIdata[,!(names(UKNIdata) %in% c("areaType.x","areaType.y"))]
#UKNIdata = UKNIdata[,!(names(UKNIdata) %in% c("areaCode.x","areaCode.y"))]
#
#renameheaders#
#UKNIdata <- UKNIdata %>% 
#  rename(
#    cDate = date,
#    country = areaName,
#    newcases = newCasesByPublishDate,
#    cumcases = cumCasesByPublishDate, 
#    newdeaths = newDeaths28DaysByPublishDate,
#    cumdeaths = cumDeaths28DaysByPublishDate 
#  )


NIdata <- filter(UKNIdata,country=="Northern Ireland")
SCOdata <- filter(UKNIdata,country=="Scotland")

getallcases <- left_join(flipcases, NIdata, by=c("cDate"))
#Get cumulative figures first
#so drop the NEW data
getallcasesTOT = subset(getallcases, select = -c(deltaIre,deltaGrl,deltaFar,deltaIsl,deltaFin,deltaSwe,deltaNor)) 

#drop "areaType" & areaCode columns
getallcasesTOT = subset(getallcasesTOT, select = -c(country,newcases,newdeaths,cumdeaths)) 
names(getallcasesTOT)[names(getallcasesTOT) == "cumcases"] <- "NorthernIreland"
getallcasesTOT <- left_join(getallcasesTOT, SCOdata, by=c("cDate"))
#drop country, newcases, newdeaths, cumdeaths (i.e. leave cumcases)
getallcasesTOT = subset(getallcasesTOT, select = -c(country,newcases,newdeaths,cumdeaths)) 
names(getallcasesTOT)[names(getallcasesTOT) == "cumcases"] <- "Scotland"
getallcasesTOT$NorthernIreland[is.na(getallcasesTOT$NorthernIreland)] <- 0
getallcasesTOT$Scotland[is.na(getallcasesTOT$Scotland)] <- 0

#then new cases - remember getallcases features 7 countries + all NI data
#remove cumulative totals
getallcasesNEW = subset(getallcases, select = -c(Ireland,Greenland,FaroeIslands,Iceland,Finland,Sweden,Norway)) 
#now rename the deltas to be the country names
names(getallcasesNEW)[names(getallcasesNEW) == "deltaFar"] <- "FaroeIslands"
names(getallcasesNEW)[names(getallcasesNEW) == "deltaGrl"] <- "Greenland"
names(getallcasesNEW)[names(getallcasesNEW) == "deltaIre"] <- "Ireland"
names(getallcasesNEW)[names(getallcasesNEW) == "deltaIsl"] <- "Iceland"
names(getallcasesNEW)[names(getallcasesNEW) == "deltaFin"] <- "Finland"
names(getallcasesNEW)[names(getallcasesNEW) == "deltaSwe"] <- "Sweden"
names(getallcasesNEW)[names(getallcasesNEW) == "deltaNor"] <- "Norway"
getallcasesNEW = subset(getallcasesNEW, select = -c(country,cumcases,newdeaths,cumdeaths)) 
names(getallcasesNEW)[names(getallcasesNEW) == "newcases"] <- "NorthernIreland"
getallcasesNEW <- left_join(getallcasesNEW, SCOdata, by=c("cDate"))
#drop country, newcases, newdeaths, cumdeaths (i.e. leave cumcases)
getallcasesNEW = subset(getallcasesNEW, select = -c(country,cumcases,newdeaths,cumdeaths)) 
names(getallcasesNEW)[names(getallcasesNEW) == "newcases"] <- "Scotland"
getallcasesNEW$NorthernIreland[is.na(getallcasesNEW$NorthernIreland)] <- 0
getallcasesNEW$Scotland[is.na(getallcasesNEW$Scotland)] <- 0

savedeltacases<-getallcasesNEW
savedeltacases$Mth= format(as.Date(savedeltacases$cDate), "%m-%y")

savedcasesmth<-savedeltacases %>% 
  group_by(Mth) %>% 
  summarise(FaroeIslands=sum(FaroeIslands),Greenland=sum(Greenland),
            Finland=sum(Finland),Iceland=sum(Iceland),Ireland=sum(Ireland),
            Norway=sum(Norway),Sweden=sum(Sweden),NorthernIreland=sum(NorthernIreland),Scotland=sum(Scotland))

# DEATHS
getalldeaths <- left_join(flipdeaths, NIdata, by=c("cDate"))
#Get cumulative figures for Deaths first - note above is the data for the 7 countries + NI
#so drop the NEW data
getalldeathsTOT = subset(getalldeaths, select = -c(deltaIre,deltaGrl,deltaFar,deltaIsl,deltaFin,deltaSwe,deltaNor)) 
#drop columns relating to NI data from above
getalldeathsTOT = subset(getalldeathsTOT, select = -c(country,newcases,cumcases,newdeaths)) 
names(getalldeathsTOT)[names(getalldeathsTOT) == "cumdeaths"] <- "NorthernIreland"
getalldeathsTOT <- left_join(getalldeathsTOT, SCOdata, by=c("cDate"))
#drop stuff
getalldeathsTOT = subset(getalldeathsTOT, select = -c(country,newcases,cumcases,newdeaths)) 
names(getalldeathsTOT)[names(getalldeathsTOT) == "cumdeaths"] <- "Scotland"

getalldeathsTOT$NorthernIreland[is.na(getalldeathsTOT$NorthernIreland)] <- 0
getalldeathsTOT$Scotland[is.na(getalldeathsTOT$Scotland)] <- 0

#Then New Deaths #drop the cumulative data columns
getalldeathsNEW = subset(getalldeaths, select = -c(Ireland,Greenland,FaroeIslands,Iceland,Finland,Sweden,Norway)) 
#now rename the deltas to be the country names
names(getalldeathsNEW)[names(getalldeathsNEW) == "deltaFar"] <- "FaroeIslands"
names(getalldeathsNEW)[names(getalldeathsNEW) == "deltaGrl"] <- "Greenland"
names(getalldeathsNEW)[names(getalldeathsNEW) == "deltaIre"] <- "Ireland"
names(getalldeathsNEW)[names(getalldeathsNEW) == "deltaIsl"] <- "Iceland"
names(getalldeathsNEW)[names(getalldeathsNEW) == "deltaFin"] <- "Finland"
names(getalldeathsNEW)[names(getalldeathsNEW) == "deltaSwe"] <- "Sweden"
names(getalldeathsNEW)[names(getalldeathsNEW) == "deltaNor"] <- "Norway"
#now deal with dropping unnecess NI stuff
getalldeathsNEW = subset(getalldeathsNEW, select = -c(country,newcases,cumcases,cumdeaths)) 
names(getalldeathsNEW)[names(getalldeathsNEW) == "newdeaths"] <- "NorthernIreland"
getalldeathsNEW <- left_join(getalldeathsNEW, SCOdata, by=c("cDate"))
#drop country, newcases, newdeaths, cumdeaths (i.e. leave cumcases)
getalldeathsNEW = subset(getalldeathsNEW, select = -c(country,newcases,cumcases,cumdeaths)) 
names(getalldeathsNEW)[names(getalldeathsNEW) == "newdeaths"] <- "Scotland"
getalldeathsNEW$NorthernIreland[is.na(getalldeathsNEW$NorthernIreland)] <- 0
getalldeathsNEW$Scotland[is.na(getalldeathsNEW$Scotland)] <- 0

# convert to per 100k popn
allcaseswgt <- getallcasesTOT
allcaseswgt[,2] <- 100000*allcaseswgt[,2]/popns[1]
allcaseswgt[,3] <- 100000*allcaseswgt[,3]/popns[2]
allcaseswgt[,4] <- 100000*allcaseswgt[,4]/popns[3]
allcaseswgt[,5] <- 100000*allcaseswgt[,5]/popns[4]
allcaseswgt[,6] <- 100000*allcaseswgt[,6]/popns[5]
allcaseswgt[,7] <- 100000*allcaseswgt[,7]/popns[6]
allcaseswgt[,8] <- 100000*allcaseswgt[,8]/popns[7]
allcaseswgt[,9] <- 100000*allcaseswgt[,9]/popns[8]
allcaseswgt[,10] <- 100000*allcaseswgt[,10]/popns[9]

newcaseswgt<- getallcasesNEW
newcaseswgt[,2] <- 100000*newcaseswgt[,2]/popns[1]
newcaseswgt[,3] <- 100000*newcaseswgt[,3]/popns[2]
newcaseswgt[,4] <- 100000*newcaseswgt[,4]/popns[3]
newcaseswgt[,5] <- 100000*newcaseswgt[,5]/popns[4]
newcaseswgt[,6] <- 100000*newcaseswgt[,6]/popns[5]
newcaseswgt[,7] <- 100000*newcaseswgt[,7]/popns[6]
newcaseswgt[,8] <- 100000*newcaseswgt[,8]/popns[7]
newcaseswgt[,9] <- 100000*newcaseswgt[,9]/popns[8]
newcaseswgt[,10] <- 100000*newcaseswgt[,10]/popns[9]

alldeathswgt <- getalldeathsTOT
alldeathswgt[,2] <- 100000*alldeathswgt[,2]/popns[1]
alldeathswgt[,3] <- 100000*alldeathswgt[,3]/popns[2]
alldeathswgt[,4] <- 100000*alldeathswgt[,4]/popns[3]
alldeathswgt[,5] <- 100000*alldeathswgt[,5]/popns[4]
alldeathswgt[,6] <- 100000*alldeathswgt[,6]/popns[5]
alldeathswgt[,7] <- 100000*alldeathswgt[,7]/popns[6]
alldeathswgt[,8] <- 100000*alldeathswgt[,8]/popns[7]
alldeathswgt[,9] <- 100000*alldeathswgt[,9]/popns[8]
alldeathswgt[,10] <- 100000*alldeathswgt[,10]/popns[9]

newdeathswgt <- getalldeathsNEW
newdeathswgt[,2] <- 100000*newdeathswgt[,2]/popns[1]
newdeathswgt[,3] <- 100000*newdeathswgt[,3]/popns[2]
newdeathswgt[,4] <- 100000*newdeathswgt[,4]/popns[3]
newdeathswgt[,5] <- 100000*newdeathswgt[,5]/popns[4]
newdeathswgt[,6] <- 100000*newdeathswgt[,6]/popns[5]
newdeathswgt[,7] <- 100000*newdeathswgt[,7]/popns[6]
newdeathswgt[,8] <- 100000*newdeathswgt[,8]/popns[7]
newdeathswgt[,9] <- 100000*newdeathswgt[,9]/popns[8]
newdeathswgt[,10] <- 100000*newdeathswgt[,10]/popns[9]

dataset1 <- allcaseswgt
dataset2 <- alldeathswgt

getallcasesTOT$Mth= format(as.Date(getallcasesTOT$cDate), "%m-%y")
getallcasesTOT$week= format(as.Date(getallcasesTOT$cDate), "%U-%y")

#now get delta values for cases (by month)
deltacases<-getallcasesTOT %>% 
  group_by(Mth) %>% 
  summarise(FaroeIslandsCmth=max(FaroeIslands),GreenlandCmth=max(Greenland),
            FinlandCmth=max(Finland),IcelandCmth=max(Iceland),IrelandCmth=max(Ireland),
            NorwayCmth=max(Norway),SwedenCmth=max(Sweden),NIrelandCmth=max(NorthernIreland),ScotlandCmth=max(Scotland), cDateCmth=max(cDate))
deltacases<-arrange(deltacases,cDateCmth)
# calculate monthly figures
deltacases<- deltacases %>%
  mutate(deltaFaroeIslandsC = FaroeIslandsCmth - lag(FaroeIslandsCmth, default = first(FaroeIslandsCmth)))%>%
  mutate(deltaGreenlandC = GreenlandCmth - lag(GreenlandCmth, default = first(GreenlandCmth)))%>%
  mutate(deltaFinlandC = FinlandCmth - lag(FinlandCmth, default = first(FinlandCmth)))%>%
  mutate(deltaIcelandC = IcelandCmth - lag(IcelandCmth, default = first(IcelandCmth)))%>%
  mutate(deltaIrelandC = IrelandCmth - lag(IrelandCmth, default = first(IrelandCmth)))%>%
  mutate(deltaNorwayC = NorwayCmth - lag(NorwayCmth, default = first(NorwayCmth)))%>%
  mutate(deltaSwedenC = SwedenCmth - lag(SwedenCmth, default = first(SwedenCmth)))%>%
  mutate(deltaNIrelandC = NIrelandCmth - lag(NIrelandCmth, default = first(NIrelandCmth)))%>%
  mutate(deltaScotlandC = ScotlandCmth - lag(ScotlandCmth, default = first(ScotlandCmth)))

# convert to per 100k popn
deltacaseswgt <- deltacases
deltacaseswgt[,12] <- 100000*deltacaseswgt[,12]/popns[1]
deltacaseswgt[,13] <- 100000*deltacaseswgt[,13]/popns[2]
deltacaseswgt[,14] <- 100000*deltacaseswgt[,14]/popns[3]
deltacaseswgt[,15] <- 100000*deltacaseswgt[,15]/popns[4]
deltacaseswgt[,16] <- 100000*deltacaseswgt[,16]/popns[5]
deltacaseswgt[,17] <- 100000*deltacaseswgt[,17]/popns[6]
deltacaseswgt[,18] <- 100000*deltacaseswgt[,18]/popns[7]
deltacaseswgt[,19] <- 100000*deltacaseswgt[,19]/popns[8]
deltacaseswgt[,20] <- 100000*deltacaseswgt[,20]/popns[9]

newcasesbymth<-deltacaseswgt[,c(1,11:20)]
names(newcasesbymth)[names(newcasesbymth) == "deltaFaroeIslandsC"] <- "FaroeIslands"
names(newcasesbymth)[names(newcasesbymth) == "deltaGreenlandC"] <- "Greenland"
names(newcasesbymth)[names(newcasesbymth) == "deltaIrelandC"] <- "Ireland"
names(newcasesbymth)[names(newcasesbymth) == "deltaIcelandC"] <- "Iceland"
names(newcasesbymth)[names(newcasesbymth) == "deltaFinlandC"] <- "Finland"
names(newcasesbymth)[names(newcasesbymth) == "deltaSwedenC"] <- "Sweden"
names(newcasesbymth)[names(newcasesbymth) == "deltaNorwayC"] <- "Norway"
names(newcasesbymth)[names(newcasesbymth) == "deltaNIrelandC"] <- "NorthernIreland"
names(newcasesbymth)[names(newcasesbymth) == "deltaScotlandC"] <- "Scotland"


# 10/1/21 getting flexiCs & flexiDs sorted for graphs
#note no deaths in Greenland as of 10/1/21 - gives an error when deleting 1st & 2nd Jan as not clear that its numeric
getalldeathsTOT$Greenland=as.numeric(getalldeathsTOT$Greenland)

flexiCs<- getallcasesTOT %>% 
  mutate(FaroeIslandsC = FaroeIslands - lag(FaroeIslands, default = first(FaroeIslands)))%>%
  mutate(GreenlandC = Greenland - lag(Greenland, default = first(Greenland)))%>%
  mutate(FinlandC = Finland - lag(Finland, default = first(Finland)))%>%
  mutate(IcelandC = Iceland - lag(Iceland, default = first(Iceland)))%>%
  mutate(IrelandC = Ireland - lag(Ireland, default = first(Ireland)))%>%
  mutate(NorwayC = Norway - lag(Norway, default = first(Norway)))%>%
  mutate(SwedenC = Sweden - lag(Sweden, default = first(Sweden)))%>%
  mutate(NorthernIrelandC = NorthernIreland - lag(NorthernIreland, default = first(NorthernIreland)))%>%
  mutate(ScotlandC = Scotland - lag(Scotland, default = first(Scotland)))

#dropcumulativecolumns
flexiCs = subset(flexiCs, select = -c(FaroeIslands,Greenland,Finland,Iceland,Ireland,Norway,Sweden,NorthernIreland,Scotland)) 
#rename to work with ctries input/user selections
names(flexiCs)[names(flexiCs) == "FaroeIslandsC"] <- "FaroeIslands"
names(flexiCs)[names(flexiCs) == "GreenlandC"] <- "Greenland"
names(flexiCs)[names(flexiCs) == "IrelandC"] <- "Ireland"
names(flexiCs)[names(flexiCs) == "IcelandC"] <- "Iceland"
names(flexiCs)[names(flexiCs) == "FinlandC"] <- "Finland"
names(flexiCs)[names(flexiCs) == "SwedenC"] <- "Sweden"
names(flexiCs)[names(flexiCs) == "NorwayC"] <- "Norway"
names(flexiCs)[names(flexiCs) == "NorthernIrelandC"] <- "NorthernIreland"
names(flexiCs)[names(flexiCs) == "ScotlandC"] <- "Scotland"
flexiCs$cDate<-as.Date(flexiCs$cDate)
#view(flexiCs)

flexiDs<- getalldeathsTOT %>% 
  mutate(FaroeIslandsD = FaroeIslands - lag(FaroeIslands, default = first(FaroeIslands)))%>%
  mutate(GreenlandD = Greenland - lag(Greenland, default = first(Greenland)))%>%
  mutate(FinlandD = Finland - lag(Finland, default = first(Finland)))%>%
  mutate(IcelandD = Iceland - lag(Iceland, default = first(Iceland)))%>%
  mutate(IrelandD = Ireland - lag(Ireland, default = first(Ireland)))%>%
  mutate(NorwayD = Norway - lag(Norway, default = first(Norway)))%>%
  mutate(SwedenD = Sweden - lag(Sweden, default = first(Sweden)))%>%
  mutate(NorthernIrelandD = NorthernIreland - lag(NorthernIreland, default = first(NorthernIreland)))%>%
  mutate(ScotlandD = Scotland - lag(Scotland, default = first(Scotland)))

#dropcumulativecolumns
flexiDs = subset(flexiDs, select = -c(FaroeIslands,Greenland,Finland,Iceland,Ireland,Norway,Sweden,NorthernIreland,Scotland)) 
#rename to work with ctries input/user selections
names(flexiDs)[names(flexiDs) == "FaroeIslandsD"] <- "FaroeIslands"
names(flexiDs)[names(flexiDs) == "GreenlandD"] <- "Greenland"
names(flexiDs)[names(flexiDs) == "FinlandD"] <- "Finland"
names(flexiDs)[names(flexiDs) == "IcelandD"] <- "Iceland"
names(flexiDs)[names(flexiDs) == "IrelandD"] <- "Ireland"
names(flexiDs)[names(flexiDs) == "NorwayD"] <- "Norway"
names(flexiDs)[names(flexiDs) == "SwedenD"] <- "Sweden"
names(flexiDs)[names(flexiDs) == "NorthernIrelandD"] <- "NorthernIreland"
names(flexiDs)[names(flexiDs) == "ScotlandD"] <- "Scotland"
flexiDs$cDate<-as.Date(flexiDs$cDate)

#1st and 2nd Jan are denoted as week 0 of 2021 - just add their values to 3rd Jan (row # 348) for simplicity
flexiCs[c(348),c('FaroeIslands','Greenland','Finland','Iceland','Ireland','Norway','Sweden','NorthernIreland','Scotland')] <- flexiCs[c(348),c('FaroeIslands','Greenland','Finland','Iceland','Ireland','Norway','Sweden','NorthernIreland','Scotland')] + flexiCs[c(347),c('FaroeIslands','Greenland','Finland','Iceland','Ireland','Norway','Sweden','NorthernIreland','Scotland')] 
flexiCs[c(348),c('FaroeIslands','Greenland','Finland','Iceland','Ireland','Norway','Sweden','NorthernIreland','Scotland')] <- flexiCs[c(348),c('FaroeIslands','Greenland','Finland','Iceland','Ireland','Norway','Sweden','NorthernIreland','Scotland')] + flexiCs[c(346),c('FaroeIslands','Greenland','Finland','Iceland','Ireland','Norway','Sweden','NorthernIreland','Scotland')] 
flexiCs<-subset(flexiCs, flexiCs$cDate != '2021-01-01')
flexiCs<-subset(flexiCs, flexiCs$cDate != '2021-01-02')
flexiDs[c(348),c('FaroeIslands','Greenland','Finland','Iceland','Ireland','Norway','Sweden','NorthernIreland','Scotland')] <- flexiDs[c(348),c('FaroeIslands','Greenland','Finland','Iceland','Ireland','Norway','Sweden','NorthernIreland','Scotland')] + flexiDs[c(347),c('FaroeIslands','Greenland','Finland','Iceland','Ireland','Norway','Sweden','NorthernIreland','Scotland')] 
flexiDs[c(348),c('FaroeIslands','Greenland','Finland','Iceland','Ireland','Norway','Sweden','NorthernIreland','Scotland')] <- flexiDs[c(348),c('FaroeIslands','Greenland','Finland','Iceland','Ireland','Norway','Sweden','NorthernIreland','Scotland')] + flexiDs[c(346),c('FaroeIslands','Greenland','Finland','Iceland','Ireland','Norway','Sweden','NorthernIreland','Scotland')] 
flexiDs<-subset(flexiDs, flexiDs$cDate != '2021-01-01')
flexiDs<-subset(flexiDs, flexiDs$cDate != '2021-01-02')

#now get delta values for Deaths (by month)
getalldeathsTOT$Mth= format(as.Date(getalldeathsTOT$cDate), "%m-%y")
getalldeathsTOT$week= format(as.Date(getalldeathsTOT$cDate), "%U-%y")

deltadeaths<-getalldeathsTOT %>% 
  group_by(Mth) %>% 
  summarise(FaroeIslandsDmth=max(FaroeIslands),GreenlandDmth=max(Greenland),
            FinlandDmth=max(Finland),IcelandDmth=max(Iceland),IrelandDmth=max(Ireland),
            NorwayDmth=max(Norway),SwedenDmth=max(Sweden),NIrelandDmth=max(NorthernIreland),ScotlandDmth=max(Scotland),cDateDmth=max(cDate))

deltadeaths<-arrange(deltadeaths,cDateDmth)

deltadeaths<- deltadeaths %>%
  mutate(deltaFaroeIslandsD = FaroeIslandsDmth - lag(FaroeIslandsDmth, default = first(FaroeIslandsDmth)))%>%
  mutate(deltaGreenlandD = GreenlandDmth - lag(GreenlandDmth, default = first(GreenlandDmth)))%>%
  mutate(deltaFinlandD = FinlandDmth - lag(FinlandDmth, default = first(FinlandDmth)))%>%
  mutate(deltaIcelandD = IcelandDmth - lag(IcelandDmth, default = first(IcelandDmth)))%>%
  mutate(deltaIrelandD = IrelandDmth - lag(IrelandDmth, default = first(IrelandDmth)))%>%
  mutate(deltaNorwayD = NorwayDmth - lag(NorwayDmth, default = first(NorwayDmth)))%>%
  mutate(deltaSwedenD = SwedenDmth - lag(SwedenDmth, default = first(SwedenDmth)))%>%
  mutate(deltaNIrelandD = NIrelandDmth - lag(NIrelandDmth, default = first(NIrelandDmth)))%>%
  mutate(deltaScotlandD = ScotlandDmth - lag(ScotlandDmth, default = first(ScotlandDmth)))

deltadeathswgt <- deltadeaths

#Convert to per 100k population
deltadeathswgt[,12] <- 100000*deltadeathswgt[,12]/popns[1]
deltadeathswgt[,13] <- 100000*deltadeathswgt[,13]/popns[2]
deltadeathswgt[,14] <- 100000*deltadeathswgt[,14]/popns[3]
deltadeathswgt[,15] <- 100000*deltadeathswgt[,15]/popns[4]
deltadeathswgt[,16] <- 100000*deltadeathswgt[,16]/popns[5]
deltadeathswgt[,17] <- 100000*deltadeathswgt[,17]/popns[6]
deltadeathswgt[,18] <- 100000*deltadeathswgt[,18]/popns[7]
deltadeathswgt[,19] <- 100000*deltadeathswgt[,19]/popns[8]
deltadeathswgt[,20] <- 100000*deltadeathswgt[,20]/popns[9]

newdeathsbymth<-deltadeathswgt[,c(1,11:20)]
names(newdeathsbymth)[names(newdeathsbymth) == "deltaFaroeIslandsD"] <- "FaroeIslands"
names(newdeathsbymth)[names(newdeathsbymth) == "deltaGreenlandD"] <- "Greenland"
names(newdeathsbymth)[names(newdeathsbymth) == "deltaIrelandD"] <- "Ireland"
names(newdeathsbymth)[names(newdeathsbymth) == "deltaIcelandD"] <- "Iceland"
names(newdeathsbymth)[names(newdeathsbymth) == "deltaFinlandD"] <- "Finland"
names(newdeathsbymth)[names(newdeathsbymth) == "deltaSwedenD"] <- "Sweden"
names(newdeathsbymth)[names(newdeathsbymth) == "deltaNorwayD"] <- "Norway"
names(newdeathsbymth)[names(newdeathsbymth) == "deltaNIrelandD"] <- "NorthernIreland"
names(newdeathsbymth)[names(newdeathsbymth) == "deltaScotlandD"] <- "Scotland"

dataset3 <- newcasesbymth
dataset4 <- newdeathsbymth
dataset6<- newcaseswgt
dataset7<- newdeathswgt

###!!! Section 17/12/20 GET WEEKLY Cases & Deaths
#!!! FIRST CASES

#now get delta values for cases (by week)
deltacasesWK<-getallcasesTOT %>% 
  group_by(week) %>% 
  summarise(FaroeIslandsCmth=max(FaroeIslands),GreenlandCmth=max(Greenland),
            FinlandCmth=max(Finland),IcelandCmth=max(Iceland),IrelandCmth=max(Ireland),
            NorwayCmth=max(Norway),SwedenCmth=max(Sweden),NIrelandCmth=max(NorthernIreland),ScotlandCmth=max(Scotland),cDateCWK=max(cDate))

deltacasesWK<-arrange(deltacasesWK,cDateCWK)
deltacasesWK<- deltacasesWK %>%
  mutate(deltaFaroeIslandsC = FaroeIslandsCmth - lag(FaroeIslandsCmth, default = first(FaroeIslandsCmth)))%>%
  mutate(deltaGreenlandC = GreenlandCmth - lag(GreenlandCmth, default = first(GreenlandCmth)))%>%
  mutate(deltaFinlandC = FinlandCmth - lag(FinlandCmth, default = first(FinlandCmth)))%>%
  mutate(deltaIcelandC = IcelandCmth - lag(IcelandCmth, default = first(IcelandCmth)))%>%
  mutate(deltaIrelandC = IrelandCmth - lag(IrelandCmth, default = first(IrelandCmth)))%>%
  mutate(deltaNorwayC = NorwayCmth - lag(NorwayCmth, default = first(NorwayCmth)))%>%
  mutate(deltaSwedenC = SwedenCmth - lag(SwedenCmth, default = first(SwedenCmth)))%>%
  mutate(deltaNIrelandC = NIrelandCmth - lag(NIrelandCmth, default = first(NIrelandCmth)))%>%
  mutate(deltaScotlandC = ScotlandCmth - lag(ScotlandCmth, default = first(ScotlandCmth)))

#convert to per 100k population
deltacaseswgtWK <- deltacasesWK
deltacaseswgtWK[,12] <- 100000*deltacaseswgtWK[,12]/popns[1]
deltacaseswgtWK[,13] <- 100000*deltacaseswgtWK[,13]/popns[2]
deltacaseswgtWK[,14] <- 100000*deltacaseswgtWK[,14]/popns[3]
deltacaseswgtWK[,15] <- 100000*deltacaseswgtWK[,15]/popns[4]
deltacaseswgtWK[,16] <- 100000*deltacaseswgtWK[,16]/popns[5]
deltacaseswgtWK[,17] <- 100000*deltacaseswgtWK[,17]/popns[6]
deltacaseswgtWK[,18] <- 100000*deltacaseswgtWK[,18]/popns[7]
deltacaseswgtWK[,19] <- 100000*deltacaseswgtWK[,19]/popns[8]
deltacaseswgtWK[,20] <- 100000*deltacaseswgtWK[,20]/popns[9]

newcasesbyWK<-deltacaseswgtWK[,c(1,11:20)]
names(newcasesbyWK)[names(newcasesbyWK) == "deltaFaroeIslandsC"] <- "FaroeIslands"
names(newcasesbyWK)[names(newcasesbyWK) == "deltaGreenlandC"] <- "Greenland"
names(newcasesbyWK)[names(newcasesbyWK) == "deltaIrelandC"] <- "Ireland"
names(newcasesbyWK)[names(newcasesbyWK) == "deltaIcelandC"] <- "Iceland"
names(newcasesbyWK)[names(newcasesbyWK) == "deltaFinlandC"] <- "Finland"
names(newcasesbyWK)[names(newcasesbyWK) == "deltaSwedenC"] <- "Sweden"
names(newcasesbyWK)[names(newcasesbyWK) == "deltaNorwayC"] <- "Norway"
names(newcasesbyWK)[names(newcasesbyWK) == "deltaNIrelandC"] <- "NorthernIreland"
names(newcasesbyWK)[names(newcasesbyWK) == "deltaScotlandC"] <- "Scotland"

#!!! THEN DEATHS
deltadeathsWK<-getalldeathsTOT %>% 
  group_by(week) %>% 
  summarise(FaroeIslandsDmth=max(FaroeIslands),GreenlandDmth=max(Greenland),
            FinlandDmth=max(Finland),IcelandDmth=max(Iceland),IrelandDmth=max(Ireland),
            NorwayDmth=max(Norway),SwedenDmth=max(Sweden),NIrelandDmth=max(NorthernIreland),ScotlandDmth=max(Scotland),cDateDWK=max(cDate))

deltadeathsWK<-arrange(deltadeathsWK,cDateDWK)

deltadeathsWK<- deltadeathsWK %>%
  mutate(deltaFaroeIslandsD = FaroeIslandsDmth - lag(FaroeIslandsDmth, default = first(FaroeIslandsDmth)))%>%
  mutate(deltaGreenlandD = GreenlandDmth - lag(GreenlandDmth, default = first(GreenlandDmth)))%>%
  mutate(deltaFinlandD = FinlandDmth - lag(FinlandDmth, default = first(FinlandDmth)))%>%
  mutate(deltaIcelandD = IcelandDmth - lag(IcelandDmth, default = first(IcelandDmth)))%>%
  mutate(deltaIrelandD = IrelandDmth - lag(IrelandDmth, default = first(IrelandDmth)))%>%
  mutate(deltaNorwayD = NorwayDmth - lag(NorwayDmth, default = first(NorwayDmth)))%>%
  mutate(deltaSwedenD = SwedenDmth - lag(SwedenDmth, default = first(SwedenDmth)))%>%
  mutate(deltaNIrelandD = NIrelandDmth - lag(NIrelandDmth, default = first(NIrelandDmth)))%>%
  mutate(deltaScotlandD = ScotlandDmth - lag(ScotlandDmth, default = first(ScotlandDmth)))

#convert to per 100k popn
deltadeathswgtWK <- deltadeathsWK
deltadeathswgtWK[,12] <- 100000*deltadeathswgtWK[,12]/popns[1]
deltadeathswgtWK[,13] <- 100000*deltadeathswgtWK[,13]/popns[2]
deltadeathswgtWK[,14] <- 100000*deltadeathswgtWK[,14]/popns[3]
deltadeathswgtWK[,15] <- 100000*deltadeathswgtWK[,15]/popns[4]
deltadeathswgtWK[,16] <- 100000*deltadeathswgtWK[,16]/popns[5]
deltadeathswgtWK[,17] <- 100000*deltadeathswgtWK[,17]/popns[6]
deltadeathswgtWK[,18] <- 100000*deltadeathswgtWK[,18]/popns[7]
deltadeathswgtWK[,19] <- 100000*deltadeathswgtWK[,19]/popns[8]
deltadeathswgtWK[,20] <- 100000*deltadeathswgtWK[,20]/popns[9]

newdeathsbyWK<-deltadeathswgtWK[,c(1,11:20)]
names(newdeathsbyWK)[names(newdeathsbyWK) == "deltaFaroeIslandsD"] <- "FaroeIslands"
names(newdeathsbyWK)[names(newdeathsbyWK) == "deltaGreenlandD"] <- "Greenland"
names(newdeathsbyWK)[names(newdeathsbyWK) == "deltaIrelandD"] <- "Ireland"
names(newdeathsbyWK)[names(newdeathsbyWK) == "deltaIcelandD"] <- "Iceland"
names(newdeathsbyWK)[names(newdeathsbyWK) == "deltaFinlandD"] <- "Finland"
names(newdeathsbyWK)[names(newdeathsbyWK) == "deltaSwedenD"] <- "Sweden"
names(newdeathsbyWK)[names(newdeathsbyWK) == "deltaNorwayD"] <- "Norway"
names(newdeathsbyWK)[names(newdeathsbyWK) == "deltaNIrelandD"] <- "NorthernIreland"
names(newdeathsbyWK)[names(newdeathsbyWK) == "deltaScotlandD"] <- "Scotland"

dataset8<- newcasesbyWK
dataset9<- newdeathsbyWK

#cumulative cfr
cumcfr.df <- merge(deltacaseswgt, deltadeathswgt, by.x="Mth", by.y="Mth", sort=FALSE)
#view(cumcfr.df)

cumcfr.df$FaroeIslands <- 100*cumcfr.df$FaroeIslandsDmth/cumcfr.df$FaroeIslandsCmth
cumcfr.df$Greenland <- 100*cumcfr.df$GreenlandDmth/cumcfr.df$GreenlandCmth
cumcfr.df$Finland <- 100*cumcfr.df$FinlandDmth/cumcfr.df$FinlandCmth
cumcfr.df$Iceland <- 100*cumcfr.df$IcelandDmth/cumcfr.df$IcelandCmth
cumcfr.df$Ireland <- 100*cumcfr.df$IrelandDmth/cumcfr.df$IrelandCmth
cumcfr.df$Norway <- 100*cumcfr.df$NorwayDmth/cumcfr.df$NorwayCmth
cumcfr.df$Sweden <- 100*cumcfr.df$SwedenDmth/cumcfr.df$SwedenCmth
cumcfr.df$NorthernIreland <- 100*cumcfr.df$NIrelandDmth/cumcfr.df$NIrelandCmth
cumcfr.df$Scotland <- 100*cumcfr.df$ScotlandDmth/cumcfr.df$ScotlandCmth
cumcfr.df$Month = month.abb[cumcfr.df$Mth]


# discrete monthly changing CFR
cfr.df <- merge(deltacases, deltadeaths, by.x="Mth", by.y="Mth", sort=FALSE)

cfr.df$FaroeIslands <- 100*cfr.df$deltaFaroeIslandsD/cfr.df$deltaFaroeIslandsC
cfr.df$Greenland <- 100*cfr.df$deltaGreenlandD/cfr.df$deltaGreenlandC
cfr.df$Finland <- 100*cfr.df$deltaFinlandD/cfr.df$deltaFinlandC
cfr.df$Iceland <- 100*cfr.df$deltaIcelandD/cfr.df$deltaIcelandC
cfr.df$Ireland <- 100*cfr.df$deltaIrelandD/cfr.df$deltaIrelandC
cfr.df$Norway <- 100*cfr.df$deltaNorwayD/cfr.df$deltaNorwayC
cfr.df$Sweden <- 100*cfr.df$deltaSwedenD/cfr.df$deltaSwedenC
cfr.df$NorthernIreland <- 100*cfr.df$deltaNIrelandD/cfr.df$deltaNIrelandC
cfr.df$Scotland <- 100*cfr.df$deltaScotlandD/cfr.df$deltaScotlandC
cfr.df$Month = month.abb[cfr.df$Mth]

colourlist<-c("brown3","aquamarine3","brown1","cyan4","cornflowerblue","darkolivegreen3","bisque3","darkgoldenrod1")
colourlisted<-c("FaroeIslands"="aquamarine3","Greenland"="brown1","Finland"="cyan4","Iceland"="skyblue","Ireland"="darkolivegreen3","Norway"="bisque3","Sweden"="darkgoldenrod1","NorthernIreland"="darkorchid","Scotland"="royalblue")


#Now onto TESTING Data

file_URL <- "https://opendata.ecdc.europa.eu/covid19/testing/csv/"
curl_download(file_URL, tmp)
mostcovtests<- read.csv(tmp)

covtests<-mostcovtests %>% filter(country_code =="FI" 
                                  | country_code =="IS"  | country_code =="IE" 
                                  | country_code =="NO" | country_code =="SE")

# new code June 2024
file_URL <- "https://raw.githubusercontent.com/mikeocLimerick/COVIDWATCH-NPA/aceb97f1337372e7b3e43cfee8350e3f9fa11bd2/UKtesting"
ukcovtests <- read_delim(file_URL, delim = "\t", col_names = TRUE)
#UKNIdata$cDate<-as.Date(UKNIdata$cDate,"%d/%m/%Y")

#ukcovtests<-read.csv("data/uktests3.csv") 
#note there's an issue that 1st column doesn't match ( a text character problem )

colnames(ukcovtests)[1] <- "CTRY"
colnames(covtests)[1]<-"CTRY"
covtests<-rbind(covtests,ukcovtests)
covtests<-covtests %>% filter(level=="national")
covtests$yearnum<-substr(covtests$year_week,1,4)
covtests$weeknum<-substr(covtests$year_week,7,length(covtests$year_week))
covtests$endweekdate<-as.Date(paste(covtests$yearnum, covtests$weeknum, 1, sep="-"), "%Y-%U-%u")
covtests$totaltestsdone <- ave(covtests$tests_done,covtests$country_code,FUN=cumsum)
covtests$ttdpp<- 100000*covtests$totaltestsdone/covtests$population
names(covtests)[1] <- "country"

covtestsslim= subset(covtests, select = c(endweekdate,year_week,country,ttdpp,testing_rate,tests_done,positivity_rate))

justoursfin <- covtestsslim %>% filter(country=="Finland")
justoursice <- covtestsslim %>% filter(country=="Iceland")
justoursire <- covtestsslim %>% filter(country=="Ireland")
justoursnor <- covtestsslim %>% filter(country=="Norway")
justoursswe <- covtestsslim %>% filter(country=="Sweden")
justoursNI <- covtestsslim %>% filter(country=="NorthernIreland")
justoursSC <- covtestsslim %>% filter(country=="Scotland")

fullours<- full_join(justoursfin, justoursice, by = "year_week")
names(fullours)[names(fullours) == "ttdpp.x"] <- "Finland4"
names(fullours)[names(fullours) == "testing_rate.x"] <- "Finland2"
names(fullours)[names(fullours) == "tests_done.x"] <- "Finland3"
names(fullours)[names(fullours) == "positivity_rate.x"] <- "Finland"

names(fullours)[names(fullours) == "ttdpp.y"] <- "Iceland4"
names(fullours)[names(fullours) == "testing_rate.y"] <- "Iceland2"
names(fullours)[names(fullours) == "tests_done.y"] <- "Iceland3"
names(fullours)[names(fullours) == "positivity_rate.y"] <- "Iceland"

fullours<-full_join(fullours, justoursire, by = "year_week")
names(fullours)[names(fullours) == "ttdpp"] <- "Ireland4"
names(fullours)[names(fullours) == "testing_rate"] <- "Ireland2"
names(fullours)[names(fullours) == "tests_done"] <- "Ireland3"
names(fullours)[names(fullours) == "positivity_rate"] <- "Ireland"

fullours<-full_join(fullours, justoursnor, by = "year_week")
names(fullours)[names(fullours) == "ttdpp"] <- "Norway4"
names(fullours)[names(fullours) == "testing_rate"] <- "Norway2"
names(fullours)[names(fullours) == "tests_done"] <- "Norway3"
names(fullours)[names(fullours) == "positivity_rate"] <- "Norway"

fullours<-full_join(fullours, justoursswe, by = "year_week")
names(fullours)[names(fullours) == "ttdpp"] <- "Sweden4"
names(fullours)[names(fullours) == "testing_rate"] <- "Sweden2"
names(fullours)[names(fullours) == "tests_done"] <- "Sweden3"
names(fullours)[names(fullours) == "positivity_rate"] <- "Sweden"

fullours<-full_join(fullours, justoursNI, by = "year_week")
names(fullours)[names(fullours) == "ttdpp"] <- "NorthernIreland4"
names(fullours)[names(fullours) == "testing_rate"] <- "NorthernIreland2"
names(fullours)[names(fullours) == "tests_done"] <- "NorthernIreland3"
names(fullours)[names(fullours) == "positivity_rate"] <- "NorthernIreland"

fullours<-full_join(fullours, justoursSC, by = "year_week")
names(fullours)[names(fullours) == "ttdpp"] <- "Scotland4"
names(fullours)[names(fullours) == "testing_rate"] <- "Scotland2"
names(fullours)[names(fullours) == "tests_done"] <- "Scotland3"
names(fullours)[names(fullours) == "positivity_rate"] <- "Scotland"

################### Add in Faroes testing
#faroescovtests <- read_excel("faroestesting.xls", sheet = "faroestestingWK")

# new code June 2024
file_URL <- "https://raw.githubusercontent.com/mikeocLimerick/COVIDWATCH-NPA/7911556ed3c469a83051b6016571d12cba7060d4/faroestesting"
faroescovtests <- read_delim(file_URL, delim = "\t", col_names = TRUE)

faroescovtests$numtestsrate <- 100000*faroescovtests$numTests/popns[1]
faroescovtests$testDate = seq(as.Date("2020-03-03"),by = "days", length.out = nrow(faroescovtests))
faroescovtests$weeklies<-format(as.Date(faroescovtests$testDate), "%U-%y")
faroescovtests$weekliestests<-ave(faroescovtests$numTests,faroescovtests$weeklies,FUN=cumsum)
faroescovtests$year_week<-paste0(year(faroescovtests$testDate),"-W",substr(faroescovtests$weeklies,1,2))
faroescovtests$Mth = format(as.Date(faroescovtests$testDate), "%m-%y")

weeklyfaroes<-faroescovtests %>% 
  group_by(year_week) %>% 
  summarise(numtests=sum(numTests),numcases=sum(numcases),cumultests=max(cumnumTests),positivity_rate=100*sum(numcases)/sum(numTests))
weeklyfaroes$cumultestsrate <- 100000*weeklyfaroes$cumultests/popns[1]
weeklyfaroes$numtestsrate <- 100000*weeklyfaroes$numtests/popns[1]

#convert 2021-00 to 2020-53 to match other countries csv data on Testing
weeklyfaroes[weeklyfaroes$year_week=="2021-W00", "year_week"] <- "2020-W53"

#drop numcases numTests
weeklyfaroes = subset(weeklyfaroes, select = -c(cumultests,numcases))

newfullours<-merge(fullours,weeklyfaroes,by=c('year_week'),all.x=T)
names(newfullours)[names(newfullours) == "cumultestsrate"] <- "FaroeIslands4"
names(newfullours)[names(newfullours) == "numtestsrate"] <- "FaroeIslands2"
names(newfullours)[names(newfullours) == "numtests"] <- "FaroeIslands3"
names(newfullours)[names(newfullours) == "positivity_rate"] <- "FaroeIslands"
newfullours<-arrange(newfullours,year_week)

newfullours$tDate = seq(as.Date("2020-01-01"),by = "weeks", length.out = nrow(newfullours))
dataset5<- newfullours

##### NOW ONTO R SHINY WEBPAGE CODE

cat("uiStub application started...\n")

ui <- uiOutput("uiStub")                                # single-output stub ui

server <- function(input, output, session) {
  
  ### 15/10/20 Mikes Reactive Dataset
  
  cat("Session started.\n")                               # this prints when a session starts
  onSessionEnded(function() {cat("Session ended.\n\n")})  # this prints when a session ends
  
  # build menu; same on all pages
  output$uiStub <- renderUI(tagList(             # a single-output stub ui basically lets you
    fluidPage( 
 #     tags$head(includeHTML("google-analytics.html")),
      img(src='npalogo.png', width="325px", height="100px", align = "left"), 
      img(src='covidwatch.png', width="375px", height="100px", align = "left"), #width="650px", height="200px",
      #     move the ui into the server function
      fluidRow(
        column(12,
               HTML("<h3><a href='?home'>Home</a> || ",
                    "<a href='?countries'>Countries </a> ||",
                    "<a href='?covidtrends'> Cases+Deaths </a> ||",
                    "<a href='?excess'> Excess Mortality </a> ||",
                    "<a href='?CFP'> Fatality % </a> ||",
                    "<a href='?testing'> Testing </a> ||",
                    "<a href='?percpos'> % Positivity </a> ||",
                    "<a href='?regional'> Regional </a>",
                    "</h3>")
               #"<a href='?page3'>Nothing</a> |",
        )
      ),
      uiOutput("pageStub")                     # loaded server code should render the
    )                                           #    rest of the page to this output$
  ))
  
  # load server code for page specified in URL
  validFiles = c("home.R",                             # valid files must be hardcoded here
                 "countries.R","covidtrends.R","excess.R","CFP.R","testing.R","percpos.R","regional.R")    #    for security (use all lower-case
  #    names to prevent Unix case problems)
  fname = isolate(session$clientData$url_search)       # isolate() deals with reactive context
  if(nchar(fname)==0) { fname = "?home" }              # blank means home page
  fname = paste0(substr(fname, 2, nchar(fname)), ".R") # remove leading "?", add ".R"
  
  cat(paste0("Session filename: ", fname, ".\n"))      # print the URL for this session
  
  if(!fname %in% validFiles){                          # is that one of our files?
    output$pageStub <- renderUI(tagList(              # 404 if no file with that name
      fluidRow(
        column(5,
               HTML("<p style='margin-left: 40px;margin-right: 30px'><BR>Welcome to our site. COVIDWATCH EU-NPA is an international project ",
                    "summarising lessons learnt from the COVID-19 pandemic",
                    "<BR><BR> Use the menu above to navigate the site</p>")
        )
      )
    ))
    return()    # to prevent a "file not found" error on the next line after a 404 error
  }
  source(fname, local=TRUE)                            # load and run server code for this page
}
# Run the application
shinyApp(ui = ui, server = server)
