#Load libraries needed
#library(raster)
#library(tidyr)
#library(stringr)
library(dplyr)


#Download data from StatCan
download.file("http://www20.statcan.gc.ca/tables-tableaux/cansim/csv/04770058-eng.zip","fiuc.zip")
download.file("http://www20.statcan.gc.ca/tables-tableaux/cansim/csv/04770060-eng.zip","fincol.zip")
download.file("http://www20.statcan.gc.ca/tables-tableaux/cansim/csv/04770019-eng.zip","enrol.zip")
download.file("http://www20.statcan.gc.ca/tables-tableaux/cansim/csv/00510001-eng.zip","pop.zip")

#Unzip all
unzip("fiuc.zip")
unzip("fincol.zip")
unzip("enrol.zip")
unzip("pop.zip")

#Data read
lookup <- read.csv("lookup.csv", header=TRUE)
lookupcol <- read.csv("lookupcol.csv", header=TRUE)
provlookup <- read.csv("province.csv", header=TRUE)

fiuc <- read.csv("04770058-eng.csv",header=TRUE)
fincol <- read.csv("04770060-eng.csv",header=TRUE)
enrol <- read.csv("04770019-eng.csv",header=TRUE)
pop <- read.csv("00510001-eng.csv",header=TRUE)

#University data filter and aggregate by province
fiuc <- fiuc %>%
  filter(GEO != "Canada", SCHOOL == "Total universities and colleges", REVENUE %in% lookup[,1], FUND != "Total funds (x 1,000)",FUND != "Entities not consolidated (x 1,000)",FUND != "Entities consolidated (x 1,000)", Value!=0) %>%
  select(-SCHOOL, -Vector, -Coordinate)

fiuc<-merge(fiuc, lookup, by = 'REVENUE', all.x=TRUE)
fiuc<-merge(fiuc, provlookup, by = 'GEO', all.x=TRUE)
fiuc<-arrange(fiuc,desc(Ref_Date))
fiuc$FUND <- as.character(fiuc$FUND)
fiuc$FUND <- substr(fiuc$FUND,1,nchar(fiuc$FUND)-10)
fiuc$FUND <- as.factor(fiuc$FUND)

#College data filter and aggregate by province
fincol <- fincol %>%
  filter(GEO != "Canada", REVENUE %in% lookupcol[,1], FUND != "Total funds (x 1,000)", Value!=0) %>%
  select(-Vector, -Coordinate)

fincol<-merge(fincol, lookupcol, by = 'REVENUE', all.x=TRUE)
fincol<-merge(fincol, provlookup, by = 'GEO', all.x=TRUE)
fincol<-arrange(fincol,desc(Ref_Date))
fincol$FUND <- as.character(fincol$FUND)
fincol$FUND <- substr(fincol$FUND,1,nchar(fincol$FUND)-10)
fincol$FUND <- as.factor(fincol$FUND)
fincol<-filter(fincol, GEO2 != "TR")

#Enrolment data filter and aggregate by province
enrol$Value<-as.numeric(as.character(enrol$Value))
enrol <- enrol %>%
  filter(GEO != "Canada",TYPE != "Total, institution type", PCSCE == "Total, Pan-Canadian Standard Classification of Education (PCSCE)",CIPPG == "Total, instructional programs",SEX == "Both sexes", IMMIGRA != "Not reported" , Value != "NA") %>%
  group_by(Ref_Date, GEO, TYPE, STATUS, IMMIGRA) %>% 
  summarise(Value=sum(Value))

enrol<-merge(enrol, provlookup, by = 'GEO', all.x=TRUE)

#Add Population data filter and aggregate by province, both total and 18-24 age
#Enrolment data filter and aggregate by province

pop$Value<-as.numeric(as.character(pop$Value))
pop <- pop %>%
  filter(Ref_Date > 1999, GEO != "Canada",SEX == "Both sexes", AGE == "All ages",Value != "NA") %>%
  group_by(Ref_Date, GEO) %>% 
  summarise(Value=sum(Value))

pop <-merge(pop, provlookup, by = 'GEO', all.x=TRUE)

#Add shapefiles for each province for map
##mapprov<-getData('GADM', country="CAN", level=1)

#Write files for Shiny vis
write.csv(fiuc,"fiuc.csv")
write.csv(fincol,"fincol.csv")
write.csv(enrol, "enrol.csv")
write.csv(pop, "pop.csv")
