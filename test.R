library(tidyr)
library(plyr)
library(dplyr)
library(stringr)
library(plotly) # devtools::install_github('ropensci/plotly')
library(ggplot2) # devtools::install_github('hadley/ggplot2')
library(flexdashboard) # devtools::install_github('rstudio/flexdashboard')
library(ggthemes)
library(leaflet)
library(scales)

#Read lookups
lookup <- read.csv("lookup.csv", header=TRUE)
provlookup <- read.csv("province.csv", header=TRUE)

#Read data from tidy and alignment script
fiuc <- read.csv("fiuc.csv", header=TRUE, col.names = c("NA","NA","REVENUE","Ref_Date", "Fund", "Value","Category","GEO"))[ ,3:8]

enrol <- read.csv("enrol.csv", header=TRUE)[,3:8]
enrol <- enrol[enrol$TYPE=="University",]

#Reactive global filters and aggregation of data depending on inputs
enrol <- enrol %>%
  filter(STATUS=="Full-time student",IMMIGRA == "Canadian students") %>%
  group_by(Ref_Date, GEO2) %>%
  summarise_each(funs(sum),-TYPE,-STATUS,-IMMIGRA)
  #summarise(Value=sum(Value))

#Prep for category graph
fiuc.c <- fiuc[fiuc$Ref_Date=="2014/2015",]
fiuc.c <- fiuc.c %>%
  group_by(GEO, Ref_Date, Category) %>%
  summarise_each(funs(sum),-REVENUE,-Fund)

##Category per enrolment
enrol.c <- enrol[enrol$Ref_Date=="2014/2015",]
fiuc.c <- merge(fiuc.c, enrol.c, by.x = 'GEO', by.y = "GEO2", all.x=TRUE)

#If total and time graph
fiuc.t <- fiuc %>%
  group_by(GEO, Ref_Date) %>%
  summarise_each(funs(sum),-REVENUE,-Fund,-Category)
fiuc.t$Value.y<-as.numeric(1)
colnames(fiuc.t)[3]<-"Value.x"
fiuc.t$Ref_Date <- as.numeric(substr(fiuc.t$Ref_Date,1,4))
fiuc.t$amount  <- fiuc.t$Value.x/fiuc.t$Value.y



fiuc.t$amount  <- fiuc.t$Value.x/fiuc.t$Value.y
fiuc.t<-fiuc.c
#fiuc.c <- ddply(fiuc.c, .(GEO), transform, pos = cumsum(amount) - (0.5 * amount))