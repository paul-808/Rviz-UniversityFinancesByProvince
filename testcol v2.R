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

#Read data from tidy and alignment script
fincol <- read.csv("fincol.csv", header=TRUE, col.names = c("NA","NA","REVENUE","Ref_Date", "Fund", "Value","Category","GEO"))[ ,3:8]

enrol <- read.csv("enrol.csv", header=TRUE)[,3:8]
enrolcol <- enrol[enrol$TYPE=="College",]

#Reactive global filters and aggregation of data depending on inputs
enrolcol.r = enrolcol %>%
    filter(STATUS=="Full-time student",IMMIGRA == "Canadian students") %>%
    group_by(Ref_Date, GEO2) %>%
    summarise_each(funs(sum),-TYPE,-STATUS,-IMMIGRA)




#Over time aggregation
fincol.t<-fincol
enrolcol.t<-enrolcol

fincol.t <- fincol.t %>%
  group_by(GEO, Ref_Date) %>%
  summarise_each(funs(sum),-REVENUE,-Fund,-Category)

#If per enrolment

fincol.t <- merge(fincol.t, enrolcol.t, by.x = c('GEO','Ref_Date'), by.y = c("GEO2",'Ref_Date'), all.x=TRUE)

fincol.t$Ref_Date <- as.numeric(substr(fincol.t$Ref_Date,1,4))
fincol.t$amount  <- fincol.t$Value.x/fincol.t$Value.y

p1a <- ggplot(fincol.t, aes(x=Ref_Date,y=amount, colour=GEO)) + 
  geom_line() +
  geom_point(alpha=.4) +
  scale_y_continuous(labels = scales::dollar) +
  theme_minimal(base_size = 11) + scale_colour_hue(name="Province") +
  ylab('Revenue ($K)') + xlab('Year')
p1a