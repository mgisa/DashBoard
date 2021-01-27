#Project: The InteractivenVisually Air Quality Report in Rwanda.
#Institution: REMA
#Data: Air Quality Stations
#Author: Mr. Murera Gisa
#_______________________________

                     #REMA AIR QUALITY REPORTS 2020
#____________________________________
#Installing packages
install.packages("latticeExtra", repos="http://R-Forge.R-project.org")
install.packages("shinyauthr",repos="http://R-Forge.R-project.org")

devtools::install_github("paulc91/shinyauthr")
install.packages("tidyverse")
install.packages("openair")

#Loading the libraries
library(tidyverse)
library(lattice)
library(openair)
#____________________________

#Setting Working Directory
setwd("C:/Users/Murera Gisa/Desktop/AQSR")

#1. Monthly Report (Monthly concentration of Air Pollutants)
#__________________
Dec2019 <- read.csv("C:/Users/Murera Gisa/Desktop/AQSR/April2020.csv",stringsAsFactors = FALSE, header = TRUE)
View(Dec2019)
Dec2019$date <- as.POSIXct(Dec2019$date,format= "%d/%m/%Y %H:%M")
View(Dec2019)

####CALENDARPLOT FOR MAY###########
MayData<-read.csv("C:/Users/Murera Gisa/Desktop/AQSR/MayData.csv", 
         stringsAsFactors = FALSE, header = TRUE)
View(MayData)
dfp <- MayData%>%
  mutate(pm25=as.numeric(MayData[,3]),
    o3 = as.numeric(MayData[,2])
  )
View(dfp)

#selectByDate(data, month = c(3,6,10), year = 2000)
#calendarPlot(selectByDate(data,month = c(3,6,10), year = 2000))

#Calendarplot of PM2.5
#_____________________________

calendarPlot(selectByDate(dfp,month = 5), year = 2020, pollutant = "pm25",annotate = "date", w.shift = 2, main="Air Quality Status (PM2.5), May 2020 in Kigali City",breaks = c(0, 15.4, 40.4, 65.4,150.4, 250.4, 500.4),labels = c("Good (0-15.4 ugm-3)", "Moderate (15.5-40.4 ugm-3)","USG (40.5-65.4 ugm-3)","Unhealthy (65.5-150.4 ugm-3)","VU (150.5-250.4 ugm-3)", "Hazardous (250.4-500.4 ugm-3)"),  cols = c("forest green","yellow","orange","red","purple","#654321"), statistic = "mean",key.position = "right", key.header = "PM2.5, ug/m3")
#Calendarplot of Ozone
#_____________________________
calendarPlot(selectByDate(dfp,month = 5), year = 2020, pollutant = "o3",annotate = "date", w.shift = 2, main ="Air Quality Status (Ozone), May 2020 in Kigali City",breaks = c(0, 54, 70, 85,105, 200, 504),labels = c("Good (0-54 ppb)", "Moderate (55-70 ppb)","USG (71-85 ppb)","Unhealthy (86-150 ppb)","VU (151-200 ppb)", "Hazardous (201-504 ppb)"),  cols = c("forest green","yellow","orange","red","purple","#654321"), statistic = "mean",key.position = "right", key.header = "O3, ppb")

#2. Weekly Report (Weekly Concentration of Air Pollutants)
#________________________
Feb2020 <- read.csv("C:/Users/Murera Gisa/Desktop/AQSR/MayW5.csv", stringsAsFactors = FALSE, header = TRUE)
View(Feb2020)
Feb2020$date <- as.POSIXct(Feb2020$date,format= "%d/%m/%Y %H:%M")
View(Feb2020)
dfp1 <- Feb2020%>%
  mutate(pm25=as.numeric(Feb2020[,3]),
    o3 = as.numeric(Feb2020[,2])
  )
View(dfp1)
dailydata <- openair::timeAverage(dfp1, avg.time = "day",data.thresh = 90)
View(dailydata)
#write.table(comparision_table, "ML_Performance.csv", sep=",", row.names = F)
write.csv( dailydata, "MayW5daily2020.csv", row.names = F)

Feb2020W1 <- read.csv("C:/Users/Murera Gisa/Desktop/AQSR/JuneW1.csv", 
                      stringsAsFactors = FALSE, header = TRUE)
View(Feb2020W1)
#BarPlot of week2 Jan
#___________________
library(tidyverse)
JuneW1_O<- ggplot(data = Feb2020W1, aes(x= Days,Avg_PM2.5, fill= AQI_PM2.5))+
  geom_bar(width = 0.8, stat = "identity")+ coord_cartesian(ylim = c(0, 55)) + 
  scale_fill_manual(values= c("yellow","orange"))+ theme_bw()+ 
  labs(x = "Week days", y = "PM2.5 daily average concentration in ugm-3", caption = "Source: Air Quality@mgisa") + 
  geom_text(aes(label = str_c(Avg_PM2.5,"ugm-3")),vjust = 4.5,angle= 45,size = 4.5, color = "black") + 
  scale_x_discrete(limits = c("Monday","Tuesday","Wednesday", "Thursday","Friday","Saturday", "Sunday")) +
  ggtitle("PM2.5 Weekly Air Quality Status (June,1-7, 2020)") +
  theme(axis.text.x = element_text(angle = 45, face = "bold", colour = "black", size = 15),
        axis.title.x= element_text(size = 10, angle = 90,vjust = 5, face = "bold"), 
        axis.title.y = element_text(size = 12, angle = 90,vjust = 0.5,face = "bold", color = "black"), 
        plot.title = element_text(size = 15, face="bold", colour="red"),legend.position="none")

JuneW1_PM2.5
ggsave("JuneW1_O3.png", width = 10, height = 7)

#3. Daily Report (Daily average concentration)
#________________
Jan12020 <- read.csv("C:/Users/Gisa/Desktop/AQRS/data142020.csv", stringsAsFactors = FALSE, header = TRUE)
View(Jan12020)
Jan12020$date <- as.POSIXct(Jan12020$date,format= "%d/%m/%Y %H:%M")
View(Jan12020)
dailydata <- openair::timeAverage(Jan12020, avg.time = "hour",data.thresh = 90)
View(dailydata)

#Barplot for Ozone Hourly Concentartion
#___________________________________________
library(tidyverse)
Hourly_1jan <- read.csv("C:/Users/Gisa/Desktop/AQRS/Hourly_9jan.csv", header = TRUE)
JanHW9_PM2.5<- ggplot(data = Hourly_1jan, aes(x= time,PM2.5, fill= AQI_PM2.5))+ geom_bar(width = 0.8, stat = "identity")+ coord_cartesian(ylim = c(0, 70)) + scale_fill_manual(values= c("yellow","orange"))+ theme_bw() + labs(x = "Time of the day", y = "PM2.5 hourly average concentration in ug m-3", caption = "Source: Air Quality@mgisa") + geom_text(aes(label = str_c(PM2.5)), hjust = 0, vjust = 0,nudge_x = -0.45, nudge_y = 0.1,angle= 360,size = 5, color = "black") + scale_x_discrete(limits = c("0h","1h","2h", "3h","4h","5h", "6h","7h","8h","9h","10h", "11h","12h","13h", "14h","15h","16h","17h","18h", "19h","20h","21h", "22h","23h")) + ggtitle("PM2.5 Hourly Air Quality Status (January 9, 2020 )") + theme(axis.text.x = element_text(angle = 45, face = "bold", colour = "black", size = 15),axis.title.x= element_text(size = 10, angle = 90,vjust = 5, face = "bold"), axis.title.y = element_text(size = 12, angle = 90,vjust = 0.5,face = "bold", color = "black"), plot.title = element_text(size = 15, face="bold", colour="red"),legend.position="none")

JanHW9_PM2.5 
ggsave("JanHW9_PM2.5.png", width = 10, height = 7)
#______________________________________________________________
      #OPENAIR DATA VISUALIZATION, TIME PLOT Ozone &PM2.5
#__________________________________________
AQ_data <- read.csv("C:/Users/Murera Gisa/Desktop/AQSR/MayData.csv", stringsAsFactors = FALSE, header = TRUE)
View(AQ_data)
dim(AQ_data)
AQ_data$date <- as.POSIXct(AQ_data$date,format= "%d/%m/%Y %H:%M")
View(AQ_data)
dailydata <- openair::timeAverage(AQ_data, avg.time = "day",data.thresh = 95)
View(dailydata)
dim(dailydata)
dailydata<-dailydata%>%
  dailydata[1:278,]
write.table(dailydata, file="dailyFEBMA.csv", row.names=FALSE,na="", col.names = TRUE, sep=",")
library(tidyverse)
AQ_data<- AQ_data%>%
  mutate(pm25=as.numeric(unlist(AQ_data[,3])),
         o3 = as.numeric(unlist(AQ_data[,2]))
  )
View(AQ_data)
selectByDate(data, month = c(3,6,10), year = 2000)
calendarPlot(selectByDate(data,month = c(3,6,10), year = 2000))
library(openair)
dailydata1<- openair::timeAverage(AQ_data, avg.time = "day",data.thresh = 0)
View(dailydata1)
write.table(dailydata, file="dailydata_May_feb.csv", row.names=FALSE,na="", col.names = TRUE, sep=",")
#Time series plot of PM2.5
#_____________________________
AQ_data <- read.csv("C:/Users/Murera Gisa/Desktop/AQSR/dailydata_May_Jan.csv", 
                    stringsAsFactors = FALSE, header = TRUE)
AQ_data1 <- read.csv("C:/Users/Murera Gisa/Desktop/AQSR/Jan_March2020.csv", 
                    stringsAsFactors = FALSE, header = TRUE)
View(AQ_data)
AQ_data$date <- as.POSIXct(AQ_data$date,format= "%m/%d/%Y")
View(AQ_data)
DT::datatable(AQ_data)
library(scales)
library(tidyverse)
Timepm2.5 <- ggplot(data =AQ_data, aes(date, pm25))+ geom_line(color = "red", size = 1) + 
  theme_bw()+ coord_cartesian(ylim = c(0, 120))+ 
  scale_x_datetime(labels = date_format("%b-%Y"))+ggtitle("Time Evolution of PM2.5 in Kigali (May, 2019 to April,2020)") + 
  labs(x = "Time alocation", y = "Timely PM2.5 Concentration",caption = "Source:Air_Quality@mgisa") + 
  theme(axis.text.x = element_text(angle = 360, face = "bold", colour = "black", size = 12), axis.title.x = element_text(size = 10, angle = 90,vjust = 5, face = "bold"), axis.title.y = element_text(size = 10, angle = 90,vjust = 0.5,face = "bold"),plot.title = element_text(size =14,colour = "red", face = "bold"))

ggsave("Timepm2.5.jpg", width = 10, height = 7)

TimeOzone <-ggplot(data = AQ_data, aes(date, o3))+ geom_line(color = "blue", size = 1) + 
  theme_bw()+ coord_cartesian(ylim = c(0, 160))+ 
  scale_x_datetime(labels = date_format("%b-%Y"))+ggtitle("Time Evolution of Ozone (O3)in Kigali (May, 2019 to April,2020)") + 
  labs(x = "Time alocation", y = "Timely Ozone Concentration",caption = "Source:Air_Quality@mgisa") + 
  theme(axis.text.x = element_text(angle = 360, face = "bold", colour = "black", size = 12), axis.title.x = element_text(size = 10, angle = 90,vjust = 5, face = "bold"), axis.title.y = element_text(size = 10, angle = 90,vjust = 0.5,face = "bold"),plot.title = element_text(size =14,colour = "red", face = "bold"))

ggsave("TimeOzone.jpg", width=10, height = 7)
Timeplot<-gridExtra::grid.arrange(Timepm2.5,TimeOzone,nrow=2)
ggsave("Timeplot.pdf", width = 10, height = 7)
#_____________________________________________

output<-openair::timePlot(AQ_data, pollutant =c("pm25","o3"),group = FALSE, 
                  normalise = "mean", avg.time = "day",data.thresh = 0,y.relation = "same",
                  statistic = "mean",date.pad = TRUE,cols = c("red","purple"),
                  log = FALSE,plot.type = "l", key = TRUE, smooth = FALSE,
                  ci = FALSE,name.pol =c("PM2.5","Ground-level Ozone"),
                  lwd = 3,lwd = 3,ylab="The Types of Air Pollutants",
                  main="The Timely Evolution of Air Pollutants (PM2.5 and O3) in Kigali City",
                  ref.y = list(h = c(0, 50), lty = c(1, 5), col = c("yellow", "black")),
                  date.format = "%b-%Y",key.position="bottom", ylim=c(0,8))
#___________________________________________________________
#MONTHLY BAR PLOT
library(tidyverse)
May_data <- read.csv("C:/Users/Murera Gisa/Desktop/AQSR/AugustW1.csv", 
                    stringsAsFactors = FALSE, header = TRUE)
View(May_data)
class(May_data$date)
strptime(May_data$date, format = "%m/%d/%Y")      
View(May_data)
May_data$date <- as.POSIXct(May_data$date,format= "%m/%d/%Y")
View(May_data)
NO2<-ggplot(May_data, aes(x= date,y=NO2, fill="cyan"))+
  geom_bar(stat="identity")+ coord_cartesian(ylim = c(0, 60))+
  theme_bw()+ geom_text(aes(label = str_c(O3,"")),angle=360,size =.6)+
  scale_fill_manual(values = "cyan")+scale_x_datetime(breaks = date_breaks("1 day"),expand = c(0,0))+
  labs(x = "Date", y = "Pollutant Concentration",caption = "Source:mgisa@Air_Quality")+ 
  ggtitle("The Nitrogen dioxide (NO2) Concentration in May 2020")+
  theme(axis.title.y = element_text(size = 17, angle = 90,vjust = 0.5,colour = "black",face = "bold"),
        axis.text.y = element_text(angle = 45, face = "bold", colour = "black", size = 12),
        axis.text.x = element_text(angle = 90, face = "bold", colour = "black", size = 12),
        axis.title.x = element_blank(),
        #axis.title.x = element_text(size = 17, angle = 360,vjust = 5,colour = "black",face = "bold"),
        plot.title = element_text(size = 20, face="bold", colour="forest green"),
        legend.position="none")
NO2
ggsave("NO2.jpg", width=10, height = 7)
###########################
May_data1 <- read.csv("C:/Users/Murera Gisa/Desktop/AQSR/EndJune.csv", 
                    stringsAsFactors = FALSE, header = TRUE)
View(May_data1)
dfp2 <- May_data1%>%
  mutate(no2 = as.numeric(May_data1[,2]),
         o3 = as.numeric(May_data1[,5]),
         co = as.numeric(May_data1[,4]),
         so2 = as.numeric(May_data1[,3]),
         pm25 = as.numeric(May_data1[,6])
  )
library(openair)
calendarPlot(selectByDate(dfp2,month = 6), year = 2020, pollutant = "co",
             annotate = "date", w.shift = 2, main="Air Quality Status (CO), June 2020 in Kigali City",
             breaks = c(0, 4400, 9400, 12400, 15400, 30400, 40400),
             labels = c("Good (0-4400 ppb)", "Moderate (44001-9400 ppb)","USG (9400- 12400 ppb)","Unhealthy (12401-15400 ppb)","VU (15401-30400 ppb)", "Hazardous (30401-40400 ppb)"), 
             cols = c("forest green","yellow","orange","red","purple","#654321"), 
             statistic = "mean",key.position = "right", key.header = "CO, ppb")
#Calendarplot of Ozone
######################################################
library(tidyverse)
library(scales)
AugustW1_SO2<- ggplot(data = May_data, aes(x= date, Avg_SO2, fill= AQI_SO2))+
  geom_bar(stat="identity")+ coord_cartesian(ylim = c(0,9)) + 
  scale_fill_manual(values= c("forest green","forest green"))+ theme_bw()+ scale_x_datetime(breaks = date_breaks("1 day"),expand = c(0,0))+
  labs(x = "Week days", y = "SO2 daily average concentration in ppb", caption = "Source: Air Quality@mgisa") + 
  geom_text(aes(label = str_c(Avg_SO2, "ppb")),vjust = 4.5,angle= 45,size = 4.5, color = "black") + 
  #scale_x_discrete(limits = c("Monday","Tuesday","Wednesday", "Thursday","Friday","Saturday", "Sunday")) 
 ggtitle("SO2 Weekly Air Quality Status (August 3-9, 2020)") +
  theme(axis.text.x = element_text(angle = 360, face = "bold", colour = "black", size = 15),axis.title.x= element_text(size = 10, angle = 90,vjust = 5, face = "bold"), axis.title.y = element_text(size = 12, angle = 90,vjust = 0.5,face = "bold", color = "black"), plot.title = element_text(size = 15, face="bold", colour="red"),legend.position="none")
AugustW1_SO2
ggsave("AugustW1_SO2.jpg", width=10, height = 7)
#__________________________________________________________
                         #END
