###################################READING DATASET###################################################
library(ggplot2)
library(lubridate)
library(tidyr)
library(dplyr)
library(OneR)
library(reshape2)
uber <- read.csv("Uber Request Data.csv",stringsAsFactors = FALSE)
uber1 <- uber

###################################CLEANING DATASET###################################################

uber1$Request.timestamp <- parse_date_time(uber1$Request.timestamp,orders = c("ymd H:M:S", "dmy H:M:S", "mdy H:M:S","ymd H:M", "dmy H:M", "mdy H:M"))
uber1<- separate(uber1,Request.timestamp,into=c("Request_Date","Request_Time"),sep=" ",remove=FALSE)
uber1$Request_Hours <- substr(uber1$Request_Time,1,2)
uber1$Request_Minutes <- substr(uber1$Request_Time,4,5)

uber1$Drop.timestamp <- parse_date_time(uber1$Drop.timestamp,orders = c("ymd H:M:S", "dmy H:M:S", "mdy H:M:S","ymd H:M", "dmy H:M", "mdy H:M"))
uber1<- separate(uber1,Drop.timestamp,into=c("Drop_Date","Drop_Time"),sep=" ",remove=FALSE)
uber1$Drop_Hours <- substr(uber1$Drop_Time,1,2)
uber1$Drop_Minutes <- substr(uber1$Drop_Time,4,5)

###################################CREATING PLOTS########################

###################################FREQUENCY DISTRIBUTION OF CANCELLED & NO CARS AVAILABLE#############################
ggplot(uber1[uber1$Status %in% c("Cancelled","No Cars Available"),], aes(x =Status,fill=Pickup.point)) +  geom_bar(position = "dodge") +ylab("Total Number of Requests") +theme_classic() + geom_text(stat='count', aes(label=..count..),position = position_dodge(width = 1),vjust = -0.3)

####################################CREATING TIME SLOTS##################################################

#00:00-03:59 Late Night
#04:00-07:59 Early Morning
#08:00-11:59 Late Morning
#12:00-15:59 Afternoon 
#16:00-19:59 Evening
#20:00-23:59 Night
 
uber1$time_slot <- bin(as.numeric(uber1$Request_Hours), nbins = 6, labels = c("Late Night","Early Morning","Late Morning", "Afternoon","Evening","Night"))

####################################Number of Requests(From Airport to City)##################################################
ggplot(uber1[uber1$Pickup.point %in% c("Airport"),], aes(x=time_slot,fill=Status)) +  geom_bar(position = "dodge") +ylab("Number of Requests from Airport to City") + theme_classic()+ geom_text(stat='count', aes(label=..count..),position = position_dodge(width = 1),vjust = -0.3)


####################################Number of Requests(From City to Airport)##################################################
ggplot(uber1[uber1$Pickup.point %in% c("City"),], aes(x=time_slot,fill=Status)) +  geom_bar(position = "dodge") +ylab("Number of Requests from City to Airport") +theme_classic()+geom_text(stat='count', aes(label=..count..) ,position = position_dodge(width = 1),vjust = -0.3)

###################################SUBSETTING DATA ON THE BASIS OF PICKUP POINT########################
###################################CREATING PLOT HOUR WISE############################################

uber1_fromAirport <- subset(uber1,Pickup.point == "Airport" )
test_fromAirport <- group_by(uber1_fromAirport,Request_Hours,Status)
df_Airport<- summarise(test_fromAirport,length(as.numeric(Request_Hours)))
colnames(df_Airport)<- c('Request_Hours','Status','Number_of_Requests')
ggplot(df_Airport ,aes(x = factor(Request_Hours),y = Number_of_Requests,group=Status,color=Status)) + geom_line()+theme_classic()+labs(x = "Request_Hours",y = "Number of Requests from Airport to City",title = paste("Hourly Plot of Requests from Airport"))

uber1_fromCity <- subset(uber1,Pickup.point == "City" )
test_fromCity <- group_by(uber1_fromCity,Request_Hours,Status)
df_City<- summarise(test_fromCity,length(as.numeric(Request_Hours)))
colnames(df_City)<- c('Request_Hours','Status','Number_of_Requests')
ggplot(df_City ,aes(x = factor(Request_Hours),y = Number_of_Requests,group=Status,color=Status)) + geom_line()+theme_classic()+labs(x = "Request_Hours",y = "Number of Requests from City to Airport",title = paste("Hourly Plot of Requests from City"))

##################################CREATING PLOT DATE WISE FROM AIRPORT TO CITY#############################################

######BAR GRAPH####
ggplot(uber1[uber1$Pickup.point %in% c("Airport"),],aes(x=factor(Request_Date),fill=Status)) +  geom_bar(position = "dodge")+theme_classic() +labs(x ="Request_Date",y ="Number of Requests from Airport to City") + geom_text(stat='count', aes(label=..count..),position = position_dodge(width = 1),vjust = -0.3)

##################################CREATING PLOT DATE WISE FROM CITY TO AIRPORT#############################################

######BAR GRAPH####
ggplot(uber1[uber1$Pickup.point %in% c("City"),],aes(x=factor(Request_Date),fill=Status)) +  geom_bar(position = "dodge")+theme_classic() +labs(x ="Request_Date",y ="Number of Requests from City to Airport") + geom_text(stat='count', aes(label=..count..),position = position_dodge(width = 1),vjust = -0.3)

###############################################PLOTTING DEMAND VS SUPPLY###########################################################################
################FROM AIRPORT TO CITY####################

get_Airport_Demand <- group_by(uber1_fromAirport,time_slot)
test_the_Airport_Demand<- summarise(get_Airport_Demand,length(time_slot))
colnames(test_the_Airport_Demand)<- c('time_slot','Demand')

get_Airport_Supply <- group_by(uber1_fromAirport[uber1_fromAirport$Status %in%  c("Trip Completed"),],time_slot )
test_the_Airport_Supply<- summarise(get_Airport_Supply,length(time_slot))
colnames(test_the_Airport_Supply)<- c('time_slot','Supply')

Demand_Supply_From_Airport_To_City <- merge(x=test_the_Airport_Supply,y=test_the_Airport_Demand,by.x=c("time_slot"),by.y = c("time_slot"))
Demand_Supply_From_Airport_To_City.long<-melt(Demand_Supply_From_Airport_To_City)

ggplot(Demand_Supply_From_Airport_To_City.long,aes(time_slot,value))+ geom_bar(aes(fill = variable), position = "dodge", stat="identity") + geom_text(aes(label =value,group=variable),position = position_dodge(width = 1),vjust = -0.2)+theme_classic()+labs(x = "Time-Slot",y = "Demand and Supply",title = paste("Demand and Supply from Airport to City at different time slots"))

################FROM CITY TO AIRPORT####################
get_City_Demand <- group_by(uber1_fromCity,time_slot)
test_the_City_Demand<- summarise(get_City_Demand,length(time_slot))
colnames(test_the_City_Demand)<- c('time_slot','Demand')

get_City_Supply <- group_by(uber1_fromCity[uber1_fromCity$Status %in%  c("Trip Completed"),],time_slot )
test_the_City_Supply<- summarise(get_City_Supply,length(time_slot))
colnames(test_the_City_Supply)<- c('time_slot','Supply')

Demand_Supply_From_City_To_Airport <- merge(x=test_the_City_Supply,y=test_the_City_Demand,by.x=c("time_slot"),by.y = c("time_slot"))
Demand_Supply_From_City_To_Airport.long<-melt(Demand_Supply_From_City_To_Airport)

ggplot(Demand_Supply_From_City_To_Airport.long,aes(time_slot,value))+ geom_bar(aes(fill = variable), position = "dodge", stat="identity") + geom_text(aes(label =value,group=variable),position = position_dodge(width = 1),vjust = -0.2)+theme_classic()+labs(x = "Time-Slot",y = "Demand and Supply",title = paste("Demand and Supply from City to Airport at different time slots"))


