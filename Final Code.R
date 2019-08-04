setwd("G:/Willis Re/C. Willis Re Knowledge Services/A. Teams/CMS/CMS - US/upgrad/EDA Assignment")

#Read the Uber Request Data.csv and save in a dataframe ub

ub <- read.csv("Uber Request Data.csv")
View(ub)

#Clean the Request and the drop time by replacing "/" with "-" and  standardising the format till minutes

ub$x_Request.time <- gsub("/","-",ub$Request.timestamp)
ub$x_Request.time  <- as.POSIXct(ub$x_Request.time, format = "%d-%m-%Y %H:%M")
View(ub)


str(ub)

# Seperating Date and Time in new columns
ub$RequestDate <- as.Date(ub$x_Request.time)
ub$RequestTime<- format(as.POSIXct(strptime(ub$x_Request.time,"%Y-%m-%d %H:%M")), format = "%H:%M")
typeof(ub$RequestTime)

# Seperating Hours from time and creating time slots
ub$RequestHour <- format(as.POSIXct(ub$RequestTime, format = "%H:%M"),"%H")

ub$TimeSlots <- case_when(as.numeric(ub$RequestHour) <3 ~"Late Night", 
                          between(as.numeric(ub$RequestHour),3,6) ~ "Early Morning", 
                          between(as.numeric(ub$RequestHour),7,11) ~ "Morning",
                          between(as.numeric(ub$RequestHour),12,15) ~ "Afternoon",
                          between(as.numeric(ub$RequestHour),16,20) ~ "Evening",
                          between(as.numeric(ub$RequestHour),21,24)~"Night")

# Plot 1: Hourly Demand
HD <- ggplot(ub, aes(x=factor(requesthour),fill=factor(Pickup.point)))
HD +geom_bar(stat = 'count', position="dodge") + ggtitle("Hourly Demand") + labs(x="Hours",y="No of request")+labs(fill="Pickup Point")
ggsave("HourlyDemand.png")

# Plot 2: trips during Time Slots
TS <- ggplot(ub,aes(x= factor(TimeSlots), fill = factor(Status) ))
TS + geom_bar(stat = 'Count',position = "stack")+ggtitle("Trips during time slots")+labs(x = "Time Slots", y= "No of request")+labs(fill="Trip Status")

ggsave("Time Slots.png")

PeakTime_cancellation <- subset(ub,ub$TimeSlots == "Morning")
PeakTime_NoAvail <- subset(ub,ub$TimeSlots == "Evening")

# Plot 3: Mornign Hour Status
PC<- ggplot(PeakTime_cancellation,aes(x=factor(Status),fill=factor(Pickup.point)))
PC + geom_bar(stat = 'Count',position = "stack")+ggtitle("Morning Cab Status")+labs(x = "Status", y= "No of Requests") +labs(fill="Pick-up Point")
ggsave("MorningCabStatus.png",width = 8)

#Calculate % of total
prop.table(table(PeakTime_cancellation$Status))

cancelled <-subset(PeakTime_cancellation,PeakTime_cancellation$Status != "Trip Completed" & PeakTime_cancellation$Status == "Cancelled")

#Calculate % of total
prop.table(table(cancelled$Pickup.point))

# Plot  4: Cancelled Requests
cancel <- ggplot(cancelled, aes(x=factor(Pickup.point)))
cancel +geom_bar(stat='count',fill ="#000077")+ggtitle("Cancelled Requests")+labs(x = "Pick up Point",y = "No of Requests")
ggsave("Cancelled request.png")


# Plot 5: Evening Hour Status
PNoAvail<- ggplot(PeakTime_NoAvail,aes(x=factor(Status),fill=factor(Pickup.point)))
PNoAvail+geom_bar(stat = 'Count',position = "stack")+ggtitle("Evening Cab Status")+labs(x = "Status", y= "No of Requests") +labs(fill="Pick-up Point")


#Calculate % of total
prop.table(table(PeakTime_NoAvail$Status))

NotAvail <-subset(PeakTime_NoAvail,PeakTime_NoAvail$Status != "Trip Completed" & PeakTime_NoAvail$Status == "No Cars Available")
prop.table(table(NotAvail$Pickup.point))
 
# Plot 6: Non Availability of Cabs
nta <- ggplot(NotAvail, aes(x=factor(Pickup.point)))
nta +geom_bar(stat='count',fill ="#000077")+ggtitle("Non Availability of Cabs")+labs(x = "Pick up Point",y = "No of Requests")
ggsave("Non Availability of Cabs.png")
 
