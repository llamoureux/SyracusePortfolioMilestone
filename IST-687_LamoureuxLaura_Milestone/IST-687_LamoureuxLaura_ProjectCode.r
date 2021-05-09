# save project survey data to a dataframe called df
df <- data.frame(ProjectSurveyData)
# structure of dataframe
str(df)

# Data Preparation Steps

# convert Reservation Date to date format
df$RESERVATION_DATE_R <- as.Date(df$RESERVATION_DATE_R)
# look at first few rows
head(df$RESERVATION_DATE_R)

# convert Check in Date to date format
df$CHECK_IN_DATE_C <- as.Date(df$CHECK_IN_DATE_C)
# look at first few rows
head(df$CHECK_IN_DATE_C)

# check for NAs in the variables needed for my analysis
any(is.na(NPS))
any(is.na(df$Likelihood_Recommend_H))
any(is.na(df$LENGTH_OF_STAY_C))
any(is.na(WALK_IN_FLG_C))
any(is.na(CHECK_IN_DATE_C))
any(is.na(RESERVATION_DATE_R))
any(is.na(Guest_Country_H))
any(is.na(Country_PL))

# calculate average length of stay
meanLS <- mean(df$LENGTH_OF_STAY_C,na.rm=TRUE)
roundedMeanLS <- round(meanLS)
roundedMeanLS

# replace NAs from LengthofStay with mean length of stay
df$LENGTH_OF_STAY_C[is.na(df$LENGTH_OF_STAY_C)]<- roundedMeanLS

# remove rows with no GuestCountry
df <- df[!(is.na(df$Guest_Country_H)),]
length(df[,1])

# assign each cleaned variable to a renamed vector
# NPS
NPS <- df$NPS
head(NPS)

tapply(NPS, NPS, length)

# Likelihood to Recommend 
LTR <- df$Likelihood_Recommend_H
head(LTR)

# Length of Stay
LS <- df$LENGTH_OF_STAY_C
head(LS)

# WalkIn Flag
WalkInStatus <- df$WALK_IN_FLG_C
head(WalkInStatus)

# CheckIn Date
CheckInDate <- df$CHECK_IN_DATE_C
head(CheckInDate)

# Reservation Date
ReserveDate <- df$RESERVATION_DATE_R
head(ReserveDate)

# GuestCountry
GuestCountry <- df$Guest_Country_H
head(GuestCountry)

# HotelCountry
HotelCountry <- df$Country_PL
head(HotelCountry)

# change United States to USA in HotelCountry Column
HotelCountry[HotelCountry=="United States"] <- "USA"
HotelCountry[HotelCountry=="United States"]
HotelCountry[HotelCountry=="USA"]
HotelCountry

# AdvanceDays
# calculate a new column called AdvanceDays representing
# how far in advance a reservation was made
AdvanceDays <- CheckInDate - ReserveDate
# make AdvanceDays numeric
AdvanceDays <- as.numeric(AdvanceDays)
# look at first few rows of AdvanceDays
head(AdvanceDays)

# Free Independent vs. Group Travel
FITvGroup <- df$GROUPS_VS_FIT_R
head(FITvGroup)

# data does not exist for this variable

# create a dataframe called HotelData to hold the variables needed for analysis
HotelData <- data.frame(NPS,LTR,LS,WalkInStatus,CheckInDate,ReserveDate,GuestCountry,AdvanceDays,HotelCountry)
# look at first few rows of HotelData
head(HotelData)
str(HotelData)

# use printVecInfo function for all continuous variables
printVecInfo(LTR)
printVecInfo(LS)

# create a bar chart for each discrete variable in HotelData

# NPS
bar_NPS <- ggplot(HotelData, aes(x=NPS)) + geom_bar()
bar_NPS 

# WalkIn Flag
bar_WalkInStatus <- ggplot(HotelData, aes(x=WalkInStatus)) + geom_bar()
bar_WalkInStatus

# create a histogram for each continous variable in HotelData

# LTR
hist_LTR <- ggplot(HotelData, aes(x=LTR)) + geom_histogram(binwidth=1,color="black", fill="white")
hist_LTR <- hist_LTR + scale_x_continuous(name="Likelihood to Recommend")
hist_LTR

# LS
hist_LS <- ggplot(HotelData, aes(x=LS)) + geom_histogram(binwidth=1,color="black", fill="white")
hist_LS <- hist_LS + scale_x_continuous(name="Length of Stay (days)", limits=c(0, 15))
hist_LS

# figure out how many outlier dates exist
# Breakdown CheckInDate onto its components: Year, Month, Day
# then add these columns to HotelData

# create Year from CheckInDate
CIYear <- year(HotelData$CheckInDate)
unique(CIYear)
HotelData$CIYear <- CIYear
# create Month from CheckInDate
CIMonth <- month(HotelData$CheckInDate)
unique(CIMonth)
HotelData$CIMonth <- CIMonth
# create Day from CheckInDate
CIDay <- day(HotelData$CheckInDate)
unique(CIDay)
HotelData$CIDay <- CIDay

# create a variable called "season"
# determine which months are in this variable
tapply(CIMonth,CIMonth,length)
# since all months are in Winter, no need to create a variable called season

head(HotelData)
str(HotelData)

sqldf("SELECT CIDay AS Day FROM HotelData GROUP BY CIDay ORDER BY CIDay")

# determine the number of record(s) with 2013 & 2014 CheckInDates
# count rows
Year1 <- HotelData[HotelData$CIYear==2013,]
length(Year1[,1])
Year2<- HotelData[!(HotelData$CIYear==2013),]
length(Year2[,1])
# remove the 1 record with the 2013 CheckInDate from the dataset
HotelData <- Year2

attach(HotelData)
head(HotelData)
str(HotelData)

# CountryMatch
# create a new column called CountryMatch representing
# whether hotel guest stayed in is in their country
# of origin (Y) or not (N)

x <- c(1:length(HotelData[,1]))
head(x)

Compare <- function(x) {
  result <- GuestCountry[x]==HotelCountry[x]
  return(result)
  }

HotelData$CountryMatch <- Compare(x)
head(HotelData$CountryMatch)
length(HotelData$CountryMatch)

# histogram of CheckIn dates
hist_CheckIn <- ggplot(HotelData, aes(x=CheckInDate)) + geom_histogram(binwidth=1,color="black", fill="white")
hist_CheckIn <- hist_CheckIn + xlab("CheckIn Date") + ylab("Number of CheckIns") + ggtitle("CheckIns per Day in early 2014")
hist_CheckIn <- hist_CheckIn + scale_x_date(limits=c("2014-01-01","2014-04-01"))
hist_CheckIn

count <- tapply(LTR, CIMonth, length)
count

# line plot of CheckIn dates
CheckIns <- sqldf("SELECT CheckInDate, COUNT(CheckInDate) AS NumCheckIns FROM HotelData GROUP BY CheckInDate")
dfCheckIns <- data.frame(CheckIns)
dfCheckIns

line_CheckIn <- ggplot(dfCheckIns,aes(x=CheckInDate, y=NumCheckIns)) + geom_line()
line_CheckIn


# ReserveDate
hist_ReserveDate <- ggplot(HotelData, aes(x=ReserveDate)) + geom_histogram(binwidth=1,color="black", fill="white")
hist_ReserveDate <- hist_ReserveDate + xlab("Reservation Date") + ylab("Number of Reservations") + ggtitle("Reservation Dates by Day for Stays in early 2014")
hist_ReserveDate <- hist_ReserveDate + scale_x_date(limits=c("2013-07-01","2014-04-01"))
hist_ReserveDate

# determine where the spike is
# count the number of reservations by date, then order them by the number of reservations
sqldf("SELECT ReserveDate, COUNT(ReserveDate) AS NumberReservations FROM HotelData
      GROUP BY ReserveDate ORDER BY NumberReservations DESC")

# AdvanceDays
hist_AD <- ggplot(HotelData, aes(x=AdvanceDays)) + geom_histogram(binwidth=5)
hist_AD <- hist_AD + ggtitle("Days in Advance Reservation was made")
hist_AD

# GuestCountry
bar_GuestCountry <- ggplot(HotelData, aes(x=GuestCountry, y=length(GuestCountry))) + geom_bar(stat="identity")
bar_GuestCountry <- bar_GuestCountry + xlab("Guest Country") + ylab("Number of Reservations") + ggtitle("Reservations per Guest Country")
bar_GuestCountry <- bar_GuestCountry + theme(axis.text.x = element_text(angle = 90, hjust = 1))
bar_GuestCountry

##############################################################

# Q1 Does length of stay impact NPS?

# look at Likelihood to Recommend by Length of Stay
tapply(LTR, list(LS==1,NPS), length)

# What relationship exists between NPS and length of stay?
# plot NPS vs Length of Stay
plot(LS, LTR)

LTRbyLS <- sqldf("SELECT AVG(LTR) AS AvgLTR,
      LS
      FROM HotelData
      GROUP BY LS")
LTRbyLS <- data.frame(LTRbyLS)
head(LTRbyLS)

# plot AvgLTR by LS
plot(LTRbyLS$LS, LTRbyLS$AvgLTR)

# build a linear model
model <- lm(formula=AvgLTR ~ LS, LTRbyLS)
summary(model)
abline(model)

# Conclusion: Length of Stay does NOT significantly impact NPS
# (Adjusted R^2 value is negative and very small)

##############################################################

# Q2(a): Does whether guest stay was a walk-in or a reservation impact NPS?
# determine how many were WalkIns vs. not
WalkInStatus_Breakdown <- tapply(WalkInStatus, WalkInStatus, length)
WalkInStatus_Breakdown

# Conslusion: Base size of walk-ins is not big enough to draw conclusions about 
# the impact of WalkInStatus on NPS

#################################################################
# Q2(b): Is NPS affected by how far in advance the reservation was made?

# plot Likelihood to Recommend by Advance Days
plot(AdvanceDays, LTR)

# build a linear model
model <- lm(LTR ~ AdvanceDays)
summary(model)
abline(model)

# create a data frame containing average likelihood to recommend by Advance Days
LTRbyAD <- sqldf("SELECT AVG(LTR) AS AvgLTR,
                 AdvanceDays AS AD
                 FROM HotelData
                 GROUP BY AD")
LTRbyAD <- data.frame(LTRbyAD)
head(LTRbyAD)

# plot AvgLTR by AD
plot(LTRbyAD$AD, LTRbyAD$AvgLTR)

# build a linear model
model <- lm(formula=LTRbyAD$AvgLTR ~ LTRbyAD$AD, LTRbyAD)
summary(model)
abline(model)

# Conclusion: AdvanceDays does NOT significantly impact NPS
# (Adjusted R^2 value is negative and very small)

############################################################################

# Q3: Does guest country of origin impact NPS?

USA_stays <- length(HotelData$GuestCountry[HotelData$GuestCountry=="USA"])
USA_stays
nonUSA_stays <- length(HotelData$GuestCountry[!(HotelData$GuestCountry=="USA")])
nonUSA_stays

HotelData$USorNot <- ifelse((HotelData$GuestCountry=="USA"), "USA", "nonUSA")
HotelData$USorNot
attach(HotelData)

# create a bar chart of Guest Country
gg_bar <- ggplot(HotelData, aes(x=HotelData$USorNot)) + geom_bar()
gg_bar

result <- sqldf("SELECT GuestCountry, AVG(LTR) AS LTR FROM HotelData GROUP BY GuestCountry ORDER BY LTR DESC")
LTRbyCountry <- data.frame(result)
LTRbyCountry

plot(LTRbyCountry$GuestCountry, LTRbyCountry$LTR)

# inspect the range of average LTRs by country
printVecInfo(LTRbyCountry$LTR)

# create a linear model of LTR by Country
CountryModel <- lm(LTR ~ GuestCountry, HotelData)
summary(CountryModel)
abline(CountryModel)

# calculate AvgLTR by whether country was US or Not
LTRbyUSorNot <- sqldf("SELECT AVG(LTR) AS AvgLTR,
      USorNot
      FROM HotelData
      GROUP BY USorNot")
LTRbyUSorNot <- data.frame(LTRbyUSorNot)
LTRbyUSorNot

############################################################################

# Q3(b): How does country of origin impact NPS in countries other than the guest's country of origin?

# create a bar chart of Country Match, depicting # of stays where hotel country was in the guest's
# country of origin or not

gg_bar <- ggplot(HotelData, aes(x=CountryMatch)) + geom_bar()
gg_bar

# calculate AvgLTR by whether guest stayed in home country or not
LTRbyCM <- sqldf("SELECT AVG(LTR) AS AvgLTR,
                 CountryMatch AS CM
                 FROM HotelData
                 GROUP BY CountryMatch")
LTRbyCM <- data.frame(LTRbyCM)
LTRbyCM

############################################################################

# Q4: During which time is NPS the highest? (season/month/day of week/weekday vs. weekend)

# since all months are in Winter, no need to create a variable called season
# most data was collected in February; base size is too small to compare to January or March

# I'm not sure how to determine which weekday each stay was on

############################################################################

# Q5: How does whether travel is free independent travel vs. group travel impact NPS?

# data does not exist for this variable

############################################################################

# Q22: Which survey data questions might help us to understand and predict NPS?

# save project survey data to a dataframe called df
df <- data.frame(ProjectSurveyData)
# structure of dataframe
str(df)

# Data Preparation Steps

# convert survey data to numeric format
df$Guest_Room_H <- as.numeric(df$Guest_Room_H)
df$Tranquility_H <- as.numeric(df$Tranquility_H)
df$Condition_Hotel_H <- as.numeric(df$Condition_Hotel_H)
df$Customer_SVC_H <- as.numeric(df$Customer_SVC_H)
df$Staff_Cared_H <- as.numeric(df$Staff_Cared_H)
df$Internet_Sat_H <- as.numeric(df$Internet_Sat_H)
df$Check_In_H <- as.numeric(df$Check_In_H)

# look at first few rows of each
head(df$Guest_Room_H)
head(df$Tranquility_H)
head(df$Condition_Hotel_H)
head(df$Customer_SVC_H)
head(df$Staff_Cared_H)
head(df$Internet_Sat_H)
head(df$Check_In_H)

# check for NAs in the variables needed for my analysis
any(is.na(df$NPS))
any(is.na(df$Likelihood_Recommend_H))
any(is.na(df$Guest_Room_H))
any(is.na(df$Tranquility_H))
any(is.na(df$Condition_Hotel_H))
any(is.na(df$Customer_SVC_H))
any(is.na(df$Staff_Cared_H))
any(is.na(df$Internet_Sat_H))
any(is.na(df$Check_In_H))

# Likelihood to Recommend 
LTR <- df$Likelihood_Recommend_H
head(LTR)

# Guest Room Satisfaction
GuestRoom <- df$Guest_Room_H
head(GuestRoom)

# Tranquility
Tranquility <- df$Tranquility_H
head(Tranquility)

# Condition
Condition <- df$Condition_Hotel_H
head(Condition)

# Customer Service
CustServ <- df$Customer_SVC_H
head(CustServ)

# Staff Cared
Staff <- df$Staff_Cared_H
head(Staff)

# Internet
Internet <- df$Internet_Sat_H
head(Internet)

# CIProcess
CIProcess <- df$Check_In_H
head(CIProc)

# create a dataframe called HotelData to hold the variables needed for analysis
HotelData <- data.frame(LTR,GuestRoom,Tranquility,Condition,CustServ,Staff,Internet,CIProcess)
# look at first few rows of HotelData
head(HotelData)
str(HotelData)

# remove rows containing NAs
HotelData <- na.omit(HotelData)
str(HotelData)

############################################################################

# Which portions of survey data might lend some insight into Likelihood to Recommend Scores?

SurveyDataModel_1 <- lm(LTR ~ GuestRoom + Tranquility + Condition + CustServ + Staff + Internet + CIProcess, HotelData)
summary(SurveyDataModel_1)

SurveyDataModel_2 <- lm(LTR ~ CustServ, HotelData)
summary(SurveyDataModel_2)

SurveyDataModel_3 <- lm(LTR ~ CustServ + GuestRoom, HotelData)
summary(SurveyDataModel_3)

SurveyDataModel_4 <- lm(LTR ~ CustServ + GuestRoom + Condition, HotelData)
summary(SurveyDataModel_4)

SurveyDataModel_5 <- lm(LTR ~ CustServ + GuestRoom + Condition + Staff + Tranquility, HotelData)
summary(SurveyDataModel_5)

# recommend using combination of Customer Service, GuestRoom & Condition of Hotel to predict LTR

str(HotelData)

############################################################################

# re-read in data set, and this time, don't remove rows that have NAs for Internet or cIProcess
# to avoid losing so many rows of data

# save project survey data to a dataframe called df
df <- data.frame(ProjectSurveyData)
# structure of dataframe
str(df)

# Data Preparation Steps

# convert survey data to numeric format
df$Guest_Room_H <- as.numeric(df$Guest_Room_H)
df$Tranquility_H <- as.numeric(df$Tranquility_H)
df$Condition_Hotel_H <- as.numeric(df$Condition_Hotel_H)
df$Customer_SVC_H <- as.numeric(df$Customer_SVC_H)
df$Staff_Cared_H <- as.numeric(df$Staff_Cared_H)


# look at first few rows of each
head(df$Guest_Room_H)
head(df$Tranquility_H)
head(df$Condition_Hotel_H)
head(df$Customer_SVC_H)
head(df$Staff_Cared_H)


# check for NAs in the variables needed for my analysis
any(is.na(df$NPS))
any(is.na(df$Likelihood_Recommend_H))
any(is.na(df$Guest_Room_H))
any(is.na(df$Tranquility_H))
any(is.na(df$Condition_Hotel_H))
any(is.na(df$Customer_SVC_H))
any(is.na(df$Staff_Cared_H))


# Likelihood to Recommend 
LTR <- df$Likelihood_Recommend_H
head(LTR)

# Guest Room Satisfaction
GuestRoom <- df$Guest_Room_H
head(GuestRoom)

# Tranquility
Tranquility <- df$Tranquility_H
head(Tranquility)

# Condition
Condition <- df$Condition_Hotel_H
head(Condition)

# Customer Service
CustServ <- df$Customer_SVC_H
head(CustServ)

# Staff Cared
Staff <- df$Staff_Cared_H
head(Staff)

# create a dataframe called HotelData to hold the variables needed for analysis
HotelData <- data.frame(LTR,GuestRoom,Tranquility,Condition,CustServ,Staff)
# look at first few rows of HotelData
head(HotelData)
str(HotelData)

# remove rows containing NAs
HotelData <- na.omit(HotelData)
str(HotelData)

############################################################################

# Which portions of survey data might lend some insight into Likelihood to Recommend Scores?

SurveyDataModel_5 <- lm(LTR ~ CustServ + GuestRoom + Condition + Staff + Tranquility, HotelData)
summary(SurveyDataModel_5)

str(HotelData)

-------------------------------------------------------------------------------------------------------------------------------
# Load file
file_path <- "~/Syracuse/IST687_Intro_DS/GroupProject/ProjectSurveyData.csv"
hotel_data <- read.csv(file=file_path, header=TRUE, sep=",", stringsAsFactors = FALSE)

attach(hotel_data)

# ---------
# Functions
# ---------

# Generate Bar Graph
generate_bar_graph <- function(df, x, y, x_label, y_label){
  # Create bar graph
  g <- ggplot(df, aes(x=reorder(x, -y), y=y)) + geom_bar(stat="identity")
  title <- paste(y_label, "by", x_label, sep = " ")
  g <- g + ggtitle(title) + theme(plot.title = element_text(hjust=0.5))
  g <- g + xlab(x_label) + ylab(y_label) +  theme(axis.text.x = element_text(angle = 90, hjust = 1))
  return(g)
}

# Clean and return dataframe of percentages
get_percentages <- function(data){
  data[is.na(data)] <- 0
  data <- round(data[,"Promoter"] / (data[,"Promoter"] + data[,"Detractor"] + data[,"Passive"]),3) # Get Percentages
  data <- data[order(-data)] # Order data by descending
  data <- data.frame(data) # Sadatae as dataframe
  return(data)
}

# -------------------------------------------------
# 6) Which regions have the highest NPS Percentage?
# -------------------------------------------------

nps.region <- tapply(NPS_Type, list(Region_PL, NPS_Type), length) # Query Data
top_region <- get_percentages(nps.region) # Generate Percentage dataframe
generate_bar_graph(top_region, rownames(top_region), top_region$data, "Region", "Net Promoter Score %" ) # Generate bar graph

# -------------------------------------------------
# 6) Which hotels have the worst NPS Percentage?
# -------------------------------------------------

nps.hotel <- tapply(NPS_Type, list(Hotel.Name.Short_PL, NPS_Type), length) # Query Data
nps_hotel <- get_percentages(nps.hotel) # Generate Percentage dataframe
nps_hotel.hist <- hist(nps_hotel$data, main="Histogram for Hotels", xlab="NPS Percentage by promoter")
nps_hotel.hist
summary(nps_hotel)
worst_hotel <- tail(nps_hotel, 20)
best_hotel <- head(nps_hotel, 20)
generate_bar_graph(worst_hotel, rownames(worst_hotel), worst_hotel$data, "Hotel", "Net Promoter Score %" ) # Generate bar graph
generate_bar_graph(best_hotel, rownames(best_hotel), best_hotel$data, "Hotel", "Net Promoter Score %" ) # Generate bar graph

# ----------------------------------------
# 7) Which countries have the highest NPS?
# ----------------------------------------

nps.country <- tapply(NPS_Type, list(Country_PL, NPS_Type), length) # Query Data
top_countries <- get_percentages(nps.country) # Generate Percentage dataframe
top_countries <- head(top_countries, 20) # Get highest percentages from dataframes
generate_bar_graph(top_countries, rownames(top_countries), top_countries$data, "Country", "Net Promoter Score %" ) # Generate bar graph

# -----------------------------------------------
# 10) How does the reason for stay impact the NPS
# -----------------------------------------------

nps.stay <- tapply(NPS_Type, list(POV_CODE_C, NPS_Type), length) # Query data
top_stay <- get_percentages(nps.stay) # Generate Percentage dataframe
generate_bar_graph(top_stay, rownames(top_stay), top_stay$data, "Reason for Stay", "Net Promoter Score %" ) # Generate bar graph

rstay <- sqldf("Select POV_CODE_C as stay,  Likelihood_Recommend_H as score from hotel_data")
rmodel <- lm(score ~ stay, data = rstay) # Run linear model
summary(rmodel)

# ---------------------------------------------------
# 11) Which rooms have the lowest net promoter score?
# ---------------------------------------------------

nps.room <- tapply(NPS_Type, list(ROOM_TYPE_CODE_C, NPS_Type), length) # Query Data
nps.room <- get_percentages(nps.room) # Generate Percentage dataframe
best_rooms <- head(nps.room, 20)
worst_room <- tail(nps.room, 20) # Get lowest pecentages from dataframe
generate_bar_graph(worst_room, rownames(worst_room), worst_room$data, "Room Type", "Net Promoter Score %" ) # Generate bar graph

nps.room.hist <- hist(nps.room$data, main="Histogram for Rooms", xlab="NPS Percentage")
summary(nps.room)

# ---------------------------------------------
# How does Award Impact likelihood to recommend
# ---------------------------------------------

df <- sqldf("Select hotel_data.'Award.Category_PL' as award,  Likelihood_Recommend_H as score from hotel_data")
rmodel <- lm(df$score ~ df$award, data = df) # Run linear model
summary(rmodel) # Summarize Model

# ----------------------------------------------------------------------------------------
# Does the difference between expected and actual costs impact the likelihood to recommend
# ----------------------------------------------------------------------------------------

df <- sqldf("Select round(abs((REVENUE_USD_R - (QUOTED_RATE_C * LENGTH_OF_STAY_C ))),2) as cost_diff,  Likelihood_Recommend_H as score from hotel_data") # Get the difference between actual and expected cost
rmodel <- lm(df$score ~ df$cost_diff, data = df) # Run linear model
summary(rmodel) # Summarize model

# -----------------------------------------------------------------------------------
# Which of the other hotel features contributes to the likelihood to recommend score? 
# -----------------------------------------------------------------------------------

s <- sqldf("Select Likelihood_Recommend_H, Guest_Room_H, Tranquility_H, Condition_Hotel_H, Customer_SVC_H, Staff_Cared_H, Internet_Sat_H, Check_In_H from hotel_data")
rmodel <- lm(Likelihood_Recommend_H ~ ., data = s) # Run linear model
summary(rmodel) # Summarize Model

s_new <- sqldf("Select Likelihood_Recommend_H, Guest_Room_H, Tranquility_H, Condition_Hotel_H, Customer_SVC_H, Staff_Cared_H from hotel_data")
rmodel <- lm(Likelihood_Recommend_H ~ ., data = s_new) # Run linear model
summary(rmodel) # Summarize Model
-------------------------------------------------------------------------------------------------------------------------------###################################################################
# Q12
# Does the room rate the guest paid stayed impact the NPS?
# Unable to answer due to the lack of data

###################################################################
# Q13
# Does size of hotel (number of rooms &/or number of floors) impact the NPS?
# Unable to answer due to the lack of data

###################################################################
# Q14
# Does whether or not the guest was offered a promotion impact the NPS?
# - Is either past or future offer more impactful?
# Unable to answer due to the lack of data

###################################################################
# Q15
# Which age groups give the highest NPS?
npsAge <- table(hotels$Age_Range_H, hotels$NPS_Type)
npsAge

# Removing NAs
npsAge <- na.omit(npsAge)
npsAge
View(npsAge)
npsAge <- npsAge[-c(1),]
npsAge
dfnpsAge <- as.data.frame(npsAge)
dfnpsAge

# Plot 1: stacked histogram
hotels$count <- 1
npsageCounts <- aggregate(hotels$count, by = list(age=hotels$Age_Range_H,
                                                     NPS_Type=hotels$NPS_Type), FUN=sum)
npsagePlot1 <- ggplot(npsageCounts, aes(x=age, y=x, fill=NPS_Type)) + 
  geom_bar(stat = "identity")
npsagePlot1  

# determine color pallette - color-blind friendly
cbfPalette <- c("#999999", "#E69F00", "#56B4E9")

# Plot 2: plot seperated by age group and promoter score
npsagePlot2 <- ggplot(dfnpsAge, aes(x=Var1, y=Freq, fill=Var2)) + 
  geom_bar(stat = "identity", position=position_dodge()) + 
  scale_fill_manual(values=cbfPalette, name="NPS Type") + labs(x="Age", y="Counts") +
  ggtitle("NPS Type by Age")
npsagePlot2

###################################################################
# Q16
# Does gender of survey taker affect NPS?


# Removing NAs
npsGender <- table(hotels$Gender_H, hotels$NPS_Type)
npsGender
npsGender <- na.omit(npsGender)
npsGender
View(npsGender)
npsGender <- npsGender[-c(1),]
npsGender <- npsGender[-c(3),]
npsGender
View(npsGender)
dfnpsGender

# Plot 1: stacked histogram
hotels$count <- 1
npsGenderCounts <- aggregate(hotels$count, by = list(gender=hotels$Gender_H,
                                                     NPS_Type=hotels$NPS_Type), FUN=sum)
npsGenderPlot1 <- ggplot(npsGenderCounts, aes(x=gender, y=x, fill=NPS_Type)) + 
  geom_bar(stat = "identity")
npsGenderPlot1  

# determine color pallette - color-blind friendly
cbfPalette <- c("#999999", "#E69F00", "#56B4E9")

# Plot 2: plot seperated by gender and promoter score
npsGenderPlot2 <- ggplot(dfnpsGender, aes(x=Var1, y=Freq, fill=Var2)) + 
  geom_bar(stat = "identity", position=position_dodge()) + 
  scale_fill_manual(values=cbfPalette, name="NPS Type") + labs(x="Gender", y="Counts") +
  ggtitle("NPS Type by Gender")
npsGenderPlot2

========================================================================

#file_path2 <- "Documents/IST Homework (Fall 2019)/ProjectSurveyDataCSV.csv"
#hotels <- read.csv(file=file_path2, header=TRUE, sep=",")
#View(hotels)


# Project Update 2
install.packages("maps")
install.packages("openintro") 
install.packages("ggmap")
install.packages("sqldf")
install.packages("propagate")
install.packages("zipcode")
install.packages("mapproj")
install.packages("readxl")
install.packages("gdata") 

library(gdata) 
library(openintro) 
library(ggmap)
library(sqldf)
library("ggplot2")
library ("propagate")
library(dplyr)
library(readxl)
library(plyr)
library(readxl)
library(zipcode)

require(ggplot2)
require(maps)
require(ggmap)


#file_path <- "./Documents/IST Homework (Fall 2019)/ProjectSurveyDataCSV.csv/"
setwd("~/Documents/IST Homework (Fall 2019)")

hotel_data <- read.csv("ProjectSurveyDataCSV.csv")
hotel_data <- hotel_data[!is.na(hotel_data$ADULT_NUM_C),]



attach(hotel_data)

# Generate Bar Graph
generate_bar_graph <- function(df, x, y, x_label, y_label){
  # Create bar graph
  g <- ggplot(df, aes(x=reorder(x, -y), y=y)) + geom_bar(stat="identity")
  title <- paste(y_label, "by", x_label, sep = " ")
  g <- g + ggtitle(title) + theme(plot.title = element_text(hjust=0.5))
  g <- g + xlab(x_label) + ylab(y_label) +  theme(axis.text.x = element_text(angle = 90, hjust = 1))
  return(g)
}

# Clean and return dataframe of percentages
get_percentages <- function(data){
  data[is.na(data)] <- 0
  data <- round(data[,"Promoter"] / (data[,"Promoter"] + data[,"Detractor"] + data[,"Passive"]),3) # Get Percentages
  data <- data[order(-data)] # Order data by descending
  data <- data.frame(data) # Sadatae as dataframe
  return(data)
}

ditch_the_axes <- theme(
  axis.text = element_blank(),
  axis.line = element_blank(),
  axis.ticks = element_blank(),
  panel.border = element_blank(),
  panel.grid = element_blank(),
  axis.title = element_blank()
)
#---------------------------------------------
#Does the class of the hotel (luxury, etc.) impact the NPS?
#---------------------------------------------

#Show percentage of Promoter NPS Rating based on Hotel Class using Daniel's Functions

nps.class <- tapply(NPS_Type, list(Class_PL, NPS_Type), length) # Query Data
top_class <- get_percentages(nps.adults) # Generate Percentage dataframe
generate_bar_graph(top_class, rownames(top_class), top_class$data, "Hotel Class", "Net Promoter Score %" ) # Generate bar graph

detach(hotel_data)

#Sub set data, taking Class and NPS together as well as Class and Likelihood together from main hotel data
new2data <- subset(hotel_data, select = c('Class_PL', 'NPS_Type'))
new2data2 <- subset(hotel_data, select = c('Class_PL', 'Likelihood_Recommend_H'))  


#Created new data frame with columns that count how many times each Class was rated with each NPS Type
Cl.NPS <- as.data.frame(count(new2data))

#Calculate percentages for each Class Type creating a df and putting them into a label column
CLperDF <- ddply(Cl.NPS, .(Class_PL), transform, percent = 100* (freq/ sum(freq)))
CLperDF <- ddply(CLperDF, .(Class_PL), transform, pos = (cumsum(freq) - 0.5 * freq))
CLperDF$Label <- paste0(sprintf("%.0f", CLperDF$percent), "%")
Clp.NPS <- CLperDF
title <- paste('NPS Rating per Hotel Class')

#creating plot for Class type as a stacked bar graph with the percentages in the middle
CNplot <- ggplot(Clp.NPS, aes(x = Class_PL, y = freq, fill = NPS_Type)) + geom_bar(position = position_stack(), stat = "identity", width =.7)
CNplot <- CNplot + ggtitle(title) + xlab('Class Type') + ylab('Number of Reviews') + scale_fill_discrete(name = 'NPS Type')
CNplot <- CNplot + geom_text(aes(label = Clp.NPS$Label), position = position_stack(vjust = 0.5),size = 3 )

CNplot

##same for Likelihood

CL.LIKE <- as.data.frame(count(new2data2))

CLperDF2 <- ddply(CL.LIKE, .(Class_PL), transform, percent = 100* (freq/ sum(freq)))
CLperDF2 <- ddply(CLperDF2, .(Class_PL), transform, pos = (cumsum(freq) - 0.5 * freq))
CLperDF2$Label <- paste0(sprintf("%.0f", CLperDF2$percent), "%")
Clp.LIKE <- CLperDF2
colnames(Clp.LIKE) <- c('Class_PL', 'Likelihood', 'freq', 'percent', 'pos', 'Label')
Clp.LIKE$Likelihood <- as.factor(Clp.LIKE$Likelihood)

title2 <- paste('LTR by Hotel Class')
CLplot <- ggplot(Clp.LIKE, aes(x = Class_PL, y = freq, fill = Likelihood)) + geom_bar(position = position_stack(), stat = "identity", width =.7)
CLplot <- CLplot + ggtitle(title2) + xlab('Class Type') + ylab('Number of Reviews') + scale_fill_discrete(name = 'Likelihood Rating')
CLplot <- CLplot + geom_text(aes(label = Clp.LIKE$Label), position = position_stack(vjust = 0.5),size = 3 )

CLplot


#Creating a plot that displays the amount of reservations per Hotel Class, with the fill of being the average likelihood

#finding the mean of the likelihood per Hotel Class
CLLiker <- tapply(hotel_data$Likelihood_Recommend_H, list(hotel_data$Class_PL), mean)
CLLiker <- as.data.frame(CLLiker)
CLLiker
#Creating a data base from the count of the reservations per hotel class with the average for each class
CLL.DB <- count(hotel_data$Class_PL)
CLL.DB <- cbind(CLL.DB, CLLiker)
colnames(CLL.DB) <- c('Class_PL', 'freq', 'Ave')
roundr2 <- round(as.numeric(CLL.DB$Ave), digits=2)
likemean <- mean(hotel_data$Likelihood_Recommend_H)

#Creating the bar graph with the fill of blue and red, red being a lower average and blue being a higher
CLLplot <- ggplot(CLL.DB, aes(x = Class_PL, y = freq, fill = Ave)) + geom_bar(stat = "identity", width =.7)
CLLplot <- CLLplot + ggtitle('Average Likelihood per Hotel Class') + xlab('Hotel Class') + ylab('Number of Reviews')
CLLplot <- CLLplot + geom_text(aes(label = roundr2), position = position_stack(vjust = 0.5),size = 3,)
CLLplot <- CLLplot + scale_fill_gradient2(midpoint = likemean, low = "red", mid = "yellow", high = 'green', space = "Lab")
CLLplot


#---------------------------------------------  
# Does having one vs. multiple adults impact the NPS?
#---------------------------------------------  

#Show percentage of Promoter NPS Rating based on Adults per Reservation using Daniel's Functions
attach(hotel_data)

nps.adults <- tapply(NPS_Type, list(ADULT_NUM_C, NPS_Type), length) # Query Data
top_adults <- get_percentages(nps.adults) # Generate Percentage dataframe
generate_bar_graph(top_adults, rownames(top_adults), top_adults$data, "Number of Adults", "Net Promoter Score %" ) # Generate bar graph
detach(hotel_data)


newdata <- subset(hotel_data, select= c("ADULT_NUM_C", "NPS_Type"))
newdata2 <- subset(hotel_data, select= c("ADULT_NUM_C", "Likelihood_Recommend_H"))

AD.NPS <- as.data.frame(count(newdata))

#Calculate percentages for each Class Type creating a df and putting them into a label column
ADperDF <- ddply(AD.NPS, .(ADULT_NUM_C), transform, percent = 100* (freq/ sum(freq)))
ADperDF <- ddply(ADperDF, .(ADULT_NUM_C), transform, pos = (cumsum(freq) - 0.5 * freq))
ADperDF$Label <- paste0(sprintf("%.0f", CLperDF$percent), "%")
ADp.NPS <- ADperDF

title3 <- paste('NPS Rating by Number of Adults per Room')

#creating plot for Class type as a stacked bar graph with the percentages in the middle
ANplot <- ggplot(ADp.NPS, aes(x = ADULT_NUM_C, y = freq, fill = NPS_Type)) + geom_bar(position = position_stack(), stat = "identity", width =.7)
ANplot <- ANplot + ggtitle(title3) + xlab('Number of Adults') + ylab('Number of Reviews') + scale_fill_discrete(name = 'NPS Type')
ANplot <- ANplot + geom_text(aes(label = ADp.NPS$Label), position = position_stack(vjust = 0.5),size = 3 )
ANplot <- ANplot + scale_x_continuous(breaks = 1:8)
ANplot


AD.LIKE <- as.data.frame(count(newdata2))

ADperDF2 <- ddply(AD.LIKE, .(ADULT_NUM_C), transform, percent = 100* (freq/ sum(freq)))
ADperDF2 <- ddply(ADperDF2, .(ADULT_NUM_C), transform, pos = (cumsum(freq) - 0.5 * freq))
ADperDF2$Label <- paste0(sprintf("%.0f", ADperDF2$percent), "%")
ADp.LIKE <- ADperDF2
colnames(ADp.LIKE) <- c('ADULT_NUM_C', 'Likelihood', 'freq', 'percent', 'pos', 'Label')
ADp.LIKE$Likelihood <- as.factor(ADp.LIKE$Likelihood)

title4 <- paste('LTR by Number of Adults')
ALplot <- ggplot(ADp.LIKE, aes(x = ADULT_NUM_C, y = freq, fill = Likelihood)) + geom_bar(position = position_stack(), stat = "identity", width =.7)
ALplot <- ALplot + ggtitle(title4) + xlab('Number of Adults') + ylab('Number of Reviews') + scale_fill_discrete(name = 'Likelihood Rating')
ALplot <- ALplot + geom_text(aes(label = ADp.LIKE$Label), position = position_stack(vjust = 0.5),size = 3,)

ALplot


#Creating a plot that displays the amount of reservations per Adults listed per reservation, with the fill being the average likelihood

#finding the mean of the likelihood per #of Adults
ADLiker <- tapply(hotel_data$Likelihood_Recommend_H, list(hotel_data$ADULT_NUM_C), mean)
ADLiker <- as.data.frame(ADLiker)

#creating a data base with the count of reservations and average likelihood for each #of Adults
ADL.DB <- count(hotel_data$ADULT_NUM_C)
ADL.DB <- cbind(ADL.DB, ADLiker)
colnames(ADL.DB) <- c('ADULT_NUM_C', 'freq', 'Ave')
roundr <- round(as.numeric(ADL.DB$Ave), digits=2)

#Creating the bar graph with the fill of blue and red, red being lower average and blue being high
ADLplot <- ggplot(ADL.DB, aes(x = ADULT_NUM_C, y = freq, fill = Ave)) + geom_bar(stat = "identity", width =.7)
ADLplot <- ADLplot + ggtitle('Average Likelihood per Number of Adults') + xlab('Number of Adults') + ylab('Number of Reviews')
ADLplot <- ADLplot + geom_text(aes(label = roundr), position = position_stack(vjust = 0.5),size = 3,)
ADLplot <- ADLplot + scale_x_continuous(breaks = 1:8)
ADLplot <- ADLplot + scale_fill_gradient2(midpoint = likemean, low = "red", mid = "yellow", high = 'green', space = "Lab")
ADLplot


###US DATA

MyKey = "AIzaSyAGxIGyEnpqPdxyUcyf5CF6aM0kgdCziUU"
register_google(key = MyKey)
data(zipcode)

#Grab the US locations only
US_Data <-  hotel_data[which(hotel_data$Country_PL=='United States'),]

#Create subset of data containing how many reservations per city
citycounts <- table(US_Data$City_PL)
citycounts <- as.data.frame(citycounts)
colnames(citycounts) <- c('City', 'Reservations')
citycounts$Reservations <- as.numeric(citycounts$Reservations)

#More Cleaning and Renaming US cities
citycounts <- citycounts[citycounts$Reservations !=0,]
citycounts$City <- gsub(',','', citycounts$City)
citycounts$City <- gsub('Irving','Irving, TX', citycounts$City)
citycounts$City <- gsub('Rosemond','Rosemond, CA', citycounts$City)
citycounts$City <- gsub('Tulsa','Tulsa, OK', citycounts$City)
citycounts$City <- gsub('Windsor','Windsor, CN', citycounts$City)
citycounts$City <- gsub('Malta','Malta, NY', citycounts$City)
citycounts$City <- gsub('Florence','Florence, SC', citycounts$City)
citycounts$City <- gsub('Dublin','Dublin, OH', citycounts$City)
citycounts$City <- gsub('Bethlehem','Bethlehem, PA', citycounts$City)
str(citycounts)
head(citycounts)

#Im not sure what this is, but google said to do this so i did
Murica <- map_data('state')

#Create column that had the average likeihood per city and clean up some NAs
likey <- tapply(US_Data$Likelihood_Recommend_H, list(US_Data$City_PL), mean)
likey <- as.data.frame(likey)
Likelihood <- likey[!is.na(likey$likey),]
mid <- mean(Likelihood)

#Binds the likelihood column and the longitude and latitude columns created with geocode function using google api to the cities DF
stays <- cbind(Likelihood, citycounts)

stays <- cbind(geocode(as.character(citycounts$City)), stays)


#Creates plot of the US with the Reservations and Likelihood represented on cities.
res_map <- ggplot(Murica, aes(map_id=region))
res_map <- res_map + geom_map(data=Murica, map=Murica,aes(x=long, y=lat, map_id=region))
res_map <- res_map + geom_point(data=stays, aes(x=lon, y=lat, size=Reservations,color=Likelihood),inherit.aes = FALSE)
res_map <- res_map + scale_color_gradient2(midpoint = mid, low = "red", mid = "yellow", high = 'lime green', space = "Lab")
res_map <- res_map + ggtitle('Likelihood and Reservations per City') + ditch_the_axes
res_map <- res_map + coord_map()
res_map


### Regression Models
attach(hotel_data)

rClass <- sqldf("Select Class_PL as class, Likelihood_Recommend_H as score from hotel_data")
rCmodel <- lm(score ~class, data=rClass)
summary(rCmodel)

rAdults <- sqldf("Select ADULT_NUM_C as adults, Likelihood_Recommend_H as scores from hotel_data")
rAmodel <- lm(scores ~adults, data=rAdults)
summary(rAmodel)
