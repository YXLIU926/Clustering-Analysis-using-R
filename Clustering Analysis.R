getwd()
data <- read.csv("SunCountry.csv")
#View(data)
gc
memory.limit()
memory.limit(size=12000) #increase memory 

#___________________________________________________________________________________________________
#                                           Data Cleaning
#___________________________________________________________________________________________________
library(dplyr) #use for data cleanup
library(lubridate)

#Remove the rows that do not have birth dates
#Remove the ones that we do not know gender
#Replace age outliers with mean 
#Remove F9 and HW in airline. We will focus on SunCountry
#standadize Uflymembership status, creditcard holder, booking channel, 
#Turn Servicedate into Quarter (Qtr)
#Add a column to indicate whether or not it's a round trip or one-way
#Did people travel in groups or by themselves?

library(lubridate) #we will need this to turn dates into quarters later

nrow(data) #3435388 rows 
summary(data)


#Clear out the rows that do not contain birthdays
data <- filter(data, !is.na(birthdateid))
nrow(data) 

#Clear out the ones that we do not know gender 
data <- filter(data, GenderCode!="") 
nrow(data)

#3391389 rows left
data$gender<-as.factor(data$gender)

#Replace odd age values with mean 
hist(data$Age)
summary(data$Age)
data$Age[data$Age < 0] <- mean(data$Age)
data$Age[data$Age > 110] <- mean(data$Age)

#Create some age groups for the following analyses
#12 - 17
#18 - 24
#25 - 34
#35 - 44
#45 - 54
#55 - 64
#65+
data <- data %>% 
  mutate(.,age_group = with(.,case_when(
                              (Age>=0 & Age<12)~"0-12",
                              (Age>=12 & Age < 18)~"12-17",
                              (Age>=18 & Age <25)~"18-24",
                              (Age>=25 & Age<35)~"25-34",
                              (Age>=35 & Age <45)~"35-44",
                              (Age>=45 & Age < 55)~"45-54",
                              (Age>=55 & Age<65)~"55-64",
                              (Age>=65)~"65+"
                            )))
#Turn categorical age groups into factors
data$age_group<-as.factor(data$age_group)

#In the MarketingAirlineCode column, remove HW (Hawaii) and F9 (Frontier)
data <- data[!grepl("HW",data$MarketingAirlineCode),]
data <- data[!grepl("F9",data$MarketingAirlineCode),]
nrow(data) #3388094 rows left 

#If customers do not have reward numbers, assign a 0
data$UFlyRewardsNumber[is.na(data$UFlyRewardsNumber)] <- 0

#If they do not have UflyMemberStatus, assign a "NA"
data$UflyMemberStatus[data$UflyMemberStatus==""] <- "NA"
table(data$UflyMemberStatus) #2696467 NA, 14312 Elite, 677315  Standard

data$UflyMemberStatus<-as.factor(data$UflyMemberStatus)

#Do the similar thing for "CardHolder," assign "NA" for blank cells
data$CardHolder[data$CardHolder==""] <- "NA"
table(data$CardHolder) #2696467 NA, 656313 false, 35314 True
#Turn the cardholder into a binary variable, 1 stands for card holder, 0 means not a card holder (N/A and false)?
data$CardHolder<-as.factor(data$CardHolder)

#Clean up the BookingChannel column, only leave the major ones and categorize all the others as "Others
#major ones are Reservation Booking, SCA Website booking, SY Vacation tour operation portal, outside booking
table(data$BookingChannel)
data$BookingChannel<-as.character(data$BookingChannel)
data$BookingChannel[data$BookingChannel!="Outside Booking" & data$BookingChannel!="SCA Website Booking" & data$BookingChannel!="Tour Operator Portal" & data$BookingChannel!="Reservations Booking" & data$BookingChannel!="SY Vacation"] <- "Other"
data$BookingChannel<-as.factor(data$BookingChannel)

#which quarter the trip took place in
data <- data %>%
  mutate(Qtr = paste0(quarter(data$ServiceStartDate)))
data$Qtr<-as.factor(data$Qtr)

##How many days in advance a trip usually got booked?
data$PNRCreateDate <- as.Date(data$PNRCreateDate) 
data$ServiceStartDate <- as.Date(data$ServiceStartDate)
data<-data%>%
  mutate(days_in_advance_booked=data$ServiceStartDate-data$PNRCreateDate)
data$days_in_advance_booked<-as.numeric(data$days_in_advance_booked)

#For a given PNR, figure out the resident city (where the traveler started the flight)
resident_city_df<-data%>%
  arrange(PNRLocatorID,CouponSeqNbr)%>% 
  group_by(PNRLocatorID,EncryptedName)%>% 
  do(data.frame(resident_city_df=first(.$ServiceStartCity)))
data<-merge(data,resident_city_df, by=c("PNRLocatorID","EncryptedName"))
data$resident_city_df<-as.factor(data$resident_city_df)

#Decide whether the flight was one-way or round-trip (assign 1 if it is a round trip, and 0 for one-way)
#we have already had the resident city 
#next, create a end_city column, if end_city = resident_city in a PNRLocateorID, then it means that the trip was a round trip
end_city <- data%>%
  arrange(PNRLocatorID,CouponSeqNbr)%>% 
  group_by(PNRLocatorID,EncryptedName)%>%
  do(data.frame(end_city=last(.$ServiceEndCity)))
data<-merge(data,end_city, by=c("PNRLocatorID","EncryptedName"))

data<-data%>%
  mutate(round_trip = ifelse(as.character(end_city)==as.character(resident_city_df), 1, 0))
data$round_trip<-as.factor(data$round_trip)

View(data$round_trip)
write.csv(data,"SunCountry_CleanedData.csv", row.names = FALSE)

#___________________________________________________________________________________________________
#                                   Use K-prototype to find Customer Segments
#___________________________________________________________________________________________________
library(tidyr)
library(dplyr)
library(plotly)
library(ggplot2)
library(fastcluster)
library(cluster)
library(clustMixType)
library(sampling)

data <- read.csv("SunCountry_CleanedData.csv")

data$UflyMemberStatus <- as.character(data$UflyMemberStatus)
data$UflyMemberStatus[is.na(data$UflyMemberStatus)] <- "non-member"
data$combination_sampling = paste(data$age_group," ",data$GenderCode)
table(data$combination_sampling)
data <- data[order(data$combination_sampling),]
summary(data$combination_sampling)
#Combine multiple columns together - this is for stratified sampling 
View(data)
temp<-data
set.seed(2)#to produce a consistent result
#Use Strata to create a around 10k stratified sampling 
temp=strata(data, stratanames = c("combination_sampling","GenderCode"), 
             size=c(35617,35092,30521,25189,1,57455,44069,2,98614,92500,1,76026,19250,1,88981,83207,4,80086,70646,1,54759,47968,1), 
             method = c("srswor"), description = TRUE)
x = getdata(data,temp)
View(data)
View(x)
table(data$combination_sampling)
#write.csv(x,"SunCountry_StratifiedData.csv", row.names = FALSE)

x = read.csv("SunCountry_StratifiedData_final.csv")
#Specify the number of clusters K - the elbow method 
#as the dataset contains a mixture of categorical and continuous variables, we use Kprototypes here

#We first create a subset out of the main dataset, this subset will only contains some useful variables that we would like to explore
#Using the groupby function will first remove some "duplicate records" - that is, for round strips, we will only use the first one in the same PNRLocator ID
#it's important to remove those "duplicate" records
cluster_data<-x%>% 
  group_by(PNRLocatorID,PaxName)%>%
  summarise(PaxName=first(PaxName),
            BookingChannel=first(BookingChannel), 
            amt=max(TotalDocAmt), 
            UflyMemberStatus=first(UflyMemberStatus), 
            AgeGroup=first(age_group), 
            ResidentCity=first(resident_city_df), 
            RoundTrip=first(round_trip), 
            Qtr=first(Qtr), 
            DaysInAdvanceBooked=max(days_in_advance_booked))
View(cluster_data)

#Normalize continuous variable

normalize <- function(x){
  return ((x - min(x))/(max(x) - min(x)))
}
cluster_data<-mutate(cluster_data, 
                     amt = normalize(cluster_data$amt),
                     days_in_advance_booked = normalize(DaysInAdvanceBooked))

#Make all the categorical ones factors
cluster_data$BookingChannel<-as.factor(cluster_data$BookingChannel)
cluster_data$UflyMemberStatus<-as.factor(cluster_data$UflyMemberStatus)
cluster_data$AgeGroup<-as.factor(cluster_data$AgeGroup)
cluster_data$RoundTrip<-as.factor(cluster_data$RoundTrip)
cluster_data$Qtr<-as.factor(cluster_data$Qtr)
View(cluster_data)

#kproto - ELBOW CHART
SSE_curve <- c()
sil_curve <- c()
for (n in 2:10) {
  kcluster <- kproto(cluster_data[,3:10], n)
  sil <- silhouette(kcluster$cluster, dist(cluster_data[,3:10]))
}
plot(2:10, sil_curve, type="b", xlab="Number of Clusters", ylab="silhouette")

# Run the clustering
kcluster<- kproto(cluster_data[,3:10],5)
View(kcluster)
kcluster$size

#combine clustering result into the original data
cluster <- as.factor(kcluster$cluster)
final_cluster <- cbind(cluster_data,cluster)
View(final_cluster)
write.csv(final_cluster,"ClusterData.csv", row.names = FALSE)
#___________________________________________________________________________________________________
#                             Discuss Customer Behavior Based on Their Segments
#___________________________________________________________________________________________________

library(ggplot2)
library(dplyr)
library(dplyr, warn.conflicts = FALSE)
library(tibble)
library(tidygeocoder)
getwd()
df_SunCountry <- read.csv("ClusterData.csv")

#Prepping work: Turn dummy variables into categorical characters, for easy visualization
df_SunCountry$RoundTrip[df_SunCountry$RoundTrip=="1"] <- "Round Trip"
df_SunCountry$RoundTrip[df_SunCountry$RoundTrip=="0"] <- "One-way"
df_SunCountry$Qtr[df_SunCountry$Qtr=="1"] <- "First Quarter"
df_SunCountry$Qtr[df_SunCountry$Qtr=="2"] <- "Second Quarter"
df_SunCountry$Qtr[df_SunCountry$Qtr=="3"] <- "Third Quarter"
df_SunCountry$Qtr[df_SunCountry$Qtr=="4"] <- "Fourth Quarter"


#prepping work: Split the rows by clusters
df_s1 <-
  df_SunCountry %>%
  filter(Segment == 1)
df_s2 <-
  df_SunCountry %>%
  filter(Segment == 2)
df_s3 <-
  df_SunCountry %>%
  filter(Segment == 3)
df_s4 <-
  df_SunCountry %>%
  filter(Segment == 4)
df_s5 <-
  df_SunCountry %>%
  filter(Segment == 5)

# PART I: Descriptive data: Use plots to understand each segment's unique characteristics #

#Age

ggplot(df_SunCountry,aes(x=Segment,fill=AgeGroup))+geom_bar(position = 'fill') + scale_fill_brewer(palette="Oranges")
#Customers in Cluster 5 tend to contain more senior (55+) than other clusters
#Cluster 3 has more middle-aged customers than the rest
#Cluster2 and 4 both have more young people who are between 25 and 45 years old 
#Cluster 1 has more 18 - 24 young people

#Age - the chart produced above have too many age groups so it may not be intuitively to read. We will merge some groups together for a straightforward graph
df_SunCountry$AgeGroup[df_SunCountry$AgeGroup=="17-Dec"] <- "12-17" #also fix the typo
df_SunCountry$AgeGroup[df_SunCountry$AgeGroup=="0-12"] <- "0-17" 
df_SunCountry$AgeGroup[df_SunCountry$AgeGroup=="12-17"] <- "0-17" 
df_SunCountry$AgeGroup[df_SunCountry$AgeGroup=="25-34"] <- "25-54" 
df_SunCountry$AgeGroup[df_SunCountry$AgeGroup=="35-44"] <- "25-54" 
df_SunCountry$AgeGroup[df_SunCountry$AgeGroup=="45-45"] <- "25-54" 
df_SunCountry$AgeGroup[df_SunCountry$AgeGroup=="55-64"] <- "55+" 
df_SunCountry$AgeGroup[df_SunCountry$AgeGroup=="65+"] <- "55+" 
ggplot(df_SunCountry,aes(x=Segment,fill=AgeGroup))+geom_bar(position = 'fill') + scale_fill_brewer(palette="Oranges")

#UflyMembershipStatus
ggplot(df_SunCountry,aes(x=Segment,fill=UflyMemberStatus))+geom_bar(position = 'fill') + scale_fill_brewer(palette="OrRd")

#Which quarter the customers would prefer to travel?
ggplot(df_SunCountry,aes(x=Segment,fill=Qtr))+geom_bar(position = 'fill') + scale_fill_brewer(palette="Oranges")

#Where are these customers primarily located?
ResidentCity <- group_by(df_SunCountry, ResidentCity)%>%
  summarise(count= n(), pct = round(n()/nrow(df_SunCountry),2))%>%
  arrange(desc(count))%>%
  head(10)
ResidentCity

#Most of the customer reside in Minneapolis, followed by Texas and New York areas
#Further examine are there any differences across clusters in terms of resident regions
ResidentCity_S1 <- group_by(df_s1, ResidentCity)%>%
  summarise(count= n(), pct = round(n()/nrow(df_SunCountry),2))%>%
  arrange(desc(count))%>%
  head(10)
ResidentCity_S1

ResidentCity_S2 <- group_by(df_s2, ResidentCity)%>%
  summarise(count= n(), pct = round(n()/nrow(df_SunCountry),2))%>%
  arrange(desc(count))%>%
  head(10)
ResidentCity_S2

ResidentCity_S3 <- group_by(df_s3, ResidentCity)%>%
  summarise(count= n(), pct = round(n()/nrow(df_SunCountry),2))%>%
  arrange(desc(count))%>%
  head(10)
ResidentCity_S3

ResidentCity_S4 <- group_by(df_s4, ResidentCity)%>%
  summarise(count= n(), pct = round(n()/nrow(df_SunCountry),2))%>%
  arrange(desc(count))%>%
  head(10)
ResidentCity_S4

ResidentCity_S5 <- group_by(df_s5, ResidentCity)%>%
  summarise(count= n(), pct = round(n()/nrow(df_SunCountry),2))%>%
  arrange(desc(count))%>%
  head(10)
ResidentCity_S5

#Do they book round trips or not
ggplot(df_SunCountry,aes(x=Segment,fill=RoundTrip))+ geom_bar(position = 'fill')+ scale_fill_brewer(palette="Oranges")
#Round trip is too few so this chart does not return anything 

#Membership
ggplot(df_SunCountry,aes(x=Segment,fill=UflyMemberStatus))+ geom_bar(position = 'fill')+ scale_fill_brewer(palette="Oranges")
#Round trip is too few so this chart does not return anything 

#Is age potentially related to membership status? 
#To make the plot intuitively to understand, we condense the age groups from 8 into 4 here
ggplot(df_SunCountry,aes(x=UflyMemberStatus,fill=AgeGroup))+geom_bar(position = 'stack')+ scale_fill_brewer(palette="Oranges")
#Doesn't seem so - the distributions of all age groups between member &non-member look identical
 
#Booking Channel
ggplot(df_SunCountry,aes(x=Segment,fill=BookingChannel))+ geom_bar(position = 'fill')+ scale_fill_brewer(palette="Oranges")

#Travel time
ggplot(df_SunCountry,aes(x=Segment,fill=Qtr))+ geom_bar(position = 'fill')+ scale_fill_brewer(palette="Oranges")

#Days booked in advance 
df_SunCountry$Segment<-as.factor(df_SunCountry$Segment)
ggplot(df_SunCountry, aes(x = Segment, y = DaysInAdvanceBooked)) +            # Applying ggplot function
  geom_boxplot(color="red", fill="orange", alpha=0.2)
#From the chart it does not seem like each group differentiates a lot in terms of how many days in advance a trip gets booked

ggplot(df_s1, aes(x = Segment, y = DaysInAdvanceBooked)) +            # Applying ggplot function
  geom_boxplot(color="red", fill="orange", alpha=0.2)
quantile(df_s1$DaysInAdvanceBooked, c(.25, .50, .75)) 
#Cluster 1: 14-56 days

ggplot(df_s2, aes(x = Segment, y = DaysInAdvanceBooked)) +            # Applying ggplot function
  geom_boxplot(color="red", fill="orange", alpha=0.2)
quantile(df_s2$DaysInAdvanceBooked, c(.25, .50, .75)) 
#Cluster 2: 30-97 days

ggplot(df_s3, aes(x = Segment, y = DaysInAdvanceBooked)) +            # Applying ggplot function
  geom_boxplot(color="red", fill="orange", alpha=0.2)
quantile(df_s3$DaysInAdvanceBooked, c(.25, .50, .75)) 
#Cluster 3: 17-117 days

ggplot(df_s4, aes(x = Segment, y = DaysInAdvanceBooked)) +            # Applying ggplot function
  geom_boxplot()
geom_boxplot(color="red", fill="orange", alpha=0.2)
quantile(df_s4$DaysInAdvanceBooked, c(.25, .50, .75)) 
#Cluster 4: 15 - 66 days

ggplot(df_s5, aes(x = Segment, y = DaysInAdvanceBooked)) +            # Applying ggplot function
  geom_boxplot()
geom_boxplot(color="red", fill="orange", alpha=0.2)
quantile(df_s5$DaysInAdvanceBooked, c(.25, .50, .75)) 
#Cluster 5: 28-86 days


#Most common ticket fare range by segments 
df_SunCountry$Segment<-as.factor(df_SunCountry$Segment)
ggplot(df_SunCountry, aes(x = Segment, y = amt)) +            # Applying ggplot function
  geom_boxplot(color="red", fill="orange", alpha=0.2)
#So cluster 3 purchases more expensive tickets than the other groups 
#Getting individual segment price range 
ggplot(df_s1, aes(x = Segment, y = amt)) +            # Applying ggplot function
  geom_boxplot(color="red", fill="orange", alpha=0.2)
quantile(df_s1$amt, c(.25, .50, .75)) 
#Cluster 1: the most common price range is between $171 - 336 

ggplot(df_s2, aes(x = Segment, y = amt)) +            # Applying ggplot function
  geom_boxplot(color="red", fill="orange", alpha=0.2)
quantile(df_s2$amt, c(.25, .50, .75)) 
#Cluster 2: the most common price range is between $278 - 358

ggplot(df_s3, aes(x = Segment, y = amt)) +            # Applying ggplot function
  geom_boxplot(color="red", fill="orange", alpha=0.2)
quantile(df_s3$amt, c(.25, .50, .75)) 
#Cluster 3: the most common price range is between $581 - 756, there are tons of outliers in this segment too

ggplot(df_s4, aes(x = Segment, y = amt)) +            # Applying ggplot function
  geom_boxplot()
geom_boxplot(color="red", fill="orange", alpha=0.2)
quantile(df_s4$amt, c(.25, .50, .75)) 
#Cluster 4: the most common price range is between $14 - 198

ggplot(df_s5, aes(x = Segment, y = amt)) +            # Applying ggplot function
  geom_boxplot()
geom_boxplot(color="red", fill="orange", alpha=0.2)
quantile(df_s5$amt, c(.25, .50, .75)) 
#Cluster 5: the most common price range is between $298 - 424

#Part II: Modelling: Develop linear regression models to further understand the differences between segments 
linear_model <-  amt ~ BookingChannel + UflyMemberStatus + AgeGroup + RoundTrip + ResidentCity + Qtr + DaysInAdvanceBooked

#Cluster 1
cluster1 <- lm(linear_model, data=df_s1[,3:10])
summary(cluster1)
#Cluster 2
cluster2 <- lm(linear_model, data=df_s2[,3:10])
summary(cluster2)
#Cluster 3
cluster3 <- lm(linear_model, data=df_s3[,3:10])
summary(cluster3)
#Cluster 4
cluster4 <- lm(linear_model, data=df_s4[,3:10])
summary(cluster4)
#Cluster 5
cluster5 <- lm(linear_model, data=df_s5[,3:10])
summary(cluster5)

#Findings from Linear Regressions
#The ticket fare of Cluster 3 is NOT impacted by the booking channel, 
#which suggests that Sun Country Airline can promote new channels to this customer segment as long as the interface is easy to navigate. 
#Moreover, when it comes to Cluster 4, the Ufly membership does not have an impact on the final ticket price. 
#This finding suggests that when promoting the Ufly program, other segments besides this can be prioritized first. 