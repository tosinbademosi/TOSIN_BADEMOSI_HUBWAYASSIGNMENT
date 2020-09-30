

# Read in hubway_trips.csv to R
trip <- read.csv("hubway_trips.csv")
#1. This file is quite large. (And we've already removed move than 75% of the data!) How many rows and columns does 
#it have?
dim(trip)
# 2. How many unique user zip codes are in this dataset?
length(unique(trip$zip_code))
#   3. How many unique bicycles are in this dataset?
length(unique(trip$bike_nr))
#   4. Calculate the count of rides for each unique bicycle.
bike_ride <- table(trip$bike_nr)
which.max(bike_ride)
#create data table for this data set.
table(trip$bike_nr)
bike_ride <-table(trip$bike_nr) 
# Do numbers 5-7 in pairs.
# 5. Which bicycle is ridden most frequently?
which.max(bike_ride)
# 6. Which bicycle is ridden least frequently
which.min(bike_ride)
# 7. Calculate the total duration of all rides for each bicycle. Hint: tapply() or aggregate()
aggregate(trip$duration,by=list(trip$bike_nr),FUN=sum)
dur_aggr <-aggregate(trip$duration,by=list(trip$bike_nr),FUN=sum)
# Check in: share and compare your answers.
# Let’s do #8-10 together
# 8. Which bicycle has been ridden for the longest total duration in this dataset?
dur_aggr[which.max(dur_aggr$x),]
#Shortest total duration?
dur_aggr[which.min(dur_aggr$x),] 
#   9. Consider only trips on the bicycle that has been ridden for the longest duration, 
#which station is its most frequent end station?
max_tripby<- subset(trip,bike_nr=='B00585')
maxtrip_table <- table(max_tripby$end_statn)
View(maxtrip_table)
which.max(maxtrip_table)
#Which station is its most frequent start station? Return the station ids.
max2_tripby<- subset(trip,bike_nr=='B00585')
max2trip_table <- table(max2_tripby$strt_statn)
View(max2trip_table)
which.max(max2trip_table)
# 10. Look up the name of the above station ids in the stations data frame.
station_name <- station[(station$id==22),]
#South Station - 700 Atlantic Ave.	

#What are the names of the most frequent start station for this bicycle?
max2 <- table(max_tripby$strt_statn)
which.max(max2)
#South Station - 700 Atlantic Ave

##The end statiion most frequent
max1 <- table(max2_tripby$end_statn)
View(max1)
which.max(max1)
#South Station - 700 Atlantic Ave.
##A. Merge the trip and station data
##Let’s do the first merge together, and then try the second one on your own:
 ## 1. Use ​merge​ (twice) to append the appropriate station names to the trips data frame. Add a column for the 
#starting station and the ending station. See ?merge.
merged_data <- merge(trip, station_existing,by.x = "strt_statn", by.y = "id")
merged_data1 <- merge(merged_data, station_existing,by.x = "end_statn", by.y = "id")
summary(merged_data1)

#Look at the summary of this new data frame. Are all of the stations "existing"? 
  yes.
 # Why is that? Did you merge using your 
#hubway station data frame with only existing stations?
  yes.
  
  #Or the data frame with all stations? Remove all trips to or from a "removed" station if necessary.
#2. Use names() or colnames() to clean up the column names in the merged data frame.
#colnames(merged_data1)$end_statn="end station")
names(merged_data1)[names(merged_data1) == "end_statn"] <- "end_station"
names(merged_data1)[names(merged_data1) == "station_station"] <- "start_station"
#Do #3-7 in pairs
#3. Which station is most frequently used as a starting station?
#most frequent start station is id =22
most_used <- table(merged_data1$start_station)
which.max(most_used)
View(most_used)


_____________________________________________________________________________________________________________________
#February 2020
#4. Which station is most frequently used as an ending station?
#the most frequent end station is id=22
most_used1 <-table(merged_data1$end_station) 
which.max(most_used1)

#  5. What is the name of the starting station with the longest average trip duration? Hint: tapply or aggregate
  long_dur <- aggregate(merged_data1$duration,by=list(merged_data1$start_station),FUN=mean)
  long_dur[which.max(long_dur$x),]
 merged_long <- subset(merged_data1,start_station==14)
 #HSPH - Ave. Louis Pasteur at Longwood Ave.	
 
  
            
#6. What is the name of the starting station with the shortest average trip duration?
long_dur[which.min(long_dur$x),]
merged_long1 <- subset(merged_data1,start_station==67)
  	#MIT at Mass Ave / Amherst St	

 # 7. What is the name of the ending station with the longest average trip duration?

long_dur1 <- aggregate(merged_data1$duration,by=list(merged_data1$end_station),FUN=mean)
long_dur1[which.max(long_dur$x),]
merged_long2 <- subset(merged_data1,end_station==14)
#HMS / HSPH - Ave. Louis Pasteur at Longwood Ave.	

#and shortest average trip duration?
long_dur1[which.min(long_dur$x),]
dur_aggr[which.max(dur_aggr$x),]
merged_long3<- subset(merged_data1,end_station==67)
#MIT at Mass Ave / Amherst St	
  
#Complete the rest of the lab on your own, and turn it in Tuesday as your homework.
#8. Which user zipcode is associated most frequently with trips ending at the station with 
#the longest average trip duration?

trip_zip <- table(merged_long2$zip_code)
which.max(trip_zip)
#02215

  #9. Create a table showing the number of trips by gender. Plot this table using ​barplot​. 
 # The argument for barplot​ is the table itself. What percent of total trips were completed by males?
trip_gender <- table(merged_data1$gender)
View(trip_gender)
barplot(trip_gender)

 # 10. Use ​hist​ with the vector for trip duration as the only argument to plot the histogram of trip durations. 
 # What do you notice?
table_his <- merged_data1$duration
hist(table_his)
#its has only one bar that represent the durtions.
 # _____________________________________________________________________________________________________________________
#February 2020

# 11. Use ​hist​ with the vector for trip duration, considering only trips less than an hour to plot the 
# histogram of trip durations under an hour.
# 12. Based on what you see in the data (and on any other analysis you want to try), make some recommendations 
# for the following groups:
#   ● The Hubway/Bluebike team
# ● The MBTA (Boston’s public transportation authority)
# ● The Boston Public Health Commission
# ● A real estate developer looking to build a new apartment building
# ● Someone who lives near the Museum of Fine Arts (465 Huntington Avenue, Boston MA) who just
# learned to ride a bike and wants to ride more frequently
# Think: what questions would this person or organization have about this data? And what analysis can you do to 
#give them insights about those questions? These answers can just be typed out, no need for visualizations or 
#slide decks yet.





