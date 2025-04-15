library(lubridate)
library(ggplot2)
library(dplyr)

df1 <- read.csv('Divvy_trips_2019_Q1.csv')


###### browse data ######
str(df1)
summary(df1)


###### cast data type ######
#  - start_time / end_time -> datetime
#  - tripduration -> float
df1$start_time <- as_datetime(df1$start_time)
df1$end_time <- as_datetime(df1$end_time)
df1$tripduration <- as.numeric(gsub(",", "", df1$tripduration))


###### null check ######
colSums(is.na(df1))
df1[!complete.cases(df1),]
# null row ratio
nrow(df1[!complete.cases(df1),]) / nrow(df1)
# not 100% confident, but probably should remove all rows with missing values
df1 <- na.omit(df1)
df1 <- df1[df1$gender != '',]

###### duplicate check ######
any(duplicated(df1$trip_id))
# -> no duplicates in trip_id

###### outlier check ######
summary(df1)

# obviously, there are some mistyped values in tripduration
ggplot(df1) +
  geom_boxplot(aes(y = tripduration))
# it's hard to determine the border between the normal values and anomalies
# for now, I treat values more than the 99th quantile as outliers
df1 <- df1[df1$tripduration <= quantile(df1$tripduration, 0.99),]

table(df1$usertype) # 94% subscriber
table(df1$gender) # 81% male (a little bit biased data set?)

# treat birthyears before 100 or more year ago as outliers
df1 <- df1[df1$birthyear > 1919,]

# add a column for the day of the week
df1$day_of_week <- wday(df1$start_time)

###### analyze ######
summary(df1)

# how tripduration differs by the user type
ggplot(df1, aes(x = usertype, y = tripduration)) +
  geom_boxplot()
# -> the tripdurtion of the subscribers tend to be shorter

# how tripduration average differs by the day of the week and the user type
df1 %>%
  group_by(day_of_week, usertype) %>%
  summarise(avg_tripduration = mean(tripduration)) %>%
  ggplot(aes(x = day_of_week, y = avg_tripduration)) +
  geom_col() +
  facet_wrap(~usertype)
# -> Subscribers tend to ride a bike for almost the same amount of time throughout the week.
# -> Customers ride a bike more on weekends than weekdays, and trip longer distance
#    than subscribers.
# -> From these facts, we can assume those who need to ride a bike on a regular basis every day
#    tend to subscribe.

# the distributions of birth year by user type
ggplot(df1, aes(x = usertype, y = birthyear)) +
  geom_boxplot(aes(color = gender))
# -> Subscribers are older than Customers.
#    Probably those who need a bike to commune or just for daily necessities tend to be subscribers.
#    Customers ride a bike with a longer distance. One reason for this may be because many of them are travelers (not 100% sure).

# Prospective subscribers: 
# Those who live in Chicago, with the age of 30~45, and need to ride a bike with a short amount of time a day but every day


########################### add 2020 Q1 data ###########################

df2 <- read.csv('Divvy_trips_2020_Q1.csv')

# make the 2020 data consistent to the 2019 data
df2 <- rename(df2, start_time = started_at)
df2$start_time <- as_datetime(df2$start_time)
df2 <- rename(df2, end_time = ended_at)
df2$end_time <- as_datetime(df2$end_time)
df2 <- rename(df2, usertype = member_casual)
df2[df2$usertype == 'casual',]$usertype <- "Customer"
df2[df2$usertype == 'member',]$usertype <- "Subscriber"
df2$tripduration <- as.numeric(df2$end_time - df2$start_time)
df2$day_of_week <- wday(df2$start_time)

###### null check ######
colSums(is.na(df2))
df2[!complete.cases(df2),]
# remove all rows with missing values
df2 <- na.omit(df2)

###### outlier check ######
df2 <- filter(df2, tripduration > 0)

# treat values more than the 99th quantile as outliers
df2 <- df2[df2$tripduration <= quantile(df1$tripduration, 0.99),]

# bind 2019 and 2020 data
m_df <- bind_rows(
  select(df1, day_of_week, tripduration, usertype, birthyear),
  select(df2, day_of_week, tripduration, usertype)
)

# how tripduration differs by the user type
ggplot(m_df, aes(x = usertype, y = tripduration)) +
  geom_boxplot()
# -> same as df1

# how tripduration average differs by the day of the week and the user type
m_df %>%
  group_by(day_of_week, usertype) %>%
  summarize(avg_tripduration = mean(tripduration)) %>%
  ggplot(aes(x = day_of_week, y = avg_tripduration)) +
  geom_col() +
  facet_wrap(~usertype)
# -> same as df1
