library(ggplot2)
library(scales)
library(ggthemes)
library(grid)
library(lubridate)
library(dplyr)

# Let's load up some data

dailyactivity <- read.csv("~/Documents/RCode/Data/dailyactivity.csv")
minutesteps <- read.csv("~/Documents/RCode/Data/minutesteps.csv")
minuteintensity <- read.csv("~/Documents/RCode/Data/minuteintensity.csv")


# Have to clean up date/time 
# I like using the lubridate package as it seems to be the most versatile for these jobs

library(lubridate)

dailyactivity$date <- mdy(dailyactivity$ActivityDate) # Daily file date fix
minutesteps$datetime <- mdy_hms(minutesteps$ActivityMinute) # Minute Steps date fix
minutesteps$date <- as.Date(minutesteps$datetime)
minutesteps$time <- format(as.POSIXct(strptime(minutesteps$datetime, "%Y-%m-%d  %H:%M:%S",tz="")) ,format = "%H:%M")
minutesteps$time <- as.POSIXct(minutesteps$time, format = "%H:%M")

## Intensity data is inconsistent. Will update this when I access full data ##

#minuteintensity$datetime <- mdy_hms(minuteintensity$ActivityMinute) # Minute Intensity date fix

#minuteactivity <- merge(minutesteps, minuteintensity, by=c("datetime")) #merge steps/intensity files

# create seperate date and minute vectors (good for viz stuff)
#minuteactivity$date <- as.Date(minuteactivity$datetime)
#minuteactivity$time <- format(as.POSIXct(strptime(minuteactivity$datetime, "%Y-%m-%d  %H:%M:%S",tz="")) ,format = "%H:%M")
#minuteactivity$time <- as.POSIXct(minuteactivity$time, format = "%H:%M")

#minuteactivity <- minuteactivity[c(-2,-6)] # remove old "ActivityMinute" vectors


# Steps Per Year
steps.year <- ggplot(dailyactivity, aes(year, TotalSteps)) +
  geom_bar(stat="identity", fill="#3ec1c1") +
  scale_x_continuous(breaks = 2011:2016) +
  scale_y_continuous(labels = comma) +
  labs(title='Steps Per Year', x='Year', y='Fitbit Steps') +
  theme_fivethirtyeight()

steps.year

# How many days?

dailyactivity$dataday <- ifelse(dailyactivity$TotalSteps > 0, 1, 0) # if more than 0 steps, then it's a step day



# let's summarize data
years <- group_by(dailyactivity, year)
per.year <- summarize(years, 
                      days = sum(dataday),   
                      steps = sum(TotalSteps),
                      maxsteps = max(TotalSteps),
                      minsteps = min(TotalSteps)
                      
)

# days per year plot

wear.days <- ggplot(days.per.year, aes(year, days)) +
  geom_bar(stat="identity", fill="#3ec1c1") +
  scale_x_continuous(breaks = 2011:2016) +
  scale_y_continuous(labels = comma) +
  labs(title='Number of Days Wearing a Fitbit', x='Year', y='Number of Days') +
  geom_hline(aes(yintercept=365), color="#343233", linetype="dashed") +
  theme_fivethirtyeight()

wear.days

# consecutive days

dailyactivity$dummywear <- 1
consistent <- ggplot(dailyactivity, aes(date, dummywear, fill=as.factor(dataday))) +
  geom_bar(stat="identity") +
  scale_fill_manual(values=c("#343233", "#3ec1c1"), name="Did I Wear It?", breaks=c("0", "1"), labels=c("Not Wearing", "Wearing")) +
  scale_x_date(breaks=date_breaks("3 month"), date_labels = "%m/%d/%y") +
  scale_y_continuous(breaks=NULL, labels = NULL, name = "") +
  labs(title='Consitency of Wearing a Fitbit', x='Date') +
  theme_fivethirtyeight() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

consistent

# So how many days have I actually worn it?

(max(dailyactivity$date) - min(dailyactivity$date)) - sum(days.per.year$days) # number of nonwear days
sum(days.per.year$days) /  as.numeric(max(dailyactivity$date) - min(dailyactivity$date)) # wearing to total days

## Time series data

daily.steps.bar <- ggplot(dailyactivity, aes(date, TotalSteps)) +
  geom_bar(stat="identity", fill="#3ec1c1") +
  scale_x_date(breaks=date_breaks("3 month"), date_labels = "%m/%d/%y") +
  labs(title='1941 Days Of Fitbit', x='Date', y="Steps per Day") +
  theme_fivethirtyeight() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
daily.steps.bar


# a lot of data so let's break it up by year

# create "month" variable
dailyactivity$month <- format(dailyactivity$date, format ="%B")
dailyactivity$month <- factor(dailyactivity$month, levels=c("January", "February", "March", "April", "May", "June", "July",
                                                            "August", "September", "October", "November", "December"))

daily.steps.bar.fg <- ggplot(dailyactivity, aes(month, TotalSteps)) +
  geom_bar(stat="identity", fill="#3ec1c1") +
  labs(title='1941 Days Of Fitbit (Steps per Month)') +
  scale_y_continuous(labels = comma) +
  theme_fivethirtyeight() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  facet_grid(year ~.) +
  theme(panel.margin = unit(1, "lines"))
daily.steps.bar.fg

# breaking it up by month and day

dailyactivity$monthday <- as.POSIXct(format(dailyactivity$date, format="%m/%d"), format="%m/%d")
daily.steps.bar.monthday.fg <- ggplot(dailyactivity, aes(monthday, TotalSteps)) +
  geom_bar(stat="identity", position="identity", fill="#3ec1c1") +
  scale_y_continuous(labels = comma) +
  scale_x_datetime(breaks=date_breaks("1 month"), labels=date_format("%b - %d")) +
  theme_fivethirtyeight() +
  facet_grid(year ~.) +
  theme(panel.margin = unit(1, "lines")) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title="1941 Days Of Fitbit (Day of the Year)")
  
daily.steps.bar.monthday.fg


# Create weekday variable
dailyactivity$weekday <- weekdays(dailyactivity$date) # we use labels=true here to return day labels instead of numeric values. 
dailyactivity$weekday <- factor(dailyactivity$weekday, levels=c("Monday", "Tuesday", "Wednesday", "Thursday",
                                                                "Friday", "Saturday", "Sunday"))
daily.steps.bar.weekday.fg <- ggplot(dailyactivity, aes(weekday, TotalSteps)) +
  geom_bar(stat="identity", fill="#3ec1c1") +
  scale_y_continuous(labels = comma) +
  theme_fivethirtyeight() +
  facet_grid(year ~.) +
  theme(panel.margin = unit(1, "lines")) +
  labs(title="1941 Days Of Fitbit (Day of the Week)")
daily.steps.bar.weekday.fg 

#goals
dailyactivity$goalday <- (ifelse(dailyactivity$TotalSteps >= 10000, 1, 0))
sum(dailyactivity$goalday) / sum(dailyactivity$dataday)
sum(dailyactivity$dataday)

goal.plot <- ggplot(dailyactivity, aes(monthday, TotalSteps, fill=goalday)) +
  geom_bar(stat="identity") +
  scale_y_continuous(labels = comma) +
  scale_x_datetime(breaks=date_breaks("1 month"), labels=date_format("%b - %d")) +
  scale_fill_manual(values=c("#343233", "#3ec1c1"), name="Did I hit 10,000 Steps?", 
                    breaks=c("0", "1"), labels=c("No", "Yes")) +
  theme_fivethirtyeight() +
  facet_grid(year ~.) +
  theme(panel.margin = unit(1, "lines")) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title='When Do I Meet My Step Goal?')

goal.plot

#cumulative sum

dailyactivity$cumulative <- cumsum(dailyactivity$TotalSteps)

cumulativesteps <- ggplot(dailyactivity, aes(date, cumulative)) +
  geom_point( fill="#3ec1c1") +
  scale_x_date(breaks=date_breaks("2 month"), date_labels = "%m/%d/%y") +
  scale_y_continuous(labels = comma) +
  labs(title='1941 Days Of Fitbit (Cumulative Steps)') +
  theme_fivethirtyeight() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
cumulativesteps



## Minute patterns

# big long plots are fun
minute.line.long <- ggplot(minutesteps, aes(x=datetime, y=Steps)) +
  geom_point(size=.3, colour="#3ec1c1") +
  scale_x_datetime(breaks=date_breaks("1 month"), date_labels = "%m/%d/%y") +
  labs(title='All of the Steps') +
  theme_fivethirtyeight() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) 
minute.line.long

# one plot for all days
# lines
minute.lines <-ggplot(minutesteps, aes(x=time, y=Steps, group=date)) + 
  geom_path(size=.5, alpha = 0.03, colour="#3ec1c1") + 
  scale_x_datetime(breaks=date_breaks("2 hour"), labels=date_format("%H:%M" ,tz=""))+
  theme_fivethirtyeight() +
  labs(title="Steps per Minute (All Days)")
minute.lines

# scatter plot
minute.points <- ggplot(minutesteps, aes(time, Steps, group=date)) +
  geom_point(size=.5, alpha = 0.1, colour="#3ec1c1") + 
  scale_x_datetime(breaks=date_breaks("2 hour"), labels=date_format("%H:%M", tz="")) +
  theme_fivethirtyeight() +
  labs(title="Steps per Minute (All Days)")
minute.points

#Summarise to find mean steps per minute
minutes <- group_by(minutesteps, time)
minutesmorezero <- subset(minutes, Steps >0)
timeprofile<- summarize(minutes, 
                       mean = mean(Steps),
                       median = median(Steps),
                       q10 = quantile(Steps, c(.10)),
                       q25 = quantile(Steps, c(.25)),
                       q75 = quantile(Steps, c(.75)),
                       q90 = quantile(Steps, c(.90))
                                                     
                )

#plot the avereage day profile
MeanDay <- ggplot(timeprofile, aes(time, mean)) +
  geom_point(color="#3ec1c1") +
  scale_x_datetime(breaks=date_breaks("2 hour"), labels=date_format("%H:%M", tz="")) +
  theme_fivethirtyeight() +
  labs(title="The Mean Day")
MeanDay


minutesteps$Day <- wday(minutesteps$date, label = TRUE) 
minutesteps$Day2 <- factor(minutesteps$Day, levels = c("Mon", "Tues", "Wed", "Thurs", "Fri", "Sat", "Sun"))

# faceted minute point plots
minutes.point.day <- ggplot(minutesteps, aes(x=time, y=Steps, colour=Day2)) + 
  geom_point(size=.5, alpha = 0.1) + 
  scale_x_datetime(breaks=date_breaks("2 hour"), labels=date_format("%H:%M", tz="")) +
  facet_grid(Day2 ~.) +
  theme_fivethirtyeight() +
  labs(title='All of the Steps (by Day)') +
  theme(legend.position="none") 
minutes.point.day


# faceted aggregated plot

minutesteps.avgdays <- aggregate(Steps ~ time + Day2, minutesteps, mean)

agg.plots<- ggplot(minutesteps.avgdays, aes(x=time, y=Steps, colour=Day2)) + 
  geom_path() + 
  scale_x_datetime(breaks=date_breaks("2 hour"), labels=date_format("%H:%M", tz="")) +
  facet_grid(Day2 ~.) +
  theme_fivethirtyeight() +
  labs(title="The Mean Days") +
  theme(legend.position="none") 
agg.plots


# create new data set to show when activity happens
minutesteps.simple <- minutesteps
minutesteps.simple$yes.steps <- ifelse(minutesteps.simple$Steps>0, 1, 0) #label observations that have steps
minutesteps.simple <- subset(minutesteps.simple, yes.steps > 0)  #only keep observations with >0 steps

#diurnal plot with all steps
diurnal <- ggplot(minutesteps.simple, aes(as.POSIXct(date), time)) +
  geom_point(size=.5, alpha = 0.2, color="#3ec1c1") +
  scale_y_datetime(breaks=date_breaks("2 hour"), labels=date_format("%H:%M", tz="")) +
  scale_x_datetime(breaks=date_breaks("2 month")) +
  theme_fivethirtyeight() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title="Stepping Through Time")
  
diurnal
  


## Not used in post ##


# let's try the ggTimeSeries calendar heatmap
library(ggplot2)
library(ggTimeSeries)
calendar.heatmap = ggplot_calendar_heatmap(subset(dailyactivity, TotalSteps >0), 'date', 'TotalSteps') +
  xlab(NULL) + 
  ylab(NULL) + 
  scale_fill_continuous() + 
  theme(
    axis.text = element_blank(), 
    axis.ticks = element_blank(), 
    legend.position = 'none', 
    strip.background = element_blank(), 
    # strip.text = element_blank(), # useful if only one year of data \cr plot.background = element_blank(), 
    panel.border = element_blank(), 
    panel.background = element_blank(), 
    panel.grid = element_blank(), 
    panel.border = element_blank() 
  ) 
