df <- read.csv("Bike_Rental_Dataset.csv", header=TRUE, sep=",")
View(df)

install.packages("ggplot2")
install.packages("lattice")
install.packages("lubridate")
install.packages("scales")
install.packages("plyr")
install.packages("readr")

library(ggplot2)
library(lattice)
library(lubridate)
library(scales)
library(plyr)
library(readr)

xyplot(cnt ~ hum|weather, data=df)

df$season  <- factor(df$season, labels = c("Spring", "Summer", "Fall", "Winter"))
df$weather <- factor(df$weather, labels = c("Good", "Normal", "Bad", "Very Bad"))
df$hour  <- df$hr
df$times <- as.POSIXct(strftime(ymd_hms(train$datetime), format="%H:%M:%S"), format="%H:%M:%S")
df$Weekday <- wday(ymd_hms(df$datetime), label=TRUE)

weather_summary <- ddply(df,.(weather,hour),
                         summarise, cnt = mean(cnt))

temp_summary <- ddply(df,.(weather,atemp),
                         summarise, cnt = mean(cnt))

season_summary <- ddply(df,.(season,hour),
                      summarise, cnt = mean(cnt))

season_temp_summary <- ddply(df,.(season,temp),
                        summarise, cnt = mean(cnt))

## ===================== Generates a plot for hour/weather ===================== ##
ggplot(df, aes(x = hour, y = cnt, colour = weather)) +
  geom_point(data = weather_summary, aes(group = weather)) +
  geom_line(data = weather_summary, aes(group = weather)) +
  scale_x_discrete("Hour", limits=0:23) +
  scale_y_continuous("Count") +
  theme_minimal() +
  ggtitle("Average hourly rentals per weather condition\n") + 
  theme(plot.title=element_text(size=18),
  panel.grid.major = element_blank(),
  panel.grid.minor = element_blank(),
  axis.line = element_line(color = 'black'),
  panel.background = element_blank())

## ===================== Generates a plot for temp/weather ===================== ##
ggplot(df, aes(x = atemp, y = cnt, colour = weather)) +
  geom_point(data = temp_summary, aes(group = weather)) +
  geom_line(data = temp_summary, aes(group = weather)) +
  scale_x_continuous("Feeling Temperature (Normalised)") +
  scale_y_continuous("Count") +
  theme_minimal() +
  ggtitle("Count and feeling temperature per weather condition\n") + 
  theme(plot.title=element_text(size=18),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line(color = 'black'),
        panel.background = element_blank())

## ===================== Generates a plot for hour/season ===================== ##
ggplot(df, aes(x = hour, y = cnt, colour = season)) +
  geom_point(data = season_summary, aes(group = season)) +
  geom_line(data = season_summary, aes(group = season)) +
  scale_x_discrete("Hour", limits=0:23) +
  scale_y_continuous("Count") +
  theme_minimal() +
  ggtitle("Average hourly rentals per season\n") + 
  theme(plot.title=element_text(size=18),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line(color = 'black'),
        panel.background = element_blank())

## ===================== Generates a plot for temp/season ===================== ##
ggplot(df, aes(x = temp, y = cnt, colour = season)) +
  geom_point(data = season_temp_summary, aes(group = season)) +
  geom_line(data = season_temp_summary, aes(group = season)) +
  scale_x_continuous("Temp") +
  scale_y_continuous("Count") +
  theme_minimal() +
  ggtitle("Rental count/temperature per season\n") + 
  theme(plot.title=element_text(size=18),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line(color = 'black'),
        panel.background = element_blank())
