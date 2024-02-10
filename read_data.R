#load libraries
library(tidyverse)
library(dplyr)
library(janitor)
library(lubridate)
library(ggplot2)
library(data.table)


#create a variable to save the zipfile
zipfile <- "activity.zip"
data_out <- "data_unzip"
#unzip the file
unzip(zipfile, exdir = data_out)

#watch files 
list.files("data_unzip/")



#read the data and watch dimesion and structura
df <- read.table("data_unzip/activity.csv",stringsAsFactors=FALSE, header = TRUE, sep = ",")
head(df)
dim(df)
str(df)

#modify the column date from string to data type
df$date <- as.Date(df$date, format = "%Y-%m-%d")

#verify the modification
head(df)
str(df)

#add a column of weekdays
df <- mutate(df,week_day = wday(df$date))

head(df)
str(df)



#df <- na.omit(df)
head(df)
dim((df))
df1 <- df %>%
  group_by(week_day) %>%
  summarise(sum(steps), na.rm =TRUE)

print(df1)
dim(df1)


head(df)
Sys.setlocale("LC_TIME", "English")

sort(df1$`df$week_day`)
ordern <- c("lunes", "martes", "miércoles", "jueves","viernes", "sábado", "domingo")
head(df1)

df1 <- df1 %>% arrange(factor(week_day, levels = ordern))

df1$week_day <- as.factor(df1$week_day)


df$steps <- as.factor(df$steps)
hist(df$week_day, df$steps)

str(df)


mean(df$steps)

#obtain the mean by day
mean_steps_day <- df %>%
  group_by(week_day) %>%
  summarise(mean_steps = sum(steps)/n_distinct(date))

mean_steps_day

#plot the mean steps by day
ggplot(data = mean_steps_day, aes(x = week_day, y = mean_steps))+
  geom_col()+
  title("Mean of steps by day of week")+
  xlab("Days")+
  ylab("Mean steps")


#sum the total steps by day
total_steps <- df %>%
  group_by(date) %>%
  summarise(total_steps = sum(steps), na.rm=TRUE)
#plot a histogram the total steps by date
hist(total_steps$total_steps, breaks = 10, col="red",xlab = "Total steps taken per  day", main = "Histogram Total Steps")

head(total_steps)
print((total_steps$total_steps, na.rm = TRUE))
print(mean(total_steps$total_steps, na.rm = TRUE))
print(median(total_steps$total_steps,na.rm = TRUE))


avg_day <- aggregate(df$steps, by = list(df$interval), 
                     FUN = mean, na.rm= TRUE)

plot(avg_day$Group.1, avg_day$x, type= "l" , col = "blue", xlab= "Interval", ylab= "Avg Number of steps", main = "Averange of steps by intervals of 5 min")

avg_day[which.max(avg_day$x),]$Group.1

sum(is.na(df$steps))

df_not_na <- df

m <-df_not_na$interval[10]
class(m[1])
#replace the NA by the mean in the intervals
p <- 0
for (n in 1:61){
  for (m in 1:288){
    p <- p+1
    if (is.na(df_not_na$steps[p])){
      df_not_na$steps[p] <- avg_day$x[m]
      }
  }   
}
# verify that doesnt exist any NA after the replacement
sum(is.na(df_not_na))

total_steps_ot_na <- df_not_na %>%
  group_by(date) %>%
  summarise(total_steps = sum(steps), na.rm=TRUE)

hist(total_steps_ot_na$total_steps, breaks = 20)

mean(total_steps_ot_na$total_steps)
mean(total_steps$total_steps, na.rm = TRUE)
median(total_steps_ot_na$total_steps)
median(total_steps$total_steps, na.rm = TRUE)


workday <-  subset(df_not_na,week_day >1 & week_day <7 )
weekend <- subset(df_not_na, week_day ==1 | week_day ==7)


head(workday)
dim(workday)
head(weekend)
dim(weekend)


avg_work <- aggregate(workday$steps, by = list(workday$interval), 
                     FUN = mean)

avg_weekend <- aggregate(weekend$steps, by = list(weekend$interval), 
                     FUN = mean)


par(mfrow = c(2,1))
plot(avg_work$Group.1, avg_work$x, type= "l" , col = "blue", xlab= "Interval", ylab= "Avg Number of steps", main = "Averange of steps by intervals of 5 min Monday to friday")
plot(avg_weekend$Group.1, avg_weekend$x, type= "l" , col = "red", xlab= "Interval", ylab= "Avg Number of steps", main = "Averange of steps by intervals of 5 min Wekend")



