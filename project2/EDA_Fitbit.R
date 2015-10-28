# run in Assignment_2 directory
# setwd("~/Dropbox/TA_Adv_Data_Science/Assignment_2")
# goal
# (1) Identify any patterns or trends in this data over time 
# (2) Predict the missing values in the data set and evaluate how well you do that 
# (3) See if you can identify any behavioral changes in your instructor 
# and try to figure out what they are
rm(list = ls())
dts = steps = NULL
library(matrixStats) # gives colSds colMedians, etc.
library(reshape2)
library(lubridate)
library(plyr)
library(dplyr)
library(accelerometry)
library(ggplot2)
xdata = load("fitbit-data.Rdata")
xdata

###############################################
# Exploring
###############################################
rmeans = rowMeans(steps)
plot(rmeans, type="l")
cmeans = colMeans(steps)
plot(dts, cmeans, type="l")
cmaxs = colMaxs(steps)
plot(dts, cmaxs, type="l")

# vector of dates with no data at all
steps_noNA = steps
steps_noNA[ steps == 0] = NA
rmeans_noNA = rowMeans(steps_noNA, na.rm= TRUE)
no_info_days = dts[cmaxs == 0]

df = as.data.frame(steps)
############### THIS WILL TAKE OUT THE Date/Time-ness of the dts object!
colnames(df) = dts
df$minute = seq(nrow(df))


# Make data set one row per day/time
# This is so that you can look at data together in time. Aka 1201AM and 1159PM can 
# be shown to be 2 minutes apart and are close but not in the matrix form
long = melt(df, 
            variable.name = "date", id.vars = "minute")
### Turn date into a date/time object
long = mutate(long, date = ymd(date))
long$datetime = long$date
# Add the minutes to the date!
long$minute = long$minute - 1
minute(long$datetime) =  long$minute

### add day of week
long$day = wday(long$date)
long$wday = wday(long$date, label = TRUE, abbr = FALSE)
long$month = month(long$date, label=TRUE)

### weekend or weekday
#data$Weekend <- factor(ifelse(weekdays(data$Date) %in% c("Saturday", "Sunday"), "Weekend", "Weekday"))
long$wkend <- factor(ifelse(long$wday %in% c("Saturday", "Sunday"), "Weekend", "Weekday"))

long$steps = long$value
# long$steps[ long$steps == 0] = NA
# 
# long$lag_value = lag(long$value, n = 1, default = 0)
# long$lead_value = lead(long$value, n = 1, default = 0)
# 
# long$steps2 = long$value
# long$steps2[ long$lead_value == 0 & 
#               long$lag_value == 0 & 
#               long$value == 0] = NA

long$weartime = accel.weartime(long$value, 
               window = 60,
               tol = 2, 
               tol.upper = 99) > 0


good = long[ long$weartime, ]

# tapply, aggregate, split, group_by
# my favorite - ddply
# by_day = group_by(long, minute) %>%
#   summarize(mean = mean(steps, na.rm = TRUE))

day_data = ddply(good, ~ wday, summarize, 
                 mean = mean(steps, na.rm = TRUE), 
                 sd = sd(steps, na.rm = TRUE),
                 n = sum(!is.na(steps))
)
# mean number of steps per minute per day 

day_min_data = ddply(good, ~wday + minute, summarize, 
                 mean = mean(steps, na.rm = TRUE), 
                 sd = sd(steps, na.rm = TRUE),
                 n = sum(!is.na(steps))
)

## plot weartime (n) vs 

min_data = ddply(good, ~ minute, summarize, 
                 mean = mean(steps, na.rm = TRUE), 
                 sd = sd(steps, na.rm = TRUE),
                 n = sum(!is.na(steps))
                 )

min_data$hour = floor(min_data$minute /60)
min_data$min = min_data$minute %% 60
min_data$dt = min_data$hour + min_data$min / 60


### plot wear time by minute

# min_data$dt = sprintf("%02.0f:%02.0f", 
#                       min_data$hour, 
#                       min_data$min)
# min_data$dt = hm(min_data$dt)

plot(min_data$dt, min_data$mean, type="l")

#g = ggplot(aes(x = minute, y = steps, colour = as.factor(date)), 
#           data = good) + geom_line() +
#  guides(colour = FALSE)
#g

g = ggplot(aes(x = dt, y = mean), data = min_data) + geom_line() +
  geom_smooth(method = "loess", 
              span = 0.1, 
              se=FALSE)
g
### is he wearing it when he sleeps???


mod = gam( mean ~ s(dt, bs = "cs"), 
     data = min_data, 
     weights = min_data$n)
p = predict(mod)



################################
#
################################
good$hour = floor(good$minute /60)
good$min = good$minute %% 60
good$dt = good$hour + good$min / 60
good_mod = gam( steps ~ s(dt, bs = "cs"), 
           data = good )
p = predict(mod)
good$full_gam_pred = predict(good_mod, newdata = good)

g2 = good
g2 = arrange(g2, dt, full_gam_pred)
plot(g2$dt, g2$full_gam_pred, type="l")

plot(min_data$dt, p, type = "l")
min_data$pred = p

g = ggplot(aes(x = dt, y = mean), data = min_data) + geom_line() +
  geom_line(aes(y = pred), col= "red") + geom_smooth(se = FALSE)
g

g = ggplot(aes(x = dt, y = n), data = min_data) + geom_line() 
g

# good = filter(long, !date %in% no_info_days)

data = good[ as.character(good$date) == "2015-01-01", ]
data = merge(data, min_data[, c("minute", "mean", "pred")], 
             by = "minute", 
             all.x = TRUE, 
             all.y = FALSE)

## do cross validation for the MSE
sqrt(mean( (data$steps - data$mean)^2))
sqrt(mean( (data$steps - data$pred)^2))
