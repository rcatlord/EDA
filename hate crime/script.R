# Code adapted from https://sotonpolitics.org/2016/10/20/did-brexit-increase-hate-crimes-probably-yes/
# Data source: https://www.gov.uk/government/uploads/system/uploads/attachment_data/file/559889/hate-crime-1516-hosb1116-tables.ods

## Load the data
library(readr)
hate_daily <- read_csv("hate_crimes_daily.csv")
hate_monthly <- read_csv("hate_crimes_monthly.csv")

## Time series plots
library(ggplot2)
ggplot(data = hate_monthly, aes(x=id, y=frequency)) +
  geom_line(color = "#c0392b", size = 1.45, alpha = 0.75) +
  geom_hline(yintercept = 2000, size = 0.4, color = "black") +
  geom_vline(xintercept = 42, linetype = "longdash", color = "gray47", alpha = 0.7) +
  geom_label(aes(x = 42, label = "Referendum", y = 2300), colour = "gray36", fontface = "bold") +
  scale_x_continuous(breaks=c(6,12,18,24,30,36,42),
                     labels=c("June 2013", "Dec 2013", "June 2014", "Dec 2014", "June 2015", "Dec 2015", "June 2016")) +
  labs(title = "Hate Crimes in England and Wales, 2013-2016",
     x = "Month",
     y = "# Hate Crimes",
     caption = "Data: UK Home Office") +
  theme_minimal()

ggplot(data = hate_daily, aes(x=id, y=frequency)) +
  geom_line(color = "#c0392b", size = 1.45, alpha = 0.75) +
  geom_hline(yintercept = 75, size = 0.4, color = "black") +
  geom_vline(xintercept = 54, linetype = "longdash", color = "gray47", alpha = 0.7) +
  geom_label(aes(x = 54, label = "Referendum", y = 85), colour = "gray36", fontface = "bold") +
  scale_y_continuous(limits = c(75,220)) +
  scale_x_continuous(breaks=seq(14,123, by = 14),
                     labels=c("14 May", "28 May", "11 June", "25 June", "9 July", "23 July", "6 August", "20 August")) +
  labs(title = "Hate Crimes in England and Wales, May-August 2016",
       x = "Month",
       y = "# Hate Crimes",
       caption = "Data: UK Home Office") +
  theme_minimal()
  
## Linear regression
# How much variation can be explained by the referendum, and how many hate crimes can be attributed to Brexit?
linear.day <- lm(hate_daily$frequency ~ hate_daily$brexit)
summary(linear.day)
linear.month <- lm(hate_monthly$frequency ~ hate_monthly$brexit)
summary(linear.month)
library(stargazer)
stargazer(linear.day, linear.month, type = "html",
          title = "The Effect of Brexit on Hate Crimes",
          column.labels = c("Daily Crime", "Monthly Crime"),
          coviariate.labels = "Brexit")

## Regression discontinuity
# the days either side of the referendum are fundamentally different, and the only plausible explanation is the referendum
library(rdrobust)
rdrobust(hate_daily$frequency, hate_daily$since_ref, c = 1)
rdplot(hate_daily$frequency, hate_daily$since_ref, c = 1,
       title = "Regression Discontinuity",
       x.label = "Days before and after EU referendum",
       y.label = "# Hate Crimes")
       
library(rdd)
rdd <- RDestimate(frequency~since_ref, data = hate_daily, cutpoint = 1)
plot(rdd)
title(main = "Regression Discontinuity",
      xlab = "Days before and after EU referendum",
      ylab = "# Hate Crimes")
