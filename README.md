---
---
title: "ITS"
output: html_document
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```
Library packages

Good time series information: https://www.itl.nist.gov/div898/handbook/pmc/section4/pmc4431.htm

More good time series information: https://otexts.org/fpp2/stationarity.html

Good time series: https://datascienceplus.com/time-series-analysis-in-r-part-1-the-time-series-object/

Good information on time series in R: https://datascienceplus.com/time-series-analysis-in-r-part-2-time-series-transformations/
```{r}
library(lavaan)
library(psych)
library(semTools)
library(dplyr)
library(ltm)
library(prettyR)
library(semTools)
library(GPArotation)
library(lavaan)
library(psych)
library(semTools)
library(dplyr)
library(ltm)
library(lordif)
library(Amelia)
library(plyr)
library(paran)
library(caret)
library(ggplot2)
library(pracma)
library(AER)
library(pscl)
library(TSA)
library(TTR)
library(smooth)
library(descr)
library(urca)

```
Load the data
```{r}
head(ITSTest)
```
Cleaning data
```{r}
### use the gsub function to break off -02 part, then get rid of -, then you have the year
ITSTest$MonthNum =  gsub("\\d", "", ITSTest$Month)
### Get rid of -0x part 
ITSTest$MonthNum = substr(ITSTest$MonthNum, start = 1, stop= 3)

ITSTest$Year = gsub("\\D", "", ITSTest$Month)

ITSTest$Year = as.numeric(ITSTest$Year)

ITSTest$Month = NULL
head(ITSTest)

### Add a time variable that is 1:length of data set see Bernal article
ITSTest$Time= 1:dim(ITSTest)[1]
dim(ITSTest)
head(ITSTest)

ITSTest[144:150,]

#Start Jan 2014 intervention starts
Intervention= c(rep(0,143), rep(1,194-143))
length(Intervention)

ITSTest$Intervention = Intervention
head(ITSTest)
ITSTest[143:145,]


### Changing the month names to numbers so we can plot
ITSTest$MonthNum = ifelse(ITSTest$MonthNum == "Jan", 1, ifelse(ITSTest$MonthNum == "Feb", 2, ifelse(ITSTest$MonthNum == "Mar", 3, ifelse(ITSTest$MonthNum=="Apr", 4, ifelse(ITSTest$MonthNum == "May", 5, ifelse(ITSTest$MonthNum == "Jun", 6, ifelse(ITSTest$MonthNum == "Jul", 7, ifelse(ITSTest$MonthNum == "Aug", 8, ifelse(ITSTest$MonthNum == "Sep", 9, ifelse(ITSTest$MonthNum == "Oct", 10, ifelse(ITSTest$MonthNum == "Nov", 11, ifelse(ITSTest$MonthNum == "Dec", 12, ITSTest$MonthNum))))))))))))

```
Just look at descirptives
```{r}
describe(ITSTest)
```


Get counts by month and year 
We are missing some months?  Why?

Getting the total number of suicide deaths by month, then year
```{r}
sucByYear = aggregate(Suicides ~ Suicides + Year, data = ITSTest, sum)
sucByMonth = aggregate(Suicides ~ Suicides + MonthNum, data = ITSTest, sum)

sucByYear

sucByMonth

plot(sucByYear$Year, sucByYear$Suicides)
plot(sucByMonth$Month, sucByMonth$Suicides)

```
Evaluate the counts if those are different
```{r}
compmeans(ITSTest$Suicides, ITSTest$Intervention)
```
Conducting analyses:

Good link: https://support.minitab.com/en-us/minitab/18/help-and-how-to/modeling-statistics/time-series/how-to/autocorrelation/interpret-the-results/autocorrelation-function-acf/

ACF and PACF: https://www.ibm.com/support/knowledgecenter/en/SS3RA7_15.0.0/com.ibm.spss.modeler.help/timeseries_acf_pacf.htm

For auto cor plots, it the relationships for the first timepoint and the time point numbered on the x-axis.
```{r}

# NO we do not want this model this just shows if a one unit increase in time is related to an increase in suicides, not if adjsecent time points are related.  We want to see if one unit in time is likely related to another.
#modelH= hurdle(Suicides ~ Time, dist = "poisson", zero.dist = "binomial", data = ITSTest)  
summary(modelH)


### We really just want whether suicides are correlated over time

Suicides = ITSTest$Suicides
plot(ITSTest$Time, ITSTest$Suicides)
acf_suic = acf(Suicides)
pacf_suic = pacf(Suicides)
### Problem is that acf is using correlation, which is assuming two normally distrubted varialbes, which is not true



### Use other test
mean_station_short =  ur.kpss(ITSTest$Suicides, type="mu", lags="short")
summary(mean_station_short)

trend_station_short =  ur.kpss(ITSTest$Suicides, type="tau", lags="short")
summary(trend_station_short)

mean_station_long =  ur.kpss(ITSTest$Suicides, type="mu", lags="long")
summary(mean_station_long)

trend_station_long =  ur.kpss(ITSTest$Suicides, type="tau", lags="long")
summary(trend_station_long)

## try auto with log of suc
Suicides_log = log(Suicides)
hist(Suicides_log)
hist(Suicides)

```
Try suicides by pre and post intervention
```{r}

ITSTest_pre = subset(ITSTest, Intervention == 0)


Suicides = ITSTest_pre$Suicides
plot(ITSTest_pre$Time, ITSTest_pre$Suicides)
acf_suic = acf(Suicides)
pacf_suic = pacf(Suicides)
### Problem is that acf is using correlation, which is assuming two normally distrubted varialbes, which is not true




Suicides = ITSTest$Suicides
plot(ITSTest$Time, ITSTest$Suicides)
acf_suic = acf(Suicides)
pacf_suic = pacf(Suicides)


### Use other test
mean_station_short =  ur.kpss(ITSTest_pre$Suicides, type="mu", lags="short")
summary(mean_station_short)

trend_station_short =  ur.kpss(ITSTest_pre$Suicides, type="tau", lags="short")
summary(trend_station_short)

mean_station_long =  ur.kpss(ITSTest_pre$Suicides, type="mu", lags="long")
summary(mean_station_long)

trend_station_long =  ur.kpss(ITSTest_pre$Suicides, type="tau", lags="long")
summary(trend_station_long)

## try auto with log of suc
Suicides_log = log(Suicides)
hist(Suicides_log)
hist(Suicides)
```



