#new charting stuff

#packages ----
library(dplyr)
library(ggplot2)
library(shiny)
library(highcharter)
library(dygraphs)
library(plotly)
library(flexdashboard)
library(lubridate)
library(tidyr)
library(reshape2)
library(RCurl)
library(rvest)
library(scales)
library(astsa)
library(ggthemes)
library(extrafont)
library(reshape)
library(car)
library(forecast)
library(fGarch)

#loads and formats shooting data----

#shooting data
data <- read.csv("https://raw.githubusercontent.com/washingtonpost/data-police-shootings/master/fatal-police-shootings-data.csv")
data$date <- ymd(data$date)
data$age <- as.numeric(data$age)
data$state.flag <- "other"
data$state.flag[data$state == "PA"] <- "PA"
data$state.flag <- as.factor(data$state.flag)
data$city.flag <- "other"
data$city.flag[data$city== "Philadelphia"] <- "PHL"
data$city.flag <- as.factor(data$city.flag)
data$month <- month(data$date, label = TRUE)
data$year <- year(data$date)
data$year <- as.factor(data$year)
data$day <- wday(data$date, label = TRUE, abbr = TRUE)
data$count <- 1
data$age[is.na(data$age)] <- 0

#re-codes race as white, black hispanic, other
data$race2 <- "Other"
data$race2[data$race == "W"] <- "White"
data$race2[data$race == "B"] <- "Black"
data$race2[data$race == "H"] <- "Hispanic"
data$race2[data$race == " "] <- "Unknown"
data$race2 <- as.factor(data$race2)

#creates age buckets
data$age2 <- "Under 18"
data$age2[18 <= data$age & data$age < 30] <- "18 to 29"
data$age2[30 <= data$age & data$age < 45] <- "30 to 44"
data$age2[45 <= data$age] <- "45 and Older"
data$age2[data$age == 0] <- "Unknown"
data$age2 <- as.factor(data$age2)

#loads and checks structure
View(data)
str(data)

#state population data
states <- read.csv("http://www.census.gov/popest/data/state/totals/2015/tables/NST-EST2015-01.csv", header = FALSE)
states <- states[-c(1, 2, 3, 4, 5, 6, 7, 8, 9, 61, 62, 63, 64, 65, 66, 67), -c(2,3,4,5,6,7,8)] #removes unwanted rows and columns, keeps only 2015 population and state name
names(states)[1] <- "state"
names(states)[2] <- "pop"
states$state <- sub('.', '', states$state) #removes "." at beginning of state names
states$pop <- gsub(',', '', states$pop) #removes commas from population column
states$state <- state.abb[match(states$state,state.name)] #adds state abbreviations
states[9,1] <- "DC" #Adds Washington DC abbreviation
states$state <- as.factor(states$state)
states$pop <- as.numeric(states$pop)

#loads and checks structure
View(states)
str(states)

#joins shooting and population data
data <- left_join(data, states, by = "state")
data$state <- as.factor(data$state)
str(data)

#creates dataframe of list of dates to fill in date gaps
date.ref <- data.frame(date=seq(as.Date("2015-01-01"), as.Date("2016-01-15"), by="days"))
date.ref$date <- ymd(date.ref$date)

#joins data and date.ref dataframes and replaces na with 0's
data <- merge(data,date.ref,by.x='date',by.y='date',all.x=T,all.y=T)
data$count[is.na(data$count)] <- 0


#ggviz, plotly, highcharter, dygraphs, 

#loads in and formats passenger, employment and earnings data ----
x.2007 <- getURL("https://raw.githubusercontent.com/brndngrhm/time_series_and_forecasting/master/project/phl/data/2007.csv")
data.2007 <- read.csv(text = x.2007)
x.2008 <- getURL("https://raw.githubusercontent.com/brndngrhm/time_series_and_forecasting/master/project/phl/data/2008.csv")
data.2008 <- read.csv(text = x.2008)
x.2009 <- getURL("https://raw.githubusercontent.com/brndngrhm/time_series_and_forecasting/master/project/phl/data/2009.csv")
data.2009 <- read.csv(text = x.2009)
x.2010 <- getURL("https://raw.githubusercontent.com/brndngrhm/time_series_and_forecasting/master/project/phl/data/2010.csv")
data.2010 <- read.csv(text = x.2010)
x.2011 <- getURL("https://raw.githubusercontent.com/brndngrhm/time_series_and_forecasting/master/project/phl/data/2011.csv")
data.2011 <- read.csv(text = x.2011)
x.2012 <- getURL("https://raw.githubusercontent.com/brndngrhm/time_series_and_forecasting/master/project/phl/data/2012.csv")
data.2012 <- read.csv(text = x.2012)
x.2013 <- getURL("https://raw.githubusercontent.com/brndngrhm/time_series_and_forecasting/master/project/phl/data/2013.csv")
data.2013 <- read.csv(text = x.2013)
x.2014 <- getURL("https://raw.githubusercontent.com/brndngrhm/time_series_and_forecasting/master/project/phl/data/2014.csv")
data.2014 <- read.csv(text = x.2014)
x.2015 <- getURL("https://raw.githubusercontent.com/brndngrhm/time_series_and_forecasting/master/project/phl/data/2015.csv")
data.2015 <- read.csv(text = x.2015)

#data.2007 <- read.csv("~/R Working Directory/Villanova/time_series_and_forecasting/project/phl/data/2007.csv")
#data.2008 <- read.csv("~/R Working Directory/Villanova/time_series_and_forecasting/project/phl/data/2008.csv")
#data.2009 <- read.csv("~/R Working Directory/Villanova/time_series_and_forecasting/project/phl/data/2009.csv")
#data.2010 <- read.csv("~/R Working Directory/Villanova/time_series_and_forecasting/project/phl/data/2010.csv")
#data.2011 <- read.csv("~/R Working Directory/Villanova/time_series_and_forecasting/project/phl/data/2011.csv")
#data.2012 <- read.csv("~/R Working Directory/Villanova/time_series_and_forecasting/project/phl/data/2012.csv")
#data.2013 <- read.csv("~/R Working Directory/Villanova/time_series_and_forecasting/project/phl/data/2013.csv")
#data.2014 <- read.csv("~/R Working Directory/Villanova/time_series_and_forecasting/project/phl/data/2014.csv")
#data.2015 <- read.csv("~/R Working Directory/Villanova/time_series_and_forecasting/project/phl/data/2015.csv")

data.2007[5] <- NULL
data.2007$year <- "2007"
data.2008[5] <- NULL
data.2008$year <- "2008"
data.2009[5] <- NULL
data.2009$year <- "2009"
data.2010[5] <- NULL
data.2010$year <- "2010"
data.2011[5] <- NULL
data.2011$year <- "2011"
data.2012[5] <- NULL
data.2012$year <- "2012"
data.2013[5] <- NULL
data.2013$year <- "2013"
data.2014[5] <- NULL
data.2014$year <- "2014"
data.2015[5] <- NULL
data.2015$year <- "2015"

pax <- rbind(data.2007, data.2008, data.2009, data.2010, data.2011, data.2012, data.2013, data.2014, data.2015)
names(pax) <- tolower(names(pax))
phl <- pax %>% dplyr::filter(origin == "PHL") %>% group_by(year, month) %>% summarise("pax" = sum(passengers))
phl$month2 <- month(phl$month, label = TRUE)
phl$month <- NULL
names(phl)[3] <- "month"
phl$date <- paste(phl$month, "1,", phl$year, sep=" ")
phl$date <- mdy(phl$date)
#write.csv(phl, file = "~/R Working Directory/Villanova/time_series_and_forecasting/project/phl/data/phl.csv")

#emp 
x.emp <- getURL("https://raw.githubusercontent.com/brndngrhm/time_series_and_forecasting/master/project/phl/data/phl_monthly_emp.csv")
emp <- read.csv(text = x.emp)
#emp <- (read.csv("~/R Working Directory/Villanova/time_series_and_forecasting/project/phl/data/phl_monthly_emp.csv"))

emp <-  melt(emp, id.vars = c("Year"))
names(emp)[1] <- "year"
names(emp)[2] <- "month"
names(emp)[3] <- "emp"
emp$year <- as.character(emp$year)
emp$emp <- emp$emp * 1000
emp$month2[emp$month == "January"] <- 1
emp$month2[emp$month == "February"] <- 2
emp$month2[emp$month == "March"] <- 3
emp$month2[emp$month == "April"] <- 4
emp$month2[emp$month == "May"] <- 5
emp$month2[emp$month == "June"] <- 6
emp$month2[emp$month == "July"] <- 7
emp$month2[emp$month == "August"] <- 8
emp$month2[emp$month == "September"] <- 9
emp$month2[emp$month == "October"] <- 10
emp$month2[emp$month == "November"] <- 11
emp$month2[emp$month == "December"] <- 12
emp$month <- month(emp$month2, label = TRUE)
emp$month2 <- NULL
emp <- emp %>% group_by(year, month) %>% summarise("emp" = sum(emp))
emp <- emp[-c(105, 106, 107, 108), ]

#earnings
x.earn <- getURL("https://raw.githubusercontent.com/brndngrhm/time_series_and_forecasting/master/project/phl/data/phl_avg_wk_earn.csv")
earnings <- read.csv(text = x.earn)
#earnings <- (read.csv("~/R Working Directory/Villanova/time_series_and_forecasting/project/phl/data/phl_avg_wk_earn.csv"))

earnings <-  melt(earnings, id.vars = c("Year"))
names(earnings)[1] <- "year"
names(earnings)[2] <- "month"
names(earnings)[3] <- "earnings"
earnings$year <- as.character(earnings$year)
earnings$earnings <- earnings$earnings * 4
earnings$month2[earnings$month == "January"] <- 1
earnings$month2[earnings$month == "February"] <- 2
earnings$month2[earnings$month == "March"] <- 3
earnings$month2[earnings$month == "April"] <- 4
earnings$month2[earnings$month == "May"] <- 5
earnings$month2[earnings$month == "June"] <- 6
earnings$month2[earnings$month == "July"] <- 7
earnings$month2[earnings$month == "August"] <- 8
earnings$month2[earnings$month == "September"] <- 9
earnings$month2[earnings$month == "October"] <- 10
earnings$month2[earnings$month == "November"] <- 11
earnings$month2[earnings$month == "December"] <- 12
earnings$month <- month(earnings$month2, label = TRUE)
earnings$month2 <- NULL
earnings <- earnings %>% group_by(year, month) %>% summarise("earnings" = sum(earnings))
earnings <- earnings[-c(105, 106, 107, 108), ]

#jet fuel SOURCE: U.S. Gulf Coast Kerosene-Type Jet Fuel Spot Price FOB Dollars per Gallon
x.fuel <- getURL("https://raw.githubusercontent.com/brndngrhm/time_series_and_forecasting/master/project/phl/data/fuel.csv")
fuel <- read.csv(text = x.fuel)
fuel$date <- mdy(fuel$date)
fuel <- fuel %>% filter(date > "2006-12-31") %>% filter(date < "2015-08-01")
fuel$year <- year(fuel$date)
fuel$month <- month(fuel$date, label=T)
fuel$year <- as.character(fuel$year)
fuel$date <- NULL

#join data and save .rda
socio <- left_join(emp, earnings, by = c("year", "month"))
phl <- left_join(phl,fuel, by = c("year", "month"))
phl <- left_join(phl, socio, by = c("year", "month"))
phl$year <- as.factor(phl$year)
phl <- phl %>% select(date, year, month, pax, price, emp, earnings)

#highcharter ----
#reference website w/ code examples - http://jkunst.com/highcharter/
#diff types of charts - http://jkunst.com/highcharter/hchart.html
#themes - http://jkunst.com/highcharter/themes.html
#manual - https://cran.r-project.org/web/packages/highcharter/highcharter.pdf
  
#plot of shootings by state
hchart(data$state, name = "State")

#plot of shootings by race
hchart(subset(data$race2, data$year == "2016"), name = "2016 Shootings", colorByPoint = T)
hchart(subset(data$race2, data$year == "2015"), name = "2015 Shootings", colorByPoint = T)

#dygraphs
monthly <- data %>% group_by(year, month) %>% summarise(total = n())
dygraph(ts(monthly$total, frequency = 12)) %>% dyRangeSelector()


#Rmarkdown/shiny page w tabs for 1) list of user instaled packages, 2) cookbook of r commands (base ploting, ggplot, highcharter, dplyr commands, timeseries ideas, regression...)

