#This will generate the police call data for Eugene and Springfield up through the latest date in December 2018.
#In the current year, this will require minor modifications to scrape more current data.

library(tidyverse)
library(rvest)
library(stringr)
library(rebus)
library(lubridate)
library(zoo)
library(mvbutils)
library(forecast)


#Scrapes dates from the police call map
#ex: "Tuesday, Sept. 30"
get_date <- function(url,year){
  read_html(url) %>%
    html_nodes('.bull li a') %>%      
    html_text() %>% 
    str_trim() %>%                       
    unlist() %>%
    #adds the current year to the end
    paste(year)
}

#Scrapes number of calls for each day.
#ex: "262 calls for service"
get_num_calls <- function(url){
  read_html(url) %>%
    html_nodes('.bull b') %>%      
    html_text() %>% 
    str_trim() %>%                       
    unlist()  %>%
    #Parse number is here to eliminate the "calls for service" text
    parse_number()
}

#This function converts date to proper POSIXct format.

convert <- function(date,month,year){
  if (month %in% abbr) {
    parse_date_time(date, orders = "%A, %b. %d %Y")
    
  } else if (month %in% full) {
    parse_date_time(date, orders = "%A, %B. %d %Y")
    
  } else {
    #September is not in an accepted format, so it needs to be changed.
    date <- sub("Sept.", "Sep.", date)
    parse_date_time(date, orders = "%A, %b. %d %Y")
    
  }
}


#2008 doesn't contain every single month
year0 <- as.character(2008)
months0 <- c("sep", "oct", "nov", "dec")

#years and months to cycle through the URLs
years <- as.character(2009:2021)

#Months are given in the format used in the URLs
months <- c("jan", "feb","mar","apr", "may", "jun",
            "jul", "aug", "sep", "oct", "nov", "dec")

#months that are abbreviated in the data.
abbr <- c("jan", "feb", "aug", "oct", "nov", "dec")

#months that are NOT abbreviated in the data.
full <- c("mar", "april", "may", "jun", "jul")

#url_base will have "month/year/" added to the end in the loop
url_base <- "http://projects.registerguard.com/police/eugene/"


Eugene <- c()

for (year in year0){
  for (month in months0){
    url <- paste(url_base, year, '/', month, '/', sep = "")
    date <- get_date(url,year)
    date <- convert(date,month,year)
    calls <- get_num_calls(url)
    new <- cbind(date, calls)
    Eugene <- rbdf(Eugene, new)
  }
}

for (year in years){
  for (month in months){
    url <- paste(url_base, year, '/', month, '/', sep = "")
    date <- get_date(url,year)
    date <- convert(date,month,year)
    calls <- get_num_calls(url)
    new <- cbind(date, calls)
    Eugene <- rbdf(Eugene, new)
  }
}

#For some reason I haven't figured out, the loops do not properly bind
#the dates. 

#Instead they end up as numbers, and when I convert them directly
#to POSIXct, the dates end up being between 7 and 8 hours behind
#(that part isn't consistent either for some reason).

#Until I figure out the solution to that problem, the step below will
#do the trick.

Eugene$date <- ceiling_date(as.POSIXct(Eugene$date, origin = lubridate::origin),
                            "day")


write.csv(Eugene,'eugene.csv', row.names = FALSE)

ggplot(Eugene, aes(x = date, y = calls)) +
  geom_line()


eugene_series <- ts(Eugene$calls, frequency=365, start=c(2008, 9, 22))

plot.ts(eugene_series)

fit <- auto.arima(eugene_series)

fit
accuracy(fit)

next_month <- c()

for (month in 'jan'){
  url <- paste(url_base, year, '/', month, '/', sep = "")
  date <- get_date(url,'2022')
  date <- convert(date,month,'2022')
  calls <- get_num_calls(url)
  new <- cbind(date, calls)
  next_month <- rbdf(next_month, new)
}

next_month$date <- ceiling_date(as.POSIXct(next_month$date, origin = lubridate::origin),
             "day")


pred <- forecast(fit, 31)

next_month$pred <- pred$mean

ggplot(next_month, aes(date)) +
  geom_line(aes(y = calls, color = "calls")) +
  geom_line(aes(y = pred, color = "prediction")) +

  
ggplot(next_month, aes(x = date, y = calls - pred)) +
  geom_line()





#Here is the same thing for the neighboring town of Springfield.
#(the home of the Simpsons!)

#2013 doesn't contain every single month
year0 <- as.character(2013)
months0 <- c("nov", "dec")

years <- as.character(2014:2021)

url_base <- "http://projects.registerguard.com/police/springfield/"


Springfield <- c()

for (year in year0){
  for (month in months0){
    url <- paste(url_base, year, '/', month, '/', sep = "")
    date <- get_date(url,year)
    date <- convert(date,month,year)
    calls <- get_num_calls(url)
    new <- cbind(date, calls)
    Springfield <- rbdf(Springfield, new)
  }
}

for (year in years){
  for (month in months){
    url <- paste(url_base, year, '/', month, '/', sep = "")
    date <- get_date(url,year)
    date <- convert(date,month,year)
    calls <- get_num_calls(url)
    new <- cbind(date, calls)
    Springfield <- rbdf(Springfield, new)
  }
}

Springfield$date <- ceiling_date(as.POSIXct(Springfield$date, origin = lubridate::origin),
                                 "day")

write.csv(Springfield, 'springfield.csv',row.names = FALSE)


spfd_series <- ts(Springfield$calls, frequency=365, start=c(2013, 11, 19))

plot.ts(spfd_series)

fit2 <- auto.arima(spfd_series)

fit2

next_month2 <- c()

for (month in 'jan'){
  url <- paste(url_base, year, '/', month, '/', sep = "")
  date <- get_date(url,'2022')
  date <- convert(date,month,'2022')
  calls <- get_num_calls(url)
  new <- cbind(date, calls)
  next_month2 <- rbdf(next_month2, new)
}

next_month2$date <- ceiling_date(as.POSIXct(next_month2$date, origin = lubridate::origin),
                                "day")


pred2 <- forecast(fit2, 31)

next_month2$pred <- pred2$mean

ggplot(next_month2, aes(date)) +
  geom_line(aes(y = calls, color = "calls")) +
  geom_line(aes(y = pred, color = "prediction"))
  
  
ggplot(next_month2, aes(x = date, y = calls - pred)) +
  geom_line()
