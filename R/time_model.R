fit_time_file <- function(datafile){
    data = read.csv(datafile, stringsAsFactors=FALSE)
    names(data)=c("Total","Year","Date")
    data$Date = as.Date(data$Date, format="%d/%m/%Y")
    fit_time_data(data)
}

fit_time_data <- function(data){
    data$Weekday = weekdays(data$Date)
    data$Yearday = as.POSIXlt(data$Date)$yday
    m = glm(Total ~ Weekday +
        sin(2*pi*Yearday/365) + cos(2*pi*Yearday/365) +
        sin(4*pi*Yearday/365) + cos(4*pi*Yearday/365)
         , data=data, family=poisson)
    m
}
