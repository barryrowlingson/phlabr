read_time_data <- function(datafile){
    data = read.csv(datafile, stringsAsFactors=FALSE)
    if(nrow(data)!=3){
        stop("Expecting 3 columns, Total, Year, Data, in the data file")
    }
    names(data)=c("Total","Year","Date")
    data$Date = as.Date(data$Date, format="%d/%m/%Y")
    data
}
fit_time_file <- function(datafile){
    data = read_time_data(datafile)
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

fit_time_nb <- function(data){
    data$Weekday = weekdays(data$Date)
    data$Yearday = as.POSIXlt(data$Date)$yday
    m = MASS::glm.nb(Total ~ Weekday +
        sin(2*pi*Yearday/365) + cos(2*pi*Yearday/365) +
        sin(4*pi*Yearday/365) + cos(4*pi*Yearday/365)
         , data=data)
    m
}

predict_time_data <- function(data,m){
    data$Weekday = weekdays(data$Date)
    data$Yearday = as.POSIXlt(data$Date)$yday
    f = predict(m, newdata=data, type="link")
    n = rpois(length(f),lambda=exp(f))
    n
}
