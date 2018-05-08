read_time_data <- function(datafile){
    data = read.csv(datafile, stringsAsFactors=FALSE)
    if(ncol(data)!=3){
        stop("Expecting 3 columns, Total, Year, Data, in the data file")
    }
    names(data)=c("Total","Year","Date")
    data$Date = as.Date(data$Date, format="%d/%m/%Y")
    data = augment_data(data)
    data
}
fit_time_file <- function(datafile){
    data = read_time_data(datafile)
    fit_time_data(data)
}

augment_data = function(data){
    # add new columns derived from the source
    data$Weekday = weekdays(data$Date)
    data$Yearday = as.POSIXlt(data$Date)$yday
    data
}
fit_time_data <- function(data){
    m = glm(Total ~ Weekday +
        sin(2*pi*Yearday/365) + cos(2*pi*Yearday/365) +
        sin(4*pi*Yearday/365) + cos(4*pi*Yearday/365)
         , data=data, family=poisson)
    m
}

fit_time_nb <- function(data){
    m = MASS::glm.nb(Total ~ Weekday +
        sin(2*pi*Yearday/365) + cos(2*pi*Yearday/365) +
        sin(4*pi*Yearday/365) + cos(4*pi*Yearday/365)
         , data=data)
    m
}

predict_time_data <- function(data,m){
    f = predict(m, newdata=data, type="link")
    n = rpois(length(f),lambda=exp(f))
    n
}

day_weeks <- function(dates, origin=as.Date("2007-01-01")){
    weekday = format(origin,"%A")
    if(weekday != "Monday"){
        warning("Origin day not a Monday")
    }
    dd = difftime(dates,origin,units = "days")

    week = as.numeric(dd) %/% 7
    daynum = as.numeric(dd) %% 7
    complete = rep(TRUE, length(week))
    
    
    first_week = week == min(week)
    last_week = week == max(week)

    if(sum(first_week, na.rm=TRUE)<7){
        complete[first_week]=FALSE
    }

    if(sum(last_week, na.rm=TRUE)<7){
        complete[last_week]=FALSE
    }

    data.frame(day=dd, week=week, complete=complete, weekday=format(dates,"%A"), daynum=daynum)
}
        

aggregate_weeks <- function(data,origin=as.Date("2007-01-01")){
    aw = day_weeks(data$Date,origin=origin)
    data = data[aw$complete,]
    aw = aw[aw$complete,]
    totals = tapply(data$Total, aw$week, sum)
    weeknum = as.numeric(names(totals))
    data.frame(WeekStart = as.Date(origin)+weeknum*7, Total=totals)
}

fit_weekly <- function(data){
    data$WeekStart = as.numeric(data$WeekStart) 
    m = glm(Total ~ 
        sin(2*pi*WeekStart/365) + cos(2*pi*WeekStart/365) +
        sin(4*pi*WeekStart/365) + cos(4*pi*WeekStart/365)
        , data=data, family=poisson)
    m
}
