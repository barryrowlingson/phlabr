##' get latest complete week of data from daily data
##'
##' weeks start on a Monday.
##' @title Get last complete week
##' @param d vector of dates of cases
##' @return TRUE for last complete week
##' @author Barry Rowlingson
last_complete_week <- function(d){
    d = as.Date(d) # convert chars to Date
    dd = data.frame(
        i=1:length(d),
        Date=d)
    dd$dayname = format(d, "%A")
    sundays = dd[dd$dayname=="Sunday",]
    last_sunday = max(sundays$Date)
    val = (dd$Date >= (last_sunday-6)) & (dd$Date <= last_sunday)
    attr(val,"last_sunday") <- last_sunday
    return(val)
}

last_possible_sunday <- function(d){
    d = as.Date(d)
    s = data.frame(d = seq(max(d)-14,max(d),"day"))
    s$dayname = format(s$d,"%A")
    s = s[s$dayname=="Sunday","d"]
    max(s)
}

last_weeks_data <- function(db, column="Date"){
    db[[column]] = as.Date(db[[column]])
    d = db[[column]]
    return(db[last_complete_week(d),])
}

join_to_spatial <- function(casedata, spatialdata, join, count="n"){

    j = left_join(spatialdata, casedata, join)
    j[[count]][is.na(j[[count]])] = 0
    j
}

full_week_start <- function(d){
    d = as.Date(d)
    s = data.frame(d=seq(min(d), min(d)+14, "day"))
    s$dayname = format(s$d,"%A")
    s = s[s$dayname == "Monday","d"]
    min(s)
}

full_week_end <- function(d){
    d = as.Date(d)
    last_day = last_possible_sunday(d)
    return(last_day)
}

week_zero_start <- function(start="Monday"){
    z = as.Date("2001-01-01")
    day = format(z,"%A")
    stopifnot(day==start)
    z
}

data_week <- function(d, start=week_zero_start()){
    d = as.Date(d)
    as.numeric((d-start)) %/% 7
}

last_n_weeks_range <- function(d,n=10){
    d = as.Date(d)
    drange = seq(full_week_start(d),full_week_end(d),"day")
    w = data_week(d)
    wr = c(max(w)-n-1, max(w))
    wr
}

st_aggregate_weekly <- function(spatial, days, data){
    
}
