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
    return((dd$Date >= (last_sunday-6)) & (dd$Date <= last_sunday))
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
