##' get latest complete week of data from daily data
##'
##' weeks start on a Monday.
##' @title Get last complete week
##' @param d vector of dates of cases
##' @return TRUE for last complete week
##' @author Barry Rowlingson
last_complete_week <- function(d){
    dd = data.frame(
        i=1:length(d),
        Date=d)
    dd$dayname = format(d, "%A")
    sundays = dd[dd$dayname=="Sunday",]
    last_sunday = max(sundays$Date)
    (dd$Date >= (last_sunday-6)) & (dd$Date <= last_sunday)
}
