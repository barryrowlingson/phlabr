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


week_data <- function(weeknum, start=week_zero_start()){
    d1 = start + weeknum*7
    d2 = d1 + 6
    data.frame(first=d1, last=d2)
}

week_data_str <- function(weeknum, start=week_zero_start(), f="%d %b %y", fs = "%s - %s"){
    dd = week_data(weeknum, start=start)
    sprintf(fs, format(dd[,1],f),format(dd[,2],f))
}

week_data_fac <- function(weeknum, start=week_zero_start(), f="%d %b %y", fs = "%s - %s"){
    ww = week_data_str(weeknum, start=start, f=f, fs=fs)
    factor(ww,levels=unique(ww[order(weeknum)]))
}

data_week_monday <- function(w, start=week_zero_start()){
    start + w*7
}

last_n_weeks_range <- function(d,n=10){
    d = as.Date(d)
    drange = seq(full_week_start(d),full_week_end(d),"day")
    w = data_week(d)
    wr = c(max(w)-n+1, max(w))
    wr
}

nweeksdata <- function(data, weeks=10, date_c="Date"){
    data$dweek = data_week(data[[date_c]])
    drange = seq(full_week_start(data[[date_c]]),full_week_end(data[[date_c]]),"day")
    wr = last_n_weeks_range(drange, n=weeks)
    inweek = data$dweek >= wr[1] & data$dweek <= wr[2]
    data = data[inweek,]
    list(data = data, weeks = seq(min(wr),max(wr)))
}

st_aggregate_weekly <- function(
    spatial, spatial_id, # a spatial database and an ID column in that database
    weeks=10,
    data, s_c, date_c # the data, with a spatial column and a date column
    ){

    datagrid = nweeksdata(data, weeks, date_c=date_c)
    
    stgrid = expand.grid(s = spatial[[spatial_id]], t = datagrid$weeks)

    tab = as.data.frame(table(s=datagrid$data[[s_c]],t=datagrid$data$dweek))
    tab$t = as.numeric(as.character(tab$t))
    stj = left_join(stgrid, tab, c("s"="s","t"="t"))
    stj$Freq[is.na(stj$Freq)]=0

    stj
}


