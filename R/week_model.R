
fit_week_data <- function(data){
    data$week = week(data$Date)
    data$YW = data$Year + (data$week %% 52)/52
    wdata = aggregate(Total~YW, data=data, sum)
    wdata$week = 52*(as.integer(wdata$YW)-wdata$YW)
    m = glm(Total ~ 
        sin(2*pi*week/52) + cos(2*pi*week/52) +
        sin(4*pi*week/52) + cos(4*pi*week/52)
         , data=wdata, family=poisson)
   list(data=wdata, model= m)
}


