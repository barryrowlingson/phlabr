##' sample independent spatial and temporal data
##'
##' etc
##' @title simulate independent spatial and temporal data
##' @param N number to simulate
##' @param time_model a poisson GLM for the temporal model
##' @param postcode_pop data frame of postcode,pop columns
##' @return data frame of simulated case postcodes and times
##' @author Barry Rowlingson
sim_data <- function(time_model, postcode_pop, timespan=range(time_model$data$Date)){

    times = data.frame(Date = seq(timespan[1],timespan[2],1))
    times = augment_data(times)
    times$Total = predict_time_data(times, time_model)

    full = data.frame(Date=rep(times$Date, times$Total))
    postcode_row = sample(1:nrow(postcode_pop),nrow(full),prob=postcode_pop$pop,replace=TRUE)

    full$postcode = as.character(postcode_pop[postcode_row,]$postcode)
    full$geometry=st_geometry(st_centroid(postcode_pop[postcode_row,]))
    full = st_sf(full, sf_column_name="geometry")
    full
    
}
