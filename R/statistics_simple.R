sir <- function(d, n, pop){
    (sum(d[[pop]])/sum(d[[n]])) * d[[n]]/d[[pop]]
}

