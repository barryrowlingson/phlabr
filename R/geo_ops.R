adjmat <- function(geom, tol, sparse=TRUE){
    st_overlaps(st_buffer(geom, tol),sparse=sparse)
}

adj2lines <- function(adjlist, geom){
    ilist = lapply(1:length(adjlist), function(i){

        n = adjlist[[i]]

        do.call(rbind,lapply(n, function(j){
            st_sf(data.frame(i=i, j=j, st_cast(geom[c(i,j)], "LINESTRING", ids=c(1,1))))
        }))
    })

    ilist = Filter(function(l){length(l)>0}, ilist)
    do.call(rbind, ilist)
    
}
