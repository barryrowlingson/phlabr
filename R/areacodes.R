add_postcode_area <- function(pcd){
    pcd$pcarea= sub("^([A-Z]+)(.*)","\\1",pcd$postcode)
    pcd
}

postcode_area_pop <- function(pcd){
    aggregate(pop ~ pcarea, data=pcd, sum)
}
