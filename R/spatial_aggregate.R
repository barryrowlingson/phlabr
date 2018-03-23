aggregate_postcodes <- function(unit_postcodes, areas=TRUE, districts=TRUE, sectors=TRUE){

    pc_areas <- pc_sectors <- pc_districts <- NULL
    
    if(areas){
        message("Aggregating areas...")
        pc_areas = dplyr::group_by(unit_postcodes,pc_area) %>%
            dplyr::summarise(pop=sum(pop),
                      pixels = sum(pixels)
                      ) %>% st_centroid()
    }

    if(districts){
        message("Aggregating districts...")
        pc_districts = dplyr::group_by(unit_postcodes,pc_district) %>%
            dplyr::summarise(pop=sum(pop),
                      pixels = sum(pixels)
                      ) %>% st_centroid()
    }

    if(sectors){
        message("Aggregating sectors...")
        pc_sectors = dplyr::group_by(unit_postcodes,pc_sector) %>%
            dplyr::summarise(pop=sum(pop),
                      pixels = sum(pixels)
                      ) %>% st_centroid()
    }
    return(list(
        pc_areas = pc_areas,
        pc_districts = pc_districts,
        pc_sectors = pc_sectors
        )
           )
}

save_aggregates <- function(agglist, dbpath){
    st_write(agglist$pc_areas, dbpath, "pc_areas", layer_options = "OVERWRITE=true")
    st_write(agglist$pc_districts, dbpath, "pc_districts", layer_options = "OVERWRITE=true")
    st_write(agglist$pc_sectors, dbpath, "pc_sectors", layer_options = "OVERWRITE=true")
}

add_centroids <- function(sfs){
    xy = st_coordinates(st_centroid(sfs))
    sfs$xc=xy[,1]
    sfs$yc=xy[,2]
    return(sfs)
}
