make_district_boundaries <- function(pts, id, boundary){

    popdata = summarise(group_by_(data.frame(pts),id),pop=sum(pop)) 
    v = st_voronoi(st_union(pts))
    v = st_sf(data.frame(geometry=st_cast(v)))
    j = st_join(v, pts)
    m = summarise(group_by_(j,id))
    b = left_join(m, popdata, by=id)
    boundary$pop = NULL
    st_intersection(boundary, b)
}

prepare_sectors <- function(pc_sectors){
    pc_sectors$pc = paste0(pc_sectors$pc_sector,"XX")
    pc_sectors$pc_area = pc_area(pc_sectors$pc)
    pc_sectors$pc_district = pc_district(pc_sectors$pc)
    pc_sectors
}

get_postcodes_for <- function(pcdb, column, value){
    
    conn <- dbConnect(SQLite(), dbname = pcdb)
    whereclause = paste0(column," = '",value,"'")
    sql = paste0('select * from "postcodes" where ',whereclause)
    v = st_as_sf(dbGetQuery(conn, sql))
    st_crs(v)=27700
    dbDisconnect(conn)
    v
}

do_all_district_boundaries <- function(area_map, codes = area_map$pc_area, pcdb){

    dd = lapply(codes, function(code){
        pcs = get_postcodes_for(pcdb, "pc_area", code)
        st_crs(pcs)=st_crs(area_map) # its all epsg:27700 
        points = st_centroid(pcs)
        bounds = area_map[area_map$pc_area==code,]
        st_crs(points) = st_crs(bounds)
        vor = make_district_boundaries(points, "pc_district", bounds)
        vor
    })
    ad = do.call(rbind, dd)
    ad
}
