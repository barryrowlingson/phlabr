
st_get_from_postcode_db <- function(db, table){
    pc_dist = st_read(db, table)
    return(pc_dist)
}
