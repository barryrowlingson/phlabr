test_postcodes <- function(postcodes_data, postcodes_db){
    !is.na(match(postcodes_data, postcodes_db))
}

read_postcodes_file <- function(pc_csv){
    pcs = read.csv(pc_csv, stringsAsFactors=FALSE)
    pcs
}

get_postcodes_db <- function(sqlite, table, column){
    con <- dbConnect(RSQLite::SQLite(), sqlite)
    qs = paste0("select ",column," from ",table)
    res = dbSendQuery(con, qs)
    dbFetch(res)
}
