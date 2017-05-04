library(RMySQL)

dbUser = "root"
dbPassword = "root"
dbName = "dp"

getConnection <- function(){
	con = dbConnect(MySQL(), user = dbUser, password = dbPassword, dbname = dbName, host="localhost")
	return(con)
}

getMapper <- function(keyName=NULL){
	if(is.null(keyName)){
			query = "SELECT * FROM slovakia_mapper"
		} else {
			query = sprintf("SELECT * FROM slovakia_mapper WHERE key_name = '%s'", keyName)
		}
	
	return(dbSelect(query))
}

dbSelect <- function(query){
	con = getConnection()
	result = dbGetQuery(con, query)
	dbDisconnect(con)
	return(result)
}

dbWrite <- function(data, tableName){
	con = getConnection()
	dbWriteTable(con, name = tableName, value = data, append = TRUE, row.names = FALSE)
	dbDisconnect(con)
}

getTableName <- function(keyName) {
	query = sprintf("SELECT * FROM slovakia_mapper WHERE key_name = '%s'", keyName)
	result = dbSelect(query)
	tableName = result[,c("table_name")]
	return(tableName)
}

getRegion <- function(keyName) {
	query = sprintf("SELECT * FROM slovakia_mapper WHERE key_name = '%s'", keyName)
	result = dbSelect(query)
	region = result[,c("city")]
	return(region)
}