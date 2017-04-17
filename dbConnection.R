library(RMySQL)

getConnection <- function(){
	# con = dbConnect(MySQL(),user="martinlosak", password="Spider77",dbname="dp", host="dplosak.cvsz6xbnnwbe.eu-central-1.rds.amazonaws.com")
	con = dbConnect(MySQL(),user="root", password="root",dbname="dp", host="localhost")
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