library(RMySQL)
con <- dbConnect(MySQL(),user="root", password="root",dbname="dp", host="localhost")
# con <- dbConnect(MySQL(),user="martinlosak", password="Spider77",dbname="dp", host="dplosak.cvsz6xbnnwbe.eu-central-1.rds.amazonaws.com")
mapper <- dbGetQuery(con, "SELECT * FROM slovakia_mapper WHERE key_name = 'BA'")
gMapper <- dbGetQuery(con, "SELECT * FROM slovakia_mapper")

getTableName <- function(keyName) {
	query <- sprintf("SELECT * FROM slovakia_mapper WHERE key_name = '%s'", keyName)
	result <- dbGetQuery(con, query)
	tableName <- result[,c("table_name")]
	return(tableName)
}