library(RMySQL)
# con <- dbConnect(MySQL(),user="root", password="852456",dbname="dp", host="localhost")
con <- dbConnect(MySQL(),user="martinlosak", password="Spider77",dbname="dp", host="losakdp.cvsz6xbnnwbe.eu-central-1.rds.amazonaws.com")
mapper <- dbGetQuery(con, "SELECT * FROM slovakia_mapper WHERE key_name = 'BA'")