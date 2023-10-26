##### CALL API #####

install.packages(c("httr","jsonlite"))
library(httr)
library(jsonlite)

raw_data <- GET("https://api.jcdecaux.com/vls/v1/stations?contract=Lyon&apiKey=d7df6e5b67b83f57bf7033708d4f380e882246ce")

raw_data_list <- fromJSON(rawToChar(raw_data$content), flatten = TRUE)

install.packages('tidygeocoder')

name <- names(raw_data_list)

for (titre in name) {print(raw_data_list[[titre]])}

library(dplyr, warn.conflicts = FALSE)
library(tidygeocoder)

df = data.frame()
df = do.call(raw_data_list, data)

#Si Jamais il y a la possibilité d'interogé L'API pour chercher "

##### SQL #####
# Projet = aumoins 3 Table


#https://www.slideshare.net/RsquaredIn/rmysql-tutorial-for-beginners
# diapo 7 connexion
# diapo 21 creat table argume (connession, nom table, database)
# diapo 15 voir data base
# sur le weeb https://www.freesqldatabase.com/ visualisation database

# package

install.packages("RMySQL")
library(RMySQL)

# info DataBase

database_host = "sql8.freesqldatabase.com"
database_name = "sql8646678"
database_username = "sql8646678"
database_pasword = "sncHTNH9Tb"


# connexion

con <- dbConnect(MySQL(),
                 user = database_username,
                 password = database_pasword,
                 host = database_host,
                 dbname = database_name)

# creation table

dbWriteTable(con,"Velib_Lyon_Info",raw_data_list)

# verif crea

dbListTables(con)
view_head <- dbGetQuery(con, "SELECT * FROM Velib_Lyon_Info LIMIT 5;")