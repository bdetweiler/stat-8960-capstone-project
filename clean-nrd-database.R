library("devtools")
# install.packages( c("MonetDB.R", "MonetDBLite") , repos=c("http://dev.monetdb.org/Assets/R/", "http://cran.rstudio.com/"))
library('MonetDB.R')
library('MonetDBLite')
library('dplyr')
library('dbplyr')
library('DBI')

# setwd('/home/bdetweiler/src/Data_Science/stat-8960-capstone-project')

# Here, we modify the columns that were initialized as VARCHARs to accommodate for missing data codes
# into their ideal data types (typically INTEGER or DOUBLE) so we can offload calculations to the database
# We'll use the technique specified here: 
# https://stackoverflow.com/questions/27770370/alter-the-data-type-of-a-column-in-monetdb

# MonetDB connection to a permanent file
# Call the below line if you get an error about connecting
# MonetDBLite::monetdblite_shutdown()
con <- DBI::dbConnect(MonetDBLite::MonetDBLite(), "data/nrd_db")
# For all integer columns, replace non-integers with null
column.names <- readLines('data/convert-varchar-columns-to-integers.txt')
for (column.name in column.names) {
  q <- DBI::dbGetQuery(con, paste0("SELECT DISTINCT(", column.name, ") as d FROM nis"))
  replace <- q$d[which(is.na(as.integer(q$d)))]
  # Enclose in single quotes for query
  replace <- paste0("'", replace, "'")
  replace <- paste(replace, collapse=",")
  update.query <- paste0("UPDATE nis SET ", column.name, " = null WHERE ", column.name, " in (", replace, ")")
  print(update.query)
  
  DBI::dbSendQuery(con, update.query)
} 

# For all double columns, replace non-doubles with null
column.names <- readLines('data/convert-varchar-columns-to-doubles.txt')
for (column.name in column.names) {
  q <- DBI::dbGetQuery(con, paste0("SELECT DISTINCT(", column.name, ") as d FROM nis"))
  replace <- q$d[which(is.na(as.double(q$d)))]
  # Enclose in single quotes for query
  replace <- paste0("'", replace, "'")
  replace <- paste(replace, collapse=",")
  update.query <- paste0("UPDATE nis SET ", column.name, " = null WHERE ", column.name, " in (", replace, ")")
  print(update.query)
  
  DBI::dbSendQuery(con, update.query)
} 

# Create a temporary column 

column.names <- readLines('data/convert-varchar-columns-to-integers.txt')
for (column in column.names) {
  column.tmp <- paste0(column, "_tmp")
  alter.query <- paste0("ALTER TABLE nis ADD COLUMN ", column.tmp, " INTEGER")
  print(alter.query)
  DBI::dbSendQuery(con, alter.query)
  
  # Set data in temporary column to original data 
  update.query <- paste0("UPDATE nis SET ", column.tmp, "=", column)
  print(update.query)
  DBI::dbSendQuery(con, update.query)
  
  # Remove the original column 
  alter.query.2 <- paste0("ALTER TABLE nis DROP COLUMN ", column)
  print(alter.query.2)
  DBI::dbSendQuery(con, alter.query.2)
  
  # Re-create the original column with the new type 
  alter.query.3 <- paste0("ALTER TABLE nis ADD COLUMN ", column, " INTEGER")
  print(alter.query.3)
  DBI::dbSendQuery(con, alter.query.3)
  
  # Move data from temporary column to new column 
  update.query.2 <- paste0("UPDATE nis set ", column, "=", column.tmp)
  print(update.query.2)
  DBI::dbSendQuery(con, update.query.2)
 
  # Drop the temporary column 
  alter.query.4 <- paste0("ALTER TABLE nis DROP COLUMN ", column.tmp)
  print(alter.query.4)
  DBI::dbSendQuery(con, alter.query.4)
  
  DBI::dbGetQuery(con, paste0("SELECT AVG(", column, ") FROM nis"))
} 

column.names <- readLines('data/convert-varchar-columns-to-doubles.txt')
for (column in column.names) {
  column.tmp <- paste0(column, "_tmp")
  alter.query <- paste0("ALTER TABLE nis ADD COLUMN ", column.tmp, " DOUBLE PRECISION")
  print(alter.query)
  DBI::dbSendQuery(con, alter.query)
  
  # Set data in temporary column to original data 
  update.query <- paste0("UPDATE nis SET ", column.tmp, "=", column)
  print(update.query)
  DBI::dbSendQuery(con, update.query)
  
  # Remove the original column 
  alter.query.2 <- paste0("ALTER TABLE nis DROP COLUMN ", column)
  print(alter.query.2)
  DBI::dbSendQuery(con, alter.query.2)
  
  # Re-create the original column with the new type 
  alter.query.3 <- paste0("ALTER TABLE nis ADD COLUMN ", column, " DOUBLE PRECISION")
  print(alter.query.3)
  DBI::dbSendQuery(con, alter.query.3)
  
  # Move data from temporary column to new column 
  update.query.2 <- paste0("UPDATE nis set ", column, "=", column.tmp)
  print(update.query.2)
  DBI::dbSendQuery(con, update.query.2)
  
  # Drop the temporary column 
  alter.query.4 <- paste0("ALTER TABLE nis DROP COLUMN ", column.tmp)
  print(alter.query.4)
  DBI::dbSendQuery(con, alter.query.4)
  
  DBI::dbGetQuery(con, paste0("SELECT AVG(", column, ") FROM nis"))
} 