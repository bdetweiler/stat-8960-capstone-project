library("devtools")
# install.packages( c("MonetDB.R", "MonetDBLite") , repos=c("http://dev.monetdb.org/Assets/R/", "http://cran.rstudio.com/"))
library('MonetDB.R')
library('MonetDBLite')
library('dplyr')
library('dbplyr')
library('DBI')

# setwd('/home/bdetweiler/src/Data_Science/stat-8960-capstone-project')

# MonetDB connection to a permanent file
# Call the below line if you get an error about connecting
# MonetDBLite::monetdblite_shutdown()
con <- DBI::dbConnect(MonetDBLite::MonetDBLite(), "data/nrd_db")

################################################################################################
####                              CREATE HOSPITAL TABLE                                     ####
################################################################################################

create.hospital.table.sql <- readLines('data/sql/nrd-hospital-create-table.sql')
create.hospital.table.sql <- paste(create.hospital.table.sql, collapse = "")

DBI::dbSendQuery(con, create.hospital.table.sql)


# DBI::dbSendQuery(con, "DROP TABLE nrd")
# DBI::dbGetQuery(con, "SELECT COUNT(nis_key) as count FROM nrd")
# Create NRD table
create.nrd.table.sql <- readLines('data/sql/nrd-create-table.sql')
create.nrd.table.sql <- paste(create.nrd.table.sql, collapse = "")

DBI::dbSendQuery(con, create.nrd.table.sql)

