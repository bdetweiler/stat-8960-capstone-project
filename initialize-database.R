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
MonetDBLite::monetdblite_shutdown()
con <- DBI::dbConnect(MonetDBLite::MonetDBLite(), "data/nis_db")

DBI::dbSendQuery(con, "DROP TABLE nis")

# Create NIS table
create.table.sql <- readLines('data/sql/nis-create-table.sql')
create.table.sql <- paste(create.table.sql, collapse = "")
DBI::dbSendQuery(con, create.table.sql)

