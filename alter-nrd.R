#library("devtools")
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
####                              ALTER NRD TABLE - ADD DESCRIPTIONS                        ####
################################################################################################

alter.table.sql <- readLines('data/sql/nrd-alter-table-add-dx-pr-descriptions.sql')

for (i in alter.table.sql) {
  DBI::dbSendQuery(con, i)
}
