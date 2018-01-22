library('MonetDB.R')
library('MonetDBLite')
library('dplyr')
library('dbplyr')
library('DBI')

#setwd('/home/bdetweiler/src/Data_Science/stat-8960-capstone-project/')
# con <- DBI::dbConnect(MonetDBLite::MonetDBLite(), "data/nis_db")
create.nis.codes.table <- readLines('data/sql/nis-codes-create-table.sql')
create.nis.codes.table <- paste0(create.nis.codes.table, collapse=' ')
create.nis.codes.table
DBI::dbSendQuery(con, create.nis.codes.table)

# I10 Procedure codes
i10prf.codes.df <- read.csv('data/nis-codes-i10prf.csv', sep=';')
head(i10prf.codes.df)

DBI::dbWriteTable(con, "nis_code", i10prf.codes.df, append=TRUE, row.names=FALSE)

# I10 Procedure codes
i10dxf.codes.df <- read.csv('data/nis-codes-i10dxf.csv', sep=';')
head(i10dxf.codes.df)

DBI::dbWriteTable(con, "nis_code", i10dxf.codes.df, append=TRUE, row.names=FALSE)

# I9 Procedure codes
i9prf.codes.df <- read.csv('data/nis-codes-i9prf.csv', sep=';', colClasses=c("character", "character", "character"))
head(i9prf.codes.df)

DBI::dbWriteTable(con, "nis_code", i9prf.codes.df, append=TRUE, row.names=FALSE)

# I9 Diagnosis codes
i9dxf.codes.df <- read.csv('data/nis-codes-i9dxf.csv', sep=';')
head(i9dxf.codes.df)

DBI::dbWriteTable(con, "nis_code", i9dxf.codes.df, append=TRUE, row.names=FALSE)


MonetDBLite::monetdblite_shutdown()
