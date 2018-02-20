library('MonetDB.R')
library('MonetDBLite')
library('dplyr')
library('dbplyr')
library('DBI')

#setwd('/home/bdetweiler/src/Data_Science/stat-8960-capstone-project/')

# We have generated the headers and renamed a few manually - see data/headers.xlsx
years <- seq(from = 2001, to = 2014, by = 1)

for (year in years) {
  
  print(paste("Processing year", year))
  data.types.file <- paste0('data/r-data-types-nis', year, '.txt')
  data.types <- readLines(data.types.file)
  data.types <- data.types[which(data.types != "")]
  
  # Copy and paste the headers from the 2001 tab in headers.xlsx into a text file
  headers.file <- paste0('data/headers-nis', year, '.txt')
  headers <- readLines(headers.file)
  headers <- headers[which(headers != "")]

  # XXX: This takes about 5 minutes to read on my Intel i7
  # Note: We specify the header names because some will be different in the database - namely KEY (NIS_KEY) and YEAR (NIS_YEAR)
  # Note: We specify the colClasses beause these are specific to the database
  nis.file <- paste0('data/NIS', year, '.csv')
  nis <- read.csv(nis.file, 
                  stringsAsFactors = FALSE, 
                  col.names = headers,
                  colClasses = data.types)
  
  # NIS_KEY, NIS_YEAR, and AGE are expected to exist in all years
  
  # Get all headers from all years 2001-2014 in the order they appear in the database
  all.headers <- readLines('data/all-headers.txt')
  
  
  # Compare what's in the CSV to all headers. 
  missing.headers <- c()
  for (column in all.headers) {
    if (!(column %in% colnames(nis))) {
      missing.headers <- c(missing.headers, column)
      nis <- cbind(nis, as.character(c(NA)))
    }
  }
  
  all.headers.out.of.order <- c(headers, missing.headers)
  
  names(nis) <- all.headers.out.of.order
  
  # Sort columns in correct order 
  nis <- nis[,  all.headers]
  
  # MonetDB connection to a permanent file
  # Call the below line if you get an error about connecting
  # MonetDBLite::monetdblite_shutdown()
  
  # Get a connection to the database
  # con <- DBI::dbConnect(MonetDBLite::MonetDBLite(), "data/nis_db")
  # Write the CSV file to the database
  DBI::dbWriteTable(con, "nis", nis, append=TRUE, row.names = FALSE)
} 

beep(sound=3)

MonetDBLite::monetdblite_shutdown()
