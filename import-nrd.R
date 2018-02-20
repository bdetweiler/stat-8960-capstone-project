library('MonetDB.R')
install.packages('MonetDBLite')
library('MonetDBLite')
library('dplyr')
library('dbplyr')
library('DBI')

#setwd('/home/bdetweiler/src/Data_Science/stat-8960-capstone-project/')

# We have generated the headers and renamed a few manually - see data/headers.xlsx

# MonetDBLite::monetdblite_shutdown()
con <- DBI::dbConnect(MonetDBLite::MonetDBLite(), "data/nrd_db")

# TODO: 2014 when we get the data!
years <- c(seq(from = 2010, to = 2013, by = 1), 2015)

for (year in years) {
  print(paste("Processing year", year))
  
  data.types.file <- paste0('data/r-data-types-nrd-hospital-', year, '.txt')
  data.types <- readLines(data.types.file)
  data.types <- data.types[which(data.types != "")]
  
  headers.file <- paste0('data/headers-nrd-hospital-', year, '.txt')
  headers <- readLines(headers.file)
  headers <- headers[which(headers != "")]

  # Note: We specify the header names because some will be different in the database - namely KEY (NIS_KEY) and YEAR (NIS_YEAR)
  # Note: We specify the colClasses beause these are specific to the database
  if (year < 2013) {
    version <- '_V2'
  } else {
    version <- ''
  }
  nrd.hospital.file <- paste0('data/NRD', year, '/NRD_', year, '_Hospital', version, '.CSV')

  nrd.hosp <- read.csv(nrd.hospital.file, 
                       stringsAsFactors = FALSE, 
                       col.names = headers,
                       colClasses = data.types)
  nrd.hosp <- nrd.hosp %>% 
    select(hosp_nrd,
           hosp_bedsize, 
           h_contrl,
           hosp_urcat4,
           hosp_ur_teach,
           nrd_stratum,
           n_disc_u,
           n_hosp_u,
           s_disc_u,
           s_hosp_u,
           total_disc,
           nrd_year)
  
  DBI::dbWriteTable(con, "nrd_hospital", nrd.hosp, append=TRUE, row.names = FALSE)
}





# TODO: Add 2014 when I get it
#years <- seq(from = 2010, to = 2015, by = 1)
years <- seq(from = 2010, to = 2013, by = 1)

paste0("data/NRD", year)

for (year in years) {
  
  #print(paste("Processing year", year))
  #data.types.file <- paste0('data/r-data-types-nis', year, '.txt')
  #data.types <- readLines(data.types.file)
  
  #data.types <- data.types[which(data.types != "")]
  
  # Copy and paste the headers from the 2001 tab in headers.xlsx into a text file
  #headers.file <- paste0('data/headers-nis', year, '.txt')
  #headers <- readLines(headers.file)
  #headers <- headers[which(headers != "")]

  # XXX: This takes about 5 minutes to read on my Intel i7
  # Note: We specify the header names because some will be different in the database - namely KEY (NIS_KEY) and YEAR (NIS_YEAR)
  # Note: We specify the colClasses beause these are specific to the database
  #nis.file <- paste0('data/NIS', year, '.csv')
  #nis <- read.csv(nis.file, 
                  #stringsAsFactors = FALSE, 
                  #col.names = headers,
                  #colClasses = data.types)
  
  # NIS_KEY, NIS_YEAR, and AGE are expected to exist in all years
  
  # Get all headers from all years 2001-2014 in the order they appear in the database
  #all.headers <- readLines('data/all-headers.txt')
  
  
  # Compare what's in the CSV to all headers. 
  #missing.headers <- c()
  #for (column in all.headers) {
    #if (!(column %in% colnames(nis))) {
      #missing.headers <- c(missing.headers, column)
      #nis <- cbind(nis, as.character(c(NA)))
    #}
  #}
  
  #all.headers.out.of.order <- c(headers, missing.headers)
  
  #names(nis) <- all.headers.out.of.order
  
  # Sort columns in correct order 
  #nis <- nis[,  all.headers]
  
  # MonetDB connection to a permanent file
  # Call the below line if you get an error about connecting
  # MonetDBLite::monetdblite_shutdown()
  
  # Get a connection to the database
  #con <- DBI::dbConnect(MonetDBLite::MonetDBLite(), "data/nis_db")
  #q <- DBI::dbGetQuery(con, "select nis_year, nis_key from nis where nis_key like '%0003328%'")
  #q
  # Write the CSV file to the database
  #DBI::dbWriteTable(con, "nis", nis, append=TRUE, row.names = FALSE)
} 

# TODO: NRD 2015 must be done separately




beep(sound=3)

MonetDBLite::monetdblite_shutdown()
