library('MonetDB.R')
install.packages('MonetDBLite')
library('MonetDBLite')
library('dplyr')
library('DBI')
library('beepr')


#setwd('/home/bdetweiler/src/Data_Science/stat-8960-capstone-project/')

# We have generated the headers and renamed a few manually - see data/headers.xlsx

# First, get the line count and divide it by 4:
#
# wc -l NRD_2010_Core_V2.CSV | awk '{print $1"/4"}' | bc
# 3476902
# 
# The files are too large when merging, so we have to split them first. We'll also get rid of the V2 to keep things consistent.
#
# split -l3476902 -d NRD_2010_Core_V2.CSV NRD_2010_Core_split
# split -l3476902 -d NRD_2010_DX_PR_GRPS_V2.CSV NRD_2010_DX_PR_GRPS_split_
# split -l3476902 -d NRD_2010_Severity_V2.CSV NRD_2010_Severity_split_

# This produces split files 01-04 for each CSV

#MonetDBLite::monetdblite_shutdown()
con <- DBI::dbConnect(MonetDBLite::MonetDBLite(), "data/nrd_db")

# TODO: 2014 when we get the data!
years <- seq(from = 2010, to = 2015, by = 1)

for (year in years) {
  print(paste("Processing year", year))
  
  data.types.file <- paste0('data/r-data-types-nrd-hospital-', year, '.txt')
  data.types <- readLines(data.types.file)
  data.types <- data.types[which(data.types != "")]
  
  headers.file <- paste0('data/headers-nrd-hospital-', year, '.txt')
  headers <- readLines(headers.file)
  headers <- headers[which(headers != "")]

  # Note: We specify the header names because some will be different in the database - namely KEY (nrd_KEY) and YEAR (nrd_YEAR)
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


all.headers <- readLines('data/all-headers-nrd.txt')
all.headers <- all.headers[which(all.headers != "")]

# XXX: We'll process 2015 separately since they changed everything around that year
years <- seq(from = 2010, to = 2013, by = 1)

for (year in years) {

  print(paste("Processing year", year))
  
  data.types.file <- paste0('data/r-data-types-nrd-core-', year, '.txt')
  data.types <- readLines(data.types.file)
  data.types <- data.types[which(data.types != "")]

  # Copy and paste the headers from the 2001 tab in headers.xlsx into a text file
  headers.file <- paste0('data/headers-nrd-core-', year, '.txt')
  headers <- readLines(headers.file)
  headers <- headers[which(headers != "")]
  
  # Get the Dx Pr data

  dx.data.types.file <- paste0('data/r-data-types-nrd-dx-pr-grps-', year, '.txt')
  dx.data.types <- readLines(dx.data.types.file)
  dx.data.types <- dx.data.types[which(dx.data.types != "")]

  # Copy and paste the headers from the 2001 tab in headers.xlsx into a text file
  dx.headers.file <- paste0('data/headers-nrd-dx-pr-grps-', year, '.txt')
  dx.headers <- readLines(dx.headers.file)
  dx.headers <- dx.headers[which(dx.headers != "")]
  
  # Get the Severity

  severity.data.types.file <- paste0('data/r-data-types-nrd-severity-', year, '.txt')
  severity.data.types <- readLines(severity.data.types.file)
  severity.data.types <- severity.data.types[which(severity.data.types != "")]
 
  # Copy and paste the headers from the 2001 tab in headers.xlsx into a text file
  severity.headers.file <- paste0('data/headers-nrd-severity-', year, '.txt')
  severity.headers <- readLines(severity.headers.file)
  severity.headers <- severity.headers[which(severity.headers != "")]

  # XXX: This takes about 5 minutes to read on my Intel i7
  # Note: We specify the header names because some will be different in the database - namely KEY (nrd_KEY) and YEAR (nrd_YEAR)
  # Note: We specify the colClasses beause these are specific to the database
  
  for(i in 0:3) {
    
    nrd.file <- paste0('data/NRD', year, '/NRD_', year, '_Core_split_0', i)
    
    print(paste0("processing split file ", nrd.file))
    
    nrd <- read.csv(nrd.file, 
                    stringsAsFactors = FALSE, 
                    header = FALSE,
                    col.names = headers,
                    colClasses = data.types)


    nrd.dx.file <- paste0('data/NRD', year, '/NRD_', year, '_DX_PR_GRPS_split_0', i)
   
    print(paste0("processing split file ", nrd.dx.file))

    nrd.dx <- read.csv(nrd.dx.file, 
                       stringsAsFactors = FALSE, 
                       header = FALSE,
                       col.names = dx.headers,
                       colClasses = dx.data.types)

    nrd.severity.file <- paste0('data/NRD', year, '/NRD_', year, '_Severity_split_0', i)
    
    print(paste0("processing split file ", nrd.severity.file))
    
    nrd.severity <- read.csv(nrd.severity.file, 
                       stringsAsFactors = FALSE, 
                       header = FALSE,
                       col.names = severity.headers,
                       colClasses = severity.data.types)

    print("merging...")

    nrd.merged <- merge(merge(nrd, nrd.dx, by="key_nrd"), nrd.severity, by="key_nrd")


    nrd.merged <- nrd.merged %>% select(-hosp_nrd.x, -hosp_nrd.y)
 
    
    # Compare what's in the CSV to all headers. 
    missing.headers <- c() 
    for (column in all.headers) {
      if (!(column %in% colnames(nrd.merged))) {
        missing.headers <- c(missing.headers, column)
        nrd.merged <- cbind(nrd.merged, as.character(c(NA)))
      }
    }

    # We need key_nrd at the front and hosp_nrd at the end
    all.headers.out.of.order <- c(headers, dx.headers, severity.headers, missing.headers)
    all.headers.out.of.order <- unique(c('key_nrd', all.headers.out.of.order))
    all.headers.out.of.order <- all.headers.out.of.order[which(all.headers.out.of.order != 'hosp_nrd')]
    all.headers.out.of.order <- c(all.headers.out.of.order, 'hosp_nrd')

    nrd.hosp_nrd <- nrd.merged$hosp_nrd

    # This is fugly. The hosp_nrd is 288 but we need it at the end -_- Sorrryyyyyyyy for this.
    hosp_nrd_column <- which(colnames(nrd.merged) == 'hosp_nrd')
    nrd.merged <- nrd.merged[,-hosp_nrd_column]
    nrd.merged <- cbind(nrd.merged, hosp_nrd=nrd.hosp_nrd)
    colnames(nrd.merged) <- all.headers.out.of.order

    # Sort columns in correct order 
    nrd.merged <- nrd.merged[,  all.headers]
    # MonetDB connection to a permanent file
    # Call the below line if you get an error about connecting
    # MonetDBLite::monetdblite_shutdown()
    
    # Get a connection to the database
    #con <- DBI::dbConnect(MonetDBLite::MonetDBLite(), "data/nrd_db")
    #q <- DBI::dbGetQuery(con, "select nrd_year, nrd_key from nrd where nrd_key like '%0003328%'")
    #q
    # Write the CSV file to the database
    print("Writing to db...")
    
    DBI::dbWriteTable(con, "nrd", nrd.merged, append=TRUE, row.names = FALSE)
  }
} 

# TODO: NRD 2015 must be done separately

year <- 2015
print(paste("Processing year", year))

data.types.file <- paste0('data/r-data-types-nrd-core-', year, '.txt')
data.types <- readLines(data.types.file)
data.types <- data.types[which(data.types != "")]

# Copy and paste the headers from the 2001 tab in headers.xlsx into a text file
headers.file <- paste0('data/headers-nrd-core-', year, '.txt')
headers <- readLines(headers.file)
headers <- headers[which(headers != "")]

# Get the Q1-Q3 Dx Pr data

q1q3.dx.data.types.file <- paste0('data/r-data-types-nrd-dx-pr-grps-', year, '-Q1-Q3.txt')
q1q3.dx.data.types <- readLines(q1q3.dx.data.types.file)
q1q3.dx.data.types <- q1q3.dx.data.types[which(q1q3.dx.data.types != "")]

# Copy and paste the headers from the 2001 tab in headers.xlsx into a text file
q1q3.dx.headers.file <- paste0('data/headers-nrd-dx-pr-grps-', year, '-Q1-Q3.txt')
q1q3.dx.headers <- readLines(q1q3.dx.headers.file)
q1q3.dx.headers <- q1q3.dx.headers[which(q1q3.dx.headers != "")]

# Get the Q1-Q3 Severity

q1q3.severity.data.types.file <- paste0('data/r-data-types-nrd-severity-', year, '-Q1-Q3.txt')
q1q3.severity.data.types <- readLines(q1q3.severity.data.types.file)
q1q3.severity.data.types <- q1q3.severity.data.types[which(q1q3.severity.data.types != "")]

# Copy and paste the headers from the 2001 tab in headers.xlsx into a text file
q1q3.severity.headers.file <- paste0('data/headers-nrd-severity-', year, '-Q1-Q3.txt')
q1q3.severity.headers <- readLines(q1q3.severity.headers.file)
q1q3.severity.headers <- q1q3.severity.headers[which(q1q3.severity.headers != "")]


nrd.file <- paste0('data/NRD', year, '/NRD_', year, '_Core.CSV')
print(paste0("processing file ", nrd.file))

nrd <- read.csv(nrd.file, 
                stringsAsFactors = FALSE, 
                header = FALSE,
                col.names = headers,
                colClasses = data.types)


all.headers <- readLines('data/all-headers-nrd.txt')
all.headers <- all.headers[which(all.headers != "")]

#### Process the split DX and Severity files, merge and insert.
for (i in 0:3) {
  q1q3.nrd.dx.file <- paste0('data/NRD', year, '/NRD_', year, 'Q1Q3_DX_PR_GRPS_split_0', i)

  print(paste0("processing split file ", q1q3.nrd.dx.file))

  q1q3.nrd.dx <- read.csv(q1q3.nrd.dx.file, 
                     stringsAsFactors = FALSE, 
                     header = FALSE,
                     col.names = q1q3.dx.headers,
                     colClasses = q1q3.dx.data.types)
  
  str(q1q3.nrd.dx, list.len=999)
  
  q1q3.nrd.severity.file <- paste0('data/NRD', year, '/NRD_', year, 'Q1Q3_Severity_split_0', i)
  
  print(paste0("processing split file ", q1q3.nrd.severity.file))
  
  q1q3.nrd.severity <- read.csv(q1q3.nrd.severity.file, 
                                stringsAsFactors = FALSE, 
                                header = FALSE,
                                col.names = q1q3.severity.headers,
                                colClasses = q1q3.severity.data.types)
  
  
  print("merging Q1-Q3...")

  q1q3.nrd.merged <- merge(merge(nrd, q1q3.nrd.dx, by="key_nrd"), q1q3.nrd.severity, by="key_nrd")
  q1q3.nrd.merged <- q1q3.nrd.merged %>% select(-hosp_nrd.x, -hosp_nrd.y)
  
  # Compare what's in the CSV to all headers. 
  missing.headers <- c() 
  for (column in all.headers) {
    if (!(column %in% colnames(q1q3.nrd.merged))) {
      missing.headers <- c(missing.headers, column)
      q1q3.nrd.merged <- cbind(q1q3.nrd.merged, as.character(c(NA)))
    }
  }

  # We need key_nrd at the front and hosp_nrd at the end
  all.headers.out.of.order <- c(headers, q1q3.dx.headers, q1q3.severity.headers, missing.headers)
  all.headers.out.of.order <- unique(c('key_nrd', all.headers.out.of.order))
  all.headers.out.of.order <- all.headers.out.of.order[which(all.headers.out.of.order != 'hosp_nrd')]
  all.headers.out.of.order <- c(all.headers.out.of.order, 'hosp_nrd')

  q1q3.nrd.hosp_nrd <- q1q3.nrd.merged$hosp_nrd

  # This is fugly. The hosp_nrd is 288 but we need it at the end -_- Sorrryyyyyyyy for this.
  hosp_nrd_column <- which(colnames(q1q3.nrd.merged) == 'hosp_nrd')
  q1q3.nrd.merged <- q1q3.nrd.merged[,-hosp_nrd_column]
  q1q3.nrd.merged <- cbind(q1q3.nrd.merged, hosp_nrd=q1q3.nrd.hosp_nrd)
  colnames(q1q3.nrd.merged) <- all.headers.out.of.order

  # Sort columns in correct order 
  q1q3.nrd.merged <- q1q3.nrd.merged[,  all.headers]
  
  # MonetDB connection to a permanent file
  # Call the below line if you get an error about connecting
  # MonetDBLite::monetdblite_shutdown()
  
  # Get a connection to the database
  #con <- DBI::dbConnect(MonetDBLite::MonetDBLite(), "data/nrd_db")
  #q <- DBI::dbGetQuery(con, "select nrd_year, nrd_key from nrd where nrd_key like '%0003328%'")
  #q
  # Write the CSV file to the database
  print("Writing to db...")

  
  DBI::dbWriteTable(con, "nrd", q1q3.nrd.merged, append=TRUE, row.names = FALSE)
} 






#### Process Q4

# Get the Q4 Dx Pr data

q4.dx.data.types.file <- paste0('data/r-data-types-nrd-dx-pr-grps-', year, '-Q4.txt')
q4.dx.data.types <- readLines(q4.dx.data.types.file)
q4.dx.data.types <- q4.dx.data.types[which(q4.dx.data.types != "")]

# Copy and paste the headers from the 2001 tab in headers.xlsx into a text file
q4.dx.headers.file <- paste0('data/headers-nrd-dx-pr-grps-', year, '-Q4.txt')
q4.dx.headers <- readLines(q4.dx.headers.file)
q4.dx.headers <- q4.dx.headers[which(q4.dx.headers != "")]

# Get the Q4 Severity

q4.severity.data.types.file <- paste0('data/r-data-types-nrd-severity-', year, '-Q4.txt')
q4.severity.data.types <- readLines(q4.severity.data.types.file)
q4.severity.data.types <- q4.severity.data.types[which(q4.severity.data.types != "")]

# Copy and paste the headers from the 2001 tab in headers.xlsx into a text file
q4.severity.headers.file <- paste0('data/headers-nrd-severity-', year, '-Q4.txt')
q4.severity.headers <- readLines(q4.severity.headers.file)
q4.severity.headers <- q4.severity.headers[which(q4.severity.headers != "")]


for (i in 0:1) {

  q4.nrd.dx.file <- paste0('data/NRD', year, '/NRD_', year, 'Q4_DX_PR_GRPS_split_0', i)

  print(paste0("processing split file ", q4.nrd.dx.file))

  q4.nrd.dx <- read.csv(q4.nrd.dx.file, 
                     stringsAsFactors = FALSE, 
                     header = FALSE,
                     col.names = q4.dx.headers,
                     colClasses = q4.dx.data.types)
  
  q4.nrd.severity.file <- paste0('data/NRD', year, '/NRD_', year, 'Q4_Severity_split_0', i)
  
  print(paste0("processing split file ", q4.nrd.severity.file))
  
  q4.nrd.severity <- read.csv(q4.nrd.severity.file, 
                                stringsAsFactors = FALSE, 
                                header = FALSE,
                                col.names = q4.severity.headers,
                                colClasses = q4.severity.data.types)
  
 
  print("merging Q4...")
  q4.nrd.merged <- merge(merge(nrd, q4.nrd.dx, by="key_nrd"), q4.nrd.severity, by="key_nrd")
  q4.nrd.merged <- q4.nrd.merged %>% select(-hosp_nrd.x, -hosp_nrd.y)

  
  # Compare what's in the CSV to all headers. 
  missing.headers <- c() 
  for (column in all.headers) {
    if (!(column %in% colnames(q4.nrd.merged))) {
      missing.headers <- c(missing.headers, column)
      q4.nrd.merged <- cbind(q4.nrd.merged, as.character(c(NA)))
    }
  }
  
  # We need key_nrd at the front and hosp_nrd at the end
  all.headers.out.of.order <- c(headers, q4.dx.headers, q4.severity.headers, missing.headers)
  all.headers.out.of.order <- unique(c('key_nrd', all.headers.out.of.order))
  all.headers.out.of.order <- all.headers.out.of.order[which(all.headers.out.of.order != 'hosp_nrd')]
  all.headers.out.of.order <- c(all.headers.out.of.order, 'hosp_nrd')

  q4.nrd.hosp_nrd <- q4.nrd.merged$hosp_nrd

  # This is fugly. The hosp_nrd is 288 but we need it at the end -_- Sorrryyyyyyyy for this.
  hosp_nrd_column <- which(colnames(q4.nrd.merged) == 'hosp_nrd')
  q4.nrd.merged <- q4.nrd.merged[,-hosp_nrd_column]
  q4.nrd.merged <- cbind(q4.nrd.merged, hosp_nrd=q4.nrd.hosp_nrd)
  colnames(q4.nrd.merged) <- all.headers.out.of.order

  # Sort columns in correct order 
  q4.nrd.merged <- q4.nrd.merged[,  all.headers]
  
  # MonetDB connection to a permanent file
  # Call the below line if you get an error about connecting
  # MonetDBLite::monetdblite_shutdown()
  
  # Get a connection to the database
  #con <- DBI::dbConnect(MonetDBLite::MonetDBLite(), "data/nrd_db")
  #q <- DBI::dbGetQuery(con, "select nrd_year, nrd_key from nrd where nrd_key like '%0003328%'")
  #q
  # Write the CSV file to the database
  print("Writing to db...")
  
  DBI::dbWriteTable(con, "nrd", q4.nrd.merged, append=TRUE, row.names = FALSE)
} 

beep(3)

MonetDBLite::monetdblite_shutdown()
