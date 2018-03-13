library('MonetDB.R')
#install.packages('MonetDBLite')
library('MonetDBLite')
library('dplyr')
library('tidyverse')
library('DBI')
library('beepr')


setwd('/home/bdetweiler/src/Data_Science/stat-8960-capstone-project/')
# MonetDBLite::monetdblite_shutdown()
con <- DBI::dbConnect(MonetDBLite::MonetDBLite(), "data/nrd_db")

# Get the C. diff file with all the DX and PR descriptions
cdiff.nrd <- read_csv('data/cdiff-nrd-desc.csv')
cdiff.nrd$key_nrd <- as.character(cdiff.nrd$key_nrd)

ccs <- read_csv('data/nis-ccs-codes.csv')
fmt.ccs.code <- ccs %>% filter(Code == 'G0455') %>% pull(CCS)

nrd.visitlink <- cdiff.nrd %>% filter(dx1 == '00845') %>% pull(nrd_visitlink) %>% unique()


# This query was taking forever, so I'm just going to do it in a loop so I can monitor the progress
#sql <- paste0("SELECT * ",
              #"  FROM nrd ",
              #" WHERE nrd_visitlink IN ('",
              #paste0(nrd.visitlink, collapse="','"),
              #"')")


overall.start <- Sys.time()
df <- data_frame()
  
chunk <- round(length(nrd.visitlink) / (round(log(length(nrd.visitlink),  base=2))))

iter <- round(length(nrd.visitlink) / chunk)

i <- 0
#for (i in 1:length(nrd.visitlink)) {
for (count in 1:(iter + 1)) {
  
  prev <- i + 1
  i <- count * chunk 
  
  if (i > length(nrd.visitlink)) {
    i <- length(nrd.visitlink)
  }
  print(nrd.visitlink[prev:i]) 
  print(count)

  loop.start <- Sys.time()
  
  print(paste0(i, "/", length(nrd.visitlink), " (", (i/length(nrd.visitlink)) * 100, "%): "))
  sql <- paste0("SELECT * ",
                "  FROM nrd ",
                " WHERE nrd_visitlink IN ('",
                paste0(nrd.visitlink[prev:i], collapse="','"),
                "')")
  q <- DBI::dbGetQuery(con, sql)
  df <- bind_rows(df, q)
  loop.end <- Sys.time()
  print(paste0("time: ", round(loop.end - loop.start, 6), " seconds"))
  print(paste0("Average: ", round((loop.end - overall.start) / i, 6), " seconds per query"))
  print(paste0("Average: ", (((round((loop.end - overall.start) / i, 6) * (length(nrd.visitlink) - i))/60)/60), " hours left"))
}

write_csv(df, 'data/nrd-cdiff-readmissions.csv')

overall.end <- Sys.time()
overall.end - overall.start
beep(3)
