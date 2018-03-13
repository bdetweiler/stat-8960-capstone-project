library('MonetDB.R')
#install.packages('MonetDBLite')
library('MonetDBLite')
library('dplyr')
library('tidyverse')
library('DBI')
library('beepr')

readm <- read_csv('data/nrd-cdiff-readmissions.csv')

readm <- readm %>%  arrange(nrd_year, nrd_visitlink, dmonth)
back <- readm

PatientProfile <- setRefClass("PatientProfile",
                              fields = list(id = "character",
                                            nrd_year = "integer",
                                            readmitted30 = "logical",
                                            readmitted90 = "logical",
                                            recvdFmt = "logical"),
                              methods = list(
                                 init = function(df) {
                                   if (!exists("df") | dim(df)[[1]] == 0) {
                                     return(NA)
                                   }
                                   
                                   id <<- df %>% 
                                     head(1) %>% 
                                     select(nrd_visitlink) %>% 
                                     pull(nrd_visitlink)
                                   
                                   nrd_year <<- df %>% 
                                     head(1) %>% 
                                     select(nrd_visitlink) %>% 
                                     pull(nrd_visitlink)
                                   
                                    end.of.event <- patient.df$los + patient.df$nrd_daystoevent
                                    readm.days <- patient.df$nrd_daystoevent[-1] - end.of.event[-length(end.of.event)]
                                    
                                    # TODO: Need logic to determine if readmission was due to C. diff
                                    
                                    # Need to populate the booleans based on C. diff AND/OR FMT
  
                                 },
                                 
                                 toString = function() {
                                   print(paste0("PatientProfile: id = [", id, "]"))
                                 }
                              )
                             )



readm <- back
print(dim(readm))

i <- 1
pat.list <- list()
while (TRUE) {
  print(dim(readm))  
  if (dim(readm)[[1]] == 0) {
    break;
  }
 
  # Get next patient ID 
  id <- readm %>% 
    head(1) %>% 
    pull(nrd_visitlink)
  
  # Get the year, just in case there is a non-unique patient ID in another year
  yr <- readm %>% 
    head(1) %>% 
    pull(nrd_year)

  # Get the records for that patient ID and year
  patient.df <- readm %>% filter(nrd_visitlink == id, nrd_year == yr)

  # Remove those records from the df
  readm <- readm %>%
    filter(!(nrd_visitlink == id & nrd_year == yr))

  # Build our new patient record
  patient <- PatientProfile$new()
  patient$init(patient.df)
 
  # Add it to the list 
  pat.list[[i]] <- patient

  i <- i + 1  
}

pat.list
