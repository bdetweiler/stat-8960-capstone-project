library('MonetDB.R')
#install.packages('MonetDBLite')
library('MonetDBLite')
library('dplyr')
library('reshape2')
library('tidyverse')
library('DBI')
library('beepr')
library('survey')
library('stringr')


readm <- read_csv('data/nrd-cdiff-readmissions.csv')

# Arrange by year, patient, and visit date
readm <- readm %>%  arrange(nrd_year, nrd_visitlink, nrd_daystoevent)

# Create a backup for testing
back <- readm

PatientProfile <- setRefClass("PatientProfile",
                              fields = list(
                                id = "character",
                                nrd_year = "integer",
                                readmitted = "logical",
                                readmition.count = "integer",
                                fmt.readmitted = "logical",
                                received.fmt = "logical",
                                died = "logical",
                                # df = "data.frame",
                                index.event = "data.frame",
                                index.event.id = "character",
                                index.event.time = "integer",
                                #fmt.index.event = "data.frame",
                                #fmt.index.event.id = "character",
                                #fmt.index.event.time = "integer",
                                age.start = "integer",
                                age.end = "integer",
                                stratum = "integer",
                                cluster = "integer",
                                weight = "numeric"
                              ),
                              
                              methods = list(
                                 init = function(df) {
                                   
                                   # Keep a copy of the patient's records
                                   #df <<- df 
                                 
                                   # Get the key_nrd ID 
                                   id <<- df[1,]$nrd_visitlink
                                  
                                   # Get the year 
                                   nrd_year <<- as.integer(df[1,]$nrd_year)
                                
                                   # Determine whether patient died at any point on readmission
                                   died <<- (sum(df$died) >= 1)
                                
                                   # TODO:
                                   received.fmt <<- FALSE
                                   # TODO:
                                   fmt.readmitted <<- FALSE
                                   
                                   readmitted <<- ((df %>% nrow()) > 1)
                                   readmition.count <<- df[-1,] %>% nrow()
                                   
                                   # The patient can have a birthday between visits 
                                   age.start <<- min(df$age) 
                                   age.end <<- max(df$age) 
                                   
                                   stratum <<- df[1,]$nrd_stratum
                                   cluster <<- df[1,]$hosp_nrd
                                   weight <<- df[1,]$discwt
                                   
                                   
                                   
                                    #end.of.event <- df$los + df$nrd_daystoevent
                                    #readm.days <- df$nrd_daystoevent[-1] - end.of.event[-length(end.of.event)]
                                    
                                    # TODO: Need logic to determine if readmission was due to C. diff
                                    # readm.days gives me a vector of days to readmission
                                    # But I need to know what they were readmitted for 
                                    # and what they were treated for and how they were treated on the last visit
                                   
                                    # Obviously this is slightly complicated.
                                    
                                    # TODO: Need to populate the booleans based on C. diff AND/OR FMT
  
                                 },
                                 
                                 getIndexEvent = function() {
                                   return(df %>% filter(key_nrd == index.event.id))
                                 },
                                 
                                 getAsDataFrame = function() {
                                   pat.df <- data.frame(id = id,
                                                        nrd_year = nrd_year,
                                                        readmitted = as.numeric(readmitted),
                                                        readmition.count = readmition.count,
                                                        fmt.readmitted = as.numeric(fmt.readmitted),
                                                        received.fmt = as.numeric(received.fmt),
                                                        died = as.numeric(died),
                                                        age.start = age.start,
                                                        age.end = age.end,
                                                        stratum = stratum,
                                                        cluster = cluster,
                                                        weight = weight)
                                   return(pat.df)
                                 }
                                 
                              )
                             )

# Determines if we should exclude the entire patient record
exclude <- function(df) {
  # If the record is empty, exclude it obviously
  if (is.null(dim(df)) || dim(df)[[1]] == 0) {
    return(TRUE)
  }
  
  should.exclude <- FALSE

  # If the patient died on the index admission, exclude it
  if (!is.na(df[1,]$died) && df[1,]$died == 1) {
    should.exclude <- TRUE 
    print("Excluding because they died")
  }

  # If the patient's length of stay is NA or 0, exclude it 
  if (!is.na(df[1,]$los) && df[1,]$los == 0) {
    should.exclude <- TRUE
    print("Excluding because the LOS was NA or 0")
  }
 
  # Exclude infants 
  if (!any(is.na(df$age))) {
    if (min(df$age) == 0) {
      should.exclude <- TRUE
      print("Excluding because the age was NA or 0")
    }
  }

  return(should.exclude)
}

calculate.readmission.window <- function(index.admission.df, readmission.df) {
  if (is.null(dim(index.admission.df))) {
    return(0)
  }
  
  if (is.null(dim(readmission.df))) {
    return(0)
  }

  if (index.admission.df %>% nrow() != 1) {
    return(0)
  }
  
  if (readmission.df %>% nrow() != 1) {
    return(0)
  }

  
  return(readmission.df[1,]$nrd_daystoevent - (index.admission.df[1,]$nrd_daystoevent + index.admission.df[1,]$los))
   
}

#readm %>% filter(nrd_visitlink == "xkosnho")
#readm <- back
dim(readm)
# REMOVME: For testing only
readm <- readm %>% head(10000)
# REMOVME: For testing only
back <- readm
# readm %>% filter(prccs1 == "95") %>% select(nrd_visitlink)
# readm %>% filter(nrd_visitlink == "x03a7bw")
# readm <- readm %>% filter(nrd_visitlink == "x05kvr6")


# d-days
readmission.windows <- c(90, 60, 30, 14, 7)
#readmission.windows <- c(90)

# Loop over all the readmission windows
for (d in readmission.windows) {
  readm <- back
  i <- 1
  pat.list <- list()
  
  readm <- readm %>%
    filter(dmonth <= (12 - ceiling(d / 30))) %>%
    filter(!(nrd_year == 2015 & dmonth > (9 - ceiling(d / 30))))
  
  # Loop over entire C. diff dataset
  while (TRUE) {
    print(paste0(dim(readm)[[1]], " rows left"))
    
    if (dim(readm)[[1]] == 0) {
      print("FINISHED!")
      #beep(3)
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
    
    print(paste0("Patient [", id, "], year [", yr, "]")) 
    print(paste0("records: ", dim(patient.df)[[1]]))
    
    # patient.df %>% select(matches("^prccs[0-9]+$|^pr[0-9]+$|dx1_desc|dmonth|nrd_year"))
  
    while (TRUE) {
     
      index.event.df <- NA
      readmission.event.df <- NA
      final.df <- NA
  
      remaining.df <- patient.df
      
      # Loop over remaining r
      for (j in 1:dim(patient.df)[[1]]) {
        row <- patient.df[j,]  
        remaining.df <- remaining.df[-1,] 
        # If we don't have an index event yet, find one
        if (is.null(dim(index.event.df))) {
          print("We don't have an index event yet. Find one...")
          
          index.event.df <- row %>% 
            select(matches("key_nrd|nrd_daystoevent|^los$|^dx[0-9]+$")) %>%
            melt(id.vars=c("key_nrd", "nrd_daystoevent", "los")) %>%
            filter(value == "00845") 
        
          if (dim(index.event.df)[[1]] == 0) {
            index.event.df <- NA 
          } else {
            ### We have an index event!
            print("WE HAVE AN INDEX EVENT!")
            index.event.df <- row
            final.df <- row
          }
        } else {
          print("Find a readmission event...")
          # We have an index event, let's find readmissions
          readmission.event.df <- row %>% 
            select(matches("key_nrd|nrd_daystoevent|^los$|^dx[0-9]+$")) %>%
            melt(id.vars=c("key_nrd", "nrd_daystoevent", "los")) %>%
            filter(value == "00845")

          # If there is no C. diff in this row, it is not a readmission 
          if (dim(readmission.event.df)[[1]] == 0) {
            print("No readmissions.")
            readmission.event.df <- NA
          } else {
            print("Might have a readmission. Let's see if it falls within our window...")
            # If there is a C. diff, see if it was less than or equal to our d
            if (calculate.readmission.window(index.event.df, row) <= d) {
              print(paste0("FOUND READMISSION: [", calculate.readmission.window(index.event.df, row), "] days"))
              readmission.event.df <- row
              final.df <- bind_rows(index.event.df, readmission.event.df)
            } else {
              print(paste0("No readmission. Readmission days: ", calculate.readmission.window(index.event.df, row)))
              # Else, this event falls outside of our d. It is now an index event. 
              # We need to save the previous index event and then reassign this as the current index event
              if (!exclude(final.df)) {
                print("Saving Patient Record!") 
                pat <- PatientProfile$new()
                pat$init(final.df)
                pat.list[[i]] <- pat
                i <- i + 1
              }
             
              print("We have a NEW index event!") 
              index.event.df <- row
              readmission.event.df <- NA 
              final.df <- NA
            }
          }
        } # End else

        if (is.null(dim(index.event.df))) {
          readmission.event.df <- NA
        }
        
        
      } # End j loop
  
      if (!exclude(final.df)) {
        print("Adding new patient record")
        pat <- PatientProfile$new()
        pat$init(final.df)
        pat.list[[i]] <- pat
        i <- i + 1
      } 
  
      patient.df <- remaining.df 

      if (dim(patient.df)[[1]] == 0) {
        break;
      }
    }
  
    # Remove those records from the df
    readm <- readm %>%
      filter(!(nrd_visitlink == id & nrd_year == yr))
  }

  file.name <- paste0("cdiff-readmissions-", d, "-day-window.csv") 
  pat.df <- data.frame()
 
  for (patient in pat.list) {
    pat.df <- bind_rows(pat.df, patient$getAsDataFrame())  
  }
 
  write_csv(pat.df, file.name) 

}
