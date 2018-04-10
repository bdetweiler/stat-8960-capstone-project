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

# 2015 is supposed to include comorbidities under the columns I10_CM* but there are none in my dataset
# So we are skipping 2015 in our analysis
# readm <- read_csv('data/nrd-cdiff-readmissions.csv')
# readm <- readm %>% filter(nrd_year < 2015)
# write_csv(readm, 'data/nrd-cdiff-readmissions-2010-2011-2012-2013-2014.csv')
readm <- read_csv('data/nrd-cdiff-readmissions-2010-2011-2012-2013-2014.csv')

# Arrange by year, patient, and visit date
readm <- readm %>%  arrange(nrd_year, nrd_visitlink, nrd_daystoevent)

# Create a backup for testing
back <- readm

PatientProfile <- setRefClass("PatientProfile",
                              fields = list(
                                id = "character",
                                nrd_year = "integer",
                                readmitted = "logical",
                                readmission.count = "integer",
                                died = "logical",
                                female = "integer",
                                aweekend = "integer",
                                pay1 = "integer",
                                index.event = "data.frame",
                                index.event.id = "character",
                                index.event.time = "integer",
                                age.start = "integer",
                                age.end = "integer",
                                cm_aids = "integer",
                                cm_alcohol = "integer",
                                cm_anemdef = "integer",
                                cm_arth = "integer",
                                cm_bldloss = "integer",
                                cm_chf = "integer",
                                cm_chrnlung = "integer",
                                cm_coag = "integer",
                                cm_depress = "integer",
                                cm_dm = "integer",
                                cm_dmcx = "integer",
                                cm_drug = "integer",
                                cm_htn_c = "integer",
                                cm_hypothy = "integer",
                                cm_liver = "integer",
                                cm_lymph = "integer",
                                cm_lytes = "integer",
                                cm_mets = "integer",
                                cm_neuro = "integer",
                                cm_obese = "integer",
                                cm_para = "integer",
                                cm_perivasc = "integer",
                                cm_psych = "integer",
                                cm_pulmcirc = "integer",
                                cm_renlfail = "integer",
                                cm_tumor = "integer",
                                cm_ulcer = "integer",
                                cm_valve = "integer",
                                cm_wghtloss = "integer",
                                
                                dx1 = "character",
                                dx2 = "character",
                                dx3 = "character",
                                dx4 = "character",
                                dx5 = "character",
                                dx6 = "character",
                                dx7 = "character",
                                dx8 = "character",
                                dx9 = "character",
                                dx10 = "character",
                                dx11 = "character",
                                dx12 = "character",
                                dx13 = "character",
                                dx14 = "character",
                                dx15 = "character",
                                dx16 = "character",
                                dx17 = "character",
                                dx18 = "character",
                                dx19 = "character",
                                dx20 = "character",
                                dx21 = "character",
                                dx22 = "character",
                                dx23 = "character",
                                dx24 = "character",
                                dx25 = "character",
                                dx26 = "character",
                                dx27 = "character",
                                dx28 = "character",
                                dx29 = "character",
                                dx30 = "character",
                                
                                pr1 = "character",
                                pr2 = "character",
                                pr3 = "character",
                                pr4 = "character",
                                pr5 = "character",
                                pr6 = "character",
                                pr7 = "character",
                                pr8 = "character",
                                pr9 = "character",
                                pr10 = "character",
                                pr11 = "character",
                                pr12 = "character",
                                pr13 = "character",
                                pr14 = "character",
                                pr15 = "character",
                                
                                prccs1 = "character",
                                prccs2 = "character",
                                prccs3 = "character",
                                prccs4 = "character",
                                prccs5 = "character",
                                prccs6 = "character",
                                prccs7 = "character",
                                prccs8 = "character",
                                prccs9 = "character",
                                prccs10 = "character",
                                prccs11 = "character",
                                prccs12 = "character",
                                prccs13 = "character",
                                prccs14 = "character",
                                prccs15 = "character",
                                
                                stratum = "integer",
                                cluster = "integer",
                                weight = "numeric"
                              ),
                              
                              methods = list(
                                init = function(df) {
                                  # The index event (first row in the dataframe) 
                                  idx <- df[1,] 
                                    
                                  # Get the key_nrd ID 
                                  id <<- idx$nrd_visitlink
                                  
                                  # Get the year 
                                  nrd_year <<- as.integer(idx$nrd_year)
                                
                                  # Determine whether patient died at any point on readmission
                                  died <<- (sum(df$died) >= 1)
                                  
                                  readmitted <<- (nrow(df) > 1)
                                  readmission.count <<- as.integer((nrow(df) - 1))
                                   
                                  # The patient can have a birthday between visits 
                                  age.start <<- min(df$age) 
                                  age.end <<- max(df$age) 
                                   
                                  female <<- idx$female
                                  aweekend <<- idx$aweekend
                                  pay1 <<- idx$pay1
                                  
                                  cm_aids <<- idx$cm_aids
                                  cm_alcohol <<- idx$cm_alcohol
                                  cm_anemdef <<- idx$cm_anemdef
                                  cm_arth <<- idx$cm_arth
                                  cm_bldloss <<- idx$cm_bldloss
                                  cm_chf <<- idx$cm_chf
                                  cm_chrnlung <<- idx$cm_chrnlung
                                  cm_coag <<- idx$cm_coag
                                  cm_depress <<- idx$cm_depress
                                  cm_dm <<- idx$cm_dm
                                  cm_dmcx <<- idx$cm_dmcx
                                  cm_drug <<- idx$cm_drug
                                  cm_htn_c <<- idx$cm_htn_c
                                  cm_hypothy <<- idx$cm_hypothy
                                  cm_liver <<- idx$cm_liver
                                  cm_lymph <<- idx$cm_lymph
                                  cm_lytes <<- idx$cm_lytes
                                  cm_mets <<- idx$cm_mets
                                  cm_neuro <<- idx$cm_neuro
                                  cm_obese <<- idx$cm_obese
                                  cm_para <<- idx$cm_para
                                  cm_perivasc <<- idx$cm_perivasc
                                  cm_psych <<- idx$cm_psych
                                  cm_pulmcirc <<- idx$cm_pulmcirc
                                  cm_renlfail <<- idx$cm_renlfail
                                  cm_tumor <<- idx$cm_tumor
                                  cm_ulcer <<- idx$cm_ulcer
                                  cm_valve <<- idx$cm_valve
                                  cm_wghtloss <<- idx$cm_wghtloss
                                  
                                  dx1 <<- as.character(idx$dx1)
                                  dx2 <<- as.character(idx$dx2)
                                  dx3 <<- as.character(idx$dx3)
                                  dx4 <<- as.character(idx$dx4)
                                  dx5 <<- as.character(idx$dx5)
                                  dx6 <<- as.character(idx$dx6)
                                  dx7 <<- as.character(idx$dx7)
                                  dx8 <<- as.character(idx$dx8)
                                  dx9 <<- as.character(idx$dx9)
                                  dx10 <<- as.character(idx$dx10)
                                  dx11 <<- as.character(idx$dx11)
                                  dx12 <<- as.character(idx$dx12)
                                  dx13 <<- as.character(idx$dx13)
                                  dx14 <<- as.character(idx$dx14)
                                  dx15 <<- as.character(idx$dx15)
                                  dx16 <<- as.character(idx$dx16)
                                  dx17 <<- as.character(idx$dx17)
                                  dx18 <<- as.character(idx$dx18)
                                  dx19 <<- as.character(idx$dx19)
                                  dx20 <<- as.character(idx$dx20)
                                  dx21 <<- as.character(idx$dx21)
                                  dx22 <<- as.character(idx$dx22)
                                  dx23 <<- as.character(idx$dx23)
                                  dx24 <<- as.character(idx$dx24)
                                  dx25 <<- as.character(idx$dx25)
                                  dx26 <<- as.character(idx$dx26)
                                  dx27 <<- as.character(idx$dx27)
                                  dx28 <<- as.character(idx$dx28)
                                  dx29 <<- as.character(idx$dx29)
                                  dx30 <<- as.character(idx$dx30)
                              
                                  pr1 <<- as.character(idx$pr1)
                                  pr2 <<- as.character(idx$pr2)
                                  pr3 <<- as.character(idx$pr3)
                                  pr4 <<- as.character(idx$pr4)
                                  pr5 <<- as.character(idx$pr5)
                                  pr6 <<- as.character(idx$pr6)
                                  pr7 <<- as.character(idx$pr7)
                                  pr8 <<- as.character(idx$pr8)
                                  pr9 <<- as.character(idx$pr9)
                                  pr10 <<- as.character(idx$pr10)
                                  pr11 <<- as.character(idx$pr11)
                                  pr12 <<- as.character(idx$pr12)
                                  pr13 <<- as.character(idx$pr13)
                                  pr14 <<- as.character(idx$pr14)
                                  pr15 <<- as.character(idx$pr15)
                              
                                  prccs1 <<- as.character(idx$prccs1)
                                  prccs2 <<- as.character(idx$prccs2)
                                  prccs3 <<- as.character(idx$prccs3)
                                  prccs4 <<- as.character(idx$prccs4)
                                  prccs5 <<- as.character(idx$prccs5)
                                  prccs6 <<- as.character(idx$prccs6)
                                  prccs7 <<- as.character(idx$prccs7)
                                  prccs8 <<- as.character(idx$prccs8)
                                  prccs9 <<- as.character(idx$prccs9)
                                  prccs10 <<- as.character(idx$prccs10)
                                  prccs11 <<- as.character(idx$prccs11)
                                  prccs12 <<- as.character(idx$prccs12)
                                  prccs13 <<- as.character(idx$prccs13)
                                  prccs14 <<- as.character(idx$prccs14)
                                  prccs15 <<- as.character(idx$prccs15)
                                   
                                  stratum <<- idx$nrd_stratum
                                  cluster <<- idx$hosp_nrd
                                  weight <<- idx$discwt
                                 },
                                 
                                 getIndexEvent = function() {
                                   return(df %>% filter(key_nrd == index.event.id))
                                 },
                                 
                                 getAsDataFrame = function() {
                                   pat.df <- data.frame(id = id,
                                                        nrd_year = nrd_year,
                                                        readmitted = as.numeric(readmitted),
                                                        readmission.count = readmission.count,
                                                        died = as.numeric(died),
                                                        age.start = age.start,
                                                        age.end = age.end,
                                                        
                                                        female = female,
                                                        aweekend = aweekend,
                                                        pay1 = pay1,
                                                        
                                                        cm_aids = cm_aids,
                                                        cm_alcohol = cm_alcohol,
                                                        cm_anemdef = cm_anemdef,
                                                        cm_arth = cm_arth,
                                                        cm_bldloss = cm_bldloss,
                                                        cm_chf  = cm_chf,
                                                        cm_chrnlung  = cm_chrnlung,
                                                        cm_coag  = cm_coag,
                                                        cm_depress  = cm_depress,
                                                        cm_dm  = cm_dm,
                                                        cm_dmcx  = cm_dmcx,
                                                        cm_drug  = cm_drug,
                                                        cm_htn_c  = cm_htn_c,
                                                        cm_hypothy  = cm_hypothy,
                                                        cm_liver  = cm_liver,
                                                        cm_lymph  = cm_lymph,
                                                        cm_lytes  = cm_lytes,
                                                        cm_mets  = cm_mets,
                                                        cm_neuro = cm_neuro,
                                                        cm_obese  = cm_obese,
                                                        cm_para  = cm_para,
                                                        cm_perivasc  = cm_perivasc,
                                                        cm_psych  = cm_psych,
                                                        cm_pulmcirc  = cm_pulmcirc,
                                                        cm_renlfail  = cm_renlfail,
                                                        cm_tumor  = cm_tumor,
                                                        cm_ulcer  = cm_ulcer,
                                                        cm_valve  = cm_valve,
                                                        cm_wghtloss = cm_wghtloss,
                                                                              
                                                        dx1 = dx1,
                                                        dx2 = dx2,
                                                        dx3 = dx3,
                                                        dx4 = dx4,
                                                        dx5 = dx5,
                                                        dx6 = dx6,
                                                        dx7 = dx7,
                                                        dx8 = dx8,
                                                        dx9 = dx9,
                                                        dx10 = dx10,
                                                        dx11 = dx11,
                                                        dx12 = dx12,
                                                        dx13 = dx13,
                                                        dx14 = dx14,
                                                        dx15 = dx15,
                                                        dx16 = dx16,
                                                        dx17 = dx17,
                                                        dx18 = dx18,
                                                        dx19 = dx19,
                                                        dx20 = dx20,
                                                        dx21 = dx21,
                                                        dx22 = dx22,
                                                        dx23 = dx23,
                                                        dx24 = dx24,
                                                        dx25 = dx25,
                                                        dx26 = dx26,
                                                        dx27 = dx27,
                                                        dx28 = dx28,
                                                        dx29 = dx29,
                                                        dx30 = dx30,
                                                    
                                                        pr1 = pr1,
                                                        pr2 = pr2,
                                                        pr3 = pr3,
                                                        pr4 = pr4,
                                                        pr5 = pr5,
                                                        pr6 = pr6,
                                                        pr7 = pr7,
                                                        pr8 = pr8,
                                                        pr9 = pr9,
                                                        pr10 = pr10,
                                                        pr11 = pr11,
                                                        pr12 = pr12,
                                                        pr13 = pr13,
                                                        pr14 = pr14,
                                                        pr15 = pr15,
                                                    
                                                        prccs1 = prccs1,
                                                        prccs2 = prccs2,
                                                        prccs3 = prccs3,
                                                        prccs4 = prccs4,
                                                        prccs5 = prccs5,
                                                        prccs6 = prccs6,
                                                        prccs7 = prccs7,
                                                        prccs8 = prccs8,
                                                        prccs9 = prccs9,
                                                        prccs10 = prccs10,
                                                        prccs12 = prccs11,
                                                        prccs13 = prccs12,
                                                        prccs14 = prccs13,
                                                        prccs15 = prccs14,
                                   
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
  if (is.null(dim(df)) || nrow(df) == 0) {
    return(TRUE)
  }
  
  should.exclude <- FALSE

  # If the patient died on the index admission, exclude it
  if (!is.na(df[1,]$died) && df[1,]$died == 1) {
    should.exclude <- TRUE 
    # print("Excluding because they died")
  }

  # If the patient's length of stay is NA or 0, exclude it 
  if (!is.na(df[1,]$los) && df[1,]$los == 0) {
    should.exclude <- TRUE
    # print("Excluding because the LOS was NA or 0")
  }
 
  # Exclude infants 
  if (!any(is.na(df$age))) {
    if (min(df$age) == 0) {
      should.exclude <- TRUE
      # print("Excluding because the age was NA or 0")
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
# dim(readm)
# REMOVME: For testing only

# readm <- head(readm, 20000)

# dim(readm)
# REMOVME: For testing only
dim(readm)

# d-days 
#readmission.windows <- c(90, 60, 30, 14, 7)
readmission.windows <- c(14, 7)
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
    print(paste0(d, " day readmissions: ", nrow(readm), " rows left"))
    
    if (nrow(readm) == 0) {
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
    
    #print(paste0("Patient [", id, "], year [", yr, "]")) 
    #print(paste0("records: ", dim(patient.df)[[1]]))
    
    # patient.df %>% select(matches("^prccs[0-9]+$|^pr[0-9]+$|dx1_desc|dmonth|nrd_year"))
  
    while (TRUE) {
     
      index.event.df <- NA
      readmission.event.df <- NA
      final.df <- NA
  
      remaining.df <- patient.df
      
      # Loop over remaining r
      for (j in 1:nrow(patient.df)) {
        row <- patient.df[j,]  
        remaining.df <- remaining.df[-1,] 
        # If we don't have an index event yet, find one
        if (is.null(dim(index.event.df))) {
          #print("We don't have an index event yet. Find one...")
          
          index.event.df <- row %>% 
            dplyr::select(matches("key_nrd|nrd_daystoevent|^los$|^dx[0-9]+$")) %>%
            melt(id.vars=c("key_nrd", "nrd_daystoevent", "los")) %>%
            filter(value == "00845") 
        
          if (nrow(index.event.df) == 0) {
            index.event.df <- NA 
          } else {
            ### We have an index event!
            #print("WE HAVE AN INDEX EVENT!")
            index.event.df <- row
            final.df <- row
          }
        } else {
          #print("Find a readmission event...")
          # We have an index event, let's find readmissions
          readmission.event.df <- row %>% 
            dplyr::select(matches("key_nrd|nrd_daystoevent|^los$|^dx[0-9]+$")) %>%
            melt(id.vars=c("key_nrd", "nrd_daystoevent", "los")) %>%
            filter(value == "00845")

          # If there is no C. diff in this row, it is not a readmission 
          if (nrow(readmission.event.df) == 0) {
            #print("No readmissions.")
            readmission.event.df <- NA
          } else {
            #print("Might have a readmission. Let's see if it falls within our window...")
            # If there is a C. diff, see if it was less than or equal to our d
            if (calculate.readmission.window(index.event.df, row) <= d) {
              #print(paste0("FOUND READMISSION: [", calculate.readmission.window(index.event.df, row), "] days"))
              readmission.event.df <- row
              final.df <- bind_rows(index.event.df, readmission.event.df)
            } else {
              #print(paste0("No readmission. Readmission days: ", calculate.readmission.window(index.event.df, row)))
              # Else, this event falls outside of our d. It is now an index event. 
              # We need to save the previous index event and then reassign this as the current index event
              if (!exclude(final.df)) {
                #print("Saving Patient Record!") 
                pat <- PatientProfile$new()
                pat$init(final.df)
                pat.list[[i]] <- pat
                i <- i + 1
              }
             
              #print("We have a NEW index event!") 
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
        #print("Adding new patient record")
        pat <- PatientProfile$new()
        pat$init(final.df)
        pat.list[[i]] <- pat
        i <- i + 1
      } 
  
      patient.df <- remaining.df 

      if (nrow(patient.df) == 0) {
        break;
      }
    }
  
    # Remove those records from the df
    readm <- readm %>%
      filter(!(nrd_visitlink == id & nrd_year == yr))
  }

  file.name <- paste0("data/cdiff-readmissions-", d, "-day-window.csv") 
  pat.df <- data.frame()
 
  for (patient in pat.list) {
    pat.df <- bind_rows(pat.df, patient$getAsDataFrame())  
  }
 
  write_csv(pat.df, file.name) 

}
