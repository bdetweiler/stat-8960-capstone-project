library('MonetDB.R')
#install.packages('MonetDBLite')
library('MonetDBLite')
library('dplyr')
library('tidyverse')
library('DBI')
library('beepr')


setwd('/home/bdetweiler/src/Data_Science/stat-8960-capstone-project/')
con <- DBI::dbConnect(MonetDBLite::MonetDBLite(), "data/nrd_db")
df <- data.frame(code=c(), desc=c(), year=c(), type=c())


# 2005 DX

icd9.dx.2005 <- readLines('data/ICD-9-CM-v23-2005/I9DX_DESC.txt')
icd9.dx.2005 <- gsub(' *$', '', icd9.dx.2005)

res <- lapply(strsplit(icd9.dx.2005, " "), function(x) c(x[1], paste(x[-1], collapse=" ")))

res <- data.frame(matrix(unlist(res), nrow=length(res), byrow=T))
colnames(res) <- c('code', 'desc')
df <- res %>% 
  mutate(year=2005, type='dx', icd=9) %>%
  bind_rows(df)

dim(df)


# 2005 PR

icd9.pr.2005 <- readLines('data/ICD-9-CM-v23-2005/I9SG_DESC.txt')
icd9.pr.2005 <- gsub(' *$', '', icd9.pr.2005)

res <- lapply(strsplit(icd9.pr.2005, " "), function(x) c(x[1], paste(x[-1], collapse=" ")))

res <- data.frame(matrix(unlist(res), nrow=length(res), byrow=T))
colnames(res) <- c('code', 'desc')

df <- res %>% 
  mutate(year=2005, type='pr', icd=9) %>%
  bind_rows(df)

dim(df)


# 2006 DX

icd9.dx.2006 <- readLines('data/ICD-9-CM-v24-2006/I9diagnosis.txt')

icd9.dx.2006 <- gsub(' *$', '', icd9.dx.2006)

res <- lapply(strsplit(icd9.dx.2006, " "), function(x) c(x[1], paste(x[-1], collapse=" ")))
res <- data.frame(matrix(unlist(res), nrow=length(res), byrow=T))

colnames(res) <- c('code', 'desc')
df <- res %>% 
  mutate(year=2006, type='dx', icd=9) %>%
  bind_rows(df)

dim(df)

# 2006 PR

icd9.pr.2006 <- readLines('data/ICD-9-CM-v24-2006/I9surgery.txt')
icd9.pr.2006 <- gsub(' *$', '', icd9.pr.2006)

res <- lapply(strsplit(icd9.pr.2006, " "), function(x) c(x[1], paste(x[-1], collapse=" ")))

res <- data.frame(matrix(unlist(res), nrow=length(res), byrow=T))
colnames(res) <- c('code', 'desc')

df <- res %>% 
  mutate(year=2006, type='pr', icd=9) %>%
  bind_rows(df)

dim(df)

# 2007 DX

icd9.dx.2007 <- readLines('data/ICD-9-CM-v25-2007/I9diagnosesV25.txt')

icd9.dx.2007 <- gsub(' *$', '', icd9.dx.2007)

res <- lapply(strsplit(icd9.dx.2007, " "), function(x) c(x[1], paste(x[-1], collapse=" ")))
res <- data.frame(matrix(unlist(res), nrow=length(res), byrow=T))

colnames(res) <- c('code', 'desc')
df <- res %>% 
  mutate(year=2007, type='dx', icd=9) %>%
  bind_rows(df)

dim(df)


# 2007 PR

icd9.pr.2007 <- readLines('data/ICD-9-CM-v25-2007/I9proceduresV25.txt')

icd9.pr.2007 <- gsub(' *$', '', icd9.pr.2007)

res <- lapply(strsplit(icd9.pr.2007, " "), function(x) c(x[1], paste(x[-1], collapse=" ")))

res <- data.frame(matrix(unlist(res), nrow=length(res), byrow=T))
colnames(res) <- c('code', 'desc')

df <- res %>% 
  mutate(year=2007, type='pr', icd=9) %>%
  bind_rows(df)

dim(df)

# 2008 DX

icd9.dx.2008 <- readLines('data/ICD-9-CM-v26-2008/V26 I-9 Diagnosis.txt')

icd9.dx.2008 <- gsub(' *$', '', icd9.dx.2008)

res <- lapply(strsplit(icd9.dx.2008, " "), function(x) c(x[1], paste(x[-1], collapse=" ")))

res <- data.frame(matrix(unlist(res), nrow=length(res), byrow=T))

colnames(res) <- c('code', 'desc')
head(df)
df <- res %>% 
  mutate(year=2008, type='dx', icd=9) %>%
  bind_rows(df)

dim(df)


# 2008 PR

icd9.pr.2008 <- readLines('data/ICD-9-CM-v26-2008/V26  I-9 Procedures.txt')

icd9.pr.2008 <- gsub(' *$', '', icd9.pr.2008)

res <- lapply(strsplit(icd9.pr.2008, " "), function(x) c(x[1], paste(x[-1], collapse=" ")))

res <- data.frame(matrix(unlist(res), nrow=length(res), byrow=T))
colnames(res) <- c('code', 'desc')

df <- res %>% 
  mutate(year=2008, type='pr', icd=9) %>%
  bind_rows(df)

dim(df)

# 2009 DX

icd9.dx.2009 <- read_csv('data/ICD-9-CM-v27-Q4-2009/V27LONG_SHORT_DX_110909u021012.csv', col_types = cols(.default = "c"))
icd9.dx.2009 <- icd9.dx.2009 %>% 
  select(-`SHORT DESCRIPTION`) %>%
  rename(code=`DIAGNOSIS CODE`, desc=`LONG DESCRIPTION`)

df <- icd9.dx.2009 %>% 
  mutate(year=2009, type='dx', icd=9) %>%
  bind_rows(df)

dim(df)


# 2009 PR
icd9.pr.2009 <- read_csv('data/ICD-9-CM-v27-Q4-2009/CMS27_DESC_LONG_SHORT_SG_092709.csv', col_types = cols(.default = "c"))

icd9.pr.2009 <- icd9.pr.2009 %>% 
  select(-`SHORT DESCRIPTION`) %>%
  rename(code=`PROCEDURE CODE`, desc=`LONG DESCRIPTION`)

df <- icd9.pr.2009 %>% 
  mutate(year=2009, type='pr', icd=9) %>%
  bind_rows(df)

dim(df)

# 2010 DX

icd9.dx.2010 <- readLines('data/ICD-9-CM-v28-Q4-2010/CMS28_DESC_LONG_DX.txt')
icd9.dx.2010 <- gsub(' *$', '', icd9.dx.2010)

res <- lapply(strsplit(icd9.dx.2010, " "), function(x) c(x[1], paste(x[-1], collapse=" ")))
res <- data.frame(matrix(unlist(res), nrow=length(res), byrow=T))

colnames(res) <- c('code', 'desc')
df <- res %>% 
  mutate(year=2010, type='dx', icd=9) %>%
  bind_rows(df)

dim(df)


# 2010 PR

icd9.pr.2010 <- readLines('data/ICD-9-CM-v28-Q4-2010/CMS28_DESC_LONG_SG.txt')

icd9.pr.2010 <- gsub(' *$', '', icd9.pr.2010)

res <- lapply(strsplit(icd9.pr.2010, " "), function(x) c(x[1], paste(x[-1], collapse=" ")))

res <- data.frame(matrix(unlist(res), nrow=length(res), byrow=T))
colnames(res) <- c('code', 'desc')

df <- res %>% 
  mutate(year=2010, type='pr', icd=9) %>%
  bind_rows(df)

dim(df)

# 2011 DX

icd9.dx.2011 <- readLines('data/ICD-9-CM-v29-Q4-2011/CMS29_DESC_LONG_DX.101111.txt')
icd9.dx.2011 <- gsub(' *$', '', icd9.dx.2011)

res <- lapply(strsplit(icd9.dx.2011, " "), function(x) c(x[1], paste(x[-1], collapse=" ")))
res <- data.frame(matrix(unlist(res), nrow=length(res), byrow=T))

colnames(res) <- c('code', 'desc')
df <- res %>% 
  mutate(year=2011, type='dx', icd=9) %>%
  bind_rows(df)

dim(df)


# 2011 PR

icd9.pr.2011 <- readLines('data/ICD-9-CM-v29-Q4-2011/CMS29_DESC_LONG_SG.txt')

icd9.pr.2011 <- gsub(' *$', '', icd9.pr.2011)

res <- lapply(strsplit(icd9.pr.2011, " "), function(x) c(x[1], paste(x[-1], collapse=" ")))

res <- data.frame(matrix(unlist(res), nrow=length(res), byrow=T))
colnames(res) <- c('code', 'desc')

df <- res %>% 
  mutate(year=2011, type='pr', icd=9) %>%
  bind_rows(df)

dim(df)

# 2012 DX

icd9.dx.2012 <- readLines('data/ICD-9-CM-v30-Q4-2012/CMS30_DESC_LONG_DX 080612.txt')
icd9.dx.2012 <- gsub(' *$', '', icd9.dx.2012)

res <- lapply(strsplit(icd9.dx.2012, " "), function(x) c(x[1], paste(x[-1], collapse=" ")))
res <- data.frame(matrix(unlist(res), nrow=length(res), byrow=T))

colnames(res) <- c('code', 'desc')
df <- res %>% 
  mutate(year=2012, type='dx', icd=9) %>%
  bind_rows(df)

dim(df)


# 2012 PR

icd9.pr.2012 <- readLines('data/ICD-9-CM-v30-Q4-2012/CMS30_DESC_LONG_SG.txt')

icd9.pr.2012 <- gsub(' *$', '', icd9.pr.2012)

res <- lapply(strsplit(icd9.pr.2012, " "), function(x) c(x[1], paste(x[-1], collapse=" ")))

res <- data.frame(matrix(unlist(res), nrow=length(res), byrow=T))
colnames(res) <- c('code', 'desc')

df <- res %>% 
  mutate(year=2012, type='pr', icd=9) %>%
  bind_rows(df)

dim(df)


# 2013 DX

icd9.dx.2013 <- readLines('data/ICD-9-CM-v31-Q4-2013/CMS31_DESC_LONG_DX.txt')
icd9.dx.2013 <- gsub(' *$', '', icd9.dx.2013)

res <- lapply(strsplit(icd9.dx.2013, " "), function(x) c(x[1], paste(x[-1], collapse=" ")))
res <- data.frame(matrix(unlist(res), nrow=length(res), byrow=T))

colnames(res) <- c('code', 'desc')
df <- res %>% 
  mutate(year=2013, type='dx', icd=9) %>%
  bind_rows(df)

dim(df)


# 2013 PR

icd9.pr.2013 <- readLines('data/ICD-9-CM-v31-Q4-2013/CMS31_DESC_LONG_SG.txt')

icd9.pr.2013 <- gsub(' *$', '', icd9.pr.2013)

res <- lapply(strsplit(icd9.pr.2013, " "), function(x) c(x[1], paste(x[-1], collapse=" ")))

res <- data.frame(matrix(unlist(res), nrow=length(res), byrow=T))
colnames(res) <- c('code', 'desc')

df <- res %>% 
  mutate(year=2013, type='pr', icd=9) %>%
  bind_rows(df)

dim(df)

# 2014 DX

icd9.dx.2014 <- readLines('data/ICD-9-CM-v32-Q4-2014/CMS32_DESC_LONG_DX.txt')
icd9.dx.2014 <- gsub(' *$', '', icd9.dx.2014)

res <- lapply(strsplit(icd9.dx.2014, " "), function(x) c(x[1], paste(x[-1], collapse=" ")))
res <- data.frame(matrix(unlist(res), nrow=length(res), byrow=T))

colnames(res) <- c('code', 'desc')
df <- res %>% 
  mutate(year=2014, type='dx', icd=9) %>%
  bind_rows(df)

dim(df)


# 2014 PR

icd9.pr.2014 <- readLines('data/ICD-9-CM-v32-Q4-2014/CMS32_DESC_LONG_SG.txt')

icd9.pr.2014 <- gsub(' *$', '', icd9.pr.2014)

res <- lapply(strsplit(icd9.pr.2014, " "), function(x) c(x[1], paste(x[-1], collapse=" ")))

res <- data.frame(matrix(unlist(res), nrow=length(res), byrow=T))
colnames(res) <- c('code', 'desc')

df <- res %>% 
  mutate(year=2014, type='pr', icd=9) %>%
  bind_rows(df)

dim(df)

# 2015 DX

icd10.dx.2015 <- readLines('data/ICD-10-CM-2015/2015-code-descriptions/icd10cm_order_2015.txt')

longest <- max(nchar(icd10.dx.2015))

icd10.dx.2015.df <- data_frame(code=trimws(substr(icd10.dx.2015, start = 7, stop = 14)), 
                               desc=trimws(substr(icd10.dx.2015, start = 78, stop = longest)))

df <- icd10.dx.2015.df %>% 
  mutate(year=2015, type='dx', icd=10) %>%
  bind_rows(df)

dim(df)

# 2015 PR
icd10.pr.2015 <- readLines('data/ICD-10-CM-2015/icd10pcs_order_2015.txt')

longest <- max(nchar(icd10.pr.2015))

icd10.pr.2015.df <- data_frame(code=trimws(substr(icd10.pr.2015, start = 7, stop = 14)), 
                               desc=trimws(substr(icd10.pr.2015, start = 78, stop = longest)))

df <- icd10.pr.2015.df %>% 
  mutate(year=2015, type='pr', icd=10) %>%
  bind_rows(df)

dim(df)

df <- df %>% select(year, type, icd, code, desc)



# Replace Unicode characters with ASCII
df <- df %>% 
  mutate(desc=replace(desc,
    (year < 2010 & type == 'dx' & code == '413'),
    "Friedlander's bacillus infection in conditions classified elsewhere and of unspecified site")) %>% 
  mutate(desc=replace(desc,
    (year >= 2010 & type == 'dx' & code == '0413'),
    "Friedlander's bacillus infection in conditions classified elsewhere and of unspecified site")) %>% 
  mutate(desc=replace(desc,
    (year >= 2010 & type == 'dx' & code == '04671'),  
    "Germstmann-Straussler-Scheinker syndrome")) %>%
  mutate(desc=replace(desc,
    (year >= 2010 & type == 'dx' & code == '4671'),  
    "Germstmann-Straussler-Scheinker syndrome")) %>%
  mutate(desc=replace(desc,
    (year >= 2010 & type == 'dx' & code == '38600'),  
    "Meniere's Disease, unspecified")) %>%
  mutate(desc=replace(desc,
    (year >= 2010 & type == 'dx' & code == '38601'),  
    "Active Meniere's disease, cochleovestibular")) %>%
  mutate(desc=replace(desc,
    (year >= 2010 & type == 'dx' & code == '38602'),  
    "Active Meniere's disease, cochlear")) %>%
  mutate(desc=replace(desc,
    (year >= 2010 & type == 'dx' & code == '38603'),  
    "Active Meniere's disease, vestibular")) %>%
  mutate(desc=replace(desc,
    (year >= 2010 & type == 'dx' & code == '38604'),  
    "Inactive Meniere's disease")) %>%
  mutate(desc=replace(desc,
    (year >= 2010 & type == 'dx' & code == '0413'),  
    "Friedlander's bacillus infection in conditions classified elsewhere and of unspecified site")) %>%
  mutate(desc=replace(desc,
    (year >= 2010 & type == 'dx' & code == '413'),  
    "Friedlander's bacillus infection in conditions classified elsewhere and of unspecified site")) %>%
  mutate(desc=replace(desc,
    (year >= 2010 & type == 'dx' & code == '04671'),  
    "Germstmann-Straussler-Scheinker syndrome")) %>%
  mutate(desc=replace(desc,
    (year >= 2010 & type == 'dx' & code == '38600'),  
    "Meniere's Disease, unspecified")) 


df$desc <- gsub("'", "", df$desc)

df <- df %>% 
  filter(year >= 2009) %>%
  arrange(year)


# Last left off at 140276 for full df
cursor.txt <- readLines('last-position.txt')
cursor <- as.numeric(gsub(".*= ", "", cursor.txt))
for (i in cursor:dim(df)[1]) {
  writeLines(paste0("Last left off at i = ", i), "last-position.txt") 
  print(paste0("processing ", i, " of ", dim(df)[1], " (", (i/dim(df)[1]), ")"))
  print(df[i,])

  code <- df[i,]$code
  descr <- df[i,]$desc
  nrd.year <- df[i,]$year
  dx.pr.type <- df[i,]$type
  
  if (dx.pr.type == 'dx') {
    for (j in 1:30) {
      
      print(paste0("dx", j))
      
      if (nrd.year == 2015) {
        sql <- paste0("UPDATE nrd SET dx", 
                      j, 
                      "_desc = '", 
                      trimws(descr), 
                      "' WHERE i10_dx", 
                      j, 
                      " = '",
                      code,
                      "' AND ((nrd_year = ", 
                      nrd.year, 
                      " AND dqtr = '4') OR (nrd_year = ",
                      (nrd.year + 1), 
                      " AND dqtr <> '4'))")
      } else {
        sql <- paste0("UPDATE nrd SET dx", 
                      j, 
                      "_desc = '", 
                      trimws(descr), 
                      "' WHERE dx", 
                      j, 
                      " = '",
                      code,
                      "' AND ((nrd_year = ", 
                      nrd.year, 
                      " AND dqtr = '4') OR (nrd_year = ",
                      (nrd.year + 1), 
                      " AND dqtr <> '4'))")
      }
      print(sql) 
      DBI::dbSendQuery(con, sql)
    }
  } else {
    for (j in 1:15) {
      
      print(paste0("pr", j))
      
      if (nrd.year == 2015) {
        sql <- paste0("UPDATE nrd SET pr", 
                      j, 
                      "_desc = '", 
                      trimws(descr), 
                      "' WHERE i10_pr", 
                      j, 
                      " = '",
                      code,
                      "' AND ((nrd_year = ", 
                      nrd.year, 
                      " AND dqtr = '4') OR (nrd_year = ",
                      (nrd.year + 1), 
                      " AND dqtr <> '4'))")
      } else {
        sql <- paste0("UPDATE nrd SET pr", 
                      j, 
                      "_desc = '", 
                      trimws(descr), 
                      "' WHERE pr", 
                      j, 
                      " = '",
                      code,
                      "' AND ((nrd_year = ", 
                      nrd.year, 
                      " AND dqtr = '4') OR (nrd_year = ",
                      (nrd.year + 1), 
                      " AND dqtr <> '4'))")
      }
      
      print(sql) 
      DBI::dbSendQuery(con, sql)
    }
  }
}
