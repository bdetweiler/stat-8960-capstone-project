#library('MonetDB.R')
#install.packages('MonetDBLite')
#library('MonetDBLite')
library('dplyr')
library('tidyverse')
library('DBI')
library('beepr')


setwd('/home/bdetweiler/src/Data_Science/stat-8960-capstone-project/')
# MonetDBLite::monetdblite_shutdown()
#con <- DBI::dbConnect(MonetDBLite::MonetDBLite(), "data/nrd_db")

df <- data.frame(code=c(), desc=c(), year=c(), type=c())

cdiff.nrd <- read_csv('data/cdiff-nrd.csv')
cdiff.nrd$key_nrd <- as.character(cdiff.nrd$key_nrd)


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

### XXX: We can't use V27LONG_SHORT_DX_110909u021012.csv because they converted the fucking code to a numeric, thereby losing
###      the left padding and hence duplicating codes and giving them differnet descriptions. This is the shit
###      that costs me hours of time figuring out and correcting. Thanks a ton, DHHS.
icd9.dx.2009 <- read_csv('data/ICD-9-CM-v27-Q4-2009/V27LONG_SHORT_DX_110909.csv', col_types = cols(.default = "c"))
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
    "Friedlanders bacillus infection in conditions classified elsewhere and of unspecified site")) %>% 
  mutate(desc=replace(desc,
    (year >= 2010 & type == 'dx' & code == '0413'),
    "Friedlanders bacillus infection in conditions classified elsewhere and of unspecified site")) %>% 
  mutate(desc=replace(desc,
    (year >= 2010 & type == 'dx' & code == '04671'),  
    "Germstmann-Straussler-Scheinker syndrome")) %>%
  mutate(desc=replace(desc,
    (year >= 2010 & type == 'dx' & code == '4671'),  
    "Germstmann-Straussler-Scheinker syndrome")) %>%
  mutate(desc=replace(desc,
    (year >= 2010 & type == 'dx' & code == '38600'),  
    "Menieres Disease, unspecified")) %>%
  mutate(desc=replace(desc,
    (year >= 2010 & type == 'dx' & code == '38601'),  
    "Active Menieres disease, cochleovestibular")) %>%
  mutate(desc=replace(desc,
    (year >= 2010 & type == 'dx' & code == '38602'),  
    "Active Menieres disease, cochlear")) %>%
  mutate(desc=replace(desc,
    (year >= 2010 & type == 'dx' & code == '38603'),  
    "Active Menieres disease, vestibular")) %>%
  mutate(desc=replace(desc,
    (year >= 2010 & type == 'dx' & code == '38604'),  
    "Inactive Menieres disease")) %>%
  mutate(desc=replace(desc,
    (year >= 2010 & type == 'dx' & code == '0413'),  
    "Friedlanders bacillus infection in conditions classified elsewhere and of unspecified site")) %>%
  mutate(desc=replace(desc,
    (year >= 2010 & type == 'dx' & code == '413'),  
    "Friedlanders bacillus infection in conditions classified elsewhere and of unspecified site")) %>%
  mutate(desc=replace(desc,
    (year >= 2010 & type == 'dx' & code == '04671'),  
    "Germstmann-Straussler-Scheinker syndrome")) %>%
  mutate(desc=replace(desc,
    (year >= 2010 & type == 'dx' & code == '38600'),  
    "Menieres Disease, unspecified"))

df$desc <- gsub("'", "", df$desc)

df <- df %>% 
  filter(year >= 2009) %>%
  arrange(year)


#############################################################################################################
###     2010 Q1 - Q3
#############################################################################################################

cdiff.2010.q1q3 <- cdiff.nrd %>% 
  filter(nrd_year == 2010, dqtr != '4')
dim(cdiff.2010.q1q3)
#############################################################################################################
###     2010 Q1 - Q3    DX
#############################################################################################################

cdiff.2010.q1q3 <- df %>% 
  filter(year == 2009, type == 'dx') %>%
  select(code, desc) %>%
  right_join(cdiff.2010.q1q3, by = c("code" = "dx1")) %>%
  rename("dx1" = "code", "dx1_desc" = "desc")

cdiff.2010.q1q3 <- df %>% 
  filter(year == 2009, type == 'dx') %>%
  select(code, desc) %>%
  right_join(cdiff.2010.q1q3, by = c("code" = "dx2")) %>%
  rename("dx2" = "code", "dx2_desc" = "desc")

cdiff.2010.q1q3 <- df %>% 
  filter(year == 2009, type == 'dx') %>%
  select(code, desc) %>%
  right_join(cdiff.2010.q1q3, by = c("code" = "dx3")) %>%
  rename("dx3" = "code", "dx3_desc" = "desc")

cdiff.2010.q1q3 <- df %>% 
  filter(year == 2009, type == 'dx') %>%
  select(code, desc) %>%
  right_join(cdiff.2010.q1q3, by = c("code" = "dx4")) %>%
  rename("dx4" = "code", "dx4_desc" = "desc")

cdiff.2010.q1q3 <- df %>% 
  filter(year == 2009, type == 'dx') %>%
  select(code, desc) %>%
  right_join(cdiff.2010.q1q3, by = c("code" = "dx5")) %>%
  rename("dx5" = "code", "dx5_desc" = "desc")

cdiff.2010.q1q3 <- df %>% 
  filter(year == 2009, type == 'dx') %>%
  select(code, desc) %>%
  right_join(cdiff.2010.q1q3, by = c("code" = "dx6")) %>%
  rename("dx6" = "code", "dx6_desc" = "desc")

cdiff.2010.q1q3 <- df %>% 
  filter(year == 2009, type == 'dx') %>%
  select(code, desc) %>%
  right_join(cdiff.2010.q1q3, by = c("code" = "dx7")) %>%
  rename("dx7" = "code", "dx7_desc" = "desc")

cdiff.2010.q1q3 <- df %>% 
  filter(year == 2009, type == 'dx') %>%
  select(code, desc) %>%
  right_join(cdiff.2010.q1q3, by = c("code" = "dx8")) %>%
  rename("dx8" = "code", "dx8_desc" = "desc")

cdiff.2010.q1q3 <- df %>% 
  filter(year == 2009, type == 'dx') %>%
  select(code, desc) %>%
  right_join(cdiff.2010.q1q3, by = c("code" = "dx9")) %>%
  rename("dx9" = "code", "dx9_desc" = "desc")

cdiff.2010.q1q3 <- df %>% 
  filter(year == 2009, type == 'dx') %>%
  select(code, desc) %>%
  right_join(cdiff.2010.q1q3, by = c("code" = "dx10")) %>%
  rename("dx10" = "code", "dx10_desc" = "desc")

cdiff.2010.q1q3 <- df %>% 
  filter(year == 2009, type == 'dx') %>%
  select(code, desc) %>%
  right_join(cdiff.2010.q1q3, by = c("code" = "dx11")) %>%
  rename("dx11" = "code", "dx11_desc" = "desc")

cdiff.2010.q1q3 <- df %>% 
  filter(year == 2009, type == 'dx') %>%
  select(code, desc) %>%
  right_join(cdiff.2010.q1q3, by = c("code" = "dx12")) %>%
  rename("dx12" = "code", "dx12_desc" = "desc")

cdiff.2010.q1q3 <- df %>% 
  filter(year == 2009, type == 'dx') %>%
  select(code, desc) %>%
  right_join(cdiff.2010.q1q3, by = c("code" = "dx13")) %>%
  rename("dx13" = "code", "dx13_desc" = "desc")

cdiff.2010.q1q3 <- df %>% 
  filter(year == 2009, type == 'dx') %>%
  select(code, desc) %>%
  right_join(cdiff.2010.q1q3, by = c("code" = "dx14")) %>%
  rename("dx14" = "code", "dx14_desc" = "desc")

cdiff.2010.q1q3 <- df %>% 
  filter(year == 2009, type == 'dx') %>%
  select(code, desc) %>%
  right_join(cdiff.2010.q1q3, by = c("code" = "dx15")) %>%
  rename("dx15" = "code", "dx15_desc" = "desc")

cdiff.2010.q1q3 <- df %>% 
  filter(year == 2009, type == 'dx') %>%
  select(code, desc) %>%
  right_join(cdiff.2010.q1q3, by = c("code" = "dx16")) %>%
  rename("dx16" = "code", "dx16_desc" = "desc")

cdiff.2010.q1q3 <- df %>% 
  filter(year == 2009, type == 'dx') %>%
  select(code, desc) %>%
  right_join(cdiff.2010.q1q3, by = c("code" = "dx17")) %>%
  rename("dx17" = "code", "dx17_desc" = "desc")

cdiff.2010.q1q3 <- df %>% 
  filter(year == 2009, type == 'dx') %>%
  select(code, desc) %>%
  right_join(cdiff.2010.q1q3, by = c("code" = "dx18")) %>%
  rename("dx18" = "code", "dx18_desc" = "desc")

cdiff.2010.q1q3 <- df %>% 
  filter(year == 2009, type == 'dx') %>%
  select(code, desc) %>%
  right_join(cdiff.2010.q1q3, by = c("code" = "dx19")) %>%
  rename("dx19" = "code", "dx19_desc" = "desc")

cdiff.2010.q1q3 <- df %>% 
  filter(year == 2009, type == 'dx') %>%
  select(code, desc) %>%
  right_join(cdiff.2010.q1q3, by = c("code" = "dx20")) %>%
  rename("dx20" = "code", "dx20_desc" = "desc")

cdiff.2010.q1q3 <- df %>% 
  filter(year == 2009, type == 'dx') %>%
  select(code, desc) %>%
  right_join(cdiff.2010.q1q3, by = c("code" = "dx21")) %>%
  rename("dx21" = "code", "dx21_desc" = "desc")

cdiff.2010.q1q3 <- df %>% 
  filter(year == 2009, type == 'dx') %>%
  select(code, desc) %>%
  right_join(cdiff.2010.q1q3, by = c("code" = "dx22")) %>%
  rename("dx22" = "code", "dx22_desc" = "desc")

cdiff.2010.q1q3 <- df %>% 
  filter(year == 2009, type == 'dx') %>%
  select(code, desc) %>%
  right_join(cdiff.2010.q1q3, by = c("code" = "dx23")) %>%
  rename("dx23" = "code", "dx23_desc" = "desc")

cdiff.2010.q1q3 <- df %>% 
  filter(year == 2009, type == 'dx') %>%
  select(code, desc) %>%
  right_join(cdiff.2010.q1q3, by = c("code" = "dx24")) %>%
  rename("dx24" = "code", "dx24_desc" = "desc")

cdiff.2010.q1q3 <- df %>% 
  filter(year == 2009, type == 'dx') %>%
  select(code, desc) %>%
  right_join(cdiff.2010.q1q3, by = c("code" = "dx25")) %>%
  rename("dx25" = "code", "dx25_desc" = "desc")

cdiff.2010.q1q3 <- df %>% 
  filter(year == 2009, type == 'dx') %>%
  select(code, desc) %>%
  right_join(cdiff.2010.q1q3, by = c("code" = "dx26")) %>%
  rename("dx26" = "code", "dx26_desc" = "desc")

cdiff.2010.q1q3 <- df %>% 
  filter(year == 2009, type == 'dx') %>%
  select(code, desc) %>%
  right_join(cdiff.2010.q1q3, by = c("code" = "dx27")) %>%
  rename("dx27" = "code", "dx27_desc" = "desc")

cdiff.2010.q1q3 <- df %>% 
  filter(year == 2009, type == 'dx') %>%
  select(code, desc) %>%
  right_join(cdiff.2010.q1q3, by = c("code" = "dx28")) %>%
  rename("dx28" = "code", "dx28_desc" = "desc")

cdiff.2010.q1q3 <- df %>% 
  filter(year == 2009, type == 'dx') %>%
  select(code, desc) %>%
  right_join(cdiff.2010.q1q3, by = c("code" = "dx29")) %>%
  rename("dx29" = "code", "dx29_desc" = "desc")

cdiff.2010.q1q3 <- df %>% 
  filter(year == 2009, type == 'dx') %>%
  select(code, desc) %>%
  right_join(cdiff.2010.q1q3, by = c("code" = "dx30")) %>%
  rename("dx30" = "code", "dx30_desc" = "desc")

#############################################################################################################
###     2010 Q1 - Q3    PR
#############################################################################################################

cdiff.2010.q1q3 <- df %>% 
  filter(year == 2009, type == 'pr') %>%
  select(code, desc) %>%
  right_join(cdiff.2010.q1q3, by = c("code" = "pr1")) %>%
  rename("pr1" = "code", "pr1_desc" = "desc")

cdiff.2010.q1q3 <- df %>% 
  filter(year == 2009, type == 'pr') %>%
  select(code, desc) %>%
  right_join(cdiff.2010.q1q3, by = c("code" = "pr2")) %>%
  rename("pr2" = "code", "pr2_desc" = "desc")

cdiff.2010.q1q3 <- df %>% 
  filter(year == 2009, type == 'pr') %>%
  select(code, desc) %>%
  right_join(cdiff.2010.q1q3, by = c("code" = "pr3")) %>%
  rename("pr3" = "code", "pr3_desc" = "desc")

cdiff.2010.q1q3 <- df %>% 
  filter(year == 2009, type == 'pr') %>%
  select(code, desc) %>%
  right_join(cdiff.2010.q1q3, by = c("code" = "pr4")) %>%
  rename("pr4" = "code", "pr4_desc" = "desc")

cdiff.2010.q1q3 <- df %>% 
  filter(year == 2009, type == 'pr') %>%
  select(code, desc) %>%
  right_join(cdiff.2010.q1q3, by = c("code" = "pr5")) %>%
  rename("pr5" = "code", "pr5_desc" = "desc")

cdiff.2010.q1q3 <- df %>% 
  filter(year == 2009, type == 'pr') %>%
  select(code, desc) %>%
  right_join(cdiff.2010.q1q3, by = c("code" = "pr6")) %>%
  rename("pr6" = "code", "pr6_desc" = "desc")

cdiff.2010.q1q3 <- df %>% 
  filter(year == 2009, type == 'pr') %>%
  select(code, desc) %>%
  right_join(cdiff.2010.q1q3, by = c("code" = "pr7")) %>%
  rename("pr7" = "code", "pr7_desc" = "desc")

cdiff.2010.q1q3 <- df %>% 
  filter(year == 2009, type == 'pr') %>%
  select(code, desc) %>%
  right_join(cdiff.2010.q1q3, by = c("code" = "pr8")) %>%
  rename("pr8" = "code", "pr8_desc" = "desc")

cdiff.2010.q1q3 <- df %>% 
  filter(year == 2009, type == 'pr') %>%
  select(code, desc) %>%
  right_join(cdiff.2010.q1q3, by = c("code" = "pr9")) %>%
  rename("pr9" = "code", "pr9_desc" = "desc")

cdiff.2010.q1q3 <- df %>% 
  filter(year == 2009, type == 'pr') %>%
  select(code, desc) %>%
  right_join(cdiff.2010.q1q3, by = c("code" = "pr10")) %>%
  rename("pr10" = "code", "pr10_desc" = "desc")

cdiff.2010.q1q3 <- df %>% 
  filter(year == 2009, type == 'pr') %>%
  select(code, desc) %>%
  right_join(cdiff.2010.q1q3, by = c("code" = "pr11")) %>%
  rename("pr11" = "code", "pr11_desc" = "desc")

cdiff.2010.q1q3 <- df %>% 
  filter(year == 2009, type == 'pr') %>%
  select(code, desc) %>%
  right_join(cdiff.2010.q1q3, by = c("code" = "pr12")) %>%
  rename("pr12" = "code", "pr12_desc" = "desc")

cdiff.2010.q1q3 <- df %>% 
  filter(year == 2009, type == 'pr') %>%
  select(code, desc) %>%
  right_join(cdiff.2010.q1q3, by = c("code" = "pr13")) %>%
  rename("pr13" = "code", "pr13_desc" = "desc")

cdiff.2010.q1q3 <- df %>% 
  filter(year == 2009, type == 'pr') %>%
  select(code, desc) %>%
  right_join(cdiff.2010.q1q3, by = c("code" = "pr14")) %>%
  rename("pr14" = "code", "pr14_desc" = "desc")

cdiff.2010.q1q3 <- df %>% 
  filter(year == 2009, type == 'pr') %>%
  select(code, desc) %>%
  right_join(cdiff.2010.q1q3, by = c("code" = "pr15")) %>%
  rename("pr15" = "code", "pr15_desc" = "desc")


#############################################################################################################
###     2010 Q4, 2011 Q1 - Q3
#############################################################################################################

cdiff.2010.q4.2011.q1q3 <- cdiff.nrd %>% 
  filter(nrd_year == 2010, dqtr == '4')

cdiff.2010.q4.2011.q1q3 <- cdiff.nrd %>% 
  filter(nrd_year == 2011, dqtr != '4') %>%
  bind_rows(cdiff.2010.q4.2011.q1q3)


#############################################################################################################
###     2010 Q4, 2011 Q1 - Q3    DX
#############################################################################################################


cdiff.2010.q4.2011.q1q3 <- df %>% 
  filter(year == 2010, type == 'dx') %>%
  select(code, desc) %>%
  right_join(cdiff.2010.q4.2011.q1q3, by = c("code" = "dx1")) %>%
  rename("dx1" = "code", "dx1_desc" = "desc")

cdiff.2010.q4.2011.q1q3 <- df %>% 
  filter(year == 2010, type == 'dx') %>%
  select(code, desc) %>%
  right_join(cdiff.2010.q4.2011.q1q3, by = c("code" = "dx2")) %>%
  rename("dx2" = "code", "dx2_desc" = "desc")

cdiff.2010.q4.2011.q1q3 <- df %>% 
  filter(year == 2010, type == 'dx') %>%
  select(code, desc) %>%
  right_join(cdiff.2010.q4.2011.q1q3, by = c("code" = "dx3")) %>%
  rename("dx3" = "code", "dx3_desc" = "desc")

cdiff.2010.q4.2011.q1q3 <- df %>% 
  filter(year == 2010, type == 'dx') %>%
  select(code, desc) %>%
  right_join(cdiff.2010.q4.2011.q1q3, by = c("code" = "dx4")) %>%
  rename("dx4" = "code", "dx4_desc" = "desc")

cdiff.2010.q4.2011.q1q3 <- df %>% 
  filter(year == 2010, type == 'dx') %>%
  select(code, desc) %>%
  right_join(cdiff.2010.q4.2011.q1q3, by = c("code" = "dx5")) %>%
  rename("dx5" = "code", "dx5_desc" = "desc")

cdiff.2010.q4.2011.q1q3 <- df %>% 
  filter(year == 2010, type == 'dx') %>%
  select(code, desc) %>%
  right_join(cdiff.2010.q4.2011.q1q3, by = c("code" = "dx6")) %>%
  rename("dx6" = "code", "dx6_desc" = "desc")

cdiff.2010.q4.2011.q1q3 <- df %>% 
  filter(year == 2010, type == 'dx') %>%
  select(code, desc) %>%
  right_join(cdiff.2010.q4.2011.q1q3, by = c("code" = "dx7")) %>%
  rename("dx7" = "code", "dx7_desc" = "desc")

cdiff.2010.q4.2011.q1q3 <- df %>% 
  filter(year == 2010, type == 'dx') %>%
  select(code, desc) %>%
  right_join(cdiff.2010.q4.2011.q1q3, by = c("code" = "dx8")) %>%
  rename("dx8" = "code", "dx8_desc" = "desc")

cdiff.2010.q4.2011.q1q3 <- df %>% 
  filter(year == 2010, type == 'dx') %>%
  select(code, desc) %>%
  right_join(cdiff.2010.q4.2011.q1q3, by = c("code" = "dx9")) %>%
  rename("dx9" = "code", "dx9_desc" = "desc")

cdiff.2010.q4.2011.q1q3 <- df %>% 
  filter(year == 2010, type == 'dx') %>%
  select(code, desc) %>%
  right_join(cdiff.2010.q4.2011.q1q3, by = c("code" = "dx10")) %>%
  rename("dx10" = "code", "dx10_desc" = "desc")

cdiff.2010.q4.2011.q1q3 <- df %>% 
  filter(year == 2010, type == 'dx') %>%
  select(code, desc) %>%
  right_join(cdiff.2010.q4.2011.q1q3, by = c("code" = "dx11")) %>%
  rename("dx11" = "code", "dx11_desc" = "desc")

cdiff.2010.q4.2011.q1q3 <- df %>% 
  filter(year == 2010, type == 'dx') %>%
  select(code, desc) %>%
  right_join(cdiff.2010.q4.2011.q1q3, by = c("code" = "dx12")) %>%
  rename("dx12" = "code", "dx12_desc" = "desc")

cdiff.2010.q4.2011.q1q3 <- df %>% 
  filter(year == 2010, type == 'dx') %>%
  select(code, desc) %>%
  right_join(cdiff.2010.q4.2011.q1q3, by = c("code" = "dx13")) %>%
  rename("dx13" = "code", "dx13_desc" = "desc")

cdiff.2010.q4.2011.q1q3 <- df %>% 
  filter(year == 2010, type == 'dx') %>%
  select(code, desc) %>%
  right_join(cdiff.2010.q4.2011.q1q3, by = c("code" = "dx14")) %>%
  rename("dx14" = "code", "dx14_desc" = "desc")

cdiff.2010.q4.2011.q1q3 <- df %>% 
  filter(year == 2010, type == 'dx') %>%
  select(code, desc) %>%
  right_join(cdiff.2010.q4.2011.q1q3, by = c("code" = "dx15")) %>%
  rename("dx15" = "code", "dx15_desc" = "desc")

cdiff.2010.q4.2011.q1q3 <- df %>% 
  filter(year == 2010, type == 'dx') %>%
  select(code, desc) %>%
  right_join(cdiff.2010.q4.2011.q1q3, by = c("code" = "dx16")) %>%
  rename("dx16" = "code", "dx16_desc" = "desc")

cdiff.2010.q4.2011.q1q3 <- df %>% 
  filter(year == 2010, type == 'dx') %>%
  select(code, desc) %>%
  right_join(cdiff.2010.q4.2011.q1q3, by = c("code" = "dx17")) %>%
  rename("dx17" = "code", "dx17_desc" = "desc")

cdiff.2010.q4.2011.q1q3 <- df %>% 
  filter(year == 2010, type == 'dx') %>%
  select(code, desc) %>%
  right_join(cdiff.2010.q4.2011.q1q3, by = c("code" = "dx18")) %>%
  rename("dx18" = "code", "dx18_desc" = "desc")

cdiff.2010.q4.2011.q1q3 <- df %>% 
  filter(year == 2010, type == 'dx') %>%
  select(code, desc) %>%
  right_join(cdiff.2010.q4.2011.q1q3, by = c("code" = "dx19")) %>%
  rename("dx19" = "code", "dx19_desc" = "desc")

cdiff.2010.q4.2011.q1q3 <- df %>% 
  filter(year == 2010, type == 'dx') %>%
  select(code, desc) %>%
  right_join(cdiff.2010.q4.2011.q1q3, by = c("code" = "dx20")) %>%
  rename("dx20" = "code", "dx20_desc" = "desc")

cdiff.2010.q4.2011.q1q3 <- df %>% 
  filter(year == 2010, type == 'dx') %>%
  select(code, desc) %>%
  right_join(cdiff.2010.q4.2011.q1q3, by = c("code" = "dx21")) %>%
  rename("dx21" = "code", "dx21_desc" = "desc")

cdiff.2010.q4.2011.q1q3 <- df %>% 
  filter(year == 2010, type == 'dx') %>%
  select(code, desc) %>%
  right_join(cdiff.2010.q4.2011.q1q3, by = c("code" = "dx22")) %>%
  rename("dx22" = "code", "dx22_desc" = "desc")

cdiff.2010.q4.2011.q1q3 <- df %>% 
  filter(year == 2010, type == 'dx') %>%
  select(code, desc) %>%
  right_join(cdiff.2010.q4.2011.q1q3, by = c("code" = "dx23")) %>%
  rename("dx23" = "code", "dx23_desc" = "desc")

cdiff.2010.q4.2011.q1q3 <- df %>% 
  filter(year == 2010, type == 'dx') %>%
  select(code, desc) %>%
  right_join(cdiff.2010.q4.2011.q1q3, by = c("code" = "dx24")) %>%
  rename("dx24" = "code", "dx24_desc" = "desc")

cdiff.2010.q4.2011.q1q3 <- df %>% 
  filter(year == 2010, type == 'dx') %>%
  select(code, desc) %>%
  right_join(cdiff.2010.q4.2011.q1q3, by = c("code" = "dx25")) %>%
  rename("dx25" = "code", "dx25_desc" = "desc")

cdiff.2010.q4.2011.q1q3 <- df %>% 
  filter(year == 2010, type == 'dx') %>%
  select(code, desc) %>%
  right_join(cdiff.2010.q4.2011.q1q3, by = c("code" = "dx26")) %>%
  rename("dx26" = "code", "dx26_desc" = "desc")

cdiff.2010.q4.2011.q1q3 <- df %>% 
  filter(year == 2010, type == 'dx') %>%
  select(code, desc) %>%
  right_join(cdiff.2010.q4.2011.q1q3, by = c("code" = "dx27")) %>%
  rename("dx27" = "code", "dx27_desc" = "desc")

cdiff.2010.q4.2011.q1q3 <- df %>% 
  filter(year == 2010, type == 'dx') %>%
  select(code, desc) %>%
  right_join(cdiff.2010.q4.2011.q1q3, by = c("code" = "dx28")) %>%
  rename("dx28" = "code", "dx28_desc" = "desc")

cdiff.2010.q4.2011.q1q3 <- df %>% 
  filter(year == 2010, type == 'dx') %>%
  select(code, desc) %>%
  right_join(cdiff.2010.q4.2011.q1q3, by = c("code" = "dx29")) %>%
  rename("dx29" = "code", "dx29_desc" = "desc")

cdiff.2010.q4.2011.q1q3 <- df %>% 
  filter(year == 2010, type == 'dx') %>%
  select(code, desc) %>%
  right_join(cdiff.2010.q4.2011.q1q3, by = c("code" = "dx30")) %>%
  rename("dx30" = "code", "dx30_desc" = "desc")

#############################################################################################################
###     2010 Q4, 2011 Q1 - Q3    PR
#############################################################################################################


cdiff.2010.q4.2011.q1q3 <- df %>% 
  filter(year == 2010, type == 'pr') %>%
  select(code, desc) %>%
  right_join(cdiff.2010.q4.2011.q1q3, by = c("code" = "pr1")) %>%
  rename("pr1" = "code", "pr1_desc" = "desc")

cdiff.2010.q4.2011.q1q3 <- df %>% 
  filter(year == 2010, type == 'pr') %>%
  select(code, desc) %>%
  right_join(cdiff.2010.q4.2011.q1q3, by = c("code" = "pr2")) %>%
  rename("pr2" = "code", "pr2_desc" = "desc")

cdiff.2010.q4.2011.q1q3 <- df %>% 
  filter(year == 2010, type == 'pr') %>%
  select(code, desc) %>%
  right_join(cdiff.2010.q4.2011.q1q3, by = c("code" = "pr3")) %>%
  rename("pr3" = "code", "pr3_desc" = "desc")

cdiff.2010.q4.2011.q1q3 <- df %>% 
  filter(year == 2010, type == 'pr') %>%
  select(code, desc) %>%
  right_join(cdiff.2010.q4.2011.q1q3, by = c("code" = "pr4")) %>%
  rename("pr4" = "code", "pr4_desc" = "desc")

cdiff.2010.q4.2011.q1q3 <- df %>% 
  filter(year == 2010, type == 'pr') %>%
  select(code, desc) %>%
  right_join(cdiff.2010.q4.2011.q1q3, by = c("code" = "pr5")) %>%
  rename("pr5" = "code", "pr5_desc" = "desc")

cdiff.2010.q4.2011.q1q3 <- df %>% 
  filter(year == 2010, type == 'pr') %>%
  select(code, desc) %>%
  right_join(cdiff.2010.q4.2011.q1q3, by = c("code" = "pr6")) %>%
  rename("pr6" = "code", "pr6_desc" = "desc")

cdiff.2010.q4.2011.q1q3 <- df %>% 
  filter(year == 2010, type == 'pr') %>%
  select(code, desc) %>%
  right_join(cdiff.2010.q4.2011.q1q3, by = c("code" = "pr7")) %>%
  rename("pr7" = "code", "pr7_desc" = "desc")

cdiff.2010.q4.2011.q1q3 <- df %>% 
  filter(year == 2010, type == 'pr') %>%
  select(code, desc) %>%
  right_join(cdiff.2010.q4.2011.q1q3, by = c("code" = "pr8")) %>%
  rename("pr8" = "code", "pr8_desc" = "desc")

cdiff.2010.q4.2011.q1q3 <- df %>% 
  filter(year == 2010, type == 'pr') %>%
  select(code, desc) %>%
  right_join(cdiff.2010.q4.2011.q1q3, by = c("code" = "pr9")) %>%
  rename("pr9" = "code", "pr9_desc" = "desc")

cdiff.2010.q4.2011.q1q3 <- df %>% 
  filter(year == 2010, type == 'pr') %>%
  select(code, desc) %>%
  right_join(cdiff.2010.q4.2011.q1q3, by = c("code" = "pr10")) %>%
  rename("pr10" = "code", "pr10_desc" = "desc")

cdiff.2010.q4.2011.q1q3 <- df %>% 
  filter(year == 2010, type == 'pr') %>%
  select(code, desc) %>%
  right_join(cdiff.2010.q4.2011.q1q3, by = c("code" = "pr11")) %>%
  rename("pr11" = "code", "pr11_desc" = "desc")

cdiff.2010.q4.2011.q1q3 <- df %>% 
  filter(year == 2010, type == 'pr') %>%
  select(code, desc) %>%
  right_join(cdiff.2010.q4.2011.q1q3, by = c("code" = "pr12")) %>%
  rename("pr12" = "code", "pr12_desc" = "desc")

cdiff.2010.q4.2011.q1q3 <- df %>% 
  filter(year == 2010, type == 'pr') %>%
  select(code, desc) %>%
  right_join(cdiff.2010.q4.2011.q1q3, by = c("code" = "pr13")) %>%
  rename("pr13" = "code", "pr13_desc" = "desc")

cdiff.2010.q4.2011.q1q3 <- df %>% 
  filter(year == 2010, type == 'pr') %>%
  select(code, desc) %>%
  right_join(cdiff.2010.q4.2011.q1q3, by = c("code" = "pr14")) %>%
  rename("pr14" = "code", "pr14_desc" = "desc")

cdiff.2010.q4.2011.q1q3 <- df %>% 
  filter(year == 2010, type == 'pr') %>%
  select(code, desc) %>%
  right_join(cdiff.2010.q4.2011.q1q3, by = c("code" = "pr15")) %>%
  rename("pr15" = "code", "pr15_desc" = "desc")

#############################################################################################################
###     2011 Q4, 2012 Q1 - Q3    
#############################################################################################################

cdiff.2011.q4.2012.q1q3 <- cdiff.nrd %>% 
  filter(nrd_year == 2011, dqtr == '4')
cdiff.2011.q4.2012.q1q3 <- cdiff.nrd %>% 
  filter(nrd_year == 2012, dqtr != '4') %>%
  bind_rows(cdiff.2011.q4.2012.q1q3)

#############################################################################################################
###     2011 Q4, 2012 Q1 - Q3    DX
#############################################################################################################

cdiff.2011.q4.2012.q1q3 <- df %>% 
  filter(year == 2011, type == 'dx') %>%
  select(code, desc) %>%
  right_join(cdiff.2011.q4.2012.q1q3, by = c("code" = "dx1")) %>%
  rename("dx1" = "code", "dx1_desc" = "desc")

cdiff.2011.q4.2012.q1q3 <- df %>% 
  filter(year == 2011, type == 'dx') %>%
  select(code, desc) %>%
  right_join(cdiff.2011.q4.2012.q1q3, by = c("code" = "dx2")) %>%
  rename("dx2" = "code", "dx2_desc" = "desc")

cdiff.2011.q4.2012.q1q3 <- df %>% 
  filter(year == 2011, type == 'dx') %>%
  select(code, desc) %>%
  right_join(cdiff.2011.q4.2012.q1q3, by = c("code" = "dx3")) %>%
  rename("dx3" = "code", "dx3_desc" = "desc")

cdiff.2011.q4.2012.q1q3 <- df %>% 
  filter(year == 2011, type == 'dx') %>%
  select(code, desc) %>%
  right_join(cdiff.2011.q4.2012.q1q3, by = c("code" = "dx4")) %>%
  rename("dx4" = "code", "dx4_desc" = "desc")

cdiff.2011.q4.2012.q1q3 <- df %>% 
  filter(year == 2011, type == 'dx') %>%
  select(code, desc) %>%
  right_join(cdiff.2011.q4.2012.q1q3, by = c("code" = "dx5")) %>%
  rename("dx5" = "code", "dx5_desc" = "desc")

cdiff.2011.q4.2012.q1q3 <- df %>% 
  filter(year == 2011, type == 'dx') %>%
  select(code, desc) %>%
  right_join(cdiff.2011.q4.2012.q1q3, by = c("code" = "dx6")) %>%
  rename("dx6" = "code", "dx6_desc" = "desc")

cdiff.2011.q4.2012.q1q3 <- df %>% 
  filter(year == 2011, type == 'dx') %>%
  select(code, desc) %>%
  right_join(cdiff.2011.q4.2012.q1q3, by = c("code" = "dx7")) %>%
  rename("dx7" = "code", "dx7_desc" = "desc")

cdiff.2011.q4.2012.q1q3 <- df %>% 
  filter(year == 2011, type == 'dx') %>%
  select(code, desc) %>%
  right_join(cdiff.2011.q4.2012.q1q3, by = c("code" = "dx8")) %>%
  rename("dx8" = "code", "dx8_desc" = "desc")

cdiff.2011.q4.2012.q1q3 <- df %>% 
  filter(year == 2011, type == 'dx') %>%
  select(code, desc) %>%
  right_join(cdiff.2011.q4.2012.q1q3, by = c("code" = "dx9")) %>%
  rename("dx9" = "code", "dx9_desc" = "desc")

cdiff.2011.q4.2012.q1q3 <- df %>% 
  filter(year == 2011, type == 'dx') %>%
  select(code, desc) %>%
  right_join(cdiff.2011.q4.2012.q1q3, by = c("code" = "dx10")) %>%
  rename("dx10" = "code", "dx10_desc" = "desc")

cdiff.2011.q4.2012.q1q3 <- df %>% 
  filter(year == 2011, type == 'dx') %>%
  select(code, desc) %>%
  right_join(cdiff.2011.q4.2012.q1q3, by = c("code" = "dx11")) %>%
  rename("dx11" = "code", "dx11_desc" = "desc")

cdiff.2011.q4.2012.q1q3 <- df %>% 
  filter(year == 2011, type == 'dx') %>%
  select(code, desc) %>%
  right_join(cdiff.2011.q4.2012.q1q3, by = c("code" = "dx12")) %>%
  rename("dx12" = "code", "dx12_desc" = "desc")

cdiff.2011.q4.2012.q1q3 <- df %>% 
  filter(year == 2011, type == 'dx') %>%
  select(code, desc) %>%
  right_join(cdiff.2011.q4.2012.q1q3, by = c("code" = "dx13")) %>%
  rename("dx13" = "code", "dx13_desc" = "desc")

cdiff.2011.q4.2012.q1q3 <- df %>% 
  filter(year == 2011, type == 'dx') %>%
  select(code, desc) %>%
  right_join(cdiff.2011.q4.2012.q1q3, by = c("code" = "dx14")) %>%
  rename("dx14" = "code", "dx14_desc" = "desc")

cdiff.2011.q4.2012.q1q3 <- df %>% 
  filter(year == 2011, type == 'dx') %>%
  select(code, desc) %>%
  right_join(cdiff.2011.q4.2012.q1q3, by = c("code" = "dx15")) %>%
  rename("dx15" = "code", "dx15_desc" = "desc")

cdiff.2011.q4.2012.q1q3 <- df %>% 
  filter(year == 2011, type == 'dx') %>%
  select(code, desc) %>%
  right_join(cdiff.2011.q4.2012.q1q3, by = c("code" = "dx16")) %>%
  rename("dx16" = "code", "dx16_desc" = "desc")

cdiff.2011.q4.2012.q1q3 <- df %>% 
  filter(year == 2011, type == 'dx') %>%
  select(code, desc) %>%
  right_join(cdiff.2011.q4.2012.q1q3, by = c("code" = "dx17")) %>%
  rename("dx17" = "code", "dx17_desc" = "desc")

cdiff.2011.q4.2012.q1q3 <- df %>% 
  filter(year == 2011, type == 'dx') %>%
  select(code, desc) %>%
  right_join(cdiff.2011.q4.2012.q1q3, by = c("code" = "dx18")) %>%
  rename("dx18" = "code", "dx18_desc" = "desc")

cdiff.2011.q4.2012.q1q3 <- df %>% 
  filter(year == 2011, type == 'dx') %>%
  select(code, desc) %>%
  right_join(cdiff.2011.q4.2012.q1q3, by = c("code" = "dx19")) %>%
  rename("dx19" = "code", "dx19_desc" = "desc")

cdiff.2011.q4.2012.q1q3 <- df %>% 
  filter(year == 2011, type == 'dx') %>%
  select(code, desc) %>%
  right_join(cdiff.2011.q4.2012.q1q3, by = c("code" = "dx20")) %>%
  rename("dx20" = "code", "dx20_desc" = "desc")

cdiff.2011.q4.2012.q1q3 <- df %>% 
  filter(year == 2011, type == 'dx') %>%
  select(code, desc) %>%
  right_join(cdiff.2011.q4.2012.q1q3, by = c("code" = "dx21")) %>%
  rename("dx21" = "code", "dx21_desc" = "desc")

cdiff.2011.q4.2012.q1q3 <- df %>% 
  filter(year == 2011, type == 'dx') %>%
  select(code, desc) %>%
  right_join(cdiff.2011.q4.2012.q1q3, by = c("code" = "dx22")) %>%
  rename("dx22" = "code", "dx22_desc" = "desc")

cdiff.2011.q4.2012.q1q3 <- df %>% 
  filter(year == 2011, type == 'dx') %>%
  select(code, desc) %>%
  right_join(cdiff.2011.q4.2012.q1q3, by = c("code" = "dx23")) %>%
  rename("dx23" = "code", "dx23_desc" = "desc")

cdiff.2011.q4.2012.q1q3 <- df %>% 
  filter(year == 2011, type == 'dx') %>%
  select(code, desc) %>%
  right_join(cdiff.2011.q4.2012.q1q3, by = c("code" = "dx24")) %>%
  rename("dx24" = "code", "dx24_desc" = "desc")

cdiff.2011.q4.2012.q1q3 <- df %>% 
  filter(year == 2011, type == 'dx') %>%
  select(code, desc) %>%
  right_join(cdiff.2011.q4.2012.q1q3, by = c("code" = "dx25")) %>%
  rename("dx25" = "code", "dx25_desc" = "desc")

cdiff.2011.q4.2012.q1q3 <- df %>% 
  filter(year == 2011, type == 'dx') %>%
  select(code, desc) %>%
  right_join(cdiff.2011.q4.2012.q1q3, by = c("code" = "dx26")) %>%
  rename("dx26" = "code", "dx26_desc" = "desc")

cdiff.2011.q4.2012.q1q3 <- df %>% 
  filter(year == 2011, type == 'dx') %>%
  select(code, desc) %>%
  right_join(cdiff.2011.q4.2012.q1q3, by = c("code" = "dx27")) %>%
  rename("dx27" = "code", "dx27_desc" = "desc")

cdiff.2011.q4.2012.q1q3 <- df %>% 
  filter(year == 2011, type == 'dx') %>%
  select(code, desc) %>%
  right_join(cdiff.2011.q4.2012.q1q3, by = c("code" = "dx28")) %>%
  rename("dx28" = "code", "dx28_desc" = "desc")

cdiff.2011.q4.2012.q1q3 <- df %>% 
  filter(year == 2011, type == 'dx') %>%
  select(code, desc) %>%
  right_join(cdiff.2011.q4.2012.q1q3, by = c("code" = "dx29")) %>%
  rename("dx29" = "code", "dx29_desc" = "desc")

cdiff.2011.q4.2012.q1q3 <- df %>% 
  filter(year == 2011, type == 'dx') %>%
  select(code, desc) %>%
  right_join(cdiff.2011.q4.2012.q1q3, by = c("code" = "dx30")) %>%
  rename("dx30" = "code", "dx30_desc" = "desc")

#############################################################################################################
###     2011 Q4, 2012 Q1 - Q3    PR
#############################################################################################################

cdiff.2011.q4.2012.q1q3 <- df %>% 
  filter(year == 2011, type == 'pr') %>%
  select(code, desc) %>%
  right_join(cdiff.2011.q4.2012.q1q3, by = c("code" = "pr1")) %>%
  rename("pr1" = "code", "pr1_desc" = "desc")

cdiff.2011.q4.2012.q1q3 <- df %>% 
  filter(year == 2011, type == 'pr') %>%
  select(code, desc) %>%
  right_join(cdiff.2011.q4.2012.q1q3, by = c("code" = "pr2")) %>%
  rename("pr2" = "code", "pr2_desc" = "desc")

cdiff.2011.q4.2012.q1q3 <- df %>% 
  filter(year == 2011, type == 'pr') %>%
  select(code, desc) %>%
  right_join(cdiff.2011.q4.2012.q1q3, by = c("code" = "pr3")) %>%
  rename("pr3" = "code", "pr3_desc" = "desc")

cdiff.2011.q4.2012.q1q3 <- df %>% 
  filter(year == 2011, type == 'pr') %>%
  select(code, desc) %>%
  right_join(cdiff.2011.q4.2012.q1q3, by = c("code" = "pr4")) %>%
  rename("pr4" = "code", "pr4_desc" = "desc")

cdiff.2011.q4.2012.q1q3 <- df %>% 
  filter(year == 2011, type == 'pr') %>%
  select(code, desc) %>%
  right_join(cdiff.2011.q4.2012.q1q3, by = c("code" = "pr5")) %>%
  rename("pr5" = "code", "pr5_desc" = "desc")

cdiff.2011.q4.2012.q1q3 <- df %>% 
  filter(year == 2011, type == 'pr') %>%
  select(code, desc) %>%
  right_join(cdiff.2011.q4.2012.q1q3, by = c("code" = "pr6")) %>%
  rename("pr6" = "code", "pr6_desc" = "desc")

cdiff.2011.q4.2012.q1q3 <- df %>% 
  filter(year == 2011, type == 'pr') %>%
  select(code, desc) %>%
  right_join(cdiff.2011.q4.2012.q1q3, by = c("code" = "pr7")) %>%
  rename("pr7" = "code", "pr7_desc" = "desc")

cdiff.2011.q4.2012.q1q3 <- df %>% 
  filter(year == 2011, type == 'pr') %>%
  select(code, desc) %>%
  right_join(cdiff.2011.q4.2012.q1q3, by = c("code" = "pr8")) %>%
  rename("pr8" = "code", "pr8_desc" = "desc")

cdiff.2011.q4.2012.q1q3 <- df %>% 
  filter(year == 2011, type == 'pr') %>%
  select(code, desc) %>%
  right_join(cdiff.2011.q4.2012.q1q3, by = c("code" = "pr9")) %>%
  rename("pr9" = "code", "pr9_desc" = "desc")

cdiff.2011.q4.2012.q1q3 <- df %>% 
  filter(year == 2011, type == 'pr') %>%
  select(code, desc) %>%
  right_join(cdiff.2011.q4.2012.q1q3, by = c("code" = "pr10")) %>%
  rename("pr10" = "code", "pr10_desc" = "desc")

cdiff.2011.q4.2012.q1q3 <- df %>% 
  filter(year == 2011, type == 'pr') %>%
  select(code, desc) %>%
  right_join(cdiff.2011.q4.2012.q1q3, by = c("code" = "pr11")) %>%
  rename("pr11" = "code", "pr11_desc" = "desc")

cdiff.2011.q4.2012.q1q3 <- df %>% 
  filter(year == 2011, type == 'pr') %>%
  select(code, desc) %>%
  right_join(cdiff.2011.q4.2012.q1q3, by = c("code" = "pr12")) %>%
  rename("pr12" = "code", "pr12_desc" = "desc")

cdiff.2011.q4.2012.q1q3 <- df %>% 
  filter(year == 2011, type == 'pr') %>%
  select(code, desc) %>%
  right_join(cdiff.2011.q4.2012.q1q3, by = c("code" = "pr13")) %>%
  rename("pr13" = "code", "pr13_desc" = "desc")

cdiff.2011.q4.2012.q1q3 <- df %>% 
  filter(year == 2011, type == 'pr') %>%
  select(code, desc) %>%
  right_join(cdiff.2011.q4.2012.q1q3, by = c("code" = "pr14")) %>%
  rename("pr14" = "code", "pr14_desc" = "desc")

cdiff.2011.q4.2012.q1q3 <- df %>% 
  filter(year == 2011, type == 'pr') %>%
  select(code, desc) %>%
  right_join(cdiff.2011.q4.2012.q1q3, by = c("code" = "pr15")) %>%
  rename("pr15" = "code", "pr15_desc" = "desc")


#############################################################################################################
###     2012 Q4, 2013 Q1 - Q3    
#############################################################################################################

cdiff.2012.q4.2013.q1q3 <- cdiff.nrd %>% 
  filter(nrd_year == 2012, dqtr == '4')
cdiff.2012.q4.2013.q1q3 <- cdiff.nrd %>% 
  filter(nrd_year == 2013, dqtr != '4') %>%
  bind_rows(cdiff.2012.q4.2013.q1q3)

#############################################################################################################
###     2012 Q4, 2013 Q1 - Q3    DX
#############################################################################################################

cdiff.2012.q4.2013.q1q3 <- df %>% 
  filter(year == 2012, type == 'dx') %>%
  select(code, desc) %>%
  right_join(cdiff.2012.q4.2013.q1q3, by = c("code" = "dx1")) %>%
  rename("dx1" = "code", "dx1_desc" = "desc")

cdiff.2012.q4.2013.q1q3 <- df %>% 
  filter(year == 2012, type == 'dx') %>%
  select(code, desc) %>%
  right_join(cdiff.2012.q4.2013.q1q3, by = c("code" = "dx2")) %>%
  rename("dx2" = "code", "dx2_desc" = "desc")

cdiff.2012.q4.2013.q1q3 <- df %>% 
  filter(year == 2012, type == 'dx') %>%
  select(code, desc) %>%
  right_join(cdiff.2012.q4.2013.q1q3, by = c("code" = "dx3")) %>%
  rename("dx3" = "code", "dx3_desc" = "desc")

cdiff.2012.q4.2013.q1q3 <- df %>% 
  filter(year == 2012, type == 'dx') %>%
  select(code, desc) %>%
  right_join(cdiff.2012.q4.2013.q1q3, by = c("code" = "dx4")) %>%
  rename("dx4" = "code", "dx4_desc" = "desc")

cdiff.2012.q4.2013.q1q3 <- df %>% 
  filter(year == 2012, type == 'dx') %>%
  select(code, desc) %>%
  right_join(cdiff.2012.q4.2013.q1q3, by = c("code" = "dx5")) %>%
  rename("dx5" = "code", "dx5_desc" = "desc")

cdiff.2012.q4.2013.q1q3 <- df %>% 
  filter(year == 2012, type == 'dx') %>%
  select(code, desc) %>%
  right_join(cdiff.2012.q4.2013.q1q3, by = c("code" = "dx6")) %>%
  rename("dx6" = "code", "dx6_desc" = "desc")

cdiff.2012.q4.2013.q1q3 <- df %>% 
  filter(year == 2012, type == 'dx') %>%
  select(code, desc) %>%
  right_join(cdiff.2012.q4.2013.q1q3, by = c("code" = "dx7")) %>%
  rename("dx7" = "code", "dx7_desc" = "desc")

cdiff.2012.q4.2013.q1q3 <- df %>% 
  filter(year == 2012, type == 'dx') %>%
  select(code, desc) %>%
  right_join(cdiff.2012.q4.2013.q1q3, by = c("code" = "dx8")) %>%
  rename("dx8" = "code", "dx8_desc" = "desc")

cdiff.2012.q4.2013.q1q3 <- df %>% 
  filter(year == 2012, type == 'dx') %>%
  select(code, desc) %>%
  right_join(cdiff.2012.q4.2013.q1q3, by = c("code" = "dx9")) %>%
  rename("dx9" = "code", "dx9_desc" = "desc")

cdiff.2012.q4.2013.q1q3 <- df %>% 
  filter(year == 2012, type == 'dx') %>%
  select(code, desc) %>%
  right_join(cdiff.2012.q4.2013.q1q3, by = c("code" = "dx10")) %>%
  rename("dx10" = "code", "dx10_desc" = "desc")

cdiff.2012.q4.2013.q1q3 <- df %>% 
  filter(year == 2012, type == 'dx') %>%
  select(code, desc) %>%
  right_join(cdiff.2012.q4.2013.q1q3, by = c("code" = "dx11")) %>%
  rename("dx11" = "code", "dx11_desc" = "desc")

cdiff.2012.q4.2013.q1q3 <- df %>% 
  filter(year == 2012, type == 'dx') %>%
  select(code, desc) %>%
  right_join(cdiff.2012.q4.2013.q1q3, by = c("code" = "dx12")) %>%
  rename("dx12" = "code", "dx12_desc" = "desc")

cdiff.2012.q4.2013.q1q3 <- df %>% 
  filter(year == 2012, type == 'dx') %>%
  select(code, desc) %>%
  right_join(cdiff.2012.q4.2013.q1q3, by = c("code" = "dx13")) %>%
  rename("dx13" = "code", "dx13_desc" = "desc")

cdiff.2012.q4.2013.q1q3 <- df %>% 
  filter(year == 2012, type == 'dx') %>%
  select(code, desc) %>%
  right_join(cdiff.2012.q4.2013.q1q3, by = c("code" = "dx14")) %>%
  rename("dx14" = "code", "dx14_desc" = "desc")

cdiff.2012.q4.2013.q1q3 <- df %>% 
  filter(year == 2012, type == 'dx') %>%
  select(code, desc) %>%
  right_join(cdiff.2012.q4.2013.q1q3, by = c("code" = "dx15")) %>%
  rename("dx15" = "code", "dx15_desc" = "desc")

cdiff.2012.q4.2013.q1q3 <- df %>% 
  filter(year == 2012, type == 'dx') %>%
  select(code, desc) %>%
  right_join(cdiff.2012.q4.2013.q1q3, by = c("code" = "dx16")) %>%
  rename("dx16" = "code", "dx16_desc" = "desc")

cdiff.2012.q4.2013.q1q3 <- df %>% 
  filter(year == 2012, type == 'dx') %>%
  select(code, desc) %>%
  right_join(cdiff.2012.q4.2013.q1q3, by = c("code" = "dx17")) %>%
  rename("dx17" = "code", "dx17_desc" = "desc")

cdiff.2012.q4.2013.q1q3 <- df %>% 
  filter(year == 2012, type == 'dx') %>%
  select(code, desc) %>%
  right_join(cdiff.2012.q4.2013.q1q3, by = c("code" = "dx18")) %>%
  rename("dx18" = "code", "dx18_desc" = "desc")

cdiff.2012.q4.2013.q1q3 <- df %>% 
  filter(year == 2012, type == 'dx') %>%
  select(code, desc) %>%
  right_join(cdiff.2012.q4.2013.q1q3, by = c("code" = "dx19")) %>%
  rename("dx19" = "code", "dx19_desc" = "desc")

cdiff.2012.q4.2013.q1q3 <- df %>% 
  filter(year == 2012, type == 'dx') %>%
  select(code, desc) %>%
  right_join(cdiff.2012.q4.2013.q1q3, by = c("code" = "dx20")) %>%
  rename("dx20" = "code", "dx20_desc" = "desc")

cdiff.2012.q4.2013.q1q3 <- df %>% 
  filter(year == 2012, type == 'dx') %>%
  select(code, desc) %>%
  right_join(cdiff.2012.q4.2013.q1q3, by = c("code" = "dx21")) %>%
  rename("dx21" = "code", "dx21_desc" = "desc")

cdiff.2012.q4.2013.q1q3 <- df %>% 
  filter(year == 2012, type == 'dx') %>%
  select(code, desc) %>%
  right_join(cdiff.2012.q4.2013.q1q3, by = c("code" = "dx22")) %>%
  rename("dx22" = "code", "dx22_desc" = "desc")

cdiff.2012.q4.2013.q1q3 <- df %>% 
  filter(year == 2012, type == 'dx') %>%
  select(code, desc) %>%
  right_join(cdiff.2012.q4.2013.q1q3, by = c("code" = "dx23")) %>%
  rename("dx23" = "code", "dx23_desc" = "desc")

cdiff.2012.q4.2013.q1q3 <- df %>% 
  filter(year == 2012, type == 'dx') %>%
  select(code, desc) %>%
  right_join(cdiff.2012.q4.2013.q1q3, by = c("code" = "dx24")) %>%
  rename("dx24" = "code", "dx24_desc" = "desc")

cdiff.2012.q4.2013.q1q3 <- df %>% 
  filter(year == 2012, type == 'dx') %>%
  select(code, desc) %>%
  right_join(cdiff.2012.q4.2013.q1q3, by = c("code" = "dx25")) %>%
  rename("dx25" = "code", "dx25_desc" = "desc")

cdiff.2012.q4.2013.q1q3 <- df %>% 
  filter(year == 2012, type == 'dx') %>%
  select(code, desc) %>%
  right_join(cdiff.2012.q4.2013.q1q3, by = c("code" = "dx26")) %>%
  rename("dx26" = "code", "dx26_desc" = "desc")

cdiff.2012.q4.2013.q1q3 <- df %>% 
  filter(year == 2012, type == 'dx') %>%
  select(code, desc) %>%
  right_join(cdiff.2012.q4.2013.q1q3, by = c("code" = "dx27")) %>%
  rename("dx27" = "code", "dx27_desc" = "desc")

cdiff.2012.q4.2013.q1q3 <- df %>% 
  filter(year == 2012, type == 'dx') %>%
  select(code, desc) %>%
  right_join(cdiff.2012.q4.2013.q1q3, by = c("code" = "dx28")) %>%
  rename("dx28" = "code", "dx28_desc" = "desc")

cdiff.2012.q4.2013.q1q3 <- df %>% 
  filter(year == 2012, type == 'dx') %>%
  select(code, desc) %>%
  right_join(cdiff.2012.q4.2013.q1q3, by = c("code" = "dx29")) %>%
  rename("dx29" = "code", "dx29_desc" = "desc")

cdiff.2012.q4.2013.q1q3 <- df %>% 
  filter(year == 2012, type == 'dx') %>%
  select(code, desc) %>%
  right_join(cdiff.2012.q4.2013.q1q3, by = c("code" = "dx30")) %>%
  rename("dx30" = "code", "dx30_desc" = "desc")

#############################################################################################################
###     2012 Q4, 2013 Q1 - Q3   PR
#############################################################################################################

cdiff.2012.q4.2013.q1q3 <- df %>% 
  filter(year == 2012, type == 'pr') %>%
  select(code, desc) %>%
  right_join(cdiff.2012.q4.2013.q1q3, by = c("code" = "pr1")) %>%
  rename("pr1" = "code", "pr1_desc" = "desc")

cdiff.2012.q4.2013.q1q3 <- df %>% 
  filter(year == 2012, type == 'pr') %>%
  select(code, desc) %>%
  right_join(cdiff.2012.q4.2013.q1q3, by = c("code" = "pr2")) %>%
  rename("pr2" = "code", "pr2_desc" = "desc")

cdiff.2012.q4.2013.q1q3 <- df %>% 
  filter(year == 2012, type == 'pr') %>%
  select(code, desc) %>%
  right_join(cdiff.2012.q4.2013.q1q3, by = c("code" = "pr3")) %>%
  rename("pr3" = "code", "pr3_desc" = "desc")

cdiff.2012.q4.2013.q1q3 <- df %>% 
  filter(year == 2012, type == 'pr') %>%
  select(code, desc) %>%
  right_join(cdiff.2012.q4.2013.q1q3, by = c("code" = "pr4")) %>%
  rename("pr4" = "code", "pr4_desc" = "desc")

cdiff.2012.q4.2013.q1q3 <- df %>% 
  filter(year == 2012, type == 'pr') %>%
  select(code, desc) %>%
  right_join(cdiff.2012.q4.2013.q1q3, by = c("code" = "pr5")) %>%
  rename("pr5" = "code", "pr5_desc" = "desc")

cdiff.2012.q4.2013.q1q3 <- df %>% 
  filter(year == 2012, type == 'pr') %>%
  select(code, desc) %>%
  right_join(cdiff.2012.q4.2013.q1q3, by = c("code" = "pr6")) %>%
  rename("pr6" = "code", "pr6_desc" = "desc")

cdiff.2012.q4.2013.q1q3 <- df %>% 
  filter(year == 2012, type == 'pr') %>%
  select(code, desc) %>%
  right_join(cdiff.2012.q4.2013.q1q3, by = c("code" = "pr7")) %>%
  rename("pr7" = "code", "pr7_desc" = "desc")

cdiff.2012.q4.2013.q1q3 <- df %>% 
  filter(year == 2012, type == 'pr') %>%
  select(code, desc) %>%
  right_join(cdiff.2012.q4.2013.q1q3, by = c("code" = "pr8")) %>%
  rename("pr8" = "code", "pr8_desc" = "desc")

cdiff.2012.q4.2013.q1q3 <- df %>% 
  filter(year == 2012, type == 'pr') %>%
  select(code, desc) %>%
  right_join(cdiff.2012.q4.2013.q1q3, by = c("code" = "pr9")) %>%
  rename("pr9" = "code", "pr9_desc" = "desc")

cdiff.2012.q4.2013.q1q3 <- df %>% 
  filter(year == 2012, type == 'pr') %>%
  select(code, desc) %>%
  right_join(cdiff.2012.q4.2013.q1q3, by = c("code" = "pr10")) %>%
  rename("pr10" = "code", "pr10_desc" = "desc")

cdiff.2012.q4.2013.q1q3 <- df %>% 
  filter(year == 2012, type == 'pr') %>%
  select(code, desc) %>%
  right_join(cdiff.2012.q4.2013.q1q3, by = c("code" = "pr11")) %>%
  rename("pr11" = "code", "pr11_desc" = "desc")

cdiff.2012.q4.2013.q1q3 <- df %>% 
  filter(year == 2012, type == 'pr') %>%
  select(code, desc) %>%
  right_join(cdiff.2012.q4.2013.q1q3, by = c("code" = "pr12")) %>%
  rename("pr12" = "code", "pr12_desc" = "desc")

cdiff.2012.q4.2013.q1q3 <- df %>% 
  filter(year == 2012, type == 'pr') %>%
  select(code, desc) %>%
  right_join(cdiff.2012.q4.2013.q1q3, by = c("code" = "pr13")) %>%
  rename("pr13" = "code", "pr13_desc" = "desc")

cdiff.2012.q4.2013.q1q3 <- df %>% 
  filter(year == 2012, type == 'pr') %>%
  select(code, desc) %>%
  right_join(cdiff.2012.q4.2013.q1q3, by = c("code" = "pr14")) %>%
  rename("pr14" = "code", "pr14_desc" = "desc")

cdiff.2012.q4.2013.q1q3 <- df %>% 
  filter(year == 2012, type == 'pr') %>%
  select(code, desc) %>%
  right_join(cdiff.2012.q4.2013.q1q3, by = c("code" = "pr15")) %>%
  rename("pr15" = "code", "pr15_desc" = "desc")

#############################################################################################################
###     2013 Q4, 2014 Q1 - Q3    
#############################################################################################################

cdiff.2013.q4.2014.q1q3 <- cdiff.nrd %>% 
  filter(nrd_year == 2013, dqtr == '4')
cdiff.2013.q4.2014.q1q3 <- cdiff.nrd %>% 
  filter(nrd_year == 2014, dqtr != '4') %>%
  bind_rows(cdiff.2013.q4.2014.q1q3)

#############################################################################################################
###     2013 Q4, 2014 Q1 - Q3    DX
#############################################################################################################

cdiff.2013.q4.2014.q1q3 <- df %>% 
  filter(year == 2013, type == 'dx') %>%
  select(code, desc) %>%
  right_join(cdiff.2013.q4.2014.q1q3, by = c("code" = "dx1")) %>%
  rename("dx1" = "code", "dx1_desc" = "desc")

cdiff.2013.q4.2014.q1q3 <- df %>% 
  filter(year == 2013, type == 'dx') %>%
  select(code, desc) %>%
  right_join(cdiff.2013.q4.2014.q1q3, by = c("code" = "dx2")) %>%
  rename("dx2" = "code", "dx2_desc" = "desc")

cdiff.2013.q4.2014.q1q3 <- df %>% 
  filter(year == 2013, type == 'dx') %>%
  select(code, desc) %>%
  right_join(cdiff.2013.q4.2014.q1q3, by = c("code" = "dx3")) %>%
  rename("dx3" = "code", "dx3_desc" = "desc")

cdiff.2013.q4.2014.q1q3 <- df %>% 
  filter(year == 2013, type == 'dx') %>%
  select(code, desc) %>%
  right_join(cdiff.2013.q4.2014.q1q3, by = c("code" = "dx4")) %>%
  rename("dx4" = "code", "dx4_desc" = "desc")

cdiff.2013.q4.2014.q1q3 <- df %>% 
  filter(year == 2013, type == 'dx') %>%
  select(code, desc) %>%
  right_join(cdiff.2013.q4.2014.q1q3, by = c("code" = "dx5")) %>%
  rename("dx5" = "code", "dx5_desc" = "desc")

cdiff.2013.q4.2014.q1q3 <- df %>% 
  filter(year == 2013, type == 'dx') %>%
  select(code, desc) %>%
  right_join(cdiff.2013.q4.2014.q1q3, by = c("code" = "dx6")) %>%
  rename("dx6" = "code", "dx6_desc" = "desc")

cdiff.2013.q4.2014.q1q3 <- df %>% 
  filter(year == 2013, type == 'dx') %>%
  select(code, desc) %>%
  right_join(cdiff.2013.q4.2014.q1q3, by = c("code" = "dx7")) %>%
  rename("dx7" = "code", "dx7_desc" = "desc")

cdiff.2013.q4.2014.q1q3 <- df %>% 
  filter(year == 2013, type == 'dx') %>%
  select(code, desc) %>%
  right_join(cdiff.2013.q4.2014.q1q3, by = c("code" = "dx8")) %>%
  rename("dx8" = "code", "dx8_desc" = "desc")

cdiff.2013.q4.2014.q1q3 <- df %>% 
  filter(year == 2013, type == 'dx') %>%
  select(code, desc) %>%
  right_join(cdiff.2013.q4.2014.q1q3, by = c("code" = "dx9")) %>%
  rename("dx9" = "code", "dx9_desc" = "desc")

cdiff.2013.q4.2014.q1q3 <- df %>% 
  filter(year == 2013, type == 'dx') %>%
  select(code, desc) %>%
  right_join(cdiff.2013.q4.2014.q1q3, by = c("code" = "dx10")) %>%
  rename("dx10" = "code", "dx10_desc" = "desc")

cdiff.2013.q4.2014.q1q3 <- df %>% 
  filter(year == 2013, type == 'dx') %>%
  select(code, desc) %>%
  right_join(cdiff.2013.q4.2014.q1q3, by = c("code" = "dx11")) %>%
  rename("dx11" = "code", "dx11_desc" = "desc")

cdiff.2013.q4.2014.q1q3 <- df %>% 
  filter(year == 2013, type == 'dx') %>%
  select(code, desc) %>%
  right_join(cdiff.2013.q4.2014.q1q3, by = c("code" = "dx12")) %>%
  rename("dx12" = "code", "dx12_desc" = "desc")

cdiff.2013.q4.2014.q1q3 <- df %>% 
  filter(year == 2013, type == 'dx') %>%
  select(code, desc) %>%
  right_join(cdiff.2013.q4.2014.q1q3, by = c("code" = "dx13")) %>%
  rename("dx13" = "code", "dx13_desc" = "desc")

cdiff.2013.q4.2014.q1q3 <- df %>% 
  filter(year == 2013, type == 'dx') %>%
  select(code, desc) %>%
  right_join(cdiff.2013.q4.2014.q1q3, by = c("code" = "dx14")) %>%
  rename("dx14" = "code", "dx14_desc" = "desc")

cdiff.2013.q4.2014.q1q3 <- df %>% 
  filter(year == 2013, type == 'dx') %>%
  select(code, desc) %>%
  right_join(cdiff.2013.q4.2014.q1q3, by = c("code" = "dx15")) %>%
  rename("dx15" = "code", "dx15_desc" = "desc")

cdiff.2013.q4.2014.q1q3 <- df %>% 
  filter(year == 2013, type == 'dx') %>%
  select(code, desc) %>%
  right_join(cdiff.2013.q4.2014.q1q3, by = c("code" = "dx16")) %>%
  rename("dx16" = "code", "dx16_desc" = "desc")

cdiff.2013.q4.2014.q1q3 <- df %>% 
  filter(year == 2013, type == 'dx') %>%
  select(code, desc) %>%
  right_join(cdiff.2013.q4.2014.q1q3, by = c("code" = "dx17")) %>%
  rename("dx17" = "code", "dx17_desc" = "desc")

cdiff.2013.q4.2014.q1q3 <- df %>% 
  filter(year == 2013, type == 'dx') %>%
  select(code, desc) %>%
  right_join(cdiff.2013.q4.2014.q1q3, by = c("code" = "dx18")) %>%
  rename("dx18" = "code", "dx18_desc" = "desc")

cdiff.2013.q4.2014.q1q3 <- df %>% 
  filter(year == 2013, type == 'dx') %>%
  select(code, desc) %>%
  right_join(cdiff.2013.q4.2014.q1q3, by = c("code" = "dx19")) %>%
  rename("dx19" = "code", "dx19_desc" = "desc")

cdiff.2013.q4.2014.q1q3 <- df %>% 
  filter(year == 2013, type == 'dx') %>%
  select(code, desc) %>%
  right_join(cdiff.2013.q4.2014.q1q3, by = c("code" = "dx20")) %>%
  rename("dx20" = "code", "dx20_desc" = "desc")

cdiff.2013.q4.2014.q1q3 <- df %>% 
  filter(year == 2013, type == 'dx') %>%
  select(code, desc) %>%
  right_join(cdiff.2013.q4.2014.q1q3, by = c("code" = "dx21")) %>%
  rename("dx21" = "code", "dx21_desc" = "desc")

cdiff.2013.q4.2014.q1q3 <- df %>% 
  filter(year == 2013, type == 'dx') %>%
  select(code, desc) %>%
  right_join(cdiff.2013.q4.2014.q1q3, by = c("code" = "dx22")) %>%
  rename("dx22" = "code", "dx22_desc" = "desc")

cdiff.2013.q4.2014.q1q3 <- df %>% 
  filter(year == 2013, type == 'dx') %>%
  select(code, desc) %>%
  right_join(cdiff.2013.q4.2014.q1q3, by = c("code" = "dx23")) %>%
  rename("dx23" = "code", "dx23_desc" = "desc")

cdiff.2013.q4.2014.q1q3 <- df %>% 
  filter(year == 2013, type == 'dx') %>%
  select(code, desc) %>%
  right_join(cdiff.2013.q4.2014.q1q3, by = c("code" = "dx24")) %>%
  rename("dx24" = "code", "dx24_desc" = "desc")

cdiff.2013.q4.2014.q1q3 <- df %>% 
  filter(year == 2013, type == 'dx') %>%
  select(code, desc) %>%
  right_join(cdiff.2013.q4.2014.q1q3, by = c("code" = "dx25")) %>%
  rename("dx25" = "code", "dx25_desc" = "desc")

cdiff.2013.q4.2014.q1q3 <- df %>% 
  filter(year == 2013, type == 'dx') %>%
  select(code, desc) %>%
  right_join(cdiff.2013.q4.2014.q1q3, by = c("code" = "dx26")) %>%
  rename("dx26" = "code", "dx26_desc" = "desc")

cdiff.2013.q4.2014.q1q3 <- df %>% 
  filter(year == 2013, type == 'dx') %>%
  select(code, desc) %>%
  right_join(cdiff.2013.q4.2014.q1q3, by = c("code" = "dx27")) %>%
  rename("dx27" = "code", "dx27_desc" = "desc")

cdiff.2013.q4.2014.q1q3 <- df %>% 
  filter(year == 2013, type == 'dx') %>%
  select(code, desc) %>%
  right_join(cdiff.2013.q4.2014.q1q3, by = c("code" = "dx28")) %>%
  rename("dx28" = "code", "dx28_desc" = "desc")

cdiff.2013.q4.2014.q1q3 <- df %>% 
  filter(year == 2013, type == 'dx') %>%
  select(code, desc) %>%
  right_join(cdiff.2013.q4.2014.q1q3, by = c("code" = "dx29")) %>%
  rename("dx29" = "code", "dx29_desc" = "desc")

cdiff.2013.q4.2014.q1q3 <- df %>% 
  filter(year == 2013, type == 'dx') %>%
  select(code, desc) %>%
  right_join(cdiff.2013.q4.2014.q1q3, by = c("code" = "dx30")) %>%
  rename("dx30" = "code", "dx30_desc" = "desc")

#############################################################################################################
###     2013 Q4, 2014 Q1 - Q3    PR
#############################################################################################################

cdiff.2013.q4.2014.q1q3 <- df %>% 
  filter(year == 2013, type == 'pr') %>%
  select(code, desc) %>%
  right_join(cdiff.2013.q4.2014.q1q3, by = c("code" = "pr1")) %>%
  rename("pr1" = "code", "pr1_desc" = "desc")

cdiff.2013.q4.2014.q1q3 <- df %>% 
  filter(year == 2013, type == 'pr') %>%
  select(code, desc) %>%
  right_join(cdiff.2013.q4.2014.q1q3, by = c("code" = "pr2")) %>%
  rename("pr2" = "code", "pr2_desc" = "desc")

cdiff.2013.q4.2014.q1q3 <- df %>% 
  filter(year == 2013, type == 'pr') %>%
  select(code, desc) %>%
  right_join(cdiff.2013.q4.2014.q1q3, by = c("code" = "pr3")) %>%
  rename("pr3" = "code", "pr3_desc" = "desc")

cdiff.2013.q4.2014.q1q3 <- df %>% 
  filter(year == 2013, type == 'pr') %>%
  select(code, desc) %>%
  right_join(cdiff.2013.q4.2014.q1q3, by = c("code" = "pr4")) %>%
  rename("pr4" = "code", "pr4_desc" = "desc")

cdiff.2013.q4.2014.q1q3 <- df %>% 
  filter(year == 2013, type == 'pr') %>%
  select(code, desc) %>%
  right_join(cdiff.2013.q4.2014.q1q3, by = c("code" = "pr5")) %>%
  rename("pr5" = "code", "pr5_desc" = "desc")

cdiff.2013.q4.2014.q1q3 <- df %>% 
  filter(year == 2013, type == 'pr') %>%
  select(code, desc) %>%
  right_join(cdiff.2013.q4.2014.q1q3, by = c("code" = "pr6")) %>%
  rename("pr6" = "code", "pr6_desc" = "desc")

cdiff.2013.q4.2014.q1q3 <- df %>% 
  filter(year == 2013, type == 'pr') %>%
  select(code, desc) %>%
  right_join(cdiff.2013.q4.2014.q1q3, by = c("code" = "pr7")) %>%
  rename("pr7" = "code", "pr7_desc" = "desc")

cdiff.2013.q4.2014.q1q3 <- df %>% 
  filter(year == 2013, type == 'pr') %>%
  select(code, desc) %>%
  right_join(cdiff.2013.q4.2014.q1q3, by = c("code" = "pr8")) %>%
  rename("pr8" = "code", "pr8_desc" = "desc")

cdiff.2013.q4.2014.q1q3 <- df %>% 
  filter(year == 2013, type == 'pr') %>%
  select(code, desc) %>%
  right_join(cdiff.2013.q4.2014.q1q3, by = c("code" = "pr9")) %>%
  rename("pr9" = "code", "pr9_desc" = "desc")

cdiff.2013.q4.2014.q1q3 <- df %>% 
  filter(year == 2013, type == 'pr') %>%
  select(code, desc) %>%
  right_join(cdiff.2013.q4.2014.q1q3, by = c("code" = "pr10")) %>%
  rename("pr10" = "code", "pr10_desc" = "desc")

cdiff.2013.q4.2014.q1q3 <- df %>% 
  filter(year == 2013, type == 'pr') %>%
  select(code, desc) %>%
  right_join(cdiff.2013.q4.2014.q1q3, by = c("code" = "pr11")) %>%
  rename("pr11" = "code", "pr11_desc" = "desc")

cdiff.2013.q4.2014.q1q3 <- df %>% 
  filter(year == 2013, type == 'pr') %>%
  select(code, desc) %>%
  right_join(cdiff.2013.q4.2014.q1q3, by = c("code" = "pr12")) %>%
  rename("pr12" = "code", "pr12_desc" = "desc")

cdiff.2013.q4.2014.q1q3 <- df %>% 
  filter(year == 2013, type == 'pr') %>%
  select(code, desc) %>%
  right_join(cdiff.2013.q4.2014.q1q3, by = c("code" = "pr13")) %>%
  rename("pr13" = "code", "pr13_desc" = "desc")

cdiff.2013.q4.2014.q1q3 <- df %>% 
  filter(year == 2013, type == 'pr') %>%
  select(code, desc) %>%
  right_join(cdiff.2013.q4.2014.q1q3, by = c("code" = "pr14")) %>%
  rename("pr14" = "code", "pr14_desc" = "desc")

cdiff.2013.q4.2014.q1q3 <- df %>% 
  filter(year == 2013, type == 'pr') %>%
  select(code, desc) %>%
  right_join(cdiff.2013.q4.2014.q1q3, by = c("code" = "pr15")) %>%
  rename("pr15" = "code", "pr15_desc" = "desc")

#############################################################################################################
###     2014 Q4, 2015 Q1 - Q3    DX
#############################################################################################################

cdiff.2014.q4.2015.q1q3 <- cdiff.nrd %>% 
  filter(nrd_year == 2014, dqtr == '4')
cdiff.2014.q4.2015.q1q3 <- cdiff.nrd %>% 
  filter(nrd_year == 2015, dqtr != '4') %>%
  bind_rows(cdiff.2014.q4.2015.q1q3)

#############################################################################################################
###     2014 Q4, 2015 Q1 - Q3    DX
#############################################################################################################

cdiff.2014.q4.2015.q1q3 <- df %>% 
  filter(year == 2014, type == 'dx') %>%
  select(code, desc) %>%
  right_join(cdiff.2014.q4.2015.q1q3, by = c("code" = "dx1")) %>%
  rename("dx1" = "code", "dx1_desc" = "desc")

cdiff.2014.q4.2015.q1q3 <- df %>% 
  filter(year == 2014, type == 'dx') %>%
  select(code, desc) %>%
  right_join(cdiff.2014.q4.2015.q1q3, by = c("code" = "dx2")) %>%
  rename("dx2" = "code", "dx2_desc" = "desc")

cdiff.2014.q4.2015.q1q3 <- df %>% 
  filter(year == 2014, type == 'dx') %>%
  select(code, desc) %>%
  right_join(cdiff.2014.q4.2015.q1q3, by = c("code" = "dx3")) %>%
  rename("dx3" = "code", "dx3_desc" = "desc")

cdiff.2014.q4.2015.q1q3 <- df %>% 
  filter(year == 2014, type == 'dx') %>%
  select(code, desc) %>%
  right_join(cdiff.2014.q4.2015.q1q3, by = c("code" = "dx4")) %>%
  rename("dx4" = "code", "dx4_desc" = "desc")

cdiff.2014.q4.2015.q1q3 <- df %>% 
  filter(year == 2014, type == 'dx') %>%
  select(code, desc) %>%
  right_join(cdiff.2014.q4.2015.q1q3, by = c("code" = "dx5")) %>%
  rename("dx5" = "code", "dx5_desc" = "desc")

cdiff.2014.q4.2015.q1q3 <- df %>% 
  filter(year == 2014, type == 'dx') %>%
  select(code, desc) %>%
  right_join(cdiff.2014.q4.2015.q1q3, by = c("code" = "dx6")) %>%
  rename("dx6" = "code", "dx6_desc" = "desc")

cdiff.2014.q4.2015.q1q3 <- df %>% 
  filter(year == 2014, type == 'dx') %>%
  select(code, desc) %>%
  right_join(cdiff.2014.q4.2015.q1q3, by = c("code" = "dx7")) %>%
  rename("dx7" = "code", "dx7_desc" = "desc")

cdiff.2014.q4.2015.q1q3 <- df %>% 
  filter(year == 2014, type == 'dx') %>%
  select(code, desc) %>%
  right_join(cdiff.2014.q4.2015.q1q3, by = c("code" = "dx8")) %>%
  rename("dx8" = "code", "dx8_desc" = "desc")

cdiff.2014.q4.2015.q1q3 <- df %>% 
  filter(year == 2014, type == 'dx') %>%
  select(code, desc) %>%
  right_join(cdiff.2014.q4.2015.q1q3, by = c("code" = "dx9")) %>%
  rename("dx9" = "code", "dx9_desc" = "desc")

cdiff.2014.q4.2015.q1q3 <- df %>% 
  filter(year == 2014, type == 'dx') %>%
  select(code, desc) %>%
  right_join(cdiff.2014.q4.2015.q1q3, by = c("code" = "dx10")) %>%
  rename("dx10" = "code", "dx10_desc" = "desc")

cdiff.2014.q4.2015.q1q3 <- df %>% 
  filter(year == 2014, type == 'dx') %>%
  select(code, desc) %>%
  right_join(cdiff.2014.q4.2015.q1q3, by = c("code" = "dx11")) %>%
  rename("dx11" = "code", "dx11_desc" = "desc")

cdiff.2014.q4.2015.q1q3 <- df %>% 
  filter(year == 2014, type == 'dx') %>%
  select(code, desc) %>%
  right_join(cdiff.2014.q4.2015.q1q3, by = c("code" = "dx12")) %>%
  rename("dx12" = "code", "dx12_desc" = "desc")

cdiff.2014.q4.2015.q1q3 <- df %>% 
  filter(year == 2014, type == 'dx') %>%
  select(code, desc) %>%
  right_join(cdiff.2014.q4.2015.q1q3, by = c("code" = "dx13")) %>%
  rename("dx13" = "code", "dx13_desc" = "desc")

cdiff.2014.q4.2015.q1q3 <- df %>% 
  filter(year == 2014, type == 'dx') %>%
  select(code, desc) %>%
  right_join(cdiff.2014.q4.2015.q1q3, by = c("code" = "dx14")) %>%
  rename("dx14" = "code", "dx14_desc" = "desc")

cdiff.2014.q4.2015.q1q3 <- df %>% 
  filter(year == 2014, type == 'dx') %>%
  select(code, desc) %>%
  right_join(cdiff.2014.q4.2015.q1q3, by = c("code" = "dx15")) %>%
  rename("dx15" = "code", "dx15_desc" = "desc")

cdiff.2014.q4.2015.q1q3 <- df %>% 
  filter(year == 2014, type == 'dx') %>%
  select(code, desc) %>%
  right_join(cdiff.2014.q4.2015.q1q3, by = c("code" = "dx16")) %>%
  rename("dx16" = "code", "dx16_desc" = "desc")

cdiff.2014.q4.2015.q1q3 <- df %>% 
  filter(year == 2014, type == 'dx') %>%
  select(code, desc) %>%
  right_join(cdiff.2014.q4.2015.q1q3, by = c("code" = "dx17")) %>%
  rename("dx17" = "code", "dx17_desc" = "desc")

cdiff.2014.q4.2015.q1q3 <- df %>% 
  filter(year == 2014, type == 'dx') %>%
  select(code, desc) %>%
  right_join(cdiff.2014.q4.2015.q1q3, by = c("code" = "dx18")) %>%
  rename("dx18" = "code", "dx18_desc" = "desc")

cdiff.2014.q4.2015.q1q3 <- df %>% 
  filter(year == 2014, type == 'dx') %>%
  select(code, desc) %>%
  right_join(cdiff.2014.q4.2015.q1q3, by = c("code" = "dx19")) %>%
  rename("dx19" = "code", "dx19_desc" = "desc")

cdiff.2014.q4.2014.q1q3 <- df %>% 
  filter(year == 2014, type == 'dx') %>%
  select(code, desc) %>%
  right_join(cdiff.2014.q4.2015.q1q3, by = c("code" = "dx20")) %>%
  rename("dx20" = "code", "dx20_desc" = "desc")

cdiff.2014.q4.2015.q1q3 <- df %>% 
  filter(year == 2014, type == 'dx') %>%
  select(code, desc) %>%
  right_join(cdiff.2014.q4.2015.q1q3, by = c("code" = "dx21")) %>%
  rename("dx21" = "code", "dx21_desc" = "desc")

cdiff.2014.q4.2014.q1q3 <- df %>% 
  filter(year == 2014, type == 'dx') %>%
  select(code, desc) %>%
  right_join(cdiff.2014.q4.2014.q1q3, by = c("code" = "dx22")) %>%
  rename("dx22" = "code", "dx22_desc" = "desc")

cdiff.2014.q4.2015.q1q3 <- df %>% 
  filter(year == 2014, type == 'dx') %>%
  select(code, desc) %>%
  right_join(cdiff.2014.q4.2015.q1q3, by = c("code" = "dx23")) %>%
  rename("dx23" = "code", "dx23_desc" = "desc")

cdiff.2014.q4.2015.q1q3 <- df %>% 
  filter(year == 2014, type == 'dx') %>%
  select(code, desc) %>%
  right_join(cdiff.2014.q4.2015.q1q3, by = c("code" = "dx24")) %>%
  rename("dx24" = "code", "dx24_desc" = "desc")

cdiff.2014.q4.2015.q1q3 <- df %>% 
  filter(year == 2014, type == 'dx') %>%
  select(code, desc) %>%
  right_join(cdiff.2014.q4.2015.q1q3, by = c("code" = "dx25")) %>%
  rename("dx25" = "code", "dx25_desc" = "desc")

cdiff.2014.q4.2015.q1q3 <- df %>% 
  filter(year == 2014, type == 'dx') %>%
  select(code, desc) %>%
  right_join(cdiff.2014.q4.2015.q1q3, by = c("code" = "dx26")) %>%
  rename("dx26" = "code", "dx26_desc" = "desc")

cdiff.2014.q4.2015.q1q3 <- df %>% 
  filter(year == 2014, type == 'dx') %>%
  select(code, desc) %>%
  right_join(cdiff.2014.q4.2015.q1q3, by = c("code" = "dx27")) %>%
  rename("dx27" = "code", "dx27_desc" = "desc")

cdiff.2014.q4.2015.q1q3 <- df %>% 
  filter(year == 2014, type == 'dx') %>%
  select(code, desc) %>%
  right_join(cdiff.2014.q4.2015.q1q3, by = c("code" = "dx28")) %>%
  rename("dx28" = "code", "dx28_desc" = "desc")

cdiff.2014.q4.2014.q1q3 <- df %>% 
  filter(year == 2014, type == 'dx') %>%
  select(code, desc) %>%
  right_join(cdiff.2014.q4.2015.q1q3, by = c("code" = "dx29")) %>%
  rename("dx29" = "code", "dx29_desc" = "desc")

cdiff.2014.q4.2015.q1q3 <- df %>% 
  filter(year == 2014, type == 'dx') %>%
  select(code, desc) %>%
  right_join(cdiff.2014.q4.2015.q1q3, by = c("code" = "dx30")) %>%
  rename("dx30" = "code", "dx30_desc" = "desc")

#############################################################################################################
###     2014 Q4, 2015 Q1 - Q3    PR
#############################################################################################################

cdiff.2014.q4.2015.q1q3 <- df %>% 
  filter(year == 2014, type == 'pr') %>%
  select(code, desc) %>%
  right_join(cdiff.2014.q4.2015.q1q3, by = c("code" = "pr1")) %>%
  rename("pr1" = "code", "pr1_desc" = "desc")

cdiff.2014.q4.2015.q1q3 <- df %>% 
  filter(year == 2014, type == 'pr') %>%
  select(code, desc) %>%
  right_join(cdiff.2014.q4.2015.q1q3, by = c("code" = "pr2")) %>%
  rename("pr2" = "code", "pr2_desc" = "desc")

cdiff.2014.q4.2015.q1q3 <- df %>% 
  filter(year == 2014, type == 'pr') %>%
  select(code, desc) %>%
  right_join(cdiff.2014.q4.2015.q1q3, by = c("code" = "pr3")) %>%
  rename("pr3" = "code", "pr3_desc" = "desc")

cdiff.2014.q4.2015.q1q3 <- df %>% 
  filter(year == 2014, type == 'pr') %>%
  select(code, desc) %>%
  right_join(cdiff.2014.q4.2015.q1q3, by = c("code" = "pr4")) %>%
  rename("pr4" = "code", "pr4_desc" = "desc")

cdiff.2014.q4.2015.q1q3 <- df %>% 
  filter(year == 2014, type == 'pr') %>%
  select(code, desc) %>%
  right_join(cdiff.2014.q4.2015.q1q3, by = c("code" = "pr5")) %>%
  rename("pr5" = "code", "pr5_desc" = "desc")

cdiff.2014.q4.2015.q1q3 <- df %>% 
  filter(year == 2014, type == 'pr') %>%
  select(code, desc) %>%
  right_join(cdiff.2014.q4.2015.q1q3, by = c("code" = "pr6")) %>%
  rename("pr6" = "code", "pr6_desc" = "desc")

cdiff.2014.q4.2015.q1q3 <- df %>% 
  filter(year == 2014, type == 'pr') %>%
  select(code, desc) %>%
  right_join(cdiff.2014.q4.2015.q1q3, by = c("code" = "pr7")) %>%
  rename("pr7" = "code", "pr7_desc" = "desc")

cdiff.2014.q4.2015.q1q3 <- df %>% 
  filter(year == 2014, type == 'pr') %>%
  select(code, desc) %>%
  right_join(cdiff.2014.q4.2015.q1q3, by = c("code" = "pr8")) %>%
  rename("pr8" = "code", "pr8_desc" = "desc")

cdiff.2014.q4.2015.q1q3 <- df %>% 
  filter(year == 2014, type == 'pr') %>%
  select(code, desc) %>%
  right_join(cdiff.2014.q4.2015.q1q3, by = c("code" = "pr9")) %>%
  rename("pr9" = "code", "pr9_desc" = "desc")

cdiff.2014.q4.2015.q1q3 <- df %>% 
  filter(year == 2014, type == 'pr') %>%
  select(code, desc) %>%
  right_join(cdiff.2014.q4.2015.q1q3, by = c("code" = "pr10")) %>%
  rename("pr10" = "code", "pr10_desc" = "desc")

cdiff.2014.q4.2015.q1q3 <- df %>% 
  filter(year == 2014, type == 'pr') %>%
  select(code, desc) %>%
  right_join(cdiff.2014.q4.2015.q1q3, by = c("code" = "pr11")) %>%
  rename("pr11" = "code", "pr11_desc" = "desc")

cdiff.2014.q4.2015.q1q3 <- df %>% 
  filter(year == 2014, type == 'pr') %>%
  select(code, desc) %>%
  right_join(cdiff.2014.q4.2015.q1q3, by = c("code" = "pr12")) %>%
  rename("pr12" = "code", "pr12_desc" = "desc")

cdiff.2014.q4.2015.q1q3 <- df %>% 
  filter(year == 2014, type == 'pr') %>%
  select(code, desc) %>%
  right_join(cdiff.2014.q4.2015.q1q3, by = c("code" = "pr13")) %>%
  rename("pr13" = "code", "pr13_desc" = "desc")

cdiff.2014.q4.2015.q1q3 <- df %>% 
  filter(year == 2014, type == 'pr') %>%
  select(code, desc) %>%
  right_join(cdiff.2014.q4.2014.q1q3, by = c("code" = "pr14")) %>%
  rename("pr14" = "code", "pr14_desc" = "desc")

cdiff.2014.q4.2015.q1q3 <- df %>% 
  filter(year == 2014, type == 'pr') %>%
  select(code, desc) %>%
  right_join(cdiff.2014.q4.2015.q1q3, by = c("code" = "pr15")) %>%
  rename("pr15" = "code", "pr15_desc" = "desc")



#############################################################################################################
###     2015 Q4
#############################################################################################################

cdiff.2015.q4 <- cdiff.nrd %>% 
  filter(nrd_year == 2015, dqtr == '4')

#############################################################################################################
###     2015 Q4    DX
#############################################################################################################

cdiff.2015.q4 <- df %>% 
  filter(year == 2015, type == 'dx') %>%
  select(code, desc) %>%
  right_join(cdiff.2015.q4, by = c("code" = "i10_dx1")) %>%
  rename("i10_dx1" = "code", "dx1_desc" = "desc")

cdiff.2015.q4 <- df %>% 
  filter(year == 2015, type == 'dx') %>%
  select(code, desc) %>%
  right_join(cdiff.2015.q4, by = c("code" = "i10_dx2")) %>%
  rename("i10_dx2" = "code", "dx2_desc" = "desc")

cdiff.2015.q4 <- df %>% 
  filter(year == 2015, type == 'dx') %>%
  select(code, desc) %>%
  right_join(cdiff.2015.q4, by = c("code" = "i10_dx3")) %>%
  rename("i10_dx3" = "code", "dx3_desc" = "desc")

cdiff.2015.q4 <- df %>% 
  filter(year == 2015, type == 'dx') %>%
  select(code, desc) %>%
  right_join(cdiff.2015.q4, by = c("code" = "i10_dx4")) %>%
  rename("i10_dx4" = "code", "dx4_desc" = "desc")

cdiff.2015.q4 <- df %>% 
  filter(year == 2015, type == 'dx') %>%
  select(code, desc) %>%
  right_join(cdiff.2015.q4, by = c("code" = "i10_dx5")) %>%
  rename("i10_dx5" = "code", "dx5_desc" = "desc")

cdiff.2015.q4 <- df %>% 
  filter(year == 2015, type == 'dx') %>%
  select(code, desc) %>%
  right_join(cdiff.2015.q4, by = c("code" = "i10_dx6")) %>%
  rename("i10_dx6" = "code", "dx6_desc" = "desc")

cdiff.2015.q4 <- df %>% 
  filter(year == 2015, type == 'dx') %>%
  select(code, desc) %>%
  right_join(cdiff.2015.q4, by = c("code" = "i10_dx7")) %>%
  rename("i10_dx7" = "code", "dx7_desc" = "desc")

cdiff.2015.q4 <- df %>% 
  filter(year == 2015, type == 'dx') %>%
  select(code, desc) %>%
  right_join(cdiff.2015.q4, by = c("code" = "i10_dx8")) %>%
  rename("i10_dx8" = "code", "dx8_desc" = "desc")

cdiff.2015.q4 <- df %>% 
  filter(year == 2015, type == 'dx') %>%
  select(code, desc) %>%
  right_join(cdiff.2015.q4, by = c("code" = "i10_dx9")) %>%
  rename("i10_dx9" = "code", "dx9_desc" = "desc")

cdiff.2015.q4 <- df %>% 
  filter(year == 2015, type == 'dx') %>%
  select(code, desc) %>%
  right_join(cdiff.2015.q4, by = c("code" = "i10_dx10")) %>%
  rename("i10_dx10" = "code", "dx10_desc" = "desc")

cdiff.2015.q4 <- df %>% 
  filter(year == 2015, type == 'dx') %>%
  select(code, desc) %>%
  right_join(cdiff.2015.q4, by = c("code" = "i10_dx11")) %>%
  rename("i10_dx11" = "code", "dx11_desc" = "desc")

cdiff.2015.q4 <- df %>% 
  filter(year == 2015, type == 'dx') %>%
  select(code, desc) %>%
  right_join(cdiff.2015.q4, by = c("code" = "i10_dx12")) %>%
  rename("i10_dx12" = "code", "dx12_desc" = "desc")

cdiff.2015.q4 <- df %>% 
  filter(year == 2015, type == 'dx') %>%
  select(code, desc) %>%
  right_join(cdiff.2015.q4, by = c("code" = "i10_dx13")) %>%
  rename("i10_dx13" = "code", "dx13_desc" = "desc")

cdiff.2015.q4 <- df %>% 
  filter(year == 2015, type == 'dx') %>%
  select(code, desc) %>%
  right_join(cdiff.2015.q4, by = c("code" = "i10_dx14")) %>%
  rename("i10_dx14" = "code", "dx14_desc" = "desc")

cdiff.2015.q4 <- df %>% 
  filter(year == 2015, type == 'dx') %>%
  select(code, desc) %>%
  right_join(cdiff.2015.q4, by = c("code" = "i10_dx15")) %>%
  rename("i10_dx15" = "code", "dx15_desc" = "desc")

cdiff.2015.q4 <- df %>% 
  filter(year == 2015, type == 'dx') %>%
  select(code, desc) %>%
  right_join(cdiff.2015.q4, by = c("code" = "i10_dx16")) %>%
  rename("i10_dx16" = "code", "dx16_desc" = "desc")

cdiff.2015.q4 <- df %>% 
  filter(year == 2015, type == 'dx') %>%
  select(code, desc) %>%
  right_join(cdiff.2015.q4, by = c("code" = "i10_dx17")) %>%
  rename("i10_dx17" = "code", "dx17_desc" = "desc")

cdiff.2015.q4 <- df %>% 
  filter(year == 2015, type == 'dx') %>%
  select(code, desc) %>%
  right_join(cdiff.2015.q4, by = c("code" = "i10_dx18")) %>%
  rename("i10_dx18" = "code", "dx18_desc" = "desc")

cdiff.2015.q4 <- df %>% 
  filter(year == 2015, type == 'dx') %>%
  select(code, desc) %>%
  right_join(cdiff.2015.q4, by = c("code" = "i10_dx19")) %>%
  rename("i10_dx19" = "code", "dx19_desc" = "desc")

cdiff.2015.q4 <- df %>% 
  filter(year == 2015, type == 'dx') %>%
  select(code, desc) %>%
  right_join(cdiff.2015.q4, by = c("code" = "i10_dx20")) %>%
  rename("i10_dx20" = "code", "dx20_desc" = "desc")

cdiff.2015.q4 <- df %>% 
  filter(year == 2015, type == 'dx') %>%
  select(code, desc) %>%
  right_join(cdiff.2015.q4, by = c("code" = "i10_dx21")) %>%
  rename("i10_dx21" = "code", "dx21_desc" = "desc")

cdiff.2015.q4 <- df %>% 
  filter(year == 2015, type == 'i10_dx') %>%
  select(code, desc) %>%
  right_join(cdiff.2015.q4, by = c("code" = "dx22")) %>%
  rename("i10_dx22" = "code", "dx22_desc" = "desc")

cdiff.2015.q4 <- df %>% 
  filter(year == 2015, type == 'dx') %>%
  select(code, desc) %>%
  right_join(cdiff.2015.q4, by = c("code" = "i10_dx23")) %>%
  rename("i10_dx23" = "code", "dx23_desc" = "desc")

cdiff.2015.q4 <- df %>% 
  filter(year == 2015, type == 'dx') %>%
  select(code, desc) %>%
  right_join(cdiff.2015.q4, by = c("code" = "i10_dx24")) %>%
  rename("i10_dx24" = "code", "dx24_desc" = "desc")

cdiff.2015.q4 <- df %>% 
  filter(year == 2015, type == 'dx') %>%
  select(code, desc) %>%
  right_join(cdiff.2015.q4, by = c("code" = "i10_dx25")) %>%
  rename("i10_dx25" = "code", "dx25_desc" = "desc")

cdiff.2015.q4 <- df %>% 
  filter(year == 2015, type == 'dx') %>%
  select(code, desc) %>%
  right_join(cdiff.2015.q4, by = c("code" = "i10_dx26")) %>%
  rename("i10_dx26" = "code", "dx26_desc" = "desc")

cdiff.2015.q4 <- df %>% 
  filter(year == 2015, type == 'dx') %>%
  select(code, desc) %>%
  right_join(cdiff.2015.q4, by = c("code" = "i10_dx27")) %>%
  rename("i10_dx27" = "code", "dx27_desc" = "desc")

cdiff.2015.q4 <- df %>% 
  filter(year == 2015, type == 'dx') %>%
  select(code, desc) %>%
  right_join(cdiff.2015.q4, by = c("code" = "i10_dx28")) %>%
  rename("i10_dx28" = "code", "dx28_desc" = "desc")

cdiff.2015.q4 <- df %>% 
  filter(year == 2015, type == 'dx') %>%
  select(code, desc) %>%
  right_join(cdiff.2015.q4, by = c("code" = "i10_dx29")) %>%
  rename("i10_dx29" = "code", "dx29_desc" = "desc")

cdiff.2015.q4 <- df %>% 
  filter(year == 2015, type == 'dx') %>%
  select(code, desc) %>%
  right_join(cdiff.2015.q4, by = c("code" = "i10_dx30")) %>%
  rename("i10_dx30" = "code", "dx30_desc" = "desc")

#############################################################################################################
###     2015 Q4, 2015 Q1 - Q3    PR
#############################################################################################################

cdiff.2015.q4 <- df %>% 
  filter(year == 2015, type == 'pr') %>%
  select(code, desc) %>%
  right_join(cdiff.2015.q4, by = c("code" = "i10_pr1")) %>%
  rename("i10_pr1" = "code", "pr1_desc" = "desc")

cdiff.2015.q4 <- df %>% 
  filter(year == 2015, type == 'pr') %>%
  select(code, desc) %>%
  right_join(cdiff.2015.q4, by = c("code" = "i10_pr2")) %>%
  rename("i10_pr2" = "code", "pr2_desc" = "desc")

cdiff.2015.q4 <- df %>% 
  filter(year == 2015, type == 'pr') %>%
  select(code, desc) %>%
  right_join(cdiff.2015.q4, by = c("code" = "i10_pr3")) %>%
  rename("i10_pr3" = "code", "pr3_desc" = "desc")

cdiff.2015.q4 <- df %>% 
  filter(year == 2015, type == 'pr') %>%
  select(code, desc) %>%
  right_join(cdiff.2015.q4, by = c("code" = "i10_pr4")) %>%
  rename("i10_pr4" = "code", "pr4_desc" = "desc")

cdiff.2015.q4 <- df %>% 
  filter(year == 2015, type == 'pr') %>%
  select(code, desc) %>%
  right_join(cdiff.2015.q4, by = c("code" = "i10_pr5")) %>%
  rename("i10_pr5" = "code", "pr5_desc" = "desc")

cdiff.2015.q4 <- df %>% 
  filter(year == 2015, type == 'pr') %>%
  select(code, desc) %>%
  right_join(cdiff.2015.q4, by = c("code" = "i10_pr6")) %>%
  rename("i10_pr6" = "code", "pr6_desc" = "desc")

cdiff.2015.q4 <- df %>% 
  filter(year == 2015, type == 'pr') %>%
  select(code, desc) %>%
  right_join(cdiff.2015.q4, by = c("code" = "i10_pr7")) %>%
  rename("i10_pr7" = "code", "pr7_desc" = "desc")

cdiff.2015.q4 <- df %>% 
  filter(year == 2015, type == 'pr') %>%
  select(code, desc) %>%
  right_join(cdiff.2015.q4, by = c("code" = "i10_pr8")) %>%
  rename("i10_pr8" = "code", "pr8_desc" = "desc")

cdiff.2015.q4 <- df %>% 
  filter(year == 2015, type == 'pr') %>%
  select(code, desc) %>%
  right_join(cdiff.2015.q4, by = c("code" = "i10_pr9")) %>%
  rename("i10_pr9" = "code", "pr9_desc" = "desc")

cdiff.2015.q4 <- df %>% 
  filter(year == 2015, type == 'pr') %>%
  select(code, desc) %>%
  right_join(cdiff.2015.q4, by = c("code" = "i10_pr10")) %>%
  rename("i10_pr10" = "code", "pr10_desc" = "desc")

cdiff.2015.q4 <- df %>% 
  filter(year == 2015, type == 'pr') %>%
  select(code, desc) %>%
  right_join(cdiff.2015.q4, by = c("code" = "i10_pr11")) %>%
  rename("i10_pr11" = "code", "pr11_desc" = "desc")

cdiff.2015.q4 <- df %>% 
  filter(year == 2015, type == 'pr') %>%
  select(code, desc) %>%
  right_join(cdiff.2015.q4, by = c("code" = "i10_pr12")) %>%
  rename("i10_pr12" = "code", "pr12_desc" = "desc")

cdiff.2015.q4 <- df %>% 
  filter(year == 2015, type == 'pr') %>%
  select(code, desc) %>%
  right_join(cdiff.2015.q4, by = c("code" = "i10_pr13")) %>%
  rename("i10_pr13" = "code", "pr13_desc" = "desc")

cdiff.2015.q4 <- df %>% 
  filter(year == 2015, type == 'pr') %>%
  select(code, desc) %>%
  right_join(cdiff.2015.q4, by = c("code" = "i10_pr14")) %>%
  rename("i10_pr14" = "code", "pr14_desc" = "desc")

cdiff.2015.q4 <- df %>% 
  filter(year == 2015, type == 'pr') %>%
  select(code, desc) %>%
  right_join(cdiff.2015.q4, by = c("code" = "i10_pr15")) %>%
  rename("i10_pr15" = "code", "pr15_desc" = "desc")



cdiff.final <- bind_rows(cdiff.2010.q1q3,
                         cdiff.2010.q4.2011.q1q3,
                         cdiff.2011.q4.2012.q1q3,
                         cdiff.2012.q4.2013.q1q3,
                         cdiff.2013.q4.2014.q1q3,
                         cdiff.2014.q4.2015.q1q3,
                         cdiff.2015.q4)
 
write_csv(cdiff.final, "data/cdiff-nrd-desc.csv") 
  

#left_join(cdiff.nrd, df, by = NULL, copy = FALSE, suffix = c(".x", ".y"), ...)
