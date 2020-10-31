library('MonetDB.R')
#install.packages('MonetDBLite')
library('MonetDBLite')
library('dplyr')
library('tidyverse')
library('DBI')
library('beepr')
library('sqlsurvey')


setwd('/home/bdetweiler/src/Data_Science/stat-8960-capstone-project/')


cdiff <- read_csv('data/cdiff.csv', guess_max = 858204)

cdiff

cdiff.preg <- cdiff %>%
  mutate(pregnant=as.integer(grepl("V22", dx1)  |
                             grepl("V22", dx2)  |
                             grepl("V22", dx3)  |
                             grepl("V22", dx4)  |
                             grepl("V22", dx5)  |
                             grepl("V22", dx6)  |
                             grepl("V22", dx7)  |
                             grepl("V22", dx8)  |
                             grepl("V22", dx9)  |
                             grepl("V22", dx10) |
                             grepl("V22", dx11) |
                             grepl("V22", dx12) |
                             grepl("V22", dx13) |
                             grepl("V22", dx14) |
                             grepl("V22", dx15) |
                             grepl("V22", dx16) |
                             grepl("V22", dx17) |
                             grepl("V22", dx18) |
                             grepl("V22", dx19) |
                             grepl("V22", dx20) |
                             grepl("V22", dx21) |
                             grepl("V22", dx22) |
                             grepl("V22", dx23) |
                             grepl("V22", dx24) |
                             grepl("V22", dx25) |
                             grepl("V22", dx26) |
                             grepl("V22", dx27) |
                             grepl("V22", dx28) |
                             grepl("V22", dx29) |
                             grepl("V22", dx30)))

write_csv(cdiff.preg, "data/cdiff-pregnant.csv")

cdiff.preg

  #filter(grepl("V22", dx1) | grepl("V22", dx2)) %>%
                              #|
                           #dx3  == '00845' |
                           #dx4  == '00845' |
                           #dx5  == '00845' |
                           #dx6  == '00845' |
                           #dx7  == '00845' |
                           #dx8  == '00845' |
                           #dx9  == '00845' |
                           #dx10 == '00845' |
                           #dx11 == '00845' |
                           #dx12 == '00845' |
                           #dx13 == '00845' |
                           #dx14 == '00845' |
                           #dx15 == '00845' |
                           #dx16 == '00845' |
                           #dx17 == '00845' |
                           #dx18 == '00845' |
                           #dx19 == '00845' |
                           #dx20 == '00845' |
                           #dx21 == '00845' |
                           #dx22 == '00845' |
                           #dx23 == '00845' |
                           #dx24 == '00845' |
                           #dx25 == '00845' |
                           #dx26 == '00845' |
                           #dx27 == '00845' |
                           #dx28 == '00845' |
                           #dx29 == '00845' |
                           #dx30 == '00845'))) %>%
  #mutate(cdi=replace(cdi, is.na(cdi), 0))

                  #nis.DX3  = '00845' OR
                  #nis.DX4  = '00845' OR
                  #nis.DX5  = '00845' OR
                  #nis.DX6  = '00845' OR
                  #nis.DX7  = '00845' OR
                  #nis.DX8  = '00845' OR
                  #nis.DX9  = '00845' OR
                  #nis.DX10 = '00845' OR
                  #nis.DX11 = '00845' OR
                  #nis.DX12 = '00845' OR
                  #nis.DX13 = '00845' OR
                  #nis.DX14 = '00845' OR
                  #nis.DX15 = '00845' OR
                  #nis.DX16 = '00845' OR
                  #nis.DX17 = '00845' OR
                  #nis.DX18 = '00845' OR
                  #nis.DX19 = '00845' OR
                  #nis.DX20 = '00845' OR
                  #nis.DX21 = '00845' OR
                  #nis.DX22 = '00845' OR
                  #nis.DX23 = '00845' OR
                  #nis.DX24 = '00845' OR
                  #nis.DX25 = '00845') 

#MonetDBLite::monetdblite_shutdown()
#con <- DBI::dbConnect(MonetDBLite::MonetDBLite(), "data/nrd_db")
con <- DBI::dbConnect(MonetDBLite::MonetDBLite(), "data/nis_db")

row.count <- DBI::dbGetQuery(con, "SELECT COUNT(*) as count FROM nrd")
row.count
patient.counts <- list()
patient.counts[["total"]] <- DBI::dbGetQuery(con, "SELECT nis_year, COUNT(nis_key) AS n FROM NIS GROUP BY nis_year")

#584 	Acute kidney failure
#584.5 	Acute kidney failure with lesion of tubular necrosis convert
#584.6 	Acute kidney failure with lesion of renal cortical necrosis convert
#584.7 	Acute kidney failure with lesion of renal medullary [papillary] necrosis
#584.8 	Acute kidney failure with lesion of with other specified pathological lesion in kidney
#584.9 	Acute kidney failure, unspecified
#585 	Chronic kidney disease (ckd)
#585.1 	Chronic kidney disease, Stage I
#585.2 	Chronic kidney disease, Stage II (mild)
#585.3 	Chronic kidney disease, Stage III (moderate)
#585.4 	Chronic kidney disease, Stage IV (severe)
#585.5 	Chronic kidney disease, Stage V (mild)
#585.6 	End stage renal disease
#585.9 	Chronic kidney disease, unspecified
#586 	Renal failure, unspecified

# Acute Kidney Infection
aki.count.q <- "SELECT nis_year,
count(nis_key) as n
FROM nis
WHERE  nis.DX1  like '584%' OR
nis.DX2  like '584%' OR
nis.DX3  like '584%' OR
nis.DX4  like '584%' OR
nis.DX5  like '584%' OR
nis.DX6  like '584%' OR
nis.DX7  like '584%' OR
nis.DX8  like '584%' OR
nis.DX9  like '584%' OR
nis.DX10 like '584%' OR
nis.DX11 like '584%' OR
nis.DX12 like '584%' OR
nis.DX13 like '584%' OR
nis.DX14 like '584%' OR
nis.DX15 like '584%' OR
nis.DX16 like '584%' OR
nis.DX17 like '584%' OR
nis.DX18 like '584%' OR
nis.DX19 like '584%' OR
nis.DX20 like '584%' OR
nis.DX21 like '584%' OR
nis.DX22 like '584%' OR
nis.DX23 like '584%' OR
nis.DX24 like '584%' OR
nis.DX25 like '584%' OR
nis.DX26 like '584%' OR
nis.DX27 like '584%' OR
nis.DX28 like '584%' OR
nis.DX29 like '584%' OR
nis.DX30 like '584%'
GROUP BY nis_year"
# Track time for query
sw.start <- Sys.time()
patient.counts[["aki"]] <- DBI::dbGetQuery(con, aki.count.q)
sw.end <- Sys.time()
print(sw.end - sw.start)
beep(3)

# WOW! AKIs have been linearly increasing
patient.counts[["aki"]] %>%
  ggplot(aes(nis_year, n)) +
  geom_histogram(stat="identity") +
  geom_smooth()



# Chronic Kidney Disease
# Note: I'm grouping 585 with 585.9, which is Chronic kidney disease, Unspecified
ckd.count.q <- "SELECT nis_year,
count(nis_key) as n
FROM nis
WHERE  nis.DX1  = '585'  OR
nis.DX1  = '5859' OR
nis.DX2  = '585'  OR
nis.DX2  = '5859' OR
nis.DX3  = '585'  OR
nis.DX3  = '5859' OR
nis.DX4  = '585'  OR
nis.DX4  = '5859' OR
nis.DX5  = '585'  OR
nis.DX5  = '5859' OR
nis.DX6  = '585'  OR
nis.DX6  = '5859' OR
nis.DX7  = '585'  OR
nis.DX7  = '5859' OR
nis.DX8  = '585'  OR
nis.DX8  = '5859' OR
nis.DX9  = '585'  OR
nis.DX9  = '5859' OR
nis.DX10 = '585'  OR
nis.DX10 = '5859' OR
nis.DX11 = '585'  OR
nis.DX11 = '5859' OR
nis.DX12 = '585'  OR
nis.DX12 = '5859' OR
nis.DX13 = '585'  OR
nis.DX13 = '5859' OR
nis.DX14 = '585'  OR
nis.DX14 = '5859' OR
nis.DX15 = '585'  OR
nis.DX15 = '5859' OR
nis.DX16 = '585'  OR
nis.DX16 = '5859' OR
nis.DX17 = '585'  OR
nis.DX17 = '5859' OR
nis.DX18 = '585'  OR
nis.DX18 = '5859' OR
nis.DX19 = '585'  OR
nis.DX19 = '5859' OR
nis.DX20 = '585'  OR
nis.DX20 = '5859' OR
nis.DX21 = '585'  OR
nis.DX21 = '5859' OR
nis.DX22 = '585'  OR
nis.DX22 = '5859' OR
nis.DX23 = '585'  OR
nis.DX23 = '5859' OR
nis.DX24 = '585'  OR
nis.DX24 = '5859' OR
nis.DX25 = '585'  OR
nis.DX25 = '5859' OR
nis.DX26 = '585'  OR
nis.DX26 = '5859' OR
nis.DX27 = '585'  OR
nis.DX27 = '5859' OR
nis.DX28 = '585'  OR
nis.DX28 = '5859' OR
nis.DX29 = '585'  OR
nis.DX29 = '5859' OR
nis.DX30 = '585'  OR
nis.DX30 = '5859' 
GROUP BY nis_year"
# Track time for query
sw.start <- Sys.time()
patient.counts[["ckd"]] <- DBI::dbGetQuery(con, ckd.count.q)
sw.end <- Sys.time()
print(sw.end - sw.start)
beep(3)
patient.counts[["ckd"]]


patient.counts[["ckd"]] %>%
  ggplot(aes(nis_year, n)) +
  geom_histogram(stat="identity") +
  geom_smooth()


# Renal failure, Stage 1
ckd1.count.q <- "SELECT nis_year, 
COUNT(*) as n
FROM nis
WHERE  nis.DX1  = '5851' OR 
nis.DX2  = '5851' OR
nis.DX3  = '5851' OR
nis.DX4  = '5851' OR
nis.DX5  = '5851' OR
nis.DX6  = '5851' OR
nis.DX7  = '5851' OR
nis.DX8  = '5851' OR
nis.DX9  = '5851' OR
nis.DX10 = '5851' OR
nis.DX11 = '5851' OR
nis.DX12 = '5851' OR
nis.DX13 = '5851' OR
nis.DX14 = '5851' OR
nis.DX15 = '5851' OR
nis.DX16 = '5851' OR
nis.DX17 = '5851' OR
nis.DX18 = '5851' OR
nis.DX19 = '5851' OR
nis.DX20 = '5851' OR
nis.DX21 = '5851' OR
nis.DX22 = '5851' OR
nis.DX23 = '5851' OR
nis.DX24 = '5851' OR
nis.DX25 = '5851' OR
nis.DX26 = '5851' OR
nis.DX27 = '5851' OR
nis.DX28 = '5851' OR
nis.DX29 = '5851' OR
nis.DX30 = '5851'
GROUP BY nis_year"


# Track time for query
sw.start <- Sys.time()
patient.counts[["ckd1"]] <- DBI::dbGetQuery(con, ckd1.count.q)
sw.end <- Sys.time()
print(sw.end - sw.start)
beep(3)
patient.counts[["ckd1"]]

patient.counts[["ckd1"]] %>%
  ggplot(aes(nis_year, n)) +
  geom_histogram(stat="identity") +
  geom_smooth()


# Renal failure, Stage 2
ckd2.count.q <- "SELECT nis_year, 
COUNT(*) as n
FROM nis
WHERE  nis.DX1  = '5852' OR 
nis.DX2  = '5852' OR
nis.DX3  = '5852' OR
nis.DX4  = '5852' OR
nis.DX5  = '5852' OR
nis.DX6  = '5852' OR
nis.DX7  = '5852' OR
nis.DX8  = '5852' OR
nis.DX9  = '5852' OR
nis.DX10 = '5852' OR
nis.DX11 = '5852' OR
nis.DX12 = '5852' OR
nis.DX13 = '5852' OR
nis.DX14 = '5852' OR
nis.DX15 = '5852' OR
nis.DX16 = '5852' OR
nis.DX17 = '5852' OR
nis.DX18 = '5852' OR
nis.DX19 = '5852' OR
nis.DX20 = '5852' OR
nis.DX21 = '5852' OR
nis.DX22 = '5852' OR
nis.DX23 = '5852' OR
nis.DX24 = '5852' OR
nis.DX25 = '5852' OR
nis.DX26 = '5852' OR
nis.DX27 = '5852' OR
nis.DX28 = '5852' OR
nis.DX29 = '5852' OR
nis.DX30 = '5852'
GROUP BY nis_year"


# Track time for query
sw.start <- Sys.time()
patient.counts[["ckd2"]] <- DBI::dbGetQuery(con, ckd2.count.q)
sw.end <- Sys.time()
print(sw.end - sw.start)
beep(3)
patient.counts[["ckd2"]]

patient.counts[["ckd2"]] %>%
  ggplot(aes(nis_year, n)) +
  geom_histogram(stat="identity") +
  geom_smooth()


# Renal failure, Stage 3
ckd3.count.q <- "SELECT nis_year, 
COUNT(*) as n
FROM nis
WHERE  nis.DX1  = '5853' OR 
nis.DX2  = '5853' OR
nis.DX3  = '5853' OR
nis.DX4  = '5853' OR
nis.DX5  = '5853' OR
nis.DX6  = '5853' OR
nis.DX7  = '5853' OR
nis.DX8  = '5853' OR
nis.DX9  = '5853' OR
nis.DX10 = '5853' OR
nis.DX11 = '5853' OR
nis.DX12 = '5853' OR
nis.DX13 = '5853' OR
nis.DX14 = '5853' OR
nis.DX15 = '5853' OR
nis.DX16 = '5853' OR
nis.DX17 = '5853' OR
nis.DX18 = '5853' OR
nis.DX19 = '5853' OR
nis.DX20 = '5853' OR
nis.DX21 = '5853' OR
nis.DX22 = '5853' OR
nis.DX23 = '5853' OR
nis.DX24 = '5853' OR
nis.DX25 = '5853' OR
nis.DX26 = '5853' OR
nis.DX27 = '5853' OR
nis.DX28 = '5853' OR
nis.DX29 = '5853' OR
nis.DX30 = '5853'
GROUP BY nis_year"


# Track time for query
sw.start <- Sys.time()
patient.counts[["ckd3"]] <- DBI::dbGetQuery(con, ckd3.count.q)
sw.end <- Sys.time()
print(sw.end - sw.start)
beep(3)
patient.counts[["ckd3"]]

patient.counts[["ckd3"]] %>%
  ggplot(aes(nis_year, n)) +
  geom_histogram(stat="identity") +
  geom_smooth()


# Renal failure, Stage 4
ckd4.count.q <- "SELECT nis_year, 
COUNT(*) as n
FROM nis
WHERE  nis.DX1  = '5854' OR 
nis.DX2  = '5854' OR
nis.DX3  = '5854' OR
nis.DX4  = '5854' OR
nis.DX5  = '5854' OR
nis.DX6  = '5854' OR
nis.DX7  = '5854' OR
nis.DX8  = '5854' OR
nis.DX9  = '5854' OR
nis.DX10 = '5854' OR
nis.DX11 = '5854' OR
nis.DX12 = '5854' OR
nis.DX13 = '5854' OR
nis.DX14 = '5854' OR
nis.DX15 = '5854' OR
nis.DX16 = '5854' OR
nis.DX17 = '5854' OR
nis.DX18 = '5854' OR
nis.DX19 = '5854' OR
nis.DX20 = '5854' OR
nis.DX21 = '5854' OR
nis.DX22 = '5854' OR
nis.DX23 = '5854' OR
nis.DX24 = '5854' OR
nis.DX25 = '5854' OR
nis.DX26 = '5854' OR
nis.DX27 = '5854' OR
nis.DX28 = '5854' OR
nis.DX29 = '5854' OR
nis.DX30 = '5854'
GROUP BY nis_year"


# Track time for query
sw.start <- Sys.time()
patient.counts[["ckd4"]] <- DBI::dbGetQuery(con, ckd4.count.q)
sw.end <- Sys.time()
print(sw.end - sw.start)
beep(3)
patient.counts[["ckd4"]]

patient.counts[["ckd4"]] %>%
  ggplot(aes(nis_year, n)) +
  geom_histogram(stat="identity") +
  geom_smooth()


# Renal failure, Stage 5
ckd5.count.q <- "SELECT nis_year, 
COUNT(*) as n
FROM nis
WHERE  nis.DX1  = '5855' OR 
nis.DX2  = '5855' OR
nis.DX3  = '5855' OR
nis.DX4  = '5855' OR
nis.DX5  = '5855' OR
nis.DX6  = '5855' OR
nis.DX7  = '5855' OR
nis.DX8  = '5855' OR
nis.DX9  = '5855' OR
nis.DX10 = '5855' OR
nis.DX11 = '5855' OR
nis.DX12 = '5855' OR
nis.DX13 = '5855' OR
nis.DX14 = '5855' OR
nis.DX15 = '5855' OR
nis.DX16 = '5855' OR
nis.DX17 = '5855' OR
nis.DX18 = '5855' OR
nis.DX19 = '5855' OR
nis.DX20 = '5855' OR
nis.DX21 = '5855' OR
nis.DX22 = '5855' OR
nis.DX23 = '5855' OR
nis.DX24 = '5855' OR
nis.DX25 = '5855' OR
nis.DX26 = '5855' OR
nis.DX27 = '5855' OR
nis.DX28 = '5855' OR
nis.DX29 = '5855' OR
nis.DX30 = '5855'
GROUP BY nis_year"


# Track time for query
sw.start <- Sys.time()
patient.counts[["ckd5"]] <- DBI::dbGetQuery(con, ckd5.count.q)
sw.end <- Sys.time()
print(sw.end - sw.start)
beep(3)
patient.counts[["ckd5"]]

patient.counts[["ckd5"]] %>%
  ggplot(aes(nis_year, n)) +
  geom_histogram(stat="identity") +
  geom_smooth()


# Renal failure, End Stage (Dialysis)
ckd6.count.q <- "SELECT nis_year, 
COUNT(*) as n
FROM nis
WHERE  nis.DX1  = '5856' OR 
nis.DX2  = '5856' OR
nis.DX3  = '5856' OR
nis.DX4  = '5856' OR
nis.DX5  = '5856' OR
nis.DX6  = '5856' OR
nis.DX7  = '5856' OR
nis.DX8  = '5856' OR
nis.DX9  = '5856' OR
nis.DX10 = '5856' OR
nis.DX11 = '5856' OR
nis.DX12 = '5856' OR
nis.DX13 = '5856' OR
nis.DX14 = '5856' OR
nis.DX15 = '5856' OR
nis.DX16 = '5856' OR
nis.DX17 = '5856' OR
nis.DX18 = '5856' OR
nis.DX19 = '5856' OR
nis.DX20 = '5856' OR
nis.DX21 = '5856' OR
nis.DX22 = '5856' OR
nis.DX23 = '5856' OR
nis.DX24 = '5856' OR
nis.DX25 = '5856' OR
nis.DX26 = '5856' OR
nis.DX27 = '5856' OR
nis.DX28 = '5856' OR
nis.DX29 = '5856' OR
nis.DX30 = '5856'
GROUP BY nis_year"


# Track time for query
sw.start <- Sys.time()
patient.counts[["ckd6"]] <- DBI::dbGetQuery(con, ckd6.count.q)
sw.end <- Sys.time()
print(sw.end - sw.start)
beep(3)
patient.counts[["ckd6"]]

patient.counts[["ckd6"]] %>%
  ggplot(aes(nis_year, n)) +
  geom_histogram(stat="identity") +
  geom_smooth()




# Renal failure, unspecified
renal_unspecified.count.q <- 
  "SELECT nis_year, 
COUNT(*) as n
FROM nis
WHERE  nis.DX1  = '586' OR 
nis.DX2  = '586' OR
nis.DX3  = '586' OR
nis.DX4  = '586' OR
nis.DX5  = '586' OR
nis.DX6  = '586' OR
nis.DX7  = '586' OR
nis.DX8  = '586' OR
nis.DX9  = '586' OR
nis.DX10 = '586' OR
nis.DX11 = '586' OR
nis.DX12 = '586' OR
nis.DX13 = '586' OR
nis.DX14 = '586' OR
nis.DX15 = '586' OR
nis.DX16 = '586' OR
nis.DX17 = '586' OR
nis.DX18 = '586' OR
nis.DX19 = '586' OR
nis.DX20 = '586' OR
nis.DX21 = '586' OR
nis.DX22 = '586' OR
nis.DX23 = '586' OR
nis.DX24 = '586' OR
nis.DX25 = '586' OR
nis.DX26 = '586' OR
nis.DX27 = '586' OR
nis.DX28 = '586' OR
nis.DX29 = '586' OR
nis.DX30 = '586'
GROUP BY nis_year"

# Track time for query
sw.start <- Sys.time()
patient.counts[["renal_unspecified"]] <- DBI::dbGetQuery(con, renal_unspecified.count.q)
sw.end <- Sys.time()
print(sw.end - sw.start)
beep(3)


# C. Diff by itself
cdi.count.q <- "SELECT nis_year,
count(nis_key) as n
FROM nis
WHERE  (nis.DX1  = '00845' OR
nis.DX2  = '00845' OR
nis.DX3  = '00845' OR
nis.DX4  = '00845' OR
nis.DX5  = '00845' OR
nis.DX6  = '00845' OR
nis.DX7  = '00845' OR
nis.DX8  = '00845' OR
nis.DX9  = '00845' OR
nis.DX10 = '00845' OR
nis.DX11 = '00845' OR
nis.DX12 = '00845' OR
nis.DX13 = '00845' OR
nis.DX14 = '00845' OR
nis.DX15 = '00845' OR
nis.DX16 = '00845' OR
nis.DX17 = '00845' OR
nis.DX18 = '00845' OR
nis.DX19 = '00845' OR
nis.DX20 = '00845' OR
nis.DX21 = '00845' OR
nis.DX22 = '00845' OR
nis.DX23 = '00845' OR
nis.DX24 = '00845' OR
nis.DX25 = '00845' OR
nis.DX26 = '00845' OR
nis.DX27 = '00845' OR
nis.DX28 = '00845' OR
nis.DX29 = '00845' OR
nis.DX30 = '00845'
)  
GROUP BY nis_year"
# Track time for query
sw.start <- Sys.time()
patient.counts[["cdi"]] <- DBI::dbGetQuery(con, cdi.count.q)
sw.end <- Sys.time()
print(sw.end - sw.start)
beep(3)
patient.counts[["cdi"]]


patient.counts[["cdi"]] %>%
  ggplot(aes(nis_year, n)) +
  geom_histogram(stat="identity") +
  geom_smooth()




# C. diff with Renal Failure (any kind)
cdi_with_renal.count.q <- 
  "SELECT nis_year,
count(nis_key) as n
FROM nis
WHERE  (nis.DX1  = '00845' OR
nis.DX2  = '00845' OR
nis.DX3  = '00845' OR
nis.DX4  = '00845' OR
nis.DX5  = '00845' OR
nis.DX6  = '00845' OR
nis.DX7  = '00845' OR
nis.DX8  = '00845' OR
nis.DX9  = '00845' OR
nis.DX10 = '00845' OR
nis.DX11 = '00845' OR
nis.DX12 = '00845' OR
nis.DX13 = '00845' OR
nis.DX14 = '00845' OR
nis.DX15 = '00845' OR
nis.DX16 = '00845' OR
nis.DX17 = '00845' OR
nis.DX18 = '00845' OR
nis.DX19 = '00845' OR
nis.DX20 = '00845' OR
nis.DX21 = '00845' OR
nis.DX22 = '00845' OR
nis.DX23 = '00845' OR
nis.DX24 = '00845' OR
nis.DX25 = '00845' OR
nis.DX26 = '00845' OR
nis.DX27 = '00845' OR
nis.DX28 = '00845' OR
nis.DX29 = '00845' OR
nis.DX30 = '00845'
)  
AND (
(nis.DX1  like '584%' OR
nis.DX2  like '584%' OR
nis.DX3  like '584%' OR
nis.DX4  like '584%' OR
nis.DX5  like '584%' OR
nis.DX6  like '584%' OR
nis.DX7  like '584%' OR
nis.DX8  like '584%' OR
nis.DX9  like '584%' OR
nis.DX10 like '584%' OR
nis.DX11 like '584%' OR
nis.DX12 like '584%' OR
nis.DX13 like '584%' OR
nis.DX14 like '584%' OR
nis.DX15 like '584%' OR
nis.DX16 like '584%' OR
nis.DX17 like '584%' OR
nis.DX18 like '584%' OR
nis.DX19 like '584%' OR
nis.DX20 like '584%' OR
nis.DX21 like '584%' OR
nis.DX22 like '584%' OR
nis.DX23 like '584%' OR
nis.DX24 like '584%' OR
nis.DX25 like '584%' OR
nis.DX26 like '584%' OR
nis.DX27 like '584%' OR
nis.DX28 like '584%' OR
nis.DX29 like '584%' OR
nis.DX30 like '584%'
)
OR (nis.DX1  like '585%'  OR
nis.DX2  like '585%'  OR
nis.DX3  like '585%'  OR
nis.DX4  like '585%'  OR
nis.DX5  like '585%'  OR
nis.DX6  like '585%'  OR
nis.DX7  like '585%'  OR
nis.DX8  like '585%'  OR
nis.DX9  like '585%'  OR
nis.DX10 like '585%'  OR
nis.DX11 like '585%'  OR
nis.DX12 like '585%'  OR
nis.DX13 like '585%'  OR
nis.DX14 like '585%'  OR
nis.DX15 like '585%'  OR
nis.DX16 like '585%'  OR
nis.DX17 like '585%'  OR
nis.DX18 like '585%'  OR
nis.DX19 like '585%'  OR
nis.DX20 like '585%'  OR
nis.DX21 like '585%'  OR
nis.DX22 like '585%'  OR
nis.DX23 like '585%'  OR
nis.DX24 like '585%'  OR
nis.DX25 like '585%'  OR
nis.DX26 like '585%'  OR
nis.DX27 like '585%'  OR
nis.DX28 like '585%'  OR
nis.DX29 like '585%'  OR
nis.DX30 like '585%'
)
OR (nis.DX1  = '586'  OR
nis.DX2  = '586'  OR
nis.DX3  = '586'  OR
nis.DX4  = '586'  OR
nis.DX5  = '586'  OR
nis.DX6  = '586'  OR
nis.DX7  = '586'  OR
nis.DX8  = '586'  OR
nis.DX9  = '586'  OR
nis.DX10 = '586'  OR
nis.DX11 = '586'  OR
nis.DX12 = '586'  OR
nis.DX13 = '586'  OR
nis.DX14 = '586'  OR
nis.DX15 = '586'  OR
nis.DX16 = '586'  OR
nis.DX17 = '586'  OR
nis.DX18 = '586'  OR
nis.DX19 = '586'  OR
nis.DX20 = '586'  OR
nis.DX21 = '586'  OR
nis.DX22 = '586'  OR
nis.DX23 = '586'  OR
nis.DX24 = '586'  OR
nis.DX25 = '586'  OR
nis.DX26 = '586'  OR
nis.DX27 = '586'  OR
nis.DX28 = '586'  OR
nis.DX29 = '586'  OR
nis.DX30 = '586'
)
)
GROUP BY nis_year"
# Track time for query
sw.start <- Sys.time()
patient.counts[["cdi_with_renal"]] <- DBI::dbGetQuery(con, cdi_with_renal.count.q)
sw.end <- Sys.time()
print(sw.end - sw.start)
beep(3)
patient.counts[["cdi_with_renal"]]


patient.counts[["cdi_with_renal"]] %>%
  ggplot(aes(nis_year, n)) +
  geom_histogram(stat="identity") +
  geom_smooth()


# C. Diff by itself
cdi.count.q <- "SELECT nis_year,
count(nis_key) as n
FROM nis
WHERE  (nis.DX1  = '00845' OR
nis.DX2  = '00845' OR
nis.DX3  = '00845' OR
nis.DX4  = '00845' OR
nis.DX5  = '00845' OR
nis.DX6  = '00845' OR
nis.DX7  = '00845' OR
nis.DX8  = '00845' OR
nis.DX9  = '00845' OR
nis.DX10 = '00845' OR
nis.DX11 = '00845' OR
nis.DX12 = '00845' OR
nis.DX13 = '00845' OR
nis.DX14 = '00845' OR
nis.DX15 = '00845' OR
nis.DX16 = '00845' OR
nis.DX17 = '00845' OR
nis.DX18 = '00845' OR
nis.DX19 = '00845' OR
nis.DX20 = '00845' OR
nis.DX21 = '00845' OR
nis.DX22 = '00845' OR
nis.DX23 = '00845' OR
nis.DX24 = '00845' OR
nis.DX25 = '00845' OR
nis.DX26 = '00845' OR
nis.DX27 = '00845' OR
nis.DX28 = '00845' OR
nis.DX29 = '00845' OR
nis.DX30 = '00845'
)  
GROUP BY nis_year"
# Track time for query
sw.start <- Sys.time()
patient.counts[["cdi"]] <- DBI::dbGetQuery(con, cdi.count.q)
sw.end <- Sys.time()
print(sw.end - sw.start)
beep(3)
patient.counts[["cdi"]]


patient.counts[["cdi"]] %>%
  ggplot(aes(nis_year, n)) +
  geom_histogram(stat="identity") +
  geom_smooth()




# C. diff with Renal Failure (any kind)
cdi_with_renal.count.q <- 
  "SELECT nis_year,
count(nis_key) as n
FROM nis
WHERE  (nis.DX1  = '00845' OR
nis.DX2  = '00845' OR
nis.DX3  = '00845' OR
nis.DX4  = '00845' OR
nis.DX5  = '00845' OR
nis.DX6  = '00845' OR
nis.DX7  = '00845' OR
nis.DX8  = '00845' OR
nis.DX9  = '00845' OR
nis.DX10 = '00845' OR
nis.DX11 = '00845' OR
nis.DX12 = '00845' OR
nis.DX13 = '00845' OR
nis.DX14 = '00845' OR
nis.DX15 = '00845' OR
nis.DX16 = '00845' OR
nis.DX17 = '00845' OR
nis.DX18 = '00845' OR
nis.DX19 = '00845' OR
nis.DX20 = '00845' OR
nis.DX21 = '00845' OR
nis.DX22 = '00845' OR
nis.DX23 = '00845' OR
nis.DX24 = '00845' OR
nis.DX25 = '00845' OR
nis.DX26 = '00845' OR
nis.DX27 = '00845' OR
nis.DX28 = '00845' OR
nis.DX29 = '00845' OR
nis.DX30 = '00845'
)  
AND (
(nis.DX1  like '584%' OR
nis.DX2  like '584%' OR
nis.DX3  like '584%' OR
nis.DX4  like '584%' OR
nis.DX5  like '584%' OR
nis.DX6  like '584%' OR
nis.DX7  like '584%' OR
nis.DX8  like '584%' OR
nis.DX9  like '584%' OR
nis.DX10 like '584%' OR
nis.DX11 like '584%' OR
nis.DX12 like '584%' OR
nis.DX13 like '584%' OR
nis.DX14 like '584%' OR
nis.DX15 like '584%' OR
nis.DX16 like '584%' OR
nis.DX17 like '584%' OR
nis.DX18 like '584%' OR
nis.DX19 like '584%' OR
nis.DX20 like '584%' OR
nis.DX21 like '584%' OR
nis.DX22 like '584%' OR
nis.DX23 like '584%' OR
nis.DX24 like '584%' OR
nis.DX25 like '584%' OR
nis.DX26 like '584%' OR
nis.DX27 like '584%' OR
nis.DX28 like '584%' OR
nis.DX29 like '584%' OR
nis.DX30 like '584%'
)
OR (nis.DX1  like '585%'  OR
nis.DX2  like '585%'  OR
nis.DX3  like '585%'  OR
nis.DX4  like '585%'  OR
nis.DX5  like '585%'  OR
nis.DX6  like '585%'  OR
nis.DX7  like '585%'  OR
nis.DX8  like '585%'  OR
nis.DX9  like '585%'  OR
nis.DX10 like '585%'  OR
nis.DX11 like '585%'  OR
nis.DX12 like '585%'  OR
nis.DX13 like '585%'  OR
nis.DX14 like '585%'  OR
nis.DX15 like '585%'  OR
nis.DX16 like '585%'  OR
nis.DX17 like '585%'  OR
nis.DX18 like '585%'  OR
nis.DX19 like '585%'  OR
nis.DX20 like '585%'  OR
nis.DX21 like '585%'  OR
nis.DX22 like '585%'  OR
nis.DX23 like '585%'  OR
nis.DX24 like '585%'  OR
nis.DX25 like '585%'  OR
nis.DX26 like '585%'  OR
nis.DX27 like '585%'  OR
nis.DX28 like '585%'  OR
nis.DX29 like '585%'  OR
nis.DX30 like '585%'
)
OR (nis.DX1  = '586'  OR
nis.DX2  = '586'  OR
nis.DX3  = '586'  OR
nis.DX4  = '586'  OR
nis.DX5  = '586'  OR
nis.DX6  = '586'  OR
nis.DX7  = '586'  OR
nis.DX8  = '586'  OR
nis.DX9  = '586'  OR
nis.DX10 = '586'  OR
nis.DX11 = '586'  OR
nis.DX12 = '586'  OR
nis.DX13 = '586'  OR
nis.DX14 = '586'  OR
nis.DX15 = '586'  OR
nis.DX16 = '586'  OR
nis.DX17 = '586'  OR
nis.DX18 = '586'  OR
nis.DX19 = '586'  OR
nis.DX20 = '586'  OR
nis.DX21 = '586'  OR
nis.DX22 = '586'  OR
nis.DX23 = '586'  OR
nis.DX24 = '586'  OR
nis.DX25 = '586'  OR
nis.DX26 = '586'  OR
nis.DX27 = '586'  OR
nis.DX28 = '586'  OR
nis.DX29 = '586'  OR
nis.DX30 = '586'
)
)
GROUP BY nis_year"
# Track time for query
sw.start <- Sys.time()
patient.counts[["cdi_with_renal"]] <- DBI::dbGetQuery(con, cdi_with_renal.count.q)
sw.end <- Sys.time()
print(sw.end - sw.start)
beep(3)
patient.counts[["cdi_with_renal"]]


patient.counts[["cdi_with_renal"]] %>%
  ggplot(aes(nis_year, n)) +
  geom_histogram(stat="identity") +
  geom_smooth()






# Join all of the stats into a table and write it out


df <- patient.counts[["total"]] %>%
  left_join(patient.counts[["aki"]], by="nis_year" ) %>% 
  rename(total = n.x, aki = n.y) %>%
  left_join(patient.counts[["ckd"]], by="nis_year") %>%
  rename(ckd = n) %>%
  left_join(patient.counts[["ckd1"]], by="nis_year") %>%
  rename(ckd1 = n) %>%
  left_join(patient.counts[["ckd2"]], by="nis_year") %>%
  rename(ckd2 = n) %>%
  left_join(patient.counts[["ckd3"]], by="nis_year") %>%
  rename(ckd3 = n) %>%
  left_join(patient.counts[["ckd4"]], by="nis_year") %>%
  rename(ckd4 = n) %>%
  left_join(patient.counts[["ckd5"]], by="nis_year") %>%
  rename(ckd5 = n) %>%
  left_join(patient.counts[["ckd6"]], by="nis_year") %>%
  rename(ckd6 = n) %>%
  left_join(patient.counts[["renal_unspecified"]], by="nis_year") %>%
  rename(renal_unspecified = n) %>%
  left_join(patient.counts[["cdi"]], by="nis_year") %>%
  rename(cdi = n) %>%
  left_join(patient.counts[["cdi_with_renal"]], by="nis_year") %>%
  rename(cdi_with_renal = n)

write_csv(df, 'data/cdi_renal_counts.csv')



# Get everything where patients had cdi.and.renal Failure. Need this to do survey calculations.
cdi.and.renal.all.q <- 
  "SELECT *
FROM nis
WHERE  (nis.DX1  = '00845' OR
nis.DX2  = '00845' OR
nis.DX3  = '00845' OR
nis.DX4  = '00845' OR
nis.DX5  = '00845' OR
nis.DX6  = '00845' OR
nis.DX7  = '00845' OR
nis.DX8  = '00845' OR
nis.DX9  = '00845' OR
nis.DX10 = '00845' OR
nis.DX11 = '00845' OR
nis.DX12 = '00845' OR
nis.DX13 = '00845' OR
nis.DX14 = '00845' OR
nis.DX15 = '00845' OR
nis.DX16 = '00845' OR
nis.DX17 = '00845' OR
nis.DX18 = '00845' OR
nis.DX19 = '00845' OR
nis.DX20 = '00845' OR
nis.DX21 = '00845' OR
nis.DX22 = '00845' OR
nis.DX23 = '00845' OR
nis.DX24 = '00845' OR
nis.DX25 = '00845' OR
nis.DX26 = '00845' OR
nis.DX27 = '00845' OR
nis.DX28 = '00845' OR
nis.DX29 = '00845' OR
nis.DX30 = '00845'
)  
OR (
(nis.DX1  like '584%' OR
nis.DX2  like '584%' OR
nis.DX3  like '584%' OR
nis.DX4  like '584%' OR
nis.DX5  like '584%' OR
nis.DX6  like '584%' OR
nis.DX7  like '584%' OR
nis.DX8  like '584%' OR
nis.DX9  like '584%' OR
nis.DX10 like '584%' OR
nis.DX11 like '584%' OR
nis.DX12 like '584%' OR
nis.DX13 like '584%' OR
nis.DX14 like '584%' OR
nis.DX15 like '584%' OR
nis.DX16 like '584%' OR
nis.DX17 like '584%' OR
nis.DX18 like '584%' OR
nis.DX19 like '584%' OR
nis.DX20 like '584%' OR
nis.DX21 like '584%' OR
nis.DX22 like '584%' OR
nis.DX23 like '584%' OR
nis.DX24 like '584%' OR
nis.DX25 like '584%' OR
nis.DX26 like '584%' OR
nis.DX27 like '584%' OR
nis.DX28 like '584%' OR
nis.DX29 like '584%' OR
nis.DX30 like '584%'
)
OR (nis.DX1  like '585%'  OR
nis.DX2  like '585%'  OR
nis.DX3  like '585%'  OR
nis.DX4  like '585%'  OR
nis.DX5  like '585%'  OR
nis.DX6  like '585%'  OR
nis.DX7  like '585%'  OR
nis.DX8  like '585%'  OR
nis.DX9  like '585%'  OR
nis.DX10 like '585%'  OR
nis.DX11 like '585%'  OR
nis.DX12 like '585%'  OR
nis.DX13 like '585%'  OR
nis.DX14 like '585%'  OR
nis.DX15 like '585%'  OR
nis.DX16 like '585%'  OR
nis.DX17 like '585%'  OR
nis.DX18 like '585%'  OR
nis.DX19 like '585%'  OR
nis.DX20 like '585%'  OR
nis.DX21 like '585%'  OR
nis.DX22 like '585%'  OR
nis.DX23 like '585%'  OR
nis.DX24 like '585%'  OR
nis.DX25 like '585%'  OR
nis.DX26 like '585%'  OR
nis.DX27 like '585%'  OR
nis.DX28 like '585%'  OR
nis.DX29 like '585%'  OR
nis.DX30 like '585%'
)
OR (nis.DX1  = '586'  OR
nis.DX2  = '586'  OR
nis.DX3  = '586'  OR
nis.DX4  = '586'  OR
nis.DX5  = '586'  OR
nis.DX6  = '586'  OR
nis.DX7  = '586'  OR
nis.DX8  = '586'  OR
nis.DX9  = '586'  OR
nis.DX10 = '586'  OR
nis.DX11 = '586'  OR
nis.DX12 = '586'  OR
nis.DX13 = '586'  OR
nis.DX14 = '586'  OR
nis.DX15 = '586'  OR
nis.DX16 = '586'  OR
nis.DX17 = '586'  OR
nis.DX18 = '586'  OR
nis.DX19 = '586'  OR
nis.DX20 = '586'  OR
nis.DX21 = '586'  OR
nis.DX22 = '586'  OR
nis.DX23 = '586'  OR
nis.DX24 = '586'  OR
nis.DX25 = '586'  OR
nis.DX26 = '586'  OR
nis.DX27 = '586'  OR
nis.DX28 = '586'  OR
nis.DX29 = '586'  OR
nis.DX30 = '586'
)
)"
# Track time for query
sw.start <- Sys.time()
cdi.and.renal <- DBI::dbGetQuery(con, cdi_or_renal.all.q)
head(cdi.and.renal)
dim(cdi.and.renal)
sw.end <- Sys.time()
print(sw.end - sw.start)
beep(3)

# Encode dummy variables so we can quickly see what the patient had
# 00845   C. diff
# 584.5 	Acute kidney failure with lesion of tubular necrosis convert
# 584.6 	Acute kidney failure with lesion of renal cortical necrosis convert
# 584.7 	Acute kidney failure with lesion of renal medullary [papillary] necrosis
# 584.8 	Acute kidney failure with lesion of with other specified pathological lesion in kidney
# 585 	Chronic kidney disease (ckd)
# 585.1 	Chronic kidney disease, Stage I
# 585.2 	Chronic kidney disease, Stage II (mild)
# 585.3 	Chronic kidney disease, Stage III (moderate)
# 585.4 	Chronic kidney disease, Stage IV (severe)
# 585.5 	Chronic kidney disease, Stage V (mild)
# 585.6 	End stage renal disease
# 585.9 	Chronic kidney disease, unspecified
# 586 	Renal failure, unspecified
cdi.and.renal <-
  cdi.and.renal %>% 
  mutate(cdi=as.integer((dx1  == '00845' |
                           dx2  == '00845' |
                           dx3  == '00845' |
                           dx4  == '00845' |
                           dx5  == '00845' |
                           dx6  == '00845' |
                           dx7  == '00845' |
                           dx8  == '00845' |
                           dx9  == '00845' |
                           dx10 == '00845' |
                           dx11 == '00845' |
                           dx12 == '00845' |
                           dx13 == '00845' |
                           dx14 == '00845' |
                           dx15 == '00845' |
                           dx16 == '00845' |
                           dx17 == '00845' |
                           dx18 == '00845' |
                           dx19 == '00845' |
                           dx20 == '00845' |
                           dx21 == '00845' |
                           dx22 == '00845' |
                           dx23 == '00845' |
                           dx24 == '00845' |
                           dx25 == '00845' |
                           dx26 == '00845' |
                           dx27 == '00845' |
                           dx28 == '00845' |
                           dx29 == '00845' |
                           dx30 == '00845'))) %>%
  mutate(cdi=replace(cdi, is.na(cdi), 0))

cdi.and.renal <-
  cdi.and.renal %>%
  mutate(aki=as.integer((dx1  == '584' | dx1  == '5845' | dx1  == '5846' | dx1  == '5847' | dx1  == '5848' | dx1  == '5849' |
                           dx2  == '584' | dx2  == '5849' | dx2  == '5846' | dx2  == '5847' | dx2  == '5848' | dx2  == '5849' |
                           dx3  == '584' | dx3  == '5849' | dx3  == '5846' | dx3  == '5847' | dx3  == '5848' | dx3  == '5849' |
                           dx4  == '584' | dx4  == '5849' | dx4  == '5846' | dx4  == '5847' | dx4  == '5848' | dx4  == '5849' |
                           dx5  == '584' | dx5  == '5849' | dx5  == '5846' | dx5  == '5847' | dx5  == '5848' | dx5  == '5849' |
                           dx6  == '584' | dx6  == '5849' | dx6  == '5846' | dx6  == '5847' | dx6  == '5848' | dx6  == '5849' |
                           dx7  == '584' | dx7  == '5849' | dx7  == '5846' | dx7  == '5847' | dx7  == '5848' | dx7  == '5849' |
                           dx8  == '584' | dx8  == '5849' | dx8  == '5846' | dx8  == '5847' | dx8  == '5848' | dx8  == '5849' |
                           dx9  == '584' | dx9  == '5849' | dx9  == '5846' | dx9  == '5847' | dx9  == '5848' | dx9  == '5849' |
                           dx10 == '584' | dx10 == '5849' | dx10 == '5846' | dx10 == '5847' | dx10 == '5848' | dx10 == '5849' |
                           dx11 == '584' | dx11 == '5849' | dx11 == '5846' | dx11 == '5847' | dx11 == '5848' | dx11 == '5849' |
                           dx12 == '584' | dx12 == '5849' | dx12 == '5846' | dx12 == '5847' | dx12 == '5848' | dx12 == '5849' |
                           dx13 == '584' | dx13 == '5849' | dx13 == '5846' | dx13 == '5847' | dx13 == '5848' | dx13 == '5849' |
                           dx14 == '584' | dx14 == '5849' | dx14 == '5846' | dx14 == '5847' | dx14 == '5848' | dx14 == '5849' |
                           dx15 == '584' | dx15 == '5849' | dx15 == '5846' | dx15 == '5847' | dx15 == '5848' | dx15 == '5849' |
                           dx16 == '584' | dx16 == '5849' | dx16 == '5846' | dx16 == '5847' | dx16 == '5848' | dx16 == '5849' |
                           dx17 == '584' | dx17 == '5849' | dx17 == '5846' | dx17 == '5847' | dx17 == '5848' | dx17 == '5849' |
                           dx18 == '584' | dx18 == '5849' | dx18 == '5846' | dx18 == '5847' | dx18 == '5848' | dx18 == '5849' |
                           dx19 == '584' | dx19 == '5849' | dx19 == '5846' | dx19 == '5847' | dx19 == '5848' | dx19 == '5849' |
                           dx20 == '584' | dx20 == '5849' | dx20 == '5846' | dx20 == '5847' | dx20 == '5848' | dx20 == '5849' |
                           dx21 == '584' | dx21 == '5849' | dx21 == '5846' | dx21 == '5847' | dx21 == '5848' | dx21 == '5849' |
                           dx22 == '584' | dx22 == '5849' | dx22 == '5846' | dx22 == '5847' | dx22 == '5848' | dx22 == '5849' |
                           dx23 == '584' | dx23 == '5849' | dx23 == '5846' | dx23 == '5847' | dx23 == '5848' | dx23 == '5849' |
                           dx24 == '584' | dx24 == '5849' | dx24 == '5846' | dx24 == '5847' | dx24 == '5848' | dx24 == '5849' |
                           dx25 == '584' | dx25 == '5849' | dx25 == '5846' | dx25 == '5847' | dx25 == '5848' | dx25 == '5849' |
                           dx26 == '584' | dx26 == '5849' | dx26 == '5846' | dx26 == '5847' | dx26 == '5848' | dx26 == '5849' |
                           dx27 == '584' | dx27 == '5849' | dx27 == '5846' | dx27 == '5847' | dx27 == '5848' | dx27 == '5849' |
                           dx28 == '584' | dx28 == '5849' | dx28 == '5846' | dx28 == '5847' | dx28 == '5848' | dx28 == '5849' |
                           dx29 == '584' | dx29 == '5849' | dx29 == '5846' | dx29 == '5847' | dx29 == '5848' | dx29 == '5849' |
                           dx30 == '584' | dx30 == '5849' | dx30 == '5846' | dx30 == '5847' | dx30 == '5848' | dx30 == '5849'))) %>%
  mutate(aki=replace(aki, is.na(aki), 0))

cdi.and.renal <-
  cdi.and.renal %>%
  mutate(ckd=as.integer((dx1  == '585' | dx1  == '5859' |
                           dx2  == '585' | dx2  == '5859' |
                           dx3  == '585' | dx3  == '5859' |
                           dx4  == '585' | dx4  == '5859' |
                           dx5  == '585' | dx5  == '5859' |
                           dx6  == '585' | dx6  == '5859' |
                           dx7  == '585' | dx7  == '5859' |
                           dx8  == '585' | dx8  == '5859' |
                           dx9  == '585' | dx9  == '5859' |
                           dx10 == '585' | dx10 == '5859' |
                           dx11 == '585' | dx11 == '5859' |
                           dx12 == '585' | dx12 == '5859' |
                           dx13 == '585' | dx13 == '5859' |
                           dx14 == '585' | dx14 == '5859' |
                           dx15 == '585' | dx15 == '5859' |
                           dx16 == '585' | dx16 == '5859' |
                           dx17 == '585' | dx17 == '5859' |
                           dx18 == '585' | dx18 == '5859' |
                           dx19 == '585' | dx19 == '5859' |
                           dx20 == '585' | dx20 == '5859' |
                           dx21 == '585' | dx21 == '5859' |
                           dx22 == '585' | dx22 == '5859' |
                           dx23 == '585' | dx23 == '5859' |
                           dx24 == '585' | dx24 == '5859' |
                           dx25 == '585' | dx25 == '5859' |
                           dx26 == '585' | dx26 == '5859' |
                           dx27 == '585' | dx27 == '5859' |
                           dx28 == '585' | dx28 == '5859' |
                           dx29 == '585' | dx29 == '5859' |
                           dx30 == '585' | dx30 == '5859'))) %>%
  mutate(ckd=replace(ckd, is.na(ckd), 0)) 


cdi.and.renal <- 
  cdi.and.renal %>%
  mutate(ckd1=as.integer((dx1  == '5851' |
                            dx2  == '5851' |
                            dx3  == '5851' |
                            dx4  == '5851' |
                            dx5  == '5851' |
                            dx6  == '5851' |
                            dx7  == '5851' |
                            dx8  == '5851' |
                            dx9  == '5851' |
                            dx10 == '5851' |
                            dx11 == '5851' |
                            dx12 == '5851' |
                            dx13 == '5851' |
                            dx14 == '5851' |
                            dx15 == '5851' |
                            dx16 == '5851' |
                            dx17 == '5851' |
                            dx18 == '5851' |
                            dx19 == '5851' |
                            dx20 == '5851' |
                            dx21 == '5851' |
                            dx22 == '5851' |
                            dx23 == '5851' |
                            dx24 == '5851' |
                            dx25 == '5851' |
                            dx26 == '5851' |
                            dx27 == '5851' |
                            dx28 == '5851' |
                            dx29 == '5851' |
                            dx30 == '5851'))) %>%
  mutate(ckd1=replace(ckd1, is.na(ckd1), 0)) 

cdi.and.renal <- 
  cdi.and.renal %>%
  mutate(ckd2=as.integer((dx1  == '5852' |
                            dx2  == '5852' |
                            dx3  == '5852' |
                            dx4  == '5852' |
                            dx5  == '5852' |
                            dx6  == '5852' |
                            dx7  == '5852' |
                            dx8  == '5852' |
                            dx9  == '5852' |
                            dx10 == '5852' |
                            dx11 == '5852' |
                            dx12 == '5852' |
                            dx13 == '5852' |
                            dx14 == '5852' |
                            dx15 == '5852' |
                            dx16 == '5852' |
                            dx17 == '5852' |
                            dx18 == '5852' |
                            dx19 == '5852' |
                            dx20 == '5852' |
                            dx21 == '5852' |
                            dx22 == '5852' |
                            dx23 == '5852' |
                            dx24 == '5852' |
                            dx25 == '5852' |
                            dx26 == '5852' |
                            dx27 == '5852' |
                            dx28 == '5852' |
                            dx29 == '5852' |
                            dx30 == '5852'))) %>%
  mutate(ckd2=replace(ckd2, is.na(ckd2), 0))

cdi.and.renal <- 
  cdi.and.renal %>%
  mutate(ckd3=as.integer((dx1  == '5853' |
                            dx2  == '5853' |
                            dx3  == '5853' |
                            dx4  == '5853' |
                            dx5  == '5853' |
                            dx6  == '5853' |
                            dx7  == '5853' |
                            dx8  == '5853' |
                            dx9  == '5853' |
                            dx10 == '5853' |
                            dx11 == '5853' |
                            dx12 == '5853' |
                            dx13 == '5853' |
                            dx14 == '5853' |
                            dx15 == '5853' |
                            dx16 == '5853' |
                            dx17 == '5853' |
                            dx18 == '5853' |
                            dx19 == '5853' |
                            dx20 == '5853' |
                            dx21 == '5853' |
                            dx22 == '5853' |
                            dx23 == '5853' |
                            dx24 == '5853' |
                            dx25 == '5853' |
                            dx26 == '5853' |
                            dx27 == '5853' |
                            dx28 == '5853' |
                            dx29 == '5853' |
                            dx30 == '5853'))) %>%
  mutate(ckd3=replace(ckd3, is.na(ckd3), 0))

cdi.and.renal <- 
  cdi.and.renal %>%
  mutate(ckd4=as.integer((dx1  == '5854' |
                            dx2  == '5854' |
                            dx3  == '5854' |
                            dx4  == '5854' |
                            dx5  == '5854' |
                            dx6  == '5854' |
                            dx7  == '5854' |
                            dx8  == '5854' |
                            dx9  == '5854' |
                            dx10 == '5854' |
                            dx11 == '5854' |
                            dx12 == '5854' |
                            dx13 == '5854' |
                            dx14 == '5854' |
                            dx15 == '5854' |
                            dx16 == '5854' |
                            dx17 == '5854' |
                            dx18 == '5854' |
                            dx19 == '5854' |
                            dx20 == '5854' |
                            dx21 == '5854' |
                            dx22 == '5854' |
                            dx23 == '5854' |
                            dx24 == '5854' |
                            dx25 == '5854' |
                            dx26 == '5854' |
                            dx27 == '5854' |
                            dx28 == '5854' |
                            dx29 == '5854' |
                            dx30 == '5854'))) %>%
  mutate(ckd4=replace(ckd4, is.na(ckd4), 0)) 
glimpse(cdi.and.renal)

cdi.and.renal <- 
  cdi.and.renal %>%
  mutate(ckd5=as.integer((dx1  == '5855' |
                            dx2  == '5855' |
                            dx3  == '5855' |
                            dx4  == '5855' |
                            dx5  == '5855' |
                            dx6  == '5855' |
                            dx7  == '5855' |
                            dx8  == '5855' |
                            dx9  == '5855' |
                            dx10 == '5855' |
                            dx11 == '5855' |
                            dx12 == '5855' |
                            dx13 == '5855' |
                            dx14 == '5855' |
                            dx15 == '5855' |
                            dx16 == '5855' |
                            dx17 == '5855' |
                            dx18 == '5855' |
                            dx19 == '5855' |
                            dx20 == '5855' |
                            dx21 == '5855' |
                            dx22 == '5855' |
                            dx23 == '5855' |
                            dx24 == '5855' |
                            dx25 == '5855' |
                            dx26 == '5855' |
                            dx27 == '5855' |
                            dx28 == '5855' |
                            dx29 == '5855' |
                            dx30 == '5855'))) %>%
  mutate(ckd5=replace(ckd5, is.na(ckd5), 0))

cdi.and.renal <-
  cdi.and.renal %>%
  mutate(ckd6=as.integer((dx1  == '5856' |
                            dx2  == '5856' |
                            dx3  == '5856' |
                            dx4  == '5856' |
                            dx5  == '5856' |
                            dx6  == '5856' |
                            dx7  == '5856' |
                            dx8  == '5856' |
                            dx9  == '5856' |
                            dx10 == '5856' |
                            dx11 == '5856' |
                            dx12 == '5856' |
                            dx13 == '5856' |
                            dx14 == '5856' |
                            dx15 == '5856' |
                            dx16 == '5856' |
                            dx17 == '5856' |
                            dx18 == '5856' |
                            dx19 == '5856' |
                            dx20 == '5856' |
                            dx21 == '5856' |
                            dx22 == '5856' |
                            dx23 == '5856' |
                            dx24 == '5856' |
                            dx25 == '5856' |
                            dx26 == '5856' |
                            dx27 == '5856' |
                            dx28 == '5856' |
                            dx29 == '5856' |
                            dx30 == '5856'))) %>%
  mutate(ckd6=replace(ckd6, is.na(ckd6), 0)) 

cdi.and.renal <- 
  cdi.and.renal %>%
  mutate(renal_failure_unspecified=as.integer((dx1  == '586' |
                                                 dx2  == '586' |
                                                 dx3  == '586' |
                                                 dx4  == '586' |
                                                 dx5  == '586' |
                                                 dx6  == '586' |
                                                 dx7  == '586' |
                                                 dx8  == '586' |
                                                 dx9  == '586' |
                                                 dx10 == '586' |
                                                 dx11 == '586' |
                                                 dx12 == '586' |
                                                 dx13 == '586' |
                                                 dx14 == '586' |
                                                 dx15 == '586' |
                                                 dx16 == '586' |
                                                 dx17 == '586' |
                                                 dx18 == '586' |
                                                 dx19 == '586' |
                                                 dx20 == '586' |
                                                 dx21 == '586' |
                                                 dx22 == '586' |
                                                 dx23 == '586' |
                                                 dx24 == '586' |
                                                 dx25 == '586' |
                                                 dx26 == '586' |
                                                 dx27 == '586' |
                                                 dx28 == '586' |
                                                 dx29 == '586' |
                                                 dx30 == '586'))) %>%
  mutate(renal_failure_unspecified=replace(renal_failure_unspecified, is.na(renal_failure_unspecified), 0))



write_csv(cdi.and.renal, '/home/bdetweiler/src/Data_Science/stat-8960-capstone-project/data/cdi_and_renal.csv')

cdi.and.renal.reduced <- cdi.and.renal %>% select(matches("nis_key|nis_year|nis_stratum|age|^discwt$|hospid|aki|cdi|ckd.*|renl.*|^los$|died|renal"))

write_csv(cdi.and.renal.reduced, '/home/bdetweiler/src/Data_Science/stat-8960-capstone-project/data/cdi_and_renal_reduced.csv')



all.q <- "SELECT nis_key,
nis_year,
nis_stratum,
age,
discwt,
hospid,
renlfail,
los,
died,
dx1,
dx2,
dx3,
dx4,
dx5,
dx6,
dx7,
dx8,
dx9,
dx10,
dx11,
dx12,
dx13,
dx14,
dx15,
dx16,
dx17,
dx18,
dx19,
dx20,
dx21,
dx22,
dx23,
dx24,
dx25,
dx26,
dx27,
dx28,
dx29,
dx30
FROM nis"

cdi.and.renal <- DBI::dbGetQuery(con, all.q)
# Join all of the stats into a table and write it out
beep(3)
cdi.and.renal <-
  cdi.and.renal %>% 
  mutate(cdi=as.integer((dx1  == '00845' |
                           dx2  == '00845' |
                           dx3  == '00845' |
                           dx4  == '00845' |
                           dx5  == '00845' |
                           dx6  == '00845' |
                           dx7  == '00845' |
                           dx8  == '00845' |
                           dx9  == '00845' |
                           dx10 == '00845' |
                           dx11 == '00845' |
                           dx12 == '00845' |
                           dx13 == '00845' |
                           dx14 == '00845' |
                           dx15 == '00845' |
                           dx16 == '00845' |
                           dx17 == '00845' |
                           dx18 == '00845' |
                           dx19 == '00845' |
                           dx20 == '00845' |
                           dx21 == '00845' |
                           dx22 == '00845' |
                           dx23 == '00845' |
                           dx24 == '00845' |
                           dx25 == '00845' |
                           dx26 == '00845' |
                           dx27 == '00845' |
                           dx28 == '00845' |
                           dx29 == '00845' |
                           dx30 == '00845'))) %>%
  mutate(cdi=replace(cdi, is.na(cdi), 0))

cdi.and.renal <-
  cdi.and.renal %>%
  mutate(aki=as.integer((dx1  == '584' | dx1  == '5845' | dx1  == '5846' | dx1  == '5847' | dx1  == '5848' | dx1  == '5849' |
                           dx2  == '584' | dx2  == '5849' | dx2  == '5846' | dx2  == '5847' | dx2  == '5848' | dx2  == '5849' |
                           dx3  == '584' | dx3  == '5849' | dx3  == '5846' | dx3  == '5847' | dx3  == '5848' | dx3  == '5849' |
                           dx4  == '584' | dx4  == '5849' | dx4  == '5846' | dx4  == '5847' | dx4  == '5848' | dx4  == '5849' |
                           dx5  == '584' | dx5  == '5849' | dx5  == '5846' | dx5  == '5847' | dx5  == '5848' | dx5  == '5849' |
                           dx6  == '584' | dx6  == '5849' | dx6  == '5846' | dx6  == '5847' | dx6  == '5848' | dx6  == '5849' |
                           dx7  == '584' | dx7  == '5849' | dx7  == '5846' | dx7  == '5847' | dx7  == '5848' | dx7  == '5849' |
                           dx8  == '584' | dx8  == '5849' | dx8  == '5846' | dx8  == '5847' | dx8  == '5848' | dx8  == '5849' |
                           dx9  == '584' | dx9  == '5849' | dx9  == '5846' | dx9  == '5847' | dx9  == '5848' | dx9  == '5849' |
                           dx10 == '584' | dx10 == '5849' | dx10 == '5846' | dx10 == '5847' | dx10 == '5848' | dx10 == '5849' |
                           dx11 == '584' | dx11 == '5849' | dx11 == '5846' | dx11 == '5847' | dx11 == '5848' | dx11 == '5849' |
                           dx12 == '584' | dx12 == '5849' | dx12 == '5846' | dx12 == '5847' | dx12 == '5848' | dx12 == '5849' |
                           dx13 == '584' | dx13 == '5849' | dx13 == '5846' | dx13 == '5847' | dx13 == '5848' | dx13 == '5849' |
                           dx14 == '584' | dx14 == '5849' | dx14 == '5846' | dx14 == '5847' | dx14 == '5848' | dx14 == '5849' |
                           dx15 == '584' | dx15 == '5849' | dx15 == '5846' | dx15 == '5847' | dx15 == '5848' | dx15 == '5849' |
                           dx16 == '584' | dx16 == '5849' | dx16 == '5846' | dx16 == '5847' | dx16 == '5848' | dx16 == '5849' |
                           dx17 == '584' | dx17 == '5849' | dx17 == '5846' | dx17 == '5847' | dx17 == '5848' | dx17 == '5849' |
                           dx18 == '584' | dx18 == '5849' | dx18 == '5846' | dx18 == '5847' | dx18 == '5848' | dx18 == '5849' |
                           dx19 == '584' | dx19 == '5849' | dx19 == '5846' | dx19 == '5847' | dx19 == '5848' | dx19 == '5849' |
                           dx20 == '584' | dx20 == '5849' | dx20 == '5846' | dx20 == '5847' | dx20 == '5848' | dx20 == '5849' |
                           dx21 == '584' | dx21 == '5849' | dx21 == '5846' | dx21 == '5847' | dx21 == '5848' | dx21 == '5849' |
                           dx22 == '584' | dx22 == '5849' | dx22 == '5846' | dx22 == '5847' | dx22 == '5848' | dx22 == '5849' |
                           dx23 == '584' | dx23 == '5849' | dx23 == '5846' | dx23 == '5847' | dx23 == '5848' | dx23 == '5849' |
                           dx24 == '584' | dx24 == '5849' | dx24 == '5846' | dx24 == '5847' | dx24 == '5848' | dx24 == '5849' |
                           dx25 == '584' | dx25 == '5849' | dx25 == '5846' | dx25 == '5847' | dx25 == '5848' | dx25 == '5849' |
                           dx26 == '584' | dx26 == '5849' | dx26 == '5846' | dx26 == '5847' | dx26 == '5848' | dx26 == '5849' |
                           dx27 == '584' | dx27 == '5849' | dx27 == '5846' | dx27 == '5847' | dx27 == '5848' | dx27 == '5849' |
                           dx28 == '584' | dx28 == '5849' | dx28 == '5846' | dx28 == '5847' | dx28 == '5848' | dx28 == '5849' |
                           dx29 == '584' | dx29 == '5849' | dx29 == '5846' | dx29 == '5847' | dx29 == '5848' | dx29 == '5849' |
                           dx30 == '584' | dx30 == '5849' | dx30 == '5846' | dx30 == '5847' | dx30 == '5848' | dx30 == '5849'))) %>%
  mutate(aki=replace(aki, is.na(aki), 0))

cdi.and.renal <-
  cdi.and.renal %>%
  mutate(ckd=as.integer((dx1  == '585' | dx1  == '5859' |
                           dx2  == '585' | dx2  == '5859' |
                           dx3  == '585' | dx3  == '5859' |
                           dx4  == '585' | dx4  == '5859' |
                           dx5  == '585' | dx5  == '5859' |
                           dx6  == '585' | dx6  == '5859' |
                           dx7  == '585' | dx7  == '5859' |
                           dx8  == '585' | dx8  == '5859' |
                           dx9  == '585' | dx9  == '5859' |
                           dx10 == '585' | dx10 == '5859' |
                           dx11 == '585' | dx11 == '5859' |
                           dx12 == '585' | dx12 == '5859' |
                           dx13 == '585' | dx13 == '5859' |
                           dx14 == '585' | dx14 == '5859' |
                           dx15 == '585' | dx15 == '5859' |
                           dx16 == '585' | dx16 == '5859' |
                           dx17 == '585' | dx17 == '5859' |
                           dx18 == '585' | dx18 == '5859' |
                           dx19 == '585' | dx19 == '5859' |
                           dx20 == '585' | dx20 == '5859' |
                           dx21 == '585' | dx21 == '5859' |
                           dx22 == '585' | dx22 == '5859' |
                           dx23 == '585' | dx23 == '5859' |
                           dx24 == '585' | dx24 == '5859' |
                           dx25 == '585' | dx25 == '5859' |
                           dx26 == '585' | dx26 == '5859' |
                           dx27 == '585' | dx27 == '5859' |
                           dx28 == '585' | dx28 == '5859' |
                           dx29 == '585' | dx29 == '5859' |
                           dx30 == '585' | dx30 == '5859'))) %>%
  mutate(ckd=replace(ckd, is.na(ckd), 0)) 


cdi.and.renal <- 
  cdi.and.renal %>%
  mutate(ckd1=as.integer((dx1  == '5851' |
                            dx2  == '5851' |
                            dx3  == '5851' |
                            dx4  == '5851' |
                            dx5  == '5851' |
                            dx6  == '5851' |
                            dx7  == '5851' |
                            dx8  == '5851' |
                            dx9  == '5851' |
                            dx10 == '5851' |
                            dx11 == '5851' |
                            dx12 == '5851' |
                            dx13 == '5851' |
                            dx14 == '5851' |
                            dx15 == '5851' |
                            dx16 == '5851' |
                            dx17 == '5851' |
                            dx18 == '5851' |
                            dx19 == '5851' |
                            dx20 == '5851' |
                            dx21 == '5851' |
                            dx22 == '5851' |
                            dx23 == '5851' |
                            dx24 == '5851' |
                            dx25 == '5851' |
                            dx26 == '5851' |
                            dx27 == '5851' |
                            dx28 == '5851' |
                            dx29 == '5851' |
                            dx30 == '5851'))) %>%
  mutate(ckd1=replace(ckd1, is.na(ckd1), 0)) 

cdi.and.renal <- 
  cdi.and.renal %>%
  mutate(ckd2=as.integer((dx1  == '5852' |
                            dx2  == '5852' |
                            dx3  == '5852' |
                            dx4  == '5852' |
                            dx5  == '5852' |
                            dx6  == '5852' |
                            dx7  == '5852' |
                            dx8  == '5852' |
                            dx9  == '5852' |
                            dx10 == '5852' |
                            dx11 == '5852' |
                            dx12 == '5852' |
                            dx13 == '5852' |
                            dx14 == '5852' |
                            dx15 == '5852' |
                            dx16 == '5852' |
                            dx17 == '5852' |
                            dx18 == '5852' |
                            dx19 == '5852' |
                            dx20 == '5852' |
                            dx21 == '5852' |
                            dx22 == '5852' |
                            dx23 == '5852' |
                            dx24 == '5852' |
                            dx25 == '5852' |
                            dx26 == '5852' |
                            dx27 == '5852' |
                            dx28 == '5852' |
                            dx29 == '5852' |
                            dx30 == '5852'))) %>%
  mutate(ckd2=replace(ckd2, is.na(ckd2), 0))

cdi.and.renal <- 
  cdi.and.renal %>%
  mutate(ckd3=as.integer((dx1  == '5853' |
                            dx2  == '5853' |
                            dx3  == '5853' |
                            dx4  == '5853' |
                            dx5  == '5853' |
                            dx6  == '5853' |
                            dx7  == '5853' |
                            dx8  == '5853' |
                            dx9  == '5853' |
                            dx10 == '5853' |
                            dx11 == '5853' |
                            dx12 == '5853' |
                            dx13 == '5853' |
                            dx14 == '5853' |
                            dx15 == '5853' |
                            dx16 == '5853' |
                            dx17 == '5853' |
                            dx18 == '5853' |
                            dx19 == '5853' |
                            dx20 == '5853' |
                            dx21 == '5853' |
                            dx22 == '5853' |
                            dx23 == '5853' |
                            dx24 == '5853' |
                            dx25 == '5853' |
                            dx26 == '5853' |
                            dx27 == '5853' |
                            dx28 == '5853' |
                            dx29 == '5853' |
                            dx30 == '5853'))) %>%
  mutate(ckd3=replace(ckd3, is.na(ckd3), 0))

cdi.and.renal <- 
  cdi.and.renal %>%
  mutate(ckd4=as.integer((dx1  == '5854' |
                            dx2  == '5854' |
                            dx3  == '5854' |
                            dx4  == '5854' |
                            dx5  == '5854' |
                            dx6  == '5854' |
                            dx7  == '5854' |
                            dx8  == '5854' |
                            dx9  == '5854' |
                            dx10 == '5854' |
                            dx11 == '5854' |
                            dx12 == '5854' |
                            dx13 == '5854' |
                            dx14 == '5854' |
                            dx15 == '5854' |
                            dx16 == '5854' |
                            dx17 == '5854' |
                            dx18 == '5854' |
                            dx19 == '5854' |
                            dx20 == '5854' |
                            dx21 == '5854' |
                            dx22 == '5854' |
                            dx23 == '5854' |
                            dx24 == '5854' |
                            dx25 == '5854' |
                            dx26 == '5854' |
                            dx27 == '5854' |
                            dx28 == '5854' |
                            dx29 == '5854' |
                            dx30 == '5854'))) %>%
  mutate(ckd4=replace(ckd4, is.na(ckd4), 0)) 
glimpse(cdi.and.renal)

cdi.and.renal <- 
  cdi.and.renal %>%
  mutate(ckd5=as.integer((dx1  == '5855' |
                            dx2  == '5855' |
                            dx3  == '5855' |
                            dx4  == '5855' |
                            dx5  == '5855' |
                            dx6  == '5855' |
                            dx7  == '5855' |
                            dx8  == '5855' |
                            dx9  == '5855' |
                            dx10 == '5855' |
                            dx11 == '5855' |
                            dx12 == '5855' |
                            dx13 == '5855' |
                            dx14 == '5855' |
                            dx15 == '5855' |
                            dx16 == '5855' |
                            dx17 == '5855' |
                            dx18 == '5855' |
                            dx19 == '5855' |
                            dx20 == '5855' |
                            dx21 == '5855' |
                            dx22 == '5855' |
                            dx23 == '5855' |
                            dx24 == '5855' |
                            dx25 == '5855' |
                            dx26 == '5855' |
                            dx27 == '5855' |
                            dx28 == '5855' |
                            dx29 == '5855' |
                            dx30 == '5855'))) %>%
  mutate(ckd5=replace(ckd5, is.na(ckd5), 0))

cdi.and.renal <-
  cdi.and.renal %>%
  mutate(ckd6=as.integer((dx1  == '5856' |
                            dx2  == '5856' |
                            dx3  == '5856' |
                            dx4  == '5856' |
                            dx5  == '5856' |
                            dx6  == '5856' |
                            dx7  == '5856' |
                            dx8  == '5856' |
                            dx9  == '5856' |
                            dx10 == '5856' |
                            dx11 == '5856' |
                            dx12 == '5856' |
                            dx13 == '5856' |
                            dx14 == '5856' |
                            dx15 == '5856' |
                            dx16 == '5856' |
                            dx17 == '5856' |
                            dx18 == '5856' |
                            dx19 == '5856' |
                            dx20 == '5856' |
                            dx21 == '5856' |
                            dx22 == '5856' |
                            dx23 == '5856' |
                            dx24 == '5856' |
                            dx25 == '5856' |
                            dx26 == '5856' |
                            dx27 == '5856' |
                            dx28 == '5856' |
                            dx29 == '5856' |
                            dx30 == '5856'))) %>%
  mutate(ckd6=replace(ckd6, is.na(ckd6), 0)) 

cdi.and.renal <- 
  cdi.and.renal %>%
  mutate(renal_failure_unspecified=as.integer((dx1  == '586' |
                                                 dx2  == '586' |
                                                 dx3  == '586' |
                                                 dx4  == '586' |
                                                 dx5  == '586' |
                                                 dx6  == '586' |
                                                 dx7  == '586' |
                                                 dx8  == '586' |
                                                 dx9  == '586' |
                                                 dx10 == '586' |
                                                 dx11 == '586' |
                                                 dx12 == '586' |
                                                 dx13 == '586' |
                                                 dx14 == '586' |
                                                 dx15 == '586' |
                                                 dx16 == '586' |
                                                 dx17 == '586' |
                                                 dx18 == '586' |
                                                 dx19 == '586' |
                                                 dx20 == '586' |
                                                 dx21 == '586' |
                                                 dx22 == '586' |
                                                 dx23 == '586' |
                                                 dx24 == '586' |
                                                 dx25 == '586' |
                                                 dx26 == '586' |
                                                 dx27 == '586' |
                                                 dx28 == '586' |
                                                 dx29 == '586' |
                                                 dx30 == '586'))) %>%
  mutate(renal_failure_unspecified=replace(renal_failure_unspecified, is.na(renal_failure_unspecified), 0))

write_csv( select(cdi.and.renal, nis_key,
                  nis_year,
                  nis_stratum,
                  age,
                  discwt,
                  hospid,
                  renlfail,
                  los,
                  died,
                  cdi,
                  aki,
                  ckd,
                  ckd1,
                  ckd2,
                  ckd3,
                  ckd4,
                  ckd5,
                  ckd6,
                  renal_failure_unspecified),
           "data/cdiff_and_renal_all.csv")
beep(3)


proportions <- list() 
cdi.and.renal.reduced <- list()
y <- 2014
for (y in seq(2001, 2014, by=1)) {
  
  print(y)
  
  #setwd('/home/bdetweiler/src/Data_Science/stat-8960-capstone-project/thesis/')
  cdi.and.renal.reduced <- read_csv(paste0('../data/cdiff_and_renal_all_', y, '.csv'))
  cdiff.design <- svydesign(ids = ~hospid, 
                            data = cdi.and.renal.reduced,
                            weights = ~discwt, 
                            strata = ~nis_stratum,
                            nest=TRUE)
  
  proportions[[paste0(y, "_cdi")]] <- svyciprop(~I(cdi==1), cdiff.design, method = "logit", level = 0.95)
  proportions[[paste0(y, "_aki")]] <- svyciprop(~I(aki==1), cdiff.design, method = "logit", level = 0.95)
  proportions[[paste0(y, "_ckd")]] <- svyciprop(~I(ckd==1), cdiff.design, method = "logit", level = 0.95)
  proportions[[paste0(y, "_ckd1")]] <- svyciprop(~I(ckd1==1), cdiff.design, method = "logit", level = 0.95)
  proportions[[paste0(y, "_ckd2")]] <- svyciprop(~I(ckd2==1), cdiff.design, method = "logit", level = 0.95)
  proportions[[paste0(y, "_ckd3")]] <- svyciprop(~I(ckd3==1), cdiff.design, method = "logit", level = 0.95)
  proportions[[paste0(y, "_ckd4")]] <- svyciprop(~I(ckd4==1), cdiff.design, method = "logit", level = 0.95)
  proportions[[paste0(y, "_ckd5")]] <- svyciprop(~I(ckd5==1), cdiff.design, method = "logit", level = 0.95)
  proportions[[paste0(y, "_ckd6")]] <- svyciprop(~I(ckd6==1), cdiff.design, method = "logit", level = 0.95)
  proportions[[paste0(y, "_renal_failure")]] <- svyciprop(~I(aki+ckd+ckd1+ckd2+ckd3+ckd4+ckd5+ckd6 > 0), cdiff.design, method = "logit", level = 0.95)
  #svyciprop(~(I(cdi==1) & I(aki+ckd+ckd1+ckd2+ckd3+ckd4+ckd5+ckd6 > 0)), cdiff.design, method = "logit", level = 0.95)
  
  rm(cdi.and.renal.reduced)
  rm(cdiff.design)
  gc()
}
beep(3)

proportions

diseases <- c("cdi", "aki", "ckd", "ckd1", "ckd2", "ckd3", "ckd4", "ckd5", "ckd6", "renal_failure")
y <- 2001
d <- diseases[1]
final.df <- data_frame(disease="", 
                       year=2000, 
                       theta=0,
                       ci2.5=0,
                       ci97.5=0)
for (y in seq(2001, 2014, by=1)) {
  for (d in diseases) {
    df <- data_frame(disease=d, 
                     year=y, 
                     theta=as.vector(proportions[[paste0(y, "_", d)]]),
                     ci2.5=attr(proportions[[paste0(y, "_", d)]], "ci")[[1]], 
                     ci97.5=attr(proportions[[paste0(y, "_", d)]], "ci")[[2]])
    
    final.df <- bind_rows(final.df, df)
  }
}

write_csv(final.df, "../data/proportions.csv")



# echo "`l NIS* | grep -i CSV | awk '{print $5}' | awk '{s+=$1} END {print s}'` + `l  NRD201* | grep CSV | awk '{print $5}' | awk '{s+=$1} END {print s}'`" | bc
proportions <- list() 
cdi.and.renal.reduced <- list()
y <- 2014
for (y in seq(2001, 2014, by=1)) {
  
  print(y)
  
  #setwd('/home/bdetweiler/src/Data_Science/stat-8960-capstone-project/thesis/')
  cdi.and.renal.reduced <- read_csv(paste0('../data/cdiff_and_renal_all_', y, '.csv'))
  cdiff.design <- svydesign(ids = ~hospid, 
                            data = cdi.and.renal.reduced,
                            weights = ~discwt, 
                            strata = ~nis_stratum,
                            nest=TRUE)
  
  proportions[[paste0(y, "_cdi")]] <- svyciprop(~I(cdi==1), cdiff.design, method = "logit", level = 0.95)
  proportions[[paste0(y, "_aki")]] <- svyciprop(~I(aki==1), cdiff.design, method = "logit", level = 0.95)
  proportions[[paste0(y, "_ckd")]] <- svyciprop(~I(ckd==1), cdiff.design, method = "logit", level = 0.95)
  proportions[[paste0(y, "_ckd1")]] <- svyciprop(~I(ckd1==1), cdiff.design, method = "logit", level = 0.95)
  proportions[[paste0(y, "_ckd2")]] <- svyciprop(~I(ckd2==1), cdiff.design, method = "logit", level = 0.95)
  proportions[[paste0(y, "_ckd3")]] <- svyciprop(~I(ckd3==1), cdiff.design, method = "logit", level = 0.95)
  proportions[[paste0(y, "_ckd4")]] <- svyciprop(~I(ckd4==1), cdiff.design, method = "logit", level = 0.95)
  proportions[[paste0(y, "_ckd5")]] <- svyciprop(~I(ckd5==1), cdiff.design, method = "logit", level = 0.95)
  proportions[[paste0(y, "_ckd6")]] <- svyciprop(~I(ckd6==1), cdiff.design, method = "logit", level = 0.95)
  proportions[[paste0(y, "_renal_failure")]] <- svyciprop(~I(aki+ckd+ckd1+ckd2+ckd3+ckd4+ckd5+ckd6 > 0), cdiff.design, method = "logit", level = 0.95)
  #svyciprop(~(I(cdi==1) & I(aki+ckd+ckd1+ckd2+ckd3+ckd4+ckd5+ckd6 > 0)), cdiff.design, method = "logit", level = 0.95)
  
  rm(cdi.and.renal.reduced)
  rm(cdiff.design)
  gc()
}
beep(3)

proportions

diseases <- c("cdi", "aki", "ckd", "ckd1", "ckd2", "ckd3", "ckd4", "ckd5", "ckd6", "renal_failure")
y <- 2001
d <- diseases[1]
final.df <- data_frame(disease="", 
                       year=2000, 
                       theta=0,
                       ci2.5=0,
                       ci97.5=0)
for (y in seq(2001, 2014, by=1)) {
  for (d in diseases) {
    df <- data_frame(disease=d, 
                     year=y, 
                     theta=as.vector(proportions[[paste0(y, "_", d)]]),
                     ci2.5=attr(proportions[[paste0(y, "_", d)]], "ci")[[1]], 
                     ci97.5=attr(proportions[[paste0(y, "_", d)]], "ci")[[2]])
    
    final.df <- bind_rows(final.df, df)
  }
}


cdiff.ages <- filter(cdiff, !is.na(age))
cdiff.design <- svydesign(ids = ~hospid, 
                          data = cdiff.ages, 
                          weights = ~discwt, 
                          strata = ~nis_stratum,
                          nest=TRUE)

mode <- mlv(cdiff.ages$age, method = "mfv")
mode <- mode$M
qntl <- svyquantile(~age, cdiff.design, c(0.25, 0.5, 0.75))

xbar.weighted <- svymean(x = ~age, design=cdiff.design, deff=TRUE)

p <- cdiff.ages %>% 
  select(age, discwt) %>%
  ggplot(aes(age, group=1, weight=discwt)) +
  geom_histogram(stat="bin", bins=30) +
  geom_vline(xintercept = qntl[[2]], col="red") +
  geom_vline(xintercept = qntl[[1]], col="blue") +
  geom_vline(xintercept = qntl[[3]], col="blue") +
  labs(title="C. diff infections by age", y="Count", x="Age")
print(p)



ts.by.year <- list()

from <- 1
to <- 0
for (i in 1:20) {
  from <- to
  to <- from + 5
  
  age.window <- cdiff %>% 
    filter(!is.na(age) & age >= from & age < to) %>% 
    select(nis_year) %>%
    group_by(nis_year) %>%
    summarise(count=n())
  
  
  my.ts <- ts(age.window$count, start = 2001, end = 2014, frequency = 1)
  
  #if (i == 2001) {
  ts.by.year[[paste0(from, "_", to)]] <- my.ts
  #} else {
  #ts(age.window$count, start = 2001, end = 2014, frequency = 1)
  #}
}

plot.ts <- data.frame(year=2001:2014)
plot.ts <- cbind(plot.ts, data.frame('0_5'=ts.by.year[['0_5']]))
plot.ts <- cbind(plot.ts, data.frame('5_10'=ts.by.year[['5_10']]))
plot.ts <- cbind(plot.ts, data.frame('10_15'=ts.by.year[['10_15']]))
plot.ts <- cbind(plot.ts, data.frame('15_20'=ts.by.year[['15_20']]))
plot.ts <- cbind(plot.ts, data.frame('20_25'=ts.by.year[['20_25']]))
plot.ts <- cbind(plot.ts, data.frame('25_30'=ts.by.year[['25_30']]))
plot.ts <- cbind(plot.ts, data.frame('30_35'=ts.by.year[['30_35']]))
plot.ts <- cbind(plot.ts, data.frame('35_40'=ts.by.year[['35_40']]))
plot.ts <- cbind(plot.ts, data.frame('40_45'=ts.by.year[['40_45']]))
plot.ts <- cbind(plot.ts, data.frame('45_50'=ts.by.year[['45_50']]))
plot.ts <- cbind(plot.ts, data.frame('50_55'=ts.by.year[['50_55']]))
plot.ts <- cbind(plot.ts, data.frame('55_60'=ts.by.year[['55_60']]))
plot.ts <- cbind(plot.ts, data.frame('60_65'=ts.by.year[['60_65']]))
plot.ts <- cbind(plot.ts, data.frame('65_70'=ts.by.year[['65_70']]))
plot.ts <- cbind(plot.ts, data.frame('70_75'=ts.by.year[['70_75']]))
plot.ts <- cbind(plot.ts, data.frame('75_80'=ts.by.year[['75_80']]))
plot.ts <- cbind(plot.ts, data.frame('80_85'=ts.by.year[['80_85']]))
plot.ts <- cbind(plot.ts, data.frame('85_90'=ts.by.year[['85_90']]))
plot.ts <- cbind(plot.ts, data.frame('90_95'=ts.by.year[['90_95']]))
plot.ts <- cbind(plot.ts, data.frame('95_100'=ts.by.year[['95_100']]))

plot.ts.m <- melt(plot.ts, id.vars=c('year'))

labels <- gsub('_', '-', gsub('X', replacement = '', as.character(plot.ts.m$variable)))
plot.ts.m$variable <- factor(labels, levels = unique(labels))

cols <- c('0-5'   = "#e6e6ff",
          '5-10'  = "#ccccff",
          '10-15' = "#b3b3ff",
          '15-20' = "#9999ff",
          '20-25' = "#8080ff",
          '25-30' = "#6666ff",
          '30-35' = "#4d4dff",
          '35-40' = "#3333ff",
          '40-45' = "#1a1aff",
          '45-50' = "#0000ff",
          
          # RED - increasing 
          '50-55' = "#cc0000",
          '55-60' = "#b30000",
          '60-65' = "#990000",
          '65-70' = "#800000",
          '70-75' = "#660000",
          
          # GREEN - Somewhat decreasing
          '75-80' = "#006600",
          '80-85' = "#004d00",
          '85-90' = "#008000",
          '90-95' = "#003300",
          
          '95-100' = "#000000")

plot.ts.m %>% 
  ggplot(aes(x=year, y=value, colour=variable)) +
  geom_line() +
  scale_colour_manual(values = cols) +
  labs(title="Time series of C. diff cases by 5-year age groups", x="Year", y="Count", colour="Ages")




######################

esrd <- list() 
y <- 2014
for (y in seq(2001, 2014, by=1)) {
  
  print(y)
  
  #setwd('/home/bdetweiler/src/Data_Science/stat-8960-capstone-project/thesis/')
  cdi.and.renal.reduced <- read_csv(paste0('../data/cdiff_and_renal_all_', y, '.csv'))
  #cdiff.design <- svydesign(ids = ~hospid, 
  #data = cdi.and.renal.reduced,
  #weights = ~discwt, 
  #strata = ~nis_stratum,
  #nest=TRUE)
  
  #fit <- svyglm(I(ckd6 == 1)~age, cdiff.design, family=quasibinomial())
  
  esrd[[y]] <- cdi.and.renal.reduced %>% 
    filter(ckd6 == 1)  %>% 
    select(age, nis_year, discwt)
  
  esrd[[2014]] 
  rm(cdi.and.renal.reduced)
  gc()
}

df <- esrd[[2001]]
for (y in seq(from=2002, to=2014, by=1)) {
  print(y)
  df <- bind_rows(df, esrd[[y]])
}

write_csv(df, '/home/bdetweiler/src/Data_Science/stat-8960-capstone-project/data/esrd.csv')

ggplot(df, aes(x = age, y = nis_year, group = nis_year)) + 
  geom_density_ridges(aes(height=..density.., weight=discwt), stat="density") +
  labs(title="ESRD distribution by age over time", x="Age", y="Year")

beep(3)



### Get ESRD 

ages <- list() 
y <- 2014
for (y in seq(2001, 2014, by=1)) {
  
  print(y)
  
  #setwd('/home/bdetweiler/src/Data_Science/stat-8960-capstone-project/thesis/')
  cdi.and.renal.reduced <- read_csv(paste0('../data/cdiff_and_renal_all_', y, '.csv'))
  cdi.and.renal.reduced <- filter(cdi.and.renal.reduced, !is.na(age))
  
  subgroup <- filter(cdi.and.renal.reduced, cdi == 1) 
  ds <- svydesign(ids = ~hospid, 
                  data = subgroup,
                  weights = ~discwt, 
                  strata = ~nis_stratum,
                  nest=TRUE)
  
  ages[[paste0(y, "_cdi")]] <- svymean(~age, ds, level = 0.95)
  
  subgroup <- filter(cdi.and.renal.reduced, (ckd == 1 | ckd1 == 1 | ckd2 == 1 | ckd3 == 1 | ckd4 == 1 | ckd5 == 1))
  
  ds <- svydesign(ids = ~hospid, 
                  data = subgroup,
                  weights = ~discwt, 
                  strata = ~nis_stratum,
                  nest=TRUE)
  
  ages[[paste0(y, "_ckd")]] <- svymean(~age, ds, level = 0.95)
  
  
  subgroup <- filter(cdi.and.renal.reduced, (aki == 1))
  
  ds <- svydesign(ids = ~hospid, 
                  data = subgroup,
                  weights = ~discwt, 
                  strata = ~nis_stratum,
                  nest=TRUE)
  
  ages[[paste0(y, "_aki")]] <- svymean(~age, ds, level = 0.95)
  
  subgroup <- filter(cdi.and.renal.reduced, (ckd6 == 1))
  
  if (nrow(subgroup) > 0) {
    ds <- svydesign(ids = ~hospid, 
                    data = subgroup,
                    weights = ~discwt, 
                    strata = ~nis_stratum,
                    nest=TRUE)
    
    ages[[paste0(y, "_esrd")]] <- svymean(~age, ds, level = 0.95)
  }
  
  rm(cdi.and.renal.reduced)
  rm(cdiff.design)
  gc()
}
ages
beep(3)


y <- 2001
d <- diseases[1]
final.df <- data_frame(disease="", 
                       year=2000, 
                       theta=0,
                       ci2.5=0,
                       ci97.5=0)
for (y in seq(2001, 2014, by=1)) {
  print(y)
  if (y < 2005 ) {
    diseases <- c("cdi", "aki", "ckd")
  } else {
    diseases <- c("cdi", "aki", "ckd", "esrd")
  }
  
  for (d in diseases) {
    print(d)
    
    df <- data_frame(disease=d, 
                     year=y, 
                     theta=as.vector(ages[[paste0(y, "_", d)]]),
                     ci2.5=as.vector(a) + sqrt(as.vector(attr(a, "var"))) * 1.96,
                     ci97.5=as.vector(a) - sqrt(as.vector(attr(a, "var"))) * 1.96)
    
    final.df <- bind_rows(final.df, df)
  }
}

write_csv(final.df, '../data/ages.csv')


ages <- list() 
y <- 2014
for (y in seq(2001, 2014, by=1)) {
  
  print(y)
  
  #setwd('/home/bdetweiler/src/Data_Science/stat-8960-capstone-project/thesis/')
  cdi.and.renal.reduced <- read_csv(paste0('../data/cdiff_and_renal_all_', y, '.csv'))
  cdi.and.renal.reduced <- filter(cdi.and.renal.reduced, !is.na(age))
  
  subgroup <- filter(cdi.and.renal.reduced, cdi == 1) 
  ds <- svydesign(ids = ~hospid, 
                  data = subgroup,
                  weights = ~discwt, 
                  strata = ~nis_stratum,
                  nest=TRUE)
  
  ages[[paste0(y, "_cdi")]] <- svyquantile(~age, ds, c(0.25, 0.5, 0.75), ci=TRUE)
  
  subgroup <- filter(cdi.and.renal.reduced, (ckd == 1 | ckd1 == 1 | ckd2 == 1 | ckd3 == 1 | ckd4 == 1 | ckd5 == 1))
  
  ds <- svydesign(ids = ~hospid, 
                  data = subgroup,
                  weights = ~discwt, 
                  strata = ~nis_stratum,
                  nest=TRUE)
  
  ages[[paste0(y, "_ckd")]] <- svyquantile(~age, ds, c(0.25, 0.5, 0.75), ci=TRUE)
  
  
  subgroup <- filter(cdi.and.renal.reduced, (aki == 1))
  
  ds <- svydesign(ids = ~hospid, 
                  data = subgroup,
                  weights = ~discwt, 
                  strata = ~nis_stratum,
                  nest=TRUE)
  
  ages[[paste0(y, "_aki")]] <- svyquantile(~age, ds, c(0.25, 0.5, 0.75), ci=TRUE)
  
  subgroup <- filter(cdi.and.renal.reduced, (ckd6 == 1))
  
  if (nrow(subgroup) > 0) {
    ds <- svydesign(ids = ~hospid, 
                    data = subgroup,
                    weights = ~discwt, 
                    strata = ~nis_stratum,
                    nest=TRUE)
    
    ages[[paste0(y, "_esrd")]] <- svyquantile(~age, ds, c(0.25, 0.5, 0.75), ci=TRUE)
  }
  
  rm(cdi.and.renal.reduced)
  rm(cdiff.design)
  gc()
}
ages
beep(3)

y <- 2001
d <- diseases[1]
final.df <- data_frame(disease="", 
                       year=2000, 
                       theta25=0,
                       theta25_2.5=0,
                       theta25_97.5=0,
                       theta50=0,
                       theta50_2.5=0,
                       theta50_97.5=0,
                       theta75=0,
                       theta75_2.5=0,
                       theta75_97.5=0)
final.df

for (y in seq(2001, 2014, by=1)) {
  print(y)
  if (y < 2005 ) {
    diseases <- c("cdi", "aki", "ckd")
  } else {
    diseases <- c("cdi", "aki", "ckd", "esrd")
  }
  
  d <- diseases[1] 
  for (d in diseases) {
    print(d)
    
    df <- data_frame(disease=d, 
                     year=y, 
                     theta25=as.vector(ages[[paste0(y, "_", d)]]$quantiles)[1],
                     theta25_2.5=as.vector(ages[[paste0(y, "_", d)]]$CIs)[1],
                     theta25_97.5=as.vector(ages[[paste0(y, "_", d)]]$CIs)[2],
                     theta50=as.vector(ages[[paste0(y, "_", d)]]$quantiles)[2],
                     theta50_2.5=as.vector(ages[[paste0(y, "_", d)]]$CIs)[3],
                     theta50_97.5=as.vector(ages[[paste0(y, "_", d)]]$CIs)[4],
                     theta75=as.vector(ages[[paste0(y, "_", d)]]$quantiles)[3],
                     theta75_2.5=as.vector(ages[[paste0(y, "_", d)]]$CIs)[5],
                     theta75_97.5=as.vector(ages[[paste0(y, "_", d)]]$CIs)[6])
    
    
    final.df <- bind_rows(final.df, df)
  }
}
final.df
write_csv(final.df, '../data/ages_quantiles.csv')





##### Get yearly age trends by age buckets

ts.by.year <- list()
ages <- list()

from <- 1
to <- 0
i <- 1
for (i in 1:20) {
  from <- to
  to <- from + 5
  
  print(paste0('age group ', from, '_', to))
  
  y <- 2001
  for (y in 2001:2014) {
    print(y)
    age.window <- cdiff %>% 
      filter(!is.na(age) & age >= from & age < to) %>%
      filter(nis_year == y) %>%
      select(nis_year, discwt, nis_stratum, hospid) %>%
      mutate(dummy=1)
    
    ds <- svydesign(ids = ~hospid, 
                    data = age.window,
                    weights = ~discwt, 
                    strata = ~nis_stratum,
                    nest=TRUE)
    
    ages[[paste0(from, "_", to, "_", y)]] <- svytotal(~dummy, ds, ci=TRUE)
  }
  
  #age.window
  
  #my.ts <- ts(age.window$count, start = 2001, end = 2014, frequency = 1)
  
  #if (i == 2001) {
  #ts.by.year[[paste0(from, "_", to)]] <- my.ts
  #} else {
  #ts(age.window$count, start = 2001, end = 2014, frequency = 1)
  #}
}

from <- 1
to <- 0
i <- 1
df <- data_frame(year=2000, age.bucket='-1', total=0, SE=0)

for (i in 1:20) {
  from <- to
  to <- from + 5
  
  print(paste0('age group ', from, '_', to))
  
  y <- 2001
  for (y in 2001:2014) {
    total <- tidy(print(ages[[paste0(from, "_", to, "_", y)]])) %>% select(total, SE) %>% pull(total)
    SE <- tidy(print(ages[[paste0(from, "_", to, "_", y)]])) %>% select(total, SE) %>% pull(SE)
    
    df <- bind_rows(df, data_frame(year=y, age.bucket=paste0(from, '_', to), total, SE))
  }
}
df <- df %>% filter(year > 2000)

for (age in unique(df$age.bucket)) {
  
  if (age == '95_100') {
    break
  }
  print(age)
  age.df <- df %>% filter(age.bucket == age) %>% select(total)
  print(age.df)
  my.ts <- ts(age.df$total, start = 2001, end = 2014, frequency = 1)
  
  ts.by.year[[paste0(age)]] <- my.ts
  
}
saveRDS(ts.by.year, '../data/cdi_ages_ts.rds')



df <- data_frame(year=2000, tot.preg=-1, tot.not.preg=-1, prop=0, prop2.5=0, prop97.5=0)
female.preg <- list()
### Get female pregnancy
y <- 2001
for (y in 2001:2014) {
  mf <- cdiff %>% 
    select(female, age, hospid, nis_stratum, discwt, nis_year) %>%
    filter(!is.na(female)) %>%
    filter(female == 1) %>%
    filter(nis_year == y)
 mf 
  
  ds <- svydesign(ids = ~hospid, 
                  data = mf,
                  weights = ~discwt, 
                  strata = ~nis_stratum,
                  nest=TRUE)
  
  
  prop <- svyciprop(~I(female==1), ds, level = .95, rm.na=TRUE) 
  prop.val <- as.vector(prop)
  prop.val.2.5 <- attr(prop, "ci")[[1]]
  prop.val.97.5 <- attr(prop, "ci")[[2]]
  
  tot <- svytotal(~I(female==1), ds, level = .95, rm.na=TRUE) 
  
  males <- round(as.vector(tot)[1])
  females <- round(as.vector(tot)[2])
  
  #svp (~age, ds, level = 0.95)
  df <- bind_rows(df, data_frame(year=y, tot.female=females, tot.male=males, prop=prop.val, prop2.5=prop.val.2.5, prop97.5=prop.val.97.5))
}

df <- df %>% filter(year > 2000)
df
write_csv(df, "../data/cdi-male-female.csv")
