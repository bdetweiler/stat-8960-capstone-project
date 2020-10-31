library('knitr')
library("devtools")
library('MonetDB.R')
#install.packages('MonetDBLite')
library('MonetDBLite')
library('MASS')
library('leaps')
library('glmnet')
library('dbplyr')
library('DBI')
library('ggplot2')
library('scales')
library('ggjoy')
library('ggridges')
library('zoo')
library('TSA')
library('forecast')
library('reshape2')
library('modeest')
library('survey')
library('tidyverse')
library('boot')
library('broom')
library('beepr')
library('Deducer')
library('pROC')
library('lasso2')
library('dplyr')
library('e1071')

set.seed(098114100)

knitr::opts_chunk$set(echo=TRUE, fig.height=5, fig.width=5, fig.align='center', scipen=1, digits=2)
options(max.print=1000000, tibble.print_max = Inf)
#MonetDBLite::monetdblite_shutdown()
con <- DBI::dbConnect(MonetDBLite::MonetDBLite(), "/home/bdetweiler/src/Data_Science/stat-8960-capstone-project/data/nrd_db")
hosp <- DBI::dbGetQuery(con, "SELECT * FROM nrd_hospital")

hosp$hosp_bedsize <- as.integer(hosp$hosp_bedsize)

hosp$h_contrl <- as.integer(hosp$h_contrl)
hosp$hosp_urcat4 <- as.integer(hosp$hosp_urcat4)
hosp$n_disc_u <- as.integer(hosp$n_disc_u)
hosp$n_hosp_u <- as.integer(hosp$n_hosp_u)
hosp$s_disc_u <- as.integer(hosp$s_disc_u)
hosp$s_hosp_u <- as.integer(hosp$s_hosp_u)
hosp$total_disc <- as.integer(hosp$total_disc)

hosp <- hosp %>%
  # Hospital control isn't quantifiable, it's categorical
  mutate(hosp_hcontrl_govt=as.integer(h_contrl == 1)) %>%
  mutate(hosp_hcontrl_priv_np=as.integer(h_contrl == 2)) %>%
  mutate(hosp_hcontrl_priv=as.integer(h_contrl == 3)) %>%
  # Swap the values of urban/rural categorization so that larger numbers == larger areas
  mutate(hosp_urcat4=replace(hosp_urcat4, hosp_urcat4==4, 0)) %>%
  mutate(hosp_urcat4=replace(hosp_urcat4, hosp_urcat4==3, 4)) %>%
  mutate(hosp_urcat4=replace(hosp_urcat4, hosp_urcat4==1, 3)) %>%
  mutate(hosp_urcat4=replace(hosp_urcat4, hosp_urcat4==4, 1)) %>%
  # Teaching isn't quantifiable, it's categorical
  mutate(hosp_ur_teach_metro=as.integer(hosp_ur_teach == 0)) %>%
  mutate(hosp_ur_teach_metro_teaching=as.integer(hosp_ur_teach == 1)) %>%
  mutate(hosp_ur_teach_non_metro=as.integer(hosp_ur_teach == 2)) 






readm.90 <- read_csv('/home/bdetweiler/src/Data_Science/stat-8960-capstone-project/data/cdiff-readmissions-90-day-window.csv')

readm.90 <- merge(readm.90, hosp, by.x=c("cluster", "nrd_year"), by.y=c("hosp_nrd", "nrd_year"))

readm.90 <- readm.90 %>%
  # Hospital control isn't quantifiable, it's categorical
  mutate(pay_NA=as.integer(pay1 < 0)) %>%
  mutate(pay_other=as.integer(pay1 == 6)) %>%
  mutate(pay_nc=as.integer(pay1 == 5)) %>%
  mutate(pay_self=as.integer(pay1 == 4)) %>%
  mutate(pay_priv_ins=as.integer(pay1 == 3)) %>%
  mutate(pay_medicaid=as.integer(pay1 == 2)) %>%
  mutate(pay_medicare=as.integer(pay1 == 1)) %>%
  mutate(acute_kidney_failure=as.integer((dx1 %in% c("584", "5845", "5846", "5847", "5848", "5849")) |
                                           (dx2 %in% c("584", "5845", "5846", "5847", "5848", "5849")) |
                                           (dx3 %in% c("584", "5845", "5846", "5847", "5848", "5849")) |
                                           (dx4 %in% c("584", "5845", "5846", "5847", "5848", "5849")) |
                                           (dx5 %in% c("584", "5845", "5846", "5847", "5848", "5849")) |
                                           (dx6 %in% c("584", "5845", "5846", "5847", "5848", "5849")) |
                                           (dx7 %in% c("584", "5845", "5846", "5847", "5848", "5849")) |
                                           (dx8 %in% c("584", "5845", "5846", "5847", "5848", "5849")) |
                                           (dx9 %in% c("584", "5845", "5846", "5847", "5848", "5849")) |
                                           (dx10 %in% c("584", "5845", "5846", "5847", "5848", "5849")) |
                                           (dx11 %in% c("584", "5845", "5846", "5847", "5848", "5849")) |
                                           (dx12 %in% c("584", "5845", "5846", "5847", "5848", "5849")) |
                                           (dx13 %in% c("584", "5845", "5846", "5847", "5848", "5849")) |
                                           (dx14 %in% c("584", "5845", "5846", "5847", "5848", "5849")) |
                                           (dx15 %in% c("584", "5845", "5846", "5847", "5848", "5849")) |
                                           (dx16 %in% c("584", "5845", "5846", "5847", "5848", "5849")) |
                                           (dx17 %in% c("584", "5845", "5846", "5847", "5848", "5849")) |
                                           (dx18 %in% c("584", "5845", "5846", "5847", "5848", "5849")) |
                                           (dx19 %in% c("584", "5845", "5846", "5847", "5848", "5849")) |
                                           (dx20 %in% c("584", "5845", "5846", "5847", "5848", "5849")) |
                                           (dx21 %in% c("584", "5845", "5846", "5847", "5848", "5849")) |
                                           (dx22 %in% c("584", "5845", "5846", "5847", "5848", "5849")) |
                                           (dx23 %in% c("584", "5845", "5846", "5847", "5848", "5849")) |
                                           (dx24 %in% c("584", "5845", "5846", "5847", "5848", "5849")) |
                                           (dx25 %in% c("584", "5845", "5846", "5847", "5848", "5849")) |
                                           (dx26 %in% c("584", "5845", "5846", "5847", "5848", "5849")) |
                                           (dx27 %in% c("584", "5845", "5846", "5847", "5848", "5849")) |
                                           (dx28 %in% c("584", "5845", "5846", "5847", "5848", "5849")) |
                                           (dx29 %in% c("584", "5845", "5846", "5847", "5848", "5849")) |
                                           (dx30 %in% c("584", "5845", "5846", "5847", "5848", "5849"))))

readm.90 <- readm.90 %>%
  mutate(chronic_kidney_disease1=as.integer((dx1 %in% c("5851")) |
                                              (dx2 %in% c("5851")) |
                                              (dx3 %in% c("5851")) |
                                              (dx4 %in% c("5851")) |
                                              (dx5 %in% c("5851")) |
                                              (dx6 %in% c("5851")) |
                                              (dx7 %in% c("5851")) |
                                              (dx8 %in% c("5851")) |
                                              (dx9 %in% c("5851")) |
                                              (dx10 %in% c("5851")) |
                                              (dx11 %in% c("5851")) |
                                              (dx12 %in% c("5851")) |
                                              (dx13 %in% c("5851")) |
                                              (dx14 %in% c("5851")) |
                                              (dx15 %in% c("5851")) |
                                              (dx16 %in% c("5851")) |
                                              (dx17 %in% c("5851")) |
                                              (dx18 %in% c("5851")) |
                                              (dx19 %in% c("5851")) |
                                              (dx20 %in% c("5851")) |
                                              (dx21 %in% c("5851")) |
                                              (dx22 %in% c("5851")) |
                                              (dx23 %in% c("5851")) |
                                              (dx24 %in% c("5851")) |
                                              (dx25 %in% c("5851")) |
                                              (dx26 %in% c("5851")) |
                                              (dx27 %in% c("5851")) |
                                              (dx28 %in% c("5851")) |
                                              (dx29 %in% c("5851")) |
                                              (dx30 %in% c("5851"))))

readm.90 <- readm.90 %>%
  mutate(chronic_kidney_disease2=as.integer((dx1 %in% c("5852")) |
                                              (dx2 %in% c("5852")) |
                                              (dx3 %in% c("5852")) |
                                              (dx4 %in% c("5852")) |
                                              (dx5 %in% c("5852")) |
                                              (dx6 %in% c("5852")) |
                                              (dx7 %in% c("5852")) |
                                              (dx8 %in% c("5852")) |
                                              (dx9 %in% c("5852")) |
                                              (dx10 %in% c("5852")) |
                                              (dx11 %in% c("5852")) |
                                              (dx12 %in% c("5852")) |
                                              (dx13 %in% c("5852")) |
                                              (dx14 %in% c("5852")) |
                                              (dx15 %in% c("5852")) |
                                              (dx16 %in% c("5852")) |
                                              (dx17 %in% c("5852")) |
                                              (dx18 %in% c("5852")) |
                                              (dx19 %in% c("5852")) |
                                              (dx20 %in% c("5852")) |
                                              (dx21 %in% c("5852")) |
                                              (dx22 %in% c("5852")) |
                                              (dx23 %in% c("5852")) |
                                              (dx24 %in% c("5852")) |
                                              (dx25 %in% c("5852")) |
                                              (dx26 %in% c("5852")) |
                                              (dx27 %in% c("5852")) |
                                              (dx28 %in% c("5852")) |
                                              (dx29 %in% c("5852")) |
                                              (dx30 %in% c("5852"))))

readm.90 <- readm.90 %>%
  mutate(chronic_kidney_disease3=as.integer((dx1 %in% c("5853")) |
                                              (dx2 %in% c("5853")) |
                                              (dx3 %in% c("5853")) |
                                              (dx4 %in% c("5853")) |
                                              (dx5 %in% c("5853")) |
                                              (dx6 %in% c("5853")) |
                                              (dx7 %in% c("5853")) |
                                              (dx8 %in% c("5853")) |
                                              (dx9 %in% c("5853")) |
                                              (dx10 %in% c("5853")) |
                                              (dx11 %in% c("5853")) |
                                              (dx12 %in% c("5853")) |
                                              (dx13 %in% c("5853")) |
                                              (dx14 %in% c("5853")) |
                                              (dx15 %in% c("5853")) |
                                              (dx16 %in% c("5853")) |
                                              (dx17 %in% c("5853")) |
                                              (dx18 %in% c("5853")) |
                                              (dx19 %in% c("5853")) |
                                              (dx20 %in% c("5853")) |
                                              (dx21 %in% c("5853")) |
                                              (dx22 %in% c("5853")) |
                                              (dx23 %in% c("5853")) |
                                              (dx24 %in% c("5853")) |
                                              (dx25 %in% c("5853")) |
                                              (dx26 %in% c("5853")) |
                                              (dx27 %in% c("5853")) |
                                              (dx28 %in% c("5853")) |
                                              (dx29 %in% c("5853")) |
                                              (dx30 %in% c("5853"))))

readm.90 <- readm.90 %>%
  mutate(chronic_kidney_disease4=as.integer((dx1 %in% c("5854")) |
                                              (dx2 %in% c("5854")) |
                                              (dx3 %in% c("5854")) |
                                              (dx4 %in% c("5854")) |
                                              (dx5 %in% c("5854")) |
                                              (dx6 %in% c("5854")) |
                                              (dx7 %in% c("5854")) |
                                              (dx8 %in% c("5854")) |
                                              (dx9 %in% c("5854")) |
                                              (dx10 %in% c("5854")) |
                                              (dx11 %in% c("5854")) |
                                              (dx12 %in% c("5854")) |
                                              (dx13 %in% c("5854")) |
                                              (dx14 %in% c("5854")) |
                                              (dx15 %in% c("5854")) |
                                              (dx16 %in% c("5854")) |
                                              (dx17 %in% c("5854")) |
                                              (dx18 %in% c("5854")) |
                                              (dx19 %in% c("5854")) |
                                              (dx20 %in% c("5854")) |
                                              (dx21 %in% c("5854")) |
                                              (dx22 %in% c("5854")) |
                                              (dx23 %in% c("5854")) |
                                              (dx24 %in% c("5854")) |
                                              (dx25 %in% c("5854")) |
                                              (dx26 %in% c("5854")) |
                                              (dx27 %in% c("5854")) |
                                              (dx28 %in% c("5854")) |
                                              (dx29 %in% c("5854")) |
                                              (dx30 %in% c("5854"))))

readm.90 <- readm.90 %>%
  mutate(chronic_kidney_disease5=as.integer((dx1 %in% c("5855")) |
                                              (dx2 %in% c("5855")) |
                                              (dx3 %in% c("5855")) |
                                              (dx4 %in% c("5855")) |
                                              (dx5 %in% c("5855")) |
                                              (dx6 %in% c("5855")) |
                                              (dx7 %in% c("5855")) |
                                              (dx8 %in% c("5855")) |
                                              (dx9 %in% c("5855")) |
                                              (dx10 %in% c("5855")) |
                                              (dx11 %in% c("5855")) |
                                              (dx12 %in% c("5855")) |
                                              (dx13 %in% c("5855")) |
                                              (dx14 %in% c("5855")) |
                                              (dx15 %in% c("5855")) |
                                              (dx16 %in% c("5855")) |
                                              (dx17 %in% c("5855")) |
                                              (dx18 %in% c("5855")) |
                                              (dx19 %in% c("5855")) |
                                              (dx20 %in% c("5855")) |
                                              (dx21 %in% c("5855")) |
                                              (dx22 %in% c("5855")) |
                                              (dx23 %in% c("5855")) |
                                              (dx24 %in% c("5855")) |
                                              (dx25 %in% c("5855")) |
                                              (dx26 %in% c("5855")) |
                                              (dx27 %in% c("5855")) |
                                              (dx28 %in% c("5855")) |
                                              (dx29 %in% c("5855")) |
                                              (dx30 %in% c("5855"))))

readm.90 <- readm.90 %>%
  mutate(chronic_kidney_disease6=as.integer((dx1 %in% c("5856")) |
                                              (dx2 %in% c("5856")) |
                                              (dx3 %in% c("5856")) |
                                              (dx4 %in% c("5856")) |
                                              (dx5 %in% c("5856")) |
                                              (dx6 %in% c("5856")) |
                                              (dx7 %in% c("5856")) |
                                              (dx8 %in% c("5856")) |
                                              (dx9 %in% c("5856")) |
                                              (dx10 %in% c("5856")) |
                                              (dx11 %in% c("5856")) |
                                              (dx12 %in% c("5856")) |
                                              (dx13 %in% c("5856")) |
                                              (dx14 %in% c("5856")) |
                                              (dx15 %in% c("5856")) |
                                              (dx16 %in% c("5856")) |
                                              (dx17 %in% c("5856")) |
                                              (dx18 %in% c("5856")) |
                                              (dx19 %in% c("5856")) |
                                              (dx20 %in% c("5856")) |
                                              (dx21 %in% c("5856")) |
                                              (dx22 %in% c("5856")) |
                                              (dx23 %in% c("5856")) |
                                              (dx24 %in% c("5856")) |
                                              (dx25 %in% c("5856")) |
                                              (dx26 %in% c("5856")) |
                                              (dx27 %in% c("5856")) |
                                              (dx28 %in% c("5856")) |
                                              (dx29 %in% c("5856")) |
                                              (dx30 %in% c("5856"))))

readm.90 <- readm.90 %>%
  mutate(chronic_kidney_disease_unk=as.integer((dx1 %in% c("5859")) |
                                                 (dx2 %in% c("5859")) |
                                                 (dx3 %in% c("5859")) |
                                                 (dx4 %in% c("5859")) |
                                                 (dx5 %in% c("5859")) |
                                                 (dx6 %in% c("5859")) |
                                                 (dx7 %in% c("5859")) |
                                                 (dx8 %in% c("5859")) |
                                                 (dx9 %in% c("5859")) |
                                                 (dx10 %in% c("5859")) |
                                                 (dx11 %in% c("5859")) |
                                                 (dx12 %in% c("5859")) |
                                                 (dx13 %in% c("5859")) |
                                                 (dx14 %in% c("5859")) |
                                                 (dx15 %in% c("5859")) |
                                                 (dx16 %in% c("5859")) |
                                                 (dx17 %in% c("5859")) |
                                                 (dx18 %in% c("5859")) |
                                                 (dx19 %in% c("5859")) |
                                                 (dx20 %in% c("5859")) |
                                                 (dx21 %in% c("5859")) |
                                                 (dx22 %in% c("5859")) |
                                                 (dx23 %in% c("5859")) |
                                                 (dx24 %in% c("5859")) |
                                                 (dx25 %in% c("5859")) |
                                                 (dx26 %in% c("5859")) |
                                                 (dx27 %in% c("5859")) |
                                                 (dx28 %in% c("5859")) |
                                                 (dx29 %in% c("5859")) |
                                                 (dx30 %in% c("5859"))))

readm.90 <- readm.90 %>%
  mutate(renal_failure_unspecified=as.integer((dx1 %in% c("586")) |
                                                (dx2 %in% c("586")) |
                                                (dx3 %in% c("586")) |
                                                (dx4 %in% c("586")) |
                                                (dx5 %in% c("586")) |
                                                (dx6 %in% c("586")) |
                                                (dx7 %in% c("586")) |
                                                (dx8 %in% c("586")) |
                                                (dx9 %in% c("586")) |
                                                (dx10 %in% c("586")) |
                                                (dx11 %in% c("586")) |
                                                (dx12 %in% c("586")) |
                                                (dx13 %in% c("586")) |
                                                (dx14 %in% c("586")) |
                                                (dx15 %in% c("586")) |
                                                (dx16 %in% c("586")) |
                                                (dx17 %in% c("586")) |
                                                (dx18 %in% c("586")) |
                                                (dx19 %in% c("586")) |
                                                (dx20 %in% c("586")) |
                                                (dx21 %in% c("586")) |
                                                (dx22 %in% c("586")) |
                                                (dx23 %in% c("586")) |
                                                (dx24 %in% c("586")) |
                                                (dx25 %in% c("586")) |
                                                (dx26 %in% c("586")) |
                                                (dx27 %in% c("586")) |
                                                (dx28 %in% c("586")) |
                                                (dx29 %in% c("586")) |
                                                (dx30 %in% c("586"))))


readm.90 <- readm.90 %>%
  mutate(year_2010=as.integer(nrd_year == 2010)) %>%
  mutate(year_2011=as.integer(nrd_year == 2011)) %>%
  mutate(year_2012=as.integer(nrd_year == 2012)) %>%
  mutate(year_2013=as.integer(nrd_year == 2013)) %>%
  mutate(year_2014=as.integer(nrd_year == 2014))

models <- list()

years <- c(2010, 2011, 2012, 2013, 2014)
year <- 2010
for (y in years) {
  readm.by.year <- readm.90 %>% 
    filter(nrd_year == y) %>%
    select(readmitted,
           died,
           cluster, 
           weight,
           stratum,
           age.start,
           hosp_hcontrl_govt,
           hosp_hcontrl_priv_np,
           hosp_urcat4,
           hosp_ur_teach_metro ,
           hosp_ur_teach_metro_teaching ,
           hosp_bedsize,
           female,
           year_2011,
           year_2012,
           year_2013,
           year_2014,
           acute_kidney_failure,
           chronic_kidney_disease2,
           chronic_kidney_disease3,
           chronic_kidney_disease4,
           chronic_kidney_disease5,
           chronic_kidney_disease6,
           chronic_kidney_disease_unk,
           renal_failure_unspecified)
  
  dim(readm.by.year) 
  
  cdiff.design <- svydesign(ids = ~cluster, 
                            data = readm.by.year, 
                            weights = ~weight, 
                            strata = ~stratum)
  
  sw.start <- Sys.time()
  m <- svyglm(readmitted~hosp_hcontrl_govt +
                hosp_hcontrl_priv_np +
                hosp_urcat4 +
                hosp_ur_teach_metro +
                hosp_ur_teach_metro_teaching +
                hosp_bedsize +
                female +
                acute_kidney_failure +
                chronic_kidney_disease2 +
                chronic_kidney_disease3 +
                chronic_kidney_disease4 +
                chronic_kidney_disease5 +
                chronic_kidney_disease6 +
                chronic_kidney_disease_unk +
                renal_failure_unspecified,
              design=as.svrepdesign(cdiff.design), 
              family=quasibinomial, 
              multicore=TRUE,
              return.replicates=TRUE)
  sw.end <- Sys.time()
  print(sw.end - sw.start)
  beep(3) 
  summary(m)
  models[[y]] <- m
}  

saveRDS(models, '../data/90dayreadmissionmodels.rds')






readm.60 <- read_csv('/home/bdetweiler/src/Data_Science/stat-8960-capstone-project/data/cdiff-readmissions-60-day-window.csv')

readm.60 <- merge(readm.60, hosp, by.x=c("cluster", "nrd_year"), by.y=c("hosp_nrd", "nrd_year"))

readm.60 <- readm.60 %>%
  # Hospital control isn't quantifiable, it's categorical
  mutate(pay_NA=as.integer(pay1 < 0)) %>%
  mutate(pay_other=as.integer(pay1 == 6)) %>%
  mutate(pay_nc=as.integer(pay1 == 5)) %>%
  mutate(pay_self=as.integer(pay1 == 4)) %>%
  mutate(pay_priv_ins=as.integer(pay1 == 3)) %>%
  mutate(pay_medicaid=as.integer(pay1 == 2)) %>%
  mutate(pay_medicare=as.integer(pay1 == 1)) %>%
  mutate(acute_kidney_failure=as.integer((dx1 %in% c("584", "5845", "5846", "5847", "5848", "5849")) |
                                           (dx2 %in% c("584", "5845", "5846", "5847", "5848", "5849")) |
                                           (dx3 %in% c("584", "5845", "5846", "5847", "5848", "5849")) |
                                           (dx4 %in% c("584", "5845", "5846", "5847", "5848", "5849")) |
                                           (dx5 %in% c("584", "5845", "5846", "5847", "5848", "5849")) |
                                           (dx6 %in% c("584", "5845", "5846", "5847", "5848", "5849")) |
                                           (dx7 %in% c("584", "5845", "5846", "5847", "5848", "5849")) |
                                           (dx8 %in% c("584", "5845", "5846", "5847", "5848", "5849")) |
                                           (dx9 %in% c("584", "5845", "5846", "5847", "5848", "5849")) |
                                           (dx10 %in% c("584", "5845", "5846", "5847", "5848", "5849")) |
                                           (dx11 %in% c("584", "5845", "5846", "5847", "5848", "5849")) |
                                           (dx12 %in% c("584", "5845", "5846", "5847", "5848", "5849")) |
                                           (dx13 %in% c("584", "5845", "5846", "5847", "5848", "5849")) |
                                           (dx14 %in% c("584", "5845", "5846", "5847", "5848", "5849")) |
                                           (dx15 %in% c("584", "5845", "5846", "5847", "5848", "5849")) |
                                           (dx16 %in% c("584", "5845", "5846", "5847", "5848", "5849")) |
                                           (dx17 %in% c("584", "5845", "5846", "5847", "5848", "5849")) |
                                           (dx18 %in% c("584", "5845", "5846", "5847", "5848", "5849")) |
                                           (dx19 %in% c("584", "5845", "5846", "5847", "5848", "5849")) |
                                           (dx20 %in% c("584", "5845", "5846", "5847", "5848", "5849")) |
                                           (dx21 %in% c("584", "5845", "5846", "5847", "5848", "5849")) |
                                           (dx22 %in% c("584", "5845", "5846", "5847", "5848", "5849")) |
                                           (dx23 %in% c("584", "5845", "5846", "5847", "5848", "5849")) |
                                           (dx24 %in% c("584", "5845", "5846", "5847", "5848", "5849")) |
                                           (dx25 %in% c("584", "5845", "5846", "5847", "5848", "5849")) |
                                           (dx26 %in% c("584", "5845", "5846", "5847", "5848", "5849")) |
                                           (dx27 %in% c("584", "5845", "5846", "5847", "5848", "5849")) |
                                           (dx28 %in% c("584", "5845", "5846", "5847", "5848", "5849")) |
                                           (dx29 %in% c("584", "5845", "5846", "5847", "5848", "5849")) |
                                           (dx30 %in% c("584", "5845", "5846", "5847", "5848", "5849"))))

readm.60 <- readm.60 %>%
  mutate(chronic_kidney_disease1=as.integer((dx1 %in% c("5851")) |
                                              (dx2 %in% c("5851")) |
                                              (dx3 %in% c("5851")) |
                                              (dx4 %in% c("5851")) |
                                              (dx5 %in% c("5851")) |
                                              (dx6 %in% c("5851")) |
                                              (dx7 %in% c("5851")) |
                                              (dx8 %in% c("5851")) |
                                              (dx9 %in% c("5851")) |
                                              (dx10 %in% c("5851")) |
                                              (dx11 %in% c("5851")) |
                                              (dx12 %in% c("5851")) |
                                              (dx13 %in% c("5851")) |
                                              (dx14 %in% c("5851")) |
                                              (dx15 %in% c("5851")) |
                                              (dx16 %in% c("5851")) |
                                              (dx17 %in% c("5851")) |
                                              (dx18 %in% c("5851")) |
                                              (dx19 %in% c("5851")) |
                                              (dx20 %in% c("5851")) |
                                              (dx21 %in% c("5851")) |
                                              (dx22 %in% c("5851")) |
                                              (dx23 %in% c("5851")) |
                                              (dx24 %in% c("5851")) |
                                              (dx25 %in% c("5851")) |
                                              (dx26 %in% c("5851")) |
                                              (dx27 %in% c("5851")) |
                                              (dx28 %in% c("5851")) |
                                              (dx29 %in% c("5851")) |
                                              (dx30 %in% c("5851"))))

readm.60 <- readm.60 %>%
  mutate(chronic_kidney_disease2=as.integer((dx1 %in% c("5852")) |
                                              (dx2 %in% c("5852")) |
                                              (dx3 %in% c("5852")) |
                                              (dx4 %in% c("5852")) |
                                              (dx5 %in% c("5852")) |
                                              (dx6 %in% c("5852")) |
                                              (dx7 %in% c("5852")) |
                                              (dx8 %in% c("5852")) |
                                              (dx9 %in% c("5852")) |
                                              (dx10 %in% c("5852")) |
                                              (dx11 %in% c("5852")) |
                                              (dx12 %in% c("5852")) |
                                              (dx13 %in% c("5852")) |
                                              (dx14 %in% c("5852")) |
                                              (dx15 %in% c("5852")) |
                                              (dx16 %in% c("5852")) |
                                              (dx17 %in% c("5852")) |
                                              (dx18 %in% c("5852")) |
                                              (dx19 %in% c("5852")) |
                                              (dx20 %in% c("5852")) |
                                              (dx21 %in% c("5852")) |
                                              (dx22 %in% c("5852")) |
                                              (dx23 %in% c("5852")) |
                                              (dx24 %in% c("5852")) |
                                              (dx25 %in% c("5852")) |
                                              (dx26 %in% c("5852")) |
                                              (dx27 %in% c("5852")) |
                                              (dx28 %in% c("5852")) |
                                              (dx29 %in% c("5852")) |
                                              (dx30 %in% c("5852"))))

readm.60 <- readm.60 %>%
  mutate(chronic_kidney_disease3=as.integer((dx1 %in% c("5853")) |
                                              (dx2 %in% c("5853")) |
                                              (dx3 %in% c("5853")) |
                                              (dx4 %in% c("5853")) |
                                              (dx5 %in% c("5853")) |
                                              (dx6 %in% c("5853")) |
                                              (dx7 %in% c("5853")) |
                                              (dx8 %in% c("5853")) |
                                              (dx9 %in% c("5853")) |
                                              (dx10 %in% c("5853")) |
                                              (dx11 %in% c("5853")) |
                                              (dx12 %in% c("5853")) |
                                              (dx13 %in% c("5853")) |
                                              (dx14 %in% c("5853")) |
                                              (dx15 %in% c("5853")) |
                                              (dx16 %in% c("5853")) |
                                              (dx17 %in% c("5853")) |
                                              (dx18 %in% c("5853")) |
                                              (dx19 %in% c("5853")) |
                                              (dx20 %in% c("5853")) |
                                              (dx21 %in% c("5853")) |
                                              (dx22 %in% c("5853")) |
                                              (dx23 %in% c("5853")) |
                                              (dx24 %in% c("5853")) |
                                              (dx25 %in% c("5853")) |
                                              (dx26 %in% c("5853")) |
                                              (dx27 %in% c("5853")) |
                                              (dx28 %in% c("5853")) |
                                              (dx29 %in% c("5853")) |
                                              (dx30 %in% c("5853"))))

readm.60 <- readm.60 %>%
  mutate(chronic_kidney_disease4=as.integer((dx1 %in% c("5854")) |
                                              (dx2 %in% c("5854")) |
                                              (dx3 %in% c("5854")) |
                                              (dx4 %in% c("5854")) |
                                              (dx5 %in% c("5854")) |
                                              (dx6 %in% c("5854")) |
                                              (dx7 %in% c("5854")) |
                                              (dx8 %in% c("5854")) |
                                              (dx9 %in% c("5854")) |
                                              (dx10 %in% c("5854")) |
                                              (dx11 %in% c("5854")) |
                                              (dx12 %in% c("5854")) |
                                              (dx13 %in% c("5854")) |
                                              (dx14 %in% c("5854")) |
                                              (dx15 %in% c("5854")) |
                                              (dx16 %in% c("5854")) |
                                              (dx17 %in% c("5854")) |
                                              (dx18 %in% c("5854")) |
                                              (dx19 %in% c("5854")) |
                                              (dx20 %in% c("5854")) |
                                              (dx21 %in% c("5854")) |
                                              (dx22 %in% c("5854")) |
                                              (dx23 %in% c("5854")) |
                                              (dx24 %in% c("5854")) |
                                              (dx25 %in% c("5854")) |
                                              (dx26 %in% c("5854")) |
                                              (dx27 %in% c("5854")) |
                                              (dx28 %in% c("5854")) |
                                              (dx29 %in% c("5854")) |
                                              (dx30 %in% c("5854"))))

readm.60 <- readm.60 %>%
  mutate(chronic_kidney_disease5=as.integer((dx1 %in% c("5855")) |
                                              (dx2 %in% c("5855")) |
                                              (dx3 %in% c("5855")) |
                                              (dx4 %in% c("5855")) |
                                              (dx5 %in% c("5855")) |
                                              (dx6 %in% c("5855")) |
                                              (dx7 %in% c("5855")) |
                                              (dx8 %in% c("5855")) |
                                              (dx9 %in% c("5855")) |
                                              (dx10 %in% c("5855")) |
                                              (dx11 %in% c("5855")) |
                                              (dx12 %in% c("5855")) |
                                              (dx13 %in% c("5855")) |
                                              (dx14 %in% c("5855")) |
                                              (dx15 %in% c("5855")) |
                                              (dx16 %in% c("5855")) |
                                              (dx17 %in% c("5855")) |
                                              (dx18 %in% c("5855")) |
                                              (dx19 %in% c("5855")) |
                                              (dx20 %in% c("5855")) |
                                              (dx21 %in% c("5855")) |
                                              (dx22 %in% c("5855")) |
                                              (dx23 %in% c("5855")) |
                                              (dx24 %in% c("5855")) |
                                              (dx25 %in% c("5855")) |
                                              (dx26 %in% c("5855")) |
                                              (dx27 %in% c("5855")) |
                                              (dx28 %in% c("5855")) |
                                              (dx29 %in% c("5855")) |
                                              (dx30 %in% c("5855"))))

readm.60 <- readm.60 %>%
  mutate(chronic_kidney_disease6=as.integer((dx1 %in% c("5856")) |
                                              (dx2 %in% c("5856")) |
                                              (dx3 %in% c("5856")) |
                                              (dx4 %in% c("5856")) |
                                              (dx5 %in% c("5856")) |
                                              (dx6 %in% c("5856")) |
                                              (dx7 %in% c("5856")) |
                                              (dx8 %in% c("5856")) |
                                              (dx9 %in% c("5856")) |
                                              (dx10 %in% c("5856")) |
                                              (dx11 %in% c("5856")) |
                                              (dx12 %in% c("5856")) |
                                              (dx13 %in% c("5856")) |
                                              (dx14 %in% c("5856")) |
                                              (dx15 %in% c("5856")) |
                                              (dx16 %in% c("5856")) |
                                              (dx17 %in% c("5856")) |
                                              (dx18 %in% c("5856")) |
                                              (dx19 %in% c("5856")) |
                                              (dx20 %in% c("5856")) |
                                              (dx21 %in% c("5856")) |
                                              (dx22 %in% c("5856")) |
                                              (dx23 %in% c("5856")) |
                                              (dx24 %in% c("5856")) |
                                              (dx25 %in% c("5856")) |
                                              (dx26 %in% c("5856")) |
                                              (dx27 %in% c("5856")) |
                                              (dx28 %in% c("5856")) |
                                              (dx29 %in% c("5856")) |
                                              (dx30 %in% c("5856"))))

readm.60 <- readm.60 %>%
  mutate(chronic_kidney_disease_unk=as.integer((dx1 %in% c("5859")) |
                                                 (dx2 %in% c("5859")) |
                                                 (dx3 %in% c("5859")) |
                                                 (dx4 %in% c("5859")) |
                                                 (dx5 %in% c("5859")) |
                                                 (dx6 %in% c("5859")) |
                                                 (dx7 %in% c("5859")) |
                                                 (dx8 %in% c("5859")) |
                                                 (dx9 %in% c("5859")) |
                                                 (dx10 %in% c("5859")) |
                                                 (dx11 %in% c("5859")) |
                                                 (dx12 %in% c("5859")) |
                                                 (dx13 %in% c("5859")) |
                                                 (dx14 %in% c("5859")) |
                                                 (dx15 %in% c("5859")) |
                                                 (dx16 %in% c("5859")) |
                                                 (dx17 %in% c("5859")) |
                                                 (dx18 %in% c("5859")) |
                                                 (dx19 %in% c("5859")) |
                                                 (dx20 %in% c("5859")) |
                                                 (dx21 %in% c("5859")) |
                                                 (dx22 %in% c("5859")) |
                                                 (dx23 %in% c("5859")) |
                                                 (dx24 %in% c("5859")) |
                                                 (dx25 %in% c("5859")) |
                                                 (dx26 %in% c("5859")) |
                                                 (dx27 %in% c("5859")) |
                                                 (dx28 %in% c("5859")) |
                                                 (dx29 %in% c("5859")) |
                                                 (dx30 %in% c("5859"))))

readm.60 <- readm.60 %>%
  mutate(renal_failure_unspecified=as.integer((dx1 %in% c("586")) |
                                                (dx2 %in% c("586")) |
                                                (dx3 %in% c("586")) |
                                                (dx4 %in% c("586")) |
                                                (dx5 %in% c("586")) |
                                                (dx6 %in% c("586")) |
                                                (dx7 %in% c("586")) |
                                                (dx8 %in% c("586")) |
                                                (dx9 %in% c("586")) |
                                                (dx10 %in% c("586")) |
                                                (dx11 %in% c("586")) |
                                                (dx12 %in% c("586")) |
                                                (dx13 %in% c("586")) |
                                                (dx14 %in% c("586")) |
                                                (dx15 %in% c("586")) |
                                                (dx16 %in% c("586")) |
                                                (dx17 %in% c("586")) |
                                                (dx18 %in% c("586")) |
                                                (dx19 %in% c("586")) |
                                                (dx20 %in% c("586")) |
                                                (dx21 %in% c("586")) |
                                                (dx22 %in% c("586")) |
                                                (dx23 %in% c("586")) |
                                                (dx24 %in% c("586")) |
                                                (dx25 %in% c("586")) |
                                                (dx26 %in% c("586")) |
                                                (dx27 %in% c("586")) |
                                                (dx28 %in% c("586")) |
                                                (dx29 %in% c("586")) |
                                                (dx30 %in% c("586"))))


readm.60 <- readm.60 %>%
  mutate(year_2010=as.integer(nrd_year == 2010)) %>%
  mutate(year_2011=as.integer(nrd_year == 2011)) %>%
  mutate(year_2012=as.integer(nrd_year == 2012)) %>%
  mutate(year_2013=as.integer(nrd_year == 2013)) %>%
  mutate(year_2014=as.integer(nrd_year == 2014))

models <- list()

years <- c(2010, 2011, 2012, 2013, 2014)
year <- 2010
for (y in years) {
  readm.by.year <- readm.60 %>% 
    filter(nrd_year == y) %>%
    select(readmitted,
           died,
           cluster, 
           weight,
           stratum,
           age.start,
           hosp_hcontrl_govt,
           hosp_hcontrl_priv_np,
           hosp_urcat4,
           hosp_ur_teach_metro ,
           hosp_ur_teach_metro_teaching ,
           hosp_bedsize,
           female,
           year_2011,
           year_2012,
           year_2013,
           year_2014,
           acute_kidney_failure,
           chronic_kidney_disease2,
           chronic_kidney_disease3,
           chronic_kidney_disease4,
           chronic_kidney_disease5,
           chronic_kidney_disease6,
           chronic_kidney_disease_unk,
           renal_failure_unspecified)
  
  dim(readm.by.year) 
  
  cdiff.design <- svydesign(ids = ~cluster, 
                            data = readm.by.year, 
                            weights = ~weight, 
                            strata = ~stratum)
  
  sw.start <- Sys.time()
  m <- svyglm(readmitted~hosp_hcontrl_govt +
                hosp_hcontrl_priv_np +
                hosp_urcat4 +
                hosp_ur_teach_metro +
                hosp_ur_teach_metro_teaching +
                hosp_bedsize +
                female +
                acute_kidney_failure +
                chronic_kidney_disease2 +
                chronic_kidney_disease3 +
                chronic_kidney_disease4 +
                chronic_kidney_disease5 +
                chronic_kidney_disease6 +
                chronic_kidney_disease_unk +
                renal_failure_unspecified,
              design=as.svrepdesign(cdiff.design), 
              family=quasibinomial, 
              multicore=TRUE,
              return.replicates=TRUE)
  sw.end <- Sys.time()
  print(sw.end - sw.start)
  beep(3) 
  summary(m)
  models[[y]] <- m
}  

saveRDS(models, '../data/60dayreadmissionmodels.rds')






readm.30<- read_csv('/home/bdetweiler/src/Data_Science/stat-8960-capstone-project/data/cdiff-readmissions-30-day-window.csv')

readm.30 <- merge(readm.30, hosp, by.x=c("cluster", "nrd_year"), by.y=c("hosp_nrd", "nrd_year"))

readm.30 <- readm.30 %>%
  # Hospital control isn't quantifiable, it's categorical
  mutate(pay_NA=as.integer(pay1 < 0)) %>%
  mutate(pay_other=as.integer(pay1 == 6)) %>%
  mutate(pay_nc=as.integer(pay1 == 5)) %>%
  mutate(pay_self=as.integer(pay1 == 4)) %>%
  mutate(pay_priv_ins=as.integer(pay1 == 3)) %>%
  mutate(pay_medicaid=as.integer(pay1 == 2)) %>%
  mutate(pay_medicare=as.integer(pay1 == 1)) %>%
  mutate(acute_kidney_failure=as.integer((dx1 %in% c("584", "5845", "5846", "5847", "5848", "5849")) |
                                           (dx2 %in% c("584", "5845", "5846", "5847", "5848", "5849")) |
                                           (dx3 %in% c("584", "5845", "5846", "5847", "5848", "5849")) |
                                           (dx4 %in% c("584", "5845", "5846", "5847", "5848", "5849")) |
                                           (dx5 %in% c("584", "5845", "5846", "5847", "5848", "5849")) |
                                           (dx6 %in% c("584", "5845", "5846", "5847", "5848", "5849")) |
                                           (dx7 %in% c("584", "5845", "5846", "5847", "5848", "5849")) |
                                           (dx8 %in% c("584", "5845", "5846", "5847", "5848", "5849")) |
                                           (dx9 %in% c("584", "5845", "5846", "5847", "5848", "5849")) |
                                           (dx10 %in% c("584", "5845", "5846", "5847", "5848", "5849")) |
                                           (dx11 %in% c("584", "5845", "5846", "5847", "5848", "5849")) |
                                           (dx12 %in% c("584", "5845", "5846", "5847", "5848", "5849")) |
                                           (dx13 %in% c("584", "5845", "5846", "5847", "5848", "5849")) |
                                           (dx14 %in% c("584", "5845", "5846", "5847", "5848", "5849")) |
                                           (dx15 %in% c("584", "5845", "5846", "5847", "5848", "5849")) |
                                           (dx16 %in% c("584", "5845", "5846", "5847", "5848", "5849")) |
                                           (dx17 %in% c("584", "5845", "5846", "5847", "5848", "5849")) |
                                           (dx18 %in% c("584", "5845", "5846", "5847", "5848", "5849")) |
                                           (dx19 %in% c("584", "5845", "5846", "5847", "5848", "5849")) |
                                           (dx20 %in% c("584", "5845", "5846", "5847", "5848", "5849")) |
                                           (dx21 %in% c("584", "5845", "5846", "5847", "5848", "5849")) |
                                           (dx22 %in% c("584", "5845", "5846", "5847", "5848", "5849")) |
                                           (dx23 %in% c("584", "5845", "5846", "5847", "5848", "5849")) |
                                           (dx24 %in% c("584", "5845", "5846", "5847", "5848", "5849")) |
                                           (dx25 %in% c("584", "5845", "5846", "5847", "5848", "5849")) |
                                           (dx26 %in% c("584", "5845", "5846", "5847", "5848", "5849")) |
                                           (dx27 %in% c("584", "5845", "5846", "5847", "5848", "5849")) |
                                           (dx28 %in% c("584", "5845", "5846", "5847", "5848", "5849")) |
                                           (dx29 %in% c("584", "5845", "5846", "5847", "5848", "5849")) |
                                           (dx30 %in% c("584", "5845", "5846", "5847", "5848", "5849"))))

readm.30 <- readm.30 %>%
  mutate(chronic_kidney_disease1=as.integer((dx1 %in% c("5851")) |
                                              (dx2 %in% c("5851")) |
                                              (dx3 %in% c("5851")) |
                                              (dx4 %in% c("5851")) |
                                              (dx5 %in% c("5851")) |
                                              (dx6 %in% c("5851")) |
                                              (dx7 %in% c("5851")) |
                                              (dx8 %in% c("5851")) |
                                              (dx9 %in% c("5851")) |
                                              (dx10 %in% c("5851")) |
                                              (dx11 %in% c("5851")) |
                                              (dx12 %in% c("5851")) |
                                              (dx13 %in% c("5851")) |
                                              (dx14 %in% c("5851")) |
                                              (dx15 %in% c("5851")) |
                                              (dx16 %in% c("5851")) |
                                              (dx17 %in% c("5851")) |
                                              (dx18 %in% c("5851")) |
                                              (dx19 %in% c("5851")) |
                                              (dx20 %in% c("5851")) |
                                              (dx21 %in% c("5851")) |
                                              (dx22 %in% c("5851")) |
                                              (dx23 %in% c("5851")) |
                                              (dx24 %in% c("5851")) |
                                              (dx25 %in% c("5851")) |
                                              (dx26 %in% c("5851")) |
                                              (dx27 %in% c("5851")) |
                                              (dx28 %in% c("5851")) |
                                              (dx29 %in% c("5851")) |
                                              (dx30 %in% c("5851"))))

readm.30 <- readm.30 %>%
  mutate(chronic_kidney_disease2=as.integer((dx1 %in% c("5852")) |
                                              (dx2 %in% c("5852")) |
                                              (dx3 %in% c("5852")) |
                                              (dx4 %in% c("5852")) |
                                              (dx5 %in% c("5852")) |
                                              (dx6 %in% c("5852")) |
                                              (dx7 %in% c("5852")) |
                                              (dx8 %in% c("5852")) |
                                              (dx9 %in% c("5852")) |
                                              (dx10 %in% c("5852")) |
                                              (dx11 %in% c("5852")) |
                                              (dx12 %in% c("5852")) |
                                              (dx13 %in% c("5852")) |
                                              (dx14 %in% c("5852")) |
                                              (dx15 %in% c("5852")) |
                                              (dx16 %in% c("5852")) |
                                              (dx17 %in% c("5852")) |
                                              (dx18 %in% c("5852")) |
                                              (dx19 %in% c("5852")) |
                                              (dx20 %in% c("5852")) |
                                              (dx21 %in% c("5852")) |
                                              (dx22 %in% c("5852")) |
                                              (dx23 %in% c("5852")) |
                                              (dx24 %in% c("5852")) |
                                              (dx25 %in% c("5852")) |
                                              (dx26 %in% c("5852")) |
                                              (dx27 %in% c("5852")) |
                                              (dx28 %in% c("5852")) |
                                              (dx29 %in% c("5852")) |
                                              (dx30 %in% c("5852"))))

readm.30 <- readm.30 %>%
  mutate(chronic_kidney_disease3=as.integer((dx1 %in% c("5853")) |
                                              (dx2 %in% c("5853")) |
                                              (dx3 %in% c("5853")) |
                                              (dx4 %in% c("5853")) |
                                              (dx5 %in% c("5853")) |
                                              (dx6 %in% c("5853")) |
                                              (dx7 %in% c("5853")) |
                                              (dx8 %in% c("5853")) |
                                              (dx9 %in% c("5853")) |
                                              (dx10 %in% c("5853")) |
                                              (dx11 %in% c("5853")) |
                                              (dx12 %in% c("5853")) |
                                              (dx13 %in% c("5853")) |
                                              (dx14 %in% c("5853")) |
                                              (dx15 %in% c("5853")) |
                                              (dx16 %in% c("5853")) |
                                              (dx17 %in% c("5853")) |
                                              (dx18 %in% c("5853")) |
                                              (dx19 %in% c("5853")) |
                                              (dx20 %in% c("5853")) |
                                              (dx21 %in% c("5853")) |
                                              (dx22 %in% c("5853")) |
                                              (dx23 %in% c("5853")) |
                                              (dx24 %in% c("5853")) |
                                              (dx25 %in% c("5853")) |
                                              (dx26 %in% c("5853")) |
                                              (dx27 %in% c("5853")) |
                                              (dx28 %in% c("5853")) |
                                              (dx29 %in% c("5853")) |
                                              (dx30 %in% c("5853"))))

readm.30 <- readm.30 %>%
  mutate(chronic_kidney_disease4=as.integer((dx1 %in% c("5854")) |
                                              (dx2 %in% c("5854")) |
                                              (dx3 %in% c("5854")) |
                                              (dx4 %in% c("5854")) |
                                              (dx5 %in% c("5854")) |
                                              (dx6 %in% c("5854")) |
                                              (dx7 %in% c("5854")) |
                                              (dx8 %in% c("5854")) |
                                              (dx9 %in% c("5854")) |
                                              (dx10 %in% c("5854")) |
                                              (dx11 %in% c("5854")) |
                                              (dx12 %in% c("5854")) |
                                              (dx13 %in% c("5854")) |
                                              (dx14 %in% c("5854")) |
                                              (dx15 %in% c("5854")) |
                                              (dx16 %in% c("5854")) |
                                              (dx17 %in% c("5854")) |
                                              (dx18 %in% c("5854")) |
                                              (dx19 %in% c("5854")) |
                                              (dx20 %in% c("5854")) |
                                              (dx21 %in% c("5854")) |
                                              (dx22 %in% c("5854")) |
                                              (dx23 %in% c("5854")) |
                                              (dx24 %in% c("5854")) |
                                              (dx25 %in% c("5854")) |
                                              (dx26 %in% c("5854")) |
                                              (dx27 %in% c("5854")) |
                                              (dx28 %in% c("5854")) |
                                              (dx29 %in% c("5854")) |
                                              (dx30 %in% c("5854"))))

readm.30 <- readm.30 %>%
  mutate(chronic_kidney_disease5=as.integer((dx1 %in% c("5855")) |
                                              (dx2 %in% c("5855")) |
                                              (dx3 %in% c("5855")) |
                                              (dx4 %in% c("5855")) |
                                              (dx5 %in% c("5855")) |
                                              (dx6 %in% c("5855")) |
                                              (dx7 %in% c("5855")) |
                                              (dx8 %in% c("5855")) |
                                              (dx9 %in% c("5855")) |
                                              (dx10 %in% c("5855")) |
                                              (dx11 %in% c("5855")) |
                                              (dx12 %in% c("5855")) |
                                              (dx13 %in% c("5855")) |
                                              (dx14 %in% c("5855")) |
                                              (dx15 %in% c("5855")) |
                                              (dx16 %in% c("5855")) |
                                              (dx17 %in% c("5855")) |
                                              (dx18 %in% c("5855")) |
                                              (dx19 %in% c("5855")) |
                                              (dx20 %in% c("5855")) |
                                              (dx21 %in% c("5855")) |
                                              (dx22 %in% c("5855")) |
                                              (dx23 %in% c("5855")) |
                                              (dx24 %in% c("5855")) |
                                              (dx25 %in% c("5855")) |
                                              (dx26 %in% c("5855")) |
                                              (dx27 %in% c("5855")) |
                                              (dx28 %in% c("5855")) |
                                              (dx29 %in% c("5855")) |
                                              (dx30 %in% c("5855"))))

readm.30 <- readm.30 %>%
  mutate(chronic_kidney_disease6=as.integer((dx1 %in% c("5856")) |
                                              (dx2 %in% c("5856")) |
                                              (dx3 %in% c("5856")) |
                                              (dx4 %in% c("5856")) |
                                              (dx5 %in% c("5856")) |
                                              (dx6 %in% c("5856")) |
                                              (dx7 %in% c("5856")) |
                                              (dx8 %in% c("5856")) |
                                              (dx9 %in% c("5856")) |
                                              (dx10 %in% c("5856")) |
                                              (dx11 %in% c("5856")) |
                                              (dx12 %in% c("5856")) |
                                              (dx13 %in% c("5856")) |
                                              (dx14 %in% c("5856")) |
                                              (dx15 %in% c("5856")) |
                                              (dx16 %in% c("5856")) |
                                              (dx17 %in% c("5856")) |
                                              (dx18 %in% c("5856")) |
                                              (dx19 %in% c("5856")) |
                                              (dx20 %in% c("5856")) |
                                              (dx21 %in% c("5856")) |
                                              (dx22 %in% c("5856")) |
                                              (dx23 %in% c("5856")) |
                                              (dx24 %in% c("5856")) |
                                              (dx25 %in% c("5856")) |
                                              (dx26 %in% c("5856")) |
                                              (dx27 %in% c("5856")) |
                                              (dx28 %in% c("5856")) |
                                              (dx29 %in% c("5856")) |
                                              (dx30 %in% c("5856"))))

readm.30 <- readm.30 %>%
  mutate(chronic_kidney_disease_unk=as.integer((dx1 %in% c("5859")) |
                                                 (dx2 %in% c("5859")) |
                                                 (dx3 %in% c("5859")) |
                                                 (dx4 %in% c("5859")) |
                                                 (dx5 %in% c("5859")) |
                                                 (dx6 %in% c("5859")) |
                                                 (dx7 %in% c("5859")) |
                                                 (dx8 %in% c("5859")) |
                                                 (dx9 %in% c("5859")) |
                                                 (dx10 %in% c("5859")) |
                                                 (dx11 %in% c("5859")) |
                                                 (dx12 %in% c("5859")) |
                                                 (dx13 %in% c("5859")) |
                                                 (dx14 %in% c("5859")) |
                                                 (dx15 %in% c("5859")) |
                                                 (dx16 %in% c("5859")) |
                                                 (dx17 %in% c("5859")) |
                                                 (dx18 %in% c("5859")) |
                                                 (dx19 %in% c("5859")) |
                                                 (dx20 %in% c("5859")) |
                                                 (dx21 %in% c("5859")) |
                                                 (dx22 %in% c("5859")) |
                                                 (dx23 %in% c("5859")) |
                                                 (dx24 %in% c("5859")) |
                                                 (dx25 %in% c("5859")) |
                                                 (dx26 %in% c("5859")) |
                                                 (dx27 %in% c("5859")) |
                                                 (dx28 %in% c("5859")) |
                                                 (dx29 %in% c("5859")) |
                                                 (dx30 %in% c("5859"))))

readm.30 <- readm.30 %>%
  mutate(renal_failure_unspecified=as.integer((dx1 %in% c("586")) |
                                                (dx2 %in% c("586")) |
                                                (dx3 %in% c("586")) |
                                                (dx4 %in% c("586")) |
                                                (dx5 %in% c("586")) |
                                                (dx6 %in% c("586")) |
                                                (dx7 %in% c("586")) |
                                                (dx8 %in% c("586")) |
                                                (dx9 %in% c("586")) |
                                                (dx10 %in% c("586")) |
                                                (dx11 %in% c("586")) |
                                                (dx12 %in% c("586")) |
                                                (dx13 %in% c("586")) |
                                                (dx14 %in% c("586")) |
                                                (dx15 %in% c("586")) |
                                                (dx16 %in% c("586")) |
                                                (dx17 %in% c("586")) |
                                                (dx18 %in% c("586")) |
                                                (dx19 %in% c("586")) |
                                                (dx20 %in% c("586")) |
                                                (dx21 %in% c("586")) |
                                                (dx22 %in% c("586")) |
                                                (dx23 %in% c("586")) |
                                                (dx24 %in% c("586")) |
                                                (dx25 %in% c("586")) |
                                                (dx26 %in% c("586")) |
                                                (dx27 %in% c("586")) |
                                                (dx28 %in% c("586")) |
                                                (dx29 %in% c("586")) |
                                                (dx30 %in% c("586"))))


readm.30 <- readm.30 %>%
  mutate(year_2010=as.integer(nrd_year == 2010)) %>%
  mutate(year_2011=as.integer(nrd_year == 2011)) %>%
  mutate(year_2012=as.integer(nrd_year == 2012)) %>%
  mutate(year_2013=as.integer(nrd_year == 2013)) %>%
  mutate(year_2014=as.integer(nrd_year == 2014))

models <- list()

years <- c(2010, 2011, 2012, 2013, 2014)
year <- 2010
for (y in years) {
  readm.by.year <- readm.30 %>% 
    filter(nrd_year == y) %>%
    select(readmitted,
           died,
           cluster, 
           weight,
           stratum,
           age.start,
           hosp_hcontrl_govt,
           hosp_hcontrl_priv_np,
           hosp_urcat4,
           hosp_ur_teach_metro ,
           hosp_ur_teach_metro_teaching ,
           hosp_bedsize,
           female,
           year_2011,
           year_2012,
           year_2013,
           year_2014,
           acute_kidney_failure,
           chronic_kidney_disease2,
           chronic_kidney_disease3,
           chronic_kidney_disease4,
           chronic_kidney_disease5,
           chronic_kidney_disease6,
           chronic_kidney_disease_unk,
           renal_failure_unspecified)
  
  dim(readm.by.year) 
  
  cdiff.design <- svydesign(ids = ~cluster, 
                            data = readm.by.year, 
                            weights = ~weight, 
                            strata = ~stratum)
  
  sw.start <- Sys.time()
  m <- svyglm(readmitted~hosp_hcontrl_govt +
                hosp_hcontrl_priv_np +
                hosp_urcat4 +
                hosp_ur_teach_metro +
                hosp_ur_teach_metro_teaching +
                hosp_bedsize +
                female +
                acute_kidney_failure +
                chronic_kidney_disease2 +
                chronic_kidney_disease3 +
                chronic_kidney_disease4 +
                chronic_kidney_disease5 +
                chronic_kidney_disease6 +
                chronic_kidney_disease_unk +
                renal_failure_unspecified,
              design=as.svrepdesign(cdiff.design), 
              family=quasibinomial, 
              multicore=TRUE,
              return.replicates=TRUE)
  sw.end <- Sys.time()
  print(sw.end - sw.start)
  beep(3) 
  summary(m)
  models[[y]] <- m
}  

saveRDS(models, '../data/30dayreadmissionmodels.rds')


### TODO: Need to do this by year

readm.30 <- read_csv('/home/bdetweiler/src/Data_Science/stat-8960-capstone-project/data/cdiff-readmissions-30-day-window.csv')
readm.60 <- read_csv('/home/bdetweiler/src/Data_Science/stat-8960-capstone-project/data/cdiff-readmissions-60-day-window.csv')
readm.90 <- read_csv('/home/bdetweiler/src/Data_Science/stat-8960-capstone-project/data/cdiff-readmissions-90-day-window.csv')

cdiff.design.30 <- svydesign(ids = ~cluster, 
                             data = readm.30, 
                             weights = ~weight, 
                             strata = ~stratum)

cdiff.design.60 <- svydesign(ids = ~cluster, 
                             data = readm.60, 
                             weights = ~weight, 
                             strata = ~stratum)

cdiff.design.90 <- svydesign(ids = ~cluster, 
                             data = readm.90, 
                             weights = ~weight, 
                             strata = ~stratum)


m.30 <- svyglm(readmitted~age.start +
                 cm_renlfail, 
               design=as.svrepdesign(cdiff.design.30), 
               family=quasibinomial, 
               multicore=TRUE,
               return.replicates=TRUE)
beep(3)
m.60 <- svyglm(readmitted~age.start +
                 cm_renlfail, 
               design=as.svrepdesign(cdiff.design.60), 
               family=quasibinomial, 
               multicore=TRUE,
               return.replicates=TRUE)
beep(3)

m.90 <- svyglm(readmitted~age.start +
                 cm_renlfail, 
               design=as.svrepdesign(cdiff.design.90), 
               family=quasibinomial, 
               multicore=TRUE,
               return.replicates=TRUE)

beep(3)
readm.90 %>% 
  ggplot(aes(age.start + cm_renlfail, readmitted)) +
  geom_bin2d(bins = 30) +
  geom_smooth(method = "glm", method.args = list(family = "binomial")) +
  #geom_point(data = pred.df, aes(x=Age, y=Prob)) +
  labs(title="Logistic Regression of 90-Day Readmission by Age", x="Age at last admission", y="Readmitted (1 = Yes, 0 = No)")

readm.60 %>% 
  ggplot(aes(age.start + cm_renlfail, readmitted)) +
  geom_bin2d(bins = 30) +
  geom_smooth(method = "glm", method.args = list(family = "binomial")) +
  #geom_point(data = pred.df, aes(x=Age, y=Prob)) +
  labs(title="Logistic Regression of 60-Day Readmission by Age", x="Age at last admission", y="Readmitted (1 = Yes, 0 = No)")

readm.30 %>% 
  ggplot(aes(age.start + cm_renlfail, readmitted)) +
  geom_bin2d(bins = 30) +
  geom_smooth(method = "glm", method.args = list(family = "binomial")) +
  #geom_point(data = pred.df, aes(x=Age, y=Prob)) +
  labs(title="Logistic Regression of 30-Day Readmission by Age", x="Age at last admission", y="Readmitted (1 = Yes, 0 = No)")
summary(m.30)
summary(m.60)
summary(m.90)
renal.fail.models <- list()
renal.fail.models[30] <- m.30
renal.fail.models[60] <- m.60
renal.fail.models[90] <- m.90
writeRDS(renal.fail.models, '../data/renal-fail-models.rds')
