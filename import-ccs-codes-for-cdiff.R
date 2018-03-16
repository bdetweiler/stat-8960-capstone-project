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

# Get the C. diff file with all the DX and PR descriptions
cdiff.nrd <- read_csv('data/cdiff-nrd-desc.csv')
cdiff.nrd$key_nrd <- as.character(cdiff.nrd$key_nrd)

ccs <- read_csv('data/nis-ccs-codes.csv')
fmt.ccs.code <- ccs %>% filter(Code == 'G0455') %>% pull(CCS)

#fmt.ccs.code
#hist(cdiff.nrd %>% filter(dx1 == '00845', prccs1 == fmt.ccs.code) %>% pull(nrd_year))
#write_csv(cdiff.nrd %>% filter(dx1 == '00845'), 'data/test.csv')






#cdiff.ccs.codes <- ccs %>% filter(CCS == fmt.ccs.code) %>% arrange(Code) 

#cdiff.ccs.codes 

#hcpcs.codes <- cdiff.ccs.codes$Code
#hcpcs.desc <- c()
# "0288T" https://www.cms.gov/Regulations-and-Guidance/Guidance/Transmittals/downloads/R2378CP.pfd  Effective Q1 2012
#hcpcs.desc <- c(hcpcs.desc, "Anoscopy w/rf delivery")
# "0377T" https://med.noridianmedicare.com/web/jea/policies/coverage-articles/injectable-bulking-agents-for-the-treatment-of-fecal-incontinence Effective Q1 2015
#hcpcs.desc <- c(hcpcs.desc, "Anoscopy with directed submucosal injection of bulking agent for fecal incontinence")
# "44705" https://www.cgsmedicare.com/partb/pubs/news/2015/0215/cope28449.html Effective Q1 2015
#hcpcs.desc <- c(hcpcs.desc, "Preparation of fecal microbiota for instillation, including assessment of donor specimen")
# "45190" 
#hcpcs.desc <- c(hcpcs.desc, "Under Destruction Procedures on the Rectum")
# "46220"
# "46900"
# "46901" 
# "46902" 
# "46903" 
# "46904" 
# "46905"
# "46906"
# "46907"
# "46908" 
# "46909"
# "46910"
# "46911"
# "0288T"
# "0377T" 
# "44705"
# "45190"
# "46220" 
# "46900"
# "46901" 
# "46902"
# "46903"
# "46904"
# "46905"
# "46906"
# "46907"
# "46908"
# "46909" 
# "46910"
# "46911"
# "46912" 
# "46913"
# "46914"
# "46915"
# "46916"
# "46917"
# "46918"
# "46919"
# "46920"
# "46921" 
# "46922"
# "46923"
# "46924"
# "46937"
# "46938"
# "46939"
# "46940"
# "46941"
# "46942"
# "91123"
# "G0455"

hcpcs.desc <- data_fram(e)
# "0288T" "0377T" "44705" "45190" "46220" "46900" "46901" "46902" "46903" "46904" "46905" "46906" "46907" "46908" "46909" "46910" "46911"
# "46912" "46913" "46914" "46915" "46916" "46917" "46918" "46919" "46920" "46921" "46922" "46923" "46924" "46937" "46938" "46939" "46940"
# "46941" "46942" "91123" "G0455"


dxs <- paste0("dx", seq(1, 30))
dx_descs <- paste0("dx", seq(1, 30), "_desc")
prs <- paste0("pr", seq(1, 15))
pr_descs <- paste0("pr", seq(1, 15), "_desc")
cols <- as.vector(rbind(dxs, dx_descs, prs, pr_descs))

sorted <- 
  cdiff.nrd %>% 
  filter(nrd_year == 2010, dqtr != '4')  %>%
  select(cols)  

write_csv(sorted, 'data/test.csv')
