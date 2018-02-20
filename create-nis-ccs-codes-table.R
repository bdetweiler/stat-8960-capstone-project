library('MonetDB.R')
library('MonetDBLite')
library('dplyr')
library('dbplyr')
library('DBI')


#setwd('/home/bdetweiler/src/Data_Science/stat-8960-capstone-project/')

ccs.codes <- read.csv('data/formats/2017_ccs_services_procedures.csv', stringsAsFactors=FALSE)
ccs.codes$Code.Range <- gsub("'", "", ccs.codes$Code.Range)

final.df <- data.frame(Code=c(""), CCS=c(""), CCS.Label=c(""))

for (i in 1:dim(ccs.codes)[1]) {
  
  print(paste0("row: ", i))
  
  # Get the current code range in row i
  code.range <- ccs.codes$Code.Range[i] 

  # Split it into two codes
  code.range <- unlist(strsplit(code.range, "-"))

  tmp.df <- c() 
  # Look for codes with a letter at the beginning of the code
  if (length(grep("^([A-Z]).*", code.range, ignore.case = FALSE, perl = TRUE, value = FALSE)) == 2) {
    
    gsub("^([A-Z]).*", "\1", code.range)
    letters <- gsub("^([A-Z]).*", "\\1", code.range)
    numbers <- gsub("^[A-Z](.*)", "\\1", code.range)
    leading.zeros <- gsub("^(0*).*", "\\1", numbers)

    # If there is no range only use the existing code, otherwise use the range 
    if (numbers[1] == numbers[2]) {
      tmp.df <- data.frame(Code=as.character(paste0(letters[1], leading.zeros[1], as.integer(numbers[1]))))
    } else {
      tmp.df <- data.frame(Code=as.character(paste0(letters, leading.zeros, seq(as.integer(numbers[1])), as.integer(numbers[2]), by=1)))
    }

  }

  # Look for codes with a letter at the end of the code
  if (length(grep(".*([A-Z])$", code.range, ignore.case = FALSE, perl = TRUE, value = FALSE)) == 2) {
    letters <- gsub(".*([A-Z])$", "\\1", code.range)
    numbers <- gsub("(.*)[A-Z]$", "\\1", code.range)
    leading.zeros <- gsub("^(0*).*", "\\1", numbers)

    # If there is no range only use the existing code, otherwise use the range 
    if (numbers[1] == numbers[2]) {
      tmp.df <- data.frame(Code=as.character(paste0(leading.zeros[1], as.integer(numbers[1]), letters[1])))
    } else {
      tmp.df <- data.frame(Code=as.character(paste0(leading.zeros, seq(as.integer(numbers[1]), as.integer(numbers[2]), by=1), letters)))
    }
  } 


  # Look for codes with no letters
  if (length(grep("^([0-9])+$", code.range, ignore.case = FALSE, perl = TRUE, value = FALSE)) == 2) {
    numbers <- gsub("^(.*)$", "\\1", code.range)
    leading.zeros <- gsub("^(0*).*", "\\1", numbers)
    
    # If there is no range only use the existing code, otherwise use the range 
    if (numbers[1] == numbers[2]) {
      tmp.df <- data.frame(Code=as.character(paste0(leading.zeros[1], as.integer(numbers[1]))))
    } else {
      tmp.df <- data.frame(Code=as.character(paste0(leading.zeros, seq(as.integer(numbers[1]), as.integer(numbers[2]), by=1))))
    }
  } 
  
  tmp.df$CCS <- as.character(ccs.codes$CCS[i])
  tmp.df$CCS.Label <- as.character(ccs.codes$CCS.Label[i])
  
  final.df <- rbind(final.df, tmp.df) 
}
final.df <- final.df[-1, ]
write.csv(final.df, "nis-ccs-codes.csv", row.names=FALSE)

# Not sure this file is adequate
# hcpcs.df <- read.csv('data/formats/2015DHSAddendum_10-30-14.csv', sep=";")

# colnames(final.df)
# merged.df <- merge(x=final.df, y=hcpcs.df, by.x="Code", by.y="HCPCS.CODE", all.x=TRUE)
# head(merged.df)

# merged.df[which(!is.na(merged.df$HCPCS.DESC)),]
