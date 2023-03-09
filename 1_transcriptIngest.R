##################################
##################################
######## 
######## Read in transcript files
######## 
######## 
##################################
##################################

library(officer)
library(tidyverse)
library(here)

# Get a list of all of the file names
dir <- '/Users/mrosen44/Johns Hopkins/Anne Links - Boss Peds Surgery Transcripts (LSM)/Visits with 1 clinician (surgeon)'
fNames <- list.files(path = dir, pattern = '*.docx')

# To test using just one file, do something like:
# fNames <- "10002_Edited Deidentified.docx"  
transcript_df <- data.frame(
  file_id = character(),
  Speaker = character(),
  speech = character(),
  text = character(),
  text_ok = logical(),
  text_pieces = integer(),
  text_remainder = character()
)

# for each file:
for (f in fNames) {
  df <- officer::read_docx(here(dir,f)) %>%
    officer::docx_summary() %>%
    select(text) %>% 
    separate_wider_delim(text, ":",
                         names = c("Speaker", "speech"),
                         too_many = "merge",
                         too_few = "debug")
  df['file_id'] <- substr(f,1,5)
  transcript_df <- rbind(transcript_df,df)
}
