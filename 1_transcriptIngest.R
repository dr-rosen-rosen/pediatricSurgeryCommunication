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
dir <- 
  # bias transcripts with one clinician
  # '/Users/mrosen44/Johns Hopkins/Anne Links - Boss Peds Surgery Transcripts (LSM)/Bias Transcripts/Bias Visits with 1 clinician (surgeon)'
  # SDM transcripts with one clinician
  '/Users/mrosen44/Johns Hopkins/Anne Links - Boss Peds Surgery Transcripts (LSM)/SDM Transcripts/SDM visits with one clinician'
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

######################
######################
# for BIAS transcripts:
######################
######################
for (f in fNames) {
  t <- docxtractr::read_docx(here(dir,f))
  tbl_no <- docxtractr::docx_tbl_count(t)
  print(paste0(f,'; number of tables: ',tbl_no))
  if (tbl_no == 0) {
    df <- officer::read_docx(here(dir,f)) %>%
      officer::docx_summary() %>%
      select(text) %>%
      separate_wider_delim(text, ":",
                           names = c("Speaker", "speech"),
                           too_many = "merge",
                           too_few = "debug")
    df['file_id'] <- substr(f,1,5)
    transcript_df <- rbind(transcript_df,df)
  } else if (tbl_no == 1) {
    df <- docxtractr::docx_extract_tbl(t, 1, header = FALSE) %>%
      mutate(
        V1 = stringr::str_remove(V1, pattern = " .*"),
        V1 = tolower(V1),
        file_id = substr(f,1,5)
      ) %>%
      rename('Speaker' = V1,'speech' = V2)
    transcript_df <- bind_rows(transcript_df,df)
  } else {
    print(paste('Too many tables for:',f))
  }
}

transcript_df <- transcript_df %>%
  janitor::clean_names() %>%
  mutate(
    speaker = gsub("[[:punct:][:blank:]]+", "", speaker), # removes all punctuation
    speaker = tolower(speaker),
    speaker = recode(speaker, 'doctor' = 'dr','mom' = 'mother')
  )
table(transcript_df$speaker)

######################
######################
# for SDM transcripts:
######################
######################
for (f in fNames) {
  t <- docxtractr::read_docx(here(dir,f))
  tbl_no <- docxtractr::docx_tbl_count(t)
  print(paste0(f,'; number of tables: ',tbl_no))
  if (tbl_no == 0) {
    # print(paste("uh oh... No tables for:",f))
    df <- officer::read_docx(here(dir,f)) %>%
      officer::docx_summary() %>%
      select(text) %>%
      separate_wider_delim(text, ":",
                           names = c("Speaker", "speech"),
                           too_many = "merge",
                           too_few = "debug")
    df['file_id'] <- substr(f,1,5)
    df$sequence <- seq(1:nrow(df))
    transcript_df <- rbind(transcript_df,df)
  } else if (tbl_no == 1) {
    print(paste('Hmm... one table for:',f))
  } else if (tbl_no == 2) {
    df <- docxtractr::docx_extract_tbl(t, 2, header = FALSE) %>%
      mutate(
        V1 = stringr::str_remove(V1, pattern = " .*"),
        V1 = tolower(V1),
        file_id = substr(f,1,5)
      ) %>%
      rename('Speaker' = V1,'speech' = V2)
    df$sequence <- seq(1:nrow(df))
    transcript_df <- bind_rows(transcript_df,df)
  } else {
    print(paste('more than 2 tables for:',f))
    }
}

transcript_df <- transcript_df %>%
  janitor::clean_names() %>%
  mutate(
    speaker = gsub("[[:punct:][:blank:]]+", "", speaker), # removes all punctuation
    speaker = tolower(speaker),
    speaker = recode(speaker, 'doctor' = 'dr', 'physician' = 'dr','mom' = 'mother')
  )
table(transcript_df$speaker)


# General function for reading transcripts from word docs

# read in function with options for body_text or table; if table, need table_num

