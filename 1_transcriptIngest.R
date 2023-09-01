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


######################
######################
# for BIAS transcripts:
######################
######################

getBiasTranscripts <- function(dirs) {
  
  transcript_df <- data.frame(
    file_id = character(),
    speaker = character(),
    speech = character(),
    text = character(),
    text_ok = logical(),
    text_pieces = integer(),
    text_remainder = character(),
    file_type = character()
  )
  
  for (dir in dirs) { 
    fNames <- list.files(
    path = dir, 
    pattern = '*.docx')
  
    for (f in fNames) {
      file_type <- if_else(is.na(stringr::str_match(f,"d_(.*?).docx")[[1,2]]), 'single_clinician',stringr::str_match(f,"d_(.*?).docx")[[1,2]])
      t <- docxtractr::read_docx(here(dir,f))
      tbl_no <- docxtractr::docx_tbl_count(t)
      print(paste0(f,'; number of tables: ',tbl_no))
      if (tbl_no == 0) {
        df <- officer::read_docx(here(dir,f)) %>%
          officer::docx_summary() %>%
          select(text) %>%
          separate_wider_delim(text, ":",
                               names = c("speaker", "speech"),
                               too_many = "merge",
                               too_few = "debug")
        df['file_id'] <- substr(f,1,5)
        df$sequence <- seq(1:nrow(df))
        df$file_type <- file_type
        transcript_df <- rbind(transcript_df,df)
      } else if (tbl_no == 1) {
        df <- docxtractr::docx_extract_tbl(t, 1, header = FALSE) %>%
          mutate(
            V1 = stringr::str_remove(V1, pattern = " .*"),
            V1 = tolower(V1),
            file_id = substr(f,1,5)
          ) %>%
          rename('speaker' = V1,'speech' = V2)
        df$sequence <- seq(1:nrow(df))
        df$file_type <- file_type
        transcript_df <- bind_rows(transcript_df,df)
      } else {
        print(paste('Too many tables for:',f))
      }
    }
  }
  transcript_df <- transcript_df %>%
    janitor::clean_names() %>%
    mutate(
      speaker = gsub("[[:punct:][:blank:]]+", "", speaker), # removes all punctuation
      speaker = tolower(speaker),
      # speaker = recode(speaker, 'doctor' = 'dr','mom' = 'mother'),
      study = 'bias'
    )
  return(transcript_df)
}

######################
######################
# for SDM transcripts:
######################
######################

getSDMTranscripts <- function(dirs) {
  
  transcript_df <- data.frame(
    file_id = character(),
    speaker = character(),
    speech = character(),
    text = character(),
    text_ok = logical(),
    text_pieces = integer(),
    text_remainder = character(),
    file_type = character()
  )
  for (dir in dirs) {
   
    fNames <- list.files(
      path = dir, 
      pattern = '*.docx')
    
    
    for (f in fNames) {
      file_type <- if_else(is.na(stringr::str_match(f,"d_(.*?).docx")[[1,2]]), 'single_clinician',stringr::str_match(f,"d_(.*?).docx")[[1,2]])
      t <- docxtractr::read_docx(here(dir,f))
      tbl_no <- docxtractr::docx_tbl_count(t)
      print(paste0(f,'; number of tables: ',tbl_no))
      if (tbl_no == 0) {
        # print(paste("uh oh... No tables for:",f))
        df <- officer::read_docx(here(dir,f)) %>%
          officer::docx_summary() %>%
          select(text) %>%
          separate_wider_delim(text, ":",
                               names = c("speaker", "speech"),
                               too_many = "merge",
                               too_few = "debug")
        df['file_id'] <- substr(f,1,5)
        df$sequence <- seq(1:nrow(df))
        df$file_type <- file_type
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
          rename('speaker' = V1,'speech' = V2)
        df$sequence <- seq(1:nrow(df))
        df$file_type <- file_type
        transcript_df <- bind_rows(transcript_df,df)
      } else {
        print(paste('more than 2 tables for:',f))
        }
    }
  } # end of directory loop 
  transcript_df <- transcript_df %>%
    janitor::clean_names() %>%
    mutate(
      speaker = gsub("[[:punct:][:blank:]]+", "", speaker), # removes all punctuation
      speaker = tolower(speaker),
      # speaker = recode(speaker, 'doctor' = 'dr', 'physician' = 'dr','mom' = 'mother'),
      study = 'sdm'
    )
  return(transcript_df)
}

######################
######################
# for CONNECTS transcripts:
######################
######################

getConnectsTranscripts <- function(dirs) {
  
  transcript_df <- data.frame(
    file_id = character(),
    speaker = character(),
    speech = character(),
    text = character(),
    text_ok = logical(),
    text_pieces = integer(),
    text_remainder = character(),
    file_type = character()
  )

  for (dir in dirs) {
    
    fNames <- list.files(
      path = dir, 
      pattern = '*.docx')
    
    for (f in fNames) {
      file_type <- if_else(is.na(stringr::str_match(f,"d_(.*?).docx")[[1,2]]), 'single_clinician',stringr::str_match(f,"d_(.*?).docx")[[1,2]])
      t <- docxtractr::read_docx(here(dir,f))
      tbl_no <- docxtractr::docx_tbl_count(t)
      print(paste0(f,'; number of tables: ',tbl_no)) 
      
      if (tbl_no == 2) {
        df <- docxtractr::docx_extract_tbl(t, 2, header = FALSE) %>%
          mutate(
            V1 = stringr::str_remove(
              stringr::str_remove(V1, pattern = 'ENROLLED *'), 
              pattern = ":.*"),
            # V1 = tolower(V1),
            file_id = substr(f,1,5)
          ) %>%
          rename('speaker' = V1,'speech' = V2)
        df$sequence <- seq(1:nrow(df))
        
        transcript_df <- bind_rows(transcript_df,df)
      } else {
        print(paste('SKIPPING... Something other than 2 tables for:',f))
      }
      
      } # end of F loop
  } # end of dirs loop
  transcript_df <- transcript_df %>%
    janitor::clean_names() %>%
    mutate(
      speaker = gsub("[[:punct:][:blank:]]+", "", speaker), # removes all punctuation
      speaker = tolower(speaker),
      speaker = recode(speaker, 'doctor' = 'dr', 'physician' = 'dr','mom' = 'mother'),
      study = 'connects'
    )
  return(transcript_df)
}

# General function for reading transcripts from word docs

# read in function with options for body_text or table; if table, need table_num

