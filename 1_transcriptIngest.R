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
  
  errors <- data.frame(
    file_id = character(),
    n_tables = integer()
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
        errors <- rbind(data.frame(file_id = f, n_tables = tbl_no),errors)
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
  if (dim(errors)[1] > 0) {
    print(errors)
    write.csv(errors, 'transcriptIngestErrors_SDM.csv')}
  return(transcript_df)
}


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
  
  errors <- data.frame(
    file_id = character(),
    n_tables = integer()
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
      } else if (tbl_no == 1 | tbl_no == 2) {
        # print(paste('Hmm... one table for:',f))
        # errors <- rbind(data.frame(file_id = f, n_tables = tbl_no),errors)
      # } else if (tbl_no == 2) {
        df <- docxtractr::docx_extract_tbl(t, tbl_no, header = FALSE) %>%
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
        errors <- rbind(data.frame(file_id = f, n_tables = tbl_no),errors)
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
  
  if (dim(errors)[1] > 0) {
    print(errors)
    write.csv(errors, 'transcriptIngestErrors_SDM.csv')}
  return(transcript_df)
}


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
  
  errors <- data.frame(
    file_id = character(),
    n_tables = integer()
  )
  
  for (dir in dirs) {
    
    fNames <- list.files(
      path = dir, 
      pattern = '*.docx')
    
    for (f in fNames) {
      # file_type <- if_else(is.na(stringr::str_match(f,"d_(.*?).docx")[[1,2]]), 'single_clinician',stringr::str_match(f,"d_(.*?).docx")[[1,2]])
      file_type <- if_else(grepl('clinician',f,fixed = TRUE), 'clinician','supporting')
      t <- docxtractr::read_docx(here(dir,f))
      tbl_no <- docxtractr::docx_tbl_count(t)
      print(paste0(f,'; number of tables: ',tbl_no," and file type:",file_type)) 
      
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
        df$file_type <- file_type
        
        transcript_df <- bind_rows(transcript_df,df)
      } else {
        print(paste('SKIPPING... Something other than 2 tables for:',f))
        errors <- rbind(data.frame(file_id = f, n_tables = tbl_no),errors)
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
  if (dim(errors)[1] > 0) {
    print(errors)
    write.csv(errors, 'transcriptIngestErrors_SDM.csv')}
  return(transcript_df)
}


# for BIAS transcripts:
bias_transcripts <- getBiasTranscripts(
  dirs = c('/Volumes/LSM/Boss\ Peds\ Surgery\ Transcripts\ (LSM)/Bias\ Transcripts/Bias_Visits_with_1_clinician_surgeon',
           '/Volumes/LSM/Boss\ Peds\ Surgery\ Transcripts\ (LSM)/Bias\ Transcripts/Bias\ Visits\ with\ 2\ transcripts\ each\ (clinician_supporting)')
)

# for SDM transcripts:
sdm_transcripts <- getSDMTranscripts(
  dirs = c('/Volumes/LSM/Boss\ Peds\ Surgery\ Transcripts\ (LSM)/SDM\ Transcripts/SDM\ visits\ with\ one\ clinician',
           '/Volumes/LSM/Boss\ Peds\ Surgery\ Transcripts\ (LSM)/SDM\ Transcripts/SDM\ visits\ with\ 2\ clinicians\ each')
)

# for Connects transcripts:
connects_transcripts <- getConnectsTranscripts(
  dirs = c('/Volumes/LSM/Boss\ Peds\ Surgery\ Transcripts\ (LSM)/CONNECTS\ Transcripts/CONNECTS\ visits\ with\ 1\ clinician',
           '/Volumes/LSM/Boss\ Peds\ Surgery\ Transcripts\ (LSM)/CONNECTS\ Transcripts/CONNECTS\ visits\ with\ 2\ clinicians')
)