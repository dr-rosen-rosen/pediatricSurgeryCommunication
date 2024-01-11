##################################
##################################
######## 
######## Main file for Peds Surgery pt/md comm
######## 
######## 
##################################
##################################

library(tidyverse)
library(here)
library(config)
library(runner)

############################################
############################################
# read in transcripts:
############################################
############################################

source(here('1_transcriptIngest.R'))

############################################
############################################
# clean transcripts:
############################################
############################################

source(here('2_transcriptCleaning.R'))

# combining transcripts and cleaning speaker labels
cmb_transcripts <- bind_rows(
  bias_transcripts,
  sdm_transcripts,
  connects_transcripts
) %>%
  cleanRoles(.) #%>%
  # strcutureFiles(.) # makes sure supporting in before index, etc. ; leave this out for getting separate index & supporting


keep_cols <- c('speaker','speech','file_id', 'sequence', 'study', 'spkr_reclass','file_study_id','file_type','file_study_ftype_id',"spkr_reclass_top_level")
# dropping this 're_sequence',
cmb_transcripts_cleanText <- cleanSpeech(
  df = cmb_transcripts[keep_cols],
  speechCol = 'speech',
  word_count_thresh = 2 # will include only lines with more than two words
) 


file_metrics <- getFileMetrics(
  df = cmb_transcripts_cleanText,
  file_by_col = 'file_study_ftype_id'
        )

smthd <- dropRolesAndSmoothe(
  df = cmb_transcripts_cleanText, 
  target_roles = c('clinician','parent'), 
  file_col = 'file_study_ftype_id',
  spkr_col = 'spkr_reclass_top_level', 
  seq_col = 'sequence',  # re_sequence
  speech_col = 'speech') %>% 
  select(-to_drop)

windowed <- window_transcripts(
  df = smthd, 
  file_col = 'file_study_ftype_id',
  spkr_col = 'spkr_reclass_top_level', 
  speech_col = 'speech_sm', 
  window = 8, 
  collapse = " ") %>%
  filter(speech_agg_wc > 0)
write.csv(windowed,'windowed_11-29-23.csv')
# run liwc
# Aggrregated index and suppoorting
#windowed_liwcd <- read.csv('LIWC-22 Results - windowed - LIWC Analysis.csv')
# separate index and suporting
windowed_liwcd <- read.csv('LIWC-22 Results - windowed_11-29-23 - LIWC Analysis.csv')

smthd %>%
  group_by(file_study_ftype_id,spkr_reclass_top_level) %>%
  summarize(speech_agg = paste(speech_sm, collapse = ' ')) %>% # speech_sm is the non-windowed text
  # openxlsx::write.xlsx(.,file = 'conv_11-09-23.xlsx')
  write.csv(.,file = 'conv_11-29-23.csv')
# run liwc
conv_liwcd <- read.csv('LIWC-22 Results - conv_11-29-23 - LIWC Analysis.csv')

# get file for baseline LSM
smthd %>%
  group_by(file_study_ftype_id,spkr_reclass_top_level) %>%
  mutate(
    # the cut function allows chunking into a dynamically specified number of bits
    time.point = cut(row_number(), b = 4, labels = FALSE)
  ) %>%
  ungroup() %>%
  filter(time.point == 1) %>%
  group_by(file_study_ftype_id,spkr_reclass_top_level) %>%
  summarize(speech_agg = paste(speech_sm, collapse = ' ')) %>% # speech_sm is the non-windowed text
  write.csv(.,'baselineLSM_11-29-2023.csv')
baseLine_liwcd <- read.csv('LIWC-22 Results - baselineLSM_11-29-2023 - LIWC Analysis.csv')

############################################
############################################
# make matching and accommodation measures:
############################################
############################################

source(here('3_accomodationMetrics.R'))


baseLineLSM_df <- getConvLSM(
  df = baseLine_liwcd,
  file_col = 'file_study_ftype_id',
  spkr_col = 'spkr_reclass_top_level',
  vars_to_match = c('auxverb','article','adverb','ipron','prep','negate','conj','quantity','ppron')
) %>%
  rename(baseline_lsm = conv_lsm)

convLSM_df <- getConvLSM(
  df = conv_liwcd,
  file_col = 'file_study_ftype_id',
  spkr_col = 'spkr_reclass_top_level',
  vars_to_match = c('auxverb','article','adverb','ipron','prep','negate','conj','quantity','ppron')
    )
rwLSM_df <- getRwLSM(
  df = windowed_liwcd, 
  file_col = 'file_study_ftype_id',
  spkr_col = 'spkr_reclass_top_level') 

window_size = 8
windowed_liwcd <- windowed_liwcd %>%
  group_by(file_study_ftype_id) %>%
  slice(((window_size-1)*2):n()) %>%
  ungroup()

rw.rLSM.by.timePoint <- getAccomodation(
  df = windowed_liwcd, 
  file_col = 'file_study_ftype_id',
  spkr_col = 'spkr_reclass_top_level',
  breaks = 4)

# getRwLSMByRow(
#   df = windowed_liwcd, 
#   file_col = 'file_study_id',
#   spkr_col = 'spkr_reclass_top_level')

dfs <- list(rwLSM_df,convLSM_df, rw.rLSM.by.timePoint, baseLineLSM_df,file_metrics)
cmbdMetrics_df <- dfs %>% reduce(full_join, by = 'file_study_ftype_id') %>%
  separate_wider_delim(file_study_ftype_id, delim = '-', names = c('id','study','ftype')) %>%
  mutate(
    # id = as.numeric(id),
    ParticipantID_combined = if_else(study == 'bias',paste0('1',id),id),
    ParticipantID_combined  = as.numeric(ParticipantID_combined)
  )

write.csv(cmbdMetrics_df,'pedsSurgery_lsm_lsa_metrics_11-29-2023.csv')

all_data <- full_join(cmbdMetrics_df,dta_df, by = 'ParticipantID_combined',relationship = "many-to-many")

scatter.smooth(y=file_metrics$prop_dom_pt_fam_wc,x=as.factor(file_metrics$n_pt_fam_sprks))
