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
  cleanRoles(.) %>%
  strcutureFiles(.) # makes sure supporting in before index, etc. 

keep_cols <- c('speaker','speech','file_id', 'sequence', 're_sequence','study', 'spkr_reclass','file_study_id','file_id_full',"spkr_reclass_top_level")
cmb_transcripts_cleanText <- cleanSpeech(
  df = cmb_transcripts[keep_cols],
  speechCol = 'speech',
  word_count_thresh = 2 # will include only lines with more than two words
)

smthd <- dropRolesAndSmoothe(
  df = cmb_transcripts_cleanText, 
  target_roles = c('clinician','parent'), 
  file_col = 'file_study_id',
  spkr_col = 'spkr_reclass_top_level', 
  seq_col = 're_sequence', 
  speech_col = 'speech') %>% 
  select(-to_drop)

windowed <- window_transcripts(
  df = smthd, 
  file_col = 'file_study_id',
  spkr_col = 'spkr_reclass_top_level', 
  speech_col = 'speech_sm', 
  window = 8, 
  collapse = " ") %>%
  filter(speech_agg_wc > 0)
write.csv(windowed,'windowed.csv')
# run liwc
windowed_liwcd <- read.csv('LIWC-22 Results - windowed - LIWC Analysis.csv')


smthd %>%
  group_by(file_study_id,spkr_reclass_top_level) %>%
  summarize(speech_agg = paste(speech_sm, collapse = ' ')) %>% # speech_sm is the non-windowed text
  openxlsx::write.xlsx(.,file = 'conv.xlsx')
# run liwc
conv_liwcd <- read.csv('LIWC-22 Results - conv - LIWC Analysis.csv')

############################################
############################################
# make matching and accommodation measures:
############################################
############################################

source(here('3_accomodationMetrics.R'))

convLSM_df <- getConvLSM(
  df = conv_liwcd,
  file_col = 'file_study_id',
  spkr_col = 'spkr_reclass_top_level',
  vars_to_match = c('auxverb','article','adverb','ipron','prep','negate','conj','quantity','ppron')
    )
rwLSM_df <- getRwLSM(
  df = windowed_liwcd, 
  file_col = 'file_study_id',
  spkr_col = 'spkr_reclass_top_level') 

window_size = 8
windowed_liwcd <- windowed_liwcd %>%
  groupby(file_study_id) %>%
  slice(((window_size-1)*2):n()) %>%
  ungroup()

rw.rLSM.by.timePoint <- getAccomodation(
  df = windowed_liwcd, 
  file_col = 'file_study_id',
  spkr_col = 'spkr_reclass_top_level',
  breaks = 4)

getRwLSMByRow(
  df = windowed_liwcd, 
  file_col = 'file_study_id',
  spkr_col = 'spkr_reclass_top_level')

cmbdMetrics_df <- full_join(
  rwLSM_df,convLSM_df, rw.rLSM.by.timePoint,
  by = 'file_study_id')
