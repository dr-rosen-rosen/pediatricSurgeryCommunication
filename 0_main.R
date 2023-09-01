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
source('1_transciptingest.R')

######################
######################
# read in transcripts:
######################
######################


# for BIAS transcripts:
bias_transcripts <- getBiasTranscripts(
  dirs = c('/Volumes/LSM/Boss\ Peds\ Surgery\ Transcripts\ (LSM)/Bias\ Transcripts/Bias_Visits_with_1_clinician_surgeon',
          '/Volumes/LSM/Boss\ Peds\ Surgery\ Transcripts\ (LSM)/Bias\ Transcripts/Bias\ Visits\ with\ 2\ transcripts\ each\ (clinician_supporting)')
)
table(bias_transcripts$speaker)
write.csv(table(bias_transcripts$speaker),'bias_speaker.csv')
write.csv(table(bias_transcripts$speaker,bias_transcripts$file_id),'bias_speakerByFile.csv')

# for SDM transcripts:
sdm_transcripts <- getSDMTranscripts(
  dirs = c('/Volumes/LSM/Boss\ Peds\ Surgery\ Transcripts\ (LSM)/SDM\ Transcripts/SDM\ visits\ with\ one\ clinician',
           '/Volumes/LSM/Boss\ Peds\ Surgery\ Transcripts\ (LSM)/SDM\ Transcripts/SDM\ visits\ with\ 2\ clinicians\ each')
)
table(sdm_transcripts$speaker)
write.csv(table(sdm_transcripts$speaker),'SDM_speaker.csv')
write.csv(table(sdm_transcripts$speaker,sdm_transcripts$file_id),'SDM_speakerByFile.csv')

# for Connects transcripts:
connects_transcripts <- getConnectsTranscripts(
  dirs = c('/Volumes/LSM/Boss\ Peds\ Surgery\ Transcripts\ (LSM)/CONNECTS\ Transcripts/CONNECTS\ visits\ with\ 1\ clinician',
           '/Volumes/LSM/Boss\ Peds\ Surgery\ Transcripts\ (LSM)/CONNECTS\ Transcripts/CONNECTS\ visits\ with\ 2\ clinicians')
)
table(connects_transcripts$speaker)
write.csv(table(connects_transcripts$speaker),'connects_speaker.csv')
write.csv(table(connects_transcripts$speaker,connects_transcripts$file_id),'connects_speakerByFile.csv')

cmb_transcripts <- bind_rows(
  bias_transcripts,
  sdm_transcripts,
  connects_transcripts
) %>%
  mutate(
    speaker = case_match(speaker,
                         c('dr', 'doctor','physician') ~ 'dr',
                         .default = speaker)
  )

write.csv(table(cmb_transcripts$speaker),'cmbd_speakers.csv')
