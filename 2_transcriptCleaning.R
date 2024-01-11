
######################
######################
# 0. for cleaning speaker roles
######################
######################

cleanRoles <- function(df) {
  
  cleaned <- df %>% 
    filter(
      !speaker %in% c('childsfriend', 'male', 'man2', 'medical', '2nd','patient',
                      'female','nurse', 'other','silence','tv','supportingstaff',
                      's','s0','s3','s4','s5','scribe','staff', 'son','2nd')
    ) %>% 
    filter(
      !str_detect(speaker,'sibling|speaker')
    ) %>%
    mutate(
      spkr_reclass = case_match(speaker,
                                c('dr', 'doctor','physician','clinician') ~ 'index_clin',
                                c('Child', 'child1', 'child2', '2yearold','kid') ~ 'child',
                                c('dad', 'family', 'familymember', 'grandparent', 'mom', 'mother', 'parent', 'parent1', 'parent2','father') ~ 'parent',
                                c('supportingclinician', 'supporting', 'studentclinician', 'resident', 'pa','support_clin', 'np','doctorresident','app') ~ 'sup_clin',
                                .default = speaker)
    )
  
  cleaned <- cleaned %>%
    mutate(
      spkr_reclass_top_level = case_match(spkr_reclass,
                                          c('index_clin','sup_clin') ~ 'clinician',
                                          .default = spkr_reclass), 
      file_type = case_match(file_type,
                             c('Supportinig','Supporting') ~ 'supporting',
                             c('Clinician','clinician','single_clinician') ~ 'index_clinician',
                             .default = file_type),
      # file_id_full = paste(file_id,file_type,study, sep = "-"),
      file_study_id = paste(file_id,study,sep = "-"),
      file_study_ftype_id = paste(file_study_id,file_type,sep = "-")
    )
  return(cleaned)
}

strcutureFiles <- function(df) {
  print(nrow(df))
  df$study <- ordered(df$study, levels = c('bias', 'connects','sdm'))
  df$file_type <- ordered(df$file_type, levels = c('supporting','index_clinician'))
  print(nrow(df))
  df <- df[with(df, order(study,file_study_id,file_type,sequence)),] # reorders df
  df <- df %>% group_by(study,file_id) %>% # adds a new sequence for the whole 
    mutate(re_sequence = seq(1:n())) %>% ungroup()
  return(df)
}

##############
## 1. Clean text (remove things in brackets, etc.) & Drop NAs
##############

cleanSpeech <- function(df, speechCol, word_count_thresh) {
    # Clean text
  df[,speechCol] <- lapply(df[,speechCol], function (x) gsub("\\[(.*?)\\]", "", x))
  df <- df[which(df[,speechCol] != ''),]
  # counting the number of words for each conversation turn; and dropping empty
  df$word_count <- stringr::str_count(unlist(df[,speechCol]), "\\w+")
  df <- df[which(df$word_count > word_count_thresh),]
  return(df)
}

getFileMetrics <- function(df, file_by_col) {
  # recovers and saves file type for late rmerging
  # file_types <- unique(df[,c('file_study_id','file_type')]) %>%
  #   group_by(file_study_id) %>%
  #   summarize(
  #     file_type = case_match(n(),
  #                            1 ~ 'index_only',
  #                            2 ~ 'index_and_supporting',
  #                            .default = NA))

  tot_counts <- df %>%
    group_by(!!sym(file_by_col)) %>%
    summarise(
      tot_wc = sum(word_count),
      tot_turns = n()
    )
  print(names(tot_counts))
  n_pt_spkrs <- df %>%
    # filter(spkr_reclass_top_level != 'clinician') %>%
    filter(spkr_reclass_top_level == 'parent') %>%
    group_by(!!sym(file_by_col)) %>%
    summarise(
      n_pt_fam_sprks = n_distinct(speaker)
    ) %>% ungroup()
  print(names(n_pt_spkrs))
  pt <- df %>%
    # filter(spkr_reclass_top_level != 'clinician') %>%
    filter(spkr_reclass_top_level == 'parent') %>%
    group_by(!!sym(file_by_col), speaker) %>%
    summarize(
      pt_wc = sum(word_count),
      pt_turn = n()
    ) %>%
    ungroup() %>%
    group_by(!!sym(file_by_col)) %>%
    summarize(
      max_pt_fam_wc = max(pt_wc),
      max_pt_fam_turn = max(pt_turn),
      tot_pt_fam_wc = sum(pt_wc),
      tot_pt_fam_turn = sum(pt_turn)
    )
  print(names(pt))
  clin <- df %>%
    filter(spkr_reclass_top_level == 'clinician') %>%
    group_by(!!sym(file_by_col), speaker) %>%
    summarize(
      clin_wc = sum(word_count),
      clin_turn = n()
    ) %>%
    ungroup() %>%
    group_by(!!sym(file_by_col)) %>%
    summarize(
      max_clin_wc = max(clin_wc),
      max_clin_turn = max(clin_turn),
      tot_clin_wc = sum(clin_wc),
      tot_clin_turn = sum(clin_turn)
    )
  print(names(clin))
  # dfs <- list(file_types,tot_counts, pt, clin, n_pt_spkrs)
  dfs <- list(tot_counts, pt, clin, n_pt_spkrs)
  all <- reduce(dfs,full_join, by = file_by_col) %>%
    rowwise() %>%
    mutate(
      prop_dom_clin_wc = max_clin_wc / tot_clin_wc,
      prop_dom_clin_turn = max_clin_turn / tot_clin_turn,
      prop_dom_pt_fam_wc = max_pt_fam_wc / tot_pt_fam_wc,
      prop_dom_pt_fam_turn = max_pt_fam_turn / tot_pt_fam_turn
    )
  
  return(all)
}
##############
## 2. Smooth transcripts (combine adjacent text for same speaker)
##############

smootheRoles <- function(df, spkr_col, seq_col, speech_col) {
  df <- df %>%
    mutate(
      speech_sm = if_else(
        lead(!!sym(spkr_col), order_by = !!(sym(seq_col)), n = 1L) == !!(sym(spkr_col)),
        str_c(!!sym(speech_col), lead(!!sym(speech_col), order_by = !!(sym(seq_col)), n = 1L), sep = " "),
        !!sym(speech_col)
      )) %>%
    filter(!(lag(!!sym(spkr_col), order_by = !!(sym(seq_col)), n = 1L)) == !!sym(spkr_col))
  return(df)
}

dropRoles <- function(df, sprk_col, target_roles) {
  # this drops all non-target roles AND target roles 
  # if they are surrounded by non-target roles
  df <- df %>%
    mutate(
      to_drop = if_else(
        (!(!!sym(sprk_col) %in% target_roles) | # drops all non-target roles
           #drops all target roles surrounded by (likely speaking to) non-target roles
           ((!!sym(sprk_col) %in% target_roles) &
              !(lag(!!sym(sprk_col), 1) %in% target_roles) &
              !(lead(!!sym(sprk_col),1) %in% target_roles))),
        1,0)
    ) %>%
    filter(to_drop == 0)
}

dropRolesAndSmoothe <- function(df, target_roles, file_col,spkr_col, seq_col, speech_col) {
  
  df <- df %>%
    group_by(!!sym(file_col)) %>%
    # smoothe
    group_modify(~smootheRoles(df = .x, spkr_col, seq_col, speech_col)) %>%
    # drop non-target and surrounded target roles
    group_modify(~dropRoles(df = .x, spkr_col, target_roles)) %>%
    # smoothe roles again after dropping non-target
    group_modify(~smootheRoles(df = .x, spkr_col, seq_col, speech_col)) %>%
    ungroup()
  return(df)
}

##############
## 3. Prep for liwc-ing
##############

window_transcripts <- function(df, file_col, spkr_col, speech_col, window, collapse) {
  
  if(missing(collapse)) {collapse <- ' '}
  
  # do some quality control
  if(nrow(unique(df[spkr_col])) != 2) {
    print('Too many different speakers...')
  } else {print('Good to go: only 2 speakers')}
  
  windowed_df <- df %>%
    group_by(!!sym(file_col), !!sym(spkr_col)) %>%
    mutate(
      speech_agg = runner::runner(
        !!sym(speech_col),
        f = paste,
        collapse = collapse,
        k = window,
        na_pad = TRUE
      ),
      speech_agg_wc = str_count(speech_agg, "\\w+")) %>% 
    ungroup()
  
  return(windowed_df)
}
