
##################################
##################################
######## 
######## Scripts for generating matching and accommodation measures
######## 
######## 
##################################
##################################
library(runner)
library(tidyverse)


getConvLSM <- function(df,file_col,spkr_col,vars_to_match) {
  if ( nrow(unique(df[spkr_col])) == 2) {
    convLSM_df <- df %>%
      select(!!sym(file_col),!!sym(spkr_col),one_of(vars_to_match)) %>%
      pivot_longer(cols = all_of(vars_to_match), names_to = 'matching_vars',values_to = 'liwc_score') %>%
      pivot_wider(
        id_cols = c(!!sym(file_col),matching_vars), 
        names_from = !!(sym(spkr_col)),
        names_prefix = 'spkr_',
        values_from = 'liwc_score') %>%
      rowwise() %>%
      mutate(
        lsm = (1 - (abs(pick(3) - pick(4)) / (pick(3) + pick(4) + .001))) # this is selection the columns representing speakers, regardless of their names
      ) %>% 
      ungroup() %>%
      select(!!sym(file_col),matching_vars,lsm) %>%
      pivot_wider(id_cols = !!sym(file_col), names_from = matching_vars, values_from = lsm, names_prefix = 'lsm_') %>%
      mutate(
        conv_lsm = rowMeans(across(starts_with('lsm_')),na.rm=TRUE)
      ) %>%
      select(-starts_with("lsm_"))
  } else {
    print('More than two speakers; fix it.')
    convLSM_df <- data.frame()
  }
  
  return(convLSM_df)
}

getRwLSMByRow <- function(df, file_col, spkr_col) {
  df <- df %>%
    dplyr::select(!!sym(file_col), !!sym(spkr_col), auxverb, article, adverb, ipron, 
                  prep, negate, conj, quantity, ppron) %>%
      group_by(!!sym(file_col)) %>%
      mutate(
        # This puts the turn before the current one on the same row with a .lag suffix
        auxverb.lag = lag(auxverb),
        article.lag = lag(article),
        adverb.lag = lag(adverb),
        ipron.lag = lag(ipron),
        prep.lag = lag(prep),
        negate.lag = lag(negate),
        conj.lag = lag(conj),
        quantity.lag = lag(quantity),
        ppron.lag = lag(ppron)
      ) %>%
      ungroup() %>%
      # This makes sure that only liwc categories prersent in the first statement are used for rlsm
      mutate(across(c(auxverb.lag, article.lag,adverb.lag, ipron.lag,
                      prep.lag, negate.lag, conj.lag, quantity.lag, ppron.lag
      ), 
      ~ if_else(. > 0,.,as.numeric(NA)))) %>%
      # This sets liwc categories in the responders speech to NA if that category was NA in the first person's speech
      # Per the rLSM paper
      group_by(!!sym(file_col)) %>%
      mutate(
        auxverb = if_else(is.na(auxverb.lag),as.numeric(NA),auxverb),
        article = if_else(is.na(article.lag),as.numeric(NA),article),
        adverb = if_else(is.na(adverb.lag),as.numeric(NA),adverb),
        ipron = if_else(is.na(ipron.lag),as.numeric(NA),ipron),
        prep = if_else(is.na(prep.lag),as.numeric(NA),prep),
        negate = if_else(is.na(negate.lag),as.numeric(NA),negate),
        conj = if_else(is.na(conj.lag),as.numeric(NA),conj),
        quantity = if_else(is.na(quantity.lag),as.numeric(NA),quantity),
        ppron = if_else(is.na(ppron.lag),as.numeric(NA),ppron)
      ) %>%
      ungroup() %>%
      rowwise() %>%
      mutate(
        auxverb.rLSM = 1 - (abs(auxverb - auxverb.lag) / (auxverb + auxverb.lag + .0001)),
        article.rLSM = 1 - (abs(article - article.lag) / (article + article.lag + .0001)),
        adverb.rLSM = 1 - (abs(adverb - adverb.lag) / (adverb + adverb.lag + .0001)),
        ipron.rLSM = 1 - (abs(ipron - ipron.lag) / (ipron + ipron.lag + .0001)),
        prep.rLSM = 1 - (abs(prep - prep.lag) / (prep + prep.lag + .0001)),
        negate.rLSM = 1 - (abs(negate - negate.lag) / (negate + negate.lag + .0001)),
        conj.rLSM = 1 - (abs(conj - conj.lag) / (conj + conj.lag + .0001)),
        quantity.rLSM = 1 - (abs(quantity - quantity.lag) / (quantity + quantity.lag + .0001)),
        ppron.rLSM = 1 - (abs(ppron - ppron.lag) / (ppron + ppron.lag + .0001))
      ) %>%
      ungroup()
  return(df)
}

getRwLSM <- function(df, file_col, spkr_col) {
  
  df_rLSM <- getRwLSMByRow(df, file_col, spkr_col) %>% # creates rLSM by row
    #creates an average rLSM across separate features
    group_by(!!sym(file_col),!!sym(spkr_col)) %>%
    summarize(
      across(contains('.rLSM'), .fns = ~ mean(.x,na.rm=TRUE))
      ) %>%
    ungroup() %>%
    mutate(rLSM = rowMeans(select(.,contains('.rLSM')),na.rm = TRUE)) %>%
    select(-ends_with(".rLSM")) %>%
    pivot_wider(
      id_cols = !!sym(file_col),
      names_from = !!sym(spkr_col),
      values_from = c(rLSM),
      names_glue = sprintf("{.value}.{%s}",spkr_col)
    ) 
  return(df_rLSM)
}

getRwLSMbySegment <- function(df, file_col, spkr_col, breaks) {
  df_rLSM <- getRwLSMByRow(df, file_col, spkr_col) %>% # creates rLSM by row
    mutate(
      rw.rlsm = rowMeans(across(ends_with(".rLSM")), na.rm = TRUE)
    ) %>%
    select(rw.rlsm,!!sym(file_col),!!sym(spkr_col)) %>%
    group_by(!!sym(file_col),!!sym(spkr_col)) %>%
    mutate(
      # the cut function allows chunking into a dynamically specified number of bits
      time.point = cut(row_number(), b = breaks, labels = FALSE) - 1
      ) %>%
    ungroup() %>%
    group_by(!!sym(file_col),!!sym(spkr_col), time.point) %>%
    summarize(rw.rlsm = mean(rw.rlsm, na.rm = TRUE)) %>%
    ungroup()
  return(df_rLSM)
}

getAccomodation <- function(df,file_col, spkr_col, breaks) {
  accom_df <- getRwLSMbySegment(df, file_col, spkr_col, breaks) %>%
    group_by(!!sym(file_col),!!sym(spkr_col)) %>%
    group_modify(function(data,key) {
      tryCatch(
        {m <- nlme::gls(
          rw.rlsm ~ time.point,
          data = data) %>%
          broom.mixed::tidy()}, error = function(e) {
            tibble(term = c(NA),
                   estimate = c(NA_real_),
                   std.error = c(NA_real_),
                   statistic = c(NA_real_),
                   p.value = c(NA_real_))
          }
        )}
      ) %>%
    filter(term == 'time.point') %>%
    # dplyr::select(!!sym(file_col),!!sym(spkr_col),estimate)
    pivot_wider(id_cols = !!sym(file_col),
                names_from = !!sym(spkr_col),
                values_from = estimate,
                names_glue = sprintf("accom.{%s}.{%s}",breaks,spkr_col))
  return(accom_df)
}
