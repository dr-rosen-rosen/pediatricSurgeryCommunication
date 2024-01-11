############################################
############################################
# outcome analyses:
############################################
############################################


############################################
# read in and descriptive:
############################################

dta_df <- haven::read_dta('/Volumes/LSM/Data/SDM_Bias_CONNECTS data for analysis.dta')
table(all_data$study)
all_data <- full_join(cmbdMetrics_df,dta_df, by = 'ParticipantID_combined',relationship = "many-to-many")
# scatter.smooth(y=file_metrics$prop_dom_pt_fam_wc,x=as.factor(file_metrics$n_pt_fam_sprks))


index_clin_df <- all_data %>%
  filter(ftype == 'index_clinician')

# index_clin_df %>%
#   # ggplot(aes(x = rLSM.clinician)) +
#   ggplot(aes(x = baseline_lsm)) +
#   # ggplot(aes(x = conv_lsm)) +
#   # ggplot(aes(x = accom.4.parent)) +
#   geom_histogram() +
#   ggthemes::theme_tufte()
# 
# index_clin_df %>% select(rLSM.clinician, conv_lsm, baseline_lsm,accom.4.clinician, study) %>%
#   gtsummary::tbl_summary(
#     # by = study,
#     missing = 'no'
#   ) %>%
#   gtsummary::as_gt() %>%
#   gt::gtsave('descrpitives.docx')
# 
# # scatter.smooth(index_clin_df$baseline_lsm,index_clin_df$rLSM.clinician)
# 
# index_clin_df %>% select(rLSM.clinician, conv_lsm, accom.4.clinician, Ethnicity_parent) %>%
#   mutate(
#     Ethnicity_parent_dic = case_match(Ethnicity_parent,
#                                       1 ~ 1, 
#                                       2 ~ 2,
#                                       .default = 3)
#   ) %>%
#   select(-Ethnicity_parent) %>%
#   gtsummary::tbl_summary(
#     by = Ethnicity_parent_dic,
#     missing = 'no'
#   ) %>%
#   gtsummary::as_gt() %>%
#   gt::gtsave('descrpitives.docx')
# 
# 
# index_clin_df %>% select(rLSM.clinician, conv_lsm, accom.4.clinician, file_type ) %>%
#   gtsummary::tbl_summary(
#     by = file_type,
#     missing = 'no'
#   ) %>%
#   gtsummary::as_gt() %>%
#   gt::gtsave('descrpitives.docx')




to_factor <- c('YearsExperience_combined','Predisposition_surgery','Ethnicity_parent_dic',
               'Ethnicity_clin_dic','Gender_clinician','Income','Best_Worst10','Trust10',
               'ClinicianID_merge','Education_merge',
               'CAHPS_ParentOpin','CAHPS_ReasonY_yn','CAHPS_ReasonN_yn',
               'Gender_parent')

to_scale <- c('accom.4.clinician','conv_lsm','rLSM.clinician','baseline_lsm',
              'accom.4.parent','rLSM.parent')

m.data <- index_clin_df %>%
  select(
    conv_lsm,accom.4.clinician,rLSM.clinician,baseline_lsm,
    rLSM.parent,accom.4.parent,
    # file_type,
    ClinicianID_merge,Education_merge, Ethnicity_parent, Income, Age_parent,
    IAT_race, IAT_ethnicity,Empathy, ConflictCombined,
    Best_Worst10, Similarity_clin, Trust10,
    CAHPS_ParentOpin,CAHPS_ReasonY_yn,CAHPS_ReasonN_yn,
    Interpersonal_Friendly, Interpersonal_Support, Interpersonal_Respect, Interpersonal_Discrimination,
    Ethnicity_clin,Gender_clinician,YearsExperience_combined,
    Predisposition_surgery,
    Gender_parent) %>%
  mutate(
    Ethnicity_parent_dic = case_match(Ethnicity_parent,
                                      1 ~ 1, 
                                      2 ~ 2,
                                      .default = 3),
    Ethnicity_clin_dic = case_match(Ethnicity_clin,
                                    1 ~ 1,
                                    2 ~ 2,
                                    .default = 3),
    accom.4.parent_dic = if_else(accom.4.parent > 0,1,0),
    accom.4.clinician_dic = if_else(accom.4.clinician > 0,1,0)
  ) %>%
  mutate(across(.cols = all_of(to_factor),~ as.factor(.x))) %>%
  mutate(across(.cols = all_of(to_scale),~ datawizard::standardize(.x,
                                                                   robust = FALSE,
                                                                   two_sd = FALSE,
                                                                   weights = NULL,
                                                                   reference = NULL,
                                                                   center = NULL,
                                                                   scale = NULL,
                                                                   verbose = TRUE)))

skimr::skim(m.data)

############################################
# Demographics -> lsm/lsa:
############################################

m.1.1 <- lme4::lmer(conv_lsm ~ 
                    Age_parent + 
                    (1|ClinicianID_merge), data = m.data)

m.1.2 <- lme4::lmer(conv_lsm ~ 
                      Ethnicity_parent_dic +
                      (1|ClinicianID_merge), data = m.data)

m.1.3 <- lme4::lmer(conv_lsm ~ 
                      Gender_parent +
                      (1|ClinicianID_merge), data = m.data)

m.1.4 <- lme4::lmer(conv_lsm ~ 
                      Income +
                      (1|ClinicianID_merge), data = m.data)

# m.1.5 <- lme4::lmer(conv_lsm ~ 
#                       IAT_race +
#                       (1|ClinicianID_merge), data = m.data)

m.1.6 <- lme4::lmer(conv_lsm ~ 
                      Ethnicity_clin +
                      (1|ClinicianID_merge), data = m.data)

m.1.7 <- lme4::lmer(conv_lsm ~ 
                      Gender_clinician +
                      (1|ClinicianID_merge), data = m.data)

m.1.8 <- lme4::lmer(conv_lsm ~ 
                      YearsExperience_combined +
                      (1|ClinicianID_merge), data = m.data)

performance::check_model(m.1.1)
performance::check_model(m.1.2)
performance::check_model(m.1.3)
performance::check_model(m.1.4)
performance::check_model(m.1.5)
performance::check_model(m.1.6)
performance::check_model(m.1.7)
performance::check_model(m.1.8)

sjPlot::tab_model(m.1.1,m.1.2,m.1.3,m.1.4)
sjPlot::tab_model(m.1.6,m.1.7,m.1.8)

m.2.1 <- lme4::lmer(rLSM.clinician ~ 
                      Age_parent + 
                      (1|ClinicianID_merge), data = m.data)

m.2.2 <- lme4::lmer(rLSM.clinician ~ 
                      Ethnicity_parent_dic +
                      (1|ClinicianID_merge), data = m.data)

m.2.3 <- lme4::lmer(rLSM.clinician ~ 
                      Gender_parent +
                      (1|ClinicianID_merge), data = m.data)

m.2.4 <- lme4::lmer(rLSM.clinician ~ 
                      Income +
                      (1|ClinicianID_merge), data = m.data)

# m.2.5 <- lme4::lmer(rLSM.clinician ~ 
#                       IAT_race +
#                       (1|ClinicianID_merge), data = m.data)

m.2.6 <- lme4::lmer(rLSM.clinician ~ 
                      Ethnicity_clin +
                      (1|ClinicianID_merge), data = m.data)

m.2.7 <- lme4::lmer(rLSM.clinician ~ 
                      Gender_clinician +
                      (1|ClinicianID_merge), data = m.data)

m.2.8 <- lme4::lmer(rLSM.clinician ~ 
                      YearsExperience_combined +
                      (1|ClinicianID_merge), data = m.data)

performance::check_model(m.2.1)
performance::check_model(m.2.2)
performance::check_model(m.2.3)
performance::check_model(m.2.4)
performance::check_model(m.2.5)
performance::check_model(m.2.6)
performance::check_model(m.2.7)
performance::check_model(m.2.8)

sjPlot::tab_model(m.2.1,m.2.2,m.2.3,m.2.4)
sjPlot::tab_model(m.2.6,m.2.7,m.2.8)


m.3.1 <- lme4::lmer(accom.4.clinician ~ 
                      Age_parent + 
                      (1|ClinicianID_merge), data = m.data)

m.3.2 <- lme4::lmer(accom.4.clinician ~ 
                      Gender_parent +
                      (1|ClinicianID_merge), data = m.data)

m.3.3 <- lme4::lmer(accom.4.clinician ~ 
                      Ethnicity_parent_dic +
                      (1|ClinicianID_merge), data = m.data)

m.3.4 <- lme4::lmer(accom.4.clinician ~ 
                      Income +
                      (1|ClinicianID_merge), data = m.data)

# m.3.5 <- lme4::lmer(accom.4.clinician ~ 
#                       IAT_race +
#                       (1|ClinicianID_merge), data = m.data)

m.3.6 <- lme4::lmer(accom.4.clinician ~ 
                      Ethnicity_clin +
                      (1|ClinicianID_merge), data = m.data)

m.3.7 <- lme4::lmer(accom.4.clinician ~ 
                      Gender_clinician +
                      (1|ClinicianID_merge), data = m.data)

m.3.8 <- lme4::lmer(accom.4.clinician ~ 
                      YearsExperience_combined +
                      (1|ClinicianID_merge), data = m.data)

performance::check_model(m.3.1)
performance::check_model(m.3.2)
performance::check_model(m.3.3)
performance::check_model(m.3.4)
performance::check_model(m.3.5)
performance::check_model(m.3.6)
performance::check_model(m.3.7)
performance::check_model(m.3.8)

sjPlot::tab_model(m.3.1,m.3.2,m.3.3,m.3.4)
sjPlot::tab_model(m.3.6,m.3.7,m.3.8)


m.4.1 <- lme4::lmer(baseline_lsm ~ 
                      Age_parent + 
                      (1|ClinicianID_merge), data = m.data)

m.4.2 <- lme4::lmer(baseline_lsm ~ 
                      Ethnicity_parent_dic +
                      (1|ClinicianID_merge), data = m.data)

m.4.3 <- lme4::lmer(baseline_lsm ~ 
                      Gender_parent +
                      (1|ClinicianID_merge), data = m.data)
m.4.4 <- lme4::lmer(baseline_lsm ~ 
                      Income +
                      (1|ClinicianID_merge), data = m.data)

# m.4.5 <- lme4::lmer(baseline_lsm ~ 
#                       IAT_race +
#                       (1|ClinicianID_merge), data = m.data)

m.4.6 <- lme4::lmer(baseline_lsm ~ 
                      Ethnicity_clin +
                      (1|ClinicianID_merge), data = m.data)

m.4.7 <- lme4::lmer(baseline_lsm ~ 
                      Gender_clinician +
                      (1|ClinicianID_merge), data = m.data)

m.4.8 <- lme4::lmer(baseline_lsm ~ 
                      YearsExperience_combined +
                      (1|ClinicianID_merge), data = m.data)

performance::check_model(m.4.1)
performance::check_model(m.4.2)
performance::check_model(m.4.3)
performance::check_model(m.4.4)
performance::check_model(m.4.5)
performance::check_model(m.4.6)
performance::check_model(m.4.7)
performance::check_model(m.4.8)

sjPlot::tab_model(m.4.1,m.4.2,m.4.3,m.4.4)
sjPlot::tab_model(m.4.6,m.4.7,m.4.8)


m.5.1 <- lme4::lmer(Similarity_clin ~ Ethnicity_parent_dic +conv_lsm + rLSM.clinician + accom.4.clinician + baseline_lsm + (1|ClinicianID_merge), data = m.data)
m.5.2 <- lme4::lmer(Interpersonal_Friendly ~ Ethnicity_parent_dic +conv_lsm + rLSM.clinician + accom.4.clinician + baseline_lsm + (1|ClinicianID_merge), data = m.data)
m.5.3 <- lme4::lmer(Interpersonal_Support ~ Ethnicity_parent_dic +conv_lsm + rLSM.clinician + accom.4.clinician + baseline_lsm + (1|ClinicianID_merge), data = m.data)
m.5.4<- lme4::lmer(Interpersonal_Respect ~ Ethnicity_parent_dic +conv_lsm + rLSM.clinician + accom.4.clinician + baseline_lsm +  (1|ClinicianID_merge), data = m.data)
m.5.5<- lme4::lmer(Interpersonal_Discrimination ~ Ethnicity_parent_dic +conv_lsm + rLSM.clinician + accom.4.clinician + baseline_lsm + (1|ClinicianID_merge), data = m.data)
m.5.6 <- lme4::glmer(Best_Worst10 ~ Ethnicity_parent_dic +conv_lsm + rLSM.clinician + accom.4.clinician + baseline_lsm + (1|ClinicianID_merge), family = 'binomial',data = m.data)
m.5.7 <- lme4::glmer(Trust10 ~ Ethnicity_parent_dic +conv_lsm + rLSM.clinician + accom.4.clinician + baseline_lsm + (1|ClinicianID_merge), family = 'binomial',data = m.data)

table(m.data$Interpersonal_Support)



performance::check_model(m.5.1)
performance::check_model(m.5.2)
performance::check_model(m.5.3)
performance::check_model(m.5.4)
performance::check_model(m.5.5)
performance::check_model(m.5.6)
performance::check_model(m.5.7)

sjPlot::tab_model(m.5.1,m.5.2,m.5.3,m.5.4,m.5.5)#, file = 'table3.html')
sjPlot::tab_model(m.5.1,m.5.2)
sjPlot::tab_model(m.5.3,m.5.4,m.5.5)
sjPlot::tab_model(m.5.6,m.5.7)
cor(m.data %>% select(all_of(to_scale)), use = 'complete.obs')


m.6.1 <- lme4::glmer(CAHPS_ParentOpin ~ 
                       # Ethnicity_parent_dic +
                       conv_lsm + rLSM.clinician + accom.4.clinician + baseline_lsm + 
                       (1|ClinicianID_merge), family = 'binomial', data = m.data)
# m.6.2 <- lme4::glmer(CAHPS_ReasonY_yn ~ conv_lsm + rLSM.clinician + accom.4.clinician + baseline_lsm + (1|ClinicianID_merge), family = 'binomial', data = m.data)

m.6.3 <- lme4::glmer(CAHPS_ReasonN_yn ~ 
                       # Ethnicity_parent_dic +
                       conv_lsm + rLSM.clinician + accom.4.clinician + baseline_lsm + 
                       # accom.4.parent +
                       (1|ClinicianID_merge), family = 'binomial', data = m.data)

sjPlot::tab_model(m.6.1,m.6.2,m.6.3)

ggplot(m.data,aes(x = baseline_lsm, y = conv_lsm, fill = Ethnicity_parent_dic)) +
  geom_point(aes(color = Ethnicity_parent_dic)) +
  ggside::geom_xsidedensity(alpha = .3, position = "stack") +
  ggside::geom_ysidedensity(alpha = .3, position = "stack") +
  # geom_smooth(method = lm) +
  ggthemes::theme_tufte()



