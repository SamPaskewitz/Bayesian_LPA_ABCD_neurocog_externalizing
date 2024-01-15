# This uses data release 5.0 instead of release 4.0.
# It adds fMRI variables and more outcome measures, and some other stuff.
# In this version, the data are NOT standardized (z-scored).
# This is because people insist that doing ANY pre-processing (including something as simple
# as standardization) on the training set that includes information about the test set
# is a no-no.
# Also, outcome data are now provided at the three year and four year followups,
# not just the two year followup as before.
# 9/12: Added conduct disorder.
# 9/13: Added PATH variables (substance use curiosity and intent to use).

# Define a convenience function to find the intersection of multiple vectors (not just two).
multi_intersect = function(list_of_vectors){
  in_all = list_of_vectors[[1]]
  for(i in 2:length(list_of_vectors)){
    in_all = intersect(in_all, list_of_vectors[[i]])
  }
  return(in_all)
}

# Define a convenience function to find the union of multiple vectors (not just two).
multi_union = function(list_of_vectors){
  in_one = list_of_vectors[[1]]
  for(i in 2:length(list_of_vectors)){
    in_one = union(in_one, list_of_vectors[[i]])
  }
  return(in_one)
}

# Define a function to import and process data (this is cleaner than just running a script).
process_data = function(){
  # Read in the relevant data files.
  loc = "~/Documents/abcd-data-release-5.0/core/"
  dl = list(nback_beh = read.csv(paste0(loc, "imaging/mri_y_tfmr_nback_beh.csv")), # Task fMRI nBack Behavior
            stop_signal_beh = read.csv(paste0(loc, "imaging/mri_y_tfmr_sst_beh.csv")), # Task fMRI SST Behavior
            nback_weights = read.csv(paste0(loc, "imaging/mri_y_tfmr_nback_2bv0b_dst.csv")), # relevant fMRI nBack beta weights
            recog = read.csv(paste0(loc, "imaging/mri_y_tfmr_nback_rec_beh.csv")), # nBack recognition
            stop_signal_weights = read.csv(paste0(loc, "imaging/mri_y_tfmr_sst_isvcg_dst.csv")), # relevant fMRI SST beta weights
            toolbox = read.csv(paste0(loc, "neurocognition/nc_y_nihtb.csv")), # Youth NIH TB Summary Scores
            game_of_dice = read.csv(paste0(loc, "neurocognition/nc_y_gdt.csv")), # Game of Dice Summary Scores
            ravlt = read.csv(paste0(loc, "neurocognition/nc_y_ravlt.csv")), # RAVLT
            little_man = read.csv(paste0(loc, "neurocognition/nc_y_lmt.csv")), # little man task
            cbcl = read.csv(paste0(loc, "mental-health/mh_p_cbcl.csv")), # Parent Child Behavior Checklist Scores (CBCL)
            ksads = read.csv(paste0(loc, "mental-health/mh_p_ksads_ss.csv")), # KSADS
            bpm = read.csv(paste0(loc, "mental-health/mh_y_bpm.csv")), # Brief Problem Monitor (ASEBA)
            upps = read.csv(paste0(loc, "mental-health/mh_y_upps.csv")), # UPPS
            substance = read.csv(paste0(loc, "substance-use/su_y_tlfb.csv")), # substance use timeline followback
            path = read.csv(paste0(loc, "substance-use/su_y_path_intuse.csv")), # PATH (substance use curiosity and intent to use)
            alc_exp = read.csv(paste0(loc, "substance-use/su_y_alc_exp.csv")), # alcohol use expectancies
            long_track = read.csv(paste0(loc, "abcd-general/abcd_y_lt.csv")), # longitudinal tracking
            demo = read.csv(paste0(loc, "abcd-general/abcd_p_demo.csv")) # demographics
  )
  task_names = c('nback_beh', 'stop_signal_beh', 'recog', 'toolbox', 'game_of_dice', 'ravlt', 'little_man')
  neuro_names = c('nback_weights', 'stop_signal_weights')
  
  ####################################################################################################
  
  # Only keep data from the 2 year follow up (except for 'long_track' and 'demo').
  for(i in 1:length(dl)){
    if(names(dl)[i] %in% c('long_track', 'demo')){
      dl[[i]] = subset(dl[[i]], eventname == "baseline_year_1_arm_1")
    } else if(names(dl)[i] %in% c(task_names, neuro_names)){
      dl[[i]] = subset(dl[[i]], eventname == "2_year_follow_up_y_arm_1")
    }
  }
  
  # Only keep people with valid performance as defined in the data dictionary.
  dl$nback = subset(dl$nback_beh, tfmri_nback_beh_performflag == 1)
  dl$stop_signal = subset(dl$stop_signal_beh, tfmri_sst_beh_performflag == 1)
  
  # Identify people who are in all data sets.
  subjectkey_list = list()
  for(i in 1:(length(dl) - 2)){
    subjectkey_list[[i]] = dl[[i]]$src_subject_id
  }
  subjectkey_in_all = multi_intersect(subjectkey_list)
  print(paste('N =', length(subjectkey_in_all), 'participants who are in all data sets.'))
  
  # Identify people who are in at least one data set.
  subjectkey_list = list()
  for(i in 1:length(dl)){
    subjectkey_list[[i]] = dl[[i]]$src_subject_id
  }
  subjectkey_in_one = multi_union(subjectkey_list)
  print(paste('N =', length(subjectkey_in_one), 'participants who are in at least one data set.'))
  
  # Create data frames to store processed data of different types.
  task_behav_df = data.frame(row.names = sort(subjectkey_in_one)) # task behavioral data
  neuro_df = data.frame(row.names = sort(subjectkey_in_one)) # fMRI measures (beta weights)
  info_df = data.frame(row.names = sort(subjectkey_in_one)) # demographic info etc.
  outcome_2year_df = data.frame(row.names = sort(subjectkey_in_one)) # outcome measures (externalizing, substance use etc.)
  outcome_3year_df = data.frame(row.names = sort(subjectkey_in_one)) # outcome measures at 3 year followup
  outcome_4year_df = data.frame(row.names = sort(subjectkey_in_one)) # outcome measures at 4 year followup
  
  # ----- TASK BEHAVIOR -----
  
  task_behav_df[dl$nback_beh$src_subject_id, 'two_back'] = dl$nback_beh$tfmri_nb_all_beh_c2b_rate
  overall_hit_rate = (dl$recog$tfmri_rec_all_beh_oldposf_hr + dl$recog$tfmri_rec_all_beh_oldnegf_hr + dl$recog$tfmri_rec_all_beh_oldnf_hr + dl$recog$tfmri_rec_all_beh_oldpl_hr)/4
  overall_hit_rate = pmax(pmin(overall_hit_rate, 0.999), 0.001) # cap hit rate at 0.001 and 0.999 so that we can compute dprime
  overall_false_alarm_rate = (dl$recog$tfmri_rec_all_beh_newposf_fa + dl$recog$tfmri_rec_all_beh_newnegf_fa + dl$recog$tfmri_rec_all_beh_newnf_fa + dl$recog$tfmri_rec_all_beh_newpl_fa)/4
  overall_false_alarm_rate = pmax(pmin(overall_false_alarm_rate, 0.999), 0.001) # cap false alarm rate at 0.001 and 0.999 so that we can compute dprime
  task_behav_df[dl$recog$src_subject_id, 'recog_dprime'] = qnorm(overall_hit_rate) - qnorm(overall_false_alarm_rate) # overall d-prime for detecting old stimuli (those used in the n-back task)
  task_behav_df[dl$stop_signal$src_subject_id, 'neg_ssrt'] = -dl$stop_signal$tfmri_sst_all_beh_total_issrt # reverse code this (multiply by -1)
  task_behav_df[dl$toolbox$src_subject_id, 'picvocab'] = dl$toolbox$nihtbx_picvocab_uncorrected # Picture Vocabulary Test
  task_behav_df[dl$toolbox$src_subject_id, 'flanker'] = dl$toolbox$nihtbx_flanker_uncorrected # Flanker Inhibitory Control and Attention Test
  task_behav_df[dl$toolbox$src_subject_id, 'pattern'] = dl$toolbox$nihtbx_pattern_uncorrected # Pattern Comparison Processing Speed Test
  task_behav_df[dl$toolbox$src_subject_id, 'picture'] = dl$toolbox$nihtbx_picture_uncorrected # Picture Sequence Memory Test
  task_behav_df[dl$toolbox$src_subject_id, 'reading'] = dl$toolbox$nihtbx_reading_uncorrected # Oral Reading Recognition Test
  task_behav_df[dl$game_of_dice$src_subject_id, 'dice'] = dl$game_of_dice$gdt_scr_expressions_net_score # no. safe choices - no. risky choices
  task_behav_df[dl$ravlt$src_subject_id, 'ravlt'] = dl$ravlt$pea_ravlt_sd_trial_vi_tc # immediate recall total correct
  
  # Add little man task data.
  # The official calculation of percent correct is not consistent across subjects.
  # For most people it is proportion correct (out of 1), but for a few it is really percent correct
  # (out of 100).
  # Thus I need to recompute percent correct from the numbers of correct, incorrect, and timed out trials.
  # I then divide by average RT on correct trials to get the efficiency score.
  lmt_percent_correct = 100*dl$little_man$lmt_scr_num_correct/(dl$little_man$lmt_scr_num_wrong + dl$little_man$lmt_scr_num_timed_out + dl$little_man$lmt_scr_num_correct) # percent correct (computed from scratch because variable in the dataset is not consistently calculated)
  task_behav_df[dl$little_man$src_subject_id, 'little_man'] = lmt_percent_correct/as.numeric(dl$little_man$lmt_scr_rt_correct) # efficiency score (percent correct/avg correct RT)
  
  # ----- IMAGING DATA -----
  
  neuro_df[dl$stop_signal_weights$src_subject_id, 'sst_lac'] = dl$stop_signal_weights$tfsstabwdp_302
  neuro_df[dl$stop_signal_weights$src_subject_id, 'sst_rac'] = dl$stop_signal_weights$tfsstabwdp_376
  neuro_df[dl$stop_signal_weights$src_subject_id, 'sst_lmac'] = dl$stop_signal_weights$tfsstabwdp_303
  neuro_df[dl$stop_signal_weights$src_subject_id, 'sst_rmac'] = dl$stop_signal_weights$tfsstabwdp_377
  neuro_df[dl$nback_weights$src_subject_id, 'nback_lmfg'] = dl$nback_weights$tfabwdp_607
  neuro_df[dl$nback_weights$src_subject_id, 'nback_rmfg'] = dl$nback_weights$tfabwdp_681
  
  # ----- DEMOGRAPHICS ETC. -----
  
  info_df[dl$long_track$src_subject_id, 'site_id'] = as.character(dl$long_track$site_id_l)
  info_df[dl$long_track$src_subject_id, 'district_id'] = as.character(dl$long_track$district_id)
  info_df[dl$long_track$src_subject_id, 'age'] = dl$long_track$interview_age
  info_df[dl$long_track$src_subject_id, 'family_id'] = as.character(dl$long_track$rel_family_id)
  info_df[dl$long_track$src_subject_id, 'birth_id'] = as.character(dl$long_track$rel_birth_id)
  sex_code = c("Male", "Female", "Intersex-Male", "Intersex-Female", "Don't know", "Refuse to answer")
  info_df[dl$demo$src_subject_id, 'sex'] = sex_code[c(1, 2, 3, 4, 999, 777)[dl$demo$demo_sex_v2]] # recoded from integers to strings
  race_code = c("White", "Black", "Hispanic", "Asian", "Other")
  info_df[dl$demo$src_subject_id, 'race_ethnicity'] = race_code[dl$demo$race_ethnicity] # recoded from integers to strings
  
  # Record how many task variables are missing from each person.
  info_df$number_behav_missing = rowSums(is.na(task_behav_df))
  info_df$number_neuro_missing = rowSums(is.na(neuro_df))
  
  # ----- OUTCOME MEASURES (2-year) -----
  
  cbcl_2year = subset(dl$cbcl, eventname == '2_year_follow_up_y_arm_1')
  bpm_2year = subset(dl$bpm, eventname == '2_year_follow_up_y_arm_1')
  upps_2year = subset(dl$upps, eventname == '2_year_follow_up_y_arm_1')
  substance_2year = subset(dl$substance, eventname == '2_year_follow_up_y_arm_1')
  path_2year = subset(dl$path, eventname == '2_year_follow_up_y_arm_1')
  ksads_2year = subset(dl$ksads, eventname == '2_year_follow_up_y_arm_1')
  conduct_disorder_vars = c(paste0('ksads_16_', c('897_p', '898_p', '899_p', '900_p')),
                            paste0('ksads2_16_', c('855_p', '856_p', '857_p', '858_p')))
  alc_exp_2year = subset(dl$alc_exp, eventname == '2_year_follow_up_y_arm_1')
  curiosity_code = c(1, 1, 1, 0, NA, NA)
  # any curiosity (1, 2, 3) is coded as 1
  # "Not at all curious" (4) is coded as 0
  # "Don't know" (5) and "Refused to answer" (6) are coded as NA
  intent_code = c(1, 1, 0, 0, NA, NA)
  # "Definitely yes" (1) and "Probably yes" (2) are coded as 1
  # "Probably not" (3) and "Definitely not" (4) are coded as 0
  # "Don't know" (5) and "Refused to answer" (6) are coded as NA
  
  outcome_2year_df[cbcl_2year$src_subject_id, 'extern'] = cbcl_2year$cbcl_scr_syn_external_t
  outcome_2year_df[cbcl_2year$src_subject_id, 'rbreak'] = cbcl_2year$cbcl_scr_syn_rulebreak_t
  outcome_2year_df[cbcl_2year$src_subject_id, 'agrs'] = cbcl_2year$cbcl_scr_syn_aggressive_t
  outcome_2year_df[cbcl_2year$src_subject_id, 'intern'] = cbcl_2year$cbcl_scr_syn_internal_t
  outcome_2year_df[cbcl_2year$src_subject_id, 'somatic'] = cbcl_2year$cbcl_scr_syn_somatic_t
  outcome_2year_df[cbcl_2year$src_subject_id, 'anx_dep'] = cbcl_2year$cbcl_scr_syn_anxdep_t
  outcome_2year_df[cbcl_2year$src_subject_id, 'with_dep'] = cbcl_2year$cbcl_scr_syn_withdep_t
  outcome_2year_df[bpm_2year$src_subject_id, 'extern_youth'] = bpm_2year$bpm_y_scr_external_t
  outcome_2year_df[bpm_2year$src_subject_id, 'intern_youth'] = bpm_2year$bpm_y_scr_internal_t
  outcome_2year_df[bpm_2year$src_subject_id, 'atten_youth'] = bpm_2year$bpm_y_scr_attention_t
  outcome_2year_df[upps_2year$src_subject_id, 'pos_urg'] = upps_2year$upps_y_ss_positive_urgency
  outcome_2year_df[upps_2year$src_subject_id, 'neg_urg'] = upps_2year$upps_y_ss_negative_urgency
  outcome_2year_df[substance_2year$src_subject_id, 'total_use_days'] = substance_2year$tlfb_cal_scr_tot_su_ud_cum
  outcome_2year_df[substance_2year$src_subject_id, 'total_use_events'] = substance_2year$tlfb_cal_scr_num_events
  outcome_2year_df[is.na(outcome_2year_df$total_use_days), 'total_use_days'] = 0
  outcome_2year_df[is.na(outcome_2year_df$total_use_events), 'total_use_events'] = 0
  outcome_2year_df$any_substance_use = (outcome_2year_df$total_use_events > 0)*1
  outcome_2year_df[path_2year$src_subject_id, 'curiosity_tobacco'] = curiosity_code[path_2year$path_alc_youth1_l]
  outcome_2year_df[path_2year$src_subject_id, 'curiosity_alcohol'] = curiosity_code[path_2year$path_alc_youth2_l]
  outcome_2year_df[path_2year$src_subject_id, 'curiosity_marijuana'] = curiosity_code[path_2year$path_alc_youth3_l]
  outcome_2year_df[path_2year$src_subject_id, 'intent_tobacco'] = intent_code[path_2year$path_alc_youth4_l]
  outcome_2year_df[path_2year$src_subject_id, 'intent_alcohol'] = intent_code[path_2year$path_alc_youth5_l]
  outcome_2year_df[path_2year$src_subject_id, 'intent_marijuana'] = intent_code[path_2year$path_alc_youth6_l]
  outcome_2year_df[ksads_2year$src_subject_id, 'conduct_disorder'] = (rowSums(ksads_2year[, conduct_disorder_vars], na.rm = TRUE) > 0)*1
  outcome_2year_df[alc_exp_2year$src_subject_id, 'alc_pos_expect'] = alc_exp_2year$aeq_positive_expectancies_ss
  outcome_2year_df[alc_exp_2year$src_subject_id, 'alc_neg_expect'] = alc_exp_2year$aeq_negative_expectancies_ss
  
  # ----- OUTCOME MEASURES (3-year) -----
  
  cbcl_3year = subset(dl$cbcl, eventname == '3_year_follow_up_y_arm_1')
  bpm_3year = subset(dl$bpm, eventname == '3_year_follow_up_y_arm_1')
  upps_3year = subset(dl$upps, eventname == '3_year_follow_up_y_arm_1')
  substance_3year = subset(dl$substance, eventname == '3_year_follow_up_y_arm_1')
  path_3year = subset(dl$path, eventname == '3_year_follow_up_y_arm_1')
  ksads_3year = subset(dl$ksads, eventname == '3_year_follow_up_y_arm_1')
  alc_exp_3year = subset(dl$alc_exp, eventname == '3_year_follow_up_y_arm_1')
  
  outcome_3year_df[cbcl_3year$src_subject_id, 'extern'] = cbcl_3year$cbcl_scr_syn_external_t
  outcome_3year_df[cbcl_3year$src_subject_id, 'rbreak'] = cbcl_3year$cbcl_scr_syn_rulebreak_t
  outcome_3year_df[cbcl_3year$src_subject_id, 'agrs'] = cbcl_3year$cbcl_scr_syn_aggressive_t
  outcome_3year_df[cbcl_3year$src_subject_id, 'intern'] = cbcl_3year$cbcl_scr_syn_internal_t
  outcome_3year_df[cbcl_3year$src_subject_id, 'somatic'] = cbcl_3year$cbcl_scr_syn_somatic_t
  outcome_3year_df[cbcl_3year$src_subject_id, 'anx_dep'] = cbcl_3year$cbcl_scr_syn_anxdep_t
  outcome_3year_df[cbcl_3year$src_subject_id, 'with_dep'] = cbcl_3year$cbcl_scr_syn_withdep_t
  outcome_3year_df[bpm_3year$src_subject_id, 'extern_youth'] = bpm_3year$bpm_y_scr_external_t
  outcome_3year_df[bpm_3year$src_subject_id, 'intern_youth'] = bpm_3year$bpm_y_scr_internal_t
  outcome_3year_df[bpm_3year$src_subject_id, 'atten_youth'] = bpm_3year$bpm_y_scr_attention_t
  outcome_3year_df[upps_3year$src_subject_id, 'pos_urg'] = upps_3year$upps_y_ss_positive_urgency
  outcome_3year_df[upps_3year$src_subject_id, 'neg_urg'] = upps_3year$upps_y_ss_negative_urgency
  outcome_3year_df[substance_3year$src_subject_id, 'total_use_days'] = substance_3year$tlfb_cal_scr_tot_su_ud_cum
  outcome_3year_df[substance_3year$src_subject_id, 'total_use_events'] = substance_3year$tlfb_cal_scr_num_events
  outcome_3year_df[is.na(outcome_3year_df$total_use_days), 'total_use_days'] = 0
  outcome_3year_df[is.na(outcome_3year_df$total_use_events), 'total_use_events'] = 0
  outcome_3year_df$any_substance_use = (outcome_3year_df$total_use_events > 0)*1
  outcome_3year_df[path_3year$src_subject_id, 'curiosity_cigarette'] = curiosity_code[path_3year$path_alc_youth1a_l]
  outcome_3year_df[path_3year$src_subject_id, 'curiosity_vaping'] = curiosity_code[path_3year$path_alc_youth1b_l]
  outcome_3year_df[path_3year$src_subject_id, 'curiosity_alcohol'] = curiosity_code[path_3year$path_alc_youth2_l]
  outcome_3year_df[path_3year$src_subject_id, 'curiosity_marijuana'] = curiosity_code[path_3year$path_alc_youth3_l]
  outcome_3year_df[path_3year$src_subject_id, 'intent_cigarette'] = intent_code[path_3year$path_alc_youth4a_l]
  outcome_3year_df[path_3year$src_subject_id, 'intent_vaping'] = intent_code[path_3year$path_alc_youth4b_l]
  outcome_3year_df[path_3year$src_subject_id, 'intent_alcohol'] = intent_code[path_3year$path_alc_youth5_l]
  outcome_3year_df[path_3year$src_subject_id, 'intent_marijuana'] = intent_code[path_3year$path_alc_youth6_l]
  outcome_3year_df[ksads_3year$src_subject_id, 'conduct_disorder'] = (rowSums(ksads_3year[, conduct_disorder_vars], na.rm = TRUE) > 0)*1
  outcome_3year_df[alc_exp_3year$src_subject_id, 'alc_pos_expect'] = alc_exp_3year$aeq_positive_expectancies_ss
  outcome_3year_df[alc_exp_3year$src_subject_id, 'alc_neg_expect'] = alc_exp_3year$aeq_negative_expectancies_ss
  
  # ----- OUTCOME MEASURES (4-year) -----
  
  cbcl_4year = subset(dl$cbcl, eventname == '4_year_follow_up_y_arm_1')
  bpm_4year = subset(dl$bpm, eventname == '4_year_follow_up_y_arm_1')
  upps_4year = subset(dl$upps, eventname == '4_year_follow_up_y_arm_1')
  substance_4year = subset(dl$substance, eventname == '4_year_follow_up_y_arm_1')
  path_4year = subset(dl$path, eventname == '4_year_follow_up_y_arm_1')
  ksads_4year = subset(dl$ksads, eventname == '4_year_follow_up_y_arm_1')
  alc_exp_4year = subset(dl$alc_exp, eventname == '4_year_follow_up_y_arm_1')
  
  outcome_4year_df[cbcl_4year$src_subject_id, 'extern'] = cbcl_4year$cbcl_scr_syn_external_t
  outcome_4year_df[cbcl_4year$src_subject_id, 'rbreak'] = cbcl_4year$cbcl_scr_syn_rulebreak_t
  outcome_4year_df[cbcl_4year$src_subject_id, 'agrs'] = cbcl_4year$cbcl_scr_syn_aggressive_t
  outcome_4year_df[cbcl_4year$src_subject_id, 'intern'] = cbcl_4year$cbcl_scr_syn_internal_t
  outcome_4year_df[cbcl_4year$src_subject_id, 'somatic'] = cbcl_4year$cbcl_scr_syn_somatic_t
  outcome_4year_df[cbcl_4year$src_subject_id, 'anx_dep'] = cbcl_4year$cbcl_scr_syn_anxdep_t
  outcome_4year_df[cbcl_4year$src_subject_id, 'with_dep'] = cbcl_4year$cbcl_scr_syn_withdep_t
  outcome_4year_df[bpm_4year$src_subject_id, 'extern_youth'] = bpm_4year$bpm_y_scr_external_t
  outcome_4year_df[bpm_4year$src_subject_id, 'intern_youth'] = bpm_4year$bpm_y_scr_internal_t
  outcome_4year_df[bpm_4year$src_subject_id, 'atten_youth'] = bpm_4year$bpm_y_scr_attention_t
  outcome_4year_df[upps_4year$src_subject_id, 'pos_urg'] = upps_4year$upps_y_ss_positive_urgency
  outcome_4year_df[upps_4year$src_subject_id, 'neg_urg'] = upps_4year$upps_y_ss_negative_urgency
  outcome_4year_df[substance_4year$src_subject_id, 'total_use_days'] = substance_4year$tlfb_cal_scr_tot_su_ud_cum
  outcome_4year_df[substance_4year$src_subject_id, 'total_use_events'] = substance_4year$tlfb_cal_scr_num_events
  outcome_4year_df[is.na(outcome_4year_df$total_use_days), 'total_use_days'] = 0
  outcome_4year_df[is.na(outcome_4year_df$total_use_events), 'total_use_events'] = 0
  outcome_4year_df$any_substance_use = (outcome_4year_df$total_use_events > 0)*1
  outcome_4year_df[path_4year$src_subject_id, 'curiosity_cigarette'] = curiosity_code[path_4year$path_alc_youth1a_l]
  outcome_4year_df[path_4year$src_subject_id, 'curiosity_vaping'] = curiosity_code[path_4year$path_alc_youth1b_l]
  outcome_4year_df[path_4year$src_subject_id, 'curiosity_alcohol'] = curiosity_code[path_4year$path_alc_youth2_l]
  outcome_4year_df[path_4year$src_subject_id, 'curiosity_marijuana'] = curiosity_code[path_4year$path_alc_youth3_l]
  outcome_4year_df[path_4year$src_subject_id, 'intent_cigarette'] = intent_code[path_4year$path_alc_youth4a_l]
  outcome_4year_df[path_4year$src_subject_id, 'intent_vaping'] = intent_code[path_4year$path_alc_youth4b_l]
  outcome_4year_df[path_4year$src_subject_id, 'intent_alcohol'] = intent_code[path_4year$path_alc_youth5_l]
  outcome_4year_df[path_4year$src_subject_id, 'intent_marijuana'] = intent_code[path_4year$path_alc_youth6_l]
  outcome_4year_df[ksads_4year$src_subject_id, 'conduct_disorder'] = (rowSums(ksads_4year[, conduct_disorder_vars], na.rm = TRUE) > 0)*1
  outcome_4year_df[alc_exp_4year$src_subject_id, 'alc_pos_expect'] = alc_exp_4year$aeq_positive_expectancies_ss
  outcome_4year_df[alc_exp_4year$src_subject_id, 'alc_neg_expect'] = alc_exp_4year$aeq_negative_expectancies_ss
  
  # ----- Return a list containing all the data. -----
  return(list(task_behav_df = task_behav_df,
              neuro_df = neuro_df,
              info_df = info_df,
              outcome_2year_df = outcome_2year_df,
              outcome_3year_df = outcome_3year_df,
              outcome_4year_df = outcome_4year_df,
              dl = dl))
}

# Use the function just defined to actually import and process the data.
output = process_data()
View(head(output$task_behav_df))
View(head(output$neuro_df))
View(head(output$info_df))
View(head(output$outcome_2year_df))

# Print a table showing the percent of people with various numbers of missing task variables.
print(round(100*table(output$info_df$number_behav_missing)/nrow(output$info_df), 1))

# Export processed data.
write.csv(output$task_behav_df, 'processed data (release 5.0)/ABCD 2-year task behavioral data.csv', row.names = TRUE)
write.csv(output$info_df, 'processed data (release 5.0)/ABCD 2-year demographic info etc.csv', row.names = TRUE)
write.csv(output$neuro_df, 'processed data (release 5.0)/ABCD 2-year imaging data.csv', row.names = TRUE)
write.csv(output$outcome_2year_df, 'processed data (release 5.0)/ABCD 2-year outcome measures.csv', row.names = TRUE)
write.csv(output$outcome_3year_df, 'processed data (release 5.0)/ABCD 3-year outcome measures.csv', row.names = TRUE)
write.csv(output$outcome_4year_df, 'processed data (release 5.0)/ABCD 4-year outcome measures.csv', row.names = TRUE)