source("paths.R")

library(dplyr)
library(lubridate)
library(readxl)

# Master list of enrolled participants and V1 dates
v1_visit_dates <- readRDS(file.path(path_raw_data, "2025-6-25-Affective Science MRT", "EMA-MRT", "v1.0.0", "Data for analysis", "v1_visit_dates.RDS"))

# Code book for EMA data
codebook_affsci_mrt <- read_excel(file.path(path_raw_data, "2025-6-25-Affective Science MRT", "EMA-MRT", "v1.0.0", "Data for analysis", "codebook_affsci_mrt.xlsx"))

# Data from two-question survey, micro-randomizations, and EMA
dat_affsci <- readRDS(file.path(path_raw_data, "2025-6-25-Affective Science MRT", "EMA-MRT", "v1.0.0", "Data for analysis", "data_final.RDS"))
dat_affsci_integers <- readRDS(file.path(path_raw_data, "2025-6-25-Affective Science MRT", "EMA-MRT", "v1.0.0", "Data for analysis", "data_final_integers.RDS"))

# Most updated version of the data
dat_demogs <- readRDS(file = file.path(path_raw_data, "2025-11-4-AffSci MRT Log Files, V1 Questionnaire & Data Demogs", "AffSci MRT Dat_Demogs", "dat_demogs.rds"))
codebook_dat_demogs <- read_excel(file.path(path_raw_data, "2025-11-4-AffSci MRT Log Files, V1 Questionnaire & Data Demogs", "AffSci MRT Dat_Demogs", "dat_demogs_codebook.xlsx"))

codebook_mrt <- read_excel(file.path(path_raw_data, "2025-11-4-AffSci MRT Log Files, V1 Questionnaire & Data Demogs", "AffSci MRT Questionnaire v1.0.0", "MRT Data for analysis", "codebook_mrt.xlsx"))
screening_quest_mrt <- readRDS(file.path(path_raw_data, "2025-11-4-AffSci MRT Log Files, V1 Questionnaire & Data Demogs", "AffSci MRT Questionnaire v1.0.0", "MRT Data for analysis", "screening_quest_mrt.rds"))
v1_baseline_quest_mrt <- readRDS(file.path(path_raw_data, "2025-11-4-AffSci MRT Log Files, V1 Questionnaire & Data Demogs", "AffSci MRT Questionnaire v1.0.0", "MRT Data for analysis", "v1_baseline_quest_mrt.rds"))

###############################################################################
# Data cleaning step for:
# One participant had one day with more than 6 blocks
# One participant had 2 days with more than 6 blocks
#
# Note that paths.R will have the values for 
# mock_mars_id_0001, mock_mars_id_0002
###############################################################################

dat_affsci <- dat_affsci %>%
  mutate(this_block0_datetime = case_when(
    (mars_id == mock_mars_id_0001) & (ec_study_day_int == 1) ~ mock_mars_id_0001_block0_day1,
    (mars_id == mock_mars_id_0002) & (ec_study_day_int == 1) ~ mock_mars_id_0002_block0_day1,
    (mars_id == mock_mars_id_0002) & (ec_study_day_int == 2) ~ mock_mars_id_0002_block0_day2,
    .default = NULL
  )) %>%
  mutate(this_interval = this_block0_datetime %--% ec_block_start_hrts_local) %>%
  mutate(this_interval_length_minutes = int_length(this_interval)/60) %>%
  mutate(this_interval_is_selected = case_when(
    is.na(this_block0_datetime) ~ TRUE,
    !(this_interval_length_minutes %in% c(0, 140, 280, 420, 560, 700)) ~ FALSE,
    .default = TRUE
  )) %>%
  select(this_block0_datetime, this_interval, this_interval_length_minutes, this_interval_is_selected, everything())

dat_affsci <- dat_affsci %>%
  filter(this_interval_is_selected == TRUE) %>%
  select(-this_block0_datetime, -this_interval, -this_interval_length_minutes, -this_interval_is_selected) %>%
  mutate(decision_point = 6*(ec_study_day_int - 1) + (ec_block_calc + 1)) %>%
  select(mars_id, decision_point, everything())

dat_combos <- expand.grid(mars_id = unique(dat_affsci[["mars_id"]]), decision_point = 1:60)
dat_combos <- dat_combos %>% arrange(mars_id, decision_point)
dat_affsci <- full_join(x = dat_combos, y = dat_affsci, by = join_by(mars_id, decision_point))
dat_affsci <- dat_affsci %>% mutate(ec_study_day_int = if_else(is.na(ec_study_day_int), ceiling(decision_point/6), ec_study_day_int))
dat_affsci <- dat_affsci %>% mutate(ec_block_calc = if_else(is.na(ec_block_calc), decision_point - 6*(ec_study_day_int - 1) -1, ec_block_calc))

###############################################################################
# Create demographics variables
###############################################################################

dat_demogs <- dat_demogs %>%
  mutate(has_partner = case_when(
    partner_status_category == "living with significant other" ~ 1,
    partner_status_category == "married" ~ 1,
    partner_status_category == "divorced" ~ 0,
    partner_status_category == "separated" ~ 0,
    partner_status_category == "single" ~ 0,
    partner_status_category == "widowed" ~ 0,
    .default = NULL
  )) %>%
  mutate(is_male = case_when(
    gender_category == "male" ~ 1,
    gender_category == "female" ~ 0,
    gender_category == "other" ~ 0,
    .default = NULL
  )) %>%
  mutate(is_latino = case_when(
    is_latino_category == "latino" ~ 1,
    is_latino_category == "not latino" ~ 0,
    .default = NULL
  )) %>%
  mutate(is_not_latino_and_black = case_when(
    (is_latino_category == "not latino") & (race_is_black_or_african_american == 1) ~ 1,
    (is_latino_category == "not latino") & (race_is_black_or_african_american == 0) ~ 0,
    is_latino_category == "latino" ~ 0,
    .default = NULL
  )) %>%
  mutate(is_not_latino_and_white = case_when(
    (is_latino_category == "not latino") & (race_is_white == 1) ~ 1,
    (is_latino_category == "not latino") & (race_is_white == 0) ~ 0,
    is_latino_category == "latino" ~ 0,
    .default = NULL
  )) %>%
  mutate(is_not_latino_and_other = case_when(
    (is_latino_category == "not latino") & (num_checked_race > 1) ~ 1,
    (is_latino_category == "not latino") & (num_checked_race == 1) & (race_is_asian == 1) ~ 1,
    (is_latino_category == "not latino") & (num_checked_race == 1) & (race_is_native_hawaiian_or_other_pacific_islander == 1) ~ 1,
    (is_latino_category == "not latino") & (num_checked_race == 1) & (race_is_american_indian_or_alaska_native == 1) ~ 1,
    (is_latino_category == "not latino") & (num_checked_race == 1) & (race_is_black_or_african_american == 1) ~ 0,
    (is_latino_category == "not latino") & (num_checked_race == 1) & (race_is_white == 1) ~ 0,
    is_latino_category == "latino" ~ 0,
    .default = NULL
  )) %>%
  # Handle coding for multiracial
  mutate(is_not_latino_and_black = replace(is_not_latino_and_black, is_not_latino_and_other == 1, 0),
         is_not_latino_and_white = replace(is_not_latino_and_white, is_not_latino_and_other == 1, 0)) %>%
  rename(age = age,
         is_male = is_male, 
         is_latino = is_latino, 
         is_not_latino_and_black = is_not_latino_and_black, 
         is_not_latino_and_white = is_not_latino_and_white, 
         is_not_latino_and_other = is_not_latino_and_other, 
         baseline_tobacco_history = baseline_tobacco_history, 
         has_partner = has_partner, 
         income_val = income_val)

dat_screener <- screening_quest_mrt %>%
  select(record_id, scr_age, scr_cpd, scr_sex, gender) %>%
  mutate(record_id = as.numeric(record_id)) %>%
  mutate(redcap_id = record_id, screener_age = scr_age, screener_cpd = scr_cpd, screener_sex = scr_sex, screener_gender = gender)

dat_v1_baseline <- v1_baseline_quest_mrt %>%
  select(record_id, dses1_1, dses1a_1) %>%
  mutate(record_id = as.numeric(record_id)) %>%
  mutate(redcap_id = record_id, baseline_gender = dses1_1, baseline_sex = dses1a_1)

dat_demogs <- left_join(x = dat_demogs, y = dat_screener, by = join_by("redcap_id" == "redcap_id"))
dat_demogs <- left_join(x = dat_demogs, y = dat_v1_baseline, by = join_by("redcap_id" == "redcap_id"))

# We perform a sanity check on the curated gender_category variable.
# If gender was reported at V1 (baseline) then gender_category uses that value.
# If gender was not reported at V1 (baseline) then gender_category uses what was reported in screener.
# However, in the screener assessment, the variable called "gender" actually describes sex at birth
# while the variable called "sex" actually describes gender.
# Here, we check whether among participants who did not report gender at V1 (baseline)
# whether they reported both "sex" and "gender" at screener and whether their
# reported "sex" and "gender" at screener were equivalent.
# The script below should output zero (0) for 
# n_missing_screener_sex, n_missing_screener_gender, n_not_equal
# if they are equivalent. We find that this is indeed the case, 
# confirming that the curated gender_category variable followed the intended construction.
dat_demogs %>%
  filter(is.na(baseline_gender)) %>%
  mutate(screener_gender_recoded = case_when(
    screener_gender == "M" ~ 1,
    screener_gender == "F" ~ 2,
    screener_gender == "I" ~ 3,
    (screener_gender == "U") | (screener_gender == -99) ~ NA_real_,
    .default = NA_real_
  )) %>%
  select(mars_id, redcap_id, screener_gender, screener_gender_recoded, screener_sex, baseline_gender, gender_category) %>%
  mutate(is_equal = if_else(screener_gender_recoded == screener_sex, 1, 0)) %>%
  summarise(n_missing_screener_sex = sum(is.na(screener_sex)),
            n_missing_screener_gender = sum(is.na(screener_gender)),
            n_not_equal = sum(screener_gender_recoded != screener_sex))


# # A tibble: 1 × 3
#          n_missing_screener_sex    n_missing_screener_gender     n_not_equal
#                           <int>                        <int>           <int>
# 1                             0                            0              0


dat_demogs <- dat_demogs %>%
  mutate(baseline_tobacco_history = if_else(is.na(baseline_tobacco_history), screener_cpd, baseline_tobacco_history))

# Create missing data indicators for counting number of participants 
# having missing values in the demographic variables
dat_demogs <- dat_demogs %>%
  mutate(is_missing_age = if_else(is.na(age), 1, 0),
         is_missing_gender = if_else(is.na(gender_category), 1, 0),
         is_missing_race_and_ethnicity = if_else(is.na(race_and_ethnicity), 1, 0),
         is_missing_baseline_tobacco_history = if_else(is.na(baseline_tobacco_history), 1, 0),
         is_missing_partner_status = if_else(is.na(partner_status_category), 1, 0),
         is_missing_income = if_else(is.na(income_val), 1, 0)) %>%
  mutate(is_missing_any_demog_data = if_else(is_missing_age + is_missing_gender + is_missing_race_and_ethnicity + is_missing_baseline_tobacco_history + is_missing_partner_status + is_missing_income >= 1, 1, 0))

###############################################################################
# Which participants did not have any mhealth data during 10-day MRT period?
###############################################################################

mars_ids_did_not_have_mhealth_data_within_mrt_period <- setdiff(x = v1_visit_dates[["mars_id"]], y = dat_affsci[["mars_id"]])

###############################################################################
# How many participants completed less than 3 EMA between Days 2-9?
# Exclude participants who completed less than 3 EMA between Days 2-9 from
# all downstream analysis.
###############################################################################

dat_summary_ncompleted_ema <- dat_affsci %>%
  filter((ec_study_day_int >= 2) & (ec_study_day_int <= 9)) %>%
  group_by(mars_id) %>%
  summarise(total_ema_administered = sum(ep_status != "undelivered"),
            total_ema_completed = sum(ep_status == "completed"),
            total_micro_randomizations = sum(!is.na(ep_emi_rand))) %>%
  arrange(total_ema_completed, total_micro_randomizations)

mars_ids_did_not_meet_ema_completion_criteria <- dat_summary_ncompleted_ema %>% 
  filter(total_ema_completed < 3) %>% 
  .[["mars_id"]]

dat_affsci <- dat_affsci %>% 
  filter(!(mars_id %in% mars_ids_did_not_meet_ema_completion_criteria))

###############################################################################
# Merge demographics to dataset
###############################################################################
dat_demogs_for_affsci <- dat_demogs %>% 
  filter(!(mars_id %in% mars_ids_did_not_have_mhealth_data_within_mrt_period)) %>%
  filter(!(mars_id %in% mars_ids_did_not_meet_ema_completion_criteria))

dat_affsci <- left_join(x = dat_affsci, y = dat_demogs_for_affsci, by = join_by("mars_id" == "mars_id"))

###############################################################################
# Process checks
###############################################################################

list_collect_process_checks <- list()

# Sanity check: randomization probabilities -----------------------------------

total_rand <- dat_affsci %>% filter(!is.na(ep_emi_rand)) %>% nrow(.)
total_none <- dat_affsci %>% filter(ep_emi_rand == "none") %>% nrow(.)
total_high_effort <- dat_affsci %>% filter(ep_emi_rand == "mars") %>% nrow(.)
total_low_effort <- dat_affsci %>% filter(ep_emi_rand == "low_effort") %>% nrow(.)

p_none <- total_none/total_rand
p_high_effort <- total_high_effort/total_rand
p_low_effort <- total_low_effort/total_rand

std_err_none <- sqrt(p_none * (1-p_none)/total_rand)
std_err_high_effort <- sqrt(p_high_effort * (1-p_high_effort)/total_rand)
std_err_low_effort <- sqrt(p_low_effort * (1-p_low_effort)/total_rand)

dat_summary_empirical_prob <- data.frame(condition = c("No Prompt", 
                                                       "High Effort Prompt", 
                                                       "Low Effort Prompt"),
                                         est_prob = c(p_none, 
                                                      p_high_effort, 
                                                      p_low_effort),
                                         LB95 = c(p_none - qnorm(0.975)*std_err_none, 
                                                  p_high_effort - qnorm(0.975)*std_err_high_effort, 
                                                  p_low_effort - qnorm(0.975)*std_err_low_effort),
                                         UB95 = c(p_none + qnorm(0.975)*std_err_none, 
                                                  p_high_effort + qnorm(0.975)*std_err_high_effort, 
                                                  p_low_effort + qnorm(0.975)*std_err_low_effort))

# Number of minutes elapsed ---------------------------------------------------
# From start of block to when 2qs was triggered

block_start_until_two_question_survey_triggered <- dat_affsci %>% 
  mutate(mins_elapsed = (ep_2qs_delivered_unix - ec_block_start_unix)/60) %>% 
  filter(ep_2qs_status != "undelivered") %>% 
  summarise(what = "From start of block to when 2qs was triggered",
            mean_mins = mean(mins_elapsed, na.rm=TRUE),
            q0 = quantile(mins_elapsed, probs = 0),
            q1 = quantile(mins_elapsed, probs = .01, na.rm = TRUE),
            q10 = quantile(mins_elapsed, probs = .10),
            q20 = quantile(mins_elapsed, probs = .20),
            q30 = quantile(mins_elapsed, probs = .30),
            q40 = quantile(mins_elapsed, probs = .40),
            q50 = quantile(mins_elapsed, probs = .50),
            q60 = quantile(mins_elapsed, probs = .60),
            q70 = quantile(mins_elapsed, probs = .70),
            q80 = quantile(mins_elapsed, probs = .80),
            q90 = quantile(mins_elapsed, probs = .90),
            q99 = quantile(mins_elapsed, probs = .99),
            q100 = quantile(mins_elapsed, probs = 1))

list_collect_process_checks <- append(list_collect_process_checks, list(block_start_until_two_question_survey_triggered))

# Number of minutes elapsed ---------------------------------------------------
# From when 2qs was triggered to when 2qs was completed or closed

two_question_survey_triggered_until_closed <- dat_affsci %>% 
  mutate(mins_elapsed = (ep_2qs_status_update_unix - ep_2qs_delivered_unix)/60) %>% 
  filter(ep_2qs_status != "undelivered") %>% 
  summarise(what = "From when 2qs was triggered to when 2qs was completed or closed",
            mean_mins = mean(mins_elapsed, na.rm=TRUE),
            q0 = quantile(mins_elapsed, probs = 0, na.rm = TRUE),
            q1 = quantile(mins_elapsed, probs = .01, na.rm = TRUE),
            q10 = quantile(mins_elapsed, probs = .10, na.rm = TRUE),
            q20 = quantile(mins_elapsed, probs = .20, na.rm = TRUE),
            q30 = quantile(mins_elapsed, probs = .30, na.rm = TRUE),
            q40 = quantile(mins_elapsed, probs = .40, na.rm = TRUE),
            q50 = quantile(mins_elapsed, probs = .50, na.rm = TRUE),
            q60 = quantile(mins_elapsed, probs = .60, na.rm = TRUE),
            q70 = quantile(mins_elapsed, probs = .70, na.rm = TRUE),
            q80 = quantile(mins_elapsed, probs = .80, na.rm = TRUE),
            q90 = quantile(mins_elapsed, probs = .90, na.rm = TRUE),
            q99 = quantile(mins_elapsed, probs = .99, na.rm = TRUE),
            q100 = quantile(mins_elapsed, probs = 1, na.rm = TRUE))

list_collect_process_checks <- append(list_collect_process_checks, list(two_question_survey_triggered_until_closed))

# Number of minutes elapsed ---------------------------------------------------
# From when 2qs was completed or closed to when micro-randomization occurred

two_question_survey_closed_until_rand <- dat_affsci %>% 
  mutate(mins_elapsed = (ep_rand_unix - ep_2qs_responded_unix)/60) %>% 
  filter(ep_2qs_status != "undelivered") %>% 
  summarise(what = "From when 2qs was completed or closed to micro-randomization",
            mean_mins = mean(mins_elapsed, na.rm=TRUE),
            q0 = quantile(mins_elapsed, probs = 0, na.rm = TRUE),
            q1 = quantile(mins_elapsed, probs = .01, na.rm = TRUE),
            q10 = quantile(mins_elapsed, probs = .10, na.rm = TRUE),
            q20 = quantile(mins_elapsed, probs = .20, na.rm = TRUE),
            q30 = quantile(mins_elapsed, probs = .30, na.rm = TRUE),
            q40 = quantile(mins_elapsed, probs = .40, na.rm = TRUE),
            q50 = quantile(mins_elapsed, probs = .50, na.rm = TRUE),
            q60 = quantile(mins_elapsed, probs = .60, na.rm = TRUE),
            q70 = quantile(mins_elapsed, probs = .70, na.rm = TRUE),
            q80 = quantile(mins_elapsed, probs = .80, na.rm = TRUE),
            q90 = quantile(mins_elapsed, probs = .90, na.rm = TRUE),
            q99 = quantile(mins_elapsed, probs = .99, na.rm = TRUE),
            q100 = quantile(mins_elapsed, probs = 1, na.rm = TRUE))

list_collect_process_checks <- append(list_collect_process_checks, list(two_question_survey_closed_until_rand))

# Number of minutes elapsed ---------------------------------------------------
# From when micro-randomization occurred to when EMA was delivered

rand_until_ema_triggered <- dat_affsci %>% 
  mutate(mins_elapsed = (ep_delivered_unix - ep_rand_unix)/60) %>% 
  filter(ep_2qs_status != "undelivered") %>% 
  summarise(what = "From micro-randomization to when EMA was delivered",
            mean_mins = mean(mins_elapsed, na.rm=TRUE),
            q0 = quantile(mins_elapsed, probs = 0, na.rm = TRUE),
            q1 = quantile(mins_elapsed, probs = .01, na.rm = TRUE),
            q10 = quantile(mins_elapsed, probs = .10, na.rm = TRUE),
            q20 = quantile(mins_elapsed, probs = .20, na.rm = TRUE),
            q30 = quantile(mins_elapsed, probs = .30, na.rm = TRUE),
            q40 = quantile(mins_elapsed, probs = .40, na.rm = TRUE),
            q50 = quantile(mins_elapsed, probs = .50, na.rm = TRUE),
            q60 = quantile(mins_elapsed, probs = .60, na.rm = TRUE),
            q70 = quantile(mins_elapsed, probs = .70, na.rm = TRUE),
            q80 = quantile(mins_elapsed, probs = .80, na.rm = TRUE),
            q90 = quantile(mins_elapsed, probs = .90, na.rm = TRUE),
            q99 = quantile(mins_elapsed, probs = .99, na.rm = TRUE),
            q100 = quantile(mins_elapsed, probs = 1, na.rm = TRUE))

list_collect_process_checks <- append(list_collect_process_checks, list(rand_until_ema_triggered))

# Number of minutes elapsed ---------------------------------------------------
# From when EMA was delivered and when EMA was completed or closed

ema_triggered_until_closed <- dat_affsci %>% 
  mutate(mins_elapsed = (ep_end_unix - ep_delivered_unix)/60) %>% 
  filter(ep_status != "undelivered") %>% 
  summarise(what = "From when EMA was delivered and when EMA was completed or closed",
            mean_mins = mean(mins_elapsed, na.rm=TRUE),
            q0 = quantile(mins_elapsed, probs = 0, na.rm=TRUE),
            q1 = quantile(mins_elapsed, probs = .01, na.rm = TRUE),
            q10 = quantile(mins_elapsed, probs = .10, na.rm=TRUE),
            q20 = quantile(mins_elapsed, probs = .20, na.rm=TRUE),
            q30 = quantile(mins_elapsed, probs = .30, na.rm=TRUE),
            q40 = quantile(mins_elapsed, probs = .40, na.rm=TRUE),
            q50 = quantile(mins_elapsed, probs = .50, na.rm=TRUE),
            q60 = quantile(mins_elapsed, probs = .60, na.rm=TRUE),
            q70 = quantile(mins_elapsed, probs = .70, na.rm=TRUE),
            q80 = quantile(mins_elapsed, probs = .80, na.rm=TRUE),
            q90 = quantile(mins_elapsed, probs = .90, na.rm=TRUE),
            q99 = quantile(mins_elapsed, probs = .99, na.rm = TRUE),
            q100 = quantile(mins_elapsed, probs = 1, na.rm=TRUE))

list_collect_process_checks <- append(list_collect_process_checks, list(ema_triggered_until_closed))

# Create a dataframe with the results of the process checks -------------------

dat_collect_process_checks <- bind_rows(list_collect_process_checks)

###############################################################################
# Create variables to be used in analysis
###############################################################################

# Participant ID must be is numeric format (not character) when performing analysis --
dat_affsci <- dat_affsci %>% mutate(participant_id = as.numeric(substring(mars_id, first = 9))) 

# Create zero-indexed study day variable --------------------------------------
dat_affsci <- dat_affsci %>% mutate(cluster_id = ec_study_day_int - 1)

# Create continuous hour of day variable and continuous day in MRT variable
# based on timestamp of when micro-randomization occurred --------------------
dat_affsci <- dat_affsci %>%
  mutate(hour_coinflip_local = hour(ep_rand_hrts_local) + minute(ep_rand_hrts_local)/60 + second(ep_rand_hrts_local)/(60*60)) %>%
  mutate(days_between_v1_and_coinflip_local = int_length(v_first_day_hrts_local %--% ep_rand_hrts_local)/(24*60*60))

# Eligibility (availability) for micro-randomization --------------------------
dat_affsci <- dat_affsci %>%
  mutate(eligibility = if_else(!is.na(ep_emi_rand), 1, 0))

# Treatment indicators --------------------------------------------------------
dat_affsci <- dat_affsci %>%
  mutate(coinflip = if_else(ep_emi_rand != "none", 1, 0),
         is_high_effort = if_else(ep_emi_rand == "mars", 1, 0),
         is_low_effort = if_else(ep_emi_rand == "low_effort", 1, 0))

# Primary proximal outcome ----------------------------------------------------
# Engagement with self-regulatory strategies reported in EMA

construct_primary_proximal_outcome <- function(cleaned_data_frame, q1_var_name, q2_var_name, q3_var_name){
  
  q1_response_value <- cleaned_data_frame[[q1_var_name]]
  q2_response_value <- cleaned_data_frame[[q2_var_name]]
  q3_response_value <- cleaned_data_frame[[q3_var_name]]
  
  primary_proximal_outcome_value <- case_when(
    !is.na(q1_response_value) & !is.na(q2_response_value) & !is.na(q3_response_value) & (q1_response_value=="Yes" | q2_response_value=="Yes" | q3_response_value=="Yes") ~ 1,
    !is.na(q1_response_value) & !is.na(q2_response_value) & !is.na(q3_response_value) & (q1_response_value=="No" & q2_response_value=="No" & q3_response_value=="No") ~ 0,
    !is.na(q1_response_value) & (is.na(q2_response_value) | is.na(q3_response_value)) & (q1_response_value=="Yes") ~ 1,
    !is.na(q2_response_value) & (is.na(q1_response_value) | is.na(q3_response_value)) & (q2_response_value=="Yes") ~ 1,
    !is.na(q3_response_value) & (is.na(q1_response_value) | is.na(q2_response_value)) & (q3_response_value=="Yes") ~ 1,
    .default = NULL
  )
  
  return(primary_proximal_outcome_value)
}

dat_affsci[["Y"]] <- construct_primary_proximal_outcome(cleaned_data_frame = dat_affsci, 
                                                        q1_var_name = "ei_mars_use", 
                                                        q2_var_name = "ei_mars_other_use", 
                                                        q3_var_name = "ei_non_mars_use")

# Create a variable for the value of the primary proximal outcome at the
# most recent eligible decision point and create an indicator for whether there 
# was any eligible decision point since the start of the study -----------------
dat_affsci[["decision_points_most_recent_eligible"]] <- NA
list_all_dat <- list()

all_ids <- unique(dat_affsci[["mars_id"]])
n_ids <- length(all_ids)

for(i in 1:n_ids){
  current_participant <- all_ids[i]
  dat_current_participant <- dat_affsci %>% filter(mars_id == current_participant)
  n_blocks <- nrow(dat_current_participant)
  arr_all_elig_indices <- which(dat_current_participant[["eligibility"]] == 1)
  
  dat_current_participant[["any_recent_eligible_dp"]] <- NA
  dat_current_participant[["decision_points_most_recent_eligible"]] <- NA
  dat_current_participant[["engagement_most_recent_eligible"]] <- NA
  
  for(current_row_number in 1:n_blocks){
    is_rand <- if_else(!is.na(dat_current_participant[current_row_number,"ep_rand_hrts_local"]), TRUE, FALSE)
    
    if(is_rand == TRUE){
      arr_found <- arr_all_elig_indices < current_row_number
      any_found <- if_else(sum(arr_found) == 0, 0, 1)
      if(any_found == 1){
        these_indices_found <- arr_all_elig_indices[arr_found]
        the_matched_index <- max(these_indices_found)
      }else{
        the_matched_index <- NA_real_
      }
      dat_current_participant[["decision_points_most_recent_eligible"]][current_row_number] <- the_matched_index
      
      if(!is.na(the_matched_index)){
        dat_current_participant[["any_recent_eligible_dp"]][current_row_number] <- 1
        dat_current_participant[["engagement_most_recent_eligible"]][current_row_number] <- dat_current_participant[["Y"]][the_matched_index]
      }else{
        dat_current_participant[["any_recent_eligible_dp"]][current_row_number] <- 0
      }
    }
  }
  
  list_all_dat <- append(list_all_dat, list(dat_current_participant))
}

dat_affsci <- bind_rows(list_all_dat)

# Whether 2qs had any response ------------------------------------------------

dat_affsci <- dat_affsci %>%
  mutate(any_response_2qs = if_else((!is.na(ei_2qs_cig_avail)) | (!is.na(ei_2qs_negaff)), 1, 0))

# Substance use variables ----------------------------------------------------- 

dat_affsci <- dat_affsci %>%
  mutate(substance_is_cigarettes = ei_cig,
         substance_is_any_nicotine = if_else(ei_cig + ei_ecig + ei_cigar + ei_smokeless_tob >= 1, 1, 0),
         substance_is_any_tobacco = if_else(ei_cig + ei_cigar + ei_smokeless_tob >= 1, 1, 0))

# Rearrange variables --------------------------------------------------------- 

dat_affsci <- dat_affsci %>%
  select(mars_id, participant_id, cluster_id, decision_point,
         any_response_2qs,
         eligibility, hour_coinflip_local, days_between_v1_and_coinflip_local,
         coinflip, is_high_effort, is_low_effort, Y,
         any_recent_eligible_dp, decision_points_most_recent_eligible, engagement_most_recent_eligible,
         substance_is_cigarettes, substance_is_any_nicotine, substance_is_any_tobacco,
         age,
         is_male, 
         is_latino, 
         is_not_latino_and_black, 
         is_not_latino_and_white, 
         is_not_latino_and_other, 
         baseline_tobacco_history, 
         has_partner, 
         income_val,
         everything())

dat_primary_aim <- dat_affsci %>%
  select(mars_id, participant_id, cluster_id, decision_point,
         any_response_2qs,
         eligibility, hour_coinflip_local, days_between_v1_and_coinflip_local,
         coinflip, is_high_effort, is_low_effort, Y,
         any_recent_eligible_dp, decision_points_most_recent_eligible, engagement_most_recent_eligible,
         substance_is_cigarettes, substance_is_any_nicotine, substance_is_any_tobacco,
         age,
         is_male, 
         is_latino, 
         is_not_latino_and_black, 
         is_not_latino_and_white, 
         is_not_latino_and_other,
         baseline_tobacco_history, 
         has_partner, 
         income_val)

###############################################################################
# Save process checks
###############################################################################

is_dir_exist <- file.exists(file.path("process-checks"))

if(isFALSE(is_dir_exist)){
  dir.create(file.path("process-checks"))
}

write.csv(dat_summary_empirical_prob, file.path("process-checks", "dat_summary_empirical_prob.csv"), row.names = FALSE)
write.csv(dat_collect_process_checks, file.path("process-checks", "dat_collect_process_checks.csv"), row.names = FALSE)

################################################################################
# Calculate summary statistics
################################################################################
dat_summary_missing_demogs <- dat_demogs_for_affsci %>%
  summarise(n_participants = n(),
            n_participants_with_missing_demogs = sum(is_missing_any_demog_data),
            n_missing_age = sum(is_missing_age),
            n_missing_gender = sum(is_missing_gender),
            n_missing_race_and_ethnicity = sum(is_missing_race_and_ethnicity),
            n_missing_baseline_tobacco_history = sum(is_missing_baseline_tobacco_history),
            n_missing_partner_status = sum(is_missing_partner_status),
            n_missing_income = sum(is_missing_income))

dat_summary_demogs_continuous <- dat_demogs_for_affsci %>%
  summarise(m_age = mean(age),
            sd_age = sd(age),
            m_baseline_tobacco_history = mean(baseline_tobacco_history, na.rm = TRUE),
            sd_baseline_tobacco_history = sd(baseline_tobacco_history, na.rm = TRUE),
            m_income = mean(income_val, na.rm = TRUE),
            sd_income = sd(income_val, na.rm = TRUE))

dat_summary_demogs_binary <- dat_demogs_for_affsci %>%
  summarise(n_male = sum(is_male),
            pct_male = mean(is_male) * 100,
            n_latino = sum(is_latino),
            pct_latino = mean(is_latino) * 100,
            n_not_latino = sum(is_latino == 0),
            pct_not_latino = mean(is_latino == 0),
            n_not_latino_and_black = sum(is_not_latino_and_black),
            pct_not_latino_and_black = mean(is_not_latino_and_black) * 100,
            n_not_latino_and_other = sum(is_not_latino_and_other),
            pct_not_latino_and_other = mean(is_not_latino_and_other) * 100,
            n_not_latino_and_white = sum(is_not_latino_and_white),
            pct_not_latino_and_white = mean(is_not_latino_and_white) * 100,
            n_has_partner = sum(has_partner, na.rm = TRUE),
            pct_has_partner = mean(has_partner, na.rm = TRUE) * 100)

dat_summary_income_tabulation <- dat_demogs_for_affsci %>% group_by(income_val) %>% summarise(num_participants = n())

################################################################################
# Save output in csv format
################################################################################
is_dir_exist <- file.exists(file.path("analysis-complete-case", "formatted-output"))

if(isFALSE(is_dir_exist)){
  dir.create(file.path("analysis-complete-case", "formatted-output"))
}

is_dir_exist <- file.exists(file.path("analysis-complete-case", "formatted-output", "summary-statistics"))

if(isFALSE(is_dir_exist)){
  dir.create(file.path("analysis-complete-case", "formatted-output", "summary-statistics"))
}

write.csv(dat_summary_missing_demogs, file.path("analysis-complete-case", "formatted-output", "summary-statistics", "dat_summary_missing_demogs.csv"), row.names = FALSE)
write.csv(dat_summary_demogs_continuous, file.path("analysis-complete-case", "formatted-output", "summary-statistics", "dat_summary_demogs_continuous.csv"), row.names = FALSE)
write.csv(dat_summary_demogs_binary, file.path("analysis-complete-case", "formatted-output", "summary-statistics", "dat_summary_demogs_binary.csv"), row.names = FALSE)
write.csv(dat_summary_income_tabulation, file.path("analysis-complete-case", "formatted-output", "summary-statistics", "dat_summary_income_tabulation.csv"), row.names = FALSE)

