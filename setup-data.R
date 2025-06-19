source("paths.R")

library(dplyr)
library(lubridate)
library(readxl)

# Code book
codebook_affsci_mrt <- read_excel(path = file.path(path_raw_data, "codebook_affsci_mrt.xlsx"))

# Master list of enrolled participants and V1 dates
v1_visit_dates <- readRDS(file = file.path(path_raw_data, "v1_visit_dates.RDS"))

# Data from two-question survey, micro-randomizations, and EMA
dat_affsci <- readRDS(file = file.path(path_raw_data, "data_final.RDS"))
dat_affsci_integers <- readRDS(file = file.path(path_raw_data, "data_final_integers.RDS"))

# Demographics data
# --- ADD CODE HERE ---

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
  mutate(mins_elapsed = (ep_2qs_start_unix - ec_block_start_unix)/60) %>% 
  filter(ep_2qs_status != "undelivered") %>% 
  summarise(what = "From start of block to when 2qs was triggered",
            mean_mins = mean(mins_elapsed, na.rm=TRUE),
            q0 = quantile(mins_elapsed, probs = 0),
            q10 = quantile(mins_elapsed, probs = .10),
            q20 = quantile(mins_elapsed, probs = .20),
            q30 = quantile(mins_elapsed, probs = .30),
            q40 = quantile(mins_elapsed, probs = .40),
            q50 = quantile(mins_elapsed, probs = .50),
            q60 = quantile(mins_elapsed, probs = .60),
            q70 = quantile(mins_elapsed, probs = .70),
            q80 = quantile(mins_elapsed, probs = .80),
            q90 = quantile(mins_elapsed, probs = .90),
            q100 = quantile(mins_elapsed, probs = 1))

list_collect_process_checks <- append(list_collect_process_checks, list(block_start_until_two_question_survey_triggered))

# Number of minutes elapsed ---------------------------------------------------
# From when 2qs was triggered to when 2qs was completed or closed
two_question_survey_triggered_until_closed <- dat_affsci %>% 
  mutate(mins_elapsed = (ep_2qs_end_unix - ep_2qs_start_unix)/60) %>% 
  filter(ep_2qs_status != "undelivered") %>% 
  summarise(what = "From when 2qs was triggered to when 2qs was completed or closed",
            mean_mins = mean(mins_elapsed, na.rm=TRUE),
            q0 = quantile(mins_elapsed, probs = 0, na.rm=TRUE),
            q10 = quantile(mins_elapsed, probs = .10, na.rm=TRUE),
            q20 = quantile(mins_elapsed, probs = .20, na.rm=TRUE),
            q30 = quantile(mins_elapsed, probs = .30, na.rm=TRUE),
            q40 = quantile(mins_elapsed, probs = .40, na.rm=TRUE),
            q50 = quantile(mins_elapsed, probs = .50, na.rm=TRUE),
            q60 = quantile(mins_elapsed, probs = .60, na.rm=TRUE),
            q70 = quantile(mins_elapsed, probs = .70, na.rm=TRUE),
            q80 = quantile(mins_elapsed, probs = .80, na.rm=TRUE),
            q90 = quantile(mins_elapsed, probs = .90, na.rm=TRUE),
            q100 = quantile(mins_elapsed, probs = 1, na.rm=TRUE))

list_collect_process_checks <- append(list_collect_process_checks, list(two_question_survey_triggered_until_closed))

# Number of minutes elapsed ---------------------------------------------------
# From when 2qs was completed or closed to when micro-randomization occurred


# --- ADD CODE HERE ---




# Number of minutes elapsed ---------------------------------------------------
# From when micro-randomization occurred to when EMA was delivered


# --- ADD CODE HERE ---




# Number of minutes elapsed ---------------------------------------------------
# From when EMA was delivered and when EMA was completed or closed
ema_triggered_until_closed <- dat_affsci %>% 
  mutate(mins_elapsed = (ep_end_unix - ep_start_unix)/60) %>% 
  filter(ep_status != "undelivered") %>% 
  summarise(what = "From when EMA was delivered and when EMA was completed or closed",
            mean_mins = mean(mins_elapsed, na.rm=TRUE),
            q0 = quantile(mins_elapsed, probs = 0, na.rm=TRUE),
            q10 = quantile(mins_elapsed, probs = .10, na.rm=TRUE),
            q20 = quantile(mins_elapsed, probs = .20, na.rm=TRUE),
            q30 = quantile(mins_elapsed, probs = .30, na.rm=TRUE),
            q40 = quantile(mins_elapsed, probs = .40, na.rm=TRUE),
            q50 = quantile(mins_elapsed, probs = .50, na.rm=TRUE),
            q60 = quantile(mins_elapsed, probs = .60, na.rm=TRUE),
            q70 = quantile(mins_elapsed, probs = .70, na.rm=TRUE),
            q80 = quantile(mins_elapsed, probs = .80, na.rm=TRUE),
            q90 = quantile(mins_elapsed, probs = .90, na.rm=TRUE),
            q100 = quantile(mins_elapsed, probs = 1, na.rm=TRUE))

list_collect_process_checks <- append(list_collect_process_checks, list(ema_triggered_until_closed))

# Create a dataframe with the results of the process checks -------------------
dat_collect_process_checks <- bind_rows(list_collect_process_checks)

###############################################################################
# Merge demographics to dataset
###############################################################################


# --- ADD CODE HERE ---




###############################################################################
# Create variables to be used in analysis
###############################################################################

# Participant ID must be is numeric format (not character) when performing analysis --
dat_affsci <- dat_affsci %>% mutate(participant_id = as.numeric(substring(mars_id, first = 9))) 

# Create zero-indexed study day variable --------------------------------------
dat_affsci <- dat_affsci %>% mutate(cluster_id = ec_study_day_int - 1)

# Create continuous hour of day variable and continuous day in MRT variable
# based on timestamp of when micro-randomization occurred --------------------


# --- ADD CODE HERE ---




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

# Create an indicator for whether there was any eligible decision point 
# since the start of the study ------------------------------------------------


# --- ADD CODE HERE ---




# Create a variable for the value of the primary proximal outcome at the
# most recent eligible decision point -----------------------------------------


# --- ADD CODE HERE ---




# Substance use variables ----------------------------------------------------- 

dat_affsci <- dat_affsci %>%
  mutate(substance_is_cigarettes = ei_cig,
         substance_is_any_nicotine = if_else(ei_cig + ei_ecig + ei_cigar + ei_smokeless_tob >= 1, 1, 0),
         substance_is_any_tobacco = if_else(ei_cig + ei_cigar + ei_smokeless_tob >= 1, 1, 0))

# Rearrange variables --------------------------------------------------------- 

dat_affsci <- dat_affsci %>%
  select(mars_id, participant_id, cluster_id, decision_point,
         eligibility, coinflip, is_high_effort, is_low_effort, Y,
         substance_is_cigarettes, substance_is_any_nicotine, substance_is_any_tobacco,
         everything())

