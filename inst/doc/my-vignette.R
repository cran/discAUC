## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup, warning = FALSE, message = FALSE----------------------------------
#Load discounting AUC package
library(discAUC)

#Load dplyr, which aids in data manipulation
library(dplyr)

## -----------------------------------------------------------------------------
#Example Tidy Delay Discounting Data
examp_DD

## ----filter_examples----------------------------------------------------------
#Filter example DD data by subject (relies on dplyr library)
examp_DD %>%
  filter(subject == 103)

#Filter example DD data by outcome type
examp_DD %>%
  filter(outcome=="alcohol")

## ----med_indiff---------------------------------------------------------------
#Subject -987.987 are precalculated median indifference points for each outcome.
DD_med_indiff = examp_DD %>%
                filter(subject == -987.987,
                outcome == "$100 Gain")

PD_med_indiff = examp_PD %>%
                filter(subject == -987.987,
                outcome == "$100 Gain")

#Note that the median indifference point subject number (-987.987) is truncated in the output.
DD_med_indiff

PD_med_indiff

## ----DD_simple----------------------------------------------------------------
AUC(dat = DD_med_indiff,
    indiff = "prop_indiff",
    x_axis = "delay_months",
    amount = 1,
    grouping = "subject")

## ----PD_simple----------------------------------------------------------------
AUC(dat = PD_med_indiff,
    indiff = "prop_indiff",
    x_axis = "prob",
    amount = 1,
    grouping = "subject",
    prob_disc = TRUE)

## ----AUCord-------------------------------------------------------------------
#Ordinal AUC for DD data
AUC(dat = DD_med_indiff,
    indiff = "prop_indiff",
    x_axis = "delay_months",
    amount = 1,
    grouping = "subject",
    type = "ordinal")

#Ordinal AUC for PD data
AUC(dat = PD_med_indiff,
    indiff = "prop_indiff",
    x_axis = "prob",
    amount = 1,
    groupings = "subject",
    prob_disc = TRUE,
    type = "ordinal")

## ----AUClog-------------------------------------------------------------------
#Ordinal AUC for DD data
AUC(dat = DD_med_indiff,
    indiff = "prop_indiff",
    x_axis = "delay_months",
    amount = 1,
    grouping = "subject",
    type = "log")

#Ordinal AUC for PD data with log base 10
AUC(dat = PD_med_indiff,
    indiff = "prop_indiff",
    x_axis = "prob",
    amount = 1,
    groupings = "subject",
    prob_disc = TRUE,
    type = "log",
    log_base = 10)

## ----one_factor_groups--------------------------------------------------------
#For demonstration, filter for median indifference points for all outcomes.
DD_med_outcomes = examp_DD %>%
              filter(subject == -987.987)

#Simple AUC
AUC(dat = DD_med_outcomes,
    indiff = "prop_indiff",
    x_axis = "delay_months",
    amount = 1,
    grouping = "outcome")

## ----mult_grouping------------------------------------------------------------
#AUC by outcome and subject
AUC(dat = examp_DD,
    indiff = "prop_indiff",
    x_axis = "delay_months",
    amount = 1,
    grouping = c("outcome", "subject"))


## ----mult_grouping2-----------------------------------------------------------
#AUC by outcome and subject
AUC(dat = examp_DD,
    indiff = "prop_indiff",
    x_axis = "delay_months",
    amount = 1,
    grouping = c("outcome", "subject"))

#AUC by subject and outcome
AUC(dat = examp_DD,
    indiff = "prop_indiff",
    x_axis = "delay_months",
    amount = 1,
    grouping = c("subject", "outcome"))

## ----AUC_zeros----------------------------------------------------------------
#Examp_DD data did not include indifference points when delay = 0
AUC_zeros(dat = examp_DD,
          indiff = "prop_indiff",
          x_axis = "delay_months",
          amount = 1,
          groupings = c("subject","outcome"))

## ----AUC_zeros_prob-----------------------------------------------------------
#Examp_PD data did not include indifference points when prob = 1
AUC_zeros(dat = examp_PD,
          indiff = "prop_indiff",
          x_axis = "prob",
          amount = 1,
          groupings = c("subject","outcome"),
          prob_disc = TRUE
          )

## ----odds_against-------------------------------------------------------------

#Make sure to indicate groupings, if necessary
prep_odds_against(dat = examp_PD,
                  x_axis = "prob",
                  groupings = c("subject","outcome"))


## ----prep_ordinals------------------------------------------------------------

#Groupings must be specified, if necessary
prep_ordinal(dat = examp_DD,
             x_axis = "delay_months",
             groupings = c("subject","outcome"))

## ----prep_ordinals_prob-------------------------------------------------------

prep_ordinal(dat = examp_PD,
             x_axis = "prob",
             groupings = c("subject","outcome"),
             prob_disc = TRUE)


## ----prep_ordinal_all_create_data---------------------------------------------

#Create data based on values included in above example
examp_ord_all = 
  tibble(
    sub = c(1, 1, 1, 2, 2, 2),
    delay_weeks = c(1, 4, 26, 1, 13, 52)
    )

#Groupings are not necessary
prep_ordinal_all(dat = examp_ord_all,
                 x_axis = "delay_weeks")

## ----log_corr-----------------------------------------------------------------

prep_log_AUC(dat = examp_DD,
             x_axis = "delay_months",
             type = "corr")

## ----log_corr_2---------------------------------------------------------------

prep_log_AUC(dat = examp_DD,
             x_axis = "delay_months",
             type = "corr",
             correction = .25)

## ----log_adjust_transformed values--------------------------------------------

#Initial vector with delays (in weeks)
delays = c(0, 0.25, 1, 4, 26)

#Log transform delays
log_delays = log(delays, base = 2)

#Display values
log_delays

#Eliminate log(0) = -Inf
non_zero_log = log_delays[-1]

#Calculate the difference between succesive non-zero indifference points
log_diff = diff(non_zero_log)

#Print log_diff
log_diff

#Adjustment factor
adjustment = mean(log_diff)

#Adjust log delays
new_log_delays = log_delays + adjustment

#Set log(0) = 0
new_log_delays[1] = 0

#Print adjusted log delays
new_log_delays


## ----log_adjust---------------------------------------------------------------

#Default adjust method
prep_log_AUC(dat = examp_DD,
             x_axis = "delay_months",
             type = "adjust")

## ----log_adjust_no_dec--------------------------------------------------------

#Log adjust method with no decimal correction
prep_log_AUC(dat = examp_DD,
             x_axis = "delay_months",
             type = "adjust",
             dec_offset = FALSE)

## ----log_IHS------------------------------------------------------------------

#IHS transformation
prep_log_AUC(dat = examp_DD,
             x_axis = "delay_months",
             type = "IHS")

## ----combinations-------------------------------------------------------------

#Calculate odds against
examp_PD_odds = prep_odds_against(dat = examp_PD,
                  x_axis = "prob",
                  groupings = c("subject","outcome"))

#Print odds against
examp_PD_odds

#Add zeros, but already converted to odds against so prob_disc = FALSE
#Note the x_axis value was changed to "prob_against" which was the newly added column
examp_PD_odds = AUC_zeros(dat = examp_PD_odds,
          x_axis = "prob_against",
          indiff = "prop_indiff",
          amount = 1,
          groupings = c("subject","outcome"),
          prob_disc = FALSE)

#Print odds agianst with zeros
examp_PD_odds

#Odds against to log_odds against.
#Note the x_axis value was changed to "prob_against" which was the newly added column
examp_PD_log_odds = prep_log_AUC(dat = examp_PD_odds,
                                 x_axis = "prob_against",
                                 type = "adjust")

examp_PD_log_odds

## ----auc----------------------------------------------------------------------

AUC(dat = examp_PD_log_odds,
    indiff = "prop_indiff",
    x_axis = "log_prob_against",
    amount = 1,
    groupings = c("subject","outcome"))

