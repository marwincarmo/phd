library(lavaan)
library(dplyr)

## IGI
## 
dat <- read.csv("../data/clean_data2.csv")
mydata <- dplyr::filter(dat, redcap_event_name %in% c(
"elegibilidade_arm_1", 
"desfechos_arm_1", "followup_arm_1")
) |> 
  dplyr::mutate(
    redcap_event_name = factor(dplyr::case_when(
      redcap_event_name == "elegibilidade_arm_1" ~ 1,
      redcap_event_name == "desfechos_arm_1" ~ 2,
      redcap_event_name == "followup_arm_1" ~ 3
    )),
    randomizacao = dplyr::case_when(
      randomizacao ==  1 ~ "act",
      randomizacao ==  2 ~ "cbt",
      randomizacao ==  3 ~ "wl",
    ))

baseline <- mydata[mydata$redcap_event_name == 1,]

modIGIg <- 'g =~ igi_1a + igi_1b + igi_1c + igi_2 + igi_3 + igi_4 + igi_5'

fitIGIg <- cfa(modIGIg, data=baseline, std.lv = TRUE, estimator = 'DWLS', ordered = TRUE)
IGIalpha <- MBESS::ci.reliability(dplyr::select(baseline, igi_1a:igi_5), type = "alpha", interval.type = "parallel")

## Hospital Anxiety and Depression Scale (HADS)

modHADS <- '

Anx =~ ehad_1 + ehad_3 + ehad_5 + ehad_7 + ehad_9 + ehad_11 + ehad_13

Dep =~ ehad_2 + ehad_4 + ehad_6 + ehad_8 + ehad_10 + ehad_12 + ehad_14

'
fitHADS <- cfa(modHADS, data=baseline, std.lv = TRUE, estimator = 'DWLS', ordered = TRUE)
HADS_Aalpha <- MBESS::ci.reliability(dplyr::select(baseline, ehad_1, ehad_3, ehad_5, ehad_7, ehad_9, ehad_11, ehad_13), 
                                     type = "alpha", interval.type = "parallel")
HADS_Dalpha <- MBESS::ci.reliability(dplyr::select(baseline, ehad_2, ehad_4, ehad_6, ehad_8, ehad_10, ehad_12, ehad_14), 
                                     type = "alpha", interval.type = "parallel")

## Action Questionnaire-II (AAQ-II)

modAAQ <- '
    g =~ aaq_1 + aaq_2 + aaq_3 + aaq_4 + aaq_5 + aaq_6 + aaq_7
    '
fitAAQ <- cfa(modAAQ, data=baseline, std.lv = TRUE, estimator = 'DWLS', ordered = TRUE)

AAQalpha <- MBESS::ci.reliability(dplyr::select(baseline, aaq_1:aaq_7), type = "alpha", interval.type = "parallel")

## ## Dysfunctional Beliefs and Attitudes about Sleep (DBAS-16)

modDBAS <- '
cons =~ dbas_5 + dbas_7 + dbas_9 + dbas_12 + dbas_16
worry =~ dbas_3 + dbas_4 + dbas_8 + dbas_10 + dbas_11 + dbas_14
exp =~ dbas_1 + dbas_2
med =~ dbas_6 + dbas_13 + dbas_15
'
fitDBAS <- cfa(modDBAS, data=baseline, std.lv = TRUE, estimator = 'DWLS', ordered = TRUE)

DBASalpha_c <- MBESS::ci.reliability(dplyr::select(baseline, c(dbas_5,dbas_7, dbas_9, dbas_12, dbas_16)), 
                                     type = "alpha", interval.type = "parallel")
DBASalpha_w <- MBESS::ci.reliability(dplyr::select(baseline, c(dbas_3, dbas_4, dbas_8, dbas_10, dbas_11, dbas_14)), 
                                     type = "alpha", interval.type = "parallel")
DBASalpha_e <- MBESS::ci.reliability(dplyr::select(baseline, c(dbas_1, dbas_2)), type = "alpha", interval.type = "parallel")
DBASalpha_m <- MBESS::ci.reliability(dplyr::select(baseline, c(dbas_6, dbas_13, dbas_15)), 
                                     type = "alpha", interval.type = "parallel")
