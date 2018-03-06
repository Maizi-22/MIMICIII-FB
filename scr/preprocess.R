# data preprocess
library(readr)
data.raw.sepsis <- read_csv("~/Documents/R-projects/MIMICIII_FB/data/cache_data/sepsis_02012018.csv")
mv.duration <- read_csv("~/Documents/R-projects/MIMICIII_FB/data/cache_data/mv_duration.csv")[, c(2,6)]
expire28 <- read_csv("~/Documents/R-projects/MIMICIII_FB/data/cache_data/expire28.csv")
# remove three record that may have wrong record in fluid balance
data.raw.sepsis <- data.raw.sepsis[which(data.raw.sepsis$subject_id != '234959' & data.raw.sepsis$icustay_id != '252601' & data.raw.sepsis$icustay_id != '235479'), ]

# Study cohort: all sepsis patients in mimic
# change age above 300 to 90
data.raw.sepsis$age[data.raw.sepsis$age >300] <- 90

# discard unuse data
data.raw.sepsis <- data.raw.sepsis[, -c(8, 10, 13:15, 17:19, 30, 37, 38, 44, 46, 48, 50, 52, 55, 57, 59, 61, 63, 65, 66, 68, 69)]

# convert gender to 0/1, 0 as famale, 1 as male
data.raw.sepsis$gender[which(data.raw.sepsis$gender == 'M')] <- 0 
data.raw.sepsis$gender[which(data.raw.sepsis$gender == 'F')] <- 1

# change expire flag to numeric
data.raw.sepsis$icu_expire_flag[which(data.raw.sepsis$icu_expire_flag == 'Y')] <- 1
data.raw.sepsis$icu_expire_flag[which(data.raw.sepsis$icu_expire_flag == 'N')] <- 0

# convert binary variable to factor
data.raw.sepsis[, c(5, 9, 14:22, 24:31, 37, 44)] <- lapply(data.raw.sepsis[, c(5, 9, 14:22, 24:31, 37, 44)], as.factor)

# convert 'LOS' variable to day
data.raw.sepsis$los_hospital <- data.raw.sepsis$los_hospital/24
data.raw.sepsis$los_icu <- data.raw.sepsis$los_icu/24

# add MV_duration to our dataset
data.raw.sepsis <- merge(data.raw.sepsis[, -11], mv.duration, by = 'hadm_id', all.x = TRUE)
colnames(data.raw.sepsis)[49] <- 'Mechanical_ventilation_duration'
data.raw.sepsis$Mechanical_ventilation_duration[is.na(data.raw.sepsis$Mechanical_ventilation_duration)] <- 0

# and 28-day expire flag
data.raw.sepsis <- merge(data.raw.sepsis, expire28[, c(1,4)], by = 'hadm_id', all.x = TRUE)
data.raw.sepsis$expire_flag_28[is.na(data.raw.sepsis$expire_flag_28)] <- 0
data.raw.sepsis$expire_flag_28 <- as.factor(data.raw.sepsis$expire_flag_28)

# remove three variables contains many missings
data.raw.sepsis <- data.raw.sepsis[, names(data.raw.sepsis) %in% c('height', 'neutrophils', 'lactate') == FALSE]

# save data
write.csv(data.raw.sepsis, '~/Documents/R-projects/MIMICIII_FB/data/finaldata/data_raw_sepsis.csv')

# data update
library(readr)
library(dplyr)
data.sepsis.new <- read_csv("~/Documents/R-projects/MIMICIII_FB/data/cache_data/final_data_25Jan2018.csv")
chart.data <- read_csv("~/Documents/R-projects/MIMICIII_FB/data/merge/chart_events_first.csv")[, -c(1,2)]
data.sepsis.new <- merge(data.sepsis.new, chart.data, by.x <- 'icustay_id', by.y = 'ICUSTAY_ID', all.x = T)
admloc <- read_csv("~/Documents/R-projects/MIMICIII_FB/data/merge/admit_loc.csv")[, -1]
data.sepsis.new <- merge(data.sepsis.new, admloc, by.x <- 'hadm_id', by.y = 'HADM_ID', all.x = T)
data.sepsis.new <- distinct(data.sepsis.new, hadm_id, .keep_all = T)
data.sepsis.new <- data.sepsis.new[, c("subject_id", "hadm_id", "icustay_id", "age", "gender"
                                       , "weight", "height", "admission_type", "los_hospital"
                                       , "first_hosp_stay", "hospital_expire_flag", "los_icu"
                                       , "first_careunit", "last_careunit", "first_icu_stay"
                                       , "duration_hours", "gcseyes", "gcsmotor", "gcsverbal"
                                       , "mingcs", "sofa", "diabetes", "hypertension"
                                       , "congestive_heart_failure", "renal_failure", "liver_disease"
                                       , "cancer", "aids", "chronic_pulmonary", "weight_loss", "obesity"
                                       , "oasis", "infection", "explicit_sepsis", "organ_dysfunction"
                                       , "mech_vent", "angus", "ADMISSION_LOCATION", "dialysis"
                                       , "eskd2", "eskd1", "eskd", "lactate", "lactate_uom"
                                       , "hemoglobin", "hemoglobin_uom", "creatinine", "creatinine_uom"
                                       , "wbc", "wbc_uom", "neutrophils", "neutrophils_uom"
                                       , "icu_expire_flag", "temp_value", "temp_uom", "resp_rate"
                                       , "resp_rate_uom", "heart_rate", "heart_rate_uom", "sys_bp"
                                       , "sys_bp_uom", "dias_bp", "dias_bp_uom", "mean_bp", "mean_bp_uom"
                                       , "fb3hr", "fb12hr", "fb24hr", "fb48hr", "fb72hr", "expire_flag_28" )]

# remove three record that may have wrong record in fluid balance
data.sepsis.new <- data.sepsis.new[which(data.sepsis.new$subject_id != '234959' & data.sepsis.new$icustay_id != '252601' & data.sepsis.new$icustay_id != '235479'), ]

# Study cohort: all sepsis patients in mimic
# change age above 300 to 90
data.sepsis.new$age[data.sepsis.new$age >300] <- 90

# discard unuse data
data.sepsis.new <- data.sepsis.new[, -c(8, 10, 13:15, 17:19, 30, 37, 38, 44, 46, 48, 50, 52, 55, 57, 59, 61, 63, 65)]

# convert gender to 0/1, 0 as famale, 1 as male
data.sepsis.new$gender[which(data.sepsis.new$gender == 'M')] <- 0 
data.sepsis.new$gender[which(data.sepsis.new$gender == 'F')] <- 1

# change expire flag to numeric
data.sepsis.new$icu_expire_flag[which(data.sepsis.new$icu_expire_flag == 'Y')] <- 1
data.sepsis.new$icu_expire_flag[which(data.sepsis.new$icu_expire_flag == 'N')] <- 0

# convert binary variable to factor
data.sepsis.new[, c(5, 9, 14:22, 24:31, 37, 44)] <- lapply(data.sepsis.new[, c(5, 9, 14:22, 24:31, 37, 44)], as.factor)

# convert 'LOS' variable to day
data.sepsis.new$los_hospital <- data.sepsis.new$los_hospital/24
data.sepsis.new$los_icu <- data.sepsis.new$los_icu/24

# add MV_duration to our dataset
mv.duration <- read_csv("~/Documents/R-projects/MIMICIII_FB/data/cache_data/mv_duration.csv")[, c(2,6)]
colnames(mv.duration)[2] <- 'Mechanical_ventilation_duration'
data.sepsis.new <- merge(data.sepsis.new, mv.duration, by = 'hadm_id', all.x = TRUE)
data.sepsis.new$duration_hours <- NULL
data.sepsis.new$Mechanical_ventilation_duration[is.na(data.sepsis.new$Mechanical_ventilation_duration)] <- 0

# set expire flag 28-days to 0 where exist null
expire28 <- read_csv("~/Documents/R-projects/MIMICIII_FB/data/cache_data/expire28.csv")[, c(1,3)]
data.sepsis.new$expire_flag_28 <- NULL
data.sepsis.new <- merge(data.sepsis.new, expire28, by = 'hadm_id', all.x = T)
data.sepsis.new$expire_flag_28[is.na(data.sepsis.new$expire_flag_28)] <- 0

# add pul
pul.data <- read_csv("~/Documents/R-projects/MIMICIII_FB/data/merge/source_of_infection.csv")[, -1]
data.sepsis.new <- merge(data.sepsis.new, pul.data, by.x = 'hadm_id', by.y = 'HADM_ID', all.x = T)

# remove three variables contains many missings
data.sepsis.new <- data.sepsis.new[, names(data.sepsis.new) %in% c('height', 'neutrophils', 'lactate') == FALSE]

# save data
write.csv(data.sepsis.new, '~/Documents/R-projects/MIMICIII_FB/data/finaldata/data_sepsis_update.csv')



