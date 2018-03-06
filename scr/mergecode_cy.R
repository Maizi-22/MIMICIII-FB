rm(list = ls())
library(data.table)
wkdir <- ''
setwd(wkdir)
files <- dir('data_merge',full.names = TRUE) # I have put expire28 and mv_duration all in the data merge folder
merged_data <- lapply(files, function(file){
  data <- fread(file)
  colnames(data) <- tolower(colnames(data))
  data 
})
#merged_data <- fread('datamerged_data_27112017.csv')
data <- merge(merged_data[[1]],merged_data[[3]], by = c('subject_id','hadm_id'), all.x = TRUE, all.y = TRUE)
data <- merge(merged_data[[4]],data, by = c('subject_id','hadm_id'), all.x = TRUE, all.y = TRUE)
data <- merge(merged_data[[7]],data, by = c('subject_id','hadm_id'), all.x = TRUE, all.y = TRUE)

data2 <- merge(merged_data[[2]],merged_data[[6]], by = c('subject_id','hadm_id','icustay_id'), all.x = TRUE, all.y = TRUE)
data2 <- merge(merged_data[[8]],data2, by = c('subject_id','hadm_id','icustay_id'), all.x = TRUE, all.y = TRUE)
data2 <- merge(merged_data[[9]],data2, by = c('subject_id','hadm_id','icustay_id'), all.x = TRUE, all.y = TRUE)
data2 <- merge(merged_data[[10]],data2, by = c('subject_id','hadm_id','icustay_id'), all.x = TRUE, all.y = TRUE)

data_all <- merge(data2,data, by = c('subject_id','hadm_id'), all.x = TRUE, all.y = TRUE)
data_all <- merge(data_all, merged_data[[5]], by = c('hadm_id'), all.x = TRUE, all.y = TRUE)
## Sepsis patients
data_sepsis <- data_all[angus==1]
length(unique(data_sepsis$subject_id)) #12636

## Step 1 Age >= 16
data <- data_sepsis[age>=16]
length(unique(data$subject_id)) #12448

## Step 2 First hospital
data <- data[first_hosp_stay == 'Y']
length(unique(data$subject_id)) #10612

## Step 3 First icu stay
data <- data[first_icu_stay == 'Y']
length(unique(data$subject_id)) #10612

## Step 4 LOS ICU > 3
data <- data[ los_icu>3]
length(unique(data$subject_id)) #10591

## Step 5 FB 
fb <- fread('final_fb_wide.csv')
tmp <- data
setnames(fb,'SUBJECT_ID','subject_id')
setnames(fb,'HADM_ID','hadm_id')
setnames(fb,'ICUSTAY_ID','icustay_id')
fb <- fb[!is.na(fb3hr)]
tmp <- merge(tmp, fb, by = colnames(merged_data)[1:3], all.x=FALSE, all.y = FALSE)
length(unique(tmp$subject_id)) #10389


final_data <- tmp
write.table(final_data, paste0(wkdir,'/final_data_25Jan2018.csv'), sep = ',', row.names = FALSE, col.names = TRUE)
