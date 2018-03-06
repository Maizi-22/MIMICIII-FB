###### Logistic regression for sepsis project 
###### And use logistic regression to predict hospital motality/ICU motality
###### Input: Data, outcome variable, fluid variable
######        pul: subgroup analysis for pul patients (0/1)
######        ckd: subgroup analysis for ckd patients (0/1)
###### Output: model summary(a table contained OR, confidence level and p value); ROC of prediction
RegressionAndPredict <- function(data, y, x, pul, ckd){
  # handle missing data
  imputeMedian <- function(x){impute(x, median)} 
  data[, c(7, 11:39)] <- lapply(data[, c(7, 11:39)], imputeMedian)
  
  # logistic regression on entire data
  if ( pul == 0 & ckd == 0){
    model1 = glm(data[[y]] ~ data[[x]] + age + gender + weight + height + Mechanical_ventilation_duration + sofa + diabetes + hypertension + congestive_heart_failure + renal_failure + liver_disease + cancer + aids + chronic_pulmonary + obesity + explicit_sepsis + organ_dysfunction + mech_vent + eskd1 + eskd2 + eskd + lactate + hemoglobin + creatinine + wbc + neutrophils + temp_value + resp_rate + heart_rate + sys_bp + dias_bp + mean_bp + pul
                 , family = binomial(link='logit')
                 , data = data)
  }
  if ( pul == 0 & ckd == 1){
    model1 = glm(data[[y]] ~ data[[x]] + age + gender + weight + height + Mechanical_ventilation_duration + sofa + diabetes + hypertension + congestive_heart_failure + liver_disease + cancer + chronic_pulmonary + obesity + explicit_sepsis + organ_dysfunction + mech_vent + eskd1 + eskd + lactate + hemoglobin + creatinine + wbc + neutrophils + temp_value + resp_rate + heart_rate + sys_bp + dias_bp + mean_bp + pul
                 , family = binomial(link='logit')
                 , data = data)
  }
  if ( pul == 1 & ckd == 0){
    model1 = glm(data[[y]] ~ data[[x]] + age + gender + weight + height + Mechanical_ventilation_duration + sofa + diabetes + hypertension + congestive_heart_failure + renal_failure + liver_disease + cancer + chronic_pulmonary + obesity +  explicit_sepsis + organ_dysfunction + mech_vent + eskd1 + eskd + lactate + hemoglobin + creatinine + wbc + neutrophils + temp_value + resp_rate + heart_rate + sys_bp + dias_bp + mean_bp
                 , family = binomial(link='logit')
                 , data = data)
  }
  if ( pul == 1 & ckd == 1){
    model1 = glm(data[[y]] ~ data[[x]] + age + gender + weight + height + Mechanical_ventilation_duration + sofa + diabetes + hypertension + congestive_heart_failure + liver_disease + cancer + chronic_pulmonary + obesity + explicit_sepsis + organ_dysfunction + mech_vent + eskd1 + eskd + lactate + hemoglobin + creatinine + wbc + neutrophils + temp_value + resp_rate + heart_rate + sys_bp + dias_bp + mean_bp
                 , family = binomial(link='logit')
                 , data = data)
  }
  model1.summary <- summary(model1)
  model2 <- step(model1)
  # output
  b <- cbind(round(exp(coef(model2)),2), round(exp(confint(model2)),2), p_value = round(coef(summary(model2))[, 4], 3))
  b <- cbind(paste(b[, 1], '(', b[, 2], ',', b[, 3], ')'), b[, 4])
  b <- as.data.frame(b)
  rownames(b) <- rownames(b)
  colnames(b) <- c('OR (95%CI)', 'p value')
  b$`p value` <- as.character(b$`p value`)
  b[which(b$`p value` == 0), 2] <- c('<0.001')
  rownames(b)[2] <- x
  
  # split data into training dataset and test dataset
  train.data <- data[sample(9140, 8226), ]
  test.data <- anti_join(data, train.data)
  
  # apply linear regression
  data <- train.data
  if ( pul == 0 & ckd == 0){
    model1 = glm(data[[y]] ~ data[[x]] + age + gender + weight + height + Mechanical_ventilation_duration + sofa + diabetes + hypertension + congestive_heart_failure + renal_failure + liver_disease + cancer + aids + chronic_pulmonary + obesity +  explicit_sepsis + organ_dysfunction + mech_vent + eskd1 + eskd2 + eskd + lactate + hemoglobin + creatinine + wbc + neutrophils + temp_value + resp_rate + heart_rate + sys_bp + dias_bp + mean_bp + pul
                 , family = binomial(link='logit')
                 , data = data)
  }
  if ( pul == 1 & ckd == 0){
    model1 = glm(data[[y]] ~ data[[x]] + age + gender + weight + height + Mechanical_ventilation_duration + sofa + diabetes + hypertension + congestive_heart_failure + renal_failure + liver_disease + cancer + chronic_pulmonary + obesity + explicit_sepsis + organ_dysfunction + mech_vent + eskd1 + eskd + lactate + hemoglobin + creatinine + wbc + neutrophils + temp_value + resp_rate + heart_rate + sys_bp + dias_bp + mean_bp
                 , family = binomial(link='logit')
                 , data = data)
  }
  if ( pul == 0 & ckd == 1){
    model1 = glm(data[[y]] ~ data[[x]] + age + gender + weight + height + Mechanical_ventilation_duration + sofa + diabetes + hypertension + congestive_heart_failure + liver_disease + cancer + chronic_pulmonary + obesity + explicit_sepsis + organ_dysfunction + mech_vent + eskd1 + eskd + lactate + hemoglobin + creatinine + wbc + neutrophils + temp_value + resp_rate + heart_rate + sys_bp + dias_bp + mean_bp + pul
                 , family = binomial(link='logit')
                 , data = data)
  }
  if ( pul == 1 & ckd == 1){
    model1 = glm(data[[y]] ~ data[[x]] + age + gender + weight + height + Mechanical_ventilation_duration + sofa + diabetes + hypertension + congestive_heart_failure + liver_disease + cancer + chronic_pulmonary + obesity + explicit_sepsis + organ_dysfunction + mech_vent + eskd1 + eskd + lactate + hemoglobin + creatinine + wbc + neutrophils + temp_value + resp_rate + heart_rate + sys_bp + dias_bp + mean_bp
                 , family = binomial(link='logit')
                 , data = data)
  }
  model1.summary <- summary(model1)
  model2 <- step(model1)
  
  # test on test data
  data <- test.data
  model2 <- glm(model2$formula
                , family = binomial(link='logit')
                , data = data)
  
  # AUC of ROC curve
  test.data$pred2 <-predict(model2, type='response')
  rocobj1 <- plot.roc(as.matrix(test.data[, y]),
                      test.data$pred2,
                      percent=TRUE,ci=TRUE,col="#1c61b6",
                      print.auc=TRUE)
  result <- list(b, rocobj1)
  return(result)
}


#############################################
#### calulate mortality on fluid interval ###
#############################################
GetMR <- function(data, x, y){
  options(scipen = 200)
  result <- data.frame()
  a <- quantile(data[[x]], c(.05,.95))
  interval <- round((a[2] - a[1])/10)
  data <- subset(data, data[, x]>= a[1] & data[, x] <= a[2])
  for(i in 1:round((max(data[, names(data)==x]) - min(data[, names(data)==x]))/interval)){
    a <- subset(data, data[, names(data)==x] >= min(data[, names(data)==x]) + (i-1)*interval & data[, names(data)==x] < min(data[, names(data)==x]) + i*interval)
    d <- table(a[, names(data)==y])
    result[4, i] <- d[2] + d[1]
    result[3, i] <- d[2]
    result[2, i] <- d[2]/(d[1]+d[2])
    result[1, i] <- min(data[, names(data)==x]) + (i-1)*interval
  }
  result <- as.matrix(result)
  return(result)
}

#############################################
#### calulate mean sofa on fluid interval ###
#############################################
GetSofa <- function(data, x, y){
  options(scipen = 200)
  result <- data.frame()
  a <- quantile(data[[x]], c(.05,.95))
  interval <- round((a[2] - a[1])/10)
  data <- subset(data, data[, x]>= a[1] & data[, x] <= a[2])
  for(i in 1:round((max(data[, names(data)==x]) - min(data[, names(data)==x]))/interval)){
    a <- subset(data, data[, names(data)==x] >= min(data[, names(data)==x]) + (i-1)*interval & data[, names(data)==x] < min(data[, names(data)==x]) + i*interval)
    result[1, i] <- min(data[, names(data)==x]) + (i-1)*interval
    result[2, i] <- mean(as.matrix(a[, names(a)==y]))
  }
  result <- as.matrix(result)
  return(result)
}


# add group variable stratified by quartile of FB -------------------------
# input : data: which needed to process
#         x : variable which are respected to
AddGroupVar <- function(data, x){
  a <- quantile(data[[x]])
  data$group[data[[x]] >= a[1] & data[[x]] < a[2]] <- 1
  data$group[data[[x]] >= a[2] & data[[x]] < a[3]] <- 2
  data$group[data[[x]] >= a[3] & data[[x]] < a[4]] <- 3
  data$group[data[[x]] >= a[4] & data[[x]] <= a[5]] <- 4
  data$group <- factor(data$group)    
  data$group <- relevel(data$group,ref="1")  
  return(data)
}


# get model summary of linear regression ----------------------------------
# input : data
#         x : fluid variable
#         y : outcome variable
#         subgroup : there are four options: all(no subgroup), pul(pulmonary infection)
#                                            ckd, chf, hpt(hypertension)
GetLmSummary <- function(data, x, y, subgroup){
  if(subgroup == 'all'){
    model.output <- lm(formula = data[[y]] ~ age + gender + weight + hemoglobin + creatinine + wbc + temp_value + resp_rate + 
                         heart_rate + mean_bp + Mechanical_ventilation_duration + mingcs + sofa + diabetes + hypertension + 
                         congestive_heart_failure + renal_failure + liver_disease + cancer + aids + chronic_pulmonary + obesity + 
                         eskd + pul + dialysis + group, data = data)
  }
  if(subgroup == 'pul'){
    model.output <- lm(formula = data[[y]] ~ age + gender + weight + hemoglobin + creatinine + wbc + temp_value + resp_rate + 
                         heart_rate + mean_bp + Mechanical_ventilation_duration + mingcs + sofa + diabetes + hypertension + 
                         congestive_heart_failure + renal_failure + liver_disease + cancer + aids + chronic_pulmonary + obesity + 
                         eskd + dialysis + group, data = data)
  }
  if(subgroup == 'ckd'){
    model.output <- lm(formula = data[[y]] ~ age + gender + weight + hemoglobin + creatinine + wbc + temp_value + resp_rate + 
                         heart_rate + mean_bp + Mechanical_ventilation_duration + mingcs + sofa + diabetes + hypertension + 
                         congestive_heart_failure + liver_disease + cancer + aids + chronic_pulmonary + obesity + 
                         eskd + pul + dialysis + group, data = data)
  }
  if(subgroup == 'chf'){
    model.output <- lm(formula = data[[y]] ~ age + gender + weight + hemoglobin + creatinine + wbc + temp_value + resp_rate + 
                         heart_rate + mean_bp + Mechanical_ventilation_duration + mingcs + sofa + diabetes + hypertension + 
                         renal_failure + liver_disease + cancer + aids + chronic_pulmonary + obesity + 
                         eskd + pul + dialysis + group, data = data)
  }
  if(subgroup == 'hpt'){
    model.output <- lm(formula = data[[y]] ~ age + gender + weight + hemoglobin + creatinine + wbc + temp_value + resp_rate + 
                         heart_rate + mean_bp + Mechanical_ventilation_duration + mingcs + sofa + diabetes + 
                         congestive_heart_failure + renal_failure + liver_disease + cancer + aids + chronic_pulmonary + obesity + 
                         eskd + pul + dialysis + group, data = data)
  }
  b1 <- cbind(round(coef(model.output), 2), round(confint(model.output),2), p_value = round(coef(summary(model.output))[, 4], 3))
  b1 <- cbind(paste(b1[, 1], '(', b1[, 2], ',', b1[, 3], ')'), b1[, 4])
  b1 <- as.data.frame(b1)
  colnames(b1) <- c('Coef(95%CI)', 'p value')
  b1$`p value` <- as.character(b1$`p value`)
  b1[which(b1$`p value` == 0), 2] <- c('< 0.001')
  
  if(subgroup == 'all'){
    model.output2 <- lm(formula = data[[y]] ~ data[[x]] + age + gender + weight + hemoglobin + creatinine + wbc + temp_value + resp_rate + 
                         heart_rate + mean_bp + Mechanical_ventilation_duration + mingcs + sofa + diabetes + hypertension + 
                         congestive_heart_failure + renal_failure + liver_disease + cancer + aids + chronic_pulmonary + obesity + 
                         eskd + pul + dialysis, data = data)
  }
  if(subgroup == 'pul'){
    model.output2 <- lm(formula = data[[y]] ~ data[[x]] + age + gender + weight + hemoglobin + creatinine + wbc + temp_value + resp_rate + 
                         heart_rate + mean_bp + Mechanical_ventilation_duration + mingcs + sofa + diabetes + hypertension + 
                         congestive_heart_failure + renal_failure + liver_disease + cancer + aids + chronic_pulmonary + obesity + 
                         eskd + dialysis, data = data)
  }
  if(subgroup == 'ckd'){
    model.output2 <- lm(formula = data[[y]] ~ data[[x]] + age + gender + weight + hemoglobin + creatinine + wbc + temp_value + resp_rate + 
                         heart_rate + mean_bp + Mechanical_ventilation_duration + mingcs + sofa + diabetes + hypertension + 
                         congestive_heart_failure + liver_disease + cancer + aids + chronic_pulmonary + obesity + 
                         eskd + pul + dialysis, data = data)
  }
  if(subgroup == 'chf'){
    model.output2 <- lm(formula = data[[y]] ~ data[[x]] + age + gender + weight + hemoglobin + creatinine + wbc + temp_value + resp_rate + 
                         heart_rate + mean_bp + Mechanical_ventilation_duration + mingcs + sofa + diabetes + hypertension + 
                         renal_failure + liver_disease + cancer + aids + chronic_pulmonary + obesity + 
                         eskd + pul + dialysis, data = data)
  }
  if(subgroup == 'hpt'){
    model.output2 <- lm(formula = data[[y]] ~ data[[x]] + age + gender + weight + hemoglobin + creatinine + wbc + temp_value + resp_rate + 
                         heart_rate + mean_bp + Mechanical_ventilation_duration + mingcs + sofa + diabetes + 
                         congestive_heart_failure + renal_failure + liver_disease + cancer + aids + chronic_pulmonary + obesity + 
                         eskd + pul + dialysis, data = data)
  }
  b2 <- cbind(round(coef(model.output2), 2), round(confint(model.output2),2), p_value = round(coef(summary(model.output2))[, 4], 3))
  b2 <- cbind(paste(b2[, 1], '(', b2[, 2], ',', b2[, 3], ')'), b2[, 4])
  b2 <- as.data.frame(b2)
  colnames(b2) <- c('Coef(95%CI)', 'p value')
  b2$`p value` <- as.character(b2$`p value`)
  b2[which(b2$`p value` == 0), 2] <- c('< 0.001')
  rownames(b2)[2] <- x
  result <- list(b1, b2)
  return(result)
}

# get linear regression result for different group ----------------------------------
#--------------------------------------------------------------------
GetModelSummeryTable <- function(result.list) {
  result <- data.frame()
  for(i in 1:length(result.list1)){
    # the result comes as a list that contains regression summary of 5 subgroup
    # and for each subgroup there are two model summary, for continuous FB and quartiled FB respectively
    # a contains all continuous FB model
    a <- result.list[[i]][1]
    # b contains all quartiled FB model
    b <- result.list[[i]][2]
    c1 <- c('1.00 Ref.')
    # coefficient of quartiled FB model
    c21 <- as.character.factor(as.data.frame(a)[rownames(as.data.frame(a)) == 'group2',1])
    c22 <- as.character(as.data.frame(a)[rownames(as.data.frame(a)) == 'group2',2])
    c2 <- paste(c21, 'p = ', c22)
    c31 <- as.character.factor(as.data.frame(a)[rownames(as.data.frame(a)) == 'group3',1])
    c32 <- as.character(as.data.frame(a)[rownames(as.data.frame(a)) == 'group3',2])
    c3 <- paste(c31, 'p = ', c32)
    c41 <- as.character.factor(as.data.frame(a)[rownames(as.data.frame(a)) == 'group4',1])
    c42 <- as.character(as.data.frame(a)[rownames(as.data.frame(a)) == 'group4',2])
    c4 <- paste(c41, 'p = ', c42)
    # coefficient of continuous FB model
    c51 <- as.character.factor(as.data.frame(b)[2,1])
    c52 <- as.character(as.data.frame(b)[2,2])
    c5 <- paste(c51, 'p = ', c52)
    modelname <- paste('model', i)
    c <- data.frame(Subgroup = c(modelname), group1 = c(c1), group2 = c(c2), group3 = c(c3), group4 = c(c4), per_increase = c(c(c5)))
    result <- rbind(result, c)
  }
  return(result)
}

# get model summary of OR and p value for logistic regression-------------------------------------
GetORSummary <- function(data, x, y, subgroup){
  # quartiled FB model
  if(subgroup == 'all'){
    model.output <- glm(formula = data[[y]] ~ age + gender + weight + hemoglobin + creatinine + wbc + temp_value + resp_rate + 
                         heart_rate + mean_bp + Mechanical_ventilation_duration + mingcs + sofa + diabetes + hypertension + 
                         congestive_heart_failure + renal_failure + liver_disease + cancer + aids + chronic_pulmonary + obesity + 
                         eskd + pul + dialysis + group, family = binomial(link='logit'), data = data)
  }
  if(subgroup == 'pul'){
    model.output <- glm(formula = data[[y]] ~ age + gender + weight + hemoglobin + creatinine + wbc + temp_value + resp_rate + 
                         heart_rate + mean_bp + Mechanical_ventilation_duration + mingcs + sofa + diabetes + hypertension + 
                         congestive_heart_failure + renal_failure + liver_disease + cancer + aids + chronic_pulmonary + obesity + 
                         eskd + dialysis + group, family = binomial(link='logit'), data = data)
  }
  if(subgroup == 'ckd'){
    model.output <- glm(formula = data[[y]] ~ age + gender + weight + hemoglobin + creatinine + wbc + temp_value + resp_rate + 
                         heart_rate + mean_bp + Mechanical_ventilation_duration + mingcs + sofa + diabetes + hypertension + 
                         congestive_heart_failure + liver_disease + cancer + aids + chronic_pulmonary + obesity + 
                         eskd + pul + dialysis + group, family = binomial(link='logit'), data = data)
  }
  if(subgroup == 'chf'){
    model.output <- glm(formula = data[[y]] ~ age + gender + weight + hemoglobin + creatinine + wbc + temp_value + resp_rate + 
                         heart_rate + mean_bp + Mechanical_ventilation_duration + mingcs + sofa + diabetes + hypertension + 
                         renal_failure + liver_disease + cancer + aids + chronic_pulmonary + obesity + 
                         eskd + pul + dialysis + group, family = binomial(link='logit'), data = data)
  }
  if(subgroup == 'hpt'){
    model.output <- glm(formula = data[[y]] ~ age + gender + weight + hemoglobin + creatinine + wbc + temp_value + resp_rate + 
                         heart_rate + mean_bp + Mechanical_ventilation_duration + mingcs + sofa + diabetes + 
                         congestive_heart_failure + renal_failure + liver_disease + cancer + aids + chronic_pulmonary + obesity + 
                         eskd + pul + dialysis + group, family = binomial(link='logit'), data = data)
  }
  b1 <- cbind(round(exp(coef(model.output)), 2), round(exp(confint(model.output)), 2), p_value = round(coef(summary(model.output))[, 4], 3))
  b1 <- cbind(paste(b1[, 1], '(', b1[, 2], ',', b1[, 3], ')'), b1[, 4])
  b1 <- as.data.frame(b1)
  colnames(b1) <- c('Per 1 mL increase in fluid balance', 'p value')
  b1$`p value` <- as.character(b1$`p value`)
  b1[which(b1$`p value` == 0), 2] <- c('< 0.001')
  
  # continuous FB model
  if(subgroup == 'all'){
    model.output2 <- glm(formula = data[[y]] ~ data[[x]] + age + gender + weight + hemoglobin + creatinine + wbc + temp_value + resp_rate + 
                          heart_rate + mean_bp + Mechanical_ventilation_duration + mingcs + sofa + diabetes + hypertension + 
                          congestive_heart_failure + renal_failure + liver_disease + cancer + aids + chronic_pulmonary + obesity + 
                          eskd + pul + dialysis, family = binomial(link='logit'), data = data)
  }
  if(subgroup == 'pul'){
    model.output2 <- glm(formula = data[[y]] ~ data[[x]] + age + gender + weight + hemoglobin + creatinine + wbc + temp_value + resp_rate + 
                          heart_rate + mean_bp + Mechanical_ventilation_duration + mingcs + sofa + diabetes + hypertension + 
                          congestive_heart_failure + renal_failure + liver_disease + cancer + aids + chronic_pulmonary + obesity + 
                          eskd + dialysis, family = binomial(link='logit'), data = data)
  }
  if(subgroup == 'ckd'){
    model.output2 <- glm(formula = data[[y]] ~ data[[x]] + age + gender + weight + hemoglobin + creatinine + wbc + temp_value + resp_rate + 
                          heart_rate + mean_bp + Mechanical_ventilation_duration + mingcs + sofa + diabetes + hypertension + 
                          congestive_heart_failure + liver_disease + cancer + aids + chronic_pulmonary + obesity + 
                          eskd + pul + dialysis, family = binomial(link='logit'), data = data)
  }
  if(subgroup == 'chf'){
    model.output2 <- glm(formula = data[[y]] ~ data[[x]] + age + gender + weight + hemoglobin + creatinine + wbc + temp_value + resp_rate + 
                          heart_rate + mean_bp + Mechanical_ventilation_duration + mingcs + sofa + diabetes + hypertension + 
                          renal_failure + liver_disease + cancer + aids + chronic_pulmonary + obesity + 
                          eskd + pul + dialysis, family = binomial(link='logit'), data = data)
  }
  if(subgroup == 'hpt'){
    model.output2 <- glm(formula = data[[y]] ~ data[[x]] + age + gender + weight + hemoglobin + creatinine + wbc + temp_value + resp_rate + 
                          heart_rate + mean_bp + Mechanical_ventilation_duration + mingcs + sofa + diabetes + 
                          congestive_heart_failure + renal_failure + liver_disease + cancer + aids + chronic_pulmonary + obesity + 
                          eskd + pul + dialysis, family = binomial(link='logit'), data = data)
  }
  b2 <- cbind(round(exp(coef(model.output2)), 2), round(exp(confint(model.output2)), 2), p_value = round(coef(summary(model.output2))[, 4], 3))
  b2 <- cbind(paste(b2[, 1], '(', b2[, 2], ',', b2[, 3], ')'), b2[, 4])
  b2 <- as.data.frame(b2)
  colnames(b2) <- c('Per 1 mL increase in fluid balance', 'p value')
  b2$`p value` <- as.character(b2$`p value`)
  b2[which(b2$`p value` == 0), 2] <- c('< 0.001')
  result <- list(b1, b2)
  return(result)
}

# linear regression for different variables ----------------------------------
# input: data:dataset
#        y: outcome variable
#        x: main interest variable
# output: three linear regression results
GetlinearRegression <- function(data.all, y, x, data.pul, data.ckd, data.chf, data.hpt){
  # result of linear regression which FB was considered as quartile group
  b1 <- GetLmSummary(data.all, x, y, 'all')
  b2 <- GetLmSummary(data.pul, x, y, 'pul')
  b3 <- GetLmSummary(data.ckd, x, y, 'ckd')
  b4 <- GetLmSummary(data.chf, x, y, 'chf')
  b5 <- GetLmSummary(data.hpt, x, y, 'hpt')
  result.list1 <- list(b1, b2, b3, b4, b5)
  result1 <- GetModelSummeryTable(result.list1)
  result1[, 1] <- c('ALL patients', 'PUL', 'CKD', 'CHF', 'Hypertension')
  return(result1)
}

# logistic regression for different variables --------------------------------
# input: data:dataset
#        y: outcome variable
#        x: main interest variable
# output: three logistic regression results
GetLogisticRegression <- function(data.all, y, x, data.pul, data.ckd, data.chf, data.hpt){
  b1 <- GetORSummary(data.all, x, y, 'all')
  b2 <- GetORSummary(data.pul, x, y, 'pul')
  b3 <- GetORSummary(data.ckd, x, y, 'ckd')
  b4 <- GetORSummary(data.chf, x, y, 'chf')
  b5 <- GetORSummary(data.hpt, x, y, 'hpt')
  result.list <- list(b1, b2, b3, b4, b5)
  result <- GetModelSummeryTable(result.list)
  result[, 1] <- c('ALL patients', 'PUL', 'CKD', 'CHF', 'Hypertension')
  return(result)
}
