library(pROC)

# #################################### CBCRisk 1.2 ######################################################

risk_12_at_3y_final <- read.csv("risk_12_at_3y_final_3_min.csv")
risk_12_at_5y_final <- read.csv("risk_12_at_5y_final_3_min.csv")


risk_12_at_3y_final <-  risk_12_at_3y_final %>%
  mutate(Observed = case_when(
    Status == "CBC" ~ 1,
    Status == "No CBC" ~ 0,
  ))

risk_12_at_5y_final <-  risk_12_at_5y_final %>%
  mutate(Observed = case_when(
    Status == "CBC" ~ 1,
    Status == "No CBC" ~ 0,
  ))


roc_cbcrisk_3y <- roc(risk_12_at_3y_final$Observed, risk_12_at_3y_final$Risk, direction="<")
plot(roc_cbcrisk_3y, legacy.axes = TRUE, main="ROC curve for 3-year risk with CBCRisk 1.2")
auc(roc_cbcrisk_3y)
ci.auc(roc_cbcrisk_3y, conf.level = 0.95)

roc_cbcrisk_5y <- roc(risk_12_at_5y_final$Observed, risk_12_at_5y_final$Risk, direction="<")
plot(roc_cbcrisk_5y, legacy.axes = TRUE, main="ROC curve for 5-year risk with CBCRisk 1.2")
auc(roc_cbcrisk_5y)
ci.auc(roc_cbcrisk_5y, conf.level = 0.95)

#################################### CBCRisk 2.0 ######################################################

risk_20_at_3y_final <- read.csv("risk_20_at_3y_final_3_min.csv")
risk_20_at_5y_final <- read.csv("risk_20_at_5y_final_3_min.csv")

colnames(risk_20_at_3y_final)[colnames(risk_20_at_3y_final)=='Risk'] <- "CBRisk 2.0"
colnames(risk_20_at_5y_final)[colnames(risk_20_at_5y_final)=='Risk'] <- "CBRisk 2.0"

risk_20_at_3y_roc <-  risk_20_at_3y_final %>%
  mutate(Observed = case_when(
    Status == "CBC" ~ 1,
    Status == "No CBC" ~ 0,
  ))

risk_20_at_5y_roc <-  risk_20_at_5y_final %>%
  mutate(Observed = case_when(
    Status == "CBC" ~ 1,
    Status == "No CBC" ~ 0,
  ))

roc_cbcrisk_3y <- roc(risk_20_at_3y_roc$Observed, risk_20_at_3y_roc$`CBRisk 2.0`, direction="<")
plot(roc_cbcrisk_3y, legacy.axes = TRUE, main="ROC curve for 3-year risk with CBCRisk 2.0")
auc(roc_cbcrisk_3y)
ci.auc(roc_cbcrisk_3y, conf.level = 0.95) 


roc_cbcrisk_5y <- roc(risk_20_at_5y_roc$Observed, risk_20_at_5y_roc$`CBRisk 2.0`, direction="<")
plot(roc_cbcrisk_5y, legacy.axes = TRUE, main="ROC curve for 5-year risk with CBCRisk 2.0")
auc(roc_cbcrisk_5y)
ci.auc(roc_cbcrisk_5y, conf.level = 0.95) 
