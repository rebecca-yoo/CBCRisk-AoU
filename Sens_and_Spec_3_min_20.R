
risk_20_at_3y_final <- read.csv("risk_20_at_3y_final_3_min.csv")
risk_20_at_5y_final <- read.csv("risk_20_at_5y_final_3_min.csv")

risk_12_at_3y_risks <- risk_20_at_3y_final[,c("person_id","Status","Risk","ER.Risk")]
risk_12_at_5y_risks <- risk_20_at_5y_final[,c("person_id","Status","Risk","ER.Risk")]

risk_12_at_3y_order <- risk_12_at_3y_risks[order(risk_12_at_3y_risks$Risk),]
risk_12_at_3y_order$Bin <- qcut(risk_12_at_3y_order$Risk, cuts = 5)

Threshold_3y <- c(0.62, 0.72, 0.88, 0.94, 1.12, 2.92)

# Sensitivity = TP/P where TP is patients with CBC who are greater than threshold out of patients who are greater than threshold
Sensitivity <- c()
for (i in 1:length(Threshold_3y)) {
  tp <- nrow(risk_12_at_3y_order[risk_12_at_3y_order$Status=="CBC" & risk_12_at_3y_order$Risk >= Threshold_3y[i],])
  fn <- nrow(risk_12_at_3y_order[risk_12_at_3y_order$Status=="CBC" & risk_12_at_3y_order$Risk < Threshold_3y[i],])
  p <- tp+fn
  sens <- tp/p
  Sensitivity <- append(Sensitivity, sens, i-1)
}

# Specificity = TP/P where TP is patients with CBC who are greater than Threshold_3y out of patients who are greater than Threshold_3y
Specificity <- c()
for (i in 1:length(Threshold_3y)) {
  tn <- nrow(risk_12_at_3y_order[risk_12_at_3y_order$Status=="No CBC" & risk_12_at_3y_order$Risk < Threshold_3y[i],])
  fp <- nrow(risk_12_at_3y_order[risk_12_at_3y_order$Status=="No CBC" & risk_12_at_3y_order$Risk >= Threshold_3y[i],])
  n <- tn + fp
  spec <- tn/n
  Specificity <- append(Specificity, spec, i-1)
}
risk_12_at_3y_thresh <- rbind(Threshold_3y, Sensitivity, Specificity)
write.csv(risk_12_at_3y_thresh, file = "risk_20_at_3y_3_min_thresh.csv")

risk_12_at_5y_order <- risk_12_at_5y_risks[order(risk_12_at_5y_risks$Risk),]
risk_12_at_5y_order$Bin <- qcut(risk_12_at_5y_order$Risk, cuts = 5)
Threshold_5y <- c(1.07, 1.20, 1.44, 1.59, 1.86, 4.74)

# Sensitivity = TP/P where TP is patients with CBC who are greater than Threshold_5y out of patients who are greater than Threshold_5y
Sensitivity <- c()
for (i in 1:length(Threshold_5y)) {
  tp <- nrow(risk_12_at_5y_order[risk_12_at_5y_order$Status=="CBC" & risk_12_at_5y_order$Risk >= Threshold_5y[i],])
  fn <- nrow(risk_12_at_5y_order[risk_12_at_5y_order$Status=="CBC" & risk_12_at_5y_order$Risk < Threshold_5y[i],])
  p <- tp+fn
  sens <- tp/p
  Sensitivity <- append(Sensitivity, sens, i-1)
}

# Specificity = TP/P where TP is patients with CBC who are greater than Threshold_5y out of patients who are greater than Threshold_5y
Specificity <- c()
for (i in 1:length(Threshold_5y)) {
  tn <- nrow(risk_12_at_5y_order[risk_12_at_5y_order$Status=="No CBC" & risk_12_at_5y_order$Risk < Threshold_5y[i],])
  fp <- nrow(risk_12_at_5y_order[risk_12_at_5y_order$Status=="No CBC" & risk_12_at_5y_order$Risk >= Threshold_5y[i],])
  n <- tn + fp
  spec <- tn/n
  Specificity <- append(Specificity, spec, i-1)
}
risk_12_at_5y_thresh <- rbind(Threshold_5y, Sensitivity, Specificity)
write.csv(risk_12_at_5y_thresh, file = "risk_20_at_5y_3_min_thresh.csv")

