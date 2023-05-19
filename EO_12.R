
risk_12_at_3y_final <- read.csv("risk_1.2_at_3y_final.csv")
risk_12_at_5y_final <- read.csv("risk_1.2_at_5y_final.csv")

risk_12_at_3y_risks <- risk_12_at_3y_final[,c("ID","Status","Risk","ER.Risk")]
risk_12_at_5y_risks <- risk_12_at_5y_final[,c("ID","Status","Risk","ER.Risk")]

risk_12_at_3y_order <- risk_12_at_3y_risks[order(risk_12_at_3y_risks$Risk),]

risk_12_at_3y_order$Bin <- qcut(risk_12_at_3y_order$Risk, cuts = 5)

risk_12_at_3y_cbc <- risk_12_at_3y_order[risk_12_at_3y_order$Status == 'CBC',]
risk_12_at_3y_exp <- risk_12_at_3y_cbc %>% group_by(Bin) %>%
  summarise(total_Risk=sum(Risk))
risk_12_at_3y_obs <- risk_12_at_3y_cbc %>% group_by(Bin) %>%
  summarise(total_count=n())
risk_12_at_3y_eo <- cbind(risk_12_at_3y_exp, risk_12_at_3y_obs$total_count)
risk_12_at_3y_eo$EO <- risk_12_at_3y_exp$total_Risk/risk_12_at_3y_obs$total_count
colnames(risk_12_at_3y_eo) <- c('Risk Quintiles', 'Expected', 'Observed', 'E/O')
write.csv(risk_12_at_3y_eo, file = "risk_12_at_3y_eo.csv")

# print(risk_12_at_5y_quint$Bin)
risk_12_at_5y_order <- risk_12_at_5y_risks[order(risk_12_at_5y_risks$Risk),]
risk_12_at_5y_order$Bin <- qcut(risk_12_at_5y_order$Risk, cuts = 5)
Threshold_5y <- c(0.92, 1.12, 1.21, 1.31, 1.54, 3.32)
risk_12_at_5y_cbc <- risk_12_at_5y_order[risk_12_at_5y_order$Status == 'CBC',]
risk_12_at_5y_exp <- risk_12_at_5y_cbc %>% group_by(Bin) %>%
  summarise(total_Risk=sum(Risk))
risk_12_at_5y_obs <- risk_12_at_5y_cbc %>% group_by(Bin) %>%
  summarise(total_count=n())
risk_12_at_5y_eo <- cbind(risk_12_at_5y_exp, risk_12_at_5y_obs$total_count)
risk_12_at_5y_eo$EO <- risk_12_at_5y_exp$total_Risk/risk_12_at_5y_obs$total_count
colnames(risk_12_at_5y_eo) <- c('Risk Quintiles', 'Expected', 'Observed', 'E/O')
write.csv(risk_12_at_5y_eo, file = "risk_12_at_5y_eo.csv")
