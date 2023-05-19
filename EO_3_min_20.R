
risk_20_at_3y_final <- read.csv("risk_20_at_3y_final_3_min.csv")
risk_20_at_5y_final <- read.csv("risk_20_at_5y_final_3_min.csv")

risk_20_at_3y_risks <- risk_20_at_3y_final[,c("person_id","Status","Risk","ER.Risk")]
risk_20_at_5y_risks <- risk_20_at_5y_final[,c("person_id","Status","Risk","ER.Risk")]

risk_20_at_3y_order <- risk_20_at_3y_risks[order(risk_20_at_3y_risks$Risk),]

risk_20_at_3y_order$Bin <- qcut(risk_20_at_3y_order$Risk, cuts = 5)
risk_20_at_3y_cbc <- risk_20_at_3y_order[risk_20_at_3y_order$Status == 'CBC',]
risk_20_at_3y_exp <- risk_20_at_3y_cbc %>% group_by(Bin) %>%
  summarise(total_Risk=sum(Risk))
risk_20_at_3y_obs <- risk_20_at_3y_cbc %>% group_by(Bin) %>%
  summarise(total_count=n())
risk_20_at_3y_eo <- cbind(risk_20_at_3y_exp, risk_20_at_3y_obs$total_count)
risk_20_at_3y_eo$EO <- risk_20_at_3y_exp$total_Risk/risk_20_at_3y_obs$total_count
colnames(risk_20_at_3y_eo) <- c('Risk Quintiles', 'Expected', 'Observed', 'E/O')
write.csv(risk_20_at_3y_eo, file = "risk_20_at_3y_3_min_eo.csv")

risk_20_at_5y_order <- risk_20_at_5y_risks[order(risk_20_at_5y_risks$Risk),]
risk_20_at_5y_order$Bin <- qcut(risk_20_at_5y_order$Risk, cuts = 5)

risk_20_at_5y_cbc <- risk_20_at_5y_order[risk_20_at_5y_order$Status == 'CBC',]
risk_20_at_5y_exp <- risk_20_at_5y_cbc %>% group_by(Bin) %>%
  summarise(total_Risk=sum(Risk))
risk_20_at_5y_obs <- risk_20_at_5y_cbc %>% group_by(Bin) %>%
  summarise(total_count=n())
risk_20_at_5y_eo <- cbind(risk_20_at_5y_exp, risk_20_at_5y_obs$total_count)
risk_20_at_5y_eo$EO <- risk_20_at_5y_exp$total_Risk/risk_20_at_5y_obs$total_count
colnames(risk_20_at_5y_eo) <- c('Risk Quintiles', 'Expected', 'Observed', 'E/O')
write.csv(risk_20_at_5y_eo, file = "risk_20_at_5y_3_min_eo.csv")

