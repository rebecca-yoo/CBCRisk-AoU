# install.packages("lattice")                                  # Install lattice package
library(lattice)   

# install.packages("ggplot2")
library(ggplot2)

# install.packages("reshape2") 
# install.packages("reshape") 
library(reshape2)
library(reshape)

#################################### CBCRisk 1.2 - 3 - min ######################################################
risk_12_at_3y_final <- read.csv("risk_12_at_3y_final_3_min.csv")
risk_12_at_5y_final <- read.csv("risk_12_at_5y_final_3_min.csv")

risk_20_at_3y_final <- read.csv("risk_20_at_3y_final_3_min.csv")
risk_20_at_5y_final <- read.csv("risk_20_at_5y_final_3_min.csv")

risk_12_at_3y_1 <- risk_12_at_3y_final[,c("person_id","Status","Risk","ER.Risk")]
risk_12_at_5y_1 <- risk_12_at_5y_final[,c("person_id","Status","Risk","ER.Risk")]

colnames(risk_12_at_3y_1)[colnames(risk_12_at_3y_1)=='Risk'] <- "CBCRisk"
colnames(risk_12_at_5y_1)[colnames(risk_12_at_5y_1)=='Risk'] <- "CBCRisk"

risk_20_at_3y_1 <- risk_20_at_3y_final[,c("person_id","Status","Risk","ER.Risk")]
risk_20_at_5y_1 <- risk_20_at_5y_final[,c("person_id","Status","Risk","ER.Risk")]

colnames(risk_20_at_3y_1)[colnames(risk_20_at_3y_1)=='Risk'] <- "CBCRisk-Black"
colnames(risk_20_at_5y_1)[colnames(risk_20_at_5y_1)=='Risk'] <- "CBCRisk-Black"

nrow(risk_12_at_3y_1[risk_12_at_3y_1$Status == "CBC",])
nrow(risk_12_at_3y_1[risk_12_at_3y_1$Status == "No CBC",])

nrow(risk_12_at_5y_1[risk_12_at_5y_1$Status == "CBC",])
nrow(risk_12_at_5y_1[risk_12_at_5y_1$Status == "No CBC",])
# 
# 
# colnames(risk_20_at_3y_1) <- c("person_id", "Status", "CBCRisk-Black")
# colnames(risk_20_at_5y_1) <- c("person_id", "Status", "CBCRisk-Black")

risk_12_20_at_3y <- merge(risk_12_at_3y_1, risk_20_at_3y_1, by = c("person_id", "Status"), all = TRUE)  # Applying inner_join() function
risk_12_20_at_5y <- merge(risk_12_at_5y_1, risk_20_at_5y_1, by = c("person_id", "Status"), all = TRUE)  # Applying inner_join() function

risk_12_20_at_3y_risks <- risk_12_20_at_3y[,c("Status", "CBCRisk", "CBCRisk-Black")]
risk_12_20_at_5y_risks <- risk_12_20_at_5y[,c("Status", "CBCRisk", "CBCRisk-Black")]
risk_12_20_at_3y_long <- melt(risk_12_20_at_3y_risks, id = "Status") 
risk_12_20_at_3y_long <- risk_12_20_at_3y_long %>% filter(!is.na(value))

# risk_12_20_at_3y_long <- melt(risk_12_20_at_3y_risks, id = "Status") 
# risk_12_20_at_3y_long <-  risk_12_20_at_3y_long %>%
#   mutate(Status = case_when(
#     Status == "CBC" ,
#     Status == "No CBC"))

print(nrow(risk_12_20_at_3y_long[risk_12_20_at_3y_long$variable == "CBCRisk" & risk_12_20_at_3y_long$Status == "CBC",]))
print(nrow(risk_12_20_at_3y_long[risk_12_20_at_3y_long$variable == "CBCRisk-Black" & risk_12_20_at_3y_long$Status == "CBC",]))
print(nrow(risk_12_20_at_3y_long[risk_12_20_at_3y_long$variable == "CBCRisk" & risk_12_20_at_3y_long$Status == "No CBC",]))
print(nrow(risk_12_20_at_3y_long[risk_12_20_at_3y_long$variable == "CBCRisk-Black" & risk_12_20_at_3y_long$Status == "No CBC",]))

risk_12_20_at_5y_long <- melt(risk_12_20_at_5y_risks, id = "Status") 
risk_12_20_at_5y_long <- melt(risk_12_20_at_5y_risks, id = "Status") 
risk_12_20_at_5y_long <- risk_12_20_at_5y_long %>% filter(!is.na(value))

print(nrow(risk_12_20_at_5y_long[risk_12_20_at_5y_long$variable == "CBCRisk" & risk_12_20_at_5y_long$Status == "CBC",]))
print(nrow(risk_12_20_at_5y_long[risk_12_20_at_5y_long$variable == "CBCRisk-Black" & risk_12_20_at_5y_long$Status == "CBC",]))
print(nrow(risk_12_20_at_5y_long[risk_12_20_at_5y_long$variable == "CBCRisk" & risk_12_20_at_5y_long$Status == "No CBC",]))
print(nrow(risk_12_20_at_5y_long[risk_12_20_at_5y_long$variable == "CBCRisk-Black" & risk_12_20_at_5y_long$Status == "No CBC",]))

# risk_12_20_at_5y_long <-  risk_12_20_at_5y_long %>%
#   mutate(Status = case_when(
#     Status == "CBC" ~ paste("CBC (n =", as.character(nrow(risk_12_20_at_5y_long[risk_12_20_at_5y_long$variable == "CBCRisk" & risk_12_20_at_5y_long$Status == "CBC",])), ")"),
#     Status == "No CBC" ~ paste("No CBC (n =", as.character(nrow(risk_12_20_at_5y_long[risk_12_20_at_5y_long$variable == "CBCRisk" & risk_12_20_at_5y_long$Status == "No CBC",])), ")")
#   ))

ggplot(risk_12_20_at_3y_long, aes(x = variable, y = value, color = Status)) +  # ggplot function
  geom_boxplot() + xlab("Model") + ylab("CBCRisk Estimate (%)") + 
  theme(text = element_text(size = 25)) + scale_y_continuous(limits=c(0,4))
ggplot(risk_12_20_at_5y_long, aes(x = variable, y = value, color = Status)) +  # ggplot function
  geom_boxplot() + xlab("Model") + ylab("CBCRisk Estimate (%)") + 
  theme(text = element_text(size = 25)) + scale_y_continuous(limits=c(0,4))



summary(risk_12_at_3y_1[risk_12_at_3y_1$Status=="CBC", 3])
summary(risk_12_at_3y_1[risk_12_at_3y_1$Status=="No CBC", 3])


summary(risk_12_at_5y_1[risk_12_at_5y_1$Status=="CBC", 3])
summary(risk_12_at_5y_1[risk_12_at_5y_1$Status=="No CBC", 3])


summary(risk_20_at_3y_1[risk_20_at_3y_1$Status=="CBC", 3])
summary(risk_20_at_3y_1[risk_20_at_3y_1$Status=="No CBC", 3])


summary(risk_20_at_5y_1[risk_20_at_5y_1$Status=="CBC", 3])
summary(risk_20_at_5y_1[risk_20_at_5y_1$Status=="No CBC", 3])
