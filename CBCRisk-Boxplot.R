# install.packages("lattice")                                  # Install lattice package
library(lattice)   

# install.packages("ggplot2")
library(ggplot2)

# install.packages("reshape2") 
# install.packages("reshape") 
library(reshape2)
library(reshape)
library(dplyr)

#################################### CBCRisk 1.2 - CBCRisk 2.0 ######################################################
risk_12_at_3y_final <- read.csv("risk_1.2_at_3y_final.csv")
risk_12_at_5y_final <- read.csv("risk_1.2_at_5y_final.csv")

risk_20_at_3y_final <- read.csv("risk_2.0_at_3y_final.csv")
risk_20_at_5y_final <- read.csv("risk_2.0_at_5y_final.csv")

risk_12_at_3y_1 <- risk_12_at_3y_final[,c('ID','Status','Risk')]
risk_12_at_5y_1 <- risk_12_at_5y_final[,c('ID','Status','Risk')]

colnames(risk_12_at_3y_1)[1] <- "ID"
colnames(risk_12_at_5y_1)[1] <- "ID"

colnames(risk_12_at_3y_1)[3] <- "CBCRisk"
colnames(risk_12_at_5y_1)[3] <- "CBCRisk"

nrow(risk_12_at_3y_1[risk_12_at_3y_1$Status == "CBC",])
nrow(risk_12_at_3y_1[risk_12_at_3y_1$Status == "No CBC",])

nrow(risk_12_at_5y_1[risk_12_at_5y_1$Status == "CBC",])
nrow(risk_12_at_5y_1[risk_12_at_5y_1$Status == "No CBC",])

risk_20_at_3y_1 <- risk_20_at_3y_final[,c('person_id','Status','Risk')]
risk_20_at_5y_1 <- risk_20_at_5y_final[,c('person_id','Status','Risk')]

colnames(risk_20_at_3y_1)[1] <- "ID"
colnames(risk_20_at_5y_1)[1] <- "ID"

colnames(risk_20_at_3y_1)[3] <- "CBCRisk-Black"
colnames(risk_20_at_5y_1)[3] <- "CBCRisk-Black"

risk_12_20_at_3y <- inner_join(risk_12_at_3y_1, risk_20_at_3y_1, by = c("ID", "Status"))  # Applying inner_join() function
risk_12_20_at_5y <- inner_join(risk_12_at_5y_1, risk_20_at_5y_1, by = c("ID", "Status"))  # Applying inner_join() function

risk_12_20_at_3y_risks <- risk_12_20_at_3y[,c(2:4)]
risk_12_20_at_5y_risks <- risk_12_20_at_5y[,c(2:4)]

risk_12_20_at_3y_long <- melt(risk_12_20_at_3y_risks, id = "Status") 
risk_12_20_at_3y_long <-  risk_12_20_at_3y_long %>%
  mutate(Status = case_when(
    Status == "CBC" ~ paste("CBC (n <=", as.character(nrow(risk_12_20_at_3y_long[risk_12_20_at_3y_long$variable == "CBCRisk" & risk_12_20_at_3y_long$Status == "CBC",])), ")"),
    Status == "No CBC" ~ paste("No CBC (n <=", as.character(nrow(risk_12_20_at_3y_long[risk_12_20_at_3y_long$variable == "CBCRisk" & risk_12_20_at_3y_long$Status == "No CBC",])), ")")
  ))

risk_12_20_at_5y_long <- melt(risk_12_20_at_5y_risks, id = "Status") 
risk_12_20_at_5y_long <-  risk_12_20_at_5y_long %>%
  mutate(Status = case_when(
    Status == "CBC" ~ paste("CBC (n <=", as.character(nrow(risk_12_20_at_5y_long[risk_12_20_at_5y_long$variable == "CBCRisk" & risk_12_20_at_5y_long$Status == "CBC",])), ")"),
    Status == "No CBC" ~ paste("No CBC (n <=", as.character(nrow(risk_12_20_at_5y_long[risk_12_20_at_5y_long$variable == "CBCRisk" & risk_12_20_at_5y_long$Status == "No CBC",])), ")")
  ))

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
