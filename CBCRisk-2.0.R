# install.packages("survival")
# Installs CBCRisk package from local CBCRisk_2.0.tar
install.packages("cbcrisk_2.0.tar", repos=NULL, type="source")
# install.packages('caret')
# install.packages('dplyr')
# install.packages('fabricatr')
# install.packages('timereg')
# install.packages('pROC')
# install.packages('mice')
# install.packages('tidyr')
library(survival)
library(cbcrisk)
library(dplyr)
library(fabricatr)
library(timereg)
library(caret)
library(pROC)
library(mice)
library(tidyr)

vec <- read.csv("final_patient_df_v7_final_4282023.csv")

prof <- vec[,c(4:14,3,15,2)]

# Profile vector names
prof$age_at_first_bc_c <- as.numeric(prof$age_at_first_bc_c)
prof$anti_est_ther_c <- as.numeric(prof$anti_est_ther_c)
prof$bc_history_c <- as.numeric(prof$bc_history_c)
prof$pre_neoplasia_diag_c <- as.numeric(prof$pre_neoplasia_diag_c)
prof$ER_status_c <- as.numeric(prof$ER_status_c)
prof$first_bc_type_c <- as.numeric(prof$first_bc_type_c)
prof$age_at_first_birth_c <- as.numeric(prof$age_at_first_birth_c)

prof$breast_density_c <- as.numeric(prof$breast_density_c)
prof$cbc_status_c <- as.numeric(prof$cbc_status_c)
prof$cbc_status_3y_c <- as.numeric(prof$cbc_status_3y_c)
prof$cbc_status_5y_c <- as.numeric(prof$cbc_status_5y_c)
prof$age_at_first_bc <- as.numeric(prof$age_at_first_bc)

prof$race_c <- as.numeric(prof$race_c)
prof$person_id <- as.numeric(prof$person_id)

###########################################################################################

cbcrisk_2_est <- function(cbc_year, year) {
  risk <- data.frame(Age_at_risk = numeric(), Risk = numeric(), Age_at_bc = numeric(), Status = character(), person_id = numeric())
  
  for (x in 1:length(cbc_year[,1])) {
    if (cbc_year[x,13] == 2) {
      if (cbc_year[x,1] == 1 | cbc_year[x,1] == 2) {
        patient_input <- c(3, cbc_year[x,3], 4, 1)
      } else {
        patient_input <- c(3, cbc_year[x,3], 4, 2)
      }
      cbcrisk_output <- cbcrisk(cbc_year[x,13], patient_input, cbc_year[x,12], year)
      risk[x,1:2] <- cbcrisk_output$risk[c(1),]
      risk[x,3] <- cbc_year[x,12]
      if (year == 3) {
        risk[x,4] <- cbc_year[x,10]
        risk[x,5] <- cbc_year[x,14]
      } else if (year == 5) {
        risk[x,4] <- cbc_year[x,11]
        risk[x,5] <- cbc_year[x,14]
      }
    } else{
      patient_input <- c(cbc_year[x,1], 3, cbc_year[x,3], cbc_year[x,4], cbc_year[x,5], cbc_year[x,6], cbc_year[x,7],
                         cbc_year[x,8])
      cbcrisk_output <- cbcrisk(cbc_year[x,13], profile = patient_input, start.age = cbc_year[x,12], pred.year = year)
      risk[x,1:2] <- cbcrisk_output$risk[c(1),]
      risk[x,3] <- cbc_year[x,12]
      if (year == 3) {
        risk[x,4] <- cbc_year[x,10]
        risk[x,5] <- cbc_year[x,14]
      } else if (year == 5) {
        risk[x,4] <- cbc_year[x,11]
        risk[x,5] <- cbc_year[x,14]
      }
    }
  }
  risk <-  risk %>%
    mutate(Status = case_when(
      Status == 1 ~ "CBC",
      Status == 2 ~ "No CBC"
    ))
  return(risk)
}

cbc_3y_pre <- prof[prof$cbc_status_3y_c == 1 | prof$cbc_status_3y_c == 2,]
cbc_3y <- cbc_3y_pre[cbc_3y_pre$age_at_first_bc < 86,]
risk_20_at_3y <- cbcrisk_2_est(cbc_3y, 3)

cbc_5y_pre <- prof[prof$cbc_status_5y_c == 1 | prof$cbc_status_5y_c == 2,]
cbc_5y <- cbc_5y_pre[cbc_5y_pre$age_at_first_bc < 84,]
risk_20_at_5y <- cbcrisk_2_est(cbc_5y, 5)

# write.csv(risk_20_at_3y, file = "risk_2.0_at_3y.csv")
# write.csv(risk_20_at_5y, file = "risk_2.0_at_5y.csv")

#################################### ER Imputation ######################################################

prof_imput <- prof
for (x in 1:length(prof_imput[,1])) {
  if (prof_imput[x,2] == 1) {
    prof_imput[x,6] == 2
  }
}

cbc_3y_imput_pre <- prof_imput[prof_imput$cbc_status_3y_c == 1 | prof_imput$cbc_status_3y_c == 2,]
cbc_3y_imput <- cbc_3y_imput_pre[cbc_3y_imput_pre$age_at_first_bc < 86,]
risk_20_imput_at_3y <- cbcrisk_2_est(cbc_3y_imput, 3)

cbc_5y_imput_pre <- prof_imput[prof_imput$cbc_status_5y_c == 1 | prof_imput$cbc_status_5y_c == 2,]
cbc_5y_imput <- cbc_5y_imput_pre[cbc_5y_imput_pre$age_at_first_bc < 84,]
risk_20_imput_at_5y <- cbcrisk_2_est(cbc_5y_imput, 5)

colnames(risk_20_imput_at_3y)[2] <- "ER-Risk"
colnames(risk_20_imput_at_5y)[2] <- "ER-Risk"

risk_20_at_3y_final <- inner_join(risk_20_at_3y, risk_20_imput_at_3y, by = c("person_id", "Age_at_risk", "Age_at_bc", "Status"))  # Applying inner_join() function
risk_20_at_5y_final <- inner_join(risk_20_at_5y, risk_20_imput_at_5y, by = c("person_id", "Age_at_risk", "Age_at_bc", "Status"))  # Applying inner_join() function

risk_20_at_3y_final <- risk_20_at_3y_final[,c(5, 3, 1, 4, 2, 6)]
risk_20_at_5y_final <- risk_20_at_5y_final[,c(5, 3, 1, 4, 2, 6)]

#################################### CSV Save ######################################################

write.csv(risk_20_at_3y_final, file = "risk_2.0_at_3y_final.csv")
write.csv(risk_20_at_5y_final, file = "risk_2.0_at_5y_final.csv")
