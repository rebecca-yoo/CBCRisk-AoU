# # install.packages("survival")
# # Installs CBCRisk package from local CBCRisk_1.2.tar
# # install.packages("CBCRisk_1.2.tar", repos=NULL, type="source")
# # install.packages('caret')
# # install.packages('dplyr')
# install.packages('fabricatr')
# install.packages('timereg')
# install.packages('pROC')
# install.packages('mice')
# install.packages('tidyr')
library(survival)
# library(CBCRisk)
library(dplyr)
library(fabricatr)
library(timereg)
library(caret)
library(pROC)
library(mice)
library(tidyr)

vec <- read.csv("final_patient_df_v7_final_4282023.csv")

prof <- vec[,c("age_at_first_bc_c", "anti_est_ther_c", "bc_history_c", "pre_neoplasia_diag_c", "breast_density_c", "ER_status_c", 
               "first_bc_type_c", "age_at_first_birth_c", "cbc_status_3y_c", "cbc_status_5y_c", "age_at_first_bc", "cbc_status_c", 
               "race_c", "person_id")]

risk_12_at_3y_final <- read.csv("risk_1.2_at_3y_final.csv")
risk_12_at_5y_final <- read.csv("risk_1.2_at_5y_final.csv")

risk_20_at_3y_final <- read.csv("risk_2.0_at_3y_final.csv")
risk_20_at_5y_final <- read.csv("risk_2.0_at_5y_final.csv")

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

# Age at First BC and Type of First BC known for all patients
prof_known <- data.frame(prof)

prof_known$age_at_first_bc_k <- 1

prof_known <-  prof_known %>%
  mutate(anti_est_ther_k = case_when(
    anti_est_ther_c == 1 ~ 1,
    anti_est_ther_c == 2 ~ 1,
    anti_est_ther_c == 3 ~ 0
  ), 
  bc_history_k = case_when(
    bc_history_c == 1 ~ 1,
    bc_history_c == 2 ~ 1,
    bc_history_c == 3 ~ 0),
  pre_neoplasia_diag_k = case_when(
    pre_neoplasia_diag_c == 1 ~ 1,
    pre_neoplasia_diag_c == 2 ~ 0,
  ))

prof_known$breast_density_k <- 0

prof_known <-  prof_known %>%
  mutate(ER_status_k = case_when(
    ER_status_c == 1 ~ 1,
    ER_status_c == 2 ~ 1,
    ER_status_c == 3 ~ 0
  ))

prof_known$type_of_first_bc_k <- 1

prof_known <-  prof_known %>%
  mutate(age_at_first_birth_k = case_when(
    age_at_first_birth_c == 1 ~ 1,
    age_at_first_birth_c == 2 ~ 1,
    age_at_first_birth_c == 3 ~ 1,
    age_at_first_birth_c == 4 ~ 0
  ))

prof_known$type_of_first_bc_k <- 1

prof_known <- prof_known[,c(15:22)]

count <- rowSums(prof_known)

risk_freq <- colSums(prof_known)

prof_known_df <- cbind(prof, prof_known, count)

prof_known_3_min <- prof_known_df[prof_known_df$count >= 3,]
prof_known_2_max <- prof_known_df[prof_known_df$count < 3,]

prof_known_20_race_1 <- prof_known_df[prof_known_df$count >= 3 & prof_known_df$race_c == 1,]
prof_known_20_race_2 <- prof_known_df[prof_known_df$age_at_first_bc_k == 1 & prof_known_df$bc_history_k == 1 & prof_known_df$race_c == 2,]
# prof_known_20_race_2 <- prof_known_20_race_2[,c("age_at_first_bc_c", "anti_est_ther_c", "bc_history_c", "pre_neoplasia_diag_c", "breast_density_c", "ER_status_c", 
#                                       "first_bc_type_c", "age_at_first_birth_c", "cbc_status_3y_c", "cbc_status_5y_c", "age_at_first_bc", "cbc_status_c", 
#                                        "race_c", "person_id","count")]
prof_known_20 <- rbind(prof_known_20_race_1, prof_known_20_race_2)

prof_known_df <-  prof_known_df %>%
  mutate(CBC_Status = case_when(
    cbc_status_c == 1 ~ paste("CBC (n <=", as.character(nrow(prof_known_df[prof_known_df$cbc_status_c == 1,])), ")"),
    cbc_status_c == 2 ~ paste("No CBC (n <=", as.character(nrow(prof_known_df[prof_known_df$cbc_status_c == 2,])), ")"),
  ))

prof_known_df$count <- as.numeric(prof_known_df$count)
prof_known_df <-  prof_known_df %>%
  mutate(two_or_more = case_when(
    count < 3 ~ "Two factors",
    count >= 3 ~ "Three or more factors",
  ))

prof_known_count <- aggregate(prof_known_df$two_or_more, by=list(prof_known_df$two_or_more, prof_known_df$CBC_Status), FUN=length)
prof_known_count$CBC_Status_count <- c(75, 75, 78, 78)
prof_known_count$percent <- (prof_known_count$x/prof_known_count$CBC_Status_count)
colnames(prof_known_count)[2] <- "CBC_Status"
prof_known_count <- prof_known_count[c(2,4,1,3),]
ggplot(prof_known_count, aes(x = reorder(Group.1, -percent), y = percent, fill = CBC_Status, label = scales::percent(percent))) +
  geom_bar(stat = "identity", position = "dodge") +
  xlab("Number of Known Risk Factors") + ylab("Percentage of Cohort") + theme(text = element_text(size = 15)) +
  scale_y_continuous(limit = c(0,0.8),labels = scales::percent) +
  geom_text(position = position_dodge(width = .9),    # move to center of bars
            vjust = -0.5,    # nudge above top of bar
            size = 6)

colnames(risk_12_at_3y_final)[colnames(risk_12_at_3y_final)=='ID'] <- "person_id"
colnames(risk_12_at_5y_final)[colnames(risk_12_at_5y_final)=='ID'] <- "person_id"

risk_12_at_3y_final_3_min <- inner_join(prof_known_3_min, risk_12_at_3y_final, by = c("person_id"))  # Applying inner_join() function
risk_12_at_5y_final_3_min <- inner_join(prof_known_3_min, risk_12_at_5y_final, by = c("person_id"))  # Applying inner_join() function

risk_20_at_3y_final_3_min <- inner_join(prof_known_20, risk_20_at_3y_final, by = c("person_id"))  # Applying inner_join() function
risk_20_at_5y_final_3_min <- inner_join(prof_known_20, risk_20_at_5y_final, by = c("person_id"))  # Applying inner_join() function

#################################### CSV Save ######################################################

write.csv(risk_20_at_3y_final_3_min, file = "risk_20_at_3y_final_3_min.csv")
write.csv(risk_20_at_5y_final_3_min, file = "risk_20_at_5y_final_3_min.csv")

write.csv(risk_12_at_3y_final_3_min, file = "risk_12_at_3y_final_3_min.csv")
write.csv(risk_12_at_5y_final_3_min, file = "risk_12_at_5y_final_3_min.csv")
