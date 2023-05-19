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

# Age at First BC
cbc_age_at_first_bc <- table(prof$age_at_first_bc_c, prof$cbc_status_5y_c)
write.csv(cbc_age_at_first_bc, file = "cbc_age_at_first_bc_5y.csv")
read.csv("cbc_age_at_first_bc_5y.csv", row.names = 1)

# Anti-Estrogen Therapy
cbc_anti_est_ther <- table(prof$anti_est_ther_c, prof$cbc_status_5y_c)
write.csv(cbc_anti_est_ther, file = "cbc_anti_est_ther_5y.csv")
read.csv("cbc_anti_est_ther_5y.csv", row.names = 1)

# First-Degree Family History of BC
cbc_bc_history <- table(prof$bc_history_c, prof$cbc_status_5y_c)
write.csv(cbc_bc_history, file = "cbc_bc_history_5y.csv")
read.csv("cbc_bc_history_5y.csv", row.names = 1)

# High Risk Preneoplasia
cbc_pre_neoplasia_diag <- table(prof$pre_neoplasia_diag_c, prof$cbc_status_5y_c)
write.csv(cbc_pre_neoplasia_diag, file = "cbc_pre_neoplasia_diag_5y.csv")
read.csv("cbc_pre_neoplasia_diag_5y.csv", row.names = 1)

# Estrogen Receptor Status
cbc_ER_status <- table(prof$ER_status_c, prof$cbc_status_5y_c)
write.csv(cbc_ER_status, file = "cbc_ER_status_5y.csv")
read.csv("cbc_ER_status_5y.csv", row.names = 1)

# Type of First BC
cbc_first_bc_type <- table(prof$first_bc_type_c, prof$cbc_status_5y_c)
write.csv(cbc_first_bc_type, file = "cbc_first_bc_type_5y.csv")
read.csv("cbc_first_bc_type_5y.csv", row.names = 1)

# Age at First Birth
cbc_age_at_first_birth <- table(prof$age_at_first_birth_c, prof$cbc_status_5y_c)
write.csv(cbc_age_at_first_birth, file = "cbc_age_at_first_birth_5y.csv")
read.csv("cbc_age_at_first_birth_5y.csv", row.names = 1)

# Race
cbc_race <- table(prof$race_c, prof$cbc_status_5y_c)
write.csv(cbc_race, file = "cbc_race_5y.csv")
read.csv("cbc_race_5y.csv", row.names = 1)
