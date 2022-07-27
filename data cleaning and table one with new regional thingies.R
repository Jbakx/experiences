# This script is used for cleaning the raw dataset and creating categorical variables.
# The output of this script is a clean dataset, table one and overview table with the 
# number/percentage of events for all regions

# created: 17-08-2021
# updated: 18-08-2021
# owner: Jeanne Bakx

##################################################################################################################################################################

rm(list = ls())   #clear work environment

# install.packages("haven")
# install.packages("ggplot2")
# install.packages("tidyverse")

library(tidyverse)
library(haven)
library(ggplot2)
library(tableone)
library(openxlsx)

#load raw data
data_path <- "C:/Users/jba2102.54374/Documents/Onderzoek/Onderzoek effecten eerdere ervaringen/data/database_v7.sas7bdat"  
dataset   <- read_sas(data_path)

##################################################################################################################################################################
# step 1: Patient selection based on morphology
##################################################################################################################################################################

#esophageal cancer topography (with cardia)
NET_EC <- c(8002, 8013, 8041, 8042, 8043, 8044, 8045, 8154, 8243, 8244, 8245, 8246)               #codes voor neuroendocriene tumoren
PCC_EC <- c(8051, 8052, 8070, 8071, 8072, 8073, 8074, 8075, 8076, 8078, 8082, 8083, 8084)         #codes voor plaveiselcelcarcinomen
ACC_EC <- c(8140, 8141, 8142, 8143, 8144, 8145, 8147, 8190, 8201, 8210, 8211, 8230, 8255,         #codes voor adenocarcinomen
            8260, 8261, 8262, 8263, 8310, 8320, 8323, 8401, 8471, 8480, 8481, 8490, 8510,         #codes voor adenocarcinomen
            8512, 8520, 8530, 8542, 8560, 8570, 8571, 8572, 8573, 8574, 8576, 8000, 8010)         #codes voor adenocarcinomen
OTH_EC <- c(8001, 8012, 8020, 8021, 8022, 8031, 8032, 8033, 8046, 8094, 8123, 8200, 8430,         #codes voor overige tumoren
            8550, 8575, 8980, 9990)


#gastric cancer topography (without cardia)
NET_GC <- c(8002, 8013, 8041, 8042, 8043, 8044, 8045, 8154, 8243, 8244, 8245, 8246)               #codes voor neuroendocriene tumoren
ACC_GC <- c(8140, 8141, 8142, 8143, 8144, 8145, 8147, 8190, 8201, 8210, 8211, 8230, 8255,         #codes voor adenocarcinomen
            8260, 8261, 8262, 8263, 8310, 8320, 8323, 8401, 8471, 8480, 8481, 8490, 8510,         #codes voor adenocarcinomen
            8512, 8520, 8530, 8542, 8560, 8570, 8571, 8572, 8573, 8574, 8576, 8000, 8010)         #codes voor adenocarcinomen
OTH_GC <- c(8001, 8012, 8020, 8021, 8022, 8031, 8032, 8033, 8046, 8070, 8071, 8072, 8082,         #codes voor overige tumoren
            8200, 8430, 8550, 8575, 8980, 9990)

#create new topgraphy variable which includes junction tumors in the esophageal group
dataset$topo_temp <- ifelse(dataset$topo_sublok == "C160", "C15", dataset$topo)

#patient who reseived esophageal resection
resec_topo_esoph <- select(dataset, c(ther_code1:ther_code15))
resec_topo_esoph$resection_code_C15_1 <- grepl("C15", resec_topo_esoph$ther_code1)
resec_topo_esoph$resection_code_C15_2 <- grepl("C15", resec_topo_esoph$ther_code2)
resec_topo_esoph$resection_code_C15_3 <- grepl("C15", resec_topo_esoph$ther_code3)
resec_topo_esoph$resection_code_C15_4 <- grepl("C15", resec_topo_esoph$ther_code4)
resec_topo_esoph$resection_code_C15_5 <- grepl("C15", resec_topo_esoph$ther_code5)
resec_topo_esoph$resection_code_C15_6 <- grepl("C15", resec_topo_esoph$ther_code6)
resec_topo_esoph$resection_code_C15_7 <- grepl("C15", resec_topo_esoph$ther_code7)
resec_topo_esoph$resection_code_C15_8 <- grepl("C15", resec_topo_esoph$ther_code8)

resec_topo_esoph$resection_code_C15_sum <-rowSums(select(resec_topo_esoph, resection_code_C15_1:resection_code_C15_8)) 
resec_topo_esoph$resection_code_C15 <- ifelse(resec_topo_esoph$resection_code_C15_sum >= 1, 1, 0)

#patient who reseived gastric resection
resec_topo_gastric <- select(dataset, c(ther_code1:ther_code15))
resec_topo_gastric$resection_code_C16_1 <- grepl("C16", resec_topo_gastric$ther_code1)
resec_topo_gastric$resection_code_C16_2 <- grepl("C16", resec_topo_gastric$ther_code2)
resec_topo_gastric$resection_code_C16_3 <- grepl("C16", resec_topo_gastric$ther_code3)
resec_topo_gastric$resection_code_C16_4 <- grepl("C16", resec_topo_gastric$ther_code4)
resec_topo_gastric$resection_code_C16_5 <- grepl("C16", resec_topo_gastric$ther_code5)
resec_topo_gastric$resection_code_C16_6 <- grepl("C16", resec_topo_gastric$ther_code6)
resec_topo_gastric$resection_code_C16_7 <- grepl("C16", resec_topo_gastric$ther_code7)
resec_topo_gastric$resection_code_C16_8 <- grepl("C16", resec_topo_gastric$ther_code8)

resec_topo_gastric$resection_code_C16_sum <-rowSums(select(resec_topo_gastric, resection_code_C16_1:resection_code_C16_8)) 
resec_topo_gastric$resection_code_C16 <- ifelse(resec_topo_gastric$resection_code_C16_sum >= 1, 1, 0)

for (i in 1:nrow(dataset)) {
  if (resec_topo_esoph$resection_code_C15[i] == 1) {
    dataset$topo_new[i] <- "C15"
  }
  else if (resec_topo_gastric$resection_code_C16[i] == 1) {
    dataset$topo_new[i] <- "C16"
  } else {
    dataset$topo_new[i] <- dataset$topo_temp[i]
  }
}

check_topo <- select(dataset, c(topo, topo_new, ther_code1:ther_code15)) 


#Create new variable for morphology based on location and morf-code
dataset$morfologie <- vector(mode = "numeric", length = nrow(dataset))

for (i in 1:nrow(dataset)) {
  if (dataset$topo_new[i] == "C15") {
    if (dataset$morf[i] %in% NET_EC) {
      dataset$morfologie[i] <- "NET"
    } else if (dataset$morf[i] %in% PCC_EC) {
      dataset$morfologie[i] <- "PCC"
    } else if (dataset$morf[i] %in% ACC_EC) {
      dataset$morfologie[i] <- "AC"
    } else if (dataset$morf[i] %in% OTH_EC) {
      dataset$morfologie[i] <- "other"
    }
  }
  if (dataset$topo_new[i] == "C16") {
    if (dataset$morf[i] %in% NET_GC) {
      dataset$morfologie[i] <- "NET"
    } else if (dataset$morf[i] %in% ACC_GC) {
      dataset$morfologie[i] <- "AC"
    } else if (dataset$morf[i] %in% OTH_GC) {
      dataset$morfologie[i] <- "other"
    }
  }
}

#exclude patients
dataset_1   <- subset(dataset, morfologie != "NET")   #remove patients with neuroendocrine tumors
dataset_5   <- subset(dataset_1, ct != "1")           #exclude cT1
dataset_6   <- subset(dataset_5, ct != "1A")          #exclude cT1A
dataset_7   <- subset(dataset_6, ct != "1B")          #exclude cT1B



##################################################################################################################################################################
# step 2: Patient selection based on region
##################################################################################################################################################################
dataset_7$resectie     <- ifelse(is.na(dataset_7$resectie), 0, dataset_7$resectie)         #vervangen van NA door 0 (= geen resectie)

#create an overview of referral patterns for EC resection
EC_dataset <- subset(dataset_7, topo_new == "C15")
EC_dataset$fuseerstezkh <- as.character(EC_dataset$fuseerstezkh)
# EC_dataset$resec_jr <- as.numeric(format(EC_dataset$resec_dat, "%Y"))
# overview_EC_resec <- table(EC_dataset$fuszkhresec, EC_dataset$resec_jr)

EC_2015 <- subset(EC_dataset, incjr == 2015)
EC_2016 <- subset(EC_dataset, incjr == 2016)
EC_2017 <- subset(EC_dataset, incjr == 2017)
EC_2018 <- subset(EC_dataset, incjr == 2018)
EC_2019 <- subset(EC_dataset, incjr == 2019)

EC_referrals  <- vector(mode = "list", length = 6)

EC_referrals[[1]] <- as.data.frame.matrix(table(EC_2015$fuseerstezkh, EC_2015$fuszkhresec))
EC_referrals[[2]] <- as.data.frame.matrix(table(EC_2016$fuseerstezkh, EC_2016$fuszkhresec))
EC_referrals[[3]] <- as.data.frame.matrix(table(EC_2017$fuseerstezkh, EC_2017$fuszkhresec))
EC_referrals[[4]] <- as.data.frame.matrix(table(EC_2018$fuseerstezkh, EC_2018$fuszkhresec))
EC_referrals[[5]] <- as.data.frame.matrix(table(EC_2019$fuseerstezkh, EC_2019$fuszkhresec))
EC_referrals[[6]] <- as.data.frame.matrix(table(EC_dataset$fuseerstezkh, EC_dataset$fuszkhresec))

for (i in 1:6){
  EC_referrals[[i]]$referred_patients <- rowSums(EC_referrals[[i]])
  EC_referrals[[i]]$max_referrals <- apply(EC_referrals[[i]][,1:(ncol(EC_referrals[[i]])-1)], MARGIN=1, FUN=max)
  EC_referrals[[i]]$most_referred_to <- colnames(EC_referrals[[i]][,1:(ncol(EC_referrals[[i]])-2)])[apply(EC_referrals[[i]][,1:(ncol(EC_referrals[[i]])-2)],1,which.max)]
  EC_referrals[[i]]$percentage_referred <- EC_referrals[[i]]$max_referrals / EC_referrals[[i]]$referred_patients
  EC_referrals[[i]]$referred_from <- rownames(EC_referrals[[i]])
}

write.xlsx(EC_referrals, "C:/Users/jba2102.54374/Documents/Onderzoek/Onderzoek effecten eerdere ervaringen/resultaten/final/EC referral of resected patients.xlsx",
           rowNames = TRUE, colNames = TRUE, overwrite = TRUE, sheetName = c("2015", "2016", "2017", "2018", "2019", "overall"))

# assign regions ------------------------------------------------------------------------------------------------------------------------------------------
# if resectie == 1, region is equal to the resection hospital (fuszkhresec)
# if resectie == 0, region is equal to the resection hospital to which at the majority of resected 
# patients are referred to by the first hospital (fuseerstezkh) in that specific year (see Excel overview)

verwijstabel_EC <- read.xlsx("C:/Users/jba2102.54374/Documents/Onderzoek/Onderzoek effecten eerdere ervaringen/resultaten/final/overview EC referral of resected patients.xlsx", sheet = "verwijstabel")

for (i in 1:nrow(EC_dataset)) {
  if (EC_dataset$resectie[i] == 1) {
    EC_dataset$region[i] <- EC_dataset$fuszkhresec[i]
  } else {
    EC_dataset$region[i] <- verwijstabel_EC$regio_zkh[verwijstabel_EC[,"jaar"] == EC_dataset$incjr[i] & verwijstabel_EC[,"zkh_nr"] == EC_dataset$fuseerstezkh[i]]
  }
}


#-------------------------------------------------------------------------------------------------------------------------------------------------------------
#create an overview of referral patterns for GC resection
GC_dataset <- subset(dataset_7, topo_new == "C16")
GC_dataset$fuseerstezkh <- as.character(GC_dataset$fuseerstezkh)
# GC_dataset$resec_jr <- as.numeric(format(GC_dataset$resec_dat, "%Y"))
# overview_GC_resec <- table(GC_dataset$fuszkhresec, GC_dataset$resec_jr)

GC_2015 <- subset(GC_dataset, incjr == 2015)
GC_2016 <- subset(GC_dataset, incjr == 2016)
GC_2017 <- subset(GC_dataset, incjr == 2017)
GC_2018 <- subset(GC_dataset, incjr == 2018)
GC_2019 <- subset(GC_dataset, incjr == 2019)

GC_referrals  <- vector(mode = "list", length = 6)

GC_referrals[[1]] <- as.data.frame.matrix(table(GC_2015$fuseerstezkh, GC_2015$fuszkhresec))
GC_referrals[[2]] <- as.data.frame.matrix(table(GC_2016$fuseerstezkh, GC_2016$fuszkhresec))
GC_referrals[[3]] <- as.data.frame.matrix(table(GC_2017$fuseerstezkh, GC_2017$fuszkhresec))
GC_referrals[[4]] <- as.data.frame.matrix(table(GC_2018$fuseerstezkh, GC_2018$fuszkhresec))
GC_referrals[[5]] <- as.data.frame.matrix(table(GC_2019$fuseerstezkh, GC_2019$fuszkhresec))
GC_referrals[[6]] <- as.data.frame.matrix(table(GC_dataset$fuseerstezkh, GC_dataset$fuszkhresec))

for (i in 1:6){
  GC_referrals[[i]]$referred_patients <- rowSums(GC_referrals[[i]])
  GC_referrals[[i]]$max_referrals <- apply(GC_referrals[[i]][,1:(ncol(GC_referrals[[i]])-1)], MARGIN=1, FUN=max)
  GC_referrals[[i]]$most_referred_to <- colnames(GC_referrals[[i]][,1:(ncol(GC_referrals[[i]])-2)])[apply(GC_referrals[[i]][,1:(ncol(GC_referrals[[i]])-2)],1,which.max)]
  GC_referrals[[i]]$percentage_referred <- GC_referrals[[i]]$max_referrals / GC_referrals[[i]]$referred_patients
  GC_referrals[[i]]$referred_from <- rownames(GC_referrals[[i]])
}

write.xlsx(GC_referrals, "C:/Users/jba2102.54374/Documents/Onderzoek/Onderzoek effecten eerdere ervaringen/resultaten/final/GC referral of resected patients.xlsx",
           rowNames = TRUE, colNames = TRUE, overwrite = TRUE, sheetName = c("2015", "2016", "2017", "2018", "2019", "overall"))





#VERWIJSTABEL AFMAKEN :)



# assign regions ------------------------------------------------------------------------------------------------------------------------------------------
# if resectie == 1, region is equal to the resection hospital (fuszkhresec)
# if resectie == 0, region is equal to the resection hospital to which at the majority of resected 
# patients are referred to by the first hospital (fuseerstezkh) in that specific year (see Excel overview)

verwijstabel_GC <- read.xlsx("C:/Users/jba2102.54374/Documents/Onderzoek/Onderzoek effecten eerdere ervaringen/resultaten/final/overview GC referral of resected patients.xlsx", sheet = "verwijstabel")

for (i in 1:nrow(GC_dataset)) {
  if (GC_dataset$resectie[i] == 1) {
    GC_dataset$region[i] <- GC_dataset$fuszkhresec[i]
  } else {
    GC_dataset$region[i] <- verwijstabel_GC$regio_zkh[verwijstabel_GC[,"jaar"] == GC_dataset$incjr[i] & verwijstabel_GC[,"zkh_nr"] == GC_dataset$fuseerstezkh[i]]
  }
}








# 
# EC_2015_extended$region <- ifelse(EC_2015_extended$resectie == 1, EC_2015_extended$fuszkhresec, ifelse((EC_2015_extended$resectie == 0 & EC_2015_extended$percentage_referred > 0.5), EC_2015_extended$most_referred_to, NA))
# EC_2016_extended$region <- ifelse(EC_2016_extended$resectie == 1, EC_2016_extended$fuszkhresec, ifelse((EC_2016_extended$resectie == 0 & EC_2016_extended$percentage_referred > 0.5), EC_2016_extended$most_referred_to, NA))
# EC_2017_extended$region <- ifelse(EC_2017_extended$resectie == 1, EC_2017_extended$fuszkhresec, ifelse((EC_2017_extended$resectie == 0 & EC_2017_extended$percentage_referred > 0.5), EC_2017_extended$most_referred_to, NA))
# EC_2018_extended$region <- ifelse(EC_2018_extended$resectie == 1, EC_2018_extended$fuszkhresec, ifelse((EC_2018_extended$resectie == 0 & EC_2018_extended$percentage_referred > 0.5), EC_2018_extended$most_referred_to, NA))
# EC_2019_extended$region <- ifelse(EC_2019_extended$resectie == 1, EC_2019_extended$fuszkhresec, ifelse((EC_2019_extended$resectie == 0 & EC_2019_extended$percentage_referred > 0.5), EC_2019_extended$most_referred_to, NA))
# 
# EC_list <- list()
# EC_list[[1]] <- EC_2015_extended
# EC_list[[2]] <- EC_2016_extended
# EC_list[[3]] <- EC_2017_extended
# EC_list[[4]] <- EC_2018_extended
# EC_list[[5]] <- EC_2019_extended
# 
# write.xlsx(EC_list, "C:/Users/jba2102.54374/Documents/Onderzoek/Onderzoek effecten eerdere ervaringen/resultaten/final/EC referral after assignment of non-resected patients.xlsx",
#            rowNames = TRUE, colNames = TRUE, overwrite = TRUE, sheetName = c("2015", "2016", "2017", "2018", "2019"))
# 
# EC_regional_dataset <- rbind(EC_2015_extended, EC_2016_extended, EC_2017_extended, EC_2018_extended, EC_2019_extended)
# 
# #EC_referrals_after_assignment <- as.data.frame.matrix(table(EC_regional_dataset$region, EC_regional_dataset$fuseerstezkh))
# 
# #write.xlsx(EC_referrals_after_assignment, "C:/Users/jba2102.54374/Documents/Onderzoek/Onderzoek effecten eerdere ervaringen/resultaten/final/EC referral after region assingment.xlsx",
#  #          rowNames = TRUE, colNames = TRUE, overwrite = TRUE)
# 
# #check regions for GC--------------------------------------------------------------------------------------------------------------------------
# GC_dataset <- subset(dataset_7, topo_new == "C16")
# GC_dataset$fuseerstezkh <- as.character(GC_dataset$fuseerstezkh)
# GC_dataset$resec_jr <- as.numeric(format(GC_dataset$resec_dat, "%Y"))
# overview_GC_resec <- table(GC_dataset$fuszkhresec, GC_dataset$resec_jr)
# 
# GC_2015 <- subset(GC_dataset, incjr == 2015)
# GC_2016 <- subset(GC_dataset, incjr == 2016)
# GC_2017 <- subset(GC_dataset, incjr == 2017)
# GC_2018 <- subset(GC_dataset, incjr == 2018)
# GC_2019 <- subset(GC_dataset, incjr == 2019)
# 
# GC_referrals  <- vector(mode = "list", length = 5)
# 
# GC_referrals[[1]] <- as.data.frame.matrix(table(GC_2015$fuseerstezkh, GC_2015$fuszkhresec))
# GC_referrals[[2]] <- as.data.frame.matrix(table(GC_2016$fuseerstezkh, GC_2016$fuszkhresec))
# GC_referrals[[3]] <- as.data.frame.matrix(table(GC_2017$fuseerstezkh, GC_2017$fuszkhresec))
# GC_referrals[[4]] <- as.data.frame.matrix(table(GC_2018$fuseerstezkh, GC_2018$fuszkhresec))
# GC_referrals[[5]] <- as.data.frame.matrix(table(GC_2019$fuseerstezkh, GC_2019$fuszkhresec))
# 
# for (i in 1:5){
#   GC_referrals[[i]]$referred_patients <- rowSums(GC_referrals[[i]])
#   GC_referrals[[i]]$max_referrals <- apply(GC_referrals[[i]][,1:(ncol(GC_referrals[[i]])-1)], MARGIN=1, FUN=max)
#   GC_referrals[[i]]$most_referred_to <- colnames(GC_referrals[[i]][,1:(ncol(GC_referrals[[i]])-2)])[apply(GC_referrals[[i]][,1:(ncol(GC_referrals[[i]])-2)],1,which.max)]
#   GC_referrals[[i]]$percentage_referred <- GC_referrals[[i]]$max_referrals / GC_referrals[[i]]$referred_patients
#   GC_referrals[[i]]$referred_from <- rownames(GC_referrals[[i]])
# }
# 
# write.xlsx(GC_referrals, "C:/Users/jba2102.54374/Documents/Onderzoek/Onderzoek effecten eerdere ervaringen/resultaten/final/GC referrals.xlsx",
#            rowNames = TRUE, colNames = TRUE, overwrite = TRUE, sheetName = c("2015", "2016", "2017", "2018", "2019"))
# 
# GC_2015_extended <- left_join(GC_2015, data.frame("referred_from" = GC_referrals[[1]]$referred_from, "most_referred_to" = GC_referrals[[1]]$most_referred_to, "percentage_referred" = GC_referrals[[1]]$percentage_referred), by = c("fuseerstezkh" = "referred_from"))
# GC_2016_extended <- left_join(GC_2016, data.frame("referred_from" = GC_referrals[[2]]$referred_from, "most_referred_to" = GC_referrals[[2]]$most_referred_to, "percentage_referred" = GC_referrals[[2]]$percentage_referred), by = c("fuseerstezkh" = "referred_from"))
# GC_2017_extended <- left_join(GC_2017, data.frame("referred_from" = GC_referrals[[3]]$referred_from, "most_referred_to" = GC_referrals[[3]]$most_referred_to, "percentage_referred" = GC_referrals[[3]]$percentage_referred), by = c("fuseerstezkh" = "referred_from"))
# GC_2018_extended <- left_join(GC_2018, data.frame("referred_from" = GC_referrals[[4]]$referred_from, "most_referred_to" = GC_referrals[[4]]$most_referred_to, "percentage_referred" = GC_referrals[[4]]$percentage_referred), by = c("fuseerstezkh" = "referred_from"))
# GC_2019_extended <- left_join(GC_2019, data.frame("referred_from" = GC_referrals[[5]]$referred_from, "most_referred_to" = GC_referrals[[5]]$most_referred_to, "percentage_referred" = GC_referrals[[5]]$percentage_referred), by = c("fuseerstezkh" = "referred_from"))
# 
# 
# # if resectie == 1, region is equal to the resection hospital (fuszkhresec)
# # if resectie == 0, region is equal to the resection hospital to which most resected 
# # patients are referred by the first hospital (fuseerstezkh) in that specific year
# 
# GC_2015_extended$region <- ifelse(GC_2015_extended$resectie == 1, GC_2015_extended$fuszkhresec, ifelse((GC_2015_extended$resectie == 0 & GC_2015_extended$percentage_referred > 0.5), GC_2015_extended$most_referred_to, NA))
# GC_2016_extended$region <- ifelse(GC_2016_extended$resectie == 1, GC_2016_extended$fuszkhresec, ifelse((GC_2016_extended$resectie == 0 & GC_2016_extended$percentage_referred > 0.5), GC_2016_extended$most_referred_to, NA))
# GC_2017_extended$region <- ifelse(GC_2017_extended$resectie == 1, GC_2017_extended$fuszkhresec, ifelse((GC_2017_extended$resectie == 0 & GC_2017_extended$percentage_referred > 0.5), GC_2017_extended$most_referred_to, NA))
# GC_2018_extended$region <- ifelse(GC_2018_extended$resectie == 1, GC_2018_extended$fuszkhresec, ifelse((GC_2018_extended$resectie == 0 & GC_2018_extended$percentage_referred > 0.5), GC_2018_extended$most_referred_to, NA))
# GC_2019_extended$region <- ifelse(GC_2019_extended$resectie == 1, GC_2019_extended$fuszkhresec, ifelse((GC_2019_extended$resectie == 0 & GC_2019_extended$percentage_referred > 0.5), GC_2019_extended$most_referred_to, NA))
# 
# GC_regional_dataset <- rbind(GC_2015_extended, GC_2016_extended, GC_2017_extended, GC_2018_extended, GC_2019_extended)
# 
# total_regional_dataset <- rbind(EC_regional_dataset, GC_regional_dataset)
# 
# #replace unknown resection hospitals with 'most likely referred to' hospital
# total_regional_dataset$region  <- ifelse((total_regional_dataset$resectie == 1 & is.na(total_regional_dataset$fuszkhresec)), total_regional_dataset$most_referred_to, total_regional_dataset$region)



#Add region names



#Remove patients from LUMC, AVL, foreign hospital and unknown hospital
regional_dataset <- subset(total_regional_dataset, !is.na(region))

# dataset_2   <- subset(dataset_1, !is.na(regio))       #remove patients without regio
# dataset_3   <- subset(dataset_2, regio != 17)         #remove patients from AvL  (= 316)
# dataset_4   <- subset(dataset_3, regio != 18)         #remove patients from LUMC (= 384)
# dataset_5   <- subset(dataset_4, regio != buitenland)         #remove patients from foreign hospitals (= 998)


temp <- cbind(regional_dataset1$resectie, regional_dataset1$resec_zkh, regional_dataset1$fuszkhresec, regional_dataset1$fuseerstezkh, regional_dataset1$most_referred_to, regional_dataset1$percentage_referred)





##################################################################################################################################################################
# step 2: Imputation of MDO dates
##################################################################################################################################################################

#create an overview of how many patients had an MDO scheduled
dataset_7$MDO_JaNee          <- ifelse(is.na(dataset_7$mdo_datum_def), 0, 1)
dataset_7$diagnose_JaNee     <- ifelse(is.na(dataset_7$incdat), 0, 1)
mdos                         <- aggregate(dataset_7$MDO_JaNee, by=list(Regio=dataset_7$regio, Incjr = dataset_7$incjr), FUN=sum)
diags                        <- aggregate(dataset_7$diagnose_JaNee, by=list(Regio=dataset_7$regio, Incjr = dataset_7$incjr), FUN=sum)
perc_mdo                     <- by(as.numeric(dataset_7$MDO_JaNee), dataset_7$regio, mean)            #percentage per regio aantal patienten met MDO


# create a histogram for the distribution in time between MDO date and start treatment
dataset_7$beslisdagen <- dataset_7$startbeh - dataset_7$mdo_datum_def
hist(as.numeric(dataset_7$beslisdagen), breaks =  seq(0, 10000, by=5) , xlim = range(0, 100))
verdeling_beslisdagen_per_regio <- by(as.numeric(dataset_7$beslisdagen), dataset_7$regio, summary)

# create a histogram for the distribution in time between incidence date and MDO date
dataset_7$diag_mdo <- as.Date(dataset_7$mdo_datum_def)-as.Date(dataset_7$incdat)
hist(as.numeric(dataset_7$diag_mdo), breaks =  seq(-10000, 10000, by=5) , xlim = range(-50, 100))
verdeling_diagnosedagen_per_regio <- by(as.numeric(dataset_7$diag_mdo), dataset_7$regio, summary)

# find the median time between MDO date and start treatment for each region separately
data_temp <- split(dataset_7, f = dataset_7$regio)                   
dagen_mdo_start   <- vector(mode = "numeric", length = length(data_temp))
dagen_incl_mdo    <- vector(mode = "numeric", length = length(data_temp))

for (i in 1:length(data_temp)) {
  temp_beslis         <- subset(data_temp[[i]], !is.na(beslisdagen))        #all patients with a known MDO date and treatment startdate
  dagen_mdo_start[i]  <- round(median(temp_beslis$beslisdagen))             #median time between MDO date and start treatment for each region 
  
  temp_diagnose       <- subset(data_temp[[i]], !is.na(diag_mdo))           #all patients with a known MDO date and incidence date 
  dagen_incl_mdo[i]   <- round(median(temp_diagnose$diag_mdo))              #median time between incidence date and MDO date for each region 
}


# first try imputing missing MDO dates based on the treatment start date
# if no treatment startdate is known, then impute MDO date based on incidence date
for (i in 1:length(data_temp)) {
  data_temp[[i]]$mdo_datum_def <- ifelse(is.na(data_temp[[i]]$mdo_datum_def) & !is.na(data_temp[[i]]$startbeh), data_temp[[i]]$startbeh - dagen_mdo_start[i], data_temp[[i]]$mdo_datum_def)       #imputatie a.d.h.v. startdatum behandeling
  data_temp[[i]]$mdo_datum_def <- ifelse(is.na(data_temp[[i]]$mdo_datum_def) & !is.na(data_temp[[i]]$incdat),   data_temp[[i]]$incdat + dagen_incl_mdo[i],    data_temp[[i]]$mdo_datum_def)       #imputatie a.d.h.v. inclusiedatum
}

dataset_regio <- do.call("rbind", data_temp)
dataset_regio$mdo_datum_def <- as.Date(dataset_regio$mdo_datum_def, origin = "1970-01-01")

# now dataset_regio contains (imputed) MDO dates for all patients


##################################################################################################################################################################
# step 3: Preprocessing variables
##################################################################################################################################################################
#add variables concerning duration of hospital admission and ICU admission
dataset_regio$ic_duur1_adj <- ifelse(is.na(dataset_regio$ic_duur1), 0, dataset_regio$ic_duur1)         #adjusted IC duur
dataset_regio$ic_duur2_adj <- ifelse(is.na(dataset_regio$ic_duur2), 0, dataset_regio$ic_duur2)         #adjusted IC duur
dataset_regio$IC_duur      <- dataset_regio$ic_duur1_adj + dataset_regio$ic_duur2_adj                  #total days ICU stay

#if ICU stay is unknown (999), than impute with median (=0 dagen)
dataset_regio$IC_duur      <- ifelse(dataset_regio$IC_duur == 999, median(dplyr::filter(dataset_regio, resectie == 1)$IC_duur), dataset_regio$IC_duur) 
dataset_regio$duur_opname  <- ifelse(is.na(dataset_regio$duur_opname), 0, dataset_regio$duur_opname)   #adjusted opname duur

# #add variables concerning recurrences
# dataset_regio$recur_6mnd       <- ifelse((dataset_regio$eerste_meta - dataset_regio$resec_dat) <= 182 & (dataset_regio$eerste_meta - dataset_regio$resec_dat) >= 7, 1, 0)                                                         #recurrence binnen 6 maanden na resectie
# #dataset_regio$recur_6mnd       <- ifelse(dataset_regio$operatie_type == "exploratief" | dataset_regio$operatie_type == "SANO", 0, dataset_regio$recur_6mnd)             #recurrence geldt alleen na (endoscopische) resectie
# dataset_regio$recur_6mnd       <- ifelse(is.na(dataset_regio$recur_6mnd), 0, dataset_regio$recur_6mnd)            
# 
# dataset_regio$recur_12mnd      <- ifelse((dataset_regio$eerste_meta - dataset_regio$resec_dat) <= 365 & (dataset_regio$eerste_meta - dataset_regio$resec_dat) >= 7, 1, 0)                                                         #recurrence binnen 12 maanden na resectie
# #dataset_regio$recur_12mnd      <- ifelse(dataset_regio$operatie_type == "exploratief" | dataset_regio$operatie_type == "SANO", 0, dataset_regio$recur_12mnd)            #recurrence geldt alleen na (endoscopische) resectie
# dataset_regio$recur_12mnd      <- ifelse(is.na(dataset_regio$recur_12mnd), 0, dataset_regio$recur_12mnd)            
# 
# check_recurrence <- data.frame(dataset_regio$rn, dataset_regio$recur_12mnd, dataset_regio$resec_dat, dataset_regio$eerste_meta)

#create variables for 30 and 90 day mortality
dataset_regio$ovltijd <- dataset_regio$ovldat - dataset_regio$resec_dat
dataset_regio$mort30  <- ifelse(dataset_regio$ovltijd <= 30, 1, 0)  
dataset_regio$mort30  <- ifelse(is.na(dataset_regio$mort30), 0, dataset_regio$mort30)
dataset_regio$mort90  <- ifelse(dataset_regio$ovltijd <= 90, 1, 0)  
dataset_regio$mort90  <- ifelse(is.na(dataset_regio$mort90), 0, dataset_regio$mort90)

#add complication date in case of cardial complications 
for (i in 1:nrow(dataset_regio)) {
  if (dataset_regio$postopcomp1[i] == "1") {
    dataset_regio$cardiaal_dtm[i] <- as.Date(dataset_regio$complicatie_dat1[i])
  } else if (dataset_regio$postopcomp2[i] == "1") {
    dataset_regio$cardiaal_dtm[i] <- as.Date(dataset_regio$complicatie_dat2[i])
  } else if (dataset_regio$postopcomp3[i] == "1") {
    dataset_regio$cardiaal_dtm[i] <- as.Date(dataset_regio$complicatie_dat3[i])
  } else {
    dataset_regio$cardiaal_dtm[i] <- NA
  }
}

dataset_regio$cardiaal_dtm <- as.Date(dataset_regio$cardiaal_dtm, origin = "1970-01-01")


#add complication date in case of pulmonal complications 
for (i in 1:nrow(dataset_regio)) {
  if (dataset_regio$postopcomp1[i] == "2") {
    dataset_regio$pulmonaal_dtm[i] <- as.Date(dataset_regio$complicatie_dat1[i])
  } else if (dataset_regio$postopcomp2[i] == "2") {
    dataset_regio$pulmonaal_dtm[i] <- as.Date(dataset_regio$complicatie_dat2[i])
  } else if (dataset_regio$postopcomp3[i] == "2") {
    dataset_regio$pulmonaal_dtm[i] <- as.Date(dataset_regio$complicatie_dat3[i])
  } else {
    dataset_regio$pulmonaal_dtm[i] <- NA
  }
}

dataset_regio$pulmonaal_dtm <- as.Date(dataset_regio$pulmonaal_dtm, origin = "1970-01-01")


#add complication date in case of anastomotic leakage
for (i in 1:nrow(dataset_regio)) {
  if (dataset_regio$naadlekkage1[i] == "3") {
    dataset_regio$naadlekkage_dtm[i] <- as.Date(dataset_regio$complicatie_dat1[i])
  } else if (dataset_regio$naadlekkage1[i] == "4") {
    dataset_regio$naadlekkage_dtm[i] <- as.Date(dataset_regio$complicatie_dat1[i])
  } else if (dataset_regio$naadlekkage1[i] == "5") {
    dataset_regio$naadlekkage_dtm[i] <- as.Date(dataset_regio$complicatie_dat1[i])
  } else {
    dataset_regio$naadlekkage_dtm[i] <- NA
  }
}

for (i in 1:nrow(dataset_regio)) {
  if (dataset_regio$naadlekkage2[i] == "3") {
    dataset_regio$naadlekkage_dtm[i] <- as.Date(dataset_regio$complicatie_dat2[i])
  } else if (dataset_regio$naadlekkage2[i] == "4") {
    dataset_regio$naadlekkage_dtm[i] <- as.Date(dataset_regio$complicatie_dat2[i])
  } else if (dataset_regio$naadlekkage2[i] == "5") {
    dataset_regio$naadlekkage_dtm[i] <- as.Date(dataset_regio$complicatie_dat2[i])
  }
}

for (i in 1:nrow(dataset_regio)) {
  if (dataset_regio$naadlekkage3[i] == "3") {
    dataset_regio$naadlekkage_dtm[i] <- as.Date(dataset_regio$complicatie_dat3[i])
  } else if (dataset_regio$naadlekkage3[i] == "4") {
    dataset_regio$naadlekkage_dtm[i] <- as.Date(dataset_regio$complicatie_dat3[i])
  } else if (dataset_regio$naadlekkage3[i] == "5") {
    dataset_regio$naadlekkage_dtm[i] <- as.Date(dataset_regio$complicatie_dat3[i])
  }
}

dataset_regio$naadlekkage_datum <- as.Date(dataset_regio$naadlekkage_dtm, origin = "1970-01-01")
dataset_regio$naadlekkage <- ifelse(is.na(dataset_regio$naadlekkage_dtm), 0, 1)

##################################################################################################################################################################
# step 4: Imputation of complication dates for cardial and pulmonal complications
##################################################################################################################################################################
dataset_regio$cardiaal_datum <- ifelse(dataset_regio$cardiaal == 1 & is.na(dataset_regio$cardiaal_dtm), 
                                      dataset_regio$resec_dat + median(dataset_regio$cardiaal_dtm - dataset_regio$resec_dat, na.rm = TRUE) , dataset_regio$cardiaal_dtm)
dataset_regio$cardiaal_datum <- as.Date(dataset_regio$cardiaal_datum, origin = "1970-01-01")


dataset_regio$pulmonaal_datum <- ifelse(dataset_regio$pulmonaal == 1 & is.na(dataset_regio$pulmonaal_dtm), 
                                       dataset_regio$resec_dat + median(dataset_regio$pulmonaal_dtm - dataset_regio$resec_dat, na.rm = TRUE) , dataset_regio$pulmonaal_dtm)
dataset_regio$pulmonaal_datum <- as.Date(dataset_regio$pulmonaal_datum, origin = "1970-01-01")


##################################################################################################################################################################
# step 5: Add variables based on "normal" duration of hospital/ICU admittance per region 
##################################################################################################################################################################

# resected_patients <- subset(dataset_regio, operatie_type == "reguliere resectie") 
# hist(as.numeric(resected_patients$IC_duur), breaks =  seq(0, 1000, by=1) , xlim = range(0, 10))
#IC_duur_grens <- round(quantile(resected_patients$IC_duur, probs = c(0.95), na.rm = TRUE)) #longer than 12 days = abnormal
#dataset_regio$IC_opname_lang <- ifelse(dataset_regio$IC_duur > IC_duur_grens, 1, 0)
dataset_regio$IC_opname_lang <- ifelse(dataset_regio$topo_new == "C15" & dataset_regio$IC_duur >= 13, 1, 0)
dataset_regio$IC_opname_lang <- ifelse(dataset_regio$topo_new == "C16" & dataset_regio$IC_duur >= 4, 1, dataset_regio$IC_opname_lang)

# hist(as.numeric(resected_patients$duur_opname), breaks =  seq(0, 1000, by=1) , xlim = range(0, 50))
#opname_duur_grens <- round(quantile(resected_patients$duur_opname, probs = c(0.95), na.rm = TRUE)) #longer than 45 days = abnormal
#dataset_regio$opname_lang <- ifelse(dataset_regio$duur_opname > opname_duur_grens, 1, 0)
dataset_regio$opname_lang <- ifelse(dataset_regio$topo_new == "C15" & dataset_regio$duur_opname >= 50, 1, 0)
dataset_regio$opname_lang <- ifelse(dataset_regio$topo_new == "C16" & dataset_regio$duur_opname >= 33, 1, dataset_regio$IC_opname_lang)

dataset_regio$opname_21 <- ifelse(dataset_regio$duur_opname > 21 | dataset_regio$IC_duur > 21, 1, 0)
dataset_regio$opname_21_datum <- ifelse(dataset_regio$opname_21 == 1, dataset_regio$resec_dat + 21, NA)
dataset_regio$opname_21_datum <- as.Date(dataset_regio$opname_21_datum, origin = "1970-01-01")

data_list <- split(dataset_regio, f = dataset_regio$regio)


#any complication that is stated in 'Gecompliceerd beloop' by DUCA
for (i in 1:length(data_list)) {
  for (j in 1:nrow(data_list[[i]])) {
    
    data_list[[i]]$som_compl[j] <-
      sum(data_list[[i]]$cardiaal[j],
          data_list[[i]]$pulmonaal[j],
          data_list[[i]]$naadlekkage[j],
          data_list[[i]]$opname_21[j],
          data_list[[i]]$mort30[j],
          na.rm = T)
    
    data_list[[i]]$datum_eerste_compl1[j] <-
      min(data_list[[i]]$cardiaal_datum[j],
          data_list[[i]]$pulmonaal_datum[j],
          data_list[[i]]$naadlekkage_datum[j],
          data_list[[i]]$opname_21_datum[j],
          data_list[[i]]$ovldat[j],
          na.rm = T)
  
    }
  data_list[[i]]$datum_eerste_compl1           <- as.Date(data_list[[i]]$datum_eerste_compl1, origin = "1970-01-01")
}

# dataset_clean <- do.call("rbind", data_list)
# check_complications <- data.frame(dataset_clean$cardiaal, dataset_clean$pulmonaal, dataset_clean$naadlekkage, dataset_clean$opname_21, dataset_clean$mort30,
#                                   dataset_clean$resec_dat, dataset_clean$datum_eerste_compl, dataset_clean$cardiaal_datum, dataset_clean$pulmonaal_datum, 
#                                   dataset_clean$naadlekkage_datum, dataset_clean$opname_21_datum, dataset_clean$ovldat)

#Only events within 30 days after resection are taken into account (geldt met name voor ovldat)
for (i in 1:length(data_list)) {
  for (j in 1:nrow(data_list[[i]])) {
    data_list[[i]]$datum_eerste_compl[j] <- ifelse( (!is.na(data_list[[i]]$resec_dat[j]) &
                                                           data_list[[i]]$resec_dat[j] + 30 >= data_list[[i]]$datum_eerste_compl1[j]) &
                                                          (data_list[[i]]$resec_dat[j] <= data_list[[i]]$datum_eerste_compl1[j]),
                                                        data_list[[i]]$datum_eerste_compl1[j], NA)
  }
  data_list[[i]]$datum_eerste_compl           <- as.Date(data_list[[i]]$datum_eerste_compl, origin = "1970-01-01")
}

dataset_clean <- do.call("rbind", data_list)
check_complications2 <- data.frame(dataset_clean$cardiaal, dataset_clean$pulmonaal, dataset_clean$naadlekkage, dataset_clean$opname_21, dataset_clean$mort30,
                                   dataset_clean$resec_dat, dataset_clean$datum_eerste_compl, dataset_clean$cardiaal_datum, dataset_clean$pulmonaal_datum, 
                                  dataset_clean$naadlekkage_datum, dataset_clean$opname_21_datum, dataset_clean$ovldat)


##################################################################################################################################################################
# step 6: Creating dummy variables and select reference values
##################################################################################################################################################################
#create categorical variables for table one ---------------------------------------------------------------------------------
dataset_clean$sex_cat <- ifelse(dataset_clean$gesl == 2, "Female", "Male")

dataset_clean$age_cat <- ifelse(dataset_clean$leeft <  60 , "<60 years", 
                          ifelse(dataset_clean$leeft >= 60 & dataset_clean$leeft <= 74, "60-74 years", 
                                 ifelse(dataset_clean$leeft >= 75 , ">74 years" , NA))) 

dataset_clean$hist_cat    <- dataset_clean$morfologie

dataset_clean$ct     <-  ifelse(dataset_clean$ct == "X" | dataset_clean$ct == "", "X", dataset_clean$ct)
dataset_clean$cT_cat <- dataset_clean$ct

dataset_clean$cn  <-  ifelse(dataset_clean$cn == "3" | dataset_clean$cn == "3A" | dataset_clean$cn == "3B", "3", dataset_clean$cn)
dataset_clean$cn  <-  ifelse(dataset_clean$cn == "X" | dataset_clean$cn == "", "X", dataset_clean$cn)
dataset_clean$cN_cat <- dataset_clean$cn

dataset_clean$topo_cat    <- ifelse(dataset_clean$topo_new == "C15", "Esophageal cancer", "Gastric cancer")

dataset_clean$cci_cat     <- ifelse(is.na(dataset_clean$cci_cat), 3, dataset_clean$cci_cat)         #combine all unknown comorbidities
dataset_clean$comorb_cat  <- ifelse(dataset_clean$cci_cat == 0, "no comorbidities", 
                              ifelse(dataset_clean$cci_cat == 1, "1 comorbidity", 
                                     ifelse(dataset_clean$cci_cat == 2, "2 or more comorbidities", "unknown"))) 

dataset_clean$perfstat      <- ifelse(is.na(dataset_clean$perf_stat), 9, dataset_clean$perf_stat)         #combine all unknown perfstats
dataset_clean$perfstat_cat  <- ifelse(dataset_clean$perf_stat == 4 | dataset_clean$perf_stat == 3, "ECOG 3 or 4", 
                                ifelse(dataset_clean$perf_stat == 2, "ECOG 2", 
                                       ifelse(dataset_clean$perf_stat == 1, "ECOG 1", 
                                              ifelse(dataset_clean$perf_stat == 0, "ECOG 0", "unknown"))))

EC_cleaned_dataset <- subset(dataset_clean, topo_cat == 'Esophageal cancer')
GC_cleaned_dataset <- subset(dataset_clean, topo_cat == 'Gastric cancer')

save(dataset_clean, file = "C:/Users/jba2102.54374/Documents/Onderzoek/Onderzoek effecten eerdere ervaringen/data/GC and EC/total_cleaned_dataset.Rdata") 
save(EC_cleaned_dataset, file = "C:/Users/jba2102.54374/Documents/Onderzoek/Onderzoek effecten eerdere ervaringen/data/EC only/EC_cleaned_dataset.Rdata") 
save(GC_cleaned_dataset, file = "C:/Users/jba2102.54374/Documents/Onderzoek/Onderzoek effecten eerdere ervaringen/data/GC only/GC_cleaned_dataset.Rdata") 


##################################################################################################################################################################
# step 7: Creating Table One patient characteristics
##################################################################################################################################################################
df_table_one <- data.frame(sex = as.factor(dataset_clean$sex_cat),
                           age = as.factor(dataset_clean$age_cat),
                           histology = as.factor(dataset_clean$hist_cat),
                           cT = as.factor(dataset_clean$cT_cat),
                           cN = as.factor(dataset_clean$cN_cat),
                           topography = as.factor(dataset_clean$topo_cat),
                           comorbidities = as.factor(dataset_clean$comorb_cat),
                           perfstat = as.factor(dataset_clean$perfstat_cat),
                           regio = as.character(dataset_clean$regio),
                           resection = as.factor(dataset_clean$resectie))
Vars1 <- colnames(df_table_one)
Vars2 <- subset(Vars1, Vars1 !="regio")
Vars3 <- subset(Vars2, Vars2 !="topography")

results  <- vector(mode = "list", length = 2)

tab1 <- CreateTableOne(vars = Vars2, data = df_table_one)
results[[1]] <- print(tab1, showAllLevels = TRUE, quote = FALSE, noSpaces = TRUE, printToggle = FALSE)

#stratified by topography
tab3 <- CreateTableOne(vars = Vars3, data = df_table_one, strata = "topography")
results[[2]] <- print(tab3, showAllLevels = TRUE, quote = FALSE, noSpaces = TRUE, printToggle = FALSE)

write.xlsx(results, "C:/Users/jba2102.54374/Documents/Onderzoek/Onderzoek effecten eerdere ervaringen/resultaten/final/table one.xlsx",
           rowNames = TRUE, colNames = TRUE, overwrite = TRUE, sheetName = c("overall", "topography"))


##################################################################################################################################################################
# step 7: Creating Table One complications in resected patients
##################################################################################################################################################################
dataset_resec <- subset(dataset_clean, resectie == 1)

df_table_one_compl <- data.frame(topography = as.factor(dataset_resec$topo_cat),
                           card_compl = as.factor(dataset_resec$cardiaal),
                           pulm_compl = as.factor(dataset_resec$pulmonaal),
                           naadlekkage = as.factor(dataset_resec$naadlekkage),
                           opname_lang = as.factor(dataset_resec$opname_21),
                           mort_30 = as.factor(dataset_resec$mort30))
Vars1 <- colnames(df_table_one_compl)
Vars2 <- subset(Vars1, Vars1 !="topography")

results  <- vector(mode = "list", length = 2)

tab1 <- CreateTableOne(vars = Vars2, data = df_table_one_compl)
results[[1]] <- print(tab1, showAllLevels = TRUE, quote = FALSE, noSpaces = TRUE, printToggle = FALSE)

#stratified by topography
tab3 <- CreateTableOne(vars = Vars2, data = df_table_one_compl, strata = "topography")
results[[2]] <- print(tab3, showAllLevels = TRUE, quote = FALSE, noSpaces = TRUE, printToggle = FALSE)

write.xlsx(results, "C:/Users/jba2102.54374/Documents/Onderzoek/Onderzoek effecten eerdere ervaringen/resultaten/final/table one complications.xlsx",
           rowNames = TRUE, colNames = TRUE, overwrite = TRUE, sheetName = c("overall", "topography"))


