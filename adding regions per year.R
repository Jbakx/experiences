# This script is used for creating correct regions

# created: 26-07-2022
# updated: 26-07-2022
# owner: Jeanne Bakx

##################################################################################################################################################################

rm(list = ls())   #clear work environment

load("C:/Users/jba2102.54374/Documents/Onderzoek/Onderzoek effecten eerdere ervaringen/data/GC and EC/total_cleaned_dataset.Rdata") 
load("C:/Users/jba2102.54374/Documents/Onderzoek/Onderzoek effecten eerdere ervaringen/data/EC only/EC_cleaned_dataset.Rdata") 
load("C:/Users/jba2102.54374/Documents/Onderzoek/Onderzoek effecten eerdere ervaringen/data/GC only/GC_cleaned_dataset.Rdata") 

#check regions for EC
EC_cleaned_dataset$fuseerstezkh <- as.character(EC_cleaned_dataset$fuseerstezkh)
EC_cleaned_dataset$resec_jr <- as.numeric(format(EC_cleaned_dataset$resec_dat, "%Y"))
overview_EC_resec <- table(EC_cleaned_dataset$resec_zkh, EC_cleaned_dataset$resec_jr)

EC_2015 <- subset(EC_cleaned_dataset, incjr == 2015)
EC_2016 <- subset(EC_cleaned_dataset, incjr == 2016)
EC_2017 <- subset(EC_cleaned_dataset, incjr == 2017)
EC_2018 <- subset(EC_cleaned_dataset, incjr == 2018)
EC_2019 <- subset(EC_cleaned_dataset, incjr == 2019)

EC_referrals  <- vector(mode = "list", length = 5)

EC_referrals[[1]] <- as.data.frame.matrix(table(EC_2015$fuseerstezkh, EC_2015$resec_zkh))
EC_referrals[[2]] <- as.data.frame.matrix(table(EC_2016$fuseerstezkh, EC_2016$resec_zkh))
EC_referrals[[3]] <- as.data.frame.matrix(table(EC_2017$fuseerstezkh, EC_2017$resec_zkh))
EC_referrals[[4]] <- as.data.frame.matrix(table(EC_2018$fuseerstezkh, EC_2018$resec_zkh))
EC_referrals[[5]] <- as.data.frame.matrix(table(EC_2019$fuseerstezkh, EC_2019$resec_zkh))

for (i in 1:5){
  EC_referrals[[i]]$referred_patients <- rowSums(EC_referrals[[i]])
  EC_referrals[[i]]$max_referrals <- apply(EC_referrals[[i]][,1:(ncol(EC_referrals[[i]])-1)], MARGIN=1, FUN=max)
  EC_referrals[[i]]$most_referred_to <- colnames(EC_referrals[[i]][,1:(ncol(EC_referrals[[i]])-2)])[apply(EC_referrals[[i]][,1:(ncol(EC_referrals[[i]])-2)],1,which.max)]
  EC_referrals[[i]]$percentage_referred <- EC_referrals[[i]]$max_referrals / EC_referrals[[i]]$referred_patients
  EC_referrals[[i]]$referred_from <- rownames(EC_referrals[[i]])
}

write.xlsx(EC_referrals, "C:/Users/jba2102.54374/Documents/Onderzoek/Onderzoek effecten eerdere ervaringen/resultaten/final/EC referrals.xlsx",
           rowNames = TRUE, colNames = TRUE, overwrite = TRUE, sheetName = c("2015", "2016", "2017", "2018", "2019"))

EC_2015_extended <- left_join(EC_2015, data.frame("referred_from" = EC_referrals[[1]]$referred_from, "most_referred_to" = EC_referrals[[1]]$most_referred_to), by = c("fuseerstezkh" = "referred_from"))
EC_2016_extended <- left_join(EC_2016, data.frame("referred_from" = EC_referrals[[2]]$referred_from, "most_referred_to" = EC_referrals[[2]]$most_referred_to), by = c("fuseerstezkh" = "referred_from"))
EC_2017_extended <- left_join(EC_2017, data.frame("referred_from" = EC_referrals[[3]]$referred_from, "most_referred_to" = EC_referrals[[3]]$most_referred_to), by = c("fuseerstezkh" = "referred_from"))
EC_2018_extended <- left_join(EC_2018, data.frame("referred_from" = EC_referrals[[4]]$referred_from, "most_referred_to" = EC_referrals[[4]]$most_referred_to), by = c("fuseerstezkh" = "referred_from"))
EC_2019_extended <- left_join(EC_2019, data.frame("referred_from" = EC_referrals[[5]]$referred_from, "most_referred_to" = EC_referrals[[5]]$most_referred_to), by = c("fuseerstezkh" = "referred_from"))


# if resectie == 1, region is equal to the resection hospital (resec_zkh)
# if resectie == 0, region is equal to the resection hospital to which most resected 
# patients are referred by the first hospital (fuseerstezkh) in that specific year

EC_2015_extended$region <- ifelse(EC_2015_extended$resectie == 1, EC_2015_extended$resec_zkh, EC_2015_extended$most_referred_to)
EC_2016_extended$region <- ifelse(EC_2016_extended$resectie == 1, EC_2016_extended$resec_zkh, EC_2016_extended$most_referred_to)
EC_2017_extended$region <- ifelse(EC_2017_extended$resectie == 1, EC_2017_extended$resec_zkh, EC_2017_extended$most_referred_to)
EC_2018_extended$region <- ifelse(EC_2018_extended$resectie == 1, EC_2018_extended$resec_zkh, EC_2018_extended$most_referred_to)
EC_2019_extended$region <- ifelse(EC_2019_extended$resectie == 1, EC_2019_extended$resec_zkh, EC_2019_extended$most_referred_to)





#check regions for GC
GC_cleaned_dataset$resec_jr <- as.numeric(format(GC_cleaned_dataset$resec_dat, "%Y"))
overview_GC_resec <- table(GC_cleaned_dataset$resec_zkh, GC_cleaned_dataset$resec_jr)


