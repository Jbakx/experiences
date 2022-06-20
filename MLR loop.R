# Multilevel logistic regression models for TIMELINE in specific population:
# The effect of all events in previous EC and GC patient on the treatment decision in the fragile 
# population of both EC and GC patients with age >= 75 and/or performance status >= 2. 

# Jeanne Bakx
# 19-08-2021 aangemaakt
# 20-04-2022 bijgewerkt

rm(list = ls())   #clear work environment

library(lme4)
library(nlme)
library(Rcpp)
library(openxlsx)
library(optimx)
#_____________________________________________________________________________________________________________________________
#state which events and which definition of recent you want to use for the input for the analysis
def_recent  <- "timeline"  #do you want to look at the events in recent time or recent cases? Can be "timeline" or "cases"
EC_events   <- TRUE     #do you want to use the events reported in EC patients as input? Can be TRUE or FALSE
GC_events   <- FALSE     #do you want to use the events reported in GC patients as input? Can be TRUE or FALSE

#_____________________________________________________________________________________________________________________________
#state which patients you want to include for the output of the analysis
EC_patients <- TRUE     #do you want to apply the analysis on EC patients? Can be TRUE or FALSE
GC_patients <- FALSE     #do you want to apply the analysis on GC patients? Can be TRUE or FALSE
fragile     <- FALSE     #do you want to apply the analysis on fragile patients only? Can be TRUE or FALSE

#_____________________________________________________________________________________________________________________________
#load folder and dataset based on set parameters
main_folder <- "C:/Users/jba2102.54374/Documents/Onderzoek/Onderzoek effecten eerdere ervaringen"

input_events <- ifelse(EC_events == TRUE & GC_events == TRUE, "GC and EC", 
                       ifelse(EC_events == TRUE & GC_events == FALSE, "EC only",
                              ifelse(EC_events == FALSE & GC_events == TRUE, "GC only", "incorrect")))

if (input_events != "incorrect" & (def_recent %in% c("timeline", "cases"))) {
  print(paste("Your input for the analysis is based on the number of events in ", input_events, 
              " using the ", def_recent, " definition of recent.", sep = ""))
} else {
  print("Check whether the selected input variables are correct")
}

folder <- (paste(main_folder, "data", input_events, def_recent, sep = "/"))                             
dataset <- get(load(paste(folder, "/data_", def_recent, ".Rdata", sep = "")))


##################################################################################################################################################################
# step 1: find regions with low resectionvolume
##################################################################################################################################################################
if (input_events == "GC and EC") {
  low_volume_regio <- which(aggregate(dataset$resectie, by=list(Regio=dataset$regio), FUN=sum)$x < 100)
} else {
  low_volume_regio <- which(aggregate(dataset$resectie, by=list(Regio=dataset$regio), FUN=sum)$x < 50)
}

##################################################################################################################################################################
# step 2: remove patients with MDTM date before the 1st resection + 90 days (when def_recent = timeline) OR remove patients with
#         MDTM date before the 30th resection (when def_recent = cases) due to lack of information from previous patients
##################################################################################################################################################################

data_list <- split(dataset, f = dataset$regio)                   
datumgrens <- vector(mode = "numeric", length = length(data_list))

if (def_recent == "timeline") {

#find patients with both neoadjuvant chemo and resection
#Than order based on resection date and take first date + 90 days
  for (i in 1:length(data_list)) {
    temp_dat <- subset(data_list[[i]], chemo == 1)        #patients with presurgical CRT
    datumgrens[i] <- temp_dat$resec_dat[order(temp_dat$resec_dat, decreasing = FALSE, na.last = TRUE)][1] + 90
  }
  datumgrens <- as.Date(datumgrens, origin = "1970-01-01")

} else if (def_recent == "cases") {

  #find the 40th patient undergoing resection for each region
  for (i in 1:length(data_list)) {
    temp_dat <- subset(data_list[[i]], resectie == 1)   
    datumgrens[i] <- temp_dat$resec_dat[order(temp_dat$resec_dat, decreasing = FALSE, na.last = TRUE)][40]
  }
  datumgrens <- as.Date(datumgrens, origin = "1970-01-01")
}


#remove patients with MDO date before datumgrens for further analysis
data_list_new <- NULL
data_list_new <- vector(mode = "list",    length = length(data_list))
patients_excl <- vector(mode = "numeric", length = length(data_list)+1)
patients_incl <- vector(mode = "numeric", length = length(data_list)+1)

for (i in 1:length(data_list)) {
  data_list_new[[i]] <- data_list[[i]][ which(data_list[[i]]$mdo_datum_def >= datumgrens[i]),]
  patients_excl[i] <- nrow(data_list[[i]])-nrow(data_list_new[[i]])
  patients_incl[i] <- nrow(data_list_new[[i]])
}

patients_incl[17] <- sum(patients_incl)
patients_excl[17] <- sum(patients_excl)
datumgrens[17] <- NA

check_datumgrens <- data.frame(regio = c(1:16, "total"),
                               datumgrens = datumgrens,
                               excluded = patients_excl,
                               included = patients_incl)

folder <- (paste(main_folder, "resultaten/final", def_recent, "datumgrens", sep = "/"))     

write.xlsx(list(check_datumgrens), paste(folder, "/datumgrens per regio", input_events, ".xlsx"),
           rowNames = FALSE, colNames = TRUE, overwrite = TRUE,
           sheetName = c("datumgrens per regio"))

print(paste0("The date threshold for patient inclusion in each region varies between ", 
             min(check_datumgrens$datumgrens, na.rm = TRUE), " and ", max(check_datumgrens$datumgrens, na.rm = TRUE)) )

#remove patients with MDTM before date threshold for that specific region
data_incl <- do.call("rbind", data_list_new)  

#exclude regions with low resectionvolume
data_incl2 <- subset(data_incl, !(regio %in% low_volume_regio))

if (fragile == FALSE) {
  if (EC_patients == FALSE) {
    data_incl3 <- subset(data_incl2, topo_cat == "Gastric cancer")
    print(paste("Only gastric patients will be considered in this analysis (n = ", nrow(data_incl3), ")", sep = ""))
    output_patients <- "GC"
  } else if (GC_patients == FALSE) {
    data_incl3 <- subset(data_incl2, topo_cat == "Esophageal cancer")
    print(paste("Only esophageal patients will be considered in this analysis (n = ", nrow(data_incl3), ")", sep = ""))
    output_patients <- "EC"
  } else if (EC_patients == TRUE & GC_patients == TRUE) {
    data_incl3 <- data_incl2
    print(paste("Both esophageal and gastric patients will be considered in this analysis (n = ", nrow(data_incl3), ")", sep = ""))
    output_patients <- "EC and GC"
  } else {
    print("Check whether the selected output variables are correct")
  }
  
} else if (fragile == TRUE) {
  if (EC_patients == FALSE) {
    data_incl3 <- dplyr::filter(data_incl2, topo_cat == "Gastric cancer" & (leeft >= 75 | perfstat_2 == 1 | perfstat_3_4 == 1))
    print(paste("Only fragile gastric patients will be considered in this analysis (n = ", nrow(data_incl3), ")", sep = ""))
    output_patients <- "fragile GC"
  } else if (GC_patients == FALSE) {
    data_incl3 <- dplyr::filter(data_incl2, topo_cat == "Esophageal cancer" & (leeft >= 75 | perfstat_2 == 1 | perfstat_3_4 == 1))
    print(paste("Only fragile esophageal patients will be considered in this analysis (n = ", nrow(data_incl3), ")", sep = ""))
    output_patients <- "fragile EC"
  } else if (EC_patients == TRUE & GC_patients == TRUE) {
    data_incl3 <- dplyr::filter(data_incl2, (leeft >= 75 | perfstat_2 == 1 | perfstat_3_4 == 1))
    print(paste("Only fragile esophageal and gastric patients will be considered in this analysis (n = ", nrow(data_incl3), ")", sep = ""))
    output_patients <- "fragile EC and GC"
  } else {
    print("Check whether the selected output variables are correct")
  }
}



##################################################################################################################################################################
# step 3: calculate the total of DUCA events (gerelateerd aan gecompliceerd beloop) 
##################################################################################################################################################################
if (def_recent == "timeline") {
  gecompliceerd_beloop_5days  <- c("mort30_5days",  "card_compl_5days",  "pulm_compl_5days",  "naadlekkage_5days",  "opname_21days_5days")
  gecompliceerd_beloop_10days <- c("mort30_10days", "card_compl_10days", "pulm_compl_10days", "naadlekkage_10days", "opname_21days_10days")
  gecompliceerd_beloop_30days <- c("mort30_30days", "card_compl_30days", "pulm_compl_30days", "naadlekkage_30days", "opname_21days_30days")
  gecompliceerd_beloop_60days <- c("mort30_60days", "card_compl_60days", "pulm_compl_60days", "naadlekkage_60days", "opname_21days_60days")
  gecompliceerd_beloop_90days <- c("mort30_90days", "card_compl_90days", "pulm_compl_90days", "naadlekkage_90days", "opname_21days_90days")

  data_incl3$every_compl_5days   <- rowSums(data_incl3[, gecompliceerd_beloop_5days])
  data_incl3$every_compl_10days  <- rowSums(data_incl3[, gecompliceerd_beloop_10days])
  data_incl3$every_compl_30days  <- rowSums(data_incl3[, gecompliceerd_beloop_30days])
  data_incl3$every_compl_60days  <- rowSums(data_incl3[, gecompliceerd_beloop_60days])
  data_incl3$every_compl_90days  <- rowSums(data_incl3[, gecompliceerd_beloop_90days])
  
} else if (def_recent == "cases") {
  gecompliceerd_beloop_5cases  <- c("mort30_5cases",  "card_compl_5cases",  "pulm_compl_5cases",  "naadlekkage_5cases",  "opname_21days_5cases")
  gecompliceerd_beloop_10cases <- c("mort30_10cases", "card_compl_10cases", "pulm_compl_10cases", "naadlekkage_10cases", "opname_21days_10cases")
  gecompliceerd_beloop_20cases <- c("mort30_20cases", "card_compl_20cases", "pulm_compl_20cases", "naadlekkage_20cases", "opname_21days_20cases")
  gecompliceerd_beloop_30cases <- c("mort30_30cases", "card_compl_30cases", "pulm_compl_30cases", "naadlekkage_30cases", "opname_21days_30cases")
  gecompliceerd_beloop_40cases <- c("mort30_40cases", "card_compl_40cases", "pulm_compl_40cases", "naadlekkage_40cases", "opname_21days_40cases")
  
  data_incl3$every_compl_5cases   <- rowSums(data_incl3[, gecompliceerd_beloop_5cases])
  data_incl3$every_compl_10cases  <- rowSums(data_incl3[, gecompliceerd_beloop_10cases])
  data_incl3$every_compl_20cases  <- rowSums(data_incl3[, gecompliceerd_beloop_20cases])
  data_incl3$every_compl_30cases  <- rowSums(data_incl3[, gecompliceerd_beloop_30cases])
  data_incl3$every_compl_40cases  <- rowSums(data_incl3[, gecompliceerd_beloop_40cases])
}


##################################################################################################################################################################
# step 4: calculate the ratio of events based on the number of resections performed
##################################################################################################################################################################

events       <- c("any_compl", "every_compl", "mort30", "card_compl", "pulm_compl", "naadlekkage", "opname_21days")
# events       <- c("any_compl", "every_compl", "mort30", "card_compl", "pulm_compl", "naadlekkage", "opname_lang", 
#                   "opname_21days", "recur_6mnd", "opname_each_day","opname_21_each_day", "opname_IC_lang","opname_IC_each_day")
recent_time  <- c("5days", "10days", "30days", "60days", "90days")
recent_cases <- c("5cases", "10cases", "20cases", "30cases", "40cases")

#calculate the ratio of events based on the number of resections within the timeperiod
if (def_recent == "timeline") {
  for (i in 1:length(events)) {
    for (j in 1:length(recent_time)) {
      temp1 <- data_incl3[paste(events[i], "_", recent_time[j], sep = "")]
      temp2 <- data_incl3[paste("at_risk", recent_time[j], sep = "_")]
      data_incl3[paste("ratio_", names(temp1), sep = "")] <-  (temp1 / temp2) 
    }
  }
} else if (def_recent == "cases") {
  for (i in 1:length(events)) {
    for (j in 1:length(recent_cases)) {
      temp1 <- data_incl3[paste(events[i], "_", recent_cases[j], sep = "")]
      temp2 <- as.numeric(stringr::str_extract(recent_cases[j], "(\\d)+(?=cases)"))
      data_incl3[paste("ratio_", names(temp1), sep = "")] <-  (temp1 / temp2)  
    }
  }
}

#replace NaN with 0 (when no resections were performed in a certain timeperiod, this leads to NAs)
ratio_names <- colnames(data_incl3[grepl("ratio_", colnames(data_incl3))])
data_incl3[ratio_names][is.na(data_incl3[ratio_names])] <- 0

#final datasets for analysis (information about ICU stay only available in data until 2017 )
data_2019 <- data_incl3
data_2017 <- subset(data_2019, data_2019$incdat < "2018-01-01")       


##################################################################################################################################################################
# step 5: create binary variables of the number of events based on 75th percentile
##################################################################################################################################################################

if (def_recent == "timeline") {
  grens_tabel <- data.frame(noDays = recent_time, 
                            ratio_any_compl = numeric(5), 
                            ratio_every_compl = numeric(5),
                            ratio_mort30 = numeric(5), 
                            ratio_card_compl = numeric(5),
                            ratio_pulm_compl = numeric(5),
                            ratio_naadlekkage = numeric(5),
                            ratio_opname_21days = numeric(5))

  
}else if (def_recent == "cases") {
  grens_tabel <- data.frame(noCases = recent_cases, 
                            ratio_any_compl = numeric(5), 
                            ratio_every_compl = numeric(5),
                            ratio_mort30 = numeric(5), 
                            ratio_card_compl = numeric(5),
                            ratio_pulm_compl = numeric(5),
                            ratio_naadlekkage = numeric(5),
                            ratio_opname_21days = numeric(5))

}

#calculate the ratio of events based on the number of resections within the timeperiod
if (def_recent == "timeline") {
  for (i in 1:length(events)) {
    for (j in 1:length(recent_time)) {
      temp1 <- data_2019[paste("ratio_", events[i], "_", recent_time[j], sep = "")]
      grens_tabel[paste("ratio_", events[i], sep = "")][j,]  <- quantile(temp1, probs = 0.75, na.rm = T)
      data_2019[paste0("ratio_", events[i], "_", recent_time[j], "_high")] <- ifelse(data_2019[paste("ratio_", events[i], "_", recent_time[j], sep = "")] > quantile(temp1, probs = 0.75, na.rm = T), 1, 0 )
     }
  }
} else if (def_recent == "cases") {
  for (i in 1:length(events)) {
    for (j in 1:length(recent_cases)) {
      temp1 <- data_2019[paste("ratio_", events[i], "_", recent_cases[j], sep = "")]
      grens_tabel[paste("ratio_", events[i], sep = "")][j,] <- quantile(temp1, probs = 0.75, na.rm = T)
      data_2019[paste("ratio_", events[i], "_", recent_cases[j], "_high", sep = "")] <- ifelse(data_2019[paste("ratio_", events[i], "_", recent_cases[j], sep = "")] > quantile(temp1, probs = 0.75, na.rm = T), 1, 0 )
    }
  }
}

#save tabel grenswaarden
folder <- (paste(main_folder, "resultaten/final", def_recent, sep = "/"))     

write.xlsx(grens_tabel, paste(folder, "/thresholds ", input_events, " events in ", output_patients, " patients.xlsx", sep = ""),
          rowNames = FALSE, colNames = TRUE, overwrite = TRUE)



##################################################################################################################################################################
# step 6: create multilevel logistic regressionmodels
##################################################################################################################################################################
# events       <- c("any_compl", "every_compl", "mort30", "card_compl", "pulm_compl",  "naadlekkage", "opname_lang", 
#                   "opname_21days", "recur_6mnd", "opname_each_day","opname_21_each_day")
# recent_time  <- c("5days", "10days", "30days", "60days", "90days")
# recent_cases <- c("5cases", "10cases", "20cases", "30cases", "40cases")


if (def_recent == "timeline") {
  combinations <- apply(expand.grid(events, recent_time), 1, paste, collapse="_")
} else if (def_recent == "cases") {
  combinations <- apply(expand.grid(events, recent_cases), 1, paste, collapse="_")
}

outcome_model <- data.frame(matrix(nrow = length(combinations), ncol = 5))
colnames(outcome_model) <- c("event", "OR", "lower_CI", "upper_CI", "p_value")

#create factors as input for the model (first level = reference value)
data_2019$sex           <- factor(data_2019$sex_cat, levels = c("Male", "Female")) 
data_2019$age           <- factor(data_2019$age_cat, levels = c("60-74 years", "<60 years", ">74 years"))                                   
data_2019$histology     <- factor(data_2019$hist_cat, levels = c("AC", "PCC", "other"))
data_2019$cT            <- factor(data_2019$cT_cat, levels = c("2", "3", "4A", "X"))
data_2019$cN            <- factor(data_2019$cN_cat, levels = c("0", "1", "2", "3", "X"))
data_2019$topography    <- factor(data_2019$topo_cat, levels = c("Esophageal cancer", "Gastric cancer")) 
data_2019$comorbidities <- factor(data_2019$comorb_cat, levels = c("no comorbidities", "1 comorbidity", "2 or more comorbidities", "unknown"))
data_2019$perform_stat  <- factor(data_2019$perfstat_cat, levels = c("ECOG 0", "ECOG 1", "ECOG 2", "ECOG 3 or 4", "unknown"))
data_2019$regio         <- factor(data_2019$regio)
data_2019$resectie      <- factor(data_2019$resectie)

convergences <- vector(length = length(combinations))

for (i in 1:length(combinations)){
  data_2019[paste("ratio_", combinations[i], "_high", sep = "")] <- factor(as.numeric(unlist(data_2019[paste("ratio_", combinations[i], "_high", sep = "")])), levels = c(0, 1)) 
    
  # when both EC and GC are analysed, than keep 'topography' as model input
  if (output_patients == "EC and GC" | output_patients == "fragile EC and GC" ) {
    f <- formula(paste("resectie ~ sex + age + histology + cT + cN + topography + comorbidities + perform_stat + ", 
                       (paste("ratio_", combinations[i], "_high", sep = "")), 
                       "+ (1 | regio)", sep = ""))
  } else {
    # when only EC or GC is analysed, remove 'topography' as model input
    f <- formula(paste("resectie ~ sex + age + histology + cT + cN + comorbidities + perform_stat + ",
                       (paste("ratio_", combinations[i], "_high", sep = "")), 
                       "+ (1 | regio)", sep = ""))
  }
  
  warning_skip_to_next <- FALSE
  error_skip_to_next <- FALSE
  convergences[i] <- "no issues"
  
  #determine whether the model results in a convergence warning
  tryCatch({model <- glmer(f, data = data_2019, family = "binomial", control=glmerControl(optimizer = 'bobyqa', optCtrl=list(maxfun=2e5)))}, 
                    warning = function(w) { warning_skip_to_next <<- TRUE}, error = function(e) { error_skip_to_next <<- TRUE})
  
  if(warning_skip_to_next == FALSE & error_skip_to_next == FALSE ) {
    model <- glmer(f, data = data_2019, family = "binomial", control=glmerControl(optimizer = 'bobyqa', optCtrl=list(maxfun=2e5)))
    OR_model <- exp(fixef(model))
    CI_model <- exp(confint(model, parm = "beta_", level = 0.99,method = "Wald"))
    sum_model <- summary(model)
    p_value <- sum_model$coefficients[, "Pr(>|z|)"]
    result_OR_model <- print(data.frame(OR_model, CI_model, p_value), quote = FALSE, noSpaces = TRUE, printToggle = FALSE )
    
    outcome_model[i, ] <- c(rownames(result_OR_model[nrow(result_OR_model), ]), result_OR_model[nrow(result_OR_model), ])
    
  } else  if(warning_skip_to_next == TRUE & error_skip_to_next == FALSE) { 
    print(paste("There is a warning in iteration ", i, sep=""))
    convergences[i] <- "convergence warning"
    
    model <- glmer(f, data = data_2019, family = "binomial", control=glmerControl(optimizer = 'bobyqa', optCtrl=list(maxfun=2e5)))
    OR_model <- exp(fixef(model))
    CI_model <- exp(confint(model, parm = "beta_", level = 0.99,method = "Wald"))
    sum_model <- summary(model)
    p_value <- sum_model$coefficients[, "Pr(>|z|)"]
    result_OR_model <- print(data.frame(OR_model, CI_model, p_value), quote = FALSE, noSpaces = TRUE, printToggle = FALSE )
    
    outcome_model[i, ] <- c(rownames(result_OR_model[nrow(result_OR_model), ]), result_OR_model[nrow(result_OR_model), ])
  } else if(error_skip_to_next == TRUE) {
    print(paste("There is an error in iteration ", i, sep=""))
    convergences[i] <- "error"
    outcome_model[i, 1] <- combinations[i]
  }
}
  
outcome_model$convergence <- convergences

folder <- (paste(main_folder, "resultaten/final", def_recent, sep = "/"))     
save(outcome_model, file = paste(folder, "/oddsratios ", input_events, " events in ", output_patients,  " patients.Rdata", sep = ""))


# # IC data only available until 2017____________________________________________________________________________________________________________ 
# data_2017 <- subset(data_2019, data_2019$incdat < "2018-01-01")  
# 
# events <- c( "opname_IC_lang","opname_IC_each_day")
# 
# if (def_recent == "timeline") {
#   combinations <- apply(expand.grid(events, recent_time), 1, paste, collapse="_")
# } else if (def_recent == "cases") {
#   combinations <- apply(expand.grid(events, recent_cases), 1, paste, collapse="_")
# }
# 
# outcome_2017_IC_model <- data.frame(matrix(nrow = length(combinations), ncol = 5))
# colnames(outcome_2017_IC_model) <- c("event", "OR", "lower_CI", "upper_CI", "p_value")
# 
# convergences <- vector(length = length(combinations))
# 
# for (i in 1:length(combinations)){
#   data_2017[paste("ratio_", combinations[i], "_high", sep = "")] <- factor(as.numeric(unlist(data_2017[paste("ratio_", combinations[i], "_high", sep = "")])), levels = c(0, 1)) 
#   
#   # when both EC and GC are analysed, than keep 'topography' as model input
#   if (output_patients == "EC and GC" | output_patients == "fragile EC and GC" ) {
#     f <- formula(paste("resectie ~ sex + age + histology + cT + cN + topography + comorbidities + perform_stat + ", 
#                        (paste("ratio_", combinations[i], "_high", sep = "")), 
#                        "+ (1 | regio)", sep = ""))
#   } else {
#     # when only EC or GC is analysed, remove 'topography' as model input
#     f <- formula(paste("resectie ~ sex + age + histology + cT + cN + comorbidities + perform_stat + ",
#                        (paste("ratio_", combinations[i], "_high", sep = "")), 
#                        "+ (1 | regio)", sep = ""))
#   }
#   
#   skip_to_next <- FALSE
#   convergences[i] <- "no issues"
#   
#   tryCatch({model <- glmer(f, data = data_2017, family = "binomial", control=glmerControl(optimizer = 'bobyqa', optCtrl=list(maxfun=2e5)))}, 
#            warning = function(w) { skip_to_next <<- TRUE})
#   
#   if(skip_to_next == FALSE) {
#     model <- glmer(f, data = data_2017, family = "binomial", control=glmerControl(optimizer = 'bobyqa', optCtrl=list(maxfun=2e5)))
#     OR_model <- exp(fixef(model))
#     CI_model <- exp(confint(model, parm = "beta_", level = 0.99,method = "Wald"))
#     sum_model <- summary(model)
#     p_value <- sum_model$coefficients[, "Pr(>|z|)"]
#     result_OR_model <- print(data.frame(OR_model, CI_model, p_value), quote = FALSE, noSpaces = TRUE, printToggle = FALSE )
#     
#     outcome_2017_IC_model[i, ] <- c(rownames(result_OR_model[nrow(result_OR_model), ]), result_OR_model[nrow(result_OR_model), ])
#     
#   } else  if(skip_to_next == TRUE) { 
#     print(paste("There is a warning in iteration ", i, sep=""))
#     convergences[i] <- "convergence warning"
#     
#     model <- glmer(f, data = data_2017, family = "binomial", control=glmerControl(optimizer = 'bobyqa', optCtrl=list(maxfun=2e5)))
#     OR_model <- exp(fixef(model))
#     CI_model <- exp(confint(model, parm = "beta_", level = 0.99,method = "Wald"))
#     sum_model <- summary(model)
#     p_value <- sum_model$coefficients[, "Pr(>|z|)"]
#     result_OR_model <- print(data.frame(OR_model, CI_model, p_value), quote = FALSE, noSpaces = TRUE, printToggle = FALSE )
#     
#     outcome_2017_IC_model[i, ] <- c(rownames(result_OR_model[nrow(result_OR_model), ]), result_OR_model[nrow(result_OR_model), ])
#   } 
# }
# 
# outcome_2017_IC_model$convergence <- convergences
# 
# 
# #save outcome odds ratios
# oddsratios <- rbind(outcome_model, outcome_2017_IC_model)
# folder <- (paste(main_folder, "resultaten/final", def_recent, sep = "/"))     
# save(oddsratios, file = paste(folder, "/oddsratios ", input_events, " events in ", output_patients,  " patients.Rdata", sep = ""))


#clean to save as excel
results          <- outcome_model
results$OR       <- round(results$OR, digits = 2)
results$lower_CI <- round(results$lower_CI, digits = 2) 
results$upper_CI <- round(results$upper_CI, digits = 2)
results$p_value  <- ifelse(results$p_value < 0.001, "<0.001", round(results$p_value, digits = 3))

#save odds ratios to Excel
write.xlsx(results, file = paste(folder, "/oddsratios ", input_events, " events in ", output_patients,  " patients.xlsx", sep = ""),
           rowNames = TRUE, colNames = TRUE, overwrite = TRUE)  


#Final messages _______________________________________________________________________________________________________________________________
if (input_events != "incorrect" & (def_recent %in% c("timeline", "cases"))) {
  input_sentence <- (paste("Your input for the analysis is based on the number of events in ", input_events, 
                                " patients using the ", def_recent, " definition of recent.", sep = ""))
} else {
  input_sentence <- ("Check whether the selected input variables are correct")
}

if (output_patients == "GC") {
  output_sentence <- (paste("Only gastric patients were considered in this analysis (n = ", nrow(data_incl3), ").", sep = ""))
} else if (output_patients == "EC") {
  output_sentence <- (paste("Only esophageal patients were considered in this analysis (n = ", nrow(data_incl3), ").", sep = ""))
} else if (output_patients == "EC and GC") {
  output_sentence <- (paste("Both esophageal and gastric patients were considered in this analysis (n = ", nrow(data_incl3), ").", sep = ""))
} else if (output_patients == "fragile GC") {
  output_sentence <- (paste("Only fragile gastric patients were considered in this analysis (n = ", nrow(data_incl3), ").", sep = ""))
} else if (output_patients == "fragile EC") {
  output_sentence <- (paste("Only fragile esophageal patients were considered in this analysis (n = ", nrow(data_incl3), ").", sep = ""))
} else if (output_patients == "fragile EC and GC") {
  output_sentence <- (paste("Only fragile esophageal and gastric patients were considered in this analysis (n = ", nrow(data_incl3), ").", sep = ""))
} else {
  output_sentence <- ("Check whether the selected output variables are correct")
}

if (length(low_volume_regio) == 0) {
  volume_sentence <- "No regions were excluded based on resection volume."
} else {
  volume_sentence <- paste(length(low_volume_regio), " region(s) were excluded due to low resection volume.")
}

if (sum(outcome_model$convergence == "no issues") == 0) {
  convergence_sentence <- "There were no models with convergence issues"
} else {
  convergence_sentence <- paste("There was/were ", sum(outcome_model$convergence == "convergence warning"), " model(s) with a convergence warning.", sep = "")
}


print(paste(input_sentence, output_sentence, volume_sentence, convergence_sentence, sep = " "))






