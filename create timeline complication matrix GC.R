# This script is created to make a dataframe which counts the recent events per region for the 
# previous 10, 30, 60 and 90 daysfor each patient. The number of recent events are calculated
# based on look-up tables (see matrix_5days, matrix_10days, matrix_30days, matrix_60days and
# matrix_90days)

# created: 17-08-2021
# updated: 01-09-2021
# owner: Jeanne Bakx

##################################################################################################################################################################

rm(list = ls())   #clear work environment

library(haven)
library(ggplot2)

#load clean dataset (the result of running 'data cleaning and table one')
load("C:/Users/jba2102.54374/Documents/Onderzoek/Onderzoek effecten eerdere ervaringen/data/GC only/GC_cleaned_dataset.Rdata") 
data_list <- split(GC_cleaned_dataset, f = GC_cleaned_dataset$regio)  
dates <- seq(as.Date("2015-01-01"), as.Date("2020-06-01"), by=1)    #use all dates to create a loop over time


##################################################################################################################################################################
# create complication matrix for the past 10, 30, 60 and 90 days
##################################################################################################################################################################

matrix_5days  <- vector(mode = "list", length = length(data_list))
matrix_10days <- vector(mode = "list", length = length(data_list))
matrix_30days <- vector(mode = "list", length = length(data_list))
matrix_60days <- vector(mode = "list", length = length(data_list))
matrix_90days <- vector(mode = "list", length = length(data_list))


for (j in 1:length(data_list)){
  mort30_5             <- vector(mode = "numeric", length = length(dates))
  card_compl_5         <- vector(mode = "numeric", length = length(dates))
  pulm_compl_5         <- vector(mode = "numeric", length = length(dates))
  naadlekkage_5        <- vector(mode = "numeric", length = length(dates))
  opname_21_lang_5     <- vector(mode = "numeric", length = length(dates))
  complicaties_5       <- vector(mode = "numeric", length = length(dates))
  resectie_5           <- vector(mode = "numeric", length = length(dates))
  at_risk_5            <- vector(mode = "numeric", length = length(dates))
  at_risk_5_temp       <- vector(mode = "numeric", length = 5)
  
  mort30_10             <- vector(mode = "numeric", length = length(dates))
  card_compl_10         <- vector(mode = "numeric", length = length(dates))
  pulm_compl_10         <- vector(mode = "numeric", length = length(dates))
  naadlekkage_10        <- vector(mode = "numeric", length = length(dates))
  opname_21_lang_10     <- vector(mode = "numeric", length = length(dates))
  complicaties_10       <- vector(mode = "numeric", length = length(dates))
  resectie_10           <- vector(mode = "numeric", length = length(dates))
  at_risk_10            <- vector(mode = "numeric", length = length(dates))
  at_risk_10_temp       <- vector(mode = "numeric", length = 10)
  
  mort30_30             <- vector(mode = "numeric", length = length(dates))
  card_compl_30         <- vector(mode = "numeric", length = length(dates))
  pulm_compl_30         <- vector(mode = "numeric", length = length(dates))
  naadlekkage_30        <- vector(mode = "numeric", length = length(dates))
  opname_21_lang_30     <- vector(mode = "numeric", length = length(dates))
  complicaties_30       <- vector(mode = "numeric", length = length(dates))
  resectie_30           <- vector(mode = "numeric", length = length(dates))
  at_risk_30            <- vector(mode = "numeric", length = length(dates))
  at_risk_30_temp       <- vector(mode = "numeric", length = 30)
  
  mort30_60             <- vector(mode = "numeric", length = length(dates))
  card_compl_60         <- vector(mode = "numeric", length = length(dates))
  pulm_compl_60         <- vector(mode = "numeric", length = length(dates))
  naadlekkage_60        <- vector(mode = "numeric", length = length(dates))
  opname_21_lang_60     <- vector(mode = "numeric", length = length(dates))
  complicaties_60       <- vector(mode = "numeric", length = length(dates))
  resectie_60           <- vector(mode = "numeric", length = length(dates))
  at_risk_60            <- vector(mode = "numeric", length = length(dates))
  at_risk_60_temp       <- vector(mode = "numeric", length = 60)
  
  mort30_90             <- vector(mode = "numeric", length = length(dates))
  card_compl_90         <- vector(mode = "numeric", length = length(dates))
  pulm_compl_90         <- vector(mode = "numeric", length = length(dates))
  naadlekkage_90        <- vector(mode = "numeric", length = length(dates))
  opname_21_lang_90     <- vector(mode = "numeric", length = length(dates))
  complicaties_90       <- vector(mode = "numeric", length = length(dates))
  resectie_90           <- vector(mode = "numeric", length = length(dates))
  at_risk_90            <- vector(mode = "numeric", length = length(dates))
  at_risk_90_temp       <- vector(mode = "numeric", length = 90)
  
  print(j)
  
  for (d in 1:length(dates)) {
    #find all patients with a resection at before 'dates'  
    temp_cases <- subset(data_list[[j]], dates[d] > data_list[[j]]$resec_dat)           
    
    # The number of previous patients with 30 day mortality in the past 5, 10, 30, 60 and 90 days
    mort30_5[d]   <- nrow(subset(temp_cases,  temp_cases$mort30  == 1 & temp_cases$ovldat < dates[d] & temp_cases$ovldat >= dates[d]-5))
    mort30_10[d]  <- nrow(subset(temp_cases,  temp_cases$mort30  == 1 & temp_cases$ovldat < dates[d] & temp_cases$ovldat >= dates[d]-10))
    mort30_30[d]  <- nrow(subset(temp_cases,  temp_cases$mort30  == 1 & temp_cases$ovldat < dates[d] & temp_cases$ovldat >= dates[d]-30))
    mort30_60[d]  <- nrow(subset(temp_cases,  temp_cases$mort30  == 1 & temp_cases$ovldat < dates[d] & temp_cases$ovldat >= dates[d]-60))
    mort30_90[d]  <- nrow(subset(temp_cases,  temp_cases$mort30  == 1 & temp_cases$ovldat < dates[d] & temp_cases$ovldat >= dates[d]-90))
    
    # The number of previous patients with anastomotic leakage in the past 5, 10, 30, 60 and 90 days
    naadlekkage_5[d]    <- nrow(subset(temp_cases,  temp_cases$naadlekkage  == 1 & temp_cases$naadlekkage_datum < dates[d] & temp_cases$naadlekkage_datum >= dates[d]-5))
    naadlekkage_10[d]   <- nrow(subset(temp_cases,  temp_cases$naadlekkage  == 1 & temp_cases$naadlekkage_datum < dates[d] & temp_cases$naadlekkage_datum >= dates[d]-10))
    naadlekkage_30[d]   <- nrow(subset(temp_cases,  temp_cases$naadlekkage  == 1 & temp_cases$naadlekkage_datum < dates[d] & temp_cases$naadlekkage_datum >= dates[d]-30))
    naadlekkage_60[d]   <- nrow(subset(temp_cases,  temp_cases$naadlekkage  == 1 & temp_cases$naadlekkage_datum < dates[d] & temp_cases$naadlekkage_datum >= dates[d]-60))
    naadlekkage_90[d]   <- nrow(subset(temp_cases,  temp_cases$naadlekkage  == 1 & temp_cases$naadlekkage_datum < dates[d] & temp_cases$naadlekkage_datum >= dates[d]-90))
    
    # The number of previous patients with cardiac complications in the past 5, 10, 30, 60 and 90 days
    card_compl_5[d]    <- nrow(subset(temp_cases,  temp_cases$cardiaal  == 1 & temp_cases$cardiaal_datum < dates[d] & temp_cases$cardiaal_datum >= dates[d]-5))
    card_compl_10[d]   <- nrow(subset(temp_cases,  temp_cases$cardiaal  == 1 & temp_cases$cardiaal_datum < dates[d] & temp_cases$cardiaal_datum >= dates[d]-10))
    card_compl_30[d]   <- nrow(subset(temp_cases,  temp_cases$cardiaal  == 1 & temp_cases$cardiaal_datum < dates[d] & temp_cases$cardiaal_datum >= dates[d]-30))
    card_compl_60[d]   <- nrow(subset(temp_cases,  temp_cases$cardiaal  == 1 & temp_cases$cardiaal_datum < dates[d] & temp_cases$cardiaal_datum >= dates[d]-60))
    card_compl_90[d]   <- nrow(subset(temp_cases,  temp_cases$cardiaal  == 1 & temp_cases$cardiaal_datum < dates[d] & temp_cases$cardiaal_datum >= dates[d]-90))
    
    # The number of previous patients with pulmonal complications in the past 5, 10, 30, 60 and 90 days
    pulm_compl_5[d]    <- nrow(subset(temp_cases,  temp_cases$pulmonaal  == 1 & temp_cases$pulmonaal_datum < dates[d] & temp_cases$pulmonaal_datum >= dates[d]-5))
    pulm_compl_10[d]   <- nrow(subset(temp_cases,  temp_cases$pulmonaal  == 1 & temp_cases$pulmonaal_datum < dates[d] & temp_cases$pulmonaal_datum >= dates[d]-10))
    pulm_compl_30[d]   <- nrow(subset(temp_cases,  temp_cases$pulmonaal  == 1 & temp_cases$pulmonaal_datum < dates[d] & temp_cases$pulmonaal_datum >= dates[d]-30))
    pulm_compl_60[d]   <- nrow(subset(temp_cases,  temp_cases$pulmonaal  == 1 & temp_cases$pulmonaal_datum < dates[d] & temp_cases$pulmonaal_datum >= dates[d]-60))
    pulm_compl_90[d]   <- nrow(subset(temp_cases,  temp_cases$pulmonaal  == 1 & temp_cases$pulmonaal_datum < dates[d] & temp_cases$pulmonaal_datum >= dates[d]-90))
    
    # The number of previous patients exceeding 21 days admittance in the past 5, 10, 30, 60 and 90 days
    opname_21_lang_5[d]   <- nrow(subset(temp_cases,  temp_cases$opname_21  == 1 & temp_cases$opname_21_datum < dates[d] & temp_cases$opname_21_datum >= dates[d]-5)) 
    opname_21_lang_10[d]  <- nrow(subset(temp_cases,  temp_cases$opname_21  == 1 & temp_cases$opname_21_datum < dates[d] & temp_cases$opname_21_datum >= dates[d]-10)) 
    opname_21_lang_30[d]  <- nrow(subset(temp_cases,  temp_cases$opname_21  == 1 & temp_cases$opname_21_datum < dates[d] & temp_cases$opname_21_datum >= dates[d]-30)) 
    opname_21_lang_60[d]  <- nrow(subset(temp_cases,  temp_cases$opname_21  == 1 & temp_cases$opname_21_datum < dates[d] & temp_cases$opname_21_datum >= dates[d]-60)) 
    opname_21_lang_90[d]  <- nrow(subset(temp_cases,  temp_cases$opname_21  == 1 & temp_cases$opname_21_datum < dates[d] & temp_cases$opname_21_datum >= dates[d]-90)) 

    # The number of previous patients with any complication in the past 5, 10, 30, 60 and 90 days
    complicaties_5[d]    <- nrow(subset(temp_cases,  temp_cases$som_compl  >= 1 & temp_cases$datum_eerste_compl < dates[d] & temp_cases$datum_eerste_compl >= dates[d]-5))
    complicaties_10[d]   <- nrow(subset(temp_cases,  temp_cases$som_compl  >= 1 & temp_cases$datum_eerste_compl < dates[d] & temp_cases$datum_eerste_compl >= dates[d]-10))
    complicaties_30[d]   <- nrow(subset(temp_cases,  temp_cases$som_compl  >= 1 & temp_cases$datum_eerste_compl < dates[d] & temp_cases$datum_eerste_compl >= dates[d]-30))
    complicaties_60[d]   <- nrow(subset(temp_cases,  temp_cases$som_compl  >= 1 & temp_cases$datum_eerste_compl < dates[d] & temp_cases$datum_eerste_compl >= dates[d]-60))
    complicaties_90[d]   <- nrow(subset(temp_cases,  temp_cases$som_compl  >= 1 & temp_cases$datum_eerste_compl < dates[d] & temp_cases$datum_eerste_compl >= dates[d]-90))

    # The number of resections in the past 5, 10, 30, 60 and 90 days
    resectie_5[d]    <- nrow(subset(temp_cases,  temp_cases$resectie  == 1 & temp_cases$resec_dat < dates[d] & temp_cases$resec_dat >= dates[d]-5))
    resectie_10[d]   <- nrow(subset(temp_cases,  temp_cases$resectie  == 1 & temp_cases$resec_dat < dates[d] & temp_cases$resec_dat >= dates[d]-10))
    resectie_30[d]   <- nrow(subset(temp_cases,  temp_cases$resectie  == 1 & temp_cases$resec_dat < dates[d] & temp_cases$resec_dat >= dates[d]-30))
    resectie_60[d]   <- nrow(subset(temp_cases,  temp_cases$resectie  == 1 & temp_cases$resec_dat < dates[d] & temp_cases$resec_dat >= dates[d]-60))
    resectie_90[d]   <- nrow(subset(temp_cases,  temp_cases$resectie  == 1 & temp_cases$resec_dat < dates[d] & temp_cases$resec_dat >= dates[d]-90))
    
    # The number of patients at risk for the events in the past 5 days
    for (q in 1:5) {
      at_risk_5_temp[q] <- nrow(subset(temp_cases,  temp_cases$resectie  == 1 & temp_cases$resec_dat < dates[d]-q & temp_cases$resec_dat >= dates[d]-(30+q)))
    }
    at_risk_5[d]     <- (sum(at_risk_5_temp))/5
    
    # The number of patients at risk for the events in the past 10 days
    for (q in 1:10) {
      at_risk_10_temp[q] <- nrow(subset(temp_cases,  temp_cases$resectie  == 1 & temp_cases$resec_dat < dates[d]-q & temp_cases$resec_dat >= dates[d]-(30+q)))
    }
    at_risk_10[d]     <- (sum(at_risk_10_temp))/10
    
    # The number of patients at risk for the events in the past 30 days
    for (q in 1:30) {
      at_risk_30_temp[q] <- nrow(subset(temp_cases,  temp_cases$resectie  == 1 & temp_cases$resec_dat < dates[d]-q & temp_cases$resec_dat >= dates[d]-(30+q)))
    }
    at_risk_30[d]     <- (sum(at_risk_30_temp))/30
    
    # The number of patients at risk for the events in the past 60 days
    for (q in 1:60) {
      at_risk_60_temp[q] <- nrow(subset(temp_cases,  temp_cases$resectie  == 1 & temp_cases$resec_dat < dates[d]-q & temp_cases$resec_dat >= dates[d]-(30+q)))
    }
    at_risk_60[d]     <- (sum(at_risk_60_temp))/60
    
    # The number of patients at risk for the events in the past 90 days
    for (q in 1:90) {
      at_risk_90_temp[q] <- nrow(subset(temp_cases,  temp_cases$resectie  == 1 & temp_cases$resec_dat < dates[d]-q & temp_cases$resec_dat >= dates[d]-(30+q)))
    }
    at_risk_90[d]     <- (sum(at_risk_90_temp))/90
  }
  
  matrix_5days[[j]] <- data.frame(MDO_date = dates, 
                                  mort30_5days = mort30_5,
                                  card_compl_5days = card_compl_5,
                                  pulm_compl_5days = pulm_compl_5,
                                  naadlekkage_5days = naadlekkage_5,
                                  any_compl_5days = complicaties_5,
                                  opname_21days_5days = opname_21_lang_5,
                                  resectie_5days =  resectie_5, 
                                  at_risk_5days =  at_risk_5)
  
  matrix_10days[[j]] <- data.frame(MDO_date = dates, 
                                  mort30_10days = mort30_10,
                                  card_compl_10days = card_compl_10,
                                  pulm_compl_10days = pulm_compl_10,
                                  naadlekkage_10days = naadlekkage_10,
                                  any_compl_10days = complicaties_10,
                                  opname_21days_10days = opname_21_lang_10,
                                  resectie_10days =  resectie_10, 
                                  at_risk_10days =  at_risk_10)
  
  matrix_30days[[j]] <- data.frame(MDO_date = dates, 
                                  mort30_30days = mort30_30,
                                  card_compl_30days = card_compl_30,
                                  pulm_compl_30days = pulm_compl_30,
                                  naadlekkage_30days = naadlekkage_30,
                                  any_compl_30days = complicaties_30,
                                  opname_21days_30days = opname_21_lang_30,
                                  resectie_30days =  resectie_30, 
                                  at_risk_30days =  at_risk_30)
  
  matrix_60days[[j]] <- data.frame(MDO_date = dates, 
                                  mort30_60days = mort30_60,
                                  card_compl_60days = card_compl_60,
                                  pulm_compl_60days = pulm_compl_60,
                                  naadlekkage_60days = naadlekkage_60,
                                  any_compl_60days = complicaties_60,
                                  opname_21days_60days = opname_21_lang_60,
                                  resectie_60days =  resectie_60, 
                                  at_risk_60days =  at_risk_60)
  
  matrix_90days[[j]] <- data.frame(MDO_date = dates, 
                                  mort30_90days = mort30_90,
                                  card_compl_90days = card_compl_90,
                                  pulm_compl_90days = pulm_compl_90,
                                  naadlekkage_90days = naadlekkage_90,
                                  any_compl_90days = complicaties_90,
                                  opname_21days_90days = opname_21_lang_90,
                                  resectie_90days =  resectie_90, 
                                  at_risk_90days =  at_risk_90)
}


save(matrix_5days,  file = "C:/Users/jba2102.54374/Documents/Onderzoek/Onderzoek effecten eerdere ervaringen/data/GC only/timeline/03-06-2022/matrix_5days.Rdata")
save(matrix_10days, file = "C:/Users/jba2102.54374/Documents/Onderzoek/Onderzoek effecten eerdere ervaringen/data/GC only/timeline/03-06-2022/matrix_10days.Rdata")
save(matrix_30days, file = "C:/Users/jba2102.54374/Documents/Onderzoek/Onderzoek effecten eerdere ervaringen/data/GC only/timeline/03-06-2022/matrix_30days.Rdata")
save(matrix_60days, file = "C:/Users/jba2102.54374/Documents/Onderzoek/Onderzoek effecten eerdere ervaringen/data/GC only/timeline/03-06-2022/matrix_60days.Rdata")
save(matrix_90days, file = "C:/Users/jba2102.54374/Documents/Onderzoek/Onderzoek effecten eerdere ervaringen/data/GC only/timeline/03-06-2022/matrix_90days.Rdata")




##################################################################################################################################################################
# step 2: link the information in the matrices to the MDO date of each patient 
##################################################################################################################################################################


#add the information of the 5 most recent cases before of each MDO date for each region
for (o in 1:length(data_list)) {
  data_list[[o]]$mort30_5days              <- vector(mode = "numeric", length = nrow(data_list[[o]]))
  data_list[[o]]$card_compl_5days          <- vector(mode = "numeric", length = nrow(data_list[[o]]))
  data_list[[o]]$pulm_compl_5days          <- vector(mode = "numeric", length = nrow(data_list[[o]]))
  data_list[[o]]$naadlekkage_5days         <- vector(mode = "numeric", length = nrow(data_list[[o]]))
  data_list[[o]]$any_compl_5days           <- vector(mode = "numeric", length = nrow(data_list[[o]]))
  data_list[[o]]$opname_21days_5days       <- vector(mode = "numeric", length = nrow(data_list[[o]]))
  data_list[[o]]$resectie_5days            <- vector(mode = "numeric", length = nrow(data_list[[o]]))
  data_list[[o]]$at_risk_5days             <- vector(mode = "numeric", length = nrow(data_list[[o]]))
  
  print(o)
  
  for (p in 1:nrow(matrix_5days[[o]])) {
    
    for (q in 1:nrow(data_list[[o]])) {
      
      if(data_list[[o]]$mdo_datum_def[q] == matrix_5days[[o]]$MDO_date[p]) {  
        data_list[[o]]$mort30_5days[q]              <- matrix_5days[[o]]$mort30_5days[p]
        data_list[[o]]$card_compl_5days[q]          <- matrix_5days[[o]]$card_compl_5days[p]
        data_list[[o]]$pulm_compl_5days[q]          <- matrix_5days[[o]]$pulm_compl_5days[p]
        data_list[[o]]$naadlekkage_5days[q]         <- matrix_5days[[o]]$naadlekkage_5days[p]
        data_list[[o]]$any_compl_5days[q]           <- matrix_5days[[o]]$any_compl_5days[p]
        data_list[[o]]$opname_21days_5days[q]       <- matrix_5days[[o]]$opname_21days_5days[p]
        data_list[[o]]$resectie_5days[q]            <- matrix_5days[[o]]$resectie_5days[p]
        data_list[[o]]$at_risk_5days[q]             <- matrix_5days[[o]]$at_risk_5days[p]
      }
    }
  }
}

save(data_list, file = "C:/Users/jba2102.54374/Documents/Onderzoek/Onderzoek effecten eerdere ervaringen/data/GC only/timeline/03-06-2022/data_met_5_cases_uitkomsten.Rdata") 

##################################################################################################################################################################

#add the information of the 10 most recent cases before of each MDO date for each region
for (o in 1:length(data_list)) {
  data_list[[o]]$mort30_10days              <- vector(mode = "numeric", length = nrow(data_list[[o]]))
  data_list[[o]]$card_compl_10days          <- vector(mode = "numeric", length = nrow(data_list[[o]]))
  data_list[[o]]$pulm_compl_10days          <- vector(mode = "numeric", length = nrow(data_list[[o]]))
  data_list[[o]]$naadlekkage_10days         <- vector(mode = "numeric", length = nrow(data_list[[o]]))
  data_list[[o]]$any_compl_10days           <- vector(mode = "numeric", length = nrow(data_list[[o]]))
  data_list[[o]]$opname_21days_10days       <- vector(mode = "numeric", length = nrow(data_list[[o]]))
  data_list[[o]]$any_compl_10days           <- vector(mode = "numeric", length = nrow(data_list[[o]]))
  data_list[[o]]$resectie_10days            <- vector(mode = "numeric", length = nrow(data_list[[o]]))
  data_list[[o]]$at_risk_10days             <- vector(mode = "numeric", length = nrow(data_list[[o]]))
   
  print(o)
  
  for (p in 1:nrow(matrix_10days[[o]])) {
    
    for (q in 1:nrow(data_list[[o]])) {
      
      if(data_list[[o]]$mdo_datum_def[q] == matrix_10days[[o]]$MDO_date[p]) {  
        data_list[[o]]$mort30_10days[q]              <- matrix_10days[[o]]$mort30_10days[p]
        data_list[[o]]$card_compl_10days[q]          <- matrix_10days[[o]]$card_compl_10days[p]
        data_list[[o]]$pulm_compl_10days[q]          <- matrix_10days[[o]]$pulm_compl_10days[p]
        data_list[[o]]$naadlekkage_10days[q]         <- matrix_10days[[o]]$naadlekkage_10days[p]
        data_list[[o]]$any_compl_10days[q]           <- matrix_10days[[o]]$any_compl_10days[p]
        data_list[[o]]$opname_21days_10days[q]       <- matrix_10days[[o]]$opname_21days_10days[p]
        data_list[[o]]$resectie_10days[q]            <- matrix_10days[[o]]$resectie_10days[p]
        data_list[[o]]$at_risk_10days[q]             <- matrix_10days[[o]]$at_risk_10days[p]
      }
    }
  }
}

save(data_list, file = "C:/Users/jba2102.54374/Documents/Onderzoek/Onderzoek effecten eerdere ervaringen/data/GC only/timeline/03-06-2022/data_met_10_cases_uitkomsten.Rdata") 

##################################################################################################################################################################

#add the information of the 20 most recent cases before of each MDO date for each region
for (o in 1:length(data_list)) {
  data_list[[o]]$mort30_30days              <- vector(mode = "numeric", length = nrow(data_list[[o]]))
  data_list[[o]]$card_compl_30days          <- vector(mode = "numeric", length = nrow(data_list[[o]]))
  data_list[[o]]$pulm_compl_30days          <- vector(mode = "numeric", length = nrow(data_list[[o]]))
  data_list[[o]]$naadlekkage_30days         <- vector(mode = "numeric", length = nrow(data_list[[o]]))
  data_list[[o]]$any_compl_30days           <- vector(mode = "numeric", length = nrow(data_list[[o]]))
  data_list[[o]]$opname_21days_30days       <- vector(mode = "numeric", length = nrow(data_list[[o]]))
  data_list[[o]]$any_compl_30days           <- vector(mode = "numeric", length = nrow(data_list[[o]]))
  data_list[[o]]$resectie_30days            <- vector(mode = "numeric", length = nrow(data_list[[o]]))
  data_list[[o]]$at_risk_30days             <- vector(mode = "numeric", length = nrow(data_list[[o]]))
  
  print(o)
  
  for (p in 1:nrow(matrix_30days[[o]])) {
    
    for (q in 1:nrow(data_list[[o]])) {
      
      if(data_list[[o]]$mdo_datum_def[q] == matrix_30days[[o]]$MDO_date[p]) {  
        data_list[[o]]$mort30_30days[q]              <- matrix_30days[[o]]$mort30_30days[p]
        data_list[[o]]$card_compl_30days[q]          <- matrix_30days[[o]]$card_compl_30days[p]
        data_list[[o]]$pulm_compl_30days[q]          <- matrix_30days[[o]]$pulm_compl_30days[p]
        data_list[[o]]$naadlekkage_30days[q]         <- matrix_30days[[o]]$naadlekkage_30days[p]
        data_list[[o]]$any_compl_30days[q]           <- matrix_30days[[o]]$any_compl_30days[p]
        data_list[[o]]$opname_21days_30days[q]       <- matrix_30days[[o]]$opname_21days_30days[p]
        data_list[[o]]$resectie_30days[q]            <- matrix_30days[[o]]$resectie_30days[p]
        data_list[[o]]$at_risk_30days[q]             <- matrix_30days[[o]]$at_risk_30days[p]
      }
    }
  }
}

save(data_list, file = "C:/Users/jba2102.54374/Documents/Onderzoek/Onderzoek effecten eerdere ervaringen/data/GC only/timeline/03-06-2022/data_met_20_cases_uitkomsten.Rdata") 

##################################################################################################################################################################

#add the information of the 30 most recent cases before of each MDO date for each region
for (o in 1:length(data_list)) {
  data_list[[o]]$mort30_60days              <- vector(mode = "numeric", length = nrow(data_list[[o]]))
  data_list[[o]]$card_compl_60days          <- vector(mode = "numeric", length = nrow(data_list[[o]]))
  data_list[[o]]$pulm_compl_60days          <- vector(mode = "numeric", length = nrow(data_list[[o]]))
  data_list[[o]]$naadlekkage_60days         <- vector(mode = "numeric", length = nrow(data_list[[o]]))
  data_list[[o]]$any_compl_60days           <- vector(mode = "numeric", length = nrow(data_list[[o]]))
  data_list[[o]]$opname_21days_60days       <- vector(mode = "numeric", length = nrow(data_list[[o]]))
  data_list[[o]]$any_compl_60days           <- vector(mode = "numeric", length = nrow(data_list[[o]]))
  data_list[[o]]$resectie_60days            <- vector(mode = "numeric", length = nrow(data_list[[o]]))
  data_list[[o]]$at_risk_60days             <- vector(mode = "numeric", length = nrow(data_list[[o]]))
  
  print(o)
  
  for (p in 1:nrow(matrix_60days[[o]])) {
    
    for (q in 1:nrow(data_list[[o]])) {
      
      if(data_list[[o]]$mdo_datum_def[q] == matrix_60days[[o]]$MDO_date[p]) {  
        data_list[[o]]$mort30_60days[q]              <- matrix_60days[[o]]$mort30_60days[p]
        data_list[[o]]$card_compl_60days[q]          <- matrix_60days[[o]]$card_compl_60days[p]
        data_list[[o]]$pulm_compl_60days[q]          <- matrix_60days[[o]]$pulm_compl_60days[p]
        data_list[[o]]$naadlekkage_60days[q]         <- matrix_60days[[o]]$naadlekkage_60days[p]
        data_list[[o]]$any_compl_60days[q]           <- matrix_60days[[o]]$any_compl_60days[p]
        data_list[[o]]$opname_21days_60days[q]       <- matrix_60days[[o]]$opname_21days_60days[p]
        data_list[[o]]$resectie_60days[q]            <- matrix_60days[[o]]$resectie_60days[p]
        data_list[[o]]$at_risk_60days[q]             <- matrix_60days[[o]]$at_risk_60days[p]
      }
    }
  }
}

save(data_list, file = "C:/Users/jba2102.54374/Documents/Onderzoek/Onderzoek effecten eerdere ervaringen/data/GC only/timeline/03-06-2022/data_met_30_cases_uitkomsten.Rdata") 

##################################################################################################################################################################

#add the information of the 40 most recent cases before of each MDO date for each region
for (o in 1:length(data_list)) {
  data_list[[o]]$mort30_90days              <- vector(mode = "numeric", length = nrow(data_list[[o]]))
  data_list[[o]]$card_compl_90days          <- vector(mode = "numeric", length = nrow(data_list[[o]]))
  data_list[[o]]$pulm_compl_90days          <- vector(mode = "numeric", length = nrow(data_list[[o]]))
  data_list[[o]]$naadlekkage_90days         <- vector(mode = "numeric", length = nrow(data_list[[o]]))
  data_list[[o]]$any_compl_90days           <- vector(mode = "numeric", length = nrow(data_list[[o]]))
  data_list[[o]]$opname_21days_90days       <- vector(mode = "numeric", length = nrow(data_list[[o]]))
  data_list[[o]]$any_compl_90days           <- vector(mode = "numeric", length = nrow(data_list[[o]]))
  data_list[[o]]$resectie_90days            <- vector(mode = "numeric", length = nrow(data_list[[o]]))
  data_list[[o]]$at_risk_90days             <- vector(mode = "numeric", length = nrow(data_list[[o]]))
  
  print(o)
  
  for (p in 1:nrow(matrix_90days[[o]])) {
    
    for (q in 1:nrow(data_list[[o]])) {
      
      if(data_list[[o]]$mdo_datum_def[q] == matrix_90days[[o]]$MDO_date[p]) {  
        data_list[[o]]$mort30_90days[q]              <- matrix_90days[[o]]$mort30_90days[p]
        data_list[[o]]$card_compl_90days[q]          <- matrix_90days[[o]]$card_compl_90days[p]
        data_list[[o]]$pulm_compl_90days[q]          <- matrix_90days[[o]]$pulm_compl_90days[p]
        data_list[[o]]$naadlekkage_90days[q]         <- matrix_90days[[o]]$naadlekkage_90days[p]
        data_list[[o]]$any_compl_90days[q]           <- matrix_90days[[o]]$any_compl_90days[p]
        data_list[[o]]$opname_21days_90days[q]       <- matrix_90days[[o]]$opname_21days_90days[p]
        data_list[[o]]$resectie_90days[q]            <- matrix_90days[[o]]$resectie_90days[p]
        data_list[[o]]$at_risk_90days[q]             <- matrix_90days[[o]]$at_risk_90days[p]
      }
    }
  }
}


save(data_list, file = "C:/Users/jba2102.54374/Documents/Onderzoek/Onderzoek effecten eerdere ervaringen/data/GC only/timeline/03-06-2022/data_met_40_cases_uitkomsten.Rdata") 

##################################################################################################################################################################


#merge the information from all regions together in one dataset
data_timeline <- do.call("rbind", data_list)
save(data_timeline, file = "C:/Users/jba2102.54374/Documents/Onderzoek/Onderzoek effecten eerdere ervaringen/data/GC only/timeline/03-06-2022/data_timeline.Rdata") 
save(data_timeline, file = "C:/Users/jba2102.54374/Documents/Onderzoek/Onderzoek effecten eerdere ervaringen/data/GC only/timeline/data_timeline.Rdata") 


