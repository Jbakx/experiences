# This script is created to make a dataframe which counts the recent events per region for the 
# previous 5, 10, 20, 30 and 40 cases for each patient. The number of recent events are calculated
# based on look-up tables (see matrix_5cases, matrix_10cases, matrix_20cases, matrix_30cases and
# matrix_40cases)

# created: 17-08-2021
# updated: 08-09-2021
# owner: Jeanne Bakx

##################################################################################################################################################################

rm(list = ls())   #clear work environment

library(haven)
library(ggplot2)

#load clean dataset (the result of running 'data cleaning and table one')

load("C:/Users/jba2102.54374/Documents/Onderzoek/Onderzoek effecten eerdere ervaringen/data/GC and EC/total_cleaned_dataset.Rdata") 
data_list <- split(dataset_clean, f = dataset_clean$regio)  
dates <- seq(as.Date("2015-01-01"), as.Date("2020-06-01"), by=1)    #use all dates to create a loop over time


##################################################################################################################################################################
# create complication matrix for the past 10, 20 and 30 cases 
##################################################################################################################################################################

matrix_5cases  <- vector(mode = "list", length = length(data_list))
matrix_10cases <- vector(mode = "list", length = length(data_list))
matrix_20cases <- vector(mode = "list", length = length(data_list))
matrix_30cases <- vector(mode = "list", length = length(data_list))
matrix_40cases <- vector(mode = "list", length = length(data_list))


for (j in 1:length(data_list)){
  mort30_5             <- vector(mode = "numeric", length = length(dates))
  card_compl_5         <- vector(mode = "numeric", length = length(dates))
  pulm_compl_5         <- vector(mode = "numeric", length = length(dates))
  naadlekkage_5        <- vector(mode = "numeric", length = length(dates))
  opname_21_lang_5     <- vector(mode = "numeric", length = length(dates))
  complicaties_5       <- vector(mode = "numeric", length = length(dates))
  
  mort30_10             <- vector(mode = "numeric", length = length(dates))
  card_compl_10         <- vector(mode = "numeric", length = length(dates))
  pulm_compl_10         <- vector(mode = "numeric", length = length(dates))
  naadlekkage_10        <- vector(mode = "numeric", length = length(dates))
  opname_21_lang_10     <- vector(mode = "numeric", length = length(dates))
  complicaties_10       <- vector(mode = "numeric", length = length(dates))
  
  mort30_20             <- vector(mode = "numeric", length = length(dates))
  card_compl_20         <- vector(mode = "numeric", length = length(dates))
  pulm_compl_20         <- vector(mode = "numeric", length = length(dates))
  naadlekkage_20        <- vector(mode = "numeric", length = length(dates))
  opname_21_lang_20     <- vector(mode = "numeric", length = length(dates))
  complicaties_20       <- vector(mode = "numeric", length = length(dates))
  
  mort30_30             <- vector(mode = "numeric", length = length(dates))
  card_compl_30         <- vector(mode = "numeric", length = length(dates))
  pulm_compl_30         <- vector(mode = "numeric", length = length(dates))
  naadlekkage_30        <- vector(mode = "numeric", length = length(dates))
  opname_21_lang_30     <- vector(mode = "numeric", length = length(dates))
  complicaties_30       <- vector(mode = "numeric", length = length(dates))
  
  mort30_40             <- vector(mode = "numeric", length = length(dates))
  card_compl_40         <- vector(mode = "numeric", length = length(dates))
  pulm_compl_40         <- vector(mode = "numeric", length = length(dates))
  naadlekkage_40        <- vector(mode = "numeric", length = length(dates))
  opname_21_lang_40     <- vector(mode = "numeric", length = length(dates))
  complicaties_40       <- vector(mode = "numeric", length = length(dates))
  
  print(j)

  for (d in 1:length(dates)) {
    #find all patients with a resection at before 'dates'  
    temp_cases <- subset(data_list[[j]], dates[d] > data_list[[j]]$resec_dat)           
    temp_sorted <- temp_cases[order(temp_cases$resec_dat, decreasing = TRUE, na.last = TRUE),]
    temp_5  <- temp_sorted[1:5,]        #5 most recent resected patients
    temp_10 <- temp_sorted[1:10,]       #10 most recent resected patients
    temp_20 <- temp_sorted[1:20,]       #20 most recent resected patients
    temp_30 <- temp_sorted[1:30,]       #30 most recent resected patients
    temp_40 <- temp_sorted[1:40,]       #40 most recent resected patients
    
    # The number of previous patients with 30 day mortality in the past 5, 10, 20, 30 and 40 cases
    mort30_5[d]  <- nrow(subset(temp_5,  temp_5$mort30  == 1 & temp_5$ovldat  < dates[d]))
    mort30_10[d] <- nrow(subset(temp_10, temp_10$mort30 == 1 & temp_10$ovldat < dates[d]))  
    mort30_20[d] <- nrow(subset(temp_20, temp_20$mort30 == 1 & temp_20$ovldat < dates[d]))  
    mort30_30[d] <- nrow(subset(temp_30, temp_30$mort30 == 1 & temp_30$ovldat < dates[d]))  
    mort30_40[d] <- nrow(subset(temp_40, temp_40$mort30 == 1 & temp_40$ovldat < dates[d]))  
    
    # The number of previous patients with anastomotic leakage in the past 5, 10, 20, 30 and 40 cases
    naadlekkage_5[d]  <- nrow(subset(temp_5,  temp_5$naadlekkage_datum  < dates[d]))
    naadlekkage_10[d] <- nrow(subset(temp_10, temp_10$naadlekkage_datum < dates[d]))
    naadlekkage_20[d] <- nrow(subset(temp_20, temp_20$naadlekkage_datum < dates[d]))
    naadlekkage_30[d] <- nrow(subset(temp_30, temp_30$naadlekkage_datum < dates[d]))
    naadlekkage_40[d] <- nrow(subset(temp_40, temp_40$naadlekkage_datum < dates[d]))
    
    # The number of previous patients with cardiac complications in the past 5, 10, 20, 30 and 40 cases
    card_compl_5[d]  <- nrow(subset(temp_5,  temp_5$cardiaal_datum  < dates[d]))
    card_compl_10[d] <- nrow(subset(temp_10, temp_10$cardiaal_datum < dates[d]))
    card_compl_20[d] <- nrow(subset(temp_20, temp_20$cardiaal_datum < dates[d]))
    card_compl_30[d] <- nrow(subset(temp_30, temp_30$cardiaal_datum < dates[d]))
    card_compl_40[d] <- nrow(subset(temp_40, temp_40$cardiaal_datum < dates[d]))
    
    # The number of previous patients with pulmonal complications in the past 5, 10, 20, 30 and 40 cases
    pulm_compl_5[d]  <- nrow(subset(temp_5,  temp_5$pulmonaal_datum  < dates[d]))
    pulm_compl_10[d] <- nrow(subset(temp_10, temp_10$pulmonaal_datum < dates[d]))
    pulm_compl_20[d] <- nrow(subset(temp_20, temp_20$pulmonaal_datum < dates[d]))
    pulm_compl_30[d] <- nrow(subset(temp_30, temp_30$pulmonaal_datum < dates[d]))
    pulm_compl_40[d] <- nrow(subset(temp_40, temp_40$pulmonaal_datum < dates[d]))
    
    # The number of previous patients exceeding 21 days admittance in the past 5, 10, 20, 30 and 40 cases
    opname_21_lang_5[d]  <- nrow(subset(temp_5,  temp_5$opname_21  == 1 & temp_5$opname_21_datum < dates[d])) 
    opname_21_lang_10[d] <- nrow(subset(temp_10, temp_10$opname_21 == 1 & temp_10$opname_21_datum < dates[d])) 
    opname_21_lang_20[d] <- nrow(subset(temp_20, temp_20$opname_21 == 1 & temp_20$opname_21_datum < dates[d])) 
    opname_21_lang_30[d] <- nrow(subset(temp_30, temp_30$opname_21 == 1 & temp_30$opname_21_datum < dates[d]))  
    opname_21_lang_40[d] <- nrow(subset(temp_40, temp_40$opname_21 == 1 & temp_40$opname_21_datum < dates[d]))
    
    # The number of previous patients with any complication (with ICU admittance) in the past 5, 10, 20, 30 and 40 cases
    complicaties_5[d]  <- nrow(subset(temp_5,  temp_5$som_compl  >= 1 & temp_5$datum_eerste_compl  < dates[d]))
    complicaties_10[d] <- nrow(subset(temp_10, temp_10$som_compl >= 1 & temp_10$datum_eerste_compl < dates[d]))
    complicaties_20[d] <- nrow(subset(temp_20, temp_20$som_compl >= 1 & temp_20$datum_eerste_compl < dates[d]))
    complicaties_30[d] <- nrow(subset(temp_30, temp_30$som_compl >= 1 & temp_30$datum_eerste_compl < dates[d]))
    complicaties_40[d] <- nrow(subset(temp_40, temp_40$som_compl >= 1 & temp_40$datum_eerste_compl < dates[d]))
  }
  
  matrix_5cases[[j]] <- data.frame(MDO_date = dates, 
                                    mort30_5cases = mort30_5,
                                    card_compl_5cases = card_compl_5,
                                    pulm_compl_5cases = pulm_compl_5,
                                    naadlekkage_5cases = naadlekkage_5,
                                    any_compl_5cases = complicaties_5,
                                    opname_21days_5cases = opname_21_lang_5
                                  )
    
  matrix_10cases[[j]] <- data.frame(MDO_date = dates, 
                                   mort30_10cases = mort30_10,
                                   card_compl_10cases = card_compl_10,
                                   pulm_compl_10cases = pulm_compl_10,
                                   naadlekkage_10cases = naadlekkage_10,
                                   any_compl_10cases = complicaties_10,
                                   opname_21days_10cases = opname_21_lang_10
                                  )
  
  matrix_20cases[[j]] <- data.frame(MDO_date = dates, 
                                   mort30_20cases = mort30_20,
                                   card_compl_20cases = card_compl_20,
                                   pulm_compl_20cases = pulm_compl_20,
                                   naadlekkage_20cases = naadlekkage_20,
                                   any_compl_20cases = complicaties_20,
                                   opname_21days_20cases = opname_21_lang_20
                                  )
  
  matrix_30cases[[j]] <- data.frame(MDO_date = dates, 
                                   mort30_30cases = mort30_30,
                                   card_compl_30cases = card_compl_30,
                                   pulm_compl_30cases = pulm_compl_30,
                                   naadlekkage_30cases = naadlekkage_30,
                                   any_compl_30cases = complicaties_30,
                                   opname_21days_30cases = opname_21_lang_30
                                 )
  
  matrix_40cases[[j]] <- data.frame(MDO_date = dates, 
                                   mort30_40cases = mort30_40,
                                   card_compl_40cases = card_compl_40,
                                   pulm_compl_40cases = pulm_compl_40,
                                   naadlekkage_40cases = naadlekkage_40,
                                   any_compl_40cases = complicaties_40,
                                   opname_21days_40cases = opname_21_lang_40
                                 )
}


save(matrix_5cases,  file = "C:/Users/jba2102.54374/Documents/Onderzoek/Onderzoek effecten eerdere ervaringen/data/GC and EC/cases/03-06-2022/matrix_5cases.Rdata")
save(matrix_10cases, file = "C:/Users/jba2102.54374/Documents/Onderzoek/Onderzoek effecten eerdere ervaringen/data/GC and EC/cases/03-06-2022/matrix_10cases.Rdata")
save(matrix_20cases, file = "C:/Users/jba2102.54374/Documents/Onderzoek/Onderzoek effecten eerdere ervaringen/data/GC and EC/cases/03-06-2022/matrix_20cases.Rdata")
save(matrix_30cases, file = "C:/Users/jba2102.54374/Documents/Onderzoek/Onderzoek effecten eerdere ervaringen/data/GC and EC/cases/03-06-2022/matrix_30cases.Rdata")
save(matrix_40cases, file = "C:/Users/jba2102.54374/Documents/Onderzoek/Onderzoek effecten eerdere ervaringen/data/GC and EC/cases/03-06-2022/matrix_40cases.Rdata")




##################################################################################################################################################################
# step 2: link the information in the matrices to the MDO date of each patient 
##################################################################################################################################################################


#add the information of the 5 most recent cases before of each MDO date for each region
for (o in 1:length(data_list)) {
  data_list[[o]]$mort30_5cases              <- vector(mode = "numeric", length = nrow(data_list[[o]]))
  data_list[[o]]$card_compl_5cases          <- vector(mode = "numeric", length = nrow(data_list[[o]]))
  data_list[[o]]$pulm_compl_5cases          <- vector(mode = "numeric", length = nrow(data_list[[o]]))
  data_list[[o]]$naadlekkage_5cases         <- vector(mode = "numeric", length = nrow(data_list[[o]]))
  data_list[[o]]$any_compl_5cases           <- vector(mode = "numeric", length = nrow(data_list[[o]]))
  data_list[[o]]$opname_21days_5cases       <- vector(mode = "numeric", length = nrow(data_list[[o]]))

    print(o)
  
  for (p in 1:nrow(matrix_5cases[[o]])) {
    
    for (q in 1:nrow(data_list[[o]])) {
      
      if(data_list[[o]]$mdo_datum_def[q] == matrix_5cases[[o]]$MDO_date[p]) {  
        data_list[[o]]$mort30_5cases[q]              <- matrix_5cases[[o]]$mort30_5cases[p]
        data_list[[o]]$card_compl_5cases[q]          <- matrix_5cases[[o]]$card_compl_5cases[p]
        data_list[[o]]$pulm_compl_5cases[q]          <- matrix_5cases[[o]]$pulm_compl_5cases[p]
        data_list[[o]]$naadlekkage_5cases[q]         <- matrix_5cases[[o]]$naadlekkage_5cases[p]
        data_list[[o]]$any_compl_5cases[q]           <- matrix_5cases[[o]]$any_compl_5cases[p]
        data_list[[o]]$opname_21days_5cases[q]       <- matrix_5cases[[o]]$opname_21days_5cases[p]
      }
    }
  }
}

save(data_list, file = "C:/Users/jba2102.54374/Documents/Onderzoek/Onderzoek effecten eerdere ervaringen/data/GC and EC/cases/03-06-2022/data_met_5_cases_uitkomsten.Rdata") 

##################################################################################################################################################################

#add the information of the 10 most recent cases before of each MDO date for each region
for (o in 1:length(data_list)) {
  data_list[[o]]$mort30_10cases              <- vector(mode = "numeric", length = nrow(data_list[[o]]))
  data_list[[o]]$card_compl_10cases          <- vector(mode = "numeric", length = nrow(data_list[[o]]))
  data_list[[o]]$pulm_compl_10cases          <- vector(mode = "numeric", length = nrow(data_list[[o]]))
  data_list[[o]]$naadlekkage_10cases         <- vector(mode = "numeric", length = nrow(data_list[[o]]))
  data_list[[o]]$any_compl_10cases           <- vector(mode = "numeric", length = nrow(data_list[[o]]))
  data_list[[o]]$opname_21days_10cases       <- vector(mode = "numeric", length = nrow(data_list[[o]]))
  
  print(o)
  
  for (p in 1:nrow(matrix_10cases[[o]])) {
    
    for (q in 1:nrow(data_list[[o]])) {
      
      if(data_list[[o]]$mdo_datum_def[q] == matrix_10cases[[o]]$MDO_date[p]) {  
        data_list[[o]]$mort30_10cases[q]              <- matrix_10cases[[o]]$mort30_10cases[p]
        data_list[[o]]$card_compl_10cases[q]          <- matrix_10cases[[o]]$card_compl_10cases[p]
        data_list[[o]]$pulm_compl_10cases[q]          <- matrix_10cases[[o]]$pulm_compl_10cases[p]
        data_list[[o]]$naadlekkage_10cases[q]         <- matrix_10cases[[o]]$naadlekkage_10cases[p]
        data_list[[o]]$any_compl_10cases[q]           <- matrix_10cases[[o]]$any_compl_10cases[p]
        data_list[[o]]$opname_21days_10cases[q]       <- matrix_10cases[[o]]$opname_21days_10cases[p]
      }
    }
  }
}

save(data_list, file = "C:/Users/jba2102.54374/Documents/Onderzoek/Onderzoek effecten eerdere ervaringen/data/GC and EC/cases/03-06-2022/data_met_10_cases_uitkomsten.Rdata") 

##################################################################################################################################################################

#add the information of the 20 most recent cases before of each MDO date for each region
for (o in 1:length(data_list)) {
  data_list[[o]]$mort30_20cases              <- vector(mode = "numeric", length = nrow(data_list[[o]]))
  data_list[[o]]$card_compl_20cases          <- vector(mode = "numeric", length = nrow(data_list[[o]]))
  data_list[[o]]$pulm_compl_20cases          <- vector(mode = "numeric", length = nrow(data_list[[o]]))
  data_list[[o]]$naadlekkage_20cases         <- vector(mode = "numeric", length = nrow(data_list[[o]]))
  data_list[[o]]$any_compl_20cases           <- vector(mode = "numeric", length = nrow(data_list[[o]]))
  data_list[[o]]$opname_21days_20cases       <- vector(mode = "numeric", length = nrow(data_list[[o]]))
  
  print(o)
  
  for (p in 1:nrow(matrix_20cases[[o]])) {
    
    for (q in 1:nrow(data_list[[o]])) {
      
      if(data_list[[o]]$mdo_datum_def[q] == matrix_20cases[[o]]$MDO_date[p]) {  
        data_list[[o]]$mort30_20cases[q]              <- matrix_20cases[[o]]$mort30_20cases[p]
        data_list[[o]]$card_compl_20cases[q]          <- matrix_20cases[[o]]$card_compl_20cases[p]
        data_list[[o]]$pulm_compl_20cases[q]          <- matrix_20cases[[o]]$pulm_compl_20cases[p]
        data_list[[o]]$naadlekkage_20cases[q]         <- matrix_20cases[[o]]$naadlekkage_20cases[p]
        data_list[[o]]$any_compl_20cases[q]           <- matrix_20cases[[o]]$any_compl_20cases[p]
        data_list[[o]]$opname_21days_20cases[q]       <- matrix_20cases[[o]]$opname_21days_20cases[p]
      }
    }
  }
}

save(data_list, file = "C:/Users/jba2102.54374/Documents/Onderzoek/Onderzoek effecten eerdere ervaringen/data/GC and EC/cases/03-06-2022/data_met_20_cases_uitkomsten.Rdata") 

##################################################################################################################################################################

#add the information of the 30 most recent cases before of each MDO date for each region
for (o in 1:length(data_list)) {
  data_list[[o]]$mort30_30cases              <- vector(mode = "numeric", length = nrow(data_list[[o]]))
  data_list[[o]]$card_compl_30cases          <- vector(mode = "numeric", length = nrow(data_list[[o]]))
  data_list[[o]]$pulm_compl_30cases          <- vector(mode = "numeric", length = nrow(data_list[[o]]))
  data_list[[o]]$naadlekkage_30cases         <- vector(mode = "numeric", length = nrow(data_list[[o]]))
  data_list[[o]]$any_compl_30cases           <- vector(mode = "numeric", length = nrow(data_list[[o]]))
  data_list[[o]]$opname_21days_30cases       <- vector(mode = "numeric", length = nrow(data_list[[o]]))
  
  print(o)
  
  for (p in 1:nrow(matrix_30cases[[o]])) {
    
    for (q in 1:nrow(data_list[[o]])) {
      
      if(data_list[[o]]$mdo_datum_def[q] == matrix_30cases[[o]]$MDO_date[p]) {  
        data_list[[o]]$mort30_30cases[q]              <- matrix_30cases[[o]]$mort30_30cases[p]
        data_list[[o]]$card_compl_30cases[q]          <- matrix_30cases[[o]]$card_compl_30cases[p]
        data_list[[o]]$pulm_compl_30cases[q]          <- matrix_30cases[[o]]$pulm_compl_30cases[p]
        data_list[[o]]$naadlekkage_30cases[q]         <- matrix_30cases[[o]]$naadlekkage_30cases[p]
        data_list[[o]]$any_compl_30cases[q]           <- matrix_30cases[[o]]$any_compl_30cases[p]
        data_list[[o]]$opname_21days_30cases[q]       <- matrix_30cases[[o]]$opname_21days_30cases[p]
      }
    }
  }
}

save(data_list, file = "C:/Users/jba2102.54374/Documents/Onderzoek/Onderzoek effecten eerdere ervaringen/data/GC and EC/cases/03-06-2022/data_met_30_cases_uitkomsten.Rdata") 

##################################################################################################################################################################

#add the information of the 40 most recent cases before of each MDO date for each region
for (o in 1:length(data_list)) {
  data_list[[o]]$mort30_40cases              <- vector(mode = "numeric", length = nrow(data_list[[o]]))
  data_list[[o]]$card_compl_40cases          <- vector(mode = "numeric", length = nrow(data_list[[o]]))
  data_list[[o]]$pulm_compl_40cases          <- vector(mode = "numeric", length = nrow(data_list[[o]]))
  data_list[[o]]$naadlekkage_40cases         <- vector(mode = "numeric", length = nrow(data_list[[o]]))
  data_list[[o]]$any_compl_40cases           <- vector(mode = "numeric", length = nrow(data_list[[o]]))
  data_list[[o]]$opname_21days_40cases       <- vector(mode = "numeric", length = nrow(data_list[[o]]))
   
  print(o)
  
  for (p in 1:nrow(matrix_40cases[[o]])) {
    
    for (q in 1:nrow(data_list[[o]])) {
      
      if(data_list[[o]]$mdo_datum_def[q] == matrix_40cases[[o]]$MDO_date[p]) {  
        data_list[[o]]$mort30_40cases[q]              <- matrix_40cases[[o]]$mort30_40cases[p]
        data_list[[o]]$card_compl_40cases[q]          <- matrix_40cases[[o]]$card_compl_40cases[p]
        data_list[[o]]$pulm_compl_40cases[q]          <- matrix_40cases[[o]]$pulm_compl_40cases[p]
        data_list[[o]]$naadlekkage_40cases[q]         <- matrix_40cases[[o]]$naadlekkage_40cases[p]
        data_list[[o]]$any_compl_40cases[q]           <- matrix_40cases[[o]]$any_compl_40cases[p]
        data_list[[o]]$opname_21days_40cases[q]       <- matrix_40cases[[o]]$opname_21days_40cases[p]
      }
    }
  }
}

save(data_list, file = "C:/Users/jba2102.54374/Documents/Onderzoek/Onderzoek effecten eerdere ervaringen/data/GC and EC/cases/03-06-2022/data_met_40_cases_uitkomsten.Rdata") 

##################################################################################################################################################################


#merge the information from all regions together in one dataset
data_cases <- do.call("rbind", data_list)
save(data_cases, file = "C:/Users/jba2102.54374/Documents/Onderzoek/Onderzoek effecten eerdere ervaringen/data/GC and EC/cases/03-06-2022/data_cases.Rdata") 
save(data_cases, file = "C:/Users/jba2102.54374/Documents/Onderzoek/Onderzoek effecten eerdere ervaringen/data/GC and EC/cases/data_cases.Rdata") 


