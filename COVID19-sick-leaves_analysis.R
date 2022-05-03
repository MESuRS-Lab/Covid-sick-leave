####
#### COVID-19 Sick leaves 
#### 
#### MESuRS - Cnam
#### November 2021

#### SETUP ###################################

## Packages
library(magrittr)
library(tidyr)
library(dplyr)
library(ggplot2)
library(ggpubr)
library(wesanderson)  # nice colour palette
library(RColorBrewer) # expand palette
library(gridExtra)    # plot grid
library(gtable)
library(cowplot)
library(grid)
library(ggrepel)

# Ensure R is associated with correct directory
list.files()


#### DATA ###################################
## consider period up to June 1 2020
date_end_peak = as.Date('2020-06-01')

## First wave probabilities estimated from occupational health survey by Malakoff Humanis
p_leave_wave1 = 0.028 # probability of sick leave during first wave
p_COVID_given_leave_wave1 = 0.1227 # probability of having COVID-19 symptoms while on sick leave
duration_sickleave = 9.33
duration_symptoms = 8.53

## EHESP COVID-19 model data (epidemiological predictions)
# Period corresponds to the time windows of successive occupational health surveys led by Malakoff Humanis 
# Here, survey data corresponding to Period 1 will be considered
data_symptinc = read.csv2(file = "Data/data_symptomatic_incidence.csv") %>%
  mutate(Time = as.Date(Time))%>%
  mutate(Period = case_when(Time >= as.Date("2020-03-01") & Time < as.Date("2020-04-01") ~ 1, # March
                            Time >= as.Date("2020-04-01") & Time < as.Date("2020-05-01") ~ 2, # April
                            Time >= as.Date("2020-05-01") & Time < as.Date("2020-06-01") ~ 3, # May
                            Time >= as.Date("2020-06-01") & Time < as.Date("2020-07-01") ~ 4, # June
                            TRUE ~ 5))%>%
  mutate(Prevalence = Symptomatic * duration_symptoms)

## Total incidence in wave 1, all age groups
data_symptinc_total = data_symptinc%>%filter(Time < date_end_peak)
sum(data_symptinc_total$Symptomatic)

## Max incidence in wave 1, all age groups
data_symptinc_total_prev = data_symptinc_total%>%
  group_by(Time)%>%
  summarise(Prevalence = sum(Prevalence))
max(data_symptinc_total_prev$Prevalence)

## INSEE data for active working population in France (N_active)
vec_regions = c('Auvergne-Rhone-Alpes', 
                'Bourgogne-Franche-Comte', 
                'Bretagne',
                'Centre-Val de Loire', 
                'Corse', 
                'Grand-Est',
                'Hauts-de-France', 
                'Ile-de-France', 
                'Normandie',
                'Nouvelle-Aquitaine', 
                'Occitanie', 
                'Pays de la Loire',
                'PACA')

# load data and aggregate age groups (corresponding to age groups from COVID-19 model)
data_insee_active = readRDS(file = "Data/data_active_occ_by_age_region.RDS")%>%
  mutate(Region = region) %>%
  mutate('15-19'=`15`+`16`+`17`+`18`+`19`)%>%
  mutate('20-24' = `20` + `21` + `22` + `23` + `24`)%>%
  mutate('25-29'=`25`+`26`+`27`+`28`+`29`)%>%
  mutate('30-34' = `30` + `31` + `32` + `33` + `34`)%>%
  mutate('35-39'=`35`+`36`+`37`+`38`+`39`)%>%
  mutate('40-44' = `40` + `41` + `42` + `43` + `44`)%>%
  mutate('45-49'=`45`+`46`+`47`+`48`+`49`)%>%
  mutate('50-54' = `50` + `51` + `52` + `53` + `54`)%>%
  mutate('55-59'=`55`+`56`+`57`+`58`+`59`)%>%
  mutate('60-64' = `60` + `61` + `62` + `63` + `64`)%>%
  dplyr::select(c(Region, '15-19', '20-24', '25-29', '30-34', '35-39', '40-44', '45-49', '50-54', '55-59', '60-64'))
data_insee_active_long = data_insee_active %>% pivot_longer(!Region, names_to = "AgeGroup", values_to = 'N_active')
data_insee_active_long$Region = factor(data_insee_active_long$Region, labels = vec_regions)

## INSEE data for total population in France (N_total)
data_insee_total = read.csv2(file = "Data/data_age_by_region2017.csv")
colnames(data_insee_total) = c('Region', '17-19', '20-24', '25-29', '30-34', '35-39', '40-44', '45-49', '50-54', '55-59', '60-64')
data_insee_total$Region <- gsub(".*le-de-France", "Ile-de-France", data_insee_total$Region)
data_insee_total_long = data_insee_total %>% pivot_longer(!Region, names_to = "AgeGroup", values_to = 'N_total')
data_insee_total_long$Region = factor(data_insee_total_long$Region, labels = vec_regions)

## Merged mixed INSEE data, calculate proportion of population actively employed per region and age group
data_insee = merge(data_insee_total_long, data_insee_active_long)%>%
  mutate(p_active = N_active/N_total)

## Merge COVID-19 data and INSEE data, limit to period prior to June 1, 2020
data_merged = merge(data_symptinc, data_insee)%>%
  mutate(Proportion_COVID_prevalence = Prevalence/N_total,
         Proportion_COVID_incidence = Symptomatic/N_total)%>%
  filter(Time < date_end_peak)



#### PROBABILITIES ###################################

data_merged_wave1 = filter(data_merged, Period == 1)

## Compute the probability of symptomatic COVID over the first wave corresponding to the same time period as Malakoff survey (Period "1"): cumulative COVID prevalence divided by person-days
p_symptomatic_COVID_wave1 = sum(data_merged_wave1$Prevalence)/sum(data_merged_wave1$N_total)
p_sick_leave_wave1 = p_leave_wave1;
p_symptomatic_COVID_given_leave_wave1 = p_COVID_given_leave_wave1 ;

## Final calculation of COVID-19 sick leave:
##   1. calculate probability of leave given COVID-19 using data from first wave
##   2. calculate overall probability of COVID-19 leave based on conditional probabilities
##   3. translate probability of COVID-19 leave into predicted number of leaves by multiplying by active population
data_final = data_merged%>%
  mutate(p_leave_given_COVID = p_symptomatic_COVID_given_leave_wave1*p_sick_leave_wave1/p_symptomatic_COVID_wave1)%>%
  mutate(p_COVID_leave_incidence = p_active * Proportion_COVID_incidence * p_leave_given_COVID)%>%
  mutate(p_COVID_leave_prevalence = p_active * Proportion_COVID_incidence * p_leave_given_COVID * duration_sickleave)%>%
  mutate(N_COVID_leave_active_incidence = p_COVID_leave_incidence * N_active)%>%
  mutate(N_COVID_leave_active_prevalence = p_COVID_leave_prevalence * N_active)%>%
  ungroup()


## Aggregate data for all of France
data_final_france = data_final%>%
  group_by(Time, AgeGroup)%>%
  dplyr::summarise(Incidence = sum(Symptomatic),
                   N_COVID_leave_active_incidence = sum(N_COVID_leave_active_incidence),
                   N_COVID_leave_active_prevalence = sum(N_COVID_leave_active_prevalence),
                   N_active = sum(N_active))%>%
  mutate(p_COVID_leave_incidence = N_COVID_leave_active_incidence/N_active,
         p_COVID_leave_prevalence = N_COVID_leave_active_prevalence/N_active)

## Data to compare prevalence peaks

data_compare_peaks = data_final%>%
  group_by(AgeGroup, Region)%>%
  dplyr::summarise(N_COVID_leave_active_max = max(N_COVID_leave_active_prevalence),
                   Prevalence_max = max(Prevalence))

data_compare_peaks_region = data_final%>%
  group_by(Region)%>%
  dplyr::summarise(N_COVID_leave_active_max = max(N_COVID_leave_active_prevalence),
                   Prevalence_max = max(Prevalence))


### CLOSE CONTACTS (CC) ###################

# Load in cas_contact estimates 
# risk_ variables = for all of my contacts, what is sum of (number of contacts in each age_group * perCapitaSymptomatic of those contacts * notificationProbability)
data_cc_raw = read.csv2(file =  "Data/risk_cc_pop_tele4.csv", stringsAsFactors = F)

# make relevant columns numeric
numeric_cols = c('risk_hh', 'risk_familprol', 'risk_travail', 'contacts_hh', 'contacts_familprol', 'contacts_travail')
data_cc_raw[,numeric_cols] = sapply(data_cc_raw[,numeric_cols],as.numeric)

### how many daily contacts?
data_cc_raw%>%mutate(contacts_total = contacts_travail + contacts_familprol + contacts_hh)%>%
  filter(Date == "2020-03-10",
         own_AgeGroup != "17-19")

# NB: must update date to reflect an assumed 2-day delay to contact notification
data_cc_correcttime = data_cc_raw%>%
  mutate(Time = as.Date(Date)+2,
         risk_total = risk_familprol + risk_hh + risk_travail,
         contacts_total = contacts_travail + contacts_familprol + contacts_hh,
         AgeGroup = own_AgeGroup)%>%
  dplyr::select(-c(Population, Date, own_AgeGroup))%>%
  filter(Time < date_end_peak, AgeGroup != '17-19')%>%
  mutate(Region = ifelse(Region == "Provence-Alpes-C\xf4te d'Azur", "PACA", Region))

# slope of ROC of symptomatic covid at start
data_final%>%
  filter(Time < as.Date("2020-03-14"))%>%
  group_by(Time)%>%
  summarise(Symptomatic = sum(Symptomatic))%>%
  mutate(ROC = c(0,diff(Symptomatic)))

ROC = 0.887 

data_cc = rbind(data_cc_correcttime%>%
                  filter(Time == "2020-03-12")%>%
                  mutate(Time = as.Date("2020-03-10"))%>%
                  mutate(risk_hh = risk_hh*ROC*ROC,
                         risk_familprol = risk_familprol*ROC*ROC,
                         risk_travail = risk_travail*ROC*ROC,
                         contacts_hh = contacts_hh*ROC*ROC,
                         contacts_familprol = contacts_familprol*ROC*ROC,
                         contacts_travail = contacts_travail*ROC*ROC,
                         risk_total = risk_total*ROC*ROC,
                         contacts_total = contacts_total*ROC*ROC),
                data_cc_correcttime%>%
                  filter(Time == "2020-03-12")%>%
                  mutate(Time = as.Date("2020-03-11"))%>%
                  mutate(risk_hh = risk_hh*ROC,
                         risk_familprol = risk_familprol*ROC,
                         risk_travail = risk_travail*ROC,
                         contacts_hh = contacts_hh*ROC,
                         contacts_familprol = contacts_familprol*ROC,
                         contacts_travail = contacts_travail*ROC,
                         risk_total = risk_total*ROC,
                         contacts_total = contacts_total*ROC),
                data_cc_correcttime)

# fix region labels using those already in place for data_final
region_levels = unique(data_cc$Region)
region_levels_ordered = region_levels[order(region_levels)]

region_labels = unique(data_final$Region)
region_labels_ordered = region_labels[order(region_labels)]

data_cc$Region = factor(data_cc$Region, levels = region_levels_ordered, labels = region_labels_ordered)

#### NUMBER OF CAS CONTACT
# Merge with previous data and estimate N_absence_cc (number of sick leaves from cas-contact) assuming different probabilities of notification

data_final_cc = merge(data_final%>%select(-c(R, Hospitalized, Period)), data_cc%>%select(-RegionCode))

# baseline sick leave: assume 50% of individuals notified take arrêt maladie
p_leave_if_notified_cc = 0.67
# baseline notification: 80% reporting workplace + familprol; 100% household
data_final_cc_base = data_final_cc%>%
  mutate(N_absence_cc_hh = risk_hh * N_active * 0.9 * p_leave_if_notified_cc,
         N_absence_cc_familprol = risk_familprol * N_active * 0.6 * p_leave_if_notified_cc,
         N_absence_cc_travail = risk_travail * N_active * 0.6 * p_leave_if_notified_cc,
         N_absence_cc_total = N_absence_cc_hh + N_absence_cc_familprol + N_absence_cc_travail)

# save(data_final_cc_base, file = paste0(filepath,"/data_final_sick_leave.Rdata"))

# sensitivity analysis: 75% probability of reporting (across all categories)
data_final_cc_100 = data_final_cc%>%
  mutate(p_reporting = 1,
         N_absence_cc_hh = risk_hh * N_active * p_reporting * p_leave_if_notified_cc,
         N_absence_cc_familprol = risk_familprol * N_active * p_reporting * p_leave_if_notified_cc,
         N_absence_cc_travail = risk_travail * N_active * p_reporting * p_leave_if_notified_cc,
         N_absence_cc_total = N_absence_cc_hh + N_absence_cc_familprol + N_absence_cc_travail)

# sensitivity analysis: 75% probability of reporting (across all categories)
data_final_cc_75 = data_final_cc%>%
  mutate(p_reporting = 3/4,
         N_absence_cc_hh = risk_hh * N_active * p_reporting * p_leave_if_notified_cc,
         N_absence_cc_familprol = risk_familprol * N_active * p_reporting * p_leave_if_notified_cc,
         N_absence_cc_travail = risk_travail * N_active * p_reporting * p_leave_if_notified_cc,
         N_absence_cc_total = N_absence_cc_hh + N_absence_cc_familprol + N_absence_cc_travail)

# sensitivity analysis: 50% probability of reporting (across all categories)
data_final_cc_50 = data_final_cc%>%
  mutate(p_reporting = 2/4,
         N_absence_cc_hh = risk_hh * N_active * p_reporting * p_leave_if_notified_cc,
         N_absence_cc_familprol = risk_familprol * N_active * p_reporting * p_leave_if_notified_cc,
         N_absence_cc_travail = risk_travail * N_active * p_reporting * p_leave_if_notified_cc,
         N_absence_cc_total = N_absence_cc_hh + N_absence_cc_familprol + N_absence_cc_travail)

# sensitivity analysis: 25% probability of reporting (across all categories)
data_final_cc_25 = data_final_cc%>%
  mutate(p_reporting = 1/4,
         N_absence_cc_hh = risk_hh * N_active * p_reporting * p_leave_if_notified_cc,
         N_absence_cc_familprol = risk_familprol * N_active * p_reporting * p_leave_if_notified_cc,
         N_absence_cc_travail = risk_travail * N_active * p_reporting * p_leave_if_notified_cc,
         N_absence_cc_total = N_absence_cc_hh + N_absence_cc_familprol + N_absence_cc_travail)


### 

data_cumulative_base = data_final_cc_base%>%
  group_by(AgeGroup, Region)%>%
  dplyr::summarise(Symptomatic = sum(Symptomatic),
                   N_COVID_leave_active_incidence = sum(N_COVID_leave_active_incidence),
                   N_absence_cc_hh = sum(N_absence_cc_hh),
                   N_absence_cc_familprol = sum(N_absence_cc_familprol),
                   N_absence_cc_travail = sum(N_absence_cc_travail),
                   N_absence_cc_total = sum(N_absence_cc_total))%>%
  mutate(N_sick_leave = N_COVID_leave_active_incidence + N_absence_cc_total)

data_cumulative_base_region = data_final_cc_base%>%
  group_by(Region)%>%
  dplyr::summarise(Symptomatic = sum(Symptomatic),
                   N_COVID_leave_active_incidence = sum(N_COVID_leave_active_incidence),
                   N_absence_cc_hh = sum(N_absence_cc_hh),
                   N_absence_cc_familprol = sum(N_absence_cc_familprol),
                   N_absence_cc_travail = sum(N_absence_cc_travail),
                   N_absence_cc_total = sum(N_absence_cc_total))%>%
  mutate(N_sick_leave = N_COVID_leave_active_incidence + N_absence_cc_total)

data_cumulative_base_age = data_final_cc_base%>%
  group_by(AgeGroup)%>%
  dplyr::summarise(Symptomatic = sum(Symptomatic),
                   N_COVID_leave_active_incidence = sum(N_COVID_leave_active_incidence),
                   N_absence_cc_hh = sum(N_absence_cc_hh),
                   N_absence_cc_familprol = sum(N_absence_cc_familprol),
                   N_absence_cc_travail = sum(N_absence_cc_travail),
                   N_absence_cc_total = sum(N_absence_cc_total))%>%
  mutate(N_sick_leave = N_COVID_leave_active_incidence + N_absence_cc_total)


