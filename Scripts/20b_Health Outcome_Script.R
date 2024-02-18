############## SCRIPT FOR HEALTH OUTCOME DATASET ##############

######## WORLD BANK SDG HEALTH OUTCOMES ###############
WB_HO_outcome <- c("SH.STA.MMRT", # 3.1.1 Maternal mortality ratio			per 100,000 live births 
                  "SH.STA.BRTC.ZS", # 3.1.2 Birth attended by skilled health personnel	 % of total 
                  "SH.DYN.MORT", # 3.2.1 Under 5 mortality rate 	per 1,000 live births  
                  "SH.DYN.NMRT", # 3.2.2 Neonatal mortality rate 	per 1,000 live births
                  "SH.DYN.AIDS.ZS", # 3.3.1 New HIV cases per 1000 uninfected population % of population ages 15-49) 
                  "SH.TBS.INCD", # 3.3.2 Tuberculosis incidence per 100,000 population  per 100,000 people
                  "SH.MLR.INCD.P3", # 3.3.3 Malaria incidence per 1000 population per 1,000 population at risk)
                              #*3.3.4 Hepatitis B incidence per 100,000 population 
                              #* 3.3.5 People requiring intervention on tropical diseases
                  "SH.DYN.NCOM.ZS", # 3.4.1 Mortality rate from cardiovascular diseases, cancer, diabetes and chronic resp disease: ages 30 and 70 (%)
                  "SH.STA.SUIC.P5", # 3.4.2 Suicide morality rate per 100,000 population)	 
                  "SH.ALC.PCAP.LI", # 3.5.2 Alcohol per capital consumption 	15+ years of age
                  "SH.STA.TRAF.P5", # 3.6.1 Death rate from road traffic injuries per 100,000 population
                  "SH.FPL.SATM.ZS", # 3.7.1 Demand for family planning satisfied % of married women with demand for family planning
                  "SP.ADO.TFRT", # 3.7.2 Adolescent birth rate per 1000 women births per 1,000 women ages 15-19
                              #* 3.8.1 Coverage of essential health services  
                  "SH.UHC.OOPC.25.TO", # 3.8.2 Rate of High OOP (>10 or >25% of HHs income) Number of pop 
                  "SH.STA.AIRP.P5", # 3.9.1 Mortality rate caused by air pollution per 100,000 population		
                  "SH.STA.WASH.P5", # 3.9.2 Mortality rate from unsafe water, sanitation or hygiene per 100,000 population
                  "SH.STA.POIS.P5", # 3.9.3 Mortality rate from unintentional poisoning per 100,000 population)		
                  "SH.VAC.TTNS.ZS", #*3b.1 Population prop vaccinated coverage by all vaccine Newborns protected against tetanus (%)
                  "SH.MED.CMHW.P3", # 3c.1 Health workers density and distribution  (community health workers per 1,000 people)	
                  "SN.ITK.DEFC.ZS", # 2.1   Incidence of undernourishment in pop % of population
                  "SH.STA.STNT.ZS", # 2.2   Incidence of undernourishment among under 5 children (Stunting children)  % of children under 5
                  "SH.STA.ARIC.ZS", # ARI treatment (% of children under 5 taken to a health provider)			 
                  "SP.DYN.LE00.IN", # Life expectancy at birth, total (years)	
)



###### INDICATORS FOR HEALTH OUTCOME AND UN SDG CODES #####
# Indicator 3.3.4 should be derived from World Bank due to too many missing values on UNstat
summary(oda_disb)
UNSDG_HO_outcomes <- c("SH_STA_MORT", # Indicator 3.1.1, Series : Maternal mortality ratio
                 "SH_STA_BRTC", # Indicator 3.1.2, Series : Proportion of births attended by skilled health personnel (%)
                 "SH_DYN_MORT", # Indicator 3.2.1, Series : Under-five mortality rate, by sex (deaths per 1,000 live births)
                 "SH_DYN_IMRT", # Indicator 3.2.1, Series : Infant mortality rate (deaths per 1,000 live births)
                 "SH_DYN_NMRT", # Indicator 3.2.2, Series : Neonatal mortality rate (deaths per 1,000 live births)
                 "SH_HIV_INCD", # Indicator 3.3.1, Series : Number of new HIV infections per 1,000 uninfected population, by sex and age (per 1,000 uninfected population)
                 "SH_TBS_INCD", # Indicator 3.3.2, Series : Tuberculosis incidence (per 100,000 population) 
                 "SH_STA_MALR", # Indicator 3.3.3, Series : Malaria incidence per 1,000 population at risk (per 1,000 population) 
                 "SH_HAP_HBSAG", # Indicator 3.3.4, Series : Prevalence of hepatitis B surface antigen (HBsAg) (%) 
                 "SH_TRP_INTVN", # Indicator 3.3.5, Series : Number of people requiring interventions against neglected tropical diseases (number) 
                 "SH_DTH_NCOM", # Indicator 3.4.1, Series : Mortality rate attributed to cardiovascular disease, cancer, diabetes or chronic respiratory disease (probability)
                 "SH_STA_SCIDE", # Indicator 3.4.2, Series : Suicide mortality rate, by sex (deaths per 100,000 population) 
                 "SH_SUD_ALCOL", # Indicator 3.5.1, Series : Alcohol use disorders, 12-month prevalence (%)
                 "SH_SUD_TREAT", # Indicator 3.5.1, Series : Coverage of treatment interventions (pharmacological, psychosocial and rehabilitation and aftercare services) for substance use disorders (%) 
                 "SH_ALC_CONSPT", # Indicator 3.5.2, Series : Alcohol consumption per capita (aged 15 years and older) within a calendar year (litres of pure alcohol) 
                 "SH_STA_TRAF", # Indicator 3.6.1, Series : Death rate due to road traffic injuries, by sex (per 100,000 population)
                 "SH_FPL_MTMM", # Indicator 3.7.1, Series : Proportion of women of reproductive age (aged 15-49 years) who have their need for family planning satisfied with modern methods (% of women aged 15-49 years)
                 "SP_DYN_ADKL", # Indicator 3.7.2, Series : Adolescent birth rate (per 1,000 women aged 15-19 and 10-14 years) 
                 "SH_ACS_UNHC", # Indicator 3.8.1, Series : Universal health coverage (UHC) service coverage index 
                 "SH_XPD_EARN25", # Indicator 3.8.2, Series : Proportion of population with large household expenditures on health (greater than 25%) as a share of total household expenditure or income (%) 
                 "SH_HAP_ASMORT", # Indicator 3.9.1, Series : Age-standardized mortality rate attributed to household air pollution (deaths per 100,000 population) 
                 "SH_STA_ASAIRP", # Indicator 3.9.1, Series : Age-standardized mortality rate attributed to household and ambient air pollution (deaths per 100,000 population) 
                 "SH_AAP_ASMORT", # Indicator 3.9.1, Series : Age-standardized mortality rate attributed to ambient air pollution (deaths per 100,000 population) 
                 "SH_STA_WASHARI", # Indicator 3.9.2, Series : Mortality rate attributed to unsafe water, unsafe sanitation and lack of hygiene from diarrhoea, intestinal nematode infections, malnutrition and acute respiratory infections (deaths per 100,000 population) 
                 "SH_STA_POISN", # Indicator 3.9.3, Series : Mortality rate attributed to unintentional poisonings, by sex (deaths per 100,000 population) 
                 "SH_PRV_SMOK", # Indicator 3.a.1, Series : Age-standardized prevalence of current tobacco use among persons aged 15 years and older, by sex (%) 
                 "SH_ACS_DTP3", # Indicator 3.b.1, Series : Proportion of the target population with access to 3 doses of diphtheria-tetanus-pertussis (DTP3) (%) 
                 "SH_ACS_MCV2", # Indicator 3.b.1, Series : Proportion of the target population with access to measles-containing-vaccine second-dose (MCV2) (%) 
                 "SH_ACS_PCV3", # Indicator 3.b.1, Series : Proportion of the target population with access to pneumococcal conjugate 3rd dose (PCV3) (%) 
                 "SH_ACS_HPV", # Indicator 3.b.1, Series : Proportion of the target population with access to affordable medicines and vaccines on a sustainable basis, human papillomavirus (HPV) (%) 
                 "DC_TOF_HLTHNT", # Indicator 3.b.2, Series : Total official development assistance to medical research and basic health sectors, net disbursement, by recipient countries (millions of constant 2021 United States dollars) 
                 "SH_HLF_EMED", # Indicator 3.b.3, Series : Proportion of health facilities that have a core set of relevant essential medicines available and affordable on a sustainable basis (%) 
                 "SH_MED_DEN", # Indicator 3.c.1, Series : Health worker density, by type of occupation (per 10,000 population) 
                 "SH_IHR_CAPS", # Indicator 3.d.1, Series : International Health Regulations (IHR) capacity, by type of IHR capacity (%) 
                 "SH_BLD_MRSA", # Indicator 3.d.2, Series : Percentage of bloodstream infection due to methicillin-resistant Staphylococcus aureus (MRSA) among patients seeking care and whose blood sample is taken and tested (%)
                 "SN_ITK_DEFC", # Indicator 2.1.1, Series : Prevalence of undernourishment (%)
                 "AG_PRD_FIESS", # Indicator 2.1.2, Series : Prevalence of severe food insecurity (%)  
                 "SH_STA_STNT", # Indicator 2.2.1, Series : Proportion of children moderately or severely stunted (%)
                 "SH_STA_WAST", # Indicator 2.2.2, Series : Proportion of children moderately or severely wasted (%)
                 "SH_STA_ANEM", # Indicator 2.2.3, Series : Proportion of women aged 15-49 years with anaemia (%) 
                 )



########### Packages #############
install.packages("devtools")
library(devtools)
devtools::install_github("DrMattG/SDGsR", dependencies = TRUE) # Load SDGsR package
library(SDGsR)
install.packages("WDI") # Package for World Bank
library(WDI)
library(tidyverse)
####### Load data to query list of countries ######
health <- read.csv2("Health_with_ODA_data.csv", sep = ";")

yearWB <- 1990:2021
year <- 2000:2020
country <- unique(oda_disb$country) # for UN SDG stat
WBcountries <- unique(oda_disb$iso3c) # World Bank prefers iso code 



###### Method took long and not suitable ######
# Initialize an empty list to store the results
result_list <- list()

# Loop through each country and indicator
for (country in countries) {
  for (indicator in Env_death) {
    # Get indicator data for each combination
    indicator_data <- get_indicator(Country = country, indicator = indicator)
    
    # Store the result in the list
    result_list[[paste(country, indicator, sep = "_")]] <- indicator_data
  }
}



####### Amateur method ##########
# Indicator 3.1.1, Series : Maternal mortality ratio
indicator_3.1.1 <- get_indicator_data(indicator = "3.1.1") %>%
  dplyr::filter(geoAreaName %in% countries, 
                timePeriodStart %in% year, 
                series == "SH_STA_MORT") %>%
  dplyr::select(geoAreaName, timePeriodStart, value) %>% 
  dplyr::mutate(across(c(value, timePeriodStart), as.numeric),  # Treat value and timePeriodStart as numbers
                value = round(value, 2)) %>%
  dplyr::rename(Mat_Mort_ratio = value, country = geoAreaName, year = timePeriodStart)



one <- indicator_3.1.1 %>%
  dplyr::filter(geoAreaName %in% countries, 
                timePeriodStart %in% year, 
                series == "SH_STA_MORT"
  ) %>%
  dplyr::select(geoAreaName, timePeriodStart, value) %>% 
  dplyr::mutate(across(c(value, timePeriodStart), as.numeric),  # Treat value and timePeriodStart as numbers
         value = round(value, 2)) %>%
  dplyr::rename(Mat_Mort_ratio = value, country = geoAreaName, year = timePeriodStart)

## Merge df as we go 
oda_disb <- merge(oda_disb, one, by = c("country", "year"), all = TRUE)
rm(one)


## SH_STA_BRTC Indicator 3.1.2, Series : Proportion of births attended by skilled health personnel (%)
indicator_3.1.2 <- get_indicator_data(indicator = "3.1.2") 
two <- indicator_3.1.2 %>%
  dplyr::filter(geoAreaName %in% countries, 
                timePeriodStart %in% year, 
                series == "SH_STA_BRTC"
  ) %>%
  dplyr::select(geoAreaName, timePeriodStart, value) %>% 
  dplyr::mutate(across(c(value, timePeriodStart), as.numeric),  # Treat value and timePeriodStart as numbers
                value = round(value, 2)) %>%
  dplyr::rename(birthToPersonel = value, country = geoAreaName, year = timePeriodStart)
## Merge df as we go 
oda_disb <- merge(oda_disb, two, by = c("country", "year"), all = TRUE)
rm(two)


# "SH_DYN_MORT", # Indicator 3.2.1, Series : Under-five mortality rate, by sex (deaths per 1,000 live births)
## Only infant mortality is avalaible from the series code' "SH_DYN_IMRT", # Indicator 3.2.1, Series : Infant mortality rate (deaths per 1,000 live births)
## Data is in absolute number 
indicator_3.2.1 <- get_indicator_data(indicator = "3.2.1") 
unique(indicator_3.2.1$series)
three <- indicator_3.2.1 %>%
  dplyr::filter(geoAreaName %in% countries, 
                indicator_3.2.1$dimensions$Sex == "BOTHSEX",
                timePeriodStart %in% year,
                series == "SH_DYN_IMRTN" 
                #indicator_3.2.1$dimensions$Age == "BOTHSEX"
  ) %>%
  dplyr::select(geoAreaName, timePeriodStart, value) %>% 
  dplyr::mutate(across(c(value, timePeriodStart), as.numeric),  # Treat value and timePeriodStart as numbers
                value = round(value, 2)) %>%
  dplyr::rename(Infant_Mort = value, country = geoAreaName, year = timePeriodStart)


## Merge df as we go 
oda_disb <- merge(oda_disb, three, by = c("country", "year"), all = TRUE)
rm(three)

# "SH_DYN_NMRT", # Indicator 3.2.2, Series : Neonatal mortality rate (deaths per 1,000 live births)
indicator_3.2.2 <- get_indicator_data(indicator = "3.2.2") 
four <- indicator_3.2.2 %>%
  dplyr::filter(geoAreaName %in% countries,
                series == "SH_DYN_NMRT",
                indicator_3.2.2$dimensions$Sex == "BOTHSEX",
                timePeriodStart %in% year
                #indicator_3.2.1$dimensions$Age == "BOTHSEX"
  ) %>%
  dplyr::select(geoAreaName, timePeriodStart, value) %>% 
  dplyr::mutate(across(c(value, timePeriodStart), as.numeric),  # Treat value and timePeriodStart as numbers
                value = round(value, 2)) %>%
  dplyr::rename(Neonatal_Mort = value, country = geoAreaName, year = timePeriodStart)



## Merge df as we go 
oda_disb <- merge(oda_disb, four, by = c("country", "year"), all = TRUE)
rm(four)


## "SH_HIV_INCD", # Indicator 3.3.1, Series : Number of new HIV infections per 1,000 uninfected population, by sex and age (per 1,000 uninfected population)

indicator_3.3.1 <- get_indicator_data(indicator = "3.3.1") 

unique(indicator_3.3.1$dimensions$Age)

five <- indicator_3.3.1 %>%
  filter(geoAreaName %in% countries,
         series == "SH_HIV_INCD",
         dimensions$Age == "ALLAGE",
         dimensions$Sex == "BOTHSEX",
         timePeriodStart %in% year
  ) %>%
  select(geoAreaName, timePeriodStart, value) %>% 
  mutate(across(c(value, timePeriodStart), as.numeric),  # Treat value and timePeriodStart as numbers
         value = round(value, 2)) %>%
  rename(HIV_incidence = value, country = geoAreaName, year = timePeriodStart)


## Merge df as we go 
oda_disb <- merge(oda_disb, five, by = c("country", "year"), all = TRUE)
rm(five)

## "SH_TBS_INCD", # Indicator 3.3.2, Series : Tuberculosis incidence (per 100,000 population) 

indicator_3.3.2 <- get_indicator_data(indicator = "3.3.2")
unique(indicator_3.3.2$series)

six <- indicator_3.3.2 %>%
  filter(geoAreaName %in% countries,
         series == "SH_TBS_INCD",
         #dimensions$Age == "ALLAGE",
         #dimensions$Sex == "BOTHSEX",
         timePeriodStart %in% year
  ) %>%
  select(geoAreaName, timePeriodStart, value) %>% 
  mutate(across(c(value, timePeriodStart), as.numeric),  # Treat value and timePeriodStart as numbers
         value = round(value, 2)) %>%
  rename(Tuberculosis_incid = value, country = geoAreaName, year = timePeriodStart)

#head(six)

## Merge df as we go 
oda_disb <- merge(oda_disb, six, by = c("country", "year"), all = TRUE)
rm(six)


## "SH_STA_MALR", # Indicator 3.3.3, Series : Malaria incidence per 1,000 population at risk (per 1,000 population) 
indicator_3.3.3 <- get_indicator_data(indicator = "3.3.3")
# unique(indicator_3.3.3$dimensions)

seven <- indicator_3.3.3 %>%
  filter(geoAreaName %in% countries,
         series == "SH_STA_MALR",
         #dimensions$Age == "ALLAGE",
         #dimensions$Sex == "BOTHSEX",
         timePeriodStart %in% year
  ) %>%
  select(geoAreaName, timePeriodStart, value) %>% 
  mutate(across(c(value, timePeriodStart), as.numeric),  # Treat value and timePeriodStart as numbers
         value = round(value, 2)) %>%
  rename(Malaria_incid = value, country = geoAreaName, year = timePeriodStart)

head(seven)

## Merge df as we go 
oda_disb <- merge(oda_disb, seven, by = c("country", "year"), all = TRUE)
rm(seven)


## "SH_HAP_HBSAG", # Indicator 3.3.4, Series : Prevalence of hepatitis B surface antigen (HBsAg) (%) (among under 5 children)
indicator_3.3.4 <- get_indicator_data(indicator = "3.3.4")
unique(indicator_3.3.4$series)

eight <- indicator_3.3.4 %>%
  filter(geoAreaName %in% countries,
         series == "SH_HAP_HBSAG",
         #dimensions$Age == "ALLAGE",
         #dimensions$Sex == "BOTHSEX",
         timePeriodStart %in% year
  ) %>%
  select(geoAreaName, timePeriodStart, value) %>% 
  mutate(across(c(value, timePeriodStart), as.numeric),  # Treat value and timePeriodStart as numbers
         value = round(value, 2)) %>%
  rename(hepatitis_B_under5 = value, country = geoAreaName, year = timePeriodStart)

head(eight)

## Merge df as we go 
oda_disb <- merge(oda_disb, eight, by = c("country", "year"), all = TRUE)
rm(eight)


## "SH_TRP_INTVN", # Indicator 3.3.5, Series : Number of people requiring interventions against neglected tropical diseases (number) 
indicator_3.3.5 <- get_indicator_data(indicator = "3.3.5")
unique(indicator_3.3.5$dimensions)

nine <- indicator_3.3.5 %>%
  filter(geoAreaName %in% countries,
         series == "SH_TRP_INTVN",
         #dimensions$Age == "ALLAGE",
         #dimensions$Sex == "BOTHSEX",
         timePeriodStart %in% year
  ) %>%
  select(geoAreaName, timePeriodStart, value) %>% 
  mutate(across(c(value, timePeriodStart), as.numeric),  # Treat value and timePeriodStart as numbers
         value = round(value, 2)) %>%
  rename(Tropical_dis = value, country = geoAreaName, year = timePeriodStart)

head(nine, 70)

## Merge df as we go 
oda_disb <- merge(oda_disb, nine, by = c("country", "year"), all = TRUE)
rm(nine)


## "SH_DTH_NCOM", # Indicator 3.4.1, Series : Mortality rate attributed to cardiovascular disease, cancer, diabetes or chronic respiratory disease (probability)
## Data is unclear. Retrive the data from World Bank

# indicator_3.4.1_UNstat <- get_indicator_data(indicator = "3.4.1")
# World Bank: "SH.DYN.NCOM.ZS", # 3.4.1 Mortality rate from cardiovascular diseases, cancer, diabetes and chronic resp disease: ages 30 and 70 (%)

miss <- c("Anguilla", "Cook Islands", 
          "Mayotte", "Montserrat", "Niue", 
          "Saint Helena", "Tokelau", "Wallis and Futuna") # The countries are missing from World Bank

oda_disb <- oda_disb %>%
  filter(!country %in% miss) # Remove countries from dataset 

WB_countrycode <- unique(oda_disb$iso3c) # World Bank prefers iso3c code
indicator_3.4.1 <- WDI(country = WB_countrycode, indicator = "SH.DYN.NCOM.ZS", 
                       start = yearWB[1], 
                       end = yearWB[length(yearWB)])

indicator_3.4.1 <- indicator_3.4.1 %>%
  rename(Noncomm_diseas = SH.DYN.NCOM.ZS) %>%
  select(iso3c, year, Noncomm_diseas)


## Merge df as we go 
oda_disb <- merge(oda_disb, indicator_3.4.1, by = c("iso3c", "year"), all = TRUE)

oda_disb <- oda_disb %>% 
  rename(country = country.x) %>%
  select(!country.y)
summary(oda_disb)


## "SH_STA_SCIDE", # Indicator 3.4.2, Series : Suicide mortality rate, by sex (deaths per 100,000 population) 
indicator_3.4.2 <- get_indicator_data(indicator = "3.4.2")
unique(indicator_3.4.2$dimensions)

ten <- indicator_3.4.2 %>%
  filter(geoAreaName %in% countries,
         series == "SH_STA_SCIDE",
         #dimensions$Age == "ALLAGE",
         dimensions$Sex == "BOTHSEX",
         timePeriodStart %in% year
  ) %>%
  select(geoAreaName, timePeriodStart, value) %>% 
  mutate(across(c(value, timePeriodStart), as.numeric),  # Treat value and timePeriodStart as numbers
         value = round(value, 2)) %>%
  rename(Suicide_rate = value, country = geoAreaName, year = timePeriodStart)

head(ten, 70)

## Merge df as we go 
oda_disb <- merge(oda_disb, ten, by = c("country", "year"), all = TRUE)
rm(ten)


## "SH_SUD_ALCOL", # Indicator 3.5.1, Series : Alcohol use disorders, 12-month prevalence (%)
## indicator_3.5.1 <- get_indicator_data(indicator = "3.5.1") UN Stat has too many missing values 
## Indicator is otherwise soruced from World Bank 
## WB: "SH.STA.SUIC.P5", # 3.5.1 Suicide morality rate per 100,000 population)	 
## WB: "SH.ALC.PCAP.LI", # 3.5.2 Alcohol per capital consumption 	15+ years of age
WB_countrycode <- unique(oda_disb$iso3c) # World Bank prefers iso3c code

indicator_3.5.1_WB <- WDI(country = WB_countrycode, indicator = c("SH.STA.SUIC.P5", "SH.ALC.PCAP.LI"), 
                       start = yearWB[1], 
                       end = yearWB[length(yearWB)])

indicator_3.5.1_WB <- indicator_3.5.1_WB %>%
  rename(Suicide_mort = SH.STA.SUIC.P5, Alcohol_percap = SH.ALC.PCAP.LI) %>%
  select(iso3c, year, Suicide_mort, Alcohol_percap)


## Merge df as we go 
oda_disb <- merge(oda_disb, indicator_3.5.1_WB, by = c("iso3c", "year"), all = TRUE)

summary(oda_disb)

#### Suspended the data and find solution later 
## "SH_SUD_TREAT", # Indicator 3.5.1, Series : Coverage of treatment interventions (pharmacological, psychosocial and rehabilitation and aftercare services) for substance use disorders (%) 
##indicator_3.5.1 <- get_indicator_data(indicator = "3.5.1") # UN Stat has too many missing values, I only used mental_health treatment coverage series 
# unique(indicator_3.5.1$dimensions$`Substance use disorders`)

# eleven_c <- indicator_3.5.1 %>%
  # filter(geoAreaName %in% countries,
    #     series == "SH_SUD_TREAT",
         #dimensions$`Substance use disorders` == "TOTAL",
         #dimensions$Age == "ALLAGE",
         #dimensions$Sex == "BOTHSEX",
     #    timePeriodStart %in% year
  #) %>%
  #select(geoAreaName, timePeriodStart, value) %>% 
  #mutate(across(c(value, timePeriodStart), as.numeric),  # Treat value and timePeriodStart as numbers
  #       value = round(value, 2)) %>%
  #rename(Mental_treat_cov = value, country = geoAreaName, year = timePeriodStart)

## Merge df as we go 
#oda_disb <- merge(oda_disb, ten, by = c("country", "year"), all = TRUE)
#rm(eleven_c)


## "SH_ALC_CONSPT", # Indicator 3.5.2, Series : Alcohol consumption per capita (aged 15 years and older) within a calendar year (litres of pure alcohol) 
## Both world bank and UN stat is missing more than 70%. I will maintain WB data for now and address it later 
indicator_3.5.2 <- get_indicator_data(indicator = "3.5.2")



## "SH_STA_TRAF", # Indicator 3.6.1, Series : Death rate due to road traffic injuries, by sex (per 100,000 population)
indicator_3.6.1 <- get_indicator_data(indicator = "3.6.1")

unique(indicator_3.6.1$dimensions)

eleven_c <- indicator_3.6.1 %>%
filter(geoAreaName %in% countries,
     series == "SH_STA_TRAF",
#dimensions$`Substance use disorders` == "TOTAL",
    #dimensions$Age == "ALLAGE",
    dimensions$Sex == "BOTHSEX",
    timePeriodStart %in% year
) %>%
    select(geoAreaName, timePeriodStart, value) %>% 
    mutate(across(c(value, timePeriodStart), as.numeric),  # Treat value and timePeriodStart as numbers
       value = round(value, 3)) %>%
    rename(Road_traffic_mort = value, country = geoAreaName, year = timePeriodStart)

## Merge df as we go 
oda_disb <- merge(oda_disb, eleven_c, by = c("country", "year"), all = TRUE)
rm(eleven_c)
summary(oda_disb)


## "SH_FPL_MTMM", # Indicator 3.7.1, Series : Proportion of women of reproductive age (aged 15-49 years) who have their need for family planning satisfied with modern methods (% of women aged 15-49 years)
indicator_3.7.1 <- get_indicator_data(indicator = "3.7.1")

unique(indicator_3.7.1$dimensions$Age)

twelve <- indicator_3.7.1 %>%
  filter(geoAreaName %in% countries,
         series == "SH_FPL_MTMM",
         #dimensions$`Substance use disorders` == "TOTAL",
         #dimensions$Age == "ALLAGE",
         #dimensions$Sex == "BOTHSEX",
         timePeriodStart %in% year
  ) %>%
  select(geoAreaName, timePeriodStart, value) %>% 
  mutate(across(c(value, timePeriodStart), as.numeric),  # Treat value and timePeriodStart as numbers
         value = round(value, 3)) %>%
  rename(Family_planning_cov = value, country = geoAreaName, year = timePeriodStart)

## Merge df as we go 
oda_disb <- merge(oda_disb, twelve, by = c("country", "year"), all = TRUE)
rm(twelve)
summary(oda_disb)

############# Note: I stopped checking World Bank from Here ##############
## "SP_DYN_ADKL", # Indicator 3.7.2, Series : Adolescent birth rate (per 1,000 women aged 15-19 and 10-14 years) 
indicator_3.7.2 <- get_indicator_data(indicator = "3.7.2")
unique(indicator_3.7.2$series)

thirtheen_a <- indicator_3.7.2 %>%
  filter(geoAreaName %in% countries,
         series == "SP_DYN_ADKL",
         #dimensions$`Substance use disorders` == "TOTAL",
         dimensions$Age == "15-19",
         #dimensions$Sex == "BOTHSEX",
         timePeriodStart %in% year
  ) %>%
  select(geoAreaName, timePeriodStart, value) %>% 
  mutate(across(c(value, timePeriodStart), as.numeric),  # Treat value and timePeriodStart as numbers
         value = round(value, 3)) %>%
  rename(Adolesc_pregn_15to19 = value, country = geoAreaName, year = timePeriodStart)


## Merge df as we go 
oda_disb <- merge(oda_disb, thirtheen_a, by = c("country", "year"), all = TRUE)
rm(thirtheen_a)
summary(oda_disb)

## Adolescent pregnacy for 10 to 14 years 

thirtheen_b <- indicator_3.7.2 %>%
  filter(geoAreaName %in% countries,
         series == "SP_DYN_ADKL",
         #dimensions$`Substance use disorders` == "TOTAL",
         dimensions$Age == "10-14",
         #dimensions$Sex == "BOTHSEX",
         timePeriodStart %in% year
  ) %>%
  select(geoAreaName, timePeriodStart, value) %>% 
  mutate(across(c(value, timePeriodStart), as.numeric),  # Treat value and timePeriodStart as numbers
         value = round(value, 3)) %>%
  rename(Adolesc_pregn_10to14 = value, country = geoAreaName, year = timePeriodStart)


## Merge df as we go 
oda_disb <- merge(oda_disb, thirtheen_b, by = c("country", "year"), all = TRUE)
rm(thirtheen_b)
summary(oda_disb)

## "SH_ACS_UNHC", # Indicator 3.8.1, Series : Universal health coverage (UHC) service coverage index 
indicator_3.8.1 <- get_indicator_data(indicator = "3.8.1")
unique(indicator_3.8.1$dimensions)

fourteen <- indicator_3.8.1 %>%
  filter(geoAreaName %in% countries,
         series == "SH_ACS_UNHC",
         #dimensions$`Substance use disorders` == "TOTAL",
         #dimensions$Age == "15-19",
         #dimensions$Sex == "BOTHSEX",
         timePeriodStart %in% year
  ) %>%
  select(geoAreaName, timePeriodStart, value) %>% 
  mutate(across(c(value, timePeriodStart), as.numeric),  # Treat value and timePeriodStart as numbers
         value = round(value, 3)) %>%
  rename(UHC = value, country = geoAreaName, year = timePeriodStart)


## Merge df as we go 
oda_disb <- merge(oda_disb, fourteen, by = c("country", "year"), all = TRUE)
rm(fourteen)
summary(oda_disb)

## "SH_XPD_EARN25", # Indicator 3.8.2, Series : Proportion of population with large household expenditures on health (greater than 25%) as a share of total household expenditure or income (%) 
indicator_3.8.2 <- get_indicator_data(indicator = "3.8.2")
unique(indicator_3.8.2$series)

fifteen <- indicator_3.8.2 %>%
  filter(geoAreaName %in% countries,
         series == "SH_XPD_EARN25",
         #dimensions$`Substance use disorders` == "TOTAL",
         #dimensions$Age == "15-19",
         #dimensions$Sex == "BOTHSEX",
         timePeriodStart %in% year
  ) %>%
  select(geoAreaName, timePeriodStart, value) %>% 
  mutate(across(c(value, timePeriodStart), as.numeric),  # Treat value and timePeriodStart as numbers
         value = round(value, 3)) %>%
  rename(CHS_25perc = value, country = geoAreaName, year = timePeriodStart)


## Merge df as we go 
oda_disb <- merge(oda_disb, fifteen, by = c("country", "year"), all = TRUE)
rm(fifteen)
summary(oda_disb)


## "SH_HAP_ASMORT", # Indicator 3.9.1, Series : Age-standardized mortality rate attributed to household air pollution (deaths per 100,000 population) 
## "SH_STA_ASAIRP", # Indicator 3.9.1, Series : Age-standardized mortality rate attributed to household and ambient air pollution (deaths per 100,000 population) 
## "SH_AAP_ASMORT", # Indicator 3.9.1, Series : Age-standardized mortality rate attributed to ambient air pollution (deaths per 100,000 population) 
## indicator_3.9.1 <- get_indicator_data(indicator = "3.9.1")
# Data is small on SDG UN stat. I am using WB data
## WB: "SH.STA.AIRP.P5", # 3.9.1 Mortality rate caused by air pollution per 100,000 population
WB_countrycode <- unique(oda_disb$iso3c) # World Bank prefers iso3c code

indicator_3.9.1_WB <- WDI(country = WB_countrycode, indicator = "SH.STA.AIRP.P5", 
                           start = yearWB[1], 
                           end = yearWB[length(yearWB)])

indicator_3.9.1_WB <- indicator_3.9.1_WB %>%
  rename(Air_polut_Mort = SH.STA.AIRP.P5) %>%
  select(iso3c, year, Air_polut_Mort)


## Merge df as we go 
oda_disb <- merge(oda_disb, indicator_3.9.1_WB, by = c("iso3c", "year"), all = TRUE)
summary(oda_disb)

## "SH_STA_WASHARI", # Indicator 3.9.2, Series : Mortality rate attributed to unsafe water, unsafe sanitation and lack of hygiene from diarrhoea, intestinal nematode infections, malnutrition and acute respiratory infections (deaths per 100,000 population) 
indicator_3.9.2 <- get_indicator_data(indicator = "3.9.2")
unique(indicator_3.9.2$dimensions)

sixteen <- indicator_3.9.2 %>%
  filter(geoAreaName %in% countries,
         series == "SH_STA_WASHARI",
         #dimensions$`Substance use disorders` == "TOTAL",
         #dimensions$Age == "15-19",
         #dimensions$Sex == "BOTHSEX",
         timePeriodStart %in% year
  ) %>%
  select(geoAreaName, timePeriodStart, value) %>% 
  mutate(across(c(value, timePeriodStart), as.numeric),  # Treat value and timePeriodStart as numbers
         value = round(value, 3)) %>%
  rename(WaterSanit_Mort = value, country = geoAreaName, year = timePeriodStart)


## Merge df as we go 
oda_disb <- merge(oda_disb, sixteen, by = c("country", "year"), all = TRUE)
rm(sixteen)
summary(oda_disb)



## "SH_STA_POISN", # Indicator 3.9.3, Series : Mortality rate attributed to unintentional poisonings, by sex (deaths per 100,000 population) 
indicator_3.9.3 <- get_indicator_data(indicator = "3.9.3")
unique(indicator_3.9.3$dimensions)

seventeen <- indicator_3.9.3 %>%
  filter(geoAreaName %in% countries,
         series == "SH_STA_POISN",
         #dimensions$`Substance use disorders` == "TOTAL",
         #dimensions$Age == "15-19",
         dimensions$Sex == "BOTHSEX",
         timePeriodStart %in% year
  ) %>%
  select(geoAreaName, timePeriodStart, value) %>% 
  mutate(across(c(value, timePeriodStart), as.numeric),  # Treat value and timePeriodStart as numbers
         value = round(value, 3)) %>%
  rename(Poisoining_Mort = value, country = geoAreaName, year = timePeriodStart)


## Merge df as we go 
oda_disb <- merge(oda_disb, seventeen, by = c("country", "year"), all = TRUE)
rm(seventeen)
summary(oda_disb)

## "SH_PRV_SMOK", # Indicator 3.a.1, Series : Age-standardized prevalence of current tobacco use among persons aged 15 years and older, by sex (%) 
indicator_3.a.1 <- get_indicator_data(indicator = "3.a.1")
unique(indicator_3.a.1$dimensions)

seventeen <- indicator_3.a.1 %>%
  filter(geoAreaName %in% countries,
         series == "SH_PRV_SMOK",
         #dimensions$`Substance use disorders` == "TOTAL",
         #dimensions$Age == "15-19",
         dimensions$Sex == "BOTHSEX",
         timePeriodStart %in% year
  ) %>%
  select(geoAreaName, timePeriodStart, value) %>% 
  mutate(across(c(value, timePeriodStart), as.numeric),  # Treat value and timePeriodStart as numbers
         value = round(value, 3)) %>%
  rename(Tobacco_rate = value, country = geoAreaName, year = timePeriodStart)


## Merge df as we go 
oda_disb <- merge(oda_disb, seventeen, by = c("country", "year"), all = TRUE)
rm(seventeen)
summary(oda_disb)





## "SH_ACS_DTP3", # Indicator 3.b.1, Series : Proportion of the target population with access to 3 doses of diphtheria-tetanus-pertussis (DTP3) (%) 
## "SH_ACS_MCV2", # Indicator 3.b.1, Series : Proportion of the target population with access to measles-containing-vaccine second-dose (MCV2) (%) 
## "SH_ACS_PCV3", # Indicator 3.b.1, Series : Proportion of the target population with access to pneumococcal conjugate 3rd dose (PCV3) (%) 
## "SH_ACS_HPV", # Indicator 3.b.1, Series : Proportion of the target population with access to affordable medicines and vaccines on a sustainable basis, human papillomavirus (HPV) (%) 

indicator_3.b.1 <- get_indicator_data(indicator = "3.b.1")
unique(indicator_3.b.1$dimensions)

b1_c <- indicator_3.b.1 %>%
  filter(geoAreaName %in% countries,
         series == "SH_ACS_PCV3",
         #dimensions$`Substance use disorders` == "TOTAL",
         #dimensions$Age == "15-19",
         #dimensions$Sex == "BOTHSEX",
         timePeriodStart %in% year
  ) %>%
  select(geoAreaName, timePeriodStart, value) %>% 
  mutate(across(c(value, timePeriodStart), as.numeric),  # Treat value and timePeriodStart as numbers
         value = round(value, 3)) %>%
  rename(Pneum_vaccine = value, country = geoAreaName, year = timePeriodStart)


## Merge df as we go 
oda_disb <- merge(oda_disb, b1_c, by = c("country", "year"), all = TRUE)
rm(b1_a, b1_b, b1_c)
summary(oda_disb)

## "DC_TOF_HLTHNT", # Indicator 3.b.2, Series : Total official development assistance to medical research and basic health sectors, net disbursement, by recipient countries (millions of constant 2021 United States dollars) 
indicator_3.b.2 <- get_indicator_data(indicator = "3.b.2")
unique(indicator_3.b.2$series)

b2 <- indicator_3.b.2 %>%
  filter(geoAreaName %in% countries,
         series == "DC_TOF_HLTHL",
         #dimensions$`Substance use disorders` == "TOTAL",
         #dimensions$Age == "15-19",
         #dimensions$Sex == "BOTHSEX",
         timePeriodStart %in% year
  ) %>%
  select(geoAreaName, timePeriodStart, value) %>% 
  mutate(across(c(value, timePeriodStart), as.numeric),  # Treat value and timePeriodStart as numbers
         value = round(value, 3)) %>%
  rename(HealthResearch_ODA = value, country = geoAreaName, year = timePeriodStart)


## Merge df as we go 
oda_disb <- merge(oda_disb, b2, by = c("country", "year"), all = TRUE)
rm(b2)
summary(oda_disb)


## "SH_HLF_EMED", # Indicator 3.b.3, Series : Proportion of health facilities that have a core set of relevant essential medicines available and affordable on a sustainable basis (%) 
indicator_3.b.3 <- get_indicator_data(indicator = "3.b.3")

unique(indicator_3.b.3$dimensions)

b3 <- indicator_3.b.3 %>%
  filter(geoAreaName %in% countries,
         series == "SH_HLF_EMED",
         #dimensions$`Substance use disorders` == "TOTAL",
         #dimensions$Age == "15-19",
         #dimensions$Sex == "BOTHSEX",
         timePeriodStart %in% year
  ) %>%
  select(geoAreaName, timePeriodStart, value) %>% 
  mutate(across(c(value, timePeriodStart), as.numeric),  # Treat value and timePeriodStart as numbers
         value = round(value, 3)) %>%
  rename(Healthfacil_adeq = value, country = geoAreaName, year = timePeriodStart)

oda_disb_1 <- oda_disb

## Merge df as we go 
oda_disb <- merge(oda_disb, b, by = c("country", "year"), all = TRUE)
rm(b3)
summary(oda_disb)



## "SH_MED_DEN", # Indicator 3.c.1, Series : Health worker density, by type of occupation (per 10,000 population) 
indicator_3.c.1 <- get_indicator_data(indicator = "3.c.1")
unique(indicator_3.c.1$dimensions$`Type of occupation`)

c1_Nurse <- indicator_3.c.1 %>%
  filter(geoAreaName %in% countries,
         series == "SH_MED_DEN",
         dimensions$`Type of occupation` == "NURS",
         #dimensions$Age == "15-19",
         #dimensions$Sex == "BOTHSEX",
         timePeriodStart %in% year
  ) %>%
  select(geoAreaName, timePeriodStart, value) %>% 
  mutate(across(c(value, timePeriodStart), as.numeric),  # Treat value and timePeriodStart as numbers
         value = round(value, 3)) %>%
  rename(HthWk_Nurse_dens = value, country = geoAreaName, year = timePeriodStart)


oda_disb_1 <- oda_disb

## Merge df as we go 
oda_disb <- merge(oda_disb, c1_midwifes, by = c("country", "year"), all = TRUE)
rm(c1_Phar, c1_Physician, c1_midwifes, c1_Nurse)
summary(oda_disb)


## "SH_IHR_CAPS", # Indicator 3.d.1, Series : International Health Regulations (IHR) capacity, by type of IHR capacity (%) 
indicator_3.d.1 <- get_indicator_data(indicator = "3.d.1")
#"SH_BLD_MRSA", # Indicator 3.d.2, Series : Percentage of bloodstream infection due to methicillin-resistant Staphylococcus aureus (MRSA) among patients seeking care and whose blood sample is taken and tested (%)
indicator_3.d.2 <- get_indicator_data(indicator = "3.d.2")

unique(indicator_3.d.1$dimensions$`IHR Capacity`)

d1 <- indicator_3.c.1 %>%
  filter(geoAreaName %in% countries,
         #series == "SH_IHR_CAPS",
         #dimensions$`Type of occupation` == "NURS",
         #dimensions$Age == "15-19",
         #dimensions$Sex == "BOTHSEX",
         timePeriodStart %in% year
  ) %>%
  select(geoAreaName, timePeriodStart, value) %>% 
  mutate(across(c(value, timePeriodStart), as.numeric),  # Treat value and timePeriodStart as numbers
         value = round(value, 3)) %>%
  rename(Int_Health_Regs = value, country = geoAreaName, year = timePeriodStart)


## Merge df as we go 
oda_disb <- merge(oda_disb, c1_midwifes, by = c("country", "year"), all = TRUE)
rm(c1_Phar, c1_Physician, c1_midwifes, c1_Nurse)
summary(oda_disb)


## "SN_ITK_DEFC", # Indicator 2.1.1, Series : Prevalence of undernourishment (%)
indicator_2.1.1 <- get_indicator_data(indicator = "2.1.1")
unique(indicator_2.1.1$series)

eighteen <- indicator_2.1.1 %>%
  filter(geoAreaName %in% countries,
         series == "SN_ITK_DEFC",
         #dimensions$`Type of occupation` == "NURS",
         #dimensions$Age == "15-19",
         #dimensions$Sex == "BOTHSEX",
         timePeriodStart %in% year
  ) %>%
  select(geoAreaName, timePeriodStart, value) %>% 
  mutate(across(c(value, timePeriodStart), as.numeric),  # Treat value and timePeriodStart as numbers
         value = round(value, 3)) %>%
  rename(UnderNoursh_inciden = value, country = geoAreaName, year = timePeriodStart)

oda_disb_1 <- oda_disb

## Merge df as we go 
oda_disb <- merge(oda_disb, eighteen, by = c("country", "year"), all = TRUE)
rm(eighteen)
summary(oda_disb)


## "SH_STA_ANEM", # Indicator 2.2.3, Series : Proportion of women aged 15-49 years with anaemia (%) 
## "SH_STA_STNT", # Indicator 2.2.1, Series : Proportion of children moderately or severely stunted (%)

unique(indicator_2.2.1$dimensions)

eighteen <- indicator_2.2.1 %>%
  filter(geoAreaName %in% countries,
         series == "SH_STA_STNT",
         #dimensions$Location == "ALLAREA",
         #dimensions$Age == "ALLAGE",
         #dimensions$Sex == "BOTHSEX",
         timePeriodStart %in% year
  ) %>%
  select(geoAreaName, timePeriodStart, value) %>% 
  mutate(across(c(value, timePeriodStart), as.numeric),  # Treat value and timePeriodStart as numbers
         value = round(value, 2)) %>%
  rename(Child_less5_stunted = value, country = geoAreaName, year = timePeriodStart)

oda_disb_1 <- oda_disb

## Merge df as we go 
oda_disb <- merge(oda_disb, eighteen, by = c("country", "year"), all = TRUE)
rm(eighteen)
summary(oda_disb)


# indicator_2.2.2 <- get_indicator_data(indicator = "2.2.2")
# "SH_STA_BRTC", # Indicator 3.1.2, Series : Proportion of births attended by skilled health personnel (%)





### Imported as supplement from World Bank 
# "SN.ITK.DEFC.ZS", # 2.1   Incidence of undernourishment in pop % of population
# "SH.STA.STNT.ZS", # 2.2   Incidence of undernourishment among under 5 children (Stunting children)  % of children under 5
# "SP.DYN.LE00.IN", # Life expectancy at birth, total (years)	

WB_countrycode <- unique(oda_disb$iso3c) # World Bank prefers iso3c code

Supplement_inds_WB <- WDI(country = WB_countrycode, indicator = c("SN.ITK.DEFC.ZS", "SH.STA.STNT.ZS", "SP.DYN.LE00.IN"), 
                           start = yearWB[1], 
                           end = yearWB[length(yearWB)])

Supplement_inds_WB <- Supplement_inds_WB %>%
  rename(undernoursh_pop = SN.ITK.DEFC.ZS, undernoursh_under5 = SH.STA.STNT.ZS, Life_expectancy = SP.DYN.LE00.IN) %>%
  select(iso3c, year, undernoursh_pop, undernoursh_under5, Life_expectancy)

## Merge df as we go 
oda_disb <- merge(oda_disb, Supplement_inds_WB, by = c("iso3c", "year"), all = TRUE)
summary(oda_disb)


## "SH.DYN.MORT", # 3.2.1 Under 5 mortality rate 	per 1,000 live births
## The indicators is not accessible from UN stat, resorting to WB

under5Mort <- WDI(country = WBcountries, indicator = "SH.DYN.MORT", 
                          start = yearWB[1], 
                          end = yearWB[length(yearWB)])

Under5Mort <- under5Mort %>%
  rename(Under5_Mort = SH.DYN.MORT) %>%
  select(iso3c, year, Under5_Mort)
oda_disb_1 <- oda_disb
## Merge df as we go 
oda_disb <- merge(oda_disb, Under5Mort, by = c("iso3c", "year"), all = TRUE)
summary(oda_disb)



########## All these indicators have too many missing values. WB database will be explored #####

# "SH.UHC.OOPC.25.TO", # 3.8.2 Rate of High OOP (>10 or >25% of HHs income) Number of pop
# "SH.STA.POIS.P5", # 3.9.3 Mortality rate from unintentional poisoning per 100,000 population)		
# "SH.STA.WASH.P5", # 3.9.2 Mortality rate from unsafe water, sanitation or hygiene per 100,000 population
# "SH.STA.AIRP.P5", # 3.9.1 Mortality rate caused by air pollution per 100,000 population		
# "SP.ADO.TFRT", # 3.7.2 Adolescent birth rate per 1000 women births per 1,000 women ages 15-19
# "SH.FPL.SATM.ZS", # 3.7.1 Demand for family planning satisfied % of married women with demand for family planning
# "SH.STA.TRAF.P5", # 3.6.1 Death rate from road traffic injuries per 100,000 population
# "SH.DYN.AIDS.ZS", # 3.3.1 New HIV cases per 1000 uninfected population % of population ages 15-49) 
# "SH.STA.BRTC.ZS", # 3.1.2 Birth attended by skilled health personnel	 % of total 
# "SH.ALC.PCAP.LI", # 3.5.2 Alcohol per capital consumption 	15+ years of age
# "SH.STA.SUIC.P5", # 3.4.2 Suicide morality rate per 100,000 population)	
summary(Vars_missing_values)
Vars_missing_values <- WDI(country = WBcountries, 
                  indicator = c("SH.UHC.OOPC.25.TO", "SH.STA.POIS.P5", "SH.STA.WASH.P5", 
                                "SH.STA.AIRP.P5", "SP.ADO.TFRT", "SH.FPL.SATM.ZS", 
                                "SH.STA.TRAF.P5", "SH.DYN.AIDS.ZS", "SH.STA.BRTC.ZS", 
                                "SH.ALC.PCAP.LI", "SH.STA.SUIC.P5"),
                  start = yearWB[1], 
                  end = yearWB[length(yearWB)])

Vars_missing_values <- Vars_missing_values %>%
  rename(OOP_25perc_WB = SH.UHC.OOPC.25.TO, Uninten_poison_MortWB = SH.STA.POIS.P5, Unsafe_water_mortWB = SH.STA.WASH.P5, 
         Airpol_MortWB = SH.STA.AIRP.P5, Adol_birth15to19WB = SP.ADO.TFRT, Family_planningWB = SH.FPL.SATM.ZS, 
         Road_traff_MortWB = SH.STA.TRAF.P5, NewHIV_casesWB = SH.DYN.AIDS.ZS, Birth_SkilledWorkWB = SH.STA.BRTC.ZS, 
         ALcohol_percapWB = SH.ALC.PCAP.LI, Suicide_MortWB = SH.STA.SUIC.P5) %>%
  select(iso3c, year, OOP_25perc_WB, Uninten_poison_MortWB, Unsafe_water_mortWB, Airpol_MortWB, Adol_birth15to19WB, 
         Family_planningWB, Road_traff_MortWB, NewHIV_casesWB, Birth_SkilledWorkWB, ALcohol_percapWB, Suicide_MortWB)
oda_disb_1 <- oda_disb
## Merge df as we go 
oda_disb <- merge(oda_disb, Vars_missing_values, by = c("iso3c", "year"), all = TRUE)
summary(oda_disb)

summary(oda_disb)
## We removed some variables
oda_disb <- oda_disb %>%
  select(-c(birthToPersonel, Infant_Mort, HIV_incidence, Suicide_rate, Suicide_MortWB, ALcohol_percapWB, Road_traffic_mort, 
            Family_planningWB, Adolesc_pregn_15to19, OOP_25perc_WB, Airpol_MortWB, WaterSanit_Mort, Poisoining_Mort, HthWk_Pharcist_dens, Pneum_vaccine, undernoursh_under5, UnderNoursh_inciden))


write.csv2(oda_disb, "Health_with_ODA_data.csv")
write_rds(oda_disb, "Health_with_ODA_data.rds")
