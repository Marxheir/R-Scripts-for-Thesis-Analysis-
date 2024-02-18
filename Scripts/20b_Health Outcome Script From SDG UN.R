############## SCRIPT FOR HEALTH OUTCOME DATASET ##############
library(tidyverse)
install.packages("devtools")
library(devtools)
devtools::install_github("DrMattG/SDGsR", dependencies = TRUE)
library(SDGsR)


indicator_3.3.1 <- get_indicator_data(indicator = "3.3.1")
indicator_3.3.2 <- get_indicator_data(indicator = "3.3.2")
indicator_3.3.3 <- get_indicator_data(indicator = "3.3.3")
indicator_3.3.4 <- get_indicator_data(indicator = "3.3.4")
indicator_3.3.4 <- get_indicator_data(indicator = "3.3.4")
indicator_3.3.5 <- get_indicator_data(indicator = "3.3.5")
indicator_3.4.1 <- get_indicator_data(indicator = "3.4.1")
indicator_3.4.2 <- get_indicator_data(indicator = "3.4.2")
indicator_3.5.1 <- get_indicator_data(indicator = "3.5.1")
indicator_3.5.2 <- get_indicator_data(indicator = "3.5.2")
indicator_3.6.1 <- get_indicator_data(indicator = "3.6.1")
indicator_3.7.1 <- get_indicator_data(indicator = "3.7.1")
indicator_3.7.2 <- get_indicator_data(indicator = "3.7.2")
indicator_3.8.1 <- get_indicator_data(indicator = "3.8.1")
indicator_3.8.2 <- get_indicator_data(indicator = "3.8.2")
indicator_3.9.1 <- get_indicator_data(indicator = "3.9.1")
indicator_3.9.2 <- get_indicator_data(indicator = "3.9.2")
indicator_3.9.3 <- get_indicator_data(indicator = "3.9.3")
indicator_3.a.1 <- get_indicator_data(indicator = "3.a.1")
indicator_3.b.1 <- get_indicator_data(indicator = "3.b.1")
indicator_3.b.2 <- get_indicator_data(indicator = "3.b.2")
indicator_3.b.3 <- get_indicator_data(indicator = "3.b.3")
indicator_3.c.1 <- get_indicator_data(indicator = "3.c.1")
indicator_3.d.1 <- get_indicator_data(indicator = "3.d.1")
indicator_2.1.1 <- get_indicator_data(indicator = "2.1.1")
indicator_2.1.2 <- get_indicator_data(indicator = "2.1.2")
indicator_2.2.1 <- get_indicator_data(indicator = "2.2.1")
indicator_2.2.2 <- get_indicator_data(indicator = "2.2.2")
indicator_2.2.3 <- get_indicator_data(indicator = "2.2.3")

#"SH_STA_ANEM", # Indicator 2.2.3, Series : Proportion of women aged 15-49 years with anaemia (%) 

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

UNSDG_HO_outcomes <- c("SH_STA_MORT", # Indicator 3.1.1, Series : Maternal mortality ratio
                 "SH_STA_BRTC", # Indicator 3.1.2, Series : Proportion of births attended by skilled health personnel (%)
                 "SH_DYN_MORT", # Indicator 3.2.1, Series : Under-five mortality rate, by sex (deaths per 1,000 live births)
                 "SH_DYN_IMRT", # Indicator 3.2.1, Series : Infant mortality rate (deaths per 1,000 live births) **
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


###### SEGMENTAION OF HEALTH OUTCOME INTO DIMENSIONS ############

#### a. Health system capacity and responsiveness ####
HSCR_dim_inds <- c("SH_STA_BRTC", # Indicator 3.1.2, Series : Proportion of births attended by skilled health personnel (%)
                   "SH_ACS_MCV2", # Indicator 3.b.1, Series : Proportion of the target population with access to measles-containing-vaccine second-dose (MCV2) (%) 
                   "SH_ACS_PCV3", # Indicator 3.b.1, Series : Proportion of the target population with access to pneumococcal conjugate 3rd dose (PCV3) (%) 
                   "SH_ACS_HPV", # Indicator 3.b.1, Series : Proportion of the target population with access to affordable medicines and vaccines on a sustainable basis, human papillomavirus (HPV) (%) 
                   "DC_TOF_HLTHNT", # Indicator 3.b.2, Series : Total official development assistance to medical research and basic health sectors, net disbursement, by recipient countries (millions of constant 2021 United States dollars) 
                   "SH_HLF_EMED", # Indicator 3.b.3, Series : Proportion of health facilities that have a core set of relevant essential medicines available and affordable on a sustainable basis (%) 
                   "SH_MED_DEN", # Indicator 3.c.1, Series : Health worker density, by type of occupation (per 10,000 population) 
                   "SH_IHR_CAPS" # Indicator 3.d.1, Series : International Health Regulations (IHR) capacity, by type of IHR capacity (%)
)

#### b. Reproductive health and pre-post natal mortality ####
Reprod_natal_Mort <- c("SH_STA_MORT", # Indicator 3.1.1, Series : Maternal mortality ratio
               "SH_DYN_MORT", # Indicator 3.2.1, Series : Under-five mortality rate, by sex (deaths per 1,000 live births)
               "SH_DYN_IMRT", # Indicator 3.2.1, Series : Infant mortality rate (deaths per 1,000 live births)
               "SH_DYN_NMRT", # Indicator 3.2.2, Series : Neonatal mortality rate (deaths per 1,000 live births)
               "SH_FPL_MTMM", # Indicator 3.7.1, Series : Proportion of women of reproductive age (aged 15-49 years) who have their need for family planning satisfied with modern methods (% of women aged 15-49 years)
               "SP_DYN_ADKL" # Indicator 3.7.2, Series : Adolescent birth rate (per 1,000 women aged 15-19 and 10-14 years) 
)
               


#### c. Burden of Infections and diseases #####
Burden_infect_dis <- c("SH_HIV_INCD", # Indicator 3.3.1, Series : Number of new HIV infections per 1,000 uninfected population, by sex and age (per 1,000 uninfected population)
                       "SH_TBS_INCD", # Indicator 3.3.2, Series : Tuberculosis incidence (per 100,000 population) 
                       "SH_STA_MALR", # Indicator 3.3.3, Series : Malaria incidence per 1,000 population at risk (per 1,000 population) 
                       "SH_HAP_HBSAG", # Indicator 3.3.4, Series : Prevalence of hepatitis B surface antigen (HBsAg) (%) 
                       "SH_TRP_INTVN", # Indicator 3.3.5, Series : Number of people requiring interventions against neglected tropical diseases (number) 
                       "SH_BLD_MRSA", # Indicator 3.d.2, Series : Percentage of bloodstream infection due to methicillin-resistant Staphylococcus aureus (MRSA) among patients seeking care and whose blood sample is taken and tested 
                       "SH_DTH_NCOM" # Indicator 3.4.1, Series : Mortality rate attributed to cardiovascular disease, cancer, diabetes or chronic respiratory disease (probability)
                       )


#### d. Mental health and substance use ####
Mental_health <- c("SH_STA_SCIDE", # Indicator 3.4.2, Series : Suicide mortality rate, by sex (deaths per 100,000 population) 
                   "SH_SUD_ALCOL", # Indicator 3.5.1, Series : Alcohol use disorders, 12-month prevalence (%)
                   "SH_SUD_TREAT", # Indicator 3.5.1, Series : Coverage of treatment interventions (pharmacological, psychosocial and rehabilitation and aftercare services) for substance use disorders (%) 
                   "SH_ALC_CONSPT", # Indicator 3.5.2, Series : Alcohol consumption per capita (aged 15 years and older) within a calendar year (litres of pure alcohol) 
                   "SH_PRV_SMOK" # Indicator 3.a.1, Series : Age-standardized prevalence of current tobacco use among persons aged 15 years and older, by sex (%) 
                   )

#### e. Nutrition #####
Nutrition <- c("SN_ITK_DEFC", # Indicator 2.1.1, Series : Prevalence of undernourishment (%)
               "AG_PRD_FIESS", # Indicator 2.1.2, Series : Prevalence of severe food insecurity (%)  
               "SH_STA_STNT", # Indicator 2.2.1, Series : Proportion of children moderately or severely stunted (%)
               "SH_STA_WAST", # Indicator 2.2.2, Series : Proportion of children moderately or severely wasted (%)
               "SH_STA_ANEM" # Indicator 2.2.3, Series : Proportion of women aged 15-49 years with anaemia (%) 
               )

#### f. Health access and affordability ####
Health_access_affd <- c("SH_ACS_UNHC", # Indicator 3.8.1, Series : Universal health coverage (UHC) service coverage index 
                        "SH_XPD_EARN25", # Indicator 3.8.2, Series : Proportion of population with large household expenditures on health (greater than 25%) as a share of total household expenditure or income (%) 
                        "SH_HLF_EMED", # Indicator 3.b.3, Series : Proportion of health facilities that have a core set of relevant essential medicines available and affordable on a sustainable basis (%) 
                        )


##### Environmental death and fatalities #####
Env_death <- c("SH_HAP_ASMORT", # Indicator 3.9.1, Series : Age-standardized mortality rate attributed to household air pollution (deaths per 100,000 population) 
               "SH_STA_ASAIRP", # Indicator 3.9.1, Series : Age-standardized mortality rate attributed to household and ambient air pollution (deaths per 100,000 population) 
               "SH_AAP_ASMORT", # Indicator 3.9.1, Series : Age-standardized mortality rate attributed to ambient air pollution (deaths per 100,000 population) 
               "SH_STA_WASHARI", # Indicator 3.9.2, Series : Mortality rate attributed to unsafe water, unsafe sanitation and lack of hygiene from diarrhoea, intestinal nematode infections, malnutrition and acute respiratory infections (deaths per 100,000 population) 
               "SH_STA_POISN", # Indicator 3.9.3, Series : Mortality rate attributed to unintentional poisonings, by sex (deaths per 100,000 population) 
               "SH_STA_TRAF" # Indicator 3.6.1, Series : Death rate due to road traffic injuries, by sex (per 100,000 population)
)







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

"SH.UHC.OOPC.25.TO", # 3.8.2 Rate of High OOP (>10 or >25% of HHs income) Number of pop
"SH.STA.POIS.P5", # 3.9.3 Mortality rate from unintentional poisoning per 100,000 population)		
"SH.STA.WASH.P5", # 3.9.2 Mortality rate from unsafe water, sanitation or hygiene per 100,000 population
"SH.STA.AIRP.P5", # 3.9.1 Mortality rate caused by air pollution per 100,000 population		
"SP.ADO.TFRT", # 3.7.2 Adolescent birth rate per 1000 women births per 1,000 women ages 15-19
"SH.FPL.SATM.ZS", # 3.7.1 Demand for family planning satisfied % of married women with demand for family planning
"SH.STA.TRAF.P5", # 3.6.1 Death rate from road traffic injuries per 100,000 population
"SH.DYN.AIDS.ZS", # 3.3.1 New HIV cases per 1000 uninfected population % of population ages 15-49) 
"SH.STA.BRTC.ZS", # 3.1.2 Birth attended by skilled health personnel	 % of total 
"SH.ALC.PCAP.LI", # 3.5.2 Alcohol per capital consumption 	15+ years of age
"SH.STA.SUIC.P5", # 3.4.2 Suicide morality rate per 100,000 population)	 
