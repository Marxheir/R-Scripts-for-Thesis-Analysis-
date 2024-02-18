########### HEALTH OUTCOME SEGMENTATION FOR DIMENSION REDUCTION ###############0

####### Packages needed ####
library(corrr)
library(corrplot)
library(caret)
library(psych) # For pairs panels correlation plot 
library(reshape2)
library(writexl)
library(tidyverse)


oda_disb <- read.csv2("Raw_data/Health_with_ODA_data.csv", sep = ";")

#### a. Health system capacity and responsiveness ####
HSCR_dim_inds <- c("Birth_SkilledWorkWB",  # "SH_STA_BRTC", # Indicator 3.1.2, Series : Proportion of births attended by skilled health personnel (%)
                   "Measles_vaccine",  # "SH_ACS_MCV2", # Indicator 3.b.1, Series : Proportion of the target population with access to measles-containing-vaccine second-dose (MCV2) (%) 
                    #"HealthResearch_ODA", # "DC_TOF_HLTHNT", # Indicator 3.b.2, Series : Total official development assistance to medical research and basic health sectors, net disbursement, by recipient countries (millions of constant 2021 United States dollars) 
                    "Healthfacil_adeq", # "SH_HLF_EMED", # Indicator 3.b.3, Series : Proportion of health facilities that have a core set of relevant essential medicines available and affordable on a sustainable basis (%) 
                    "HthWk_Physician_dens",  # "SH_MED_DEN", # Indicator 3.c.1, Series : Health worker density, by type of occupation (per 10,000 population) 
                    "HthWk_Midwivies_dens", 
                   "Tetanus_vaccine")

healthsystem_plots <- pairs.panels(oda_disb[, HSCR_dim_inds], density = TRUE, scale = TRUE, 
             stars = TRUE, breaks = 40, cex.cor = 1, 
             main = "Health system capacity and responsiveness")

ggplot2::ggsave("plots/healthsystem_plots.png", plot = healthsystem_plots, device = "png", width = 8, height = 9, dpi = 300
       )


#### b. Reproductive health and pre-post natal mortality ####
Reprod_health_risk <- c("Mat_Mort_ratio", # "SH_STA_MORT", # Indicator 3.1.1, Series : Maternal mortality ratio
                      "Under5_Mort",  # "SH_DYN_MORT", # Indicator 3.2.1, Series : Under-five mortality rate, by sex (deaths per 1,000 live births)
                      "Neonatal_Mort", # "SH_DYN_NMRT", # Indicator 3.2.2, Series : Neonatal mortality rate (deaths per 1,000 live births)
                      "Family_planning_cov", #  "SH_FPL_MTMM", # Indicator 3.7.1, Series : Proportion of women of reproductive age (aged 15-49 years) who have their need for family planning satisfied with modern methods (% of women aged 15-49 years)
                      "Adolesc_pregn_10to14", 
                      "Adol_birth15to19WB" #  "SP_DYN_ADKL" # Indicator 3.7.2, Series : Adolescent birth rate (per 1,000 women aged 15-19 and 10-14 years) 
)

pairs.panels(oda_disb[, Reprod_health_risk], density = TRUE, scale = TRUE, 
                                   stars = TRUE, breaks = 40, cex.cor = 1, 
                                   main = "Reproductive health risk and mortality")


#### c. Burden of Infections and diseases #####
Burden_infect_dis <- c("NewHIV_casesWB", # "SH_HIV_INCD", # Indicator 3.3.1, Series : Number of new HIV infections per 1,000 uninfected population, by sex and age (per 1,000 uninfected population)
                       "Tuberculosis_incid", # "SH_TBS_INCD", # Indicator 3.3.2, Series : Tuberculosis incidence (per 100,000 population) 
                      "Malaria_incid",  #  "SH_STA_MALR", # Indicator 3.3.3, Series : Malaria incidence per 1,000 population at risk (per 1,000 population) 
                      "hepatitis_B_under5", #  "SH_HAP_HBSAG", # Indicator 3.3.4, Series : Prevalence of hepatitis B surface antigen (HBsAg) (%) 
                      "Tropical_dis", #  "SH_TRP_INTVN", # Indicator 3.3.5, Series : Number of people requiring interventions against neglected tropical diseases (number) 
                      "Noncomm_diseas" #  "SH_DTH_NCOM" # Indicator 3.4.1, Series : Mortality rate attributed to cardiovascular disease, cancer, diabetes or chronic respiratory disease (probability)
)

pairs.panels(oda_disb[, Burden_infect_dis], density = TRUE, scale = TRUE, 
             stars = TRUE, breaks = 40, cex.cor = 1.5, 
             main = "Burden of infections and diseases")

summary(oda_disb)
tail(oda_disb_plot, 20)
#### d. Mental health and substance use ####
Mental_health <- c("Suicide_mort", # "SH_STA_SCIDE", # Indicator 3.4.2, Series : Suicide mortality rate, by sex (deaths per 100,000 population) 
                  "Alcohol_percap",  # "SH_SUD_ALCOL", # Indicator 3.5.1, Series : Alcohol use disorders, 12-month prevalence (%) "SH_ALC_CONSPT", # Indicator 3.5.2, Series : Alcohol consumption per capita (aged 15 years and older) within a calendar year (litres of pure alcohol) 
                  "Tobacco_rate"  # "SH_PRV_SMOK" # Indicator 3.a.1, Series : Age-standardized prevalence of current tobacco use among persons aged 15 years and older, by sex (%) 
)

pairs.panels(oda_disb[, Mental_health], density = TRUE, scale = TRUE, 
             stars = TRUE, breaks = 40, cex.cor = 1.5, 
             main = "Mental health")


#### e. Nutrition #####
Nutrition <- c("undernoursh_pop", # "SN_ITK_DEFC", # Indicator 2.1.1, Series : Prevalence of undernourishment (%)
               "Child_less5_stunted", # "SH_STA_STNT", # Indicator 2.2.1, Series : Proportion of children moderately or severely stunted (%)
              "Women_Anaemia" #  "SH_STA_ANEM" # Indicator 2.2.3, Series : Proportion of women aged 15-49 years with anaemia (%) 
)

pairs.panels(oda_disb[, Nutrition], density = TRUE, scale = TRUE, 
             stars = TRUE, breaks = 40, cex.cor = 1.5, 
             main = "Nutrition")
#### f. Health access and affordability ####
Health_access_affd <- c("UHC",  # "SH_ACS_UNHC", # Indicator 3.8.1, Series : Universal health coverage (UHC) service coverage index 
                        "CHS_25perc", # "SH_XPD_EARN25", # Indicator 3.8.2, Series : Proportion of population with large household expenditures on health (greater than 25%) as a share of total household expenditure or income (%) 
                        "Healthfacil_adeq" # "SH_HLF_EMED", # Indicator 3.b.3, Series : Proportion of health facilities that have a core set of relevant essential medicines available and affordable on a sustainable basis (%) 
)

pairs.panels(oda_disb[, Health_access_affd], density = TRUE, scale = TRUE, 
             stars = TRUE, breaks = 40, cex.cor = 1.5, 
             main = "Health access and affordability")


##### Environmental death and fatalities #####
Env_death <- c("Air_polut_Mort", #  "SH_HAP_ASMORT", # Indicator 3.9.1, Series : Age-standardized mortality rate attributed to household air pollution (deaths per 100,000 population) 
              "Unsafe_water_mortWB", # "SH_STA_WASHARI", # Indicator 3.9.2, Series : Mortality rate attributed to unsafe water, unsafe sanitation and lack of hygiene from diarrhoea, intestinal nematode infections, malnutrition and acute respiratory infections (deaths per 100,000 population) 
              "Uninten_poison_MortWB", #  "SH_STA_POISN", # Indicator 3.9.3, Series : Mortality rate attributed to unintentional poisonings, by sex (deaths per 100,000 population) 
              "Road_traff_MortWB" #  "SH_STA_TRAF" # Indicator 3.6.1, Series : Death rate due to road traffic injuries, by sex (per 100,000 population)
)


pairs.panels(oda_disb[, Env_death], density = TRUE, scale = TRUE, 
             stars = TRUE, breaks = 40, cex.cor = 1.5, 
             main = "Environment related Mortality")


colnames(oda_disb)
health_Outcomes_inds <- c("Birth_SkilledWorkWB",  # "SH_STA_BRTC", # Indicator 3.1.2, Series : Proportion of births attended by skilled health personnel (%)
                          "Measles_vaccine",  # "SH_ACS_MCV2", # Indicator 3.b.1, Series : Proportion of the target population with access to measles-containing-vaccine second-dose (MCV2) (%) 
                          "HealthResearch_ODA", # "DC_TOF_HLTHNT", # Indicator 3.b.2, Series : Total official development assistance to medical research and basic health sectors, net disbursement, by recipient countries (millions of constant 2021 United States dollars) 
                          "Healthfacil_adeq", # "SH_HLF_EMED", # Indicator 3.b.3, Series : Proportion of health facilities that have a core set of relevant essential medicines available and affordable on a sustainable basis (%) 
                          "HthWk_Physician_dens",  # "SH_MED_DEN", # Indicator 3.c.1, Series : Health worker density, by type of occupation (per 10,000 population) 
                          "HthWk_Midwivies_dens", 
                          "Tetanus_vaccine", 
                          "Mat_Mort_ratio", # "SH_STA_MORT", # Indicator 3.1.1, Series : Maternal mortality ratio
                          "Under5_Mort",  # "SH_DYN_MORT", # Indicator 3.2.1, Series : Under-five mortality rate, by sex (deaths per 1,000 live births)
                          "Neonatal_Mort", # "SH_DYN_NMRT", # Indicator 3.2.2, Series : Neonatal mortality rate (deaths per 1,000 live births)
                          "Family_planning_cov", #  "SH_FPL_MTMM", # Indicator 3.7.1, Series : Proportion of women of reproductive age (aged 15-49 years) who have their need for family planning satisfied with modern methods (% of women aged 15-49 years)
                          "Adolesc_pregn_10to14", 
                          "Adol_birth15to19WB", #  "SP_DYN_ADKL" # Indicator 3.7.2, Series : Adolescent birth rate (per 1,000 women aged 15-19 and 10-14 years) 
                          "NewHIV_casesWB", # "SH_HIV_INCD", # Indicator 3.3.1, Series : Number of new HIV infections per 1,000 uninfected population, by sex and age (per 1,000 uninfected population)
                          "Tuberculosis_incid", # "SH_TBS_INCD", # Indicator 3.3.2, Series : Tuberculosis incidence (per 100,000 population) 
                          "Malaria_incid",  #  "SH_STA_MALR", # Indicator 3.3.3, Series : Malaria incidence per 1,000 population at risk (per 1,000 population) 
                          "hepatitis_B_under5", #  "SH_HAP_HBSAG", # Indicator 3.3.4, Series : Prevalence of hepatitis B surface antigen (HBsAg) (%) 
                          "Tropical_dis", #  "SH_TRP_INTVN", # Indicator 3.3.5, Series : Number of people requiring interventions against neglected tropical diseases (number) 
                          "Noncomm_diseas", #  "SH_DTH_NCOM" # Indicator 3.4.1, Series : Mortality rate attributed to cardiovascular disease, cancer, diabetes or chronic respiratory disease (probability)
                          "Suicide_mort", # "SH_STA_SCIDE", # Indicator 3.4.2, Series : Suicide mortality rate, by sex (deaths per 100,000 population) 
                          "Alcohol_percap",  # "SH_SUD_ALCOL", # Indicator 3.5.1, Series : Alcohol use disorders, 12-month prevalence (%) "SH_ALC_CONSPT", # Indicator 3.5.2, Series : Alcohol consumption per capita (aged 15 years and older) within a calendar year (litres of pure alcohol) 
                          "Tobacco_rate", 
                          "undernoursh_pop", # "SN_ITK_DEFC", # Indicator 2.1.1, Series : Prevalence of undernourishment (%)
                          "Child_less5_stunted", # "SH_STA_STNT", # Indicator 2.2.1, Series : Proportion of children moderately or severely stunted (%)
                          "Women_Anaemia", 
                          "UHC",  # "SH_ACS_UNHC", # Indicator 3.8.1, Series : Universal health coverage (UHC) service coverage index 
                          "CHS_25perc", # "SH_XPD_EARN25", # Indicator 3.8.2, Series : Proportion of population with large household expenditures on health (greater than 25%) as a share of total household expenditure or income (%) 
                          "Air_polut_Mort", #  "SH_HAP_ASMORT", # Indicator 3.9.1, Series : Age-standardized mortality rate attributed to household air pollution (deaths per 100,000 population) 
                          "Unsafe_water_mortWB", # "SH_STA_WASHARI", # Indicator 3.9.2, Series : Mortality rate attributed to unsafe water, unsafe sanitation and lack of hygiene from diarrhoea, intestinal nematode infections, malnutrition and acute respiratory infections (deaths per 100,000 population) 
                          "Uninten_poison_MortWB", #  "SH_STA_POISN", # Indicator 3.9.3, Series : Mortality rate attributed to unintentional poisonings, by sex (deaths per 100,000 population) 
                          "Road_traff_MortWB" #  "SH_STA_TRAF" # Indicator 3.6.1, Series : Death rate due to road traffic injuries, by sex (per 100,000 population)
)

health_Outcomes_inds_1 <- c("Birth_SkilledWorkWB",  # "SH_STA_BRTC", # Indicator 3.1.2, Series : Proportion of births attended by skilled health personnel (%)
                          "Measles_vaccine",  # "SH_ACS_MCV2", # Indicator 3.b.1, Series : Proportion of the target population with access to measles-containing-vaccine second-dose (MCV2) (%) 
                          "HealthResearch_ODA", # "DC_TOF_HLTHNT", # Indicator 3.b.2, Series : Total official development assistance to medical research and basic health sectors, net disbursement, by recipient countries (millions of constant 2021 United States dollars) 
                          "Healthfacil_adeq", # "SH_HLF_EMED", # Indicator 3.b.3, Series : Proportion of health facilities that have a core set of relevant essential medicines available and affordable on a sustainable basis (%) 
                          "HthWk_Physician_dens",  # "SH_MED_DEN", # Indicator 3.c.1, Series : Health worker density, by type of occupation (per 10,000 population) 
                          "HthWk_Midwivies_dens", 
                          "Tetanus_vaccine", 
                          "Mat_Mort_ratio", # "SH_STA_MORT", # Indicator 3.1.1, Series : Maternal mortality ratio
                          "Under5_Mort",  # "SH_DYN_MORT", # Indicator 3.2.1, Series : Under-five mortality rate, by sex (deaths per 1,000 live births)
                          "Neonatal_Mort", # "SH_DYN_NMRT", # Indicator 3.2.2, Series : Neonatal mortality rate (deaths per 1,000 live births)
                          "Family_planning_cov", #  "SH_FPL_MTMM", # Indicator 3.7.1, Series : Proportion of women of reproductive age (aged 15-49 years) who have their need for family planning satisfied with modern methods (% of women aged 15-49 years)
                          "Adolesc_pregn_10to14")

health_Outcomes_inds_2 <- c("Adol_birth15to19WB", #  "SP_DYN_ADKL" # Indicator 3.7.2, Series : Adolescent birth rate (per 1,000 women aged 15-19 and 10-14 years) 
                          "NewHIV_casesWB", # "SH_HIV_INCD", # Indicator 3.3.1, Series : Number of new HIV infections per 1,000 uninfected population, by sex and age (per 1,000 uninfected population)
                          "Tuberculosis_incid", # "SH_TBS_INCD", # Indicator 3.3.2, Series : Tuberculosis incidence (per 100,000 population) 
                          "Malaria_incid",  #  "SH_STA_MALR", # Indicator 3.3.3, Series : Malaria incidence per 1,000 population at risk (per 1,000 population) 
                          "hepatitis_B_under5", #  "SH_HAP_HBSAG", # Indicator 3.3.4, Series : Prevalence of hepatitis B surface antigen (HBsAg) (%) 
                          "Tropical_dis", #  "SH_TRP_INTVN", # Indicator 3.3.5, Series : Number of people requiring interventions against neglected tropical diseases (number) 
                          "Noncomm_diseas", #  "SH_DTH_NCOM" # Indicator 3.4.1, Series : Mortality rate attributed to cardiovascular disease, cancer, diabetes or chronic respiratory disease (probability)
                          "Suicide_mort", # "SH_STA_SCIDE", # Indicator 3.4.2, Series : Suicide mortality rate, by sex (deaths per 100,000 population) 
                          "Alcohol_percap",  # "SH_SUD_ALCOL", # Indicator 3.5.1, Series : Alcohol use disorders, 12-month prevalence (%) "SH_ALC_CONSPT", # Indicator 3.5.2, Series : Alcohol consumption per capita (aged 15 years and older) within a calendar year (litres of pure alcohol) 
                          "Tobacco_rate")

health_Outcomes_inds_3 <- c("undernoursh_pop", # "SN_ITK_DEFC", # Indicator 2.1.1, Series : Prevalence of undernourishment (%)
                          "Child_less5_stunted", # "SH_STA_STNT", # Indicator 2.2.1, Series : Proportion of children moderately or severely stunted (%)
                          "Women_Anaemia", 
                          "UHC",  # "SH_ACS_UNHC", # Indicator 3.8.1, Series : Universal health coverage (UHC) service coverage index 
                          "CHS_25perc", # "SH_XPD_EARN25", # Indicator 3.8.2, Series : Proportion of population with large household expenditures on health (greater than 25%) as a share of total household expenditure or income (%) 
                          "Healthfacil_adeq", # "SH_HLF_EMED", # Indicator 3.b.3, Series : Proportion of health facilities that have a core set of relevant essential medicines available and affordable on a sustainable basis (%) 
                          "Air_polut_Mort", #  "SH_HAP_ASMORT", # Indicator 3.9.1, Series : Age-standardized mortality rate attributed to household air pollution (deaths per 100,000 population) 
                          "Unsafe_water_mortWB", # "SH_STA_WASHARI", # Indicator 3.9.2, Series : Mortality rate attributed to unsafe water, unsafe sanitation and lack of hygiene from diarrhoea, intestinal nematode infections, malnutrition and acute respiratory infections (deaths per 100,000 population) 
                          "Uninten_poison_MortWB", #  "SH_STA_POISN", # Indicator 3.9.3, Series : Mortality rate attributed to unintentional poisonings, by sex (deaths per 100,000 population) 
                          "Road_traff_MortWB" #  "SH_STA_TRAF" # Indicator 3.6.1, Series : Death rate due to road traffic injuries, by sex (per 100,000 population)
)

pairs.panels(oda_disb[, health_Outcomes_inds_3], density = TRUE, scale = TRUE, 
             stars = TRUE, breaks = 40, cex.cor = 2, 
             main = "All health outcome indicators (Third Subset)")


