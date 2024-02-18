################# EXCTRACTING DATA FOR COVARIATES ##############

#####Load basci packages ######
library(tidyverse)
library(WDI)
library(psych) # For pairs panels correlation plot 

###### Load the work-in-progress data ##########
oda_disb <- read.csv2("Raw_data/Health_with_ODA_data.csv", sep = ";")
countries <- unique(oda_disb$iso3c) # Select countries to query World Bank ASIPRE database. WB prefers iso3c code
yearWB <- 1990:2021









########### COVARIATES INDICATORS ############
Covariates_inds <- c(hlth_spdng_percap = "NY.GDP.PCAP.PP.KD", # Domestic health spending: Current health expenditure per capita
                     hlth_spdng_perGDP = "SH.XPD.CHEX.GD.ZS", # Current health expenditure (% of GDP)
                     govt_hth_spnd_1 = "SH.XPD.GHED.CH.ZS", # Domestic general government health expenditure % of current hth spending
                     govt_hth_spnd_2 = "SH.XPD.GHED.GD.ZS", # Domestic general govt health spending
                     govt_hth_sp_percap3 ="SH.XPD.GHED.PC.CD", # Domestic general government health expenditure per capita (current US$
                     Urb_pop_rate = "SP.URB.TOTL.IN.ZS",  # Urban population (% of total population)
                     UrbPop_grth_rate = "SP.URB.GROW", # Urban population growth (annual %)
                     GDP_grth_rate = "NY.GDP.MKTP.KD.ZG", # GDP Growth rate annual %
                     GDP_percap = "NY.GDP.PCAP.PP.KD", # GDP Per capita constant 2017 international $
                     Propty_right = "IQ.CPA.PROP.XQ", # 	CPIA property rights and rule-based governance rating (1=low to 6=high)
                     Pol_stabty_Est = "PV.EST", #	Political Stability and Absence of Violence/Terrorism: -2.5 to 2.5
                     Pol_stabty_Perctile = "PV.PER.RNK", #	Political Stability and Absence of Violence/Terrorism: Percentile Rank
                     Gender_violence = "SG.VAW.1549.ZS", #  Proportion of women subjected to physical and/or sexual violence in the last 12 months (% of ever-partnered women ages 15-49)
                     civil_violence = "VC.IDP.NWCV",  # Internally displaced persons, new displacement associated with conflict and violence (number of cases)
                     corr_percentile = "CC.PER.RNK",  # Control of Corruption: Percentile Rank
                     corr_estimate = "CC.EST", # Control of Corruption: Estimate -2.5 to 2.5
                     # Human_cap = "HD.HCI.OVRL", # Human capital index (0 to 1)
                     Resch_dev = "GB.XPD.RSDV.GD.ZS", # Research and development expenditure (% of GDP)
                     Capit_Forma = "NE.GDI.TOTL.KD", # Gross capital formation (constant 2015 US$)
                     Lab_force = "SL.TLF.CACT.NE.ZS",  # Labor force participation rate, total (% of total population ages 15+) (national estimate)
                     Pop_0to14 = "SP.POP.0014.TO", # Population ages 0-14, total
                     Pop_65abv =  "SP.POP.65UP.TO",  # Population ages 65 and above, total
                     Pop_tot = "SP.POP.TOTL", # Population, total
                     Pop_grwth = "SP.POP.GROW", # Population growth (annual %) 
                     Pop_densty = "EN.POP.DNST", # Population density (people per sq. km of land area)
                     Emission_tot_ghg = "EN.ATM.GHGT.KT.CE",  # Total greenhouse gas emissions (kt of CO2 equivalent)
                     Emission_co2 = "EN.ATM.CO2E.KD.GD",  # CO2 emissions (kg per 2015 US$ of GDP)
                     Electri_access = "EG.ELC.ACCS.ZS",  # Access to electricity (% of population)
                     Agric_emp = "SL.AGR.EMPL.ZS",  # Employment in agriculture (% of total employment) (modeled ILO estimate)
                     Unemply_rate = "SL.UEM.TOTL.NE.ZS", # Unemployment, total (% of total labor force) (national estimate)
                     Tech_export_rate = "TX.VAL.TECH.MF.ZS",  # High-technology exports (% of manufactured exports)
                     Female_educ = "SE.SEC.CUAT.UP.FE.ZS",  # Educational attainment, at least completed upper secondary, population 25+, female (%) (cumulative)
                     Trade = "NE.TRD.GNFS.ZS", # Trade (% of GDP)
                     FDI = "BX.KLT.DINV.WD.GD.ZS", # Foreign direct investment, net inflows (% of GDP)
                     Inflation = "FP.CPI.TOTL.ZG", # Inflation, consumer prices (annual %)
                     clean_cook_enrgy = "EG.CFT.ACCS.ZS"  # Access to clean fuels and technologies for cooking (% of population)
                     )


######## LOAD CV DATA From World Bank ########
CV_data_Wth_iso2c <- WDI(country = iso2c, indicator = Covariates_inds, 
                        start = yearWB[1], 
                        end = yearWB[length(yearWB)])
summary(CV_data_wthout_humanCap)
iso3c <- unique(CV_data$iso3c)

## Get Human Capital inds 

CV_Human_Cap <- WDI(country = iso3c, indicator = c(Human_cap = "HD.HCI.OVRL"), 
                         start = yearWB[1], 
                         end = yearWB[length(yearWB)])

view(CV_Human_Cap)
summary(CV_data_wthout_humanCap)

colnames(CV_data_wthout_humanCap)[which(colnames(CV_data_wthout_humanCap) == 'Pol_stabty_Est')[2]] <- 'Pol_stabty_Perctile' # Two vars are named the same name, rename one of them 


oda_disb <- merge(oda_disb, CV_data_wthout_humanCap, by = c("iso3c", "year"), all =TRUE)
summary(oda_disb)
summary(CV_data_wthout_humanCap)

colnames(oda_disb)
oda_disb <- oda_disb %>%
  select(!c(country, iso2c)) %>%
  rename(country = country.x)


### Save the data for covariates ###
write.csv2(CV_data_wthout_humanCap, "Covariates_data_noHumanCap.csv")
write.csv2(oda_disb, "ODA_Health_CV_data.csv") # Updated ODA_Health_CV data


########### SEGMENTING THE COVARIATES ############

CV_inds_1 <- oda_disb %>% 
select(hlth_spdng_percap, hlth_spdng_perGDP, govt_hth_spnd_1, 
       govt_hth_spnd_2, govt_hth_sp_percap3, Urb_pop_rate, 
       UrbPop_grth_rate, GDP_grth_rate, Propty_right, 
       Pol_stabty_Est)

CV_inds_2 <- c("Pol_stabty_Perctile", "Gender_violence", "civil_violence", "corr_percentile",  "corr_estimate", 
               "Resch_dev", "Capit_Forma", "Lab_force", "Pop_0to14", "Pop_65abv", "Pop_tot", "Pop_grwth")

CV_inds_3 <- c("Pop_densty", "Emission_tot_ghg", "Emission_co2", "Electri_access", "Agric_emp", 
               "Unemply_rate", "Tech_export_rate", "Female_educ", "Trade", "FDI", "Inflation", 
               "clean_cook_enrgy")

######### CORRELATION OF COVARIATES ###########
library(psych)

pairs.panels(CV_inds_1, density = TRUE, scale = TRUE, 
             stars = TRUE, breaks = 40, cex.cor = 1.5, 
             main = "COVARIATES (First Subset)")


pairs.panels(oda_disb[, CV_inds_2], density = TRUE, scale = TRUE, 
             stars = TRUE, breaks = 40, cex.cor = 1.5, 
             main = "COVARIATES (Second Subset)")


pairs.panels(oda_disb[, CV_inds_3], density = TRUE, scale = TRUE, 
             stars = TRUE, breaks = 40, cex.cor = 1.5, 
             main = "COVARIATES (First Subset)")


