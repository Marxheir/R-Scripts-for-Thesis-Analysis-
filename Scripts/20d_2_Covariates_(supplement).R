########### COVARIATES SUPPLEMENTS ###########
library(tidyverse)
library(WDI)

###### Load the work-in-progress data ##########
oda_disb <- read.csv2("Raw_data/ODA_Health_CV_data", sep = ";")
countries <- unique(MisF_imp_clean_thesis_data_Indices$iso3c) # Select countries to query World Bank ASIPRE database. WB prefers iso3c code
yearWB <- 2000:2022



supple_covariates_inds <- c(GDP_per_cap = "NY.GDP.PCAP.PP.KD", # GDP per capita, PPP (constant 2017 international $)
                            Gross_FixCap = "NE.GDI.FTOT.ZS", # Gross fixed capital formation (% of GDP)
                            #"HD.HCI.OVRL", # Human capital index (HCI) (scale 0-1)
                            Ext_debt_GNI = "DT.DOD.PVLX.GN.ZS", # Present value of external debt (% of GNI)
                            Sec_Educ = "SE.SEC.CUAT.UP.ZS", # Educational attainment, at least completed upper secondary, population 25+, total (%) (cumulative)
                            GNI = "NY.GNP.MKTP.KD" # GNI (constant 2015 US$)
)

CV_supple <- WDI(country = countries, indicator = supple_covariates_inds, 
                         start = yearWB[1], 
                         end = yearWB[length(yearWB)])

summary(CV_supple)
iso2 <-unique(CV_supple$iso2c)

oda_disb <- merge(oda_disb, CV_supple, by = c("iso3c", "year"), all =TRUE)

oda_disb_1 <- oda_disb
oda_disb <- oda_disb_1

summary(oda_disb)

oda_disb <- oda_disb %>%
  select(!c(X, country.y, iso2c)) %>%
  rename(country = country.x)

##### SECOND SET OF SUPPLEMENT ########
secnd_sup_covariates_inds <- c(Total_debt_Service = "DT.TDS.DECT.GN.ZS", # Total debt service (% of GNI)
                            PV_exter_debt = "DT.DOD.PVLX.GN.ZS",  # Present value of external debt (% of GNI)()
                           External_debt_stock = "DT.DOD.DECT.GN.ZS", # External debt stocks (% of GNI)()
                          Water_access = "SH.H2O.BASW.ZS",  # People using at least basic drinking water services (% of population)()
 Sanitaion_access = "SH.STA.BASS.ZS" # People using at least basic sanitation services (% of population)()
)
CV_supple <- WDI(country = countries, indicator = secnd_sup_covariates_inds, 
                 start = yearWB[1], 
                 end = yearWB[length(yearWB)])
library(tidyverse)
CV_supple_sele <- CV_supple %>%
  select(iso3c, year, Total_debt_Service, External_debt_stock, Water_access, Sanitaion_access)

MisF_imp_clean_thesis_data_1 <- MisF_imp_clean_thesis_data


MisF_imp_clean_thesis_data <- merge(MisF_imp_clean_thesis_data, CV_supple_sele, by = c("iso3c", "year"), all =TRUE)
MisF_imp_clean_thesis_data_Indices <- merge(MisF_imp_clean_thesis_data_Indices, CV_supple_sele, by = c("iso3c", "year"), all =TRUE)
supp_inds <- c("Total_debt_Service", "External_debt_stock", "Water_access", "Sanitaion_access")

Missforest_imput_suppl <- missForest::missForest(MisF_imp_clean_thesis_data[, supp_inds])

summary(clean_thesis_data)
summary(Missforest_imput_suppl$ximp)


MisF_imp_clean_thesis_data[, supp_inds] <- Missforest_imput_suppl$ximp
MisF_imp_clean_thesis_data_Indices[, supp_inds] <- Missforest_imput_suppl$ximp

### Save the data for covariates ###
write.csv2(CV_supple, "Raw_data/Covariate_supplement_2.csv")
write.csv2(MisF_imp_clean_thesis_data, "Raw_data/MisF_imp_clean_thesis_data.csv") # Updated ODA_Health_CV data

