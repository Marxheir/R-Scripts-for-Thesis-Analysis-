library(devtools)
library(panelr)
library(dpm)
library(tidyverse)
library(lavaan)



MisF_imp_clean_thesis_data_Indices <-  read.csv2("Raw_data/MisF_imp_clean_thesis_data_Indices.csv", sep = ";")
MisF_imp_clean_thesis_data <-  read.csv2("Raw_data/MisF_imp_clean_thesis_data.csv", sep = ";")
Impact_data <-  read.csv2("./New_impact_data_few_Vars.csv", sep = ";")

library(tidyverse)


MisF_imp_clean_thesis_data_Indices_1 <- MisF_imp_clean_thesis_data_Indices


MisF_imp_clean_thesis_data_Indices_1$year <- as.numeric(as.character(MisF_imp_clean_thesis_data_Indices_1$year))
MisF_imp_clean_thesis_data_Indices_1$period <- cut(MisF_imp_clean_thesis_data_Indices_1$year, breaks = 6, labels = FALSE)

view(MisF_imp_clean_thesis_data_Indices_1[, c("iso3c", "year", "period")])

agg_indicators <- names(clean_thesis_data[, setdiff(names(clean_thesis_data), c("iso3c", "year", "country", "region"))])

agg_data <- MisF_imp_clean_thesis_data_Indices_1 %>%
  group_by(iso3c, period) %>%
  summarise(mean)
view(agg_data)


?aggregate

rm(MisF_imp_clean_thesis_data_Indices_1)

# Aggregate the data by averaging within each period
Level <-cut(MisF_imp_clean_thesis_data_Indices$year, breaks = 6, right=T)

needed_noyeariso3c <- colnames(MisF_imp_clean_thesis_data_Indices[, !(colnames(MisF_imp_clean_thesis_data_Indices) %in% c("year", "iso3c", "region", "country", "SSA_dum", "MENA_dum", "Europe_dum", "LAC_dum", 
                                                                                                                            "SCA_dum", "X", "time_var"))])


agg_data <- aggregate(MisF_imp_clean_thesis_data_Indices[needed_noyeariso3c], 
                             list(MisF_imp_clean_thesis_data_Indices$iso3c, Level), mean)

# Rename the ids columns after aggregation
agg_data <- agg_data %>% 
  rename(iso3c = Group.1, period = Group.2) %>%
  group_by(iso3c) %>%
  mutate(time_var = row_number())



## Check the distributions and variables
summary(agg_data)

hist(MisF_imp_clean_thesis_data_Indices$Zscore_Reprd_index)

head(agg_data, 10)

view(agg_data[, c("iso3c", "period", "time_var")])

### Merge regions and country names back
ids_cols <- unique(MisF_imp_clean_thesis_data_Indices[, c("country", "iso3c", "region")])
agg_data <- merge(agg_data, ids_cols, by = "iso3c", all.x = TRUE)

view(agg_data[, c("iso3c", "country", "region", "period")])

names(agg_data)
####### Creating Regional Dummies ############
MisF_imp_clean_thesis_data_1 <- MisF_imp_clean_thesis_data
agg_data$SSA_dum <- as.numeric(agg_data$region == "SSA")
agg_data$MENA_dum <- as.numeric(agg_data$region == "MENA")
agg_data$Europe_dum <- as.numeric(agg_data$region == "Europe")
agg_data$LAC_dum <- as.numeric(agg_data$region == "LAC")
agg_data$SCA_dum <- as.numeric(agg_data$region == "SCA")


unique(MisF_imp_clean_thesis_data$region)

view(MisF_imp_clean_thesis_data_1[, c("country", "SSA_dum")])





### Rearrange the columns 
agg_data <- agg_data %>%
  select(country, iso3c, region, period, time_var, everything())

write.csv2(agg_data, "Raw_data/Agg_data_Thesis.csv")# To save the aggregated data 



######## New Aggregation with selected small variables #############
### Aggregate the new data ###
MisF_imp_clean_thesis_data_Indices <- merge(MisF_imp_clean_thesis_data_Indices, Impact_data[, c("iso3c", "year", "remit")], by = c("iso3c", "year"), all.x = TRUE)

model_vars <- c("ae", "bci", "UHC", "CHS_25perc", "oda_disb", "SOC_INF_ODA", "hlth_spdng_perGDP", 
                "govt_hth_spnd_1", "govt_hth_spnd_2", "govt_hth_sp_percap3", "Urb_pop_rate", 
                "Pol_stabty_Est", "Resch_dev", "Pop_tot", "Pop_densty", "Unemply_rate", "Female_educ", 
                "Trade", "FDI", "GDP_per_cap", "CF_LF_ratio", "cri_score", "pub_invest", "Cov_all_SPL", 
                "aid_Per_GNI", "Zscore_Reprd_index", "Zscore_InfDis_index", "Zscore_Mental_index",
                "Zscore_Nutrit_index", "Zscore_EnvDeath_index", "Total_debt_Service", "External_debt_stock", 
                "Zscore_HSCR_index", "gov", "remit")

library(tidyverse)


hist(log(Impact_data$remit))

### Need to double check the aggregation process using alternative method to aggregate function
merged_data <- MisF_imp_clean_thesis_data_Indices %>%
  filter(year != 2022) %>% # Focus on 2021 since there is no data for ODA till 2022
  mutate(periods = case_when(
    year >= 2000 & year <= 2004 ~ "2000-2004", # Five years for the first period, this has implication for lag  
    year >= 2005 & year <= 2009 ~ "2005-2009", # Five years for the second period
    year >= 2010 & year <= 2013 ~ "2010-2013",
    year >= 2014 & year <= 2017 ~ "2014-2017",
    year >= 2018 & year <= 2021 ~ "2018-2021",
    TRUE ~ as.character(year))) %>%
  group_by(iso3c, periods) %>%
  summarise(across(.cols = model_vars, .fns = mean, na.rm = TRUE))%>%
  mutate(
    log_remittance = log(remit + 1),
    log_ae = log(ae + 1),     
    log_bci = log(bci + 1),
    log_gov = log(gov + 3), 
    log_CHS = log(CHS_25perc + 1), 
    log_ODA = log(oda_disb + 1),
    log_Soc_Infra = log(SOC_INF_ODA + 1),
    Log_Govt_Spend_1 = log(govt_hth_spnd_1),
    log_hlth_Per_Cap = log(hlth_spdng_perGDP),
    log_CF_LF_ratio = log(CF_LF_ratio + 1),
    log_Pop = log(Pop_tot),
    log_CRI_Score = log(cri_score), 
    #log_FDI = log(FDI),
    log_Pop_dens = log(Pop_densty), 
    log_External_debt = log(External_debt_stock), 
    log_Trade = log(Trade), 
    log_GDP_Cap = log(GDP_per_cap),
    log_Env_Dth_Tr = log(Zscore_EnvDeath_index + 2), 
    log_BID_Tr = log(Zscore_InfDis_index + 1), 
    log_HSCR_Tr = log(Zscore_HSCR_index + 2.5), 
    log_BMD_Tr = log(Zscore_Mental_index + 1.5), 
    log_RFTP_Tr = log(Zscore_Reprd_index + 1.5), 
    log_Malnutri_Tr = log(Zscore_Nutrit_index + 2), 
    log_ODA_per_GNI = log(aid_Per_GNI + 1), 
    log_unemp = log(Unemply_rate + 1)
  )


summary(MisF_imp_clean_thesis_data_Indices$hlth_spdng_perGDP)
### Create lag for ODA
names(merged_data)
merged_data_1 <- merged_data_1 %>%
  mutate(collapse::L(merged_data_1, n = 1, # Only one lag is logical for Aggregated data 
                     by = ~ iso3c, t = ~ periods, 
                     cols = 48:50)) # Creates 1st Lag order of the above Logged variables

merged_data <- merged_data %>%
  group_by(iso3c) %>%
  mutate_at(vars(43:44), list(lag = ~collapse::L(., n = 1))) %>%
  ungroup()

# Confirm if the lag structure is correct 
view(merged_data[, c("iso3c", "periods", "log_ODA_lag")])

## Add region and country names back
ids_cols <- unique(MisF_imp_clean_thesis_data_Indices[, c("country", "iso3c", "region")])
merged_data <- merge(merged_data, ids_cols, by = "iso3c", all.x = TRUE)

view(agg_data[, c("iso3c", "country", "region", "period")])


### Regioal dummies and period trends
merged_data <- merged_data %>%
  group_by(iso3c) %>%
  mutate(time_var = row_number())

######### CREATING REGIONAL DUMMIES AND INTERRACTIONS OF VARIABLES ###########
merged_data$SSA_dum <- as.numeric(merged_data$region == "SSA")
merged_data$MENA_dum <- as.numeric(merged_data$region == "MENA")
merged_data$Europe_dum <- as.numeric(merged_data$region == "Europe")
merged_data$LAC_dum <- as.numeric(merged_data$region == "LAC")
merged_data$SCA_dum <- as.numeric(merged_data$region == "SCA")

merged_data <- merged_data %>%
  mutate(SSA_ODA = SSA_dum * log_ODA_lag, 
         MENA_ODA = MENA_dum * log_ODA_lag, 
         LAC_ODA = LAC_dum * log_ODA_lag, 
         SCA_ODA = SCA_dum * log_ODA_lag, 
         Europe_ODA = Europe_dum * log_ODA_lag, 
         SSA_SocInf_ODA = SSA_dum * log_Soc_Infra_lag, 
         MENA_SocInf_ODA = MENA_dum * log_Soc_Infra_lag, 
         LAC_SocInf_ODA = LAC_dum * log_Soc_Infra_lag, 
         SCA_SocInfra_ODA = SCA_dum * log_Soc_Infra_lag, 
         Europe_SocInfra_ODA = Europe_dum * log_Soc_Infra_lag) 



### Rearrange the columns 
merged_data <- merged_data %>%
  select(country, iso3c, region, periods, time_var, everything())
  
summary(agg_data_1)
write.csv2(merged_data, "./Raw_data/Aggregated_Supplem_data.csv")
rm(Impact_data, ids_cols)

