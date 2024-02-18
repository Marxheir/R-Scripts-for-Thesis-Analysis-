# Install and load required packages
install.packages(c("mediation", "lavaan", "lme4", "robmed"))

library(plm)

library()
library(mediation)



##### Since there is no automatic method for panel data in mediation, we need data transformation manually 
library(tidyverse)
model_inds <- c("oda_disb", "aid_Per_GNI", "Soc_Assistance_cov", "SOC_INF_ODA", "Cov_all_SPL", 
                "govt_hth_spnd_1", "GDP_per_cap", "Pop_tot", "ae", "External_debt_stock", 
                "Trade", "cri_score", "gov", "FDI", "Pop_densty", 
                "Zscore_Reprd_index", "Zscore_Nutrit_index", "Zscore_EnvDeath_index", 
                "Zscore_HSCR_index", "Zscore_InfDis_index", "Zscore_Mental_index")

model_inds
demeaned_dat <- MisF_imp_clean_thesis_data_Indices %>%
  group_by(iso3c) %>%
  summarize(across(all_of(model_inds), ~ mean(., na.rm = TRUE), .names = "mean_{.col}")) %>%
  merge(MisF_imp_clean_thesis_data_Indices, agg_data_1, by = c("iso3c", "year"), 
        all.x = TRUE)


demeaned_dat <- cbind(
  MisF_imp_clean_thesis_data_Indices,
  datawizard::demean(MisF_imp_clean_thesis_data_Indices, select = all_of(model_inds), 
                     group = "iso3c")
)




demeaned_dat <- cbind(
  MisF_imp_clean_thesis_data_Indices,
  datawizard::demean(MisF_imp_clean_thesis_data_Indices, select = all_of(model_inds), 
                     group = "iso3c")
)


#MisF_imp_clean_thesis_data_Indices <- MisF_imp_clean_thesis_data_Indices %>%
#  mutate(log_remit = log(remit + 1))

model_inds <- c("log_ODA", "aid_Per_GNI", "Soc_Assistance_cov", "log_Soc_inf_ODA", "Cov_all_SPL", 
                "log_Govt_Hth_spd", "log_GDP_CAP", "log_Pop", "ae", "log_debt", 
                 "cri_score", "gov", "FDI", "log_pop_dens", "log_remit", "log_hlth_Per_Cap",
                "Zscore_Reprd_index", "Zscore_Nutrit_index", "Zscore_EnvDeath_index", 
                "Zscore_HSCR_index", "Zscore_InfDis_index", "Zscore_Mental_index")

summary(demeaned_dat[, model_inds])
########## Create lag and logs of variables #######
demeaned_dat <- MisF_imp_clean_thesis_data_Indices %>%
  mutate(log_pop_dens = log(Pop_densty), 
         log_ODA = log(oda_disb + 1), log_GDP_CAP = log(GDP_per_cap), log_Pop = log(Pop_tot), 
         log_Govt_Hth_spd = log(hlth_spdng_perGDP), log_debt = log(External_debt_stock), 
         log_Soc_inf_ODA = log(SOC_INF_ODA + 1), log_remit = log(remit + 1))



view(demeaned_dat[, model_inds])


rm(demeaned_dat, demeaned_dat_1)

demeaned_dat <- MisF_imp_clean_thesis_data_Indices

demeaned_dat <- cbind(
  demeaned_dat,
  datawizard::demean(demeaned_dat, select = all_of(model_inds), 
                     group = "iso3c")
)


## Lag 
demean_dat_lag <- collapse::flag(demeaned_dat[, lag_inds], n = 3, g = "iso3c", t = "year", cols = c("log_ODA_within", "Cov_all_SPL_within"))

demean_dat_lag <- collapse::L(demeaned_dat_1[, lag_inds], n = 5, by = c("iso3c", "year"))
rm(demeaned_dat_1)

demeaned_dat_1 <- demeaned_dat %>%
  group_by(iso3c) %>%
  mutate_at(vars(c("log_ODA_within", "Cov_all_SPL_within")), list(lag = ~collapse::L(., n = 5))) %>%
  mutate_at(vars(c("log_ODA_within", "Cov_all_SPL_within")), list(lag_3 = ~collapse::L(., n = 3))) %>%
  ungroup()

names(demeaned_dat)

lag_inds <- c("iso3c", "year", "L1.Cov_all_SPL_within", "L2.Cov_all_SPL_within", "L3.Cov_all_SPL_within", "L4.Cov_all_SPL_within", "L5.Cov_all_SPL_within", 
              "L6.Cov_all_SPL_within", "L1.log_ODA_within", "L2.log_ODA_within", "L3.log_ODA_within", "L4.log_ODA_within", "L5.log_ODA_within", 
              "L6.log_ODA_within")


view(lad_dat_1[lag_inds])
library(dplyr)
summary(demeaned_dat_1)

library(collapse)
names(demeaned_dat_1)
lag_data <- collapse::L(demeaned_dat_1, n = 1:6, g = iso3c, t = year, cols = 10:12)
view(lag_data)

lad_dat_1 <- merge(demeaned_dat_1, lag_data[, lag_inds], by = c("iso3c", "year"), all.x = TRUE)

view(demean_dat_lag[, c("iso3c", "year", "l1.log_ODA_within")])


plm::plm(Zscore_Reprd_index ~ plm::lag(log(Cov_all_SPL + 1), 2) + plm::lag(log(oda_disb + 1), 4) +
           log(govt_hth_spnd_1) +  log(GDP_per_cap) + log(Pop_tot) 
         + ae + log(External_debt_stock)  #+  
         #+ Pol_stabty_Est + 
         #   Female_educ + log(Trade)  + # clean_cook_enrgy: multicollinearity + 
         #covid_stringency + cri_score + ae + 
         #   + resource_rent   + Pop_densty +
         #  bci + #undp_hdi + 
         # pub_invest, # + #Water_access +  
         #Sanitaion_access  +
         #Total_debt_Service + 
         #  External_debt_stock  + time_var, #+ HealthResearch_ODA
         + log(Trade) + cri_score + gov + FDI + log(Pop_densty) + time_var, 
         data = MisF_imp_clean_thesis_data_Indices, 
         index = c("iso3c", "year"), model = "within", effect = "individual"))





view(demeaned_dat[, c("iso3c", "year", "Zscore_HSCR_index", "Zscore_HSCR_index_within")])


Mediation_spec <- "
# Path c' (Direct effect)
Zscore_Reprd_index ~  c*oda_disb +
           govt_hth_spnd_1 +  GDP_per_cap + Pop_tot
         + ae + External_debt_stock
         + Trade + cri_score + gov + FDI + Pop_densty + time_var
         

# Path a 
Cov_all_SPL ~  a*oda_disb +
           govt_hth_spnd_1 +  GDP_per_cap + Pop_tot
         + ae + External_debt_stock
         + Trade + cri_score + gov + FDI + Pop_densty + time_var

# Path b

Zscore_Reprd_index ~  b*Cov_all_SPL +
           govt_hth_spnd_1 +  GDP_per_cap + Pop_tot
         + ae + External_debt_stock
         + Trade + cri_score + gov + FDI + Pop_densty + time_var

# Indirect effect (mechanism)
ab:= a*b
"

########### Mediation Models ##########



library(robmed)
Mediation_spec <- Zscore_Reprd_index_within ~  log_ODA_within_lag + m(Cov_all_SPL_within_lag_3) + 
  covariates(log_Govt_Hth_spd_within,  log_GDP_CAP_within, log_debt_within, log_pop_dens_within, 
             ae_within, log_Pop_within, cri_score_within, gov_within, log_remit_within, 
              time_var)

Med_test_Repro <- test_mediation(Mediation_spec, data = demeaned_dat_1,
                 robust = TRUE)

summary(Med_test_Repro)

 
##
Mediation_spec <- Zscore_InfDis_index_within ~   L4.log_ODA_within + m(L3.Cov_all_SPL_within) + 
  covariates(log_Govt_Hth_spd_within,  log_GDP_CAP_within, log_debt_within, log_pop_dens_within, 
             ae_within, log_Pop_within, cri_score_within, gov_within, 
             FDI_within, log_trade_within, time_var)

Med_test_InfDis <- test_mediation(Mediation_spec, data = lad_dat_1,
                           robust = TRUE)
summary(Med_test_InfDis)
##


Mediation_spec <- Zscore_Nutrit_index ~   L4.log_ODA_within + m(L3.Cov_all_SPL_within) + 
  covariates(log_Govt_Hth_spd_within,  log_GDP_CAP_within, log_debt_within, log_pop_dens_within, 
             ae_within, log_Pop_within, cri_score_within, gov_within, 
             FDI_within, log_trade_within, time_var)

Med_test_Nutri <- test_mediation(Mediation_spec, data = lad_dat_1,
                                 robust = TRUE)
summary(Med_test_Nutri)
##


Mediation_spec <- Zscore_EnvDeath_index ~   L4.log_ODA_within + m(L3.Cov_all_SPL_within) + 
  covariates(log_Govt_Hth_spd_within,  log_GDP_CAP_within, log_debt_within, log_pop_dens_within, 
             ae_within, log_Pop_within, cri_score_within, gov_within, 
             FDI_within, log_trade_within, time_var)


Med_test_EnvDeath <- test_mediation(Mediation_spec, data = lad_dat_1,
                                 robust = TRUE)
summary(Med_test_EnvDeath)
##

Mediation_spec <- Zscore_HSCR_index ~   L4.log_ODA_within + m(L3.Cov_all_SPL_within) + 
  covariates(log_Govt_Hth_spd_within,  log_GDP_CAP_within, log_debt_within, log_pop_dens_within, 
             ae_within, log_Pop_within, cri_score_within, gov_within, 
             FDI_within, log_trade_within, time_var)


Med_test_HSCR <- test_mediation(Mediation_spec, data = lad_dat_1,
                                  robust = TRUE)
summary(Med_test_HSCR)
###

Mediation_spec <- Zscore_Mental_index ~   L4.log_ODA_within + m(L3.Cov_all_SPL_within) + 
  covariates(log_Govt_Hth_spd_within,  log_GDP_CAP_within, log_debt_within, log_pop_dens_within, 
             ae_within, log_Pop_within, cri_score_within, gov_within, 
             FDI_within, log_trade_within, time_var)

Med_test_Mental <- test_mediation(Mediation_spec, data = lad_dat_1,
                                  robust = TRUE)

summary(Med_test_Mental)














library(lavaan)
library(lme4)

summary(m1 <- lmer(Zscore_Reprd_index  ~ log(oda_disb + 1) + (1 + log(oda_disb + 1) | iso3c), data = agg_data))

sem(Mediation_spec, data = MisF_imp_clean_thesis_data_Indices)

plm::lag(oda_disb, k = 4, index = c("iso3c", "year"), data = MisF_imp_clean_thesis_data_Indices)




summary(plm::plm(Zscore_Reprd_index ~ plm::lag(Cov_all_SPL + 1) + plm::lag(log(oda_disb + 1)) +
           log(govt_hth_spnd_1) +  log(GDP_per_cap) + log(Pop_tot) 
         + ae + log(External_debt_stock)  #+  
         #+ Pol_stabty_Est + 
         #   Female_educ + log(Trade)  + # clean_cook_enrgy: multicollinearity + 
         #covid_stringency + cri_score + ae + 
         #   + resource_rent   + Pop_densty +
         #  bci + #undp_hdi + 
         # pub_invest, # + #Water_access +  
         #Sanitaion_access  +
         #Total_debt_Service + 
         #  External_debt_stock  + time_var, #+ HealthResearch_ODA
         + log(Trade) + cri_score + gov + FDI + log(Pop_densty) + time_var, 
         data = agg_data, 
         index = c("iso3c", "period"), model = "within", effect = "individual"))





# Assuming your_data is a panel data frame with columns: HealthOutcome, ODA, Mediator, and other relevant variables
panel_data <- pdata.frame(your_data, index = c("Country", "Year"))

# Fixed Effects model
fe_model <- plm(HealthOutcome ~ ODA + Mediator, data = panel_data, model = "within")

# Mediation analysis using the mediation package
mediation_model <- mediate(fe_model, mediator = "Mediator", treat = "ODA")

# Display mediation results
summary(mediation_model)
