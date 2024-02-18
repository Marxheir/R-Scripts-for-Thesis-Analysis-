########## Model Estimation ##########
### Trandform data to long panel 
dpm_long_panel <- merged_data %>%
  mutate(periods = as.factor(periods)
  ) %>%
  panel_data(id = iso3c, wave = periods)

view(dpm_long_panel)
view(d_long_panel)
summary(merged_data$Log_Govt_Spend_1)
#Zscore_Reprd_index, Zscore_InfDis_index, Zscore_Mental_index, Zscore_Nutrit_index, Zscore_EnvDeath_index, Zscore_HSCR_index 
# log_RFTP_Tr, log_BID_Tr, log_BMD_Tr, log_Malnutri_Tr, log_Env_Dth_Tr, log_HSCR_Tr, 
# log_ODA, log_Soc_Infra, log_ODA_lag, log_Soc_Infra_lag, 
# log_ae, log_bci, log_gov, log_CHS, log_hlth_Per_Cap, log_CF_LF_ratio, log_CRI_Score, log_Pop, log_Pop_dens, log_External_debt,
# log_GDP_Cap, log_Trade, log_unemp, Log_Govt_Spend_1
hist(merged_data$log_ae)

############### Reseach Question One ###########
### RFTP
#merged_data$log_Pop_dens
RFTP_dpm_log <- dpm(log_RFTP_Tr ~ pre(log_ODA_lag) + ae + log_GDP_Cap + cri_score  + log_remittance + 
                      log_hlth_Per_Cap + gov + log_External_debt, 
                    data = dpm_long_panel,
                    fixed.effects = TRUE, x.free = FALSE, # allows the regression coefficient(s) for the ldv to vary over time.
                    y.free = FALSE, se = "robust" #error.inv = TRUE
) ## This is the best so far, given the covariates

summary(RFTP_dpm_log)

RFTP_dpm <- dpm(Zscore_Reprd_index ~ pre(log_ODA_lag) + ae + log_GDP_Cap + cri_score  + log_remittance + 
                  log_hlth_Per_Cap + gov + log_External_debt, # + log_Pop_dens,
                data = dpm_long_panel,
                fixed.effects = TRUE, x.free = FALSE, # allows the regression coefficient(s) for the ldv to vary over time.
                y.free = FALSE, se = "robust" #error.inv = TRUE
) ## This is the best so far, given the covariates
 
### Burden of Infection and Disease
summary(BID_dpm)

## Removed log of population and population density. Remember to include it in FE models
BID_dpm <- dpm(Zscore_InfDis_index ~ pre(log_ODA_lag) + log_GDP_Cap + log_remittance + ae + log_hlth_Per_Cap + gov + 
            log_External_debt + cri_score, 
               data = dpm_long_panel,
               fixed.effects = TRUE, x.free = FALSE, # allows the regression coefficient(s) for the ldv to vary over time.
               y.free = FALSE, se = "robust" #error.inv = TRUE
) ## This is the best so far, given the covariates
summary(BID_dpm)



BID_dpm_log <- dpm(log_BID_Tr ~ pre(log_ODA_lag) + log_GDP_Cap + log_remittance + ae + log_hlth_Per_Cap  +# ,# + 
                 log_External_debt + gov + cri_score,
               data = dpm_long_panel,
               fixed.effects = TRUE, x.free = FALSE, # allows the regression coefficient(s) for the ldv to vary over time.
               y.free = FALSE, se = "robust" #error.inv = TRUE
) ## This is the best so far, given the covariates


summary(BID_dpm_log)



### Burden of Mental Problems ######
summary(BMP_dpm_log)
BMP_dpm <- dpm(Zscore_Mental_index ~ pre(log_ODA_lag) + log_GDP_Cap + log_remittance  + ae + log_hlth_Per_Cap + gov + 
                 log_External_debt + cri_score, 
               data = dpm_long_panel,
               fixed.effects = TRUE, x.free = FALSE, # allows the regression coefficient(s) for the ldv to vary over time.
               y.free = FALSE, se = "robust" #error.inv = TRUE
) ## This is the best so far, given the covariates



BMP_dpm_log <- dpm(log_BMD_Tr ~ pre(log_ODA_lag) + log_GDP_Cap + ae + log_hlth_Per_Cap + log_External_debt + gov +
                   cri_score  + log_remittance, 
                   data = dpm_long_panel,
                   fixed.effects = TRUE, x.free = FALSE, # allows the regression coefficient(s) for the ldv to vary over time.
                   y.free = FALSE, se = "robust" #error.inv = TRUE
) ## This is the best so far, given the covariates

summary(BMP_dpm_log)


## Malnutrition

summary(Malnuti_dpm)
Malnuti_dpm <- dpm(Zscore_Nutrit_index ~ pre(log_ODA_lag) + log_GDP_Cap + log_remittance  + ae + log_hlth_Per_Cap + gov + 
                 log_External_debt + cri_score, 
               data = dpm_long_panel,
               fixed.effects = TRUE, x.free = FALSE, # allows the regression coefficient(s) for the ldv to vary over time.
               y.free = FALSE, se = "robust" #error.inv = TRUE
) ## This is the best so far, given the covariates


Malnutr_dpm_log <- dpm(log_Malnutri_Tr ~ pre(log_ODA_lag) + log_GDP_Cap + ae + log_hlth_Per_Cap + log_External_debt + gov +
              log_remittance + cri_score, 
                   data = dpm_long_panel,
                   fixed.effects = TRUE, x.free = FALSE, # allows the regression coefficient(s) for the ldv to vary over time.
                   y.free = FALSE, se = "robust" #error.inv = TRUE
) ## This is the best so far, given the covariates


### Environmental Death

EnvDeath_dpm <- dpm(Zscore_EnvDeath_index ~ pre(log_ODA_lag) + log_GDP_Cap + log_remittance  + ae + log_hlth_Per_Cap + gov + 
                     log_External_debt + cri_score, 
                   data = dpm_long_panel,
                   fixed.effects = TRUE, x.free = FALSE, # allows the regression coefficient(s) for the ldv to vary over time.
                   y.free = FALSE, se = "robust" #error.inv = TRUE
) ## This is the best so far, given the covariates


EnvDeath_dpm_log <- dpm(log_Env_Dth_Tr ~ pre(log_ODA_lag) + log_GDP_Cap + ae + log_hlth_Per_Cap + log_External_debt + gov +
                         cri_score + log_remittance, 
                       data = dpm_long_panel,
                       fixed.effects = TRUE, x.free = FALSE, # allows the regression coefficient(s) for the ldv to vary over time.
                       y.free = FALSE, se = "robust" #error.inv = TRUE
) ## This is the best so far, given the covariates


#### Health system capacity
summary(HSCR_dpm_log)
HSCR_dpm <- dpm(Zscore_HSCR_index ~ pre(log_ODA_lag) + log_GDP_Cap + log_remittance + ae + log_hlth_Per_Cap + gov + 
                      log_External_debt + cri_score, #+ log_Pop, #+ log_ae + log_GDP_Cap +  
                    #FDI + log_CRI_Score + 
                    #log_Trade + log_hlth_Per_Cap + log_gov + log_External_debt, # + log_Pop_dens,
                    data = dpm_long_panel,
                    fixed.effects = TRUE, x.free = FALSE, # allows the regression coefficient(s) for the ldv to vary over time.
                    y.free = FALSE, se = "robust" #error.inv = TRUE
) ## This is the best so far, given the covariates


HSCR_dpm_log <- dpm(log_HSCR_Tr ~ pre(log_ODA_lag) + log_GDP_Cap + ae + log_hlth_Per_Cap + log_External_debt + gov +
                          cri_score + log_remittance, 
                        data = dpm_long_panel,
                        fixed.effects = TRUE, x.free = FALSE, # allows the regression coefficient(s) for the ldv to vary over time.
                        y.free = FALSE, se = "robust" #error.inv = TRUE
) ## This is the best so far, given the covariates

DPM_models <- list(RFTP_dpm, BID_dpm, BMP_dpm, Malnuti_dpm, EnvDeath_dpm, HSCR_dpm)
DPM_models_log <- list(RFTP_dpm_log, BID_dpm_log, BMP_dpm_log, Malnutr_dpm_log, EnvDeath_dpm_log, HSCR_dpm_log)

cat(texreg::texreg(DPM_models, digits = 3), "\n\n")
rm(DPM_models, DPM_models_log)



######## DPM ANALYSIS FOR RESEARCH QUESTION 2: REGIONAL HETEROGENEITY OF EFFECT ########
rm(RFTP_Region_DPM_log)

RFTP_Region_DPM <- dpm(Zscore_Reprd_index ~ pre(log_ODA_lag) + pre(SSA_ODA) +  pre(MENA_ODA) + pre(LAC_ODA) + pre(SCA_ODA) + pre(Europe_ODA) +
                  ae + log_GDP_Cap +  
                    log_remittance + cri_score + 
                   log_hlth_Per_Cap + gov + log_External_debt, 
                data = dpm_long_panel,
                fixed.effects = TRUE, x.free = FALSE, # allows the regression coefficient(s) for the ldv to vary over time.
                y.free = FALSE, se = "robust" #error.inv = TRUE
) ## This is the best so far, given the covariates

## BID

BID_Region_dpm <- dpm::dpm(Zscore_InfDis_index ~ pre(log_ODA_lag) + pre(SSA_ODA) +  pre(MENA_ODA) + pre(LAC_ODA) + pre(SCA_ODA) + pre(Europe_ODA) +
                        log_GDP_Cap + log_remittance + ae + log_hlth_Per_Cap + gov + 
                 log_External_debt + cri_score, 
               data = dpm_long_panel,
               fixed.effects = TRUE, x.free = FALSE, # allows the regression coefficient(s) for the ldv to vary over time.
               y.free = FALSE, se = "robust" #error.inv = TRUE
) ## This is the best so far, given the covariates

### Burden of Mental Problems ######
summary(BMP_Region_dpm)
BMP_Region_dpm <- dpm(Zscore_Mental_index ~ pre(log_ODA_lag) + pre(SSA_ODA) +  pre(MENA_ODA) + pre(LAC_ODA) + pre(SCA_ODA) + pre(Europe_ODA) +
                 log_GDP_Cap + log_remittance + ae + log_hlth_Per_Cap + gov + 
                 log_External_debt + cri_score, 
               data = dpm_long_panel,
               fixed.effects = TRUE, x.free = FALSE, # allows the regression coefficient(s) for the ldv to vary over time.
               y.free = FALSE, se = "robust" #error.inv = TRUE
) ## This is the best so far, given the covariates


## Malnutrition
Malnuti_Region_dpm <- dpm(Zscore_Nutrit_index ~ pre(log_ODA_lag) + pre(SSA_ODA) +  pre(MENA_ODA) + pre(LAC_ODA) + pre(SCA_ODA) + pre(Europe_ODA) +
                     log_GDP_Cap + log_remittance + ae + log_hlth_Per_Cap + gov + 
                     log_External_debt + cri_score, 
                   data = dpm_long_panel,
                   fixed.effects = TRUE, x.free = FALSE, # allows the regression coefficient(s) for the ldv to vary over time.
                   y.free = FALSE, se = "robust" #error.inv = TRUE
) ## This is the best so far, given the covariates


### Environmental Death
EnvDeath_Region_dpm <- dpm(Zscore_EnvDeath_index ~ pre(log_ODA_lag) + pre(SSA_ODA) +  pre(MENA_ODA) + pre(LAC_ODA) + pre(SCA_ODA) + pre(Europe_ODA) +
                             log_GDP_Cap + log_remittance + ae + log_hlth_Per_Cap + gov + 
                      log_External_debt + cri_score, 
                    data = dpm_long_panel,
                    fixed.effects = TRUE, x.free = FALSE, # allows the regression coefficient(s) for the ldv to vary over time.
                    y.free = FALSE, se = "robust" #error.inv = TRUE
) ## This is the best so far, given the covariates

#### Health system capacity

HSCR_Region_dpm <- dpm(Zscore_HSCR_index ~ pre(log_ODA_lag) + pre(SSA_ODA) +  pre(MENA_ODA) + pre(LAC_ODA) + pre(SCA_ODA) + pre(Europe_ODA) +
                  log_GDP_Cap + log_remittance + ae + log_hlth_Per_Cap + gov + 
                  log_External_debt + cri_score, 
                data = dpm_long_panel,
                fixed.effects = TRUE, x.free = FALSE, # allows the regression coefficient(s) for the ldv to vary over time.
                y.free = FALSE, se = "robust" #error.inv = TRUE
) ## This is the best so far, given the covariates



DPM_models <- list(RFTP_dpm, BID_dpm, BMP_dpm, Malnuti_dpm, EnvDeath_dpm, HSCR_dpm)
DPM_models_log <- list(RFTP_dpm_log, BID_dpm_log, BMP_dpm_log, Malnutr_dpm_log, EnvDeath_dpm_log, HSCR_dpm_log)
DPM_Region_models <- list(RFTP_Region_DPM, BID_Region_dpm, BMP_Region_dpm, Malnuti_Region_dpm, EnvDeath_Region_dpm, HSCR_Region_dpm)

overleaf_code <- texreg::texreg(DPM_models_log, digits = 3)
cat(overleaf_code, "\n\n")


######### ROBURSTNESS CHECKS ############
## Use Social Infrastucture ODA "log_Soc_Infra_lag" for RQ 1 and 2

#### Roburstness Models 
### RFTP
#merged_data$log_Pop_dens

RFTP_Robst_dpm <- dpm(Zscore_Reprd_index ~ pre(log_Soc_Infra_lag) + ae + log_GDP_Cap +  
                        log_remittance + cri_score + 
                   log_hlth_Per_Cap + gov + log_External_debt, 
                data = dpm_long_panel,
                fixed.effects = TRUE, x.free = FALSE, # allows the regression coefficient(s) for the ldv to vary over time.
                y.free = FALSE, se = "robust" #error.inv = TRUE
) ## This is the best so far, given the covariates


### Burden of Infection and Disease
summary(BID_dpm_log)

## Removed log of population and population density. Remember to include it in FE models
BID_Robst_dpm <- dpm(Zscore_InfDis_index ~ pre(log_Soc_Infra_lag) + log_GDP_Cap +  log_remittance + ae + log_hlth_Per_Cap + gov + 
                 log_External_debt + cri_score, 
               data = dpm_long_panel,
               fixed.effects = TRUE, x.free = FALSE, # allows the regression coefficient(s) for the ldv to vary over time.
               y.free = FALSE, se = "robust" #error.inv = TRUE
) ## This is the best so far, given the covariates


### Burden of Mental Problems ######

BMP_Robst_dpm <- dpm(Zscore_Mental_index ~ pre(log_Soc_Infra_lag) + log_GDP_Cap + log_remittance + ae + log_hlth_Per_Cap + gov + 
                 log_External_debt + cri_score,
               data = dpm_long_panel,
               fixed.effects = TRUE, x.free = FALSE, # allows the regression coefficient(s) for the ldv to vary over time.
               y.free = FALSE, se = "robust" #error.inv = TRUE
) ## This is the best so far, given the covariates


## Malnutrition

summary(Malnuti_dpm)
Malnuti_Robst_dpm <- dpm(Zscore_Nutrit_index ~ pre(log_Soc_Infra_lag) + log_GDP_Cap +  log_remittance + ae + log_hlth_Per_Cap + gov + 
                     log_External_debt + cri_score, 
                   data = dpm_long_panel,
                   fixed.effects = TRUE, x.free = FALSE, # allows the regression coefficient(s) for the ldv to vary over time.
                   y.free = FALSE, se = "robust" #error.inv = TRUE
) ## This is the best so far, given the covariates


### Environmental Death

EnvDeath_Robst_dpm <- dpm(Zscore_EnvDeath_index ~ pre(log_Soc_Infra_lag) + log_GDP_Cap +  log_remittance + ae + log_hlth_Per_Cap + gov + 
                      log_External_debt + cri_score, 
                    data = dpm_long_panel,
                    fixed.effects = TRUE, x.free = FALSE, # allows the regression coefficient(s) for the ldv to vary over time.
                    y.free = FALSE, se = "robust" #error.inv = TRUE
) ## This is the best so far, given the covariates



#### Health system capacity

HSCR_Robst_dpm <- dpm(Zscore_HSCR_index ~ pre(log_Soc_Infra_lag) + log_GDP_Cap +  log_remittance + ae + log_hlth_Per_Cap + gov + 
                  log_External_debt + cri_score, 
                data = dpm_long_panel,
                fixed.effects = TRUE, x.free = FALSE, # allows the regression coefficient(s) for the ldv to vary over time.
                y.free = FALSE, se = "robust" #error.inv = TRUE
) ## This is the best so far, given the covariates



DPM_Robst_models <- list(RFTP_Robst_dpm, BID_Robst_dpm, BMP_Robst_dpm, Malnuti_Robst_dpm, EnvDeath_Robst_dpm, HSCR_Robst_dpm)


cat(texreg::texreg(DPM_Robst_models, digits = 3, custom.model.names = DV_vars), "\n\n")



######## RBUSTNESS DPM ANALYSIS FOR RESEARCH QUESTION 2: REGIONAL HETEROGENEITY OF EFFECT ########

RFTP_Region_Robst_DPM <- dpm(Zscore_Reprd_index ~ pre(log_Soc_Infra_lag) + pre(SSA_SocInf_ODA) +  pre(MENA_SocInf_ODA) + pre( LAC_SocInf_ODA) + pre(SCA_SocInfra_ODA) + pre(Europe_SocInfra_ODA) +
                             ae + log_GDP_Cap + log_hlth_Per_Cap +  log_remittance + cri_score + 
                             gov + log_External_debt,
                           data = dpm_long_panel,
                           fixed.effects = TRUE, x.free = FALSE, # allows the regression coefficient(s) for the ldv to vary over time.
                           y.free = FALSE, se = "robust" #error.inv = TRUE
) ## This is the best so far, given the covariates


## BID
BID_Region_Robst_dpm <- dpm(Zscore_InfDis_index ~ pre(log_Soc_Infra_lag) + pre(SSA_SocInf_ODA) +  pre(MENA_SocInf_ODA) + pre( LAC_SocInf_ODA) + pre(SCA_SocInfra_ODA) + pre(Europe_SocInfra_ODA) +                        
                              log_GDP_Cap +  log_remittance + ae + log_hlth_Per_Cap + gov + 
                        log_External_debt + cri_score, 
                      data = dpm_long_panel,
                      fixed.effects = TRUE, x.free = FALSE, # allows the regression coefficient(s) for the ldv to vary over time.
                      y.free = FALSE, se = "robust" #error.inv = TRUE
) ## This is the best so far, given the covariates


### Burden of Mental Problems ######
summary(BMP_Region_Robst_dpm)
BMP_Region_Robst_dpm <- dpm(Zscore_Mental_index ~ pre(log_Soc_Infra_lag) + pre(SSA_SocInf_ODA) +  pre(MENA_SocInf_ODA) + pre( LAC_SocInf_ODA) + 
                              pre(SCA_SocInfra_ODA) + 
                        log_GDP_Cap + log_remittance + ae + pre(Europe_SocInfra_ODA)  + gov + log_CRI_score,
                      data = dpm_long_panel,
                      fixed.effects = TRUE, x.free = FALSE, # allows the regression coefficient(s) for the ldv to vary over time.
                      y.free = FALSE, se = "robust" #error.inv = TRUE
) ## This is the best so far, given the covariates


## Malnutrition
Malnuti_Region_Robst_dpm <- dpm(Zscore_Nutrit_index ~ pre(log_Soc_Infra_lag) + pre(SSA_SocInf_ODA) +  pre(MENA_SocInf_ODA) + pre( LAC_SocInf_ODA) + pre(SCA_SocInfra_ODA) + pre(Europe_SocInfra_ODA) +
                            log_GDP_Cap + log_remittance + ae + log_hlth_Per_Cap + gov + 
                            log_External_debt + cri_score, 
                          data = dpm_long_panel,
                          fixed.effects = TRUE, x.free = FALSE, # allows the regression coefficient(s) for the ldv to vary over time.
                          y.free = FALSE, se = "robust" #error.inv = TRUE
) ## This is the best so far, given the covariates


### Environmental Death
EnvDeath_Region_Robst_dpm <- dpm(Zscore_EnvDeath_index ~ pre(log_Soc_Infra_lag) + pre(SSA_SocInf_ODA) +  pre(MENA_SocInf_ODA) + pre( LAC_SocInf_ODA) + pre(SCA_SocInfra_ODA) + pre(Europe_SocInfra_ODA) +
                             log_GDP_Cap + log_remittance + ae + log_hlth_Per_Cap + gov + 
                             log_External_debt + cri_score, #+ log_Pop, #+ log_ae + log_GDP_Cap +  
                           #FDI + log_CRI_Score + 
                           #log_Trade + log_hlth_Per_Cap + log_gov + log_External_debt, # + log_Pop_dens,
                           data = dpm_long_panel,
                           fixed.effects = TRUE, x.free = FALSE, # allows the regression coefficient(s) for the ldv to vary over time.
                           y.free = FALSE, se = "robust" #error.inv = TRUE
) ## This is the best so far, given the covariates

#### Health system capacity

HSCR_Region_Robst_dpm <- dpm(Zscore_HSCR_index ~ pre(log_Soc_Infra_lag) + pre(SSA_SocInf_ODA) +  pre(MENA_SocInf_ODA) + pre( LAC_SocInf_ODA) + pre(SCA_SocInfra_ODA) + pre(Europe_SocInfra_ODA) +
                         log_GDP_Cap + log_remittance + ae + log_hlth_Per_Cap + gov + 
                         log_External_debt + cri_score, 
                       data = dpm_long_panel,
                       fixed.effects = TRUE, x.free = FALSE, # allows the regression coefficient(s) for the ldv to vary over time.
                       y.free = FALSE, se = "robust" #error.inv = TRUE
) ## This is the best so far, given the covariates




DPM_Region_Robst_models <- list(RFTP_Region_Robst_DPM, BID_Region_Robst_dpm, Malnuti_Region_Robst_dpm, EnvDeath_Region_Robst_dpm, HSCR_Region_Robst_dpm)

cat(texreg::texreg(DPM_Region_Robst_models, digits = 3, custom.model.names = DV_vars), "\n\n")








### Function for plm models
dpm_function <- function(DV) {
  model_sp <- as.formula(paste(DV, "~ pre(log_ODA_lag) + log_Pop + log_ae + log_hlth_Per_Cap + 
                               log_GDP_Cap + log_hlth_Per_Cap + FDI + log_CRI_Score + log_Trade + log_gov 
                               + log_External_debt"))
  
  result <- dpm::dpm(model_sp, data = dpm_long_panel,
                     fixed.effects = TRUE, x.free = FALSE,
                     y.free = FALSE, se = "robust")
  return(result)
}



# Initialize an empty list to store models

model_list <- list()
# Loop through each DV variable
for (var in DV_vars) {
  DPM_models <- dpm_function(var)
  model_list[[var]] <- DPM_models
  overleaf_code <- texreg::texreg(model_list)
  cat(overleaf_code, "\n\n")
}

