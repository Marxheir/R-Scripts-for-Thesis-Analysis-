Unit_FE <- plm::plm(log_RFTP_Tr ~ log_ODA_lag +#  + SSA_ODA +  MENA_ODA + LAC_ODA + SCA_ODA + Europe_ODA +
                      ae + log_GDP_Cap +  
                       cri_score + log_remittance + log_Pop + log_Pop_dens + 
                      #log_Trade + 
                   log_hlth_Per_Cap + gov + log_External_debt + time_var, # + log_Pop_dens,
                    data = dpm_long_panel, index = c("iso3c", "periods"), model = "within", effect = "individual")

lmtest::coeftest(Unit_FE, vcov. = vcovHC, type = "HC1")

?vcovHAC()
cor(merged_data$FDI, log(merged_data$remit + 1))
summary(Unit_FE)
hist(log(merged_data$External_debt_stock))
hist(merged_data$Zscore_Reprd_index)

lmtest::coeftest(Unit_FE, 
                 vcov = vcovHC(Unit_FE, method = "arellano",
                               type = "sss", cluster = "group"))

library(lmtest)
library(sandwich)
library(tidyverse)
library(plm)
sandwich::vcovHAC(Unit_FE)
DV_vars <- c("Zscore_Reprd_index", "Zscore_InfDis_index", "Zscore_Mental_index", 
             "Zscore_Nutrit_index", "Zscore_EnvDeath_index", "Zscore_HSCR_index") 


DV_vars <- c("log_RFTP_Tr", "log_BID_Tr", "log_BMD_Tr", "log_Malnutri_Tr", "log_Env_Dth_Tr", "log_HSCR_Tr")

########### RESEARCH QUESTION ONE ############
plm_func_RQ1 <- function(DV) {
  plm_form <- as.formula(paste(DV, "~ log_ODA_lag + ae + log_GDP_Cap + cri_score + log_remittance + log_Pop + log_Pop_dens + 
                   log_hlth_Per_Cap + gov + log_External_debt + time_var"))
  
  plm_result <- plm::plm(plm_form, data = dpm_long_panel, 
                         index = c("iso3c", "periods"), model = "within", effect = "individual")
  
  plm_result_std <- lmtest::coeftest(plm_result, save = TRUE,
                                     vcov = vcovHC(plm_result, #method = "arellano", 
                                                   type = "sss", cluster = "group"))#[, ## Methods: sss, HC1, HC2, HC3  
  
  return(plm_result)
}


model_list <- list()
# Loop through each DV variable
for (var in DV_vars) {
  FE_models <- plm_func_RQ1(var)
  model_list[[var]] <- FE_models
  cat(texreg::texreg(model_list, digits = 3), "\n\n")
}


modelsummary::modelsummary(model_list, stars = TRUE, output = "latex")



######## Research Question Two FE ##########
dpm_long_panel$MENA_dum

plm_func_RQ2 <- function(DV) {
  plm_form <- as.formula(paste(DV, "~  SSA_dum  +  MENA_dum + LAC_dum + SCA_dum + Europe_dum +
                          log_Soc_Infra_lag * (SSA_dum  +  MENA_dum + LAC_dum + SCA_dum + Europe_dum) +
                          ae + log_GDP_Cap + cri_score + log_remittance + log_Pop + log_Pop_dens + 
                          log_hlth_Per_Cap + gov + log_External_debt + time_var"))
  
  plm_result <- plm::plm(plm_form, data = dpm_long_panel, 
                         index = c("iso3c", "periods"), model = "within", effect = "individual")
  
  plm_result <- lmtest::coeftest(plm_result, save = TRUE,
                                    vcov = vcovHC(plm_result, #method = "arellano", 
                                                type = "sss", cluster = "group"))#[, ## Methods: sss, HC1, HC2, HC3  
  
  return(plm_result)
}

cor(dpm_long_panel$log_ODA, (dpm_long_panel$MENA_dum * dpm_long_panel$log_ODA)) # None of the interractions exhibits multicolinearity


model_list <- list()
# Loop through each DV variable
for (var in DV_vars) {
  FE_models <- plm_func_RQ2(var)
  model_list[[var]] <- FE_models
  cat(texreg::texreg(model_list, digits = 3), "\n\n")
}


modelsummary::modelsummary(model_list, stars = TRUE, output = "latex")


  

######## ROBUSTNESS CHECKS FOR FIXED EFFECT ########
plm_func_RQ1 <- function(DV) {
  plm_form <- as.formula(paste(DV, "~ log_Soc_Infra_lag +
                          ae + log_GDP_Cap + cri_score + log_remittance + log_Pop + log_Pop_dens + 
                   log_hlth_Per_Cap + gov + log_External_debt + time_var"))
  
  plm_result <- plm::plm(plm_form, data = dpm_long_panel, 
                         index = c("iso3c", "periods"), model = "within", effect = "individual")
  
 # plm_result_std <- lmtest::coeftest(plm_result, save = TRUE,
  #                                   vcov = vcovHC(plm_result, #method = "arellano", 
   #                                                type = "sss", cluster = "group"))#[, ## Methods: sss, HC1, HC2, HC3  
  
  return(plm_result)
}


model_list <- list()
# Loop through each DV variable
for (var in DV_vars) {
  FE_models <- plm_func_RQ1(var)
  model_list[[var]] <- FE_models
  cat(texreg::texreg(model_list, digits = 3), "\n\n")
}


modelsummary::modelsummary(model_list, stars = TRUE, output = "latex")



######## Research Question Two FE ##########
plm_func_RQ2 <- function(DV) {
  plm_form <- as.formula(paste(DV, "~ log_Soc_Infra_lag + SSA_SocInf_ODA +  MENA_SocInf_ODA + LAC_SocInf_ODA + SCA_SocInfra_ODA + Europe_SocInfra_ODA +
                          ae + log_GDP_Cap + cri_score + log_remittance + log_Pop + log_Pop_dens + 
                          log_hlth_Per_Cap + gov + log_External_debt + time_var"))
  
  plm_result <- plm::plm(plm_form, data = dpm_long_panel, 
                         index = c("iso3c", "periods"), model = "within", effect = "individual")
  
  plm_result_std <- lmtest::coeftest(plm_result, save = TRUE,
                                     vcov = vcovHC(plm_result, #method = "arellano", 
                                                   type = "sss", cluster = "group"))#[, ## Methods: sss, HC1, HC2, HC3  
  
  return(plm_result_std)
}


model_list <- list()
# Loop through each DV variable
for (var in DV_vars) {
  FE_models <- plm_func_RQ2(var)
  model_list[[var]] <- FE_models
  cat(texreg::texreg(model_list, digits = 3), "\n\n")
}


modelsummary::modelsummary(model_list, stars = TRUE, output = "latex")



Loc_Proj_funct <- function(DV) {
  plm_form <- as.formula(paste(DV, "~ log_ODA_lag +
                          log_GDP_Cap + FDI + log_ae + log_hlth_Per_Cap + log_gov + 
                          log_External_debt + log_CRI_Score + log_Trade + log_Pop + log_Pop_dens + time_var"))
  
  plm_result <- plm::plm(plm_form, data = MisF_imp_clean_thesis_data_Indices, 
                         index = c("iso3c", "year"), model = "within", effect = "individual")
  
  plm_result_std <- lmtest::coeftest(plm_result, save = TRUE,
                                     vcov = vcovHC(plm_result, #method = "arellano", 
                                                   type = "sss", cluster = "group"))#[, ## Methods: sss, HC1, HC2, HC3  
  
  return(plm_result_std)
}


unique(MisF_imp_clean_thesis_data_Indices$region)

for (var in DV_vars) {
  # Loop through each horizon
  for (horizon in 0:10) {
    # Create a dynamic linear model for local projection
    model <- dynlm(get(paste("Outcome", i, sep = "")) ~ L(predictor, horizon), data = MisF_imp_clean_thesis_data_Indices)
    
    # Extract coefficient, standard error, and p-value
    coef_result <- summary(model)$coefficients[2, c("Estimate", "Std. Error", "Pr(>|t|)")]
    
    # Create a data frame with the results
    result_df <- data.frame(
      Outcome = paste("Outcome", i, sep = ""),
      Horizon = horizon,
      Coefficient = coef_result["Estimate"],
      Std_Error = coef_result["Std. Error"],
      P_Value = coef_result["Pr(>|t|)"]
    )
    
    # Append the result to the overall results data frame
    results <- rbind(results, result_df)
  }
}