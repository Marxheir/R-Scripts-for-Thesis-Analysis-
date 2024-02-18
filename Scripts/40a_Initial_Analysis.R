library(plm)

MisF_imp_clean_thesis_data_Indices <-  read.csv2("Raw_data/MisF_imp_clean_thesis_data_Indices.csv", sep = ";")
MisF_imp_clean_thesis_data <-  read.csv2("Raw_data/MisF_imp_clean_thesis_data.csv", sep = ";")

## Three Predictors 
# aid_Per_GNI
# oda_disb
# SOC_INF_ODA

install.packages("modelsummary")
library(texreg)

########### SUMMARY STATISTICS ###########
# Create an empty data frame to store summary statistics
summary_stats <- data.frame()

# Add all your variable names to the above vector

# Loop through each variable and calculate summary statistics
for (variable in model_inds) {
  summary_stat <- agg_data %>%
    summarize(
      Mean = mean(!!sym(variable), na.rm = TRUE),
      Median = median(!!sym(variable), na.rm = TRUE),
      SD = sd(!!sym(variable), na.rm = TRUE),
      Min = min(!!sym(variable), na.rm = TRUE),
      Max = max(!!sym(variable), na.rm = TRUE)
    ) %>%
    # Add a column for variable name
    mutate(Variable = variable)
  
  # Append the summary statistics to the data frame
  summary_stats <- bind_rows(summary_stats, summary_stat)
}

# Print the summary statistics
print(summary_stats)

# Rearrange columns
summary_stats <- summary_stats %>%
  select(Variable, Max, Min, Mean, Median, SD) %>%
  mutate(across(where(is.numeric), ~round(., 2)))


summary(agg_data$Zscore_Reprd_index)

# Export the summary statistics to a LaTeX table
kableExtra::kable(summary_stats, "latex", row.names = FALSE) 
stargazer::stargazer(summary_stats, type = "latex", title = "Descriptive Statistics")



########### Multicolinaerity #########

vif_mod <- lm(Zscore_Reprd_index ~., data = MisF_imp_clean_thesis_data_Indices[, model_inds])

vif_result <- car::vif(vif_mod)
barplot(vif_result, horiz = FALSE, col = "steelblue")

abline(h = 5, lwd = 3, lty = 2)

cor_matrix <- cor(merged_data[, model_vars])
# Basic correlation plot
corrplot::corrplot(cor_matrix, method = "circle", type = "lower", tl.col = "black", 
         diag = TRUE, order = "hclust", tl.cex = 0.7, addCoef.col = "black", 
         addCoefasPercent = TRUE, number.font = 0.07) 
# 
########### Model Estimation #########
########## RESEARCH QUESTION ONE ##########
## Model is sensitive to lag. 
## Effect became sig in first, second, third and fourth lag
merged_data$remit

model_inds <- c("oda_disb", "aid_Per_GNI", "Soc_Assistance_cov", "SOC_INF_ODA", "Cov_all_SPL", 
                "govt_hth_spnd_1", "GDP_per_cap", "Pop_tot", "ae", "External_debt_stock", 
                "Trade", "cri_score", "gov", "FDI", "Pop_densty", "remit",
                "Zscore_Reprd_index", "Zscore_Nutrit_index", "Zscore_EnvDeath_index", 
                "Zscore_HSCR_index", "Zscore_InfDis_index", "Zscore_Mental_index")

hist(log(agg_data$Zscore_Reprd_index + 2))
hist(agg_data$Zscore_InfDis_index)

hist(log(agg_data$GDP_per_cap)) ## Log 
hist(agg_data$FDI)


car::scatterplot(log(agg_data$Zscore_Reprd_index + 2) ~ log(agg_data$External_debt_stock))
car::scatterplot(agg_data$Zscore_Reprd_index ~ log(agg_data$External_debt_stock))
car::scatterplot(agg_data$Zscore_Reprd_index ~ agg_data$vdem_polyarchy)

#### One Way FE with time variable #####
model_inds <- c("oda_disb", "aid_Per_GNI", "Soc_Assistance_cov", "SOC_INF_ODA", "Cov_all_SPL", 
                "govt_hth_spnd_1", "GDP_per_cap", "Pop_tot", "ae", "External_debt_stock", 
                "Trade", "cri_score", "gov", "FDI", "Pop_densty", 
                "Zscore_Reprd_index", "Zscore_Nutrit_index", "Zscore_EnvDeath_index", 
                "Zscore_HSCR_index", "Zscore_InfDis_index", "Zscore_Mental_index")



Repr_model <- summary(plm::plm(Zscore_Reprd_index ~ plm::lag(log(oda_disb + 1), 1)  + SSA_dum + SSA_dum*plm::lag(log(oda_disb + 1), 1) +
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
                 data = merged_data, 
                 index = c("iso3c", "periods"), model = "within", effect = "individual"))



summary(plm::plm(Zscore_Reprd_index ~ plm::lag(log(oda_disb + 1), 1) +
                   log(govt_hth_spnd_1) +  log(GDP_per_cap) + log(Pop_tot) 
                 + ae + log(External_debt_stock) +
                 + log(Trade) + cri_score + log_gov + FDI + log(Pop_densty) + log_CF_LF_ratio + time_var, 
                 data = merged_data, 
                 index = c("iso3c", "periods"), model = "within", effect = "individual"))
hist(log(merged_data$FDI + 6.5))

#Zscore_Reprd_index, Zscore_InfDis_index, Zscore_Mental_index, Zscore_Nutrit_index, Zscore_EnvDeath_index, Zscore_HSCR_index 
# log_RFTP_Tr, log_BID_Tr, log_BMD_Tr, log_Malnutri_Tr, log_Env_Dth_Tr, log_HSCR_Tr, 
# log_ODA, log_Soc_Infra, log_ODA_lag, log_Soc_Infra_lag, 
# log_ae, log_bci, log_gov, log_CHS, log_hlth_Per_Cap, log_CF_LF_ratio, log_CRI_Score, log_Pop, log_Pop_dens, log_External_debt,
# log_GDP_Cap, log_Trade, log_unemp, Log_Govt_Spend_1


Nutri_model <-  plm::plm(Zscore_Nutrit_index ~ plm::lag(log(oda_disb + 1), 1)  + SSA_dum + SSA_dum*plm::lag(log(oda_disb + 1), 1) +
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
                         index = c("iso3c", "period"), model = "within", effect = "individual")

Env_model <-  plm::plm(Zscore_EnvDeath_index ~ plm::lag(log(oda_disb + 1), 1)  + SSA_dum + SSA_dum*plm::lag(log(oda_disb + 1), 1) +
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
                       index = c("iso3c", "period"), model = "within", effect = "individual")


HSCR_model <-  plm::plm(Zscore_HSCR_index ~ plm::lag(log(oda_disb + 1), 1)  + SSA_dum + SSA_dum*plm::lag(log(oda_disb + 1), 1) +
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
                        index = c("iso3c", "period"), model = "within", effect = "individual")

InfDis_model <-  plm::plm(Zscore_InfDis_index ~ plm::lag(log(oda_disb + 1), 1)  + SSA_dum + SSA_dum*plm::lag(log(oda_disb + 1), 1) +
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
                          index = c("iso3c", "period"), model = "within", effect = "individual")

Mental_model <-  plm::plm(Zscore_Mental_index ~ plm::lag(log(oda_disb + 1), 1)  + SSA_dum + SSA_dum*plm::lag(log(oda_disb + 1), 1) +
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
                          index = c("iso3c", "period"), model = "within", effect = "individual")



library(kableExtra)
library(broom)
plmtest(Repr_model, data = agg_data, effect = "twoways", 
        type = "ghm") ## Testing the presence of Individual and time effect

plmtest(HSCR_model, data = agg_data, effect = "twoways", 
        type = "ghm") ## Testing the presence of Individual and time effect
plmtest(Env_model, data = agg_data, effect = "twoways", 
        type = "ghm") ## Testing the presence of Individual and time effect
plmtest(Mental_model, data = agg_data, effect = "twoways", 
        type = "ghm") ## Testing the presence of Individual and time effect
plmtest(InfDis_model, data = agg_data, effect = "twoways", 
        type = "ghm") ## Testing the presence of Individual and time effect
plmtest(Nutri_model, data = agg_data, effect = "twoways", 
        type = "ghm") ## Testing the presence of Individual and time effect

# Assume "model" is your plm model
summary_table <- summary(Repr_model, robust = "vcovHC")

plm::pFtest(one_FE_Repro, Pooling_Repro) 
plm::phtest(one_FE_Repro, rand_FE_Repro) ## Hausman Test between random and FE effect
lmtest::bptest(Repr_model, studentize = T) ## Testing for Heteroskedacity with Brausch Pagan
lmtest::bptest(HSCR_model, studentize = T) ## Testing for Heteroskedacity with Brausch Pagan
lmtest::bptest(Env_model, studentize = T) ## Testing for Heteroskedacity with Brausch Pagan
lmtest::bptest(Mental_model, studentize = T) ## Testing for Heteroskedacity with Brausch Pagan
lmtest::bptest(InfDis_model, studentize = T) ## Testing for Heteroskedacity with Brausch Pagan
lmtest::bptest(Nutri_model, studentize = T) ## Testing for Heteroskedacity with Brausch Pagan

# There is strong evidence for the heteroskedacity presence, therefore, we implemented roburst standard error 
## Since the data spans for 22 years, there is need for serial correlation of residuals. 
## Serial Correlation underestimates standard error and elevate the R square. We use Breusch_Godfrey and Wooldridge test
plm::pbgtest(Two_FE_Repro, order = 2 
)
pwtest(Model_form_Repr, data = MisF_imp_clean_thesis_data_Indices)
## The test shows the presence of serial correlation

### To address the serial correlation of error term, we implement clustered standard error 
library(sandwich)
library(plm)
library(stats)

#coeftest(model, 
#         vcov = vcovHC(model), method = "arellano")

Repr_model_std <- lmtest::coeftest(Repr_model, 
                                   vcov = vcovHC(Repr_model, #method = "arellano", 
                                                 type = "HC3", cluster = "group"))#[, ## Methods: sss, HC1, HC2, HC3  
#c(1, 2, 3, 4)]
Env_model_std <- lmtest::coeftest(Env_model, 
                                  vcov = vcovHC(Env_model, 
                                                type = "HC1"))#[, ## Methods: sss, HC1, HC2, HC3  



Nutri_model_std <- lmtest::coeftest(Nutri_model, 
                                    vcov = vcovHC(Nutri_model, 
                                                  type = "HC1"))#[, ## Methods: sss, HC1, HC2, HC3  
#c(1, 2, 3, 4)]
InfDis_model_std <- lmtest::coeftest(InfDis_model, 
                                     vcov = vcovHC(InfDis_model, 
                                                   type = "HC1"))#[, ## Methods: sss, HC1, HC2, HC3  

HSCR_model_std <- lmtest::coeftest(HSCR_model, 
                                   vcov = vcovHC(HSCR_model, 
                                                 type = "HC1"))#[, ## Methods: sss, HC1, HC2, HC3  



Mental_model_std <- lmtest::coeftest(Mental_model, 
                                     vcov = vcovHC(Mental_model, 
                                                   type = "HC1"))#[, ## Methods: sss, HC1, HC2, HC3  

library(stargazer)

# Assuming "model" is your regression model
model_list_std <- list(Repr_model_std, Nutri_model_std, InfDis_model_std, 
                       HSCR_model_std, Env_model_std, Mental_model_std)
model_list_Raw_FE <- list(Repr_model, Nutri_model, InfDis_model, 
                          HSCR_model, Env_model, Mental_model)

stargazer(model_list_std, digits = 3)
modelsummary::modelsummary(model_list_Raw_FE, stars = TRUE, output = "latex", file = "models.tex")



########### RESEARCH QUESTION TWO ##############
## Regional heterogeneity of ODA effect
Repr_model <- plm::plm(Zscore_Reprd_index ~ plm::lag(log(SOC_INF_ODA + 1), 1)  +
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
                       index = c("iso3c", "period"), model = "within", effect = "individual")



Nutri_model <-  plm::plm(Zscore_Nutrit_index ~ plm::lag(log(SOC_INF_ODA + 1), 1)  + 
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
                         index = c("iso3c", "period"), model = "within", effect = "individual")


Env_model <-  plm::plm(Zscore_EnvDeath_index ~ plm::lag(log(SOC_INF_ODA+ 1), 1)  + 
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
                       index = c("iso3c", "period"), model = "within", effect = "individual")


HSCR_model <-  plm::plm(Zscore_HSCR_index ~ plm::lag(log(SOC_INF_ODA + 1), 1)  + 
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
                        index = c("iso3c", "period"), model = "within", effect = "individual")


InfDis_model <-  plm::plm(Zscore_InfDis_index ~ plm::lag(log(SOC_INF_ODA + 1), 1)  + 
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
                          index = c("iso3c", "period"), model = "within", effect = "individual")


Mental_model <-  plm::plm(Zscore_Mental_index ~ plm::lag(log(SOC_INF_ODA + 1), 1)  + 
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
                          index = c("iso3c", "period"), model = "within", effect = "individual")


library(kableExtra)
library(broom)
plmtest(Repr_model, data = agg_data, effect = "twoways", 
        type = "ghm") ## Testing the presence of Individual and time effect

plmtest(HSCR_model, data = agg_data, effect = "twoways", 
        type = "ghm") ## Testing the presence of Individual and time effect
plmtest(Env_model, data = agg_data, effect = "twoways", 
        type = "ghm") ## Testing the presence of Individual and time effect
plmtest(Mental_model, data = agg_data, effect = "twoways", 
        type = "ghm") ## Testing the presence of Individual and time effect
plmtest(InfDis_model, data = agg_data, effect = "twoways", 
        type = "ghm") ## Testing the presence of Individual and time effect
plmtest(Nutri_model, data = agg_data, effect = "twoways", 
        type = "ghm") ## Testing the presence of Individual and time effect

# Assume "model" is your plm model
summary_table <- summary(Repr_model, robust = "vcovHC")

plm::pFtest(one_FE_Repro, Pooling_Repro) 
plm::phtest(one_FE_Repro, rand_FE_Repro) ## Hausman Test between random and FE effect
lmtest::bptest(Repr_model, studentize = T) ## Testing for Heteroskedacity with Brausch Pagan
lmtest::bptest(HSCR_model, studentize = T) ## Testing for Heteroskedacity with Brausch Pagan
lmtest::bptest(Env_model, studentize = T) ## Testing for Heteroskedacity with Brausch Pagan
lmtest::bptest(Mental_model, studentize = T) ## Testing for Heteroskedacity with Brausch Pagan
lmtest::bptest(InfDis_model, studentize = T) ## Testing for Heteroskedacity with Brausch Pagan
lmtest::bptest(Nutri_model, studentize = T) ## Testing for Heteroskedacity with Brausch Pagan

# There is strong evidence for the heteroskedacity presence, therefore, we implemented roburst standard error 
## Since the data spans for 22 years, there is need for serial correlation of residuals. 
## Serial Correlation underestimates standard error and elevate the R square. We use Breusch_Godfrey and Wooldridge test
plm::pbgtest(Two_FE_Repro, order = 2 
)
pwtest(Model_form_Repr, data = MisF_imp_clean_thesis_data_Indices)
## The test shows the presence of serial correlation

### To address the serial correlation of error term, we implement clustered standard error 
library(sandwich)
library(plm)
library(stats)
Repr_model_std <- lmtest::coeftest(Repr_model, 
                                   vcov = vcovHC(Repr_model, 
                                                 type = "HC1"))#[, ## Methods: sss, HC1, HC2, HC3  
#c(1, 2, 3, 4)]
Env_model_std <- lmtest::coeftest(Env_model, 
                                  vcov = vcovHC(Env_model, 
                                                type = "HC1"))#[, ## Methods: sss, HC1, HC2, HC3  



Nutri_model_std <- lmtest::coeftest(Nutri_model, 
                                    vcov = vcovHC(Nutri_model, 
                                                  type = "HC1"))#[, ## Methods: sss, HC1, HC2, HC3  
#c(1, 2, 3, 4)]
InfDis_model_std <- lmtest::coeftest(InfDis_model, 
                                     vcov = vcovHC(InfDis_model, 
                                                   type = "HC1"))#[, ## Methods: sss, HC1, HC2, HC3  

HSCR_model_std <- lmtest::coeftest(HSCR_model, 
                                   vcov = vcovHC(HSCR_model, 
                                                 type = "HC1"))#[, ## Methods: sss, HC1, HC2, HC3  



Mental_model_std <- lmtest::coeftest(Mental_model, 
                                     vcov = vcovHC(Mental_model, 
                                                   type = "HC1"))#[, ## Methods: sss, HC1, HC2, HC3  

library(stargazer)

# Assuming "model" is your regression model
model_list_std <- list(Repr_model_std, Nutri_model_std, InfDis_model_std, 
                       HSCR_model_std, Env_model_std, Mental_model_std)
model_list_Raw_FE <- list(Repr_model, Nutri_model, InfDis_model, 
                          HSCR_model, Env_model, Mental_model)
 
stargazer(model_list_std, digits = 3)
modelsummary::modelsummary(model_list_Raw_FE, stars = TRUE, output = "latex", file = "models.tex")


hist(agg_data$ae)

########### IMPLEMENTING GMM AB SYSTEM METHOD ########
library(lmtest)
library(tidyverse)

grangertest(Zscore_Reprd_index ~ lag(Zscore_Reprd_index), data = agg_data)
agg_data$SOC_INF_ODA


Repr_model <- summary(plm::pgmm(Zscore_Reprd_index ~ plm::lag(Zscore_Reprd_index, 1) + plm::lag(log(SOC_INF_ODA + 1), 1) + 
                    log(GDP_per_cap) + log(govt_hth_spnd_1)  + log(Pop_tot) + log(External_debt_stock) + 
                    ae + gov + log(Pop_densty) | 
                    plm::lag(Zscore_Reprd_index, 2:99)  + 
                    time_var + plm::lag(log(govt_hth_spnd_1), 2:99) +  plm::lag(gov, 2:99) + plm::lag(Pop_densty, 2:99) , #+ HealthResearch_ODA, 
                       data = agg_data, 
                       index = c("iso3c", "period"), model = "twosteps", effect = "twoways", 
                  transformation = "ld"))



Nutri_model <- plm::pgmm(Zscore_Nutrit_index ~ plm::lag(Zscore_Nutrit_index, 1) + plm::lag(log(SOC_INF_ODA + 1)) + 
                          log(GDP_per_cap) + log(govt_hth_spnd_1)  + log(Pop_tot) + log(External_debt_stock) + 
                          ae + gov + log(Pop_densty) | 
                          plm::lag(Zscore_Nutrit_index, 2:99)  + 
                          time_var + plm::lag(log(govt_hth_spnd_1), 2:99) +  plm::lag(gov, 2:99) + plm::lag(Pop_densty, 2:99) , #+ HealthResearch_ODA, 
                        data = agg_data, 
                        index = c("iso3c", "period"), model = "twosteps", effect = "twoways", 
                        transformation = "ld")

HSCR_model <- plm::pgmm(Zscore_HSCR_index ~ plm::lag(Zscore_HSCR_index, 1) + plm::lag(log(SOC_INF_ODA + 1)) + 
                         log(GDP_per_cap) + log(govt_hth_spnd_1)  + log(Pop_tot) + log(External_debt_stock) + 
                         ae + gov + log(Pop_densty) | 
                         plm::lag(Zscore_HSCR_index, 2:99)  + 
                         time_var + plm::lag(log(govt_hth_spnd_1), 2:99) +  plm::lag(gov, 2:99) + plm::lag(Pop_densty, 2:99) , #+ HealthResearch_ODA, 
                       data = agg_data, 
                       index = c("iso3c", "period"), model = "twosteps", effect = "twoways", 
                       transformation = "ld")

Env_model <- summary(plm::pgmm(Zscore_EnvDeath_index ~ plm::lag(Zscore_EnvDeath_index, 1) + plm::lag(log(oda_disb+ 1)) + 
                          log(GDP_per_cap) + log(govt_hth_spnd_1)  + log(Pop_tot) + log(External_debt_stock) + 
                          ae + gov + log(Pop_densty) | 
                          plm::lag(Zscore_EnvDeath_index, 2:99)  + 
                           plm::lag(UN_voting, 2:99) + plm::lag(log(govt_hth_spnd_1), 2:99) +  plm::lag(gov, 2:99) + plm::lag(Pop_densty, 2:99) , #+ HealthResearch_ODA, 
                        data = agg_data, 
                        index = c("iso3c", "period"), model = "twosteps", effect = "twoways", 
                        transformation = "ld"))

agg_data$UN_voting
InfDis_model <- plm::pgmm(Zscore_InfDis_index ~ plm::lag(Zscore_InfDis_index, 1) + plm::lag(log(SOC_INF_ODA + 1)) + 
                         log(GDP_per_cap) + log(govt_hth_spnd_1)  + log(Pop_tot) + log(External_debt_stock) + 
                         ae + gov + log(Pop_densty) | 
                         plm::lag(Zscore_InfDis_index, 2:99)  + 
                         time_var + plm::lag(log(govt_hth_spnd_1), 2:99) +  plm::lag(gov, 2:99) + plm::lag(Pop_densty, 2:99) , #+ HealthResearch_ODA, 
                       data = agg_data, 
                       index = c("iso3c", "period"), model = "twosteps", effect = "twoways", 
                       transformation = "ld")


Mental_model <- plm::pgmm(Zscore_Mental_index ~ plm::lag(Zscore_Mental_index, 1) + plm::lag(log(SOC_INF_ODA + 1)) + 
                            log(GDP_per_cap) + log(govt_hth_spnd_1)  + log(Pop_tot) + log(External_debt_stock) + 
                            ae + gov + log(Pop_densty) | 
                            plm::lag(Zscore_Mental_index, 2:99)  + 
                            time_var + plm::lag(log(govt_hth_spnd_1), 2:99) +  plm::lag(gov, 2:99) + plm::lag(Pop_densty, 2:99) , #+ HealthResearch_ODA, 
                          data = agg_data, 
                          index = c("iso3c", "period"), model = "twosteps", effect = "twoways", 
                          transformation = "ld")




model_list_Raw_FE <- list(Repr_model, Nutri_model, InfDis_model, 
                          HSCR_model, Env_model, Mental_model)

summary(Mental_model)
stargazer(model_list_Raw_FE, digits = 3)
modelsummary::modelsummary(model_list_Raw_FE, stars = TRUE, output = "latex", file = "models.tex")

library(texreg)
texreg(model_list_Raw_FE)
GMM_models_list <- model_list_Raw_FE







 

+ ae + plm::lag(log(govt_hth_spnd_1), 2:99)
  ae  +  covid_stringency  + bci + log(External_debt_stock) + Pop_densty + 
  resource_rent

plm::plm(Zscore_InfDis_index ~ plm::lag(log(SOC_INF_ODA + 1), 1)  + 
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
         index = c("iso3c", "period"), model = "within", effect = "individual")

pool_mod <- plm::plm(Zscore_Reprd_index ~ plm::lag(Zscore_Reprd_index) + plm::lag(log(oda_disb + 1)) + 
                    plm::lag(log(GDP_per_cap), 1) + plm::lag(log(govt_hth_spnd_1), 1) + 
                    ae  + bci + log(External_debt_stock) + Pop_densty, #+ HealthResearch_ODA, 
                  data = agg_data, 
                  index = c("iso3c", "period"), model = "pooling")


FE_mod <- plm::plm(Zscore_Reprd_index ~ plm::lag(Zscore_Reprd_index) + plm::lag(log(oda_disb + 1)) + 
                       plm::lag(log(GDP_per_cap), 1) + plm::lag(log(govt_hth_spnd_1), 1) + 
                       ae  + bci + log(External_debt_stock) + Pop_densty, #+ HealthResearch_ODA, 
                     data = agg_data, 
                     index = c("iso3c", "period"), model = "within")

Gmm_mod <- plm::pgmm(Zscore_Reprd_index ~ plm::lag(Zscore_Reprd_index) + plm::lag(log(oda_disb + 1)) + 
                       plm::lag(log(GDP_per_cap), 1) + plm::lag(log(govt_hth_spnd_1), 1) + 
                       ae  + bci + log(External_debt_stock) + Pop_densty | plm::lag(Zscore_Reprd_index, 2:99) , #+ HealthResearch_ODA, 
                     data = agg_data, 
                     index = c("iso3c", "period"), model = "twosteps", effect = "twoways")


summary(pool_mod)
summary(FE_mod)
summary(Gmm_mod)



phtest(your_model, type = "Hansen")

Nutri_model <-  plm::plm(Zscore_Nutrit_index ~ plm::lag(log(SOC_INF_ODA + 1), 1)  + SSA_dum  + (SSA_dum * plm::lag(log(SOC_INF_ODA + 1), 1)) +
                           govt_hth_spnd_1 +  log(GDP_per_cap) +  
                           #+ Pol_stabty_Est + 
                           Female_educ + log(Trade)  + # clean_cook_enrgy: multicollinearity + 
                           covid_stringency + cri_score + ae + 
                           + resource_rent   + Pop_densty +
                           bci + #undp_hdi + 
                           # pub_invest, # + #Water_access +  
                           #Sanitaion_access  +
                           #Total_debt_Service + 
                           External_debt_stock  + time_var, #+ HealthResearch_ODA, 
                         data = agg_data, 
                         index = c("iso3c", "period"), model = "within", effect = "individual")










######### ARCHIVE ###########



?lmtest::coeftest
res <- residuals(model_trn)
yhat <- fitted(model_trn)
plot(acf(yhat))


###
vif_mod <- lm(Zscore_Reprd_index ~., data = MisF_imp_clean_thesis_data_Indices[, model_inds])

vif_result <- car::vif(vif_mod)
barplot(vif_result, horiz = FALSE, col = "steelblue")

abline(h = 5, lwd = 3, lty = 2)

MisF_imp_clean_thesis_data_Indices$vdem_polyarchy
base_inds <- c("oda_disb", "Pop_densty", "Pop_tot", "Pop_65abv", "Water_access", "Sanitaion_access", "Total_debt_Service", "External_debt_stock", "wparl", 
               "vdem_polyarchy", "govt_hth_spnd_1", "Urb_pop_rate", "GDP_per_cap", 
                   "Pol_stabty_Est", "CF_LF_ratio", "Pop_0to14", "Agric_emp", "Female_educ", 
                   "Trade", "FDI", "Inflation", "clean_cook_enrgy", "covid_stringency", 
                   "cri_score", "ae", "emdat_ndeath", "resource_rent", "gov", "bci", "undp_hdi", "pub_invest")

## Remove variables 

base_inds_1 <- c("aid_Per_GNI", "oda_disb", "Pop_densty", "Pop_tot", 
                  "Total_debt_Service", "External_debt_stock", "wparl", 
                 "vdem_polyarchy", "govt_hth_spnd_1", "Urb_pop_rate", "GDP_per_cap", 
                 "CF_LF_ratio", "Female_educ", "GNI", 
                 "Trade", "FDI", "Inflation", "covid_stringency", 
                 "cri_score", "ae", "emdat_ndeath", "resource_rent", "gov", "pub_invest")
rm(base_inds)
corr_matrix_base_inds <- cor(MisF_imp_clean_thesis_data_Indices[, base_inds_1])

corrplot::corrplot(corr_matrix_base_inds, method = "circle", tl.cex = 0.7, type = "lower", diag = TRUE,
                   order = "hclust", # or "FPC"
                   tl.col = "black",
                   addCoef.col = "black", addCoefasPercent = TRUE, number.font = 0.4
)


#plm::plmtest()


########### Model Estimation #########
########## RESEARCH QUESTION ONE ##########
## Model is sensitive to lag. 
## Effect became sig in first, second, third and fourth lag
Model_form_Repr <- Zscore_Reprd_index ~ plm::lag(log(oda_disb + 1), 4) + 
           govt_hth_spnd_1 + Urb_pop_rate + 
           log(GDP_per_cap) + Pol_stabty_Est + log(CF_LF_ratio) + log(Pop_tot) + 
           Agric_emp + Female_educ + log(Trade) + FDI + Inflation + # clean_cook_enrgy: multicollinearity + 
           covid_stringency + cri_score + ae + 
           emdat_ndeath + resource_rent + 
           gov + #undp_hdi + 
           pub_invest + Water_access +  
           #Sanitaion_access +
           #Total_debt_Service + 
           External_debt_stock


rand_FE_Repro <- plm::plm(Zscore_Reprd_index ~ plm::lag(log(oda_disb + 1), 4) + 
                   govt_hth_spnd_1 + Urb_pop_rate + 
                   log(GDP_per_cap) + Pol_stabty_Est + log(CF_LF_ratio) + log(Pop_tot) + 
                   Agric_emp + Female_educ + log(Trade) + FDI + Inflation + # clean_cook_enrgy: multicollinearity + 
                   covid_stringency + cri_score + ae + 
                   emdat_ndeath + resource_rent + 
                   gov + #undp_hdi + 
                   pub_invest + Water_access +  
                   #Sanitaion_access +
                   #Total_debt_Service + 
                   External_debt_stock, 
                 data = MisF_imp_clean_thesis_data_Indices, 
                 index = c("iso3c", "year"), model = "random", effect = "individual")


plm::pFtest(one_FE_Repro, Two_FE_Repro)

Model_form_InfDis <- Zscore_InfDis_index ~ plm::lag(log(oda_disb + 1), 4) + 
  govt_hth_spnd_1 + Urb_pop_rate + 
  log(GDP_per_cap) + Pol_stabty_Est + log(CF_LF_ratio) + log(Pop_tot) + 
  Agric_emp + Female_educ + log(Trade) + FDI + Inflation + # clean_cook_enrgy: multicollinearity + 
  covid_stringency + cri_score + ae + 
  emdat_ndeath + resource_rent + 
  gov + #undp_hdi + 
  pub_invest + Water_access +  
  #Sanitaion_access +
  #Total_debt_Service + 
  External_debt_stock


Model_form_HSCR <- Zscore_HSCR_index ~ plm::lag(log(oda_disb + 1), 4) + 
  govt_hth_spnd_1 + Urb_pop_rate + 
  log(GDP_per_cap) + Pol_stabty_Est + log(CF_LF_ratio) + log(Pop_tot) + 
  Agric_emp + Female_educ + log(Trade) + FDI + Inflation + # clean_cook_enrgy: multicollinearity + 
  covid_stringency + cri_score + ae + 
  emdat_ndeath + resource_rent + 
  gov + #undp_hdi + 
  pub_invest + Water_access +  
  #Sanitaion_access +
  #Total_debt_Service + 
  External_debt_stock


Model_form_Mental <- Zscore_Mental_index ~ plm::lag(log(oda_disb + 1), 4) + 
  govt_hth_spnd_1 + Urb_pop_rate + 
  log(GDP_per_cap) + Pol_stabty_Est + log(CF_LF_ratio) + log(Pop_tot) + 
  Agric_emp + Female_educ + log(Trade) + FDI + Inflation + # clean_cook_enrgy: multicollinearity + 
  covid_stringency + cri_score + ae + 
  emdat_ndeath + resource_rent + 
  gov + #undp_hdi + 
  pub_invest + Water_access +  
  #Sanitaion_access +
  #Total_debt_Service + 
  External_debt_stock


Model_form_Nutri <- Zscore_Nutrit_index ~ plm::lag(log(oda_disb + 1), 4) + 
  govt_hth_spnd_1 + Urb_pop_rate + 
  log(GDP_per_cap) + Pol_stabty_Est + log(CF_LF_ratio) + log(Pop_tot) + 
  Agric_emp + Female_educ + log(Trade) + FDI + Inflation + # clean_cook_enrgy: multicollinearity + 
  covid_stringency + cri_score + ae + 
  emdat_ndeath + resource_rent + 
  gov + #undp_hdi + 
  pub_invest + Water_access +  
  #Sanitaion_access +
  #Total_debt_Service + 
  External_debt_stock


Model_form_EnvDeath <- Zscore_EnvDeath_index ~ plm::lag(log(oda_disb + 1), 4) + 
  govt_hth_spnd_1 + Urb_pop_rate + 
  log(GDP_per_cap) + Pol_stabty_Est + log(CF_LF_ratio) + log(Pop_tot) + 
  Agric_emp + Female_educ + log(Trade) + FDI + Inflation + # clean_cook_enrgy: multicollinearity + 
  covid_stringency + cri_score + ae + 
  emdat_ndeath + resource_rent + 
  gov + #undp_hdi + 
  pub_invest + Water_access +  
  #Sanitaion_access +
  #Total_debt_Service + 
  External_debt_stock

Model_form_CHS <- CHS_25perc ~ plm::lag(log(oda_disb + 1), 4) + 
  govt_hth_spnd_1 + Urb_pop_rate + 
  log(GDP_per_cap) + Pol_stabty_Est + log(CF_LF_ratio) + log(Pop_tot) + 
  Agric_emp + Female_educ + log(Trade) + FDI + Inflation + # clean_cook_enrgy: multicollinearity + 
  covid_stringency + cri_score + ae + 
  emdat_ndeath + resource_rent + 
  gov + #undp_hdi + 
  pub_invest + Water_access +  
  #Sanitaion_access +
  #Total_debt_Service + 
  External_debt_stock

#### Estimation 
plm::plm(Zscore_Reprd_index ~ plm::lag(log(oda_disb + 1), 4) + 
                   govt_hth_spnd_1 + Urb_pop_rate + 
                   log(GDP_per_cap) + Pol_stabty_Est + log(CF_LF_ratio) + log(Pop_tot) + 
                   Agric_emp + Female_educ + log(Trade) + FDI + Inflation + # clean_cook_enrgy: multicollinearity + 
                   covid_stringency + cri_score + ae + 
                   emdat_ndeath + resource_rent + 
                   gov + #undp_hdi + 
                   pub_invest + Water_access +  
                   #Sanitaion_access +
                   #Total_debt_Service + 
                   External_debt_stock, 
                 data = MisF_imp_clean_thesis_data_Indices, 
                 index = c("iso3c", "year"), model = "within", effect = "twoways")



#c(1, 2, 3, 4)]


MisF_imp_clean_thesis_data <- MisF_imp_clean_thesis_data %>%
  group_by(iso3c) %>%
  fill(country, region) # To fill the country and region for 2022


library(texreg)
screenreg(Repr_model, custom.model.names = FALSE, custom.coef.names = rownames(coef(Repr_model)),
          custom.model.names.label = "Model", single.row = TRUE, digits = 3,
          override.se = list(vcovHC(Repr_model, type = "HC1")))


# Assuming "coef_table" is your table of coefficients
coef_table_with_dollars <- format(coef_table, digits = 2, nsmall = 2)
coef_table_with_dollars <- gsub("([-0-9.]+)", "\\$$1\\$", coef_table_with_dollars)
write.table(coef_table_with_dollars, "output_table.txt")
