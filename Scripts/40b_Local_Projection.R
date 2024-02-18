
MisF_imp_clean_thesis_data_Indices <- MisF_imp_clean_thesis_data_Indices %>% 
  group_by(iso3c) %>%
  mutate(time_var = row_number())

MisF_imp_clean_thesis_data_Indices <- MisF_imp_clean_thesis_data_Indices %>% 
mutate( 
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




#### One Way FE with time variable #####
model_inds <- c("oda_disb", "aid_Per_GNI", "Soc_Assistance_cov", "SOC_INF_ODA", "Cov_all_SPL", 
                "govt_hth_spnd_1", "GDP_per_cap", "Pop_tot", "ae", "External_debt_stock", 
                "Trade", "cri_score", "gov", "FDI", "Pop_densty", 
                "Zscore_Reprd_index", "Zscore_Nutrit_index", "Zscore_EnvDeath_index", 
                "Zscore_HSCR_index", "Zscore_InfDis_index", "Zscore_Mental_index")


# Define DV_vars and horizon
DV_vars <- c("Zscore_Reprd_index", "Zscore_InfDis_index", "Zscore_Mental_index",
             "Zscore_Nutrit_index", "Zscore_EnvDeath_index", "Zscore_HSCR_index")

horizon <- 0:8

# Initialize an empty list to store results
model_list <- list()
names(MisF_imp_clean_thesis_data_Indices)
# Define the local_Proj_funct function
local_Proj_funct <- function(horizon) {
  plm_formula <- as.formula(paste("plm::lead(Zscore_HSCR_index,", horizon, ") ~  log_ODA +
                          log_GDP_Cap + log(remit + 1) + ae + log_hlth_Per_Cap + gov +
                          log_External_debt + cri_score + log_Pop + log_Pop_dens +
                          time_var"))
  
  plm_result <- plm::plm(plm_formula, data = MisF_imp_clean_thesis_data_Indices,
                         index = c("iso3c", "year"), model = "within", effect = "individual")
  
  # Use robust standard errors
  plm_result <- lmtest::coeftest(plm_result, save = TRUE,
                                 vcov = vcovHC(plm_result, type = "sss", cluster = "group"))
  
  return(print(plm_result[1, ]))
}

library(plm)
library(lmtest)
library(sandwich)


local_projection_results <- data.frame(
  Horizons = rep(0:8, 6),
  Var = rep(c("RFTP", "BID", "BMP", "Malnutrition", "Env Death", "HSCR"), each = 9),
  Estimates = c(-0.003765085, -0.015899074, -0.018260365, -0.019226552, -0.021218161, -0.020976703, -0.019433321, -0.020864698, -0.015710505, 
                7.389784e-05, -0.005611365, -0.010812680, -0.009324973, -0.012508278, -0.012791260, -0.010902851, -0.013653748, -0.010484673, 
                -0.005948746, -0.004206465, -0.003749098, -0.001940441, -0.004132661, -0.004613305, -0.006586897, -0.01393344, -0.02089349, 
                0.004221039, -0.006579386, -0.010073076, -0.009671523, -0.010158632, -0.007951559, -0.005478156, -0.005896628, 0.001208323, 
                -0.009309931, -0.01681527, -0.02062939, -0.01962301, -0.01949337, -0.02112878, -0.022758703, -0.0284363361, -0.024547832,
                -0.01592305, -8.358991e-05, 0.002041000, 0.003773357, 0.007525733, 0.003866753, 0.004403729, 0.007107307, -0.0007684608),
  
  Std_Error = c(0.005181776, 0.007372277, 0.007721974, 0.007485363, 0.007512988, 0.007352575, 0.007089354, 0.006985527, 0.007161834, 
                3.901640e-03, 0.005539721, 0.005584693, 0.005236147, 0.004817405, 0.004850954, 0.004761002, 0.005039030, 0.005958513, 
                0.008002560, 0.008186579, 0.008803672, 0.008780876, 0.008519639, 0.008158582, 0.008774265, 0.01059566, 0.01055394, 
                0.005505688, 0.006031063, 0.006216539, 0.006228355, 0.005841033, 0.005599856, 0.005963043, 0.006671582, 0.006775905, 
                0.009718430, 0.01042727, 0.01050330, 0.01086776, 0.01066968, 0.00950430, 0.008719365, 0.0080203883, 0.007729183, 
                0.00696880, 7.591782e-03, 0.007838052, 0.008221944, 0.008386327, 0.008354420, 0.007500318, 0.007286775, 0.0077655241),
  
  Pr_t = c(0.467526692, 0.031119464, 0.018113053, 0.010268170, 0.004778452, 0.004370052, 0.006172253, 0.002852887, 0.028384412, 
           9.848901e-01, 0.311177673, 0.052955359, 0.075048920, 0.009475151, 0.008424354, 0.022115897, 0.006793815, 0.078637862, 
           0.457324506, 0.607414293, 0.670246275, 0.825121626, 0.627667010, 0.571820123, 0.452911529, 0.18865548, 0.04788597,
           0.443338537, 0.275401186, 0.105269105, 0.120588414, 0.082128674, 0.155755921, 0.358363612, 0.376887145, 0.858486077, 
           0.338156479, 0.10693605, 0.04962164, 0.07109466, 0.06782394, 0.02630785, 0.009113556, 0.0004008454, 0.001517708, 
           0.02238757, 9.912158e-01, 0.794577754, 0.646317811, 0.369604087, 0.643523579, 0.557172069, 0.329494104, 0.9211822663)
)

local_Proj_funct(horizon = 8)

Local_Proj <- ggplot(local_projection_results, aes(x = Horizons, y = Estimates, fill = Var)) +
  geom_ribbon(
    aes(ymin = Estimates - 1.96 * Std_Error, ymax = Estimates + 1.96 * Std_Error),
    alpha = 0.3,
    color = "black",
    linetype = "dotted"
  ) +
  geom_line() +
  geom_point() +
  geom_text(data = filter(local_projection_results, Pr_t < 0.1), aes(label = ifelse(Pr_t < 0.001, "***", ifelse(Pr_t < 0.005, "**", ifelse(Pr_t < 0.01, "*", "")))), vjust = -1) +
  labs(#title = "Local Projection Results",
    x = "Horizons",
    y = "Impulse Response Functions of Health Outcome to Log(ODA)",
    fill = "Variable") +
  facet_wrap(~Var, scales = "fixed", ncol = 3) +
  theme_minimal() +
  theme(legend.position = "none", 
        strip.text = element_text(color = "black", face = "bold")) + 
  geom_hline(yintercept = 0, linetype = "dotdash",  color = "black") +
  scale_x_continuous(breaks = seq(0, 8, 1))

ggsave("Plots/Local_Projt.pdf", plot = Local_Proj, width = 7, height = 5, dpi = 300)



################## ARCHIVE OF LOCAL PROJECTION #############

horizon
# Initialize an empty data frame to store results
local_projection_results <- data.frame(
  Horizons = rep(0:8, each  = length(DV_vars)), 
  Var = rep(DV_vars, times = 9)
)
local_projection_results
#local_projection_results <- data.frame(
 # Outcome_var = character(),
  #Coefficient = numeric(),
  #Std_Error = numeric(),
  #P_Value = numeric(),
  #stringsAsFactors = FALSE
#)


# Loop through each DV variable and horizon
for (var in DV_vars) {
  for (h in horizon) {
    result <- local_Proj_funct(var, h)
    local_projection_results[local_projection_results$Var == var & local_projection_results$Horizons == h, "Coefficient"] <- FE_models
#    local_projection_results <- rbind(local_projection_results, result)
  }
}

MisF_imp_clean_thesis_data_Indices
# Print or use local_projection_results as needed
print(local_projection_results)

#### Zscore_Reprd_index
plm::lead
#c(1, 2, 3, 4)]

Repr_model <- plm::plm(plm::lead(Zscore_Reprd_index, 10) ~ log(oda_disb + 1) +
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
                       index = c("iso3c", "year"), model = "within", effect = "individual")


lmtest::coeftest(Repr_model, vcov = vcovHC(Repr_model, 
                                           type = "HC1"))#[, ## Methods: sss, HC1, HC2, HC3  


# Assuming your local projection results are stored in a data frame called "local_projection_results"

# Load the necessary libraries
library(ggplot2)
library(dplyr)

# Your local projection results (replace this with your actual data)
loc_Proj_results <- data.frame(
  Horizons = 0:8,
  Var = rep("Reproduction", 9),
  Estimates = c(-0.00589860, -0.017497, -0.01960150, -0.02029182, -0.022307, -0.02238596, -0.02132187, -0.02284208, -0.01887854),
  Std_Error = c(0.00512692, 0.0077266, 0.00797890, 0.00777425, 0.0077549, 0.00766739, 0.00736314, 0.00737094, 0.00740319),
  t_value = c(-1.1505, -2.2645, -2.4567, -2.6101, -2.8765, -2.9196, -2.8958, -3.0989, -2.5501),
  Pr_t = c(0.2500183, 0.023615, 0.01408, 0.0091, 0.004055, 0.003537, 0.003818, 0.001967, 0.010844)
)

# Create a ggplot
plot <- ggplot(loc_Proj_results, aes(x = Horizons, y = Estimates)) +
  geom_line() +
  geom_errorbar(aes(ymin = Estimates - 1.96 * Std_Error, ymax = Estimates + 1.96 * Std_Error), width = 0.2) +
  geom_point() +
  geom_text(data = filter(loc_Proj_results, Pr_t < 0.05), aes(label = ifelse(Pr_t < 0.001, "***", ifelse(Pr_t < 0.01, "**", ifelse(Pr_t < 0.05, "*", "")))), vjust = -1) +
  labs(title = "Local Projection Results",
       x = "Horizon",
       y = "Estimates") +
  theme_minimal()


plot <- ggplot(loc_Proj_results, aes(x = Horizons, y = Estimates)) +
  geom_line() +
  geom_ribbon(aes(ymin = Estimates - 1.96 * Std_Error, ymax = Estimates + 1.96 * Std_Error, fill = factor(Pr_t < 0.05)), alpha = 0.3) +
  geom_point() +
  geom_text(data = filter(loc_Proj_results, Pr_t < 0.05), aes(label = ifelse(Pr_t < 0.001, "***", ifelse(Pr_t < 0.01, "**", ifelse(Pr_t < 0.05, "*", "")))), vjust = -1) +
  labs(title = "Local Projection Results",
       x = "Horizon",
       y = "Estimates") +
  theme_minimal() +
  guides(fill = "none")  # Hide legend for significance


library(ggdist)

# Create a ggplot with uncertainty intervals
plot <- ggplot(loc_Proj_results, aes(x = Horizons, y = Estimates)) +
  geom_line() +
  geom_ribbon(aes(ymin = Estimates - 1.96 * Std_Error, ymax = Estimates + 1.96 * Std_Error),
              fill = "grey", color = "black", linetype = "dotted", alpha = 0.3) +
  geom_point() +
  geom_text(data = filter(loc_Proj_results, Pr_t < 0.05),
            aes(label = ifelse(Pr_t < 0.001, "***", ifelse(Pr_t < 0.01, "**", ifelse(Pr_t < 0.05, "*", "")))),
            vjust = -1) +
  labs(title = "Local Projection Results",
       x = "Horizon",
       y = "Estimates") +
  theme_minimal() +
  guides(fill = "none")  # Hide legend for significance

# Display the plot
print(plot)



Nutri_model <- plm::plm( plm::lead(Zscore_Nutrit_index, 1) ~ log(oda_disb + 1) +
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
                         index = c("iso3c", "year"), model = "within", effect = "individual")



lmtest::coeftest(Nutri_model, vcov = vcovHC(Nutri_model, 
                                           type = "HC1"))#[, ## Methods: sss, HC1, HC2, HC3  


Env_model <-  plm::plm(plm::lead(Zscore_EnvDeath_index, 8) ~ log(oda_disb + 1) +
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
                       index = c("iso3c", "year"), model = "within", effect = "individual")


lmtest::coeftest(Env_model, vcov = vcovHC(Env_model, 
                                            type = "HC1"))#[, ## Methods: sss, HC1, HC2, HC3  


# Assuming your local projection results are stored in a list of data frames called "local_projection_results_list"

# Load necessary packages if not already loaded
# install.packages(c("ggplot2", "dplyr", "tidyr"))
library(ggplot2)
library(dplyr)
library(tidyr)

# Sample local projection results

#





# Assuming your local projection results are stored in a data frame called "local_projection_results"

# Load necessary packages if not already loaded
# install.packages(c("ggplot2", "dplyr", "tidyr"))
library(ggplot2)
library(dplyr)
library(tidyr)







HSCR_model <-  plm::plm(plm::lead(Zscore_HSCR_index, 8) ~ log(oda_disb + 1) +
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
                        index = c("iso3c", "year"), model = "within", effect = "individual")


InfDis_model <-  plm::plm(plm::lead(Zscore_InfDis_index, 8) ~ log(oda_disb + 1) +
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
                          index = c("iso3c", "year"), model = "within", effect = "individual")







Mental_model <-  plm::plm(plm::lead(Zscore_Mental_index, 8) ~ log(oda_disb + 1)   +
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
                          index = c("iso3c", "year"), model = "within", effect = "individual")



lmtest::coeftest(Mental_model, vcov = vcovHC(Mental_model, 
                                             type = "HC1"))#[, ## Methods: sss, HC1, HC2, HC3  
# Sample local projection results
local_projection_results <- data.frame(
  Horizons = rep(0:8, 6),
  Var = rep(c("Reproductive Fatalities", "Malnutrition", "Env Death", "HSCR", "Infections and Diseases", "Mental Burden"), each = 9),
  Estimates = c(-0.00589860, -0.017497, -0.01960150, -0.02029182, -0.022307, -0.02238596, -0.02132187, -0.02284208, -0.01887854, 
                0.00357225, -0.00574395, -0.00978117, -0.00959640, -0.010810, -0.0092615, -0.00709690, -0.00686998, -0.00027013,
                -0.01135053, -0.01830391, -0.02231163, -0.02116836, -0.020944, -0.02271381, -0.02363296, -0.02878819, -0.025512, 
                -0.01705642, -0.00142464, 0.00135180, 3.5980e-03, 6.4687e-03, 2.9456e-03, 3.2566e-03, 4.6970e-03, -2.1525e-03,
                2.8912e-05, -0.00514354, -0.01049348, -0.00903634, -1.2078e-02, -0.01274648, -0.01123510, -0.01308528, -1.0490e-02, 
                -0.00903817, -0.00494732, -0.00476831, -2.8632e-03, -0.00489971, -0.00720462, -0.00820189, -1.5470e-02, -2.1924e-02 
  ),
  Std_Error = c(0.00512692, 0.0077266, 0.00797890, 0.00777425, 0.0077549, 0.00766739, 0.00736314, 0.00737094, 0.00740319,
                0.00547813, 0.00599048, 0.00624268, 0.00621901, 0.0059135, 0.0057423, 0.00612408, 0.00687994, 0.00699239,
                0.00974173, 0.01082844, 0.01107198, 0.01140796, 0.011365, 0.01050102, 0.00940724, 0.00894256, 0.0084702, 
                0.00759750, 0.00788668, 0.00816989, 8.5146e-03, 8.7214e-03, 8.7957e-03, 7.8411e-03, 7.4258e-03, 7.7609e-03, 
                4.0345e-03, 0.00562133, 0.00564129, 0.00535599, 5.1262e-03, 0.00511169, 0.00497418, 0.00509861,  5.9025e-03, 
                0.00862399, 0.00869721, 0.00923337, 9.2337e-03, 0.00894196, 0.00860154, 0.00938095, 1.0850e-02,  1.0774e-02
  ),
  t_value = c(-1.1505, -2.2645, -2.4567, -2.6101, -2.8765, -2.9196, -2.8958, -3.0989, -2.5501,
              0.6521, -0.9588, -1.5668, -1.5431, -1.8280, -1.6129, -1.1589, -0.9986, -0.0386,
              -1.1651, -1.6904, -2.0151, -1.8556, -1.8429, -2.1630, -2.5122, -3.2192, -3.0120, 
              -2.2450, -0.1806, 0.1655, 0.4226, 0.7417, 0.3349, 0.4153, 0.6325, -0.2774, 
              0.0072, -0.9150, -1.8601, -1.6871, -2.3561, -2.4936, -2.2587, -2.5664, -1.7771,
              -1.0480, -0.5688, -0.5164, -0.3101, -0.5479, -0.8376, -0.8743, -1.4259, -2.0350
  ),
  Pr_t = c(0.2500183, 0.023615, 0.01408, 0.0091, 0.004055, 0.003537, 0.003818, 0.001967, 0.010844,
           0.51439, 0.337713, 0.117266, 0.1229288, 0.067665, 0.106906, 0.24664, 0.31812, 0.9692,
           0.244048, 0.0910634, 0.043983, 0.063622, 0.065459, 0.030637, 0.01206, 0.00131, 0.0026277, 
           0.024837, 0.856663, 0.868593, 0.6726419, 0.458334, 0.73774, 0.67795, 0.527114, 0.781534, 
           0.9942826, 0.3602628, 0.0629706, 0.0916896, 0.0185413, 0.0127116, 0.0239967, 0.010342, 0.075698, 
           0.294706, 0.5695071, 0.6056002, 0.75652, 0.58378, 0.402340, 0.3820, 0.1540, 0.04198
  )
)



# Create a plot
Local_Proj <- ggplot(local_projection_results, aes(x = Horizons, y = Estimates, fill = Var)) +
  geom_ribbon(
    aes(ymin = Estimates - 1.96 * Std_Error, ymax = Estimates + 1.96 * Std_Error),
    alpha = 0.3,
    color = "black",
    linetype = "dotted"
  ) +
  geom_line() +
  geom_point() +
  geom_text(data = filter(local_projection_results, Pr_t < 0.1), aes(label = ifelse(Pr_t < 0.001, "***", ifelse(Pr_t < 0.005, "**", ifelse(Pr_t < 0.01, "*", "")))), vjust = -1) +
  labs(#title = "Local Projection Results",
       x = "Horizons",
       y = "Impulse Response Functions of Health Outcome to Log(ODA)",
       fill = "Variable") +
  facet_wrap(~Var, scales = "fixed", ncol = 3) +
  theme_minimal() +
  theme(legend.position = "none", 
        strip.text = element_text(color = "black", face = "bold")) + 
  geom_hline(yintercept = 0, linetype = "dotdash",  color = "black") +
  scale_x_continuous(breaks = seq(0, 8, 1))

ggsave("Plots/Local_Projt.pdf", plot = Local_Proj, width = 7, height = 5, dpi = 300)

#dotdash
#"3313"
#"1", 
#The linetype aesthetic can be specified with either an integer (0-6), a name (0 = blank, 1 = solid, 2 = dashed, 3 = dotted, 4 = dotdash, 5 = longdash, 6 = twodash),


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
