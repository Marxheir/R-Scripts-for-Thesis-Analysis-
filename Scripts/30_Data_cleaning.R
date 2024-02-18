########## CLEANING COUNTRIES #########
## List f countries that only existed after 2000
# South sudan (SSD) - established in 2011
# kosovo (XKX) - established in 2008
# Timor Leste (TLS) - established in 2002
# Montenegro (MNE) - 2006
# Serbia (SRB) - 2006
# Eritrea (ERI) - 1993
# Palau (PLW) - 1994
# Marshall Islands (MHL) - 1990
# Micronesia (FSM) - 1990
# Slovakia (SVK) - 1993
# Bosnia and Herzegovina (BIH) - 1992
# Moldova (MDA) - 1991

Thomas_data <- read.csv2("impact_data_yearWave.csv", sep = ";")
# Assuming your data frame is named 'panel_data' and has columns 'iso3c', 'year', and other variables
oda_disb_Map
summary(oda_disb_plot)
borrowed_inds <- c("iso3c", "year", "CF_LF_ratio", "covid_stringency", "cri_score", "emdat_naffect", 
                   "emdat_ndeath", "FFrent", "gdppopgrowth", "ghg","gov", "oil_prod_Twh", 
                   "ppp", "resource_rent", "vdem_polyarchy", "UN_voting", "pub_invest", 
                   "wparl", "GASrent", "COALrent", "undp_hdi", "bci", "ae")

####### We maintained List of eligible countries in the Thomas Data. #####

#### Since we employ SDG data, SDG began in 2000, not ealier. Therefore, data must begin frm 2000

# a. Create a complete data frame with all combinations of iso3c and years from 2000 to 2022
ISOandYears_desired <- expand.grid(iso3c = unique(Thomas_data$iso3c), year = 2000:2022)

ISOandYears_desired <- merge(ISOandYears_desired, oda_disb[, c("iso3c", "year", "country", "region")], by = c("iso3c", "year"), all.x = TRUE)

ISOandYears_desired <- ISOandYears_desired %>%
  group_by(iso3c) %>%
  fill(country, region) # To fill the country and region for 2022
view(ISOandYears_desired)

# b. Merge the container created to Thomas data to create year 2000
Thomas_transfm <- merge(ISOandYears_desired, Thomas_data, by = c("iso3c", "year"), all.x = TRUE)

# c. Extract needed variables from Thomas data and maintain the desired data grid years and countries
oda_disb_1 <- merge(oda_disb, Thomas_transfm[, borrowed_inds], by = c("iso3c", "year"), all.y = TRUE)
soc_pro_filtered <- merge(Soc_protect_data, ISOandYears_desired, by = c("iso3c", "year"), all.y = TRUE)
summary(soc_pro_filtered)
oda_disb_1 <- merge(oda_disb_1, soc_pro_filtered[, c("iso3c", "year", "Cov_all_SPL")], by = c("iso3c", "year"))

summary(oda_disb_1)

write.csv2(oda_disb_1, "Raw_data/Thesis_data.csv")
write_rds(oda_disb_1, "Raw_data/Thesis_data.rds")

######### CLEANING OF DATA #########
# A. Remove all variables with 70% of missingness from the data ##

view(oda_disb_1)
library(tidyverse)
high_missing_inds <- c("Maternity_cash_benefit", "Cov_poor_pop", 
                       "Child_benef_cov", "Unemplt_beneft_cov", "Vulnrb_pop_cov", 
                       "work_injury_cov", "Atleast_oneSP_cov", "Disab_benef_cov", 
                       "LMKT_benef_cov", "Propty_right", "Gender_violence", "civil_violence", 
                       "Ext_debt_GNI")

clean_thesis_data <- oda_disb_1 %>%
  select(-one_of(high_missing_inds))

###### IMPUTATION OF DATA ##########
## Create container of varisbles to imput
Imp_indicators <- names(clean_thesis_data[, setdiff(names(clean_thesis_data), c("iso3c", "year", "country", "region"))])

## Now imput variables
library(missForest)
Missforest_imput_data <- missForest::missForest(clean_thesis_data[, Imp_indicators])

summary(clean_thesis_data)
## Access the perfomormance of the imputation by missforest
Missforest_imput_data$OOBerror
## NRMSE = 0.1167006 is low, indicating better performance 
##  The Normalized Root Mean Squared Error (NRMSE) metric, which is commonly used to evaluate the performance of regression models, including imputation models like MissForest.
## The NRMSE value is a proportion of the root mean squared error relative to the range of the observed values.

summary(Missforest_imput_data$ximp)
MisF_imp_clean_thesis_data <- clean_thesis_data
MisF_imp_clean_thesis_data[, Imp_indicators] <- Missforest_imput_data$ximp
summary(MisF_imp_clean_thesis_data)


install.packages(c("purrr", "cowplot"))
library(purrr)
library(cowplot)

# Create before-and-after plots for each variable
plots_list <- map(Imp_indicators, function(variable_name) {
  # Before imputation
  before_imputation_plot <- ggplot(clean_thesis_data, aes(x = !!as.name(variable_name))) +
    geom_histogram(fill = "lightblue", color = "black", bins = 30) +
    labs(title = paste("Before Imputation -", variable_name), x = variable_name, y = "Frequency")
  
  # After imputation
  after_imputation_plot <- ggplot(Missforest_imput_data$ximp, aes(x = !!as.name(variable_name))) +
    geom_histogram(fill = "lightgreen", color = "black", bins = 30) +
    labs(title = paste("After Imputation -", variable_name), x = variable_name, y = "Frequency")
  
  # Combine plots
  plot_grid(before_imputation_plot, after_imputation_plot, ncol = 2)
})

# Display plots
plots_list[[94]]  # Display the plots for the first variable


####### Set all negative ODA values to zero and clean ODA data ####
MisF_imp_clean_thesis_data$oda_disb[MisF_imp_clean_thesis_data$oda_disb < 0] <- 0

MisF_imp_clean_thesis_data <- MisF_imp_clean_thesis_data %>%
  mutate(GNI_million = (GNI / 1000000), 
         aid_Per_GNI = (oda_disb / GNI_million))


summary(MisF_imp_clean_thesis_data[, c("oda_disb", "GNI_million", 
                                               "aid_Per_GNI", "GNI")])

## Correlation Visualisation
library(corrplot)

corr_matrix <- cor(MisF_imp_clean_thesis_data[, Imp_indicators])
corrplot::corrplot(corr_matrix, method = "circle", tl.cex = 0.6, type = "lower", diag = TRUE,
                   order = "hclust" # or "FPC"
)

## Check for correlation of health indicators 
health_Inds <- c("Mat_Mort_ratio", "Neonatal_Mort", "Tuberculosis_incid", "Malaria_incid",
                 "hepatitis_B_under5", "Tropical_dis", "Noncomm_diseas", "Suicide_mort",
                 "Alcohol_percap", "Family_planning_cov", "Adolesc_pregn_10to14", "UHC",
                 "CHS_25perc", "Air_polut_Mort", "Tobacco_rate", "Tetanus_vaccine", "Measles_vaccine",
                 "HealthResearch_ODA", "Healthfacil_adeq", "HthWk_Physician_dens", "HthWk_Midwivies_dens", "Women_Anaemia",
                 "Child_less5_stunted", "undernoursh_pop", "Life_expectancy", "Under5_Mort", "Uninten_poison_MortWB",
                 "Unsafe_water_mortWB", "Adol_birth15to19WB", "Road_traff_MortWB", "NewHIV_casesWB", "Birth_SkilledWorkWB"
)

corr_matrix_hlth <- cor(MisF_imp_clean_thesis_data[, health_Inds])
corrplot::corrplot(corr_matrix_hlth, method = "circle", tl.cex = 0.6, type = "lower", diag = TRUE,
                   order = "hclust", # or "FPC"
                   tl.col = "black"#,
                   #addCoef.col = "black", addCoefasPercent = TRUE, number.font = 0.5
)

## Checks for correlation of Covariates 
covarites <- c(
  "hlth_spdng_percap", "hlth_spdng_perGDP", "govt_hth_spnd_1", "govt_hth_spnd_2", "govt_hth_sp_percap3",
  "Urb_pop_rate", "UrbPop_grth_rate", "GDP_grth_rate", "Pol_stabty_Est", "Pol_stabty_Perctile",
  "corr_percentile", "corr_estimate", "Resch_dev", "Capit_Forma", "Lab_force", "Pop_0to14", "Pop_65abv",
  "Pop_tot", "Pop_grwth", "Pop_densty", "Emission_tot_ghg", "Emission_co2", "Electri_access",
  "Agric_emp", "Unemply_rate",  "Tech_export_rate", "Female_educ", "Trade", "FDI", "Inflation",
  "clean_cook_enrgy", "GDP_per_cap", "Gross_FixCap",  "Sec_Educ", "GNI", "CF_LF_ratio", "covid_stringency",
  "cri_score", "emdat_naffect", "emdat_ndeath", "FFrent", "gdppopgrowth", "ghg", "gov", "oil_prod_Twh",
  "ppp",  "resource_rent","vdem_polyarchy", "UN_voting",  "pub_invest", "wparl",  "GASrent", "COALrent",
  "undp_hdi", "bci", "ae")

corr_matrix_covariates <- cor(MisF_imp_clean_thesis_data[, covariates])
corrplot::corrplot(corr_matrix_covariates, method = "circle", tl.cex = 0.6, type = "lower", diag = TRUE,
                   order = "hclust", # or "FPC"
                   tl.col = "black"#,
                   #addCoef.col = "black", addCoefasPercent = TRUE, number.font = 0.5
)
# remove correlated covariates 
MisF_imp_clean_thesis_data <- MisF_imp_clean_thesis_data %>%
  select(!c(gdppopgrowth, Sec_Educ, Electri_access, hlth_spdng_percap, ppp,
            Pol_stabty_Perctile, corr_percentile, corr_estimate, Emission_co2, 
            ghg, Capit_Forma, emdat_naffect))

## Social Protection Correlation
Soc_prot_inds <- c("Soc_Assistance_cov", "Soc_Insur_cov", "Pension_cov", "Cov_all_SPL")
corr_matrix_soc_prot <- cor(MisF_imp_clean_thesis_data[, Soc_prot_inds])

corrplot::corrplot(corr_matrix_soc_prot, method = "circle", tl.cex = 0.6, type = "lower", diag = TRUE,
                   order = "hclust", # or "FPC"
                   tl.col = "black"#,
                   #addCoef.col = "black", addCoefasPercent = TRUE, number.font = 0.5
)


### Time series plot for all countries
ggplot(MisF_imp_clean_thesis_data_Indices, 
       aes(x = year, y = Zscore_Reprd_index, color = iso3c)) +
  geom_line() +
  ggtitle("Time Series Plot for Variable by Country") +
  xlab("Year") +
  ylab("Your Variable") +
  guides("NULL")
library(tidyverse)
######### DATA TRANSFORMATION AND INDICES CREATION #########

## Index for HSCR
MisF_imp_clean_thesis_data_Indices <- MisF_imp_clean_thesis_data %>%
  mutate(Zscore_HSCR_index = rowMeans(scale(MisF_imp_clean_thesis_data[, HSCR_dim_inds])), 
         Norm_HSCR_index = rowMeans((MisF_imp_clean_thesis_data[, HSCR_dim_inds] - min(MisF_imp_clean_thesis_data[, HSCR_dim_inds])) / (max(MisF_imp_clean_thesis_data[, HSCR_dim_inds] - min(MisF_imp_clean_thesis_data[, HSCR_dim_inds])))))

## Index for Reproductive risk
MisF_imp_clean_thesis_data_Indices <- MisF_imp_clean_thesis_data_Indices %>%
  mutate(Zscore_Reprd_index = rowMeans(scale(MisF_imp_clean_thesis_data_Indices[, Reprod_health_risk])), 
         Norm_Reprd_index = rowMeans((MisF_imp_clean_thesis_data_Indices[, Reprod_health_risk] - min(MisF_imp_clean_thesis_data_Indices[, Reprod_health_risk])) / (max(MisF_imp_clean_thesis_data_Indices[, Reprod_health_risk] - min(MisF_imp_clean_thesis_data_Indices[, Reprod_health_risk])))))

## Index for infection and diseases 
MisF_imp_clean_thesis_data_Indices <- MisF_imp_clean_thesis_data_Indices %>%
  mutate(Zscore_InfDis_index = rowMeans(scale(MisF_imp_clean_thesis_data_Indices[, Burden_infect_dis])), 
         Norm_InfDis_index = rowMeans((MisF_imp_clean_thesis_data_Indices[, Burden_infect_dis] - min(MisF_imp_clean_thesis_data_Indices[, Burden_infect_dis])) / (max(MisF_imp_clean_thesis_data_Indices[, Burden_infect_dis] - min(MisF_imp_clean_thesis_data_Indices[, Burden_infect_dis])))))

## Index for Mental health
MisF_imp_clean_thesis_data_Indices <- MisF_imp_clean_thesis_data_Indices %>%
  mutate(Zscore_Mental_index = rowMeans(scale(MisF_imp_clean_thesis_data_Indices[, Mental_health])), 
         Norm_Mental_index = rowMeans((MisF_imp_clean_thesis_data_Indices[, Mental_health] - min(MisF_imp_clean_thesis_data_Indices[, Mental_health])) / (max(MisF_imp_clean_thesis_data_Indices[, Mental_health] - min(MisF_imp_clean_thesis_data_Indices[, Mental_health])))))

## Index for malnutrition
MisF_imp_clean_thesis_data_Indices <- MisF_imp_clean_thesis_data_Indices %>%
  mutate(Zscore_Nutrit_index = rowMeans(scale(MisF_imp_clean_thesis_data_Indices[, Nutrition])), 
         Norm_Nutrit_index = rowMeans((MisF_imp_clean_thesis_data_Indices[, Nutrition] - min(MisF_imp_clean_thesis_data_Indices[, Nutrition])) / (max(MisF_imp_clean_thesis_data_Indices[, Nutrition] - min(MisF_imp_clean_thesis_data_Indices[, Nutrition])))))

## Index for Env death 
MisF_imp_clean_thesis_data_Indices <- MisF_imp_clean_thesis_data_Indices %>%
  mutate(Zscore_EnvDeath_index = rowMeans(scale(MisF_imp_clean_thesis_data_Indices[, Env_death])), 
         Norm_EnvDeath_index = rowMeans((MisF_imp_clean_thesis_data_Indices[, Env_death] - min(MisF_imp_clean_thesis_data_Indices[, Env_death])) / (max(MisF_imp_clean_thesis_data_Indices[, Env_death] - min(MisF_imp_clean_thesis_data_Indices[, Env_death])))))

summary(MisF_imp_clean_thesis_data_Indices)

###### CREATING AGGREGATED PERIOD FOR DATA ########

# Assuming 'year' is the column representing the year
levels <- cut(2000:2022, breaks = seq(2000, 2022, by = 5), 
               right = T)

# Assuming your data frame is named 'your_data'
# Replace 'your_data' with the actual name of your data frame

# Convert 'year' to a factor with 5-year intervals
your_data$year_interval <- cut(your_data$year, breaks = seq(2000, 2022, by = 4), labels = FALSE, include.lowest = TRUE)

# Aggregate data by iso3c and year_interval, calculating the mean for each group

# Print the first few rows of the aggregated data
head(aggregated_data)



countries_info <- unique(MisF_imp_clean_thesis_data_Indices[, c(, "country", "iso3c", "region")])
write.csv(countries_info, "Raw_data/countries_info.csv", row.names = FALSE)

library(xtable)


# Add a column for row numbers
countries_info <- countries_info[, c("country", "iso3c", "region")]

# Use xtable to create LaTeX code with longtable
latex_code <- print.xtable(
  xtable(countries_info, caption = "Country Information", label = "tab:country_info"),
  floating = FALSE, # Prevents the table from floating
  tabular.environment = "longtable" # Use longtable environment
  #hline.after = c(-1, 0, nrow(countries_info)), # Add horizontal lines
  #add.to.row = list(pos = list(-1, 0, nrow(countries_info)), command = c('\\caption{Country Information}', '\\label{tab:country_info}'))
)

# Print the LaTeX code
print(latex_code, include.rownames = FALSE)


####### Creating Regional Dummies ############
MisF_imp_clean_thesis_data_1 <- MisF_imp_clean_thesis_data
MisF_imp_clean_thesis_data_Indices$SSA_dum <- as.numeric(MisF_imp_clean_thesis_data$region == "SSA")
MisF_imp_clean_thesis_data_Indices$MENA_dum <- as.numeric(MisF_imp_clean_thesis_data$region == "MENA")
MisF_imp_clean_thesis_data_Indices$Europe_dum <- as.numeric(MisF_imp_clean_thesis_data$region == "Europe")
MisF_imp_clean_thesis_data_Indices$LAC_dum <- as.numeric(MisF_imp_clean_thesis_data$region == "LAC")
MisF_imp_clean_thesis_data_Indices$SCA_dum <- as.numeric(MisF_imp_clean_thesis_data$region == "SCA")

unique(MisF_imp_clean_thesis_data$region)
view(MisF_imp_clean_thesis_data_1[, c("country", "SSA_dum")])


######## Save the new data #####
write.csv2(MisF_imp_clean_thesis_data, "Raw_data/MisF_imp_clean_thesis_data.csv")
write.csv2(MisF_imp_clean_thesis_data_Indices, "Raw_data/MisF_imp_clean_thesis_data_Indices.csv")






######## Archived ######
view(MisF_imp_clean_thesis_data_1)
hist(log(MisF_imp_clean_thesis_data_1$Norm_Reprd_index))
hist(log(MisF_imp_clean_thesis_data_1$Under5_Mort))

install.packages("radiant")
library(radiant)

summary(pre_factor(MisF_imp_clean_thesis_data, 
                   vars = Reprod_health_risk))
summary(full_factor(MisF_imp_clean_thesis_data, 
                   vars = Reprod_health_risk, nr_fact = 4))
