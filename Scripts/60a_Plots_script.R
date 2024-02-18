########### VISUALISATION OF DATA ###########

oda_disb <- read.csv2("Raw_data/ODA_Health_CV_data", sep = ";")


####### Create separate script later ######### 
library(stats)
library(ggsignif)
library(missForest)
library(tidyverse)
library(viridis)
require("ggpubr")


MisF_imp_clean_thesis_data_Indices <- MisF_imp_clean_thesis_data_Indices %>%
  select(- Zscore_HSCR_index)
############ CREATING PLOTS FOR HEALTH INDICATORS ###########
MisF_imp_clean_thesis_data_Indices <- oda_disb
oda_disb_Map <- oda_disb
imputed_data <- missForest(MisF_imp_clean_thesis_data_Indices[, health_Outcomes_inds])
GNI_imp <- missForest(MisF_imp_clean_thesis_data_Indices[, GNI])

imputed_data$ximp <- imputed_data$ximp %>%
  select(- Healthfacil_adeq.1)

# Now unique_imputed_data contains the imputed data without duplicate columns
MisF_imp_clean_thesis_data_Indices[, health_Outcomes_inds] <- imputed_data$ximp

summary(MisF_imp_clean_thesis_data_Indices)
hist(MisF_imp_clean_thesis_data_Indices$Mat_Mort_ratio)
hist(oda_disb$Mat_Mort_ratio)


combined_scatplot <- ggarrange(Reprod_Risks_scatPlot, Burd_Infs_scatPlot, Mental_Hth_scatPlot, 
          Malnutrition_scatPlot, Env_death_scatPlot, HSCR_scatPlot + rremove("x.text"), 
          labels = c("A:RFTP", "B: BID", "C: BMP", "D: Malnutrition", "E: Environmental death", "F: HSCR")
               #     "B: BID"), 
                    #"C: BMP", 
                    #"D: Malnutrition", 
                    #"E: Environmental Death", 
                    #"F: HSCR"), 
           )
combined_boxplot <- ggarrange(Reprod_boxplt, Burd_Infs_boxplt, Mental_Hth_boxplt, 
                               Malnutrition_boxplt, Env_death_boxplt, HSCR_Boxplt,# + rremove("x.text"), 
                               labels = c("A:RFTP", "B: BID", "C: BMP", "D: Malnutrition", "E: Env. death", "F: HSCR")
                               #     "B: BID"), 
                               #"C: BMP", 
                               #"D: Malnutrition", 
                               #"E: Environmental Death", 
                               #"F: HSCR"), 
)
?ggarrange
summary(BID_dpm)
ggsave("Plots/combined_scatplot.pdf", plot = combined_scatplot, width = 10, height = 5, dpi = 300)


#### a. Health system capacity and responsiveness####
HSCR_dim_inds <- c("Birth_SkilledWorkWB",  # "SH_STA_BRTC", # Indicator 3.1.2, Series : Proportion of births attended by skilled health personnel (%)
                   "Measles_vaccine",  # "SH_ACS_MCV2", # Indicator 3.b.1, Series : Proportion of the target population with access to measles-containing-vaccine second-dose (MCV2) (%) 
                   #"HealthResearch_ODA", # "DC_TOF_HLTHNT", # Indicator 3.b.2, Series : Total official development assistance to medical research and basic health sectors, net disbursement, by recipient countries (millions of constant 2021 United States dollars) 
                   "Healthfacil_adeq", # "SH_HLF_EMED", # Indicator 3.b.3, Series : Proportion of health facilities that have a core set of relevant essential medicines available and affordable on a sustainable basis (%) 
                   "HthWk_Physician_dens",  # "SH_MED_DEN", # Indicator 3.c.1, Series : Health worker density, by type of occupation (per 10,000 population) 
                   "HthWk_Midwivies_dens", 
                   "Tetanus_vaccine" 
                   )

MisF_imp_clean_thesis_data_Indices <- MisF_imp_clean_thesis_data %>%
  mutate(Zscore_HSCR_index = rowMeans(scale(MisF_imp_clean_thesis_data[, HSCR_dim_inds])), 
         Norm_HSCR_index = rowMeans((MisF_imp_clean_thesis_data[, HSCR_dim_inds] - min(MisF_imp_clean_thesis_data[, HSCR_dim_inds])) / (max(MisF_imp_clean_thesis_data[, HSCR_dim_inds] - min(MisF_imp_clean_thesis_data[, HSCR_dim_inds])))))


# Create a boxplot with embedded violin plots
region_colors <- c("red", "blue", "orange", "purple", "brown", "green")
HSCR_Boxplt <- ggplot(MisF_imp_clean_thesis_data_Indices, aes(x = region, y = Zscore_HSCR_index)) +
  geom_violin(trim = FALSE, width = 0.7) +
  geom_boxplot(aes(fill = region), notch = 2, width = 0.2, outlier.shape = NA, color = "black") +
  stat_compare_means(
    comparisons = list(c("EAP", "Europe"), c("Europe", "LAC"), c("LAC", "MENA"), 
                       c("MENA", "SCA"), c("SCA", "SSA"), c("SSA", "EAP", c("SSA", "Europe"), c("SSA", "LAC"), 
                                                            c("SSA", "MENA"))),
    aes(label = ..p.format..),
    method = "t.test",
    map_signif_level = TRUE,
    textsize = 12,
    size = 3,
    #vjust = 2,
    # label.y = 1,
    label.sep = " "
  ) +
 # scale_fill_brewer(palette = "Set3") +
  scale_fill_manual(values = setNames(region_colors, unique(MisF_imp_clean_thesis_data_Indices$region))) +
  guides(fill = "none") +
  labs(x = "Region", 
       y = "HSCR Z-score values") +
  theme_minimal() +
  scale_y_continuous(breaks = seq(floor(min(MisF_imp_clean_thesis_data_Indices$Zscore_HSCR_index)), 
                                  ceiling(max(MisF_imp_clean_thesis_data_Indices$Zscore_HSCR_index)), 1))

ggsave("Plots/HSCR_Boxplt.pdf", plot = HSCR_Boxplt, width = 6, height = 3.6, dpi = 300)



HSCR_scatPlot <- ggplot(MisF_imp_clean_thesis_data_Indices, aes(y = Zscore_HSCR_index, x = iso3c, colour = region)) +
  geom_point(size = 1) +
  geom_text(aes(label = iso3c), hjust = 1.5, size = 3.2) +
  scale_color_manual(values = c("red", "blue", "orange", "purple", "brown", "black")) +
  #theme_bw(base_size = 15) +
   guides(color = "none") +
  theme(axis.text.x = element_blank(),
        #axis.title.y = element_blank(),
        axis.title.x = element_blank(),
        legend.text = element_text(size = 8)) +  # Adjust the legend text size
  #ggtitle("HSCR") +  # Add your desired title
  ylab("HSCR Z-score values") + # Replace with your desired y-axis label
  xlab("Country") 


ggsave("Plots/HSCR_scatPlot.pdf", plot = HSCR_scatPlot, width = 6, height = 4, dpi = 300)


#### b. Reproductive health and pre-post natal mortality ####
Reprod_health_risk <- c("Mat_Mort_ratio", # "SH_STA_MORT", # Indicator 3.1.1, Series : Maternal mortality ratio
                        "Under5_Mort",  # "SH_DYN_MORT", # Indicator 3.2.1, Series : Under-five mortality rate, by sex (deaths per 1,000 live births)
                        "Neonatal_Mort", # "SH_DYN_NMRT", # Indicator 3.2.2, Series : Neonatal mortality rate (deaths per 1,000 live births)
                      #  "Family_planning_cov", #  "SH_FPL_MTMM", # Indicator 3.7.1, Series : Proportion of women of reproductive age (aged 15-49 years) who have their need for family planning satisfied with modern methods (% of women aged 15-49 years)
                       # "Adolesc_pregn_10to14", 
                        "Adol_birth15to19WB" #  "SP_DYN_ADKL" # Indicator 3.7.2, Series : Adolescent birth rate (per 1,000 women aged 15-19 and 10-14 years) 
)



# Scale the indicators
MisF_imp_clean_thesis_data_Indices <- MisF_imp_clean_thesis_data_Indices %>%
  mutate(Zscore_Reprd_index = rowMeans(scale(MisF_imp_clean_thesis_data_Indices[, Reprod_health_risk])), 
         Norm_Reprd_index = rowMeans((MisF_imp_clean_thesis_data_Indices[, Reprod_health_risk] - min(MisF_imp_clean_thesis_data_Indices[, Reprod_health_risk])) / (max(MisF_imp_clean_thesis_data_Indices[, Reprod_health_risk] - min(MisF_imp_clean_thesis_data_Indices[, Reprod_health_risk])))))


# Create a boxplot with embedded violin plots
Reprod_boxplt <- ggplot(MisF_imp_clean_thesis_data_Indices, aes(x = region, y =  Zscore_Reprd_index)) +
  geom_violin(trim = FALSE, width = 0.7) +
  geom_boxplot(aes(fill = region), notch = 2, width = 0.2, outlier.shape = NA, color = "black") +
  stat_compare_means(
    comparisons = list(c("EAP", "Europe"), c("Europe", "LAC"), c("LAC", "MENA"), 
                       c("MENA", "SCA"), c("SCA", "SSA"), c("SSA", "EAP", c("SSA", "Europe"), c("SSA", "LAC"), 
                                                            c("SSA", "MENA"))),
    aes(label = ..p.format..),
    method = "t.test",
    map_signif_level = TRUE,
    textsize = 12,
    size = 3,
    #vjust = 2,
     #label.y = 5,
    label.sep = " "
  ) +
  # scale_fill_brewer(palette = "Set3") +
  scale_fill_manual(values = setNames(region_colors, unique(MisF_imp_clean_thesis_data_Indices$region))) +
  guides(fill = "none") + 
  labs(#title = "Reproductive Health Risk and Mortality Index",
       x = "Region",
       y = "RFTP Z-score values") +
  theme_minimal()  +
  scale_y_continuous(breaks = seq(floor(min(MisF_imp_clean_thesis_data_Indices$Zscore_Reprd_index)), ceiling(max(MisF_imp_clean_thesis_data_Indices$Zscore_Reprd_index)), 1))


ggsave("Plots/Reprod_Risks_boxplt.pdf", plot = Reprod_boxplt, width = 6, height = 3.6, dpi = 300)


Reprod_Risks_scatPlot <- ggplot(MisF_imp_clean_thesis_data_Indices, aes(y = Zscore_Reprd_index, x = iso3c, colour = region)) +
  geom_point(size = 1) +
  geom_text(aes(label = iso3c), hjust = 1.5, size = 3.2) +
  scale_color_manual(values = c("red", "blue", "orange", "purple", "brown", "black")) +
  #theme_bw(base_size = 15) +
   guides(color = "none") +
  theme(axis.text.x = element_blank(),
        #axix.title.x =element_blank(),
        #axis.title.y = element_blank(),
        # axis.title.x = element_blank(),
       # legend.text = element_text(size = 8)
       ) +  # Adjust the legend text size
  #ggtitle("Reproductive Fatality") #+  # Add your desired title
  ylab("RFTP Z-score values") + # Replace with your desired y-axis label
  xlab("Country")


ggsave("Plots/Reprod_Risks_scatPlot.pdf", plot = Reprod_Risks_scatPlot, width = 6, height = 4, dpi = 300)



#### c. Burden of Infections and diseases #####
Burden_infect_dis <- c("NewHIV_casesWB", # "SH_HIV_INCD", # Indicator 3.3.1, Series : Number of new HIV infections per 1,000 uninfected population, by sex and age (per 1,000 uninfected population)
                       "Tuberculosis_incid", # "SH_TBS_INCD", # Indicator 3.3.2, Series : Tuberculosis incidence (per 100,000 population) 
                       "Malaria_incid",  #  "SH_STA_MALR", # Indicator 3.3.3, Series : Malaria incidence per 1,000 population at risk (per 1,000 population) 
                       "hepatitis_B_under5", #  "SH_HAP_HBSAG", # Indicator 3.3.4, Series : Prevalence of hepatitis B surface antigen (HBsAg) (%) 
                       "Tropical_dis", #  "SH_TRP_INTVN", # Indicator 3.3.5, Series : Number of people requiring interventions against neglected tropical diseases (number) 
                       "Noncomm_diseas" #  "SH_DTH_NCOM" # Indicator 3.4.1, Series : Mortality rate attributed to cardiovascular disease, cancer, diabetes or chronic respiratory disease (probability)
)

# Scale indicators and create index
MisF_imp_clean_thesis_data_Indices <- MisF_imp_clean_thesis_data_Indices %>%
  mutate(Zscore_InfDis_index = rowMeans(scale(MisF_imp_clean_thesis_data_Indices[, Burden_infect_dis])), 
         Norm_InfDis_index = rowMeans((MisF_imp_clean_thesis_data_Indices[, Burden_infect_dis] - min(MisF_imp_clean_thesis_data_Indices[, Burden_infect_dis])) / (max(MisF_imp_clean_thesis_data_Indices[, Burden_infect_dis] - min(MisF_imp_clean_thesis_data_Indices[, Burden_infect_dis])))))


# Create a boxplot with embedded violin plots
Burd_Infs_boxplt <- ggplot(MisF_imp_clean_thesis_data_Indices, aes(x = region, y =  Zscore_InfDis_index)) +
  geom_violin(trim = FALSE, width = 0.7) +
  geom_boxplot(aes(fill = region), notch = 2, width = 0.2, outlier.shape = NA, color = "black") +
  stat_compare_means(
    comparisons = list(c("EAP", "Europe"), c("Europe", "LAC"), c("LAC", "MENA"), 
                       c("MENA", "SCA"), c("SCA", "SSA"), c("SSA", "EAP", c("SSA", "Europe"), c("SSA", "LAC"), 
                                                            c("SSA", "MENA"))),
    aes(label = ..p.format..),
    method = "t.test",
    map_signif_level = TRUE,
    textsize = 12,
    size = 3,
    #vjust = 2,
    # label.y = 1,
    label.sep = " "
  ) +
  # scale_fill_brewer(palette = "Set3") +
  scale_fill_manual(values = setNames(region_colors, unique(MisF_imp_clean_thesis_data_Indices$region))) +
  guides(fill = "none") +
  labs(#title = "Burden of Infections and Diseases Index by Region",
       x = "Region",
       y = "BID Z-score values") +
  theme_minimal()  +
  scale_y_continuous(breaks = seq(floor(min(MisF_imp_clean_thesis_data_Indices$Zscore_InfDis_index)), ceiling(max(MisF_imp_clean_thesis_data_Indices$Zscore_InfDis_index)), 1))


ggsave("Plots/Burd_Infs_boxplt.pdf", plot = Burd_Infs_boxplt, width = 6, height = 3.6, dpi = 300)


Burd_Infs_scatPlot <- ggplot(MisF_imp_clean_thesis_data_Indices, aes(y = Zscore_InfDis_index, x = iso3c, colour = region)) +
  geom_point(size = 1) +
  geom_text(aes(label = iso3c
                ), hjust = 1.5, size = 3.2) +
  scale_color_manual(values = c("red", "blue", "orange", "purple", "brown", "black")) +
  #theme_bw(base_size = 15) +
   guides(color = "none") +
  theme(axis.text.x = element_blank(),
        #axis.ticks.x = element_blank(),
        #axis.title.y = element_blank(),
        #legend.text = element_text(size = 8)
        ) +  # Adjust the legend text size
 # ggtitle("Burden of Infections and Diseases Index") +  # Add your desired title
  ylab("BID Z-score values") + # Replace with your desired y-axis label
  xlab("Country")


ggsave("Plots/Burd_Infs_scatPlot.pdf", plot = Burd_Infs_scatPlot, width = 6, height = 4, dpi = 300)


#### d. Mental health and substance use ####

Mental_health <- c("Suicide_mort", # "SH_STA_SCIDE", # Indicator 3.4.2, Series : Suicide mortality rate, by sex (deaths per 100,000 population) 
                   "Alcohol_percap",  # "SH_SUD_ALCOL", # Indicator 3.5.1, Series : Alcohol use disorders, 12-month prevalence (%) "SH_ALC_CONSPT", # Indicator 3.5.2, Series : Alcohol consumption per capita (aged 15 years and older) within a calendar year (litres of pure alcohol) 
                   "Tobacco_rate"  # "SH_PRV_SMOK" # Indicator 3.a.1, Series : Age-standardized prevalence of current tobacco use among persons aged 15 years and older, by sex (%) 
)
# Scale the indicators and create index 
MisF_imp_clean_thesis_data_Indices <- MisF_imp_clean_thesis_data_Indices %>%
  mutate(Zscore_Mental_index = rowMeans(scale(MisF_imp_clean_thesis_data_Indices[, Mental_health])), 
         Norm_Mental_index = rowMeans((MisF_imp_clean_thesis_data_Indices[, Mental_health] - min(MisF_imp_clean_thesis_data_Indices[, Mental_health])) / (max(MisF_imp_clean_thesis_data_Indices[, Mental_health] - min(MisF_imp_clean_thesis_data_Indices[, Mental_health])))))


# Create a boxplot with embedded violin plots
Mental_Hth_boxplt <- ggplot(MisF_imp_clean_thesis_data_Indices, aes(x = region, y = Zscore_Mental_index)) +
  geom_violin(trim = FALSE, width = 0.7) +
  geom_boxplot(aes(fill = region), notch = 2, width = 0.2, outlier.shape = NA, color = "black") +
  stat_compare_means(
    comparisons = list(c("EAP", "Europe"), c("Europe", "LAC"), c("LAC", "MENA"), 
                       c("MENA", "SCA"), c("SCA", "SSA"), c("SSA", "EAP", c("SSA", "Europe"), c("SSA", "LAC"), 
                                                            c("SSA", "MENA"))),
    aes(label = ..p.format..),
    method = "wilcox.test",
    map_signif_level = TRUE,
    textsize = 12,
    size = 3,
    #vjust = 2,
    # label.y = 1,
    label.sep = " "
  ) +
  # scale_fill_brewer(palette = "Set3") +
  scale_fill_manual(values = setNames(region_colors, unique(MisF_imp_clean_thesis_data_Indices$region))) +
  guides(fill = "none") +
  labs(#title = "Burden of Mental Health Problems (Suicide, Alcohol and Tobacco)",
       x = "Region",
       y = "BMP Z-score values") +
  theme_minimal()  +
  scale_y_continuous(breaks = seq(floor(min(MisF_imp_clean_thesis_data_Indices$Zscore_Mental_index)), ceiling(max(MisF_imp_clean_thesis_data_Indices$Zscore_Mental_index)), 1))


ggsave("Plots/Mental_Hth_boxplt.pdf", plot = Mental_Hth_boxplt, width = 6, height = 3.6, dpi = 300)



Mental_Hth_scatPlot <- ggplot(MisF_imp_clean_thesis_data_Indices, aes(y = Zscore_Mental_index, x = iso3c, colour = region)) +
  geom_point(size = 1) +
  geom_text(aes(label = iso3c), hjust = 1.5, size = 3.2) +
  scale_color_manual(values = c("red", "blue", "orange", "purple", "brown", "black")) +
  #theme_bw(base_size = 15) +
  # guides(color = "none") +
  theme(axis.text.x = element_blank(),
        #axis.ticks.x = element_blank(),
         #axis.title.y = element_blank(),
        #legend.text = element_text(size = 8)
        ) +  # Adjust the legend text size
 # ggtitle("Burden of Mental Health Problems") +  # Add your desired title
  ylab("BMP Z-score values") + # Replace with your desired y-axis label
  xlab("Country")


ggsave("Plots/Mental_Hth_scatPlot.pdf", plot = Mental_Hth_scatPlot, width = 6, height = 4, dpi = 300)



#### e. Nutrition #####
Nutrition <- c("undernoursh_pop", # "SN_ITK_DEFC", # Indicator 2.1.1, Series : Prevalence of undernourishment (%)
               "Child_less5_stunted", # "SH_STA_STNT", # Indicator 2.2.1, Series : Proportion of children moderately or severely stunted (%)
               "Women_Anaemia" #  "SH_STA_ANEM" # Indicator 2.2.3, Series : Proportion of women aged 15-49 years with anaemia (%) 
)

# Scale the indicators and create index 
MisF_imp_clean_thesis_data_Indices <- MisF_imp_clean_thesis_data_Indices %>%
  mutate(Zscore_Nutrit_index = rowMeans(scale(MisF_imp_clean_thesis_data_Indices[, Nutrition])), 
         Norm_Nutrit_index = rowMeans((MisF_imp_clean_thesis_data_Indices[, Nutrition] - min(MisF_imp_clean_thesis_data_Indices[, Nutrition])) / (max(MisF_imp_clean_thesis_data_Indices[, Nutrition] - min(MisF_imp_clean_thesis_data_Indices[, Nutrition])))))

# Create a boxplot with embedded violin plots
Malnutrition_boxplt <- ggplot(MisF_imp_clean_thesis_data_Indices, aes(x = region, y =  Zscore_Nutrit_index)) +
  geom_violin(trim = FALSE, width = 0.7) +
  geom_boxplot(aes(fill = region), notch = 2, width = 0.2, outlier.shape = NA, color = "black") +
  stat_compare_means(
    comparisons = list(c("EAP", "Europe"), c("Europe", "LAC"), c("LAC", "MENA"), 
                       c("MENA", "SCA"), c("SCA", "SSA"), c("SSA", "EAP", c("SSA", "Europe"), c("SSA", "LAC"), 
                                                            c("SSA", "MENA"))),
    aes(label = ..p.format..),
    method = "t.test",
    map_signif_level = TRUE,
    textsize = 12,
    size = 3,
    #vjust = 2,
    # label.y = 1,
    label.sep = " "
  ) +
  # scale_fill_brewer(palette = "Set3") +
  scale_fill_manual(values = setNames(region_colors, unique(MisF_imp_clean_thesis_data_Indices$region))) +
  guides(fill = "none") +
  labs(#title = "Malnutrition Index",
       x = "Region",
       y = "Malnutrition Z-score values") +
  theme_minimal()  +
  scale_y_continuous(breaks = seq(floor(min(MisF_imp_clean_thesis_data_Indices$Zscore_Nutrit_index)), ceiling(max(MisF_imp_clean_thesis_data_Indices$undernoursh_pop)), 1))


ggsave("Plots/Malnutrition_boxplt.pdf", plot = Malnutrition_boxplt, width = 6, height = 3.6, dpi = 300)


Malnutrition_scatPlot <- ggplot(MisF_imp_clean_thesis_data_Indices, aes(y = Zscore_Nutrit_index, x = iso3c, colour = region)) +
  geom_point(size = 1) +
  geom_text(aes(label = iso3c), hjust = 1.5, size = 3.2) +
  scale_color_manual(values = c("red", "blue", "orange", "purple", "brown", "black")) +
  #theme_bw(base_size = 15) +
   guides(color = "none") +
  theme(axis.text.x = element_blank(),
        #axis.ticks.x = element_blank(),
         #axis.title.y = element_blank(),
        #legend.text = element_text(size = 8)
        ) +  # Adjust the legend text size
        #legend.position = "bottom") +
  # ggtitle("Intensity of Undernutrition Index") +  # Add your desired title
  ylab("Malnutrition Z-score values") + # Replace with your desired y-axis label
  xlab("Country")

ggsave("Plots/Malnutrition_scatPlot.pdf", plot = Malnutrition_scatPlot, width = 6, height = 4, dpi = 300)


#### f. Health access and affordability ####
Hlth_access_affd <- c("UHC",  # "SH_ACS_UNHC", # Indicator 3.8.1, Series : Universal health coverage (UHC) service coverage index 
                        "CHS_25perc", # "SH_XPD_EARN25", # Indicator 3.8.2, Series : Proportion of population with large household expenditures on health (greater than 25%) as a share of total household expenditure or income (%) 
                        "Healthfacil_adeq" # "SH_HLF_EMED", # Indicator 3.b.3, Series : Proportion of health facilities that have a core set of relevant essential medicines available and affordable on a sustainable basis (%) 
)




# Scale the indicators
MisF_imp_clean_thesis_data_Indices[, Hlth_access_affd] <- scale(MisF_imp_clean_thesis_data_Indices[, Hlth_access_affd])

# Calculate row means
MisF_imp_clean_thesis_data_Indices$Zscore_Nutrit_index <- rowMeans(MisF_imp_clean_thesis_data_Indices[, Nutrition])

summary(MisF_imp_clean_thesis_data_Indices$Nutrition)

# Create a boxplot with embedded violin plots
CHS_boxplt <- ggplot(MisF_imp_clean_thesis_data_Indices, aes(x = region, y =  CHS_25perc)) +
  geom_violin(trim = FALSE, width = 0.7) +
  geom_boxplot(aes(fill = region), notch = 2, width = 0.2, outlier.shape = NA, color = "black") +
  stat_compare_means(
    comparisons = list(c("EAP", "Europe"), c("Europe", "LAC"), c("LAC", "MENA"), 
                       c("MENA", "SCA"), c("SCA", "SSA"), c("SSA", "EAP", c("SSA", "Europe"), c("SSA", "LAC"), 
                                                            c("SSA", "MENA"))),
    aes(label = ..p.format..),
    method = "t.test",
    map_signif_level = TRUE,
    textsize = 12,
    size = 3,
    #vjust = 2,
    # label.y = 1,
    label.sep = " "
  ) +
  # scale_fill_brewer(palette = "Set3") +
  scale_fill_manual(values = setNames(region_colors, unique(MisF_imp_clean_thesis_data_Indices$region))) +
  guides(fill = "none") +
  labs(#title = "Catastrophic Health Spending (CHS > 25% HHs income)",
       x = "Region",
       y = "Percentage of Population with CHS") +
  theme_minimal()  +
  scale_y_continuous(breaks = seq(floor(min(MisF_imp_clean_thesis_data_Indices$CHS_25perc)), ceiling(max(MisF_imp_clean_thesis_data_Indices$CHS_25perc)), 5))


ggsave("Plots/CHS_boxplt.pdf", plot = CHS_boxplt, width = 6, height = 3.6, dpi = 300)


CHS_scatPlot <- ggplot(MisF_imp_clean_thesis_data_Indices, aes(y = CHS_25perc, x = iso3c, colour = region)) +
  geom_point(size = 1) +
  geom_text(aes(label = iso3c), hjust = 1.5, size = 3.2) +
  scale_color_manual(values = c("red", "blue", "orange", "purple", "brown", "black")) +
  theme_bw(base_size = 15) +
  # guides(color = "none") +
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        # axis.title.x = element_blank(),
        legend.text = element_text(size = 8)) +  # Adjust the legend text size
  #ggtitle("Intensity of Undernutrition Index") +  # Add your desired title
  ylab("Percentage of Population with CHS") + # Replace with your desired y-axis label
  xlab("Country")


ggsave("Plots/Malnutrition_scatPlot.pdf", plot = Malnutrition_scatPlot, width = 6, height = 4, dpi = 300)

##### g. Environmental death and fatalities #####
Env_death <- c("Air_polut_Mort", #  "SH_HAP_ASMORT", # Indicator 3.9.1, Series : Age-standardized mortality rate attributed to household air pollution (deaths per 100,000 population) 
               "Unsafe_water_mortWB", # "SH_STA_WASHARI", # Indicator 3.9.2, Series : Mortality rate attributed to unsafe water, unsafe sanitation and lack of hygiene from diarrhoea, intestinal nematode infections, malnutrition and acute respiratory infections (deaths per 100,000 population) 
               "Uninten_poison_MortWB", #  "SH_STA_POISN", # Indicator 3.9.3, Series : Mortality rate attributed to unintentional poisonings, by sex (deaths per 100,000 population) 
               "Road_traff_MortWB" #  "SH_STA_TRAF" # Indicator 3.6.1, Series : Death rate due to road traffic injuries, by sex (per 100,000 population)
)

# Scale the indicators and create index 
MisF_imp_clean_thesis_data_Indices <- MisF_imp_clean_thesis_data_Indices %>%
  mutate(Zscore_EnvDeath_index = rowMeans(scale(MisF_imp_clean_thesis_data_Indices[, Env_death])), 
         Norm_EnvDeath_index = rowMeans((MisF_imp_clean_thesis_data_Indices[, Env_death] - min(MisF_imp_clean_thesis_data_Indices[, Env_death])) / (max(MisF_imp_clean_thesis_data_Indices[, Env_death] - min(MisF_imp_clean_thesis_data_Indices[, Env_death])))))


# Create a boxplot with embedded violin plots
Env_death_boxplt <- ggplot(MisF_imp_clean_thesis_data_Indices, aes(x = region, y =  Zscore_EnvDeath_index)) +
  geom_violin(trim = FALSE, width = 0.7) +
  geom_boxplot(aes(fill = region), notch = 2, width = 0.2, outlier.shape = NA, color = "black") +
  stat_compare_means(
    comparisons = list(c("EAP", "Europe"), c("Europe", "LAC"), c("LAC", "MENA"), 
                       c("MENA", "SCA"), c("SCA", "SSA"), c("SSA", "EAP", c("SSA", "Europe"), c("SSA", "LAC"), 
                                                            c("SSA", "MENA"))),
    aes(label = ..p.format..),
    method = "t.test",
    map_signif_level = TRUE,
    textsize = 12,
    size = 3,
    #vjust = 2,
    # label.y = 1,
    label.sep = " "
  ) +
  # scale_fill_brewer(palette = "Set3") +
  scale_fill_manual(values = setNames(region_colors, unique(MisF_imp_clean_thesis_data_Indices$region))) +
  guides(fill = "none") +
  labs(#title = "Environment Induced Mortality Index",
       x = "Region",
       y = "Environmental death Z-score values") +
  theme_minimal()  +
  scale_y_continuous(breaks = seq(floor(min(MisF_imp_clean_thesis_data_Indices$Zscore_EnvDeath_index)), ceiling(max(MisF_imp_clean_thesis_data_Indices$Zscore_EnvDeath_index)), 1))


ggsave("Plots/Env_death_boxplt.pdf", plot = Env_death_boxplt, width = 6, height = 3.6, dpi = 300)


Env_death_scatPlot <- ggplot(MisF_imp_clean_thesis_data_Indices, aes(y = Zscore_EnvDeath_index, x = iso3c, colour = region)) +
  geom_point(size = 1) +
  geom_text(aes(label = iso3c), hjust = 1.5, size = 3.2) +
  scale_color_manual(values = c("red", "blue", "orange", "purple", "brown", "black")) +
  #theme_bw(base_size = 15) +
   guides(color = "none") +
  theme(axis.text.x = element_blank(),
        #axis.ticks.x = element_blank(),
        # axis.title.y = element_blank(),
        # legend.text = element_text(size = 8)
        ) +  # Adjust the legend text size
  #ggtitle("Intensity of Undernutrition Index") +  # Add your desired title
  ylab("Environmental death Z-score values") + # Replace with your desired y-axis label
  xlab("Country")


ggsave("Plots/Env_death_scatPlot.pdf", plot = Env_death_scatPlot, width = 6, height = 4, dpi = 300)




##### ODA Boxplot ##########

####### Creating Scatterplot of ODA against Each Health Dimension ###
library(car)
library(psych)

MisF_imp_clean_thesis_data_Indices <- MisF_imp_clean_thesis_data_Indices %>%
  mutate(GNI_million = GNI/1000000, 
         aid_Per_GNI = (oda_disb / GNI_million))

summary(MisF_imp_clean_thesis_data_Indices[, c("oda_disb", "GNI_million", 
                                               "aid_Per_GNI", "GNI")])
# Create a boxplot with embedded violin plots
ODA_GNI_boxplt <- ggplot(MisF_imp_clean_thesis_data_Indices, aes(x = region, y =  aid_Per_GNI)) +
  geom_boxplot(aes(fill = region), notch = 2, width = 0.2, outlier.shape = NA, color = "black") +
  stat_compare_means(
    comparisons = list(c("EAP", "Europe"), c("Europe", "LAC"), c("LAC", "MENA"), 
                       c("MENA", "SCA"), c("SCA", "SSA"), c("SSA", "EAP", c("SSA", "Europe"), c("SSA", "LAC"), 
                                                            c("SSA", "MENA"))),
    aes(label = ..p.format..),
    method = "t.test",
    map_signif_level = TRUE,
    textsize = 12,
    size = 3,
    #vjust = 2,
    #label.y = 6,
    label.sep = " "
  ) +
  # scale_fill_brewer(palette = "Set3") +
  scale_fill_manual(values = setNames(region_colors, unique(MisF_imp_clean_thesis_data_Indices$region))) +
  guides(fill = "none") +
  labs(#title = "ODA Dependency (Net ODA as Percentage of Country GNI) Accross Regions",
       x = "Region",
       y = "Net ODA Per GNI") +
  theme_minimal() 

ggsave("Plots/ODA_GNI_boxplt.pdf", plot = ODA_GNI_boxplt, width = 6, height = 3.6, dpi = 300)




Soc_Inf_ODA_BxPlt <- ggplot(MisF_imp_clean_thesis_data_Indices, aes(x = region, y = SOC_INF_ODA)) +
  geom_boxplot(aes(fill = region), notch = 2, width = 0.2, outlier.shape = NA, color = "black") +
  stat_compare_means(
    comparisons = list(c("EAP", "Europe"), c("Europe", "LAC"), c("LAC", "MENA"), 
                       c("MENA", "SCA"), c("SCA", "SSA"), c("SSA", "EAP", c("SSA", "Europe"), c("SSA", "LAC"), 
                                                            c("SSA", "MENA"))),
    aes(label = ..p.format..),
    method = "t.test",
    map_signif_level = TRUE,
    textsize = 12,
    size = 3,
    #vjust = 2,
    #label.y = 6,
    label.sep = " "
  ) +
  # scale_fill_brewer(palette = "Set3") +
  scale_fill_manual(values = setNames(region_colors, unique(MisF_imp_clean_thesis_data_Indices$region))) +
  guides(fill = "none") +
  labs(#title = "ODA Dependency (Net ODA as Percentage of Country GNI) Accross Regions",
    x = "Region",
    y = "Social Infrastructure ODA (Commitments)") +
  theme_minimal() #+
#  scale_y_continuous(breaks = seq(0, 2000, 200))


ggsave("Plots/Soc_Inf_ODA_BxPlt.pdf", plot = Soc_Inf_ODA_BxPlt, width = 6, height = 4, dpi = 300)


library(ggplot2)
library(ggsignif)


ODA_GNI_scatPlot <- ggplot(MisF_imp_clean_thesis_data_Indices, aes(y = aid_Per_GNI, x = iso3c, colour = region)) +
  geom_point(size = 1) +
  geom_text(aes(label = iso3c), hjust = 1.5, size = 3.2) +
  scale_color_manual(values = c("red", "blue", "orange", "purple", "brown", "black")) +
  theme_bw(base_size = 15) +
  # guides(color = "none") +
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        # axis.title.x = element_blank(),
        legend.text = element_text(size = 8)) +  # Adjust the legend text size
  #ggtitle("Intensity of Undernutrition Index") +  # Add your desired title
  ylab("ODA Per GNI") + # Replace with your desired y-axis label
  xlab("Country")


ggsave("Plots/ODA_GNI_scatPlot.pdf", plot = ODA_GNI_scatPlot, width = 6, height = 4, dpi = 300)





######### Does ODA flow to countries with health problems ########
# Pivot the data longer
# Plot using ggplot2
agg_inds <- c("aid_Per_GNI", "SOC_INF_ODA", "region", "log_CHS", "Zscore_Reprd_index", "Zscore_InfDis_index", "Zscore_Mental_index",           
              "Zscore_Nutrit_index", "Zscore_EnvDeath_index", "Zscore_HSCR_index", "CHS_25perc", "Cov_all_SPL")

ODA_Hth_plt <- MisF_imp_clean_thesis_data_Indices %>%
  select(all_of(agg_inds)) %>%
  pivot_longer(cols = -c(iso3c, region, aid_Per_GNI, SOC_INF_ODA, CHS_25perc, log_CHS, Cov_all_SPL), names_to = "Health_Dimension") %>%
  
  ggplot(aes(x = value, y = log(aid_Per_GNI + 1), color = region)) +
  geom_point(size = 1, shape = 16) +
  geom_smooth(method = "lm", se = FALSE, linewidth = 1, color = "red") +
  geom_smooth(method = "loess", se = FALSE, size = 1, color = "black") +
  #geom_text(aes(label = sprintf("R^2: %.2f", summary(lm(log(aid_Per_GNI + 1.5) ~ value))$r.squared)),
   #         x = Inf, y = -Inf, hjust = 1, vjust = 0, size = 3, color = "black") +
  facet_wrap(~Health_Dimension, scales = "free", labeller = labeller(Health_Dimension = c(
    "Zscore_InfDis_index" = "BID",
    "Zscore_HSCR_index" = "HSCR",
    "Zscore_EnvDeath_index" = "Environmental Death", 
    "Zscore_Mental_index" = "Mental Problem (BMP)", 
    "Zscore_Reprd_index" = "RFTP", 
    "Zscore_Nutrit_index" = "Malnutrition"
    # Add more as needed
  ))) +
  labs(
    x = "Health Indices",
    y = "Log(ODA Per GNI)"
  ) +
  theme_minimal() +
  theme(
    strip.background = element_rect(fill = "lightblue", color = "white"),  # Change the background color
    strip.text = element_text(color = "black", face = "bold")  # Change the text color and style
  )


ggsave("Plots/ODA_Hth_plt.pdf", plot = ODA_Hth_plt, width = 7, height = 4.5, dpi = 300)




Soc_ODA_Hth_plt <- MisF_imp_clean_thesis_data_Indices %>%
  select(all_of(agg_inds)) %>%
  pivot_longer(cols = -c(iso3c, region, aid_Per_GNI, SOC_INF_ODA, CHS_25perc, log_CHS, Cov_all_SPL), names_to = "Health_Dimension") %>%
  
  ggplot(aes(x = value, y = log(SOC_INF_ODA + 1), color = region)) +
  geom_point(size = 1, shape = 16) +
  geom_smooth(method = "lm", se = FALSE, linewidth = 1, color = "red") +
  geom_smooth(method = "loess", se = FALSE, size = 1, color = "black") +
  #geom_text(aes(label = sprintf("R^2: %.2f", summary(lm(log(aid_Per_GNI + 1.5) ~ value))$r.squared)),
  #         x = Inf, y = -Inf, hjust = 1, vjust = 0, size = 3, color = "black") +
  facet_wrap(~Health_Dimension, scales = "free", labeller = labeller(Health_Dimension = c(
    "Zscore_InfDis_index" = "BID",
    "Zscore_HSCR_index" = "HSCR",
    "Zscore_EnvDeath_index" = "Environmental Death", 
    "Zscore_Mental_index" = "Mental Problem (BMP)", 
    "Zscore_Reprd_index" = "RFTP", 
    "Zscore_Nutrit_index" = "Malnutrition"
    # Add more as needed
  ))) +
  labs(
    x = "Health Indices",
    y = "Log(Social Infrastructure ODA)"
  ) +
  theme_minimal() +
  theme(
    strip.background = element_rect(fill = "lightblue", color = "white"),  # Change the background color
    strip.text = element_text(color = "black", face = "bold")  # Change the text color and style
  )


ggsave("Plots/Soc_ODA_Hth_plt.pdf", plot = Soc_ODA_Hth_plt, width = 7, height = 4.5, dpi = 300)





########## SOCIAL PROTECTION ############
soc_Prot_inds <- c("Maternity_cash_benefit", "Cov_poor_pop", "Soc_Assistance_cov", 
                   "Soc_Insur_cov", "Child_benef_cov", "Unemplt_beneft_cov", 
                   "Vulnrb_pop_cov", "work_injury_cov", "Atleast_oneSP_cov", "Disab_benef_cov", 
                   "LMKT_benef_cov", "Pension_cov")

MisF_imp_clean_thesis_data_Indices <- MisF_imp_clean_thesis_data_Indices %>%
  mutate(log_CHS = log(CHS_25perc + 1))




Soc_Prot_SocInfrODA_plt <- MisF_imp_clean_thesis_data_Indices %>%
  select(all_of(agg_inds)) %>%
  pivot_longer(cols = -c(iso3c, region, aid_Per_GNI, SOC_INF_ODA, Zscore_InfDis_index, 
                         Zscore_HSCR_index, Zscore_EnvDeath_index, Zscore_Mental_index, 
                         Zscore_Reprd_index, Zscore_Nutrit_index, CHS_25perc), names_to = "Health_Dimension") %>%
  
  ggplot(aes(x = value, y = log(SOC_INF_ODA + 1), color = region)) +
  geom_point(size = 1, shape = 16) +
  geom_smooth(method = "lm", se = FALSE, linewidth = 1, color = "red") +
  geom_smooth(method = "loess", se = FALSE, size = 1, color = "black") +
  #geom_text(aes(label = sprintf("R^2: %.2f", summary(lm(log(aid_Per_GNI + 1.5) ~ value))$r.squared)),
  #         x = Inf, y = -Inf, hjust = 1, vjust = 0, size = 3, color = "black") +
  facet_wrap(~Health_Dimension, scales = "free", labeller = labeller(Health_Dimension = c(
    "log_CHS" = "Catastrophic Health Spending >25%",
    "Cov_all_SPL" = "Social Protection Coverage"
    # Add more as needed
  ))) +
  labs(
   # x = "H",
    y = "Log(Social Infrastructure ODA)"
  ) +
  theme_minimal() +
  theme(
    strip.background = element_rect(fill = "lightblue", color = "white"),  # Change the background color
    strip.text = element_text(color = "black", face = "bold")  # Change the text color and style
  )


ggsave("Plots/Soc_Prot_SocInfrODA_plt.pdf", plot = Soc_Prot_SocInfrODA_plt, width = 7, height = 3.5, dpi = 300)


Soc_Prot_aid_Per_GNI_plt <- MisF_imp_clean_thesis_data_Indices %>%
  select(all_of(agg_inds)) %>%
  pivot_longer(cols = -c(iso3c, region, aid_Per_GNI, SOC_INF_ODA, Zscore_InfDis_index, 
                         Zscore_HSCR_index, Zscore_EnvDeath_index, Zscore_Mental_index, 
                         Zscore_Reprd_index, Zscore_Nutrit_index, CHS_25perc), names_to = "Health_Dimension") %>%
  
  ggplot(aes(x = value, y = log(aid_Per_GNI + 1), color = region)) +
  geom_point(size = 1, shape = 16) +
  geom_smooth(method = "lm", se = FALSE, linewidth = 1, color = "red") +
  geom_smooth(method = "loess", se = FALSE, size = 1, color = "black") +
  #geom_text(aes(label = sprintf("R^2: %.2f", summary(lm(log(aid_Per_GNI + 1.5) ~ value))$r.squared)),
  #         x = Inf, y = -Inf, hjust = 1, vjust = 0, size = 3, color = "black") +
  facet_wrap(~Health_Dimension, scales = "free", labeller = labeller(Health_Dimension = c(
    "log_CHS" = "Catastrophic Health Spending >25%",
    "Cov_all_SPL" = "Social Protection Coverage"
    # Add more as needed
  ))) +
  labs(
    # x = "H",
    y = "Log(aid_Per_GNI)"
  ) +
  theme_minimal() +
  theme(
    strip.background = element_rect(fill = "lightblue", color = "white"),  # Change the background color
    strip.text = element_text(color = "black", face = "bold")  # Change the text color and style
  )


ggsave("Plots/Soc_Prot_aid_Per_GNI_plt.pdf", plot = Soc_Prot_aid_Per_GNI_plt, width = 7, height = 3.5, dpi = 300)


SocProct_Hlth_plt <- MisF_imp_clean_thesis_data_Indices %>%
  select(all_of(agg_inds)) %>%
  pivot_longer(cols = -c(iso3c, region, aid_Per_GNI, SOC_INF_ODA, CHS_25perc, log_CHS, Cov_all_SPL), names_to = "Health_Dimension") %>%
  
  ggplot(aes(x = value, y = Cov_all_SPL, color = region)) +
  geom_point(size = 1, shape = 16) +
  geom_smooth(method = "lm", se = FALSE, linewidth = 1, color = "red") +
  geom_smooth(method = "loess", se = FALSE, size = 1, color = "black") +
  #geom_text(aes(label = sprintf("R^2: %.2f", summary(lm(log(aid_Per_GNI + 1.5) ~ value))$r.squared)),
  #         x = Inf, y = -Inf, hjust = 1, vjust = 0, size = 3, color = "black") +
  facet_wrap(~Health_Dimension, scales = "free", labeller = labeller(Health_Dimension = c(
    "Zscore_InfDis_index" = "BID",
    "Zscore_HSCR_index" = "HSCR",
    "Zscore_EnvDeath_index" = "Environmental Death", 
    "Zscore_Mental_index" = "Mental Problem (BMP)", 
    "Zscore_Reprd_index" = "RFTP", 
    "Zscore_Nutrit_index" = "Malnutrition"
    # Add more as needed
  ))) +
  labs(
    x = "Health Indices",
    y = "Coverage of Social Protection"
  ) +
  theme_minimal() +
  theme(
    strip.background = element_rect(fill = "lightblue", color = "white"),  # Change the background color
    strip.text = element_text(color = "black", face = "bold")  # Change the text color and style
  )

ggsave("Plots/SocProct_Hlth_plt.pdf", plot = SocProct_Hlth_plt, width = 7, height = 4.5, dpi = 300)


################## ARCHIVE ###############

summary(MisF_imp_clean_thesis_data_Indices[, soc_Prot_inds])

install.packages("mice")
library(VIM)

# Assuming your data frame is named 'your_data'
aggr_plot <- VIM::aggr(MisF_imp_clean_thesis_data_Indices[, soc_Prot_inds, drop = FALSE], numbers = TRUE, 
                       sortVars = TRUE, labels = names(MisF_imp_clean_thesis_data_Indices[, soc_Prot_inds, drop = FALSE]), 
                       cex.axis = 0.6, gap = 2, 
                       ylab = c("Histogram of missing data", "Pattern"))

library(mice)

# Create a pattern plot
mice::md.pattern(MisF_imp_clean_thesis_data_Indices[, soc_Prot_inds], plot = TRUE, 
                 rotate.names = TRUE)


######## Archive ##########

# Example weights for each dimension
weights <- c(0.4, 0.3, 0.3)

# Calculate weighted mean for each dimension
weighted_means <- c(
  weighted.mean(health_data$Reproductive_Risk_Mortality, weights),
  weighted.mean(health_data$Nutrition, weights),
  weighted.mean(health_data$Health_System_Capacity, weights)
)

# Display or store the results
weighted_means

