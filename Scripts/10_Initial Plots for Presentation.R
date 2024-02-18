
############# CREATING PLOT FOR PRESENTATION ##############
##### HEALTH OUTCOME INDICATORS ###########
# 3.1.1 Maternal mortality ratio			per 100,000 live births 		(SH.STA.MMRT)
# Life expectancy at birth, total (years)							(SP.DYN.LE00.IN)
# 3.3.2 Tuberculosis incidence per 100,000 population  per 100,000 people)		(SH.TBS.INCD)
# 3.3.3 Malaria incidence per 1000 population per 1,000 population at risk)		(SH.MLR.INCD.P3)
# 3.2.1 Under 5 mortality rate 				per 1,000 live births) 		(SH.DYN.MORT)
# 3.2.2 Neonatal mortality rate 				per 1,000 live births)		(SH.DYN.NMRT)
# 3b.1 Population prop vaccinated (coverage by all vaccine) Newborns protected against tetanus (%)			(SH.VAC.TTNS.ZS)
# 2.2   Incidence of undernourishment among under 5 children (Stunting children)  % of children under 5)	(SH.STA.STNT.ZS)



######## SOCIAL PROTECTION #############
# 5b. Gini inequality index reduction 			(%) – All SPL			per_allsp_gini_tot
# 6. Poverty Headcount reduction (%) - All SPL  (<$1.9 a day)			(per_allsp_p0_ep_tot)
# 1. Coverage 		      				(%) All SPL 			per_allsp.cov_pop_tot
# 3b. Population in the 1st quintile (poorest) receiving 1 program (%) All SPL	per_numprog1_q1_tot

###### ODA ###########
#  For Net ODA received at (constant 2018 US$) 							(DT.ODA.ODAT.KD)

############# Loading data for Trade openness  from World Bank###############
install.packages("WDI") 
library(WDI)

Country_source <- read.csv2(file = "ODA eligible.csv", sep = ";")

country_list <- unique(Plot_data$country)


#install.packages("countrycode")
#library(countrycode)


# Assuming your filtered data is stored in the 'filtered_data' variable
filtered_data <- filtered_data %>%
  mutate(iso3c_code = countrycode(sourcevar = Recipient, origin = "country.name", destination = "iso3c"))

names(filtered_data)[names(filtered_data) == "Recipient"] <- "country" # To change the country names 

### Regional groupings ###
filtered_data$regions <- countrycode::countrycode(sourcevar = filtered_data$Recipient, origin = "country.name", destination = "continent"))

#### Alternative 

install.packages("poliscidata")
library(poliscidata)
data("world")

world <- world %>%
  rename(Country = country, Region = regionun)

filtered_data <- filtered_data %>%
  mutate(ODA_values = as.numeric(Value))

# Perform the right join
wale <- world %>%
  right_join(filtered_data, by = c("Country" = "country")) %>%
  group_by(Region, Year) %>%
  summarise(ODA_sum = sum(ODA_values, na.rm = TRUE), .groups = "drop")

filtered_data <- filtered_data %>%
  rename(year = Year) %>%
  mutate(ODA_value = as.numeric(Value))
dim(filtered_data)


summary(filtered_data)
Plot_data_1 <- merge(x = Plot_data, y = filtered_data, by = c("country", "year"), all = TRUE)
Plot_data_1 <- left_join(x = Plot_data, y = filtered_data, by = c("country", "year"), relationship = "many-to-many")

summary(Plot_data$SPL_coverage)
summary(filtered_data)
summary(Plot_data_1)

dim(Plot_data)
any(is.element(filtered_data$country, Plot_data$country))

Plot_data_1[Plot_data_1$year == 2022,]

###################### Loading all the indicators from World Bank ###################

# Temporarily import the three indicators, remember to discuss with Thomas 
# Specify the country code
country_code <- unique(oda_disb$iso3c)
years <- 2000:2020 # Specify the desired years (e.g., 2000 to 2022)
Plot_indicator <- c("SH.STA.MMRT", # Maternal mortality ratio			per 100,000 live births
                    "SP.DYN.LE00.IN", # Life expectancy at birth, total (years)		
                    "SH.TBS.INCD", # Tuberculosis incidence per 100,000 population  per 100,000 people)
                    "SH.MLR.INCD.P3", # 3.3.3 Malaria incidence per 1000 population per 1,000 population at risk)	
                    "SH.DYN.MORT", # 3.2.1 Under 5 mortality rate 				per 1,000 live births) 		(SH.DYN.MORT)
                    "SH.DYN.NMRT", # 3.2.2 Neonatal mortality rate 				per 1,000 live births)		(SH.DYN.NMRT)
                    "SH.VAC.TTNS.ZS", # 3b.1 Population prop vaccinated (coverage by all vaccine) Newborns protected against tetanus (%)
                    "SH.STA.STNT.ZS", # Under 5 Stunting as % of all children under 5 
                    "per_allsp_gini_tot", # Gini inequality reduction
                    "per_allsp_p0_ep_tot", # Headcount poverty reduction as % of all SPL
                    "per_allsp.cov_pop_tot", # Coverage of all SPL as % of ppulation
                    "per_numprog1_q1_tot", # 1st qintile population receiveing atleast 1 programme as % of all SPL
                    "DT.ODA.ODAT.KD" # Net ODA received (constant 2018 US$)
)

Plot_data <- WDI(country = country_code, 
                      indicator = "SH.STA.MMRT",
                      start = years[1], 
                      end = years[length(years)])

summary(Plot_data)
# renaming 
Plot_data <- Plot_data %>% 
  rename("MatMort" = "SH.STA.MMRT", # Maternal mortality ratio			per 100,000 live births
        "Life_Exp" =  "SP.DYN.LE00.IN", # Life expectancy at birth, total (years)		
        "Tuberculosis"  = "SH.TBS.INCD", # Tuberculosis incidence per 100,000 population  per 100,000 people)
        "Malaria"  = "SH.MLR.INCD.P3", # 3.3.3 Malaria incidence per 1000 population per 1,000 population at risk)	
        "Under5_Mort" =  "SH.DYN.MORT", # 3.2.1 Under 5 mortality rate 				per 1,000 live births) 		(SH.DYN.MORT)
        "NeoNat_Mort"  = "SH.DYN.NMRT", # 3.2.2 Neonatal mortality rate 				per 1,000 live births)		(SH.DYN.NMRT)
        "Newborns_Vaccine" = "SH.VAC.TTNS.ZS", # 3b.1 Population prop vaccinated (coverage by all vaccine) Newborns protected against tetanus (%)
         "Malnutirtion_Under5" =  "SH.STA.STNT.ZS", # Under 5 Stunting as % of all children under 5 
        "Gini_reduction" =  "per_allsp_gini_tot", # Gini inequality reduction
        "Poverty_Head_Red" =  "per_allsp_p0_ep_tot", # Headcount poverty reduction as % of all SPL
        "SPL_coverage" = "per_allsp.cov_pop_tot", # Coverage of all SPL as % of ppulation
        "Poorest_Quint_SPL" =  "per_numprog1_q1_tot", # 1st qintile population receiveing atleast 1 programme as % of all SPL
        "ODA_Net" = "DT.ODA.ODAT.KD" # Net ODA received (constant 2018 US$)
)

summary(Plot_data)
hist(Plot_data$SPL_coverage)

unique(filtered_data$Recipient)




#### Imputation
# Assuming your dataset is named 'your_dataset'
# Assuming 'MatMort,' 'Life_Exp,' and 'Tuberculosis' are the columns to be imputed with mean

# Create a copy of the dataset to preserve the original
Plot_data_1 <- Plot_data
Plot_data <- Plot_data_1

summary(Plot_data$MatMort)
hist(Plot_data$MatMort)
# Impute missing values with mean
Plot_data$MatMort[is.na(Plot_data$MatMort)] <- mean(Plot_data$MatMort, na.rm = TRUE)
your_dataset_imputed_mean$Life_Exp[is.na(your_dataset_imputed_mean$Life_Exp)] <- mean(your_dataset_imputed_mean$Life_Exp, na.rm = TRUE)
your_dataset_imputed_mean$Tuberculosis[is.na(your_dataset_imputed_mean$Tuberculosis)] <- mean(your_dataset_imputed_mean$Tuberculosis, na.rm = TRUE)

# Load the missForest package
library(missForest)

# Select the columns to be imputed with missForest
Health_outcomes <- c("MatMort", "Life_Exp",            "Tuberculosis",       "Malaria",
                     "Under5_Mort",         "NeoNat_Mort",       
                      "Newborns_Vaccine",    "Malnutirtion_Under5")
# Impute missing values with missForest
your_dataset_imputed_forest <- missForest::missForest(Plot_data[, Health_outcomes])
ODA_imputed <- missForest::missForest(Plot_data[, "ODA_Net"])
# Group by country and impute missing values in ODA_Net with the country average
Plot_data <- Plot_data %>%
  group_by(country) %>%
  mutate(ODA_Net_imputed = ifelse(is.na(ODA_Net), mean(ODA_Net, na.rm = TRUE), ODA_Net)) %>%
  ungroup()
hist(Plot_data$ODA_Net_imputed)
hist(Plot_data$ODA_Net)

# Replace the imputed values in the original dataset
Plot_data[, Health_outcomes] <- your_dataset_imputed_forest$ximp

Plot_data[, ODA_Net] <- ODA_imputed$ximp


######## CREATE THE SIMPLE INDEX OF ESD ##########
# Normalize each variable to a 0 to 1 range
Plot_data[, Health_outcomes] <- apply(Plot_data[, Health_outcomes], 2, function(x) (x - min(x, na.rm = TRUE)) / (max(x, na.rm = TRUE) - min(x, na.rm = TRUE)))

Plot_data$Health_index <- rowMeans(Plot_data[, Health_outcomes], na.rm = TRUE)

Plot_data <- Plot_data %>%
  mutate(Recipient_group = cut(
    ODA_Net, 
    breaks = quantile(ODA_Net, probs = seq(0, 1, 1/3), na.rm = TRUE),
    labels = c('Low', 'Medium', 'High'),
    include.lowest = TRUE
  ))

hist(log10(Plot_data$ODA_Net))
summary(Plot_data$Health_index)
hist(Plot_data$Health_index)
## 1c. Create the density graphs ##

# Define the color palette for recipient groups
my_colors <- c("Low" = "#1f77b4", "Medium" = "#ff7f0e", "High" = "#2ca02c")

# Create density plots for each recipient group with log-transformed emissions
ggplot(Plot_data, aes(x = Health_index, fill = Recipient_group)) +
  geom_density(alpha = 0.6) +
  labs(title = "Density Plots of Log-transformed ghg/pop by Recipient Group",
       x = "Log(Emission level per Pop)",
       y = "Density") +
  scale_fill_manual(values = my_colors) +
  theme_minimal()

hist(log10(Plot_data$ODA_Net))


ggplot(Plot_data[!is.na(Plot_data$Recipient_group), ], 
       aes(x = Health_index, fill = Recipient_group)) +
  geom_density(alpha = 0.6) +
  labs(title = "Dist of ODA Net by Health Index",
       x = "Health_Index",
       y = "Density") +
  scale_fill_manual(values = my_colors) +
  theme_minimal()


dev.off()
Plot_data <- na.omit(Plot_data)
# Density log10 for emission per population among the recipients of ODA 

gg <- ggplot(d, aes(x = ghgpop, fill = Recipient_Group)) +
  geom_density(alpha = 0.6) +
  labs(title = "Density Plots of Log-transformed ghg/pop by Recipient Group",
       x = "Log(Emission level per Pop)",
       y = "Density") +
  scale_fill_manual(values = my_colors) +
  scale_x_log10() +
  theme_minimal()


unique(Plot_data$Recipient_group)

# Add vertical line at 0 and set x-axis limits symmetrically
gg <- gg + geom_vline(xintercept = 0, linetype = "dashed", color = "black") +
  coord_cartesian(xlim = c(-2, 2)) # Adjust the limits as needed








install.packages("expersso/OECD")
library(devtools)
install_github("expersso/OECD")
library(OECD)

title <-OECD::get_datasets()
head(title, 20)
search_dataset("ODA", data = dataset_list)
search_dataset()

title %>%
  filter(grepl("oda", title, ignore.case = TRUE))

get_data_structure(datatset) %>%
  str(., max.level = 2)
datatset <- "REF_TOTALOFFICIAL"
###### Save the output graph as a PDF or png using ggsave ##

ggsave("my_plot.pdf", plot = gg, width = 6, height = 4)
ggsave("my_plot.png", gg, width = 6, height = 4, dpi = 300)

ggsave("my_plot1.png", gg, width = 6, height = 4, dpi = 300)
