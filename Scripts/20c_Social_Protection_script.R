############ SOCIAL PROTECTION INDICATORS ##############

Soc_protect_inds <- c(
  Tot_Cov_SPL = "per_allsp.cov_pop_tot", 
  Cov_Poor = "per_numprog2_ep_tot",
  First_quitile_cov = "per_numprog1_q1_tot",   
  Gini_inequlty_SP = "per_allsp_gini_tot",  			 
 Poverty_change = "per_allsp_p0_ep_tot"   
)
##### Package #######
library(tidyverse)
library(WDI)

###### Load the work-in-progress data ##########
oda_disb <- read.csv2("Raw_data\Health_with_ODA_data.csv", sep = ";")

countries <- unique(oda_disb_1$iso3c) # Select countries to query World Bank ASIPRE database. WB prefers iso3c code
yearWB <- 2000:20212

summary(Soc_protect_data)
### Load the data from World Bank #########
Soc_protect_data <- WDI(country = countries, indicator = soc_Prot_inds, 
                        start = yearWB[1], 
                        end = yearWB[length(yearWB)])

view(Soc_protect_data)
Soc_protect_data <- Soc_protect_data %>%
  rename(Coverage_allSPL = per_allsp.cov_pop_tot, Benefit_Adeq_allSPL = per_allsp.adq_pop_tot, 
         Beneficiary_incd_allSPL = per_allsp.bry_q1_tot, Atleast_two_SPL = per_numprog2_ep_tot, 
         Poorest_Quitle_oneSPL = per_numprog1_q1_tot, Benefit_incd_allSPL = per_allsp.ben_q1_tot,
         Gini_ineqty_chnge = per_allsp_gini_tot, Poverty_change = per_allsp_p0_ep_tot) %>%
  select(country, year, Coverage_allSPL, Benefit_Adeq_allSPL, Beneficiary_incd_allSPL, Atleast_two_SPL, 
         Poorest_Quitle_oneSPL, Benefit_incd_allSPL, Gini_ineqty_chnge, Poverty_change)

summary(Soc_protect_data)

#### Due to too many missing values on ASPIRE data database, we are resorting to Social protection floor on UN stat SDG 

library(devtools)
devtools::install_github("DrMattG/SDGsR", dependencies = TRUE) # Load SDGsR package
library(SDGsR)
year <- 2000:2021
country <- unique(oda_disb$country)

#### SDG Social Protection Floor ######
## Social Protection Floor is measured in SDG 1.3.1 using twelve indicators
# Indicator 1.3.1, Series : [ILO] Proportion of mothers with newborns receiving maternity cash benefit (%) SI_COV_MATNL
indicator_1.3.1 <- get_indicator_data(indicator = "1.3.1")
unique(indicator_1.3.1$dimensions)
summary(Floor_a)
Floor_a <- indicator_1.3.1 %>%
  filter(geoAreaName %in% country,
         series == "SI_COV_MATNL",
         #dimensions$`Type of occupation` == "NURS",
         #dimensions$Age == "15-19",
         #dimensions$Sex == "BOTHSEX",
         timePeriodStart %in% year
  ) %>%
  select(geoAreaName, timePeriodStart, value) %>% 
  mutate(across(c(value, timePeriodStart), as.numeric),  # Treat value and timePeriodStart as numbers
         value = round(value, 3)) %>%
  rename(Maternity_cash_benefit = value, country = geoAreaName, year = timePeriodStart)


## Merge df as we go 
oda_disb <- merge(oda_disb, Floor_a, by = c("country", "year"), all = TRUE)
rm(Floor_a)
summary(oda_disb)


## Indicator 1.3.1, Series : [ILO] Proportion of poor population receiving social assistance cash benefit, by sex (%) SI_COV_POOR
unique(indicator_1.3.1$series)
Floor_b <- indicator_1.3.1 %>%
  filter(geoAreaName %in% country,
         series == "SI_COV_POOR",
         #dimensions$`Type of occupation` == "NURS",
         #dimensions$Age == "15-19",
         dimensions$Sex == "BOTHSEX",
         timePeriodStart %in% year
  ) %>%
  select(geoAreaName, timePeriodStart, value) %>% 
  mutate(across(c(value, timePeriodStart), as.numeric),  # Treat value and timePeriodStart as numbers
         value = round(value, 3)) %>%
  rename(Cov_poor_pop = value, country = geoAreaName, year = timePeriodStart)

## Merge df as we go 
oda_disb <- merge(oda_disb, Floor_b, by = c("country", "year"), all = TRUE)
summary(Floor_b)
summary(oda_disb)


#  Indicator 1.3.1, Series : [World Bank] Proportion of population covered by social assistance programs (%) SI_COV_SOCAST
unique(indicator_1.3.1$dimensions)
Floor_c <- indicator_1.3.1 %>%
  filter(geoAreaName %in% country,
         series == "SI_COV_SOCAST",
         #dimensions$`Type of occupation` == "NURS",
         dimensions$Quantile == "_T",
         # dimensions$Sex == "BOTHSEX",
         timePeriodStart %in% year
  ) %>%
  select(geoAreaName, timePeriodStart, value) %>% 
  mutate(across(c(value, timePeriodStart), as.numeric),  # Treat value and timePeriodStart as numbers
         value = round(value, 3)) %>%
  rename(Soc_Assistance_cov = value, country = geoAreaName, year = timePeriodStart)

summary(Floor_c)

## Merge df as we go 
oda_disb <- merge(oda_disb, Floor_c, by = c("country", "year"), all = TRUE)
rm(Floor_b)
summary(oda_disb)

# Indicator 1.3.1, Series : [World Bank] Proportion of population covered by social insurance programs (%) SI_COV_SOCINS
unique(indicator_1.3.1$series)
Floor_d <- indicator_1.3.1 %>%
  filter(geoAreaName %in% country,
         series == "SI_COV_SOCINS",
         #dimensions$`Type of occupation` == "NURS",
         dimensions$Quantile == "_T",
         # dimensions$Sex == "BOTHSEX",
         timePeriodStart %in% year
  ) %>%
  select(geoAreaName, timePeriodStart, value) %>% 
  mutate(across(c(value, timePeriodStart), as.numeric),  # Treat value and timePeriodStart as numbers
         value = round(value, 3)) %>%
  rename(Soc_Insur_cov = value, country = geoAreaName, year = timePeriodStart)

summary(Floor_d)

## Merge df as we go 
oda_disb <- merge(oda_disb, Floor_d, by = c("country", "year"), all = TRUE)
rm(Floor_b)
summary(oda_disb)

# Indicator 1.3.1, Series : [ILO] Proportion of children/households receiving child/family cash benefit, by sex (%) SI_COV_CHLD

unique(indicator_1.3.1$series)
Floor_e <- indicator_1.3.1 %>%
  filter(geoAreaName %in% country,
         series == "SI_COV_CHLD",
         #dimensions$`Type of occupation` == "NURS",
         # dimensions$Quantile == "_T",
         dimensions$Sex == "BOTHSEX",
         timePeriodStart %in% year
  ) %>%
  select(geoAreaName, timePeriodStart, value) %>% 
  mutate(across(c(value, timePeriodStart), as.numeric),  # Treat value and timePeriodStart as numbers
         value = round(value, 3)) %>%
  rename(Child_benef_cov = value, country = geoAreaName, year = timePeriodStart)

summary(Floor_e)

## Merge df as we go 
oda_disb <- merge(oda_disb, Floor_e, by = c("country", "year"), all = TRUE)
rm(Floor_b)
summary(oda_disb)


# Indicator 1.3.1, Series : [ILO] Proportion of unemployed persons receiving unemployment cash benefit, by sex (%) SI_COV_UEMP

unique(indicator_1.3.1$series)
Floor_f <- indicator_1.3.1 %>%
  filter(geoAreaName %in% country,
         series ==  "SI_COV_UEMP",
         #dimensions$`Type of occupation` == "NURS",
         # dimensions$Quantile == "_T",
         dimensions$Sex == "BOTHSEX",
         timePeriodStart %in% year
  ) %>%
  select(geoAreaName, timePeriodStart, value) %>% 
  mutate(across(c(value, timePeriodStart), as.numeric),  # Treat value and timePeriodStart as numbers
         value = round(value, 3)) %>%
  rename(Unemplt_beneft_cov = value, country = geoAreaName, year = timePeriodStart)

summary(Floor_f)

## Merge df as we go 
oda_disb <- merge(oda_disb, Floor_f, by = c("country", "year"), all = TRUE)
rm(Floor_b)
summary(oda_disb)

# Indicator 1.3.1, Series : [ILO] Proportion of vulnerable population receiving social assistance cash benefit, by sex (%) SI_COV_VULN
unique(indicator_1.3.1$series)
Floor_g <- indicator_1.3.1 %>%
  filter(geoAreaName %in% country,
         series ==  "SI_COV_VULN",
         #dimensions$`Type of occupation` == "NURS",
         # dimensions$Quantile == "_T",
         dimensions$Sex == "BOTHSEX",
         timePeriodStart %in% year
  ) %>%
  select(geoAreaName, timePeriodStart, value) %>% 
  mutate(across(c(value, timePeriodStart), as.numeric),  # Treat value and timePeriodStart as numbers
         value = round(value, 3)) %>%
  rename(Vulnrb_pop_cov = value, country = geoAreaName, year = timePeriodStart)

summary(Floor_g)

## Merge df as we go 
oda_disb <- merge(oda_disb, Floor_g, by = c("country", "year"), all = TRUE)
rm(Floor_f)
summary(oda_disb)


#  Indicator 1.3.1, Series : [ILO] Proportion of employed population covered in the event of work injury, by sex (%) SI_COV_WKINJRY
unique(indicator_1.3.1$series)
Floor_h <- indicator_1.3.1 %>%
  filter(geoAreaName %in% country,
         series ==  "SI_COV_WKINJRY",
         #dimensions$`Type of occupation` == "NURS",
         # dimensions$Quantile == "_T",
         dimensions$Sex == "BOTHSEX",
         timePeriodStart %in% year
  ) %>%
  select(geoAreaName, timePeriodStart, value) %>% 
  mutate(across(c(value, timePeriodStart), as.numeric),  # Treat value and timePeriodStart as numbers
         value = round(value, 3)) %>%
  rename(work_injury_cov = value, country = geoAreaName, year = timePeriodStart)

summary(Floor_h)

## Merge df as we go 
oda_disb <- merge(oda_disb, Floor_h, by = c("country", "year"), all = TRUE)
rm(Floor_f)
summary(oda_disb)

# Indicator 1.3.1, Series : [ILO] Proportion of population covered by at least one social protection benefit, by sex (%) SI_COV_BENFTS

unique(indicator_1.3.1$series)
Floor_i <- indicator_1.3.1 %>%
  filter(geoAreaName %in% country,
         series ==  "SI_COV_BENFTS",
         #dimensions$`Type of occupation` == "NURS",
         # dimensions$Quantile == "_T",
         dimensions$Sex == "BOTHSEX",
         timePeriodStart %in% year
  ) %>%
  select(geoAreaName, timePeriodStart, value) %>% 
  mutate(across(c(value, timePeriodStart), as.numeric),  # Treat value and timePeriodStart as numbers
         value = round(value, 3)) %>%
  rename(Atleast_oneSP_cov = value, country = geoAreaName, year = timePeriodStart)

## Merge df as we go 
oda_disb <- merge(oda_disb, Floor_i, by = c("country", "year"), all = TRUE)
rm(Floor_i)
summary(oda_disb)



#  Indicator 1.3.1, Series : [ILO] Proportion of population with severe disabilities receiving disability cash benefit, by sex (%) SI_COV_DISAB
unique(indicator_1.3.1$series)
Floor_i <- indicator_1.3.1 %>%
  filter(geoAreaName %in% country,
         series ==  "SI_COV_DISAB",
         #dimensions$`Type of occupation` == "NURS",
         # dimensions$Quantile == "_T",
         dimensions$Sex == "BOTHSEX",
         timePeriodStart %in% year
  ) %>%
  select(geoAreaName, timePeriodStart, value) %>% 
  mutate(across(c(value, timePeriodStart), as.numeric),  # Treat value and timePeriodStart as numbers
         value = round(value, 3)) %>%
  rename(Disab_benef_cov = value, country = geoAreaName, year = timePeriodStart)

summary(Floor_i)

## Merge df as we go 
oda_disb <- merge(oda_disb, Floor_i, by = c("country", "year"), all = TRUE)
rm(Floor_f)
summary(oda_disb)

#  Indicator 1.3.1, Series : [World Bank] Proportion of population covered by labour market programs (%) SI_COV_LMKT
unique(indicator_1.3.1$series)
Floor_i <- indicator_1.3.1 %>%
  filter(geoAreaName %in% country,
         series ==  "SI_COV_LMKT",
         #dimensions$`Type of occupation` == "NURS",
         dimensions$Quantile == "_T",
         # dimensions$Sex == "BOTHSEX",
         timePeriodStart %in% year
  ) %>%
  select(geoAreaName, timePeriodStart, value) %>% 
  mutate(across(c(value, timePeriodStart), as.numeric),  # Treat value and timePeriodStart as numbers
         value = round(value, 3)) %>%
  rename(LMKT_benef_cov = value, country = geoAreaName, year = timePeriodStart)

summary(Floor_i)

## Merge df as we go 
oda_disb <- merge(oda_disb, Floor_i, by = c("country", "year"), all = TRUE)
rm(Floor_f)
summary(oda_disb)


#  Indicator 1.3.1, Series : [ILO] Proportion of population above statutory pensionable age receiving a pension, by sex (%) SI_COV_PENSN
unique(indicator_1.3.1$series)
Floor_i <- indicator_1.3.1 %>%
  filter(geoAreaName %in% country,
         series ==  "SI_COV_PENSN",
         #dimensions$`Type of occupation` == "NURS",
         # dimensions$Quantile == "_T",
         dimensions$Sex == "BOTHSEX",
         timePeriodStart %in% year
  ) %>%
  select(geoAreaName, timePeriodStart, value) %>% 
  mutate(across(c(value, timePeriodStart), as.numeric),  # Treat value and timePeriodStart as numbers
         value = round(value, 3)) %>%
  rename(Pension_cov = value, country = geoAreaName, year = timePeriodStart)

summary(Floor_i)

## Merge df as we go 
oda_disb <- merge(oda_disb, Floor_i, by = c("country", "year"), all = TRUE)
rm(Floor_f)
summary(oda_disb)





##### Saving the updated data ########
write.csv2(oda_disb, "Health_with_ODA_data.csv")
write_rds(oda_disb, "Health_with_ODA_data.rds")







#ggsave("../Figures/Chard/test.png",plot=plot(q),width=8,height=4,dpi=300,  units = "cm")


#if (FALSE) {
# ggplot(mtcars, aes(mpg, wt)) +
#  geom_point()

#ggsave("mtcars.pdf")
#ggsave("mtcars.png")

#ggsave("mtcars.pdf", width = 4, height = 4)
#ggsave("mtcars.pdf", width = 20, height = 20, units = "cm")

# delete files with base::unlink()
#  unlink("mtcars.pdf")
# unlink("mtcars.png")

# specify device when saving to a file with unknown extension
# (for example a server supplied temporary file)
#file <- tempfile()
#ggsave(file, device = "pdf")
#unlink(file)

# save plot to file without using ggsave
#p <-
# ggplot(mtcars, aes(mpg, wt)) +
#geom_point()
#png("mtcars.png")
#print(p)
#dev.off()

#}







# save a single plot with a legend
#p1 <- ggplot(mpg, aes(x = cty, y = hwy, color = factor(cyl))) +
# geom_point(size = 2) +
#theme_half_open()

#file1 <- tempfile("file1", fileext = ".png")
#file2 <- tempfile("file2", fileext = ".png")
#save_plot(file1, p1)
# same as file1 but determine base_width given base_height
#save_plot(file2, p1, base_height = NULL, base_width = 6)

# save a single plot without legend, adjust aspect ratio
#x <- (1:100)/10
#p3 <- ggplot(data.frame(x = x, y = x*sin(x)), aes(x, y)) +
#  geom_line() +
# theme_minimal_hgrid()
#file3 <- tempfile("file3", fileext = ".pdf")
#save_plot(file3, p3, base_asp = 1.1)

# now combine with a second plot and save
#p3b <- ggplot(data.frame(x = x, y = cos(x)+x), aes(x, y)) +
#  geom_line() +
# theme_minimal_hgrid()
#p4 <- plot_grid(p3, p3b, labels = "AUTO")
#file4 <- tempfile("file4", fileext = ".pdf")
#save_plot(file4, p4, ncol = 2, base_asp = 1.1)


#https://rdrr.io/cran/cowplot/man/save_plot.html
