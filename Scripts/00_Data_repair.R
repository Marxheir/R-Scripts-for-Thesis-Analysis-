####### Load data to query list of countries ######
health <- read.csv2("Health_with_ODA_data.csv", sep = ";")
oda_disb <- read.csv2("Total_and_Sector_ODA.csv", sep = ";")



##### THE DATASET IS DUPLICATED ALL ALONG #######
## ODA sector data caused the problem 
summary(health)
summary(oda_disb)
unique(health$iso3c)
unique(oda_disb$iso3c) ## Remove unwanted countries from new ODA

######## Remove duplicated data ################
# duplicat <- health[duplicated(health[, c("country", "iso3c", "year")]), ]
health <- health[!duplicated(health[, c("country", "iso3c", "year")]), ]


health <- health %>%
  select(!c(X, oda_disb, SOC_INF_ODA))

# Assuming 'oda_disb' and 'health' are your datasets
oda_disb <- merge(health, oda_disb, by = c("country", "iso3c", "year"), all.x = TRUE)
summary(health)
summary(oda_disb)

# oda_disb <- oda_disb %>%
#  select(-regions)

write.csv2(oda_disb, "Health_with_ODA_data.csv")
write_rds(oda_disb, "Health_with_ODA_data.rds")

# rm(health_t, health, oda_disb_t)
