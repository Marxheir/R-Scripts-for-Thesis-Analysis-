############## DATA FOR OFFICIAL DEVELOPMENT ASSISTANCE (ODA) MERGING AND CLEANING #############

# Note: Data is downloaded in Excel data inspection 
# Note: Regional classification is already done in Excel using the UNICEF programme regional classification 
# 6 Regions: MENA, SCA, Europe, LAC, SSA

library(readxl)
########## LOADING OECD DATA FOR ODA ############
oda_disb <- read_excel("Raw_data/Net ODA by recipients at constant price 2021.xls.xlsx", sheet = 1) # For total ODA


oda_list <- unique(ODA_list)

#### Cleaning of data ####
## a. Turn data to long format
oda_disb <- oda_disb %>%
  gather(key = "year", value = "oda_disb", -c(countries, regions))

## b. Turn year and value column to double format
oda_disb$year <- as.numeric(oda_disb$year)
oda_disb$oda_disb <- as.numeric(oda_disb$oda_disb)


## c. There are about 424 missing values with negative values
oda_disb <- oda_disb %>%
  mutate(oda_disb = replace_na(oda_disb, 0))

## d. Turn country names to iso3c codes 
oda_disb <- oda_disb %>%
  mutate(iso3c = countrycode(sourcevar  = countries, origin = "country.name", 
                             destination = "iso3c"))

## Create a manual iso code for the two countries missing
# Kosovo, Micronesia are not found in the country code. We do it manually 
exceptions <- data.frame(
  countries = c("Kosovo", "Micronesia"), 
  iso3c = c("XKX", "FSM")
)

## merge the temporary exceptions created 
oda_disb <- merge(oda_disb, exceptions, by = "countries", all.x = TRUE)
oda_disb$iso3c <- ifelse(is.na(oda_disb$iso3c.x), oda_disb$iso3c.y, oda_disb$iso3c.x)
oda_disb <- oda_disb[, !(names(oda_disb) %in% c("iso3c.x", "iso3c.y"))]

# rm(exceptions) 

##### SOCIAL INFRASTURCTURE SECTOR ODA DATA WITH EXISTING COUNTRIES IN THE CLEAN DATA #########
Sector_ODA <- read.csv2("Raw_data/Social Infrastructure Sectors ODA.csv", sep = ",") # For Sector ODA on Social Infrastructure 
Sector_ODA <- read_excel("Raw_data/Social Infrastructure ODA.xlsx", sheet = "Soc", col_names = TRUE) # For total ODA

# Note: Sector Data begins from 2005 to 2021 
# Extract year, value and existing countries in the total cleaned ODA
countries <- unique(oda_disb$countries)

# Filter the data for the selected countries
Sector_ODA <- Sector_ODA %>%
  filter(country %in% countries) %>%
  select(country, starts_with("20")) %>%
  pivot_longer(cols = starts_with("20"), names_to = "Year", values_to = "SOC_INF_ODA")


## Change the values to numbers
Sector_ODA$Year <- as.numeric(Sector_ODA$Year)
Sector_ODA$SOC_INF_ODA <- as.numeric(Sector_ODA$SOC_INF_ODA)



# head(Sector_ODA, 20)
summary(oda_disb)
summary(Sector_ODA)
oda_t <- oda_disb

####### Merge the two data on ODA total and Sector for Social Infrastructure #######
oda_disb <- merge(oda_disb, Sector_ODA, by = c("country", "year"), all.x = TRUE)


# head(oda_disb, 20)

######### Save the data with ODA_rawdata
write_csv2(oda_disb, "Raw_data/Total_and_Sector_ODA.csv")




##### Basic Exploration of ODA Data ########

####### Creating Categories of countries based on Average ODA in last 10years ######
oda_disb_last10yrs <- oda_disb%>%
  filter(year >= 2011)


# Calculate average and create groups 
average_oda <- oda_disb_last10yrs %>%
  group_by(iso3c) %>%
  summarise(AvgODA = mean(oda_disb, na.rm = TRUE)) %>%
  mutate(ODA_Group = cut(AvgODA, breaks = 3, labels = c("Low", "Medium", "High")))

# Creating Maps for average value in the last 10years 

world_cordinates <- map_data("world")
yes
head(world_cordinates)

world_cordinates <- world_cordinates %>%
  rename(countries = region) %>%
  mutate(iso3c = countrycode(sourcevar = countries, origin = "country.name", 
                             destination = "iso3c"))

world_map <- left_join(world_cordinates, average_oda, by = "iso3c")

map1 = ggplot(world_map, aes(x = long, y = lat, group = group)) + 
  geom_polygon(aes(fill = AvgODA), color = "white")


plot(map1)

map2 = map1 + scale_fill_gradient(name = "AvgODA", low = "yellow", high = "red", 
                                  na.value = "grey") + 
  theme(rect =element_blank(), axis.title.x = element_blank(), axis.title.y = element_blank(), 
        axis.text = element_blank()) + 
  labs(title = "Top Recipients of Net ODA in the last 10years (2012:2021)")




interactive_map <- ggplotly(map2)


# library(countrycode, sf, map_data)

# Assuming "Country" is the column containing country names and "Net_ODA" is the column for Net ODA
top_recipients <- oda_disb %>%
  group_by(countries) %>%
  summarize(total_net = sum(oda_disb, na.rm = TRUE)) %>%
  arrange(desc(total_net)) %>%
  head(10)

# Assuming "Country" is the column containing country names
top_recipients_data <- oda_disb %>%
  filter(countries %in% top_recipients$countries)

ggplot(top_recipients_data, aes(x = countries, y = oda_disb)) +
  geom_boxplot() +
  labs(title = "Statistical Summary of Net ODA for Top 10 Recipients",
       x = "Country", y = "Net ODA")
