# load libraries ####
library(haven)
library(tidyverse)
library(tidyr)
library(corrplot)
library(Hmisc)
library(PerformanceAnalytics)
library(writexl)
library(psych)
library(MASS)
library(stargazer)
library(htmlTable)
library(plm)
library(reshape2)
library(dplyr)

# load all ebs and input data ####
setwd("C:/Users/JohnDoe/Desktop/Master/R")

# eb data
eb_2019_original <- read_dta("2019_ZA7576_v1-0-0.dta")
eb_2020_original <- read_dta("2020_ZA7649_v2-0-0.dta")
eb_2021_original <- read_dta("2021_ZA7783_v1-0-0.dta")
eb_2022_original <- read_dta("2022_ZA7902_v1-0-0.dta")


# ####
# gdp per capita data ####
gdp_per_capita_PPP <- readxl::read_xlsx("gdp_per_capita_WEO.xlsx", col_names = TRUE, sheet = "data")

# load consultation data ####
input <- readxl::read_xlsx("DataOutput_year.xlsx", col_names = TRUE, sheet = "data")

# assign them to the dfs i use in the further coding ####
eb_19 <- eb_2019_original
eb_20 <- eb_2020_original
eb_21 <- eb_2021_original
eb_22 <- eb_2022_original


# creating subsetting vectors for the DV data ####
subset_vector <- c("Austria", 
                   "Belgium", 
                   "Bulgaria", 
                   "Cyprus", 
                   "Czechia", 
                   "Germany", 
                   "Denmark", 
                   "Estonia", 
                   "Spain", 
                   "Finland", 
                   "France", 
                   "Greece", 
                   "Croatia",
                   "Hungary", 
                   "Ireland", 
                   "Italy", 
                   "Lithuania", 
                   "Luxembourg", 
                   "Latvia", 
                   "Malta", 
                   "Netherlands", 
                   "Poland", 
                   "Portugal", 
                   "Romania",
                   "Sweden",
                   "Slovenia", 
                   "Slovakia", 
                   "EU citizen",	"Company/business",	"Non-governmental organisation (NGO)",	"Public authority",	"Other	Non-EU citizen", "Business association",	"Trade union",	"Consumer organisation",	"Environmental organisation",	"Academic/research Institution",
                   "legislation",	"category",	"sum", "year"
)

names.use <- names(input)[(names(input) %in% subset_vector)]
input <- input[, names.use]


# creating a country_info df with population information ####
country_info <- data.frame(
  country_column = c("AT",
                    "BE",
                    "BG",
                    "CY",
                    "CZ",
                    "DE",
                    "DK",
                    "EE",
                    "ES",
                    "FI",
                    "FR",
                    "GR",
                    "HR",
                    "HU",
                    "IE", 
                    "IT", 
                    "LT", 
                    "LU", 
                    "LV", 
                    "MT", 
                    "NL", 
                    "PL", 
                    "PT", 
                    "RO",
                    "SE",
                    "SI", 
                    "SK"),
population_column = c(8932664,
                       11554767,
                       6916548,
                       896007,
                       10701777,
                       83155031,
                       5840045,
                       1330068,
                       47398695,
                       5533793,
                       67656682,
                       10678632,
                       4036355,
                       9730772,
                       5006324,
                       59236213,
                       2795680,
                       634730,
                       1893223,
                       516100,
                       17475415,
                       37840001,
                       10298252,
                       19201662,
                       10379295,
                       2108977,
                       5459781))
# country_info <- data.frame(country_column, population_column)
names(country_info)[which(names(country_info) == "country_column")] <- "isocntry"
names(country_info)[which(names(country_info) == "population_column")] <- "population"

# Calculating the total population
country_info$pop_share <- NULL
total_population <- sum(country_info$population)  
country_info$pop_share <- country_info$population / total_population  # Calculating the population share


# subsetting and renaming the eb dataframes to have 27 EU member states ####
cntry <- c("AT", "BE", "BG", "CY", "CZ", "DE-W", "DE-E", "DK", "EE", "ES", "FI", "FR", "GR", "HR",
           "HU", "IE", "IT", "LT", "LU", "LV", "MT", "NL", "PL", "PT", "RO", "SE", "SI", "SK")

## Subset the eb dataframes based on the country codes
subset_eb_19 <- eb_19[eb_19$isocntry %in% cntry, ]
subset_eb_20 <- eb_20[eb_20$isocntry %in% cntry, ]
subset_eb_21 <- eb_21[eb_21$isocntry %in% cntry, ]
subset_eb_22 <- eb_22[eb_22$isocntry %in% cntry, ]

## merge DE-W and DE-E to DE
subset_eb_19 <- subset_eb_19 %>% mutate(isocntry=recode(isocntry, "DE-W"="DE", "DE-E"="DE"))
subset_eb_20 <- subset_eb_20 %>% mutate(isocntry=recode(isocntry, "DE-W"="DE", "DE-E"="DE"))
subset_eb_21 <- subset_eb_21 %>% mutate(isocntry=recode(isocntry, "DE-W"="DE", "DE-E"="DE"))
subset_eb_22 <- subset_eb_22 %>% mutate(isocntry=recode(isocntry, "DE-W"="DE", "DE-E"="DE"))

subset_eb_19$year <- 2019
subset_eb_20$year <- 2020
subset_eb_21$year <- 2021
subset_eb_22$year <- 2022


# rename the column of input to isocntry ####
names(input)[which(names(input) == "A2")] <- "isocntry"

# calculate the share of the responses of each country for every legislative proposal in the input data frame ####
input$Austria <- input$Austria / input$sum
input$Belgium <- input$Belgium / input$sum
input$Bulgaria <- input$Bulgaria / input$sum
input$Cyprus <- input$Cyprus / input$sum
input$Czechia <- input$Czechia / input$sum
input$Germany <- input$Germany / input$sum
input$Denmark <- input$Denmark / input$sum
input$Estonia <- input$Estonia / input$sum
input$Spain <- input$Spain / input$sum
input$Finland  <- input$Finland / input$sum
input$France <- input$France / input$sum
input$Greece <- input$Greece / input$sum
input$Croatia <- input$Croatia / input$sum
input$Hungary <- input$Hungary / input$sum
input$Ireland <- input$Ireland / input$sum
input$Italy <- input$Italy / input$sum
input$Lithuania <- input$Lithuania / input$sum
input$Luxembourg <- input$Luxembourg / input$sum
input$Latvia <- input$Latvia / input$sum
input$Malta <- input$Malta / input$sum
input$Netherlands <- input$Netherlands / input$sum
input$Poland <- input$Poland / input$sum
input$Portugal <- input$Portugal / input$sum
input$Romania <- input$Romania / input$sum
input$Sweden <- input$Sweden / input$sum
input$Slovenia <- input$Slovenia / input$sum
input$Slovakia <- input$Slovakia / input$sum


#rename the column headers to country shortcuts
names(input)[which(names(input) == "Austria")] <- "AT"
names(input)[which(names(input) == "Belgium")] <- "BE"
names(input)[which(names(input) == "Bulgaria")] <- "BG"
names(input)[which(names(input) == "Cyprus")] <- "CY"
names(input)[which(names(input) == "Czechia")] <- "CZ"
names(input)[which(names(input) == "Germany")] <- "DE"
names(input)[which(names(input) == "Denmark")] <- "DK"
names(input)[which(names(input) == "Estonia")] <- "EE"
names(input)[which(names(input) == "Spain")] <- "ES"
names(input)[which(names(input) == "Finland")] <- "FI"
names(input)[which(names(input) == "France")] <- "FR"
names(input)[which(names(input) == "Greece")] <- "GR"
names(input)[which(names(input) == "Croatia")] <- "HR"
names(input)[which(names(input) == "Hungary")] <- "HU"
names(input)[which(names(input) == "Ireland")] <- "IE"
names(input)[which(names(input) == "Italy")] <- "IT"
names(input)[which(names(input) == "Lithuania")] <- "LT"
names(input)[which(names(input) == "Luxembourg")] <- "LU"
names(input)[which(names(input) == "Latvia")] <- "LV"
names(input)[which(names(input) == "Malta")] <- "MT"
names(input)[which(names(input) == "Netherlands")] <- "NL"
names(input)[which(names(input) == "Poland")] <- "PL"
names(input)[which(names(input) == "Portugal")] <- "PT"
names(input)[which(names(input) == "Romania")] <- "RO"
names(input)[which(names(input) == "Sweden")] <- "SE"
names(input)[which(names(input) == "Slovenia")] <- "SI"
names(input)[which(names(input) == "Slovakia")] <- "SK"






# ####
# Build the panel dataframe "daten" ####

# Create an empty dataframe "daten" with the appropriate structure
daten <- data.frame(
  legislation = rep(unique(input$legislation), length(unique(colnames(input)[5:31]))),
  isocntry = rep(unique(colnames(input)[5:31]), each = length(unique(input$legislation))),
  year = NA,  
  part_share = NA,
  category = NA
)

# Iterate over each legislation and country combination in "daten"
for (i in 1:nrow(daten)) {
  legislation <- daten$legislation[i]
  isocntry <- daten$isocntry[i]
  
  # Get the corresponding part_share, year and category values from "input"
  part_share <- input[input$legislation == legislation, isocntry] # for
  year <- input[input$legislation == legislation, "year"]
  category <- input[input$legislation == legislation, "category"]
  
  # Assign the part_share and year values to "daten"
  daten$part_share[i] <- part_share
  daten$year[i] <- year
  daten$category[i] <- category
}

## add country_info data to "daten"
daten <- merge(daten, country_info, by.x = "isocntry", by.y = "isocntry", all.x = TRUE)

# calculate and add dependent variable "y" for each country and each legislation ####

# percentage difference between expected and observed share
daten$y <- vector("numeric", nrow(daten))
daten$y <- as.numeric(((as.numeric(daten$part_share) - as.numeric(daten$pop_share)) / as.numeric(daten$pop_share) * 100))
# ratio of expected and observed share
daten$y_1 <- vector("numeric", nrow(daten))
daten$y_1 <- as.numeric(as.numeric(as.numeric(daten$part_share)+0.0001) / as.numeric(daten$pop_share))
daten$y_2 <- vector("numeric", nrow(daten))
daten$y_2 <- as.numeric(as.numeric(as.numeric(daten$part_share)) / as.numeric(daten$pop_share))



# log of ratio
daten$y_log <- vector("numeric", nrow(daten))
daten$y_log_helper <- vector("numeric", nrow(daten))
daten$y_log_helper <- daten$y_1 
daten$y_log <- as.numeric(log(daten$y_log_helper))
daten$y_log[daten$y_log == "-Inf"] <- NA

# log of ratio 2
daten$y_log_2 <- vector("numeric", nrow(daten))
daten$y_log_helper_2 <- vector("numeric", nrow(daten))
daten$y_log_helper_2 <- daten$y_2 
daten$y_log_2 <- as.numeric(log(daten$y_log_helper_2))
daten$y_log_2[daten$y_log_2 == "-Inf"] <- NA


daten_2019 <- daten[daten$year == 2019, ]
daten_2020 <- daten[daten$year == 2020, ]
daten_2021 <- daten[daten$year == 2021, ]
daten_2022 <- daten[daten$year == 2022, ]

# aggregate and add independent variables ####

# for 2019 ####

# #### (1) trust_pol_parties qa6a_2
# subset_eb_19$qa6a_2 <- replace(subset_eb_19$qa6a_2, subset_eb_19$qa6a_2 == 3, NA)
# subset_eb_19$qa6a_2 <- replace(subset_eb_19$qa6a_2, subset_eb_19$qa6a_2 == 2, -1)
# # aggregate to country-level data
# eb_19_by_country <- aggregate(qa6a_2 ~ isocntry, data = subset_eb_19, FUN = mean)
# eb_19_by_country$year <- as.numeric(2019)
# #merge
# daten_2019 <- merge(daten_2019, eb_19_by_country, by = c("isocntry", "year"), all.x = TRUE)

# #### (2) trust_eu: qa6a_10
# subset_eb_19$qa6a_10 <- replace(subset_eb_19$qa6a_10, subset_eb_19$qa6a_10 == 3, NA)
# subset_eb_19$qa6a_10 <- replace(subset_eb_19$qa6a_10, subset_eb_19$qa6a_10 == 2, -1)
# # aggregate to country-level data
# eb_19_by_country <- aggregate(qa6a_10 ~ isocntry, data = subset_eb_19, FUN = mean)
# eb_19_by_country$year <- as.numeric(2019)
# daten_2019 <- merge(daten_2019, eb_19_by_country, by = c("isocntry", "year"), all.x = TRUE)

# ####(3) image_eu: qa7
# subset_eb_19$qa7 <- replace(subset_eb_19$qa7, subset_eb_19$qa7 == 5, -2)
# subset_eb_19$qa7 <- replace(subset_eb_19$qa7, subset_eb_19$qa7 == 4, -1)
# subset_eb_19$qa7 <- replace(subset_eb_19$qa7, subset_eb_19$qa7 == 3, 0)
# subset_eb_19$qa7 <- replace(subset_eb_19$qa7, subset_eb_19$qa7 == 2, 11)
# subset_eb_19$qa7 <- replace(subset_eb_19$qa7, subset_eb_19$qa7 == 1, 2)
# subset_eb_19$qa7 <- replace(subset_eb_19$qa7, subset_eb_19$qa7 == 11, 1)
# subset_eb_19$qa7 <- replace(subset_eb_19$qa7, subset_eb_19$qa7 == 6, NA)
# # aggregate to country-level data
# eb_19_by_country <- aggregate(qa7 ~ isocntry, data = subset_eb_19, FUN = mean)
# eb_19_by_country$year <- as.numeric(2019)
# daten_2019 <- merge(daten_2019, eb_19_by_country, by = c("isocntry", "year"), all.x = TRUE)

#### (4) discuss_EU: d71a_2
subset_eb_19$d71a_2 <- replace(subset_eb_19$d71a_2, subset_eb_19$d71a_2 == 4, NA)
subset_eb_19$d71a_2 <- replace(subset_eb_19$d71a_2, subset_eb_19$d71a_2 == 2, 0)
subset_eb_19$d71a_2 <- replace(subset_eb_19$d71a_2, subset_eb_19$d71a_2 == 3, -1)
### aggregate to country-level data
eb_19_by_country <- aggregate(d71a_2 ~ isocntry, data = subset_eb_19, FUN = mean)
eb_19_by_country$year <- as.numeric(2019)
daten_2019 <- merge(daten_2019, eb_19_by_country, by = c("isocntry", "year"), all.x = TRUE)

# #### (5) attach_EU: qd1a_3
# subset_eb_19$qd1a_3 <- replace(subset_eb_19$qd1a_3, subset_eb_19$qd1a_3 == 5, NA)
# subset_eb_19$qd1a_3 <- replace(subset_eb_19$qd1a_3, subset_eb_19$qd1a_3 == 4, -3)
# subset_eb_19$qd1a_3 <- replace(subset_eb_19$qd1a_3, subset_eb_19$qd1a_3 == 3, -1)
# subset_eb_19$qd1a_3 <- replace(subset_eb_19$qd1a_3, subset_eb_19$qd1a_3 == 1, 3)
# subset_eb_19$qd1a_3 <- replace(subset_eb_19$qd1a_3, subset_eb_19$qd1a_3 == 2, 1)
# ### aggregate to country-level data
# eb_19_by_country <- aggregate(qd1a_3 ~ isocntry, data = subset_eb_19, FUN = mean)
# eb_19_by_country$year <- as.numeric(2019)
# daten_2019 <- merge(daten_2019, eb_19_by_country, by = c("isocntry", "year"), all.x = TRUE)

#### (6) citizen_EU: qd2_1
subset_eb_19$qd2_1 <- replace(subset_eb_19$qd2_1, subset_eb_19$qd2_1 == 5, NA)
subset_eb_19$qd2_1 <- replace(subset_eb_19$qd2_1, subset_eb_19$qd2_1 == 4, -3/3)
subset_eb_19$qd2_1 <- replace(subset_eb_19$qd2_1, subset_eb_19$qd2_1 == 3, -1/3)
subset_eb_19$qd2_1 <- replace(subset_eb_19$qd2_1, subset_eb_19$qd2_1 == 1, 3/3)
subset_eb_19$qd2_1 <- replace(subset_eb_19$qd2_1, subset_eb_19$qd2_1 == 2, 1/3)
### aggregate to country-level data
eb_19_by_country <- aggregate(qd2_1 ~ isocntry, data = subset_eb_19, FUN = mean)
eb_19_by_country$year <- as.numeric(2019)
daten_2019 <- merge(daten_2019, eb_19_by_country, by = c("isocntry", "year"), all.x = TRUE)

# ### (7) es_future_outside: qa18a_5
# subset_eb_19$qa18a_5 <- replace(subset_eb_19$qa18a_5, subset_eb_19$qa18a_5 == 5, NA)
# subset_eb_19$qa18a_5 <- replace(subset_eb_19$qa18a_5, subset_eb_19$qa18a_5 == 4, -3)
# subset_eb_19$qa18a_5 <- replace(subset_eb_19$qa18a_5, subset_eb_19$qa18a_5 == 3, -1)
# subset_eb_19$qa18a_5 <- replace(subset_eb_19$qa18a_5, subset_eb_19$qa18a_5 == 1, 3)
# subset_eb_19$qa18a_5 <- replace(subset_eb_19$qa18a_5, subset_eb_19$qa18a_5 == 2, 1)
# ### aggregate to country-level data
# eb_19_by_country <- aggregate(qa18a_5 ~ isocntry, data = subset_eb_19, FUN = mean)
# eb_19_by_country$year <- as.numeric(2019)
# daten_2019 <- merge(daten_2019, eb_19_by_country, by = c("isocntry", "year"), all.x = TRUE)

### (8)  trust_com: qa14_2
subset_eb_19$qa14_2 <- replace(subset_eb_19$qa14_2, subset_eb_19$qa14_2 == 3, NA)
subset_eb_19$qa14_2 <- replace(subset_eb_19$qa14_2, subset_eb_19$qa14_2 == 2, -1)
### aggregate to country-level data
eb_19_by_country <- aggregate(qa14_2 ~ isocntry, data = subset_eb_19, FUN = mean)
eb_19_by_country$year <- as.numeric(2019)
daten_2019 <- merge(daten_2019, eb_19_by_country, by = c("isocntry", "year"), all.x = TRUE)

### (9) edu_years: d8
subset_eb_19$d8 <- replace(subset_eb_19$d8, which(subset_eb_19$d8 >= 35 & subset_eb_19$d8 <= 99), NA)
subset_eb_19$d8 <- replace(subset_eb_19$d8, subset_eb_19$d8 == 0, NA)
subset_eb_19$d8 <- replace(subset_eb_19$d8, subset_eb_19$d8 == 1, 6) # 1 is coded as "no education"
subset_eb_19$d8 <- replace(subset_eb_19$d8, subset_eb_19$d8 == 2, NA)
subset_eb_19$d8 <- replace(subset_eb_19$d8, subset_eb_19$d8 == 3, NA)
subset_eb_19$d8 <- replace(subset_eb_19$d8, subset_eb_19$d8 == 4, NA)
subset_eb_19$d8 <- replace(subset_eb_19$d8, subset_eb_19$d8 == 5, NA)
# assuming that the full time education starts at age 6 in all countries
subset_eb_19$d8 <- subset_eb_19$d8-6 
### aggregate to country-level data
eb_19_by_country <- aggregate(d8 ~ isocntry, data = subset_eb_19, FUN = mean)
eb_19_by_country$year <- as.numeric(2019)
daten_2019 <- merge(daten_2019, eb_19_by_country, by = c("isocntry", "year"), all.x = TRUE)

# ### (10) financial_situation: qa1a_5
# hist(subset_eb_19$qa1a_5)
# subset_eb_19$qa1a_5 <- replace(subset_eb_19$qa1a_5, subset_eb_19$qa1a_5 == 5, NA)
# subset_eb_19$qa1a_5 <- replace(subset_eb_19$qa1a_5, subset_eb_19$qa1a_5 == 4, -3)
# subset_eb_19$qa1a_5 <- replace(subset_eb_19$qa1a_5, subset_eb_19$qa1a_5 == 3, -1)
# subset_eb_19$qa1a_5 <- replace(subset_eb_19$qa1a_5, subset_eb_19$qa1a_5 == 2, 22)
# subset_eb_19$qa1a_5 <- replace(subset_eb_19$qa1a_5, subset_eb_19$qa1a_5 == 1, 3)
# subset_eb_19$qa1a_5 <- replace(subset_eb_19$qa1a_5, subset_eb_19$qa1a_5 == 22, 1)
# ### aggregate to country-level data
# eb_19_by_country <- aggregate(qa1a_5 ~ isocntry, data = subset_eb_19, FUN = mean)
# eb_19_by_country$year <- as.numeric(2019)
# daten_2019 <- merge(daten_2019, eb_19_by_country, by = c("isocntry", "year"), all.x = TRUE)

# add gdp variable
daten_2019 <- merge(daten_2019, gdp_per_capita_PPP[, c("isocntry", "2019")], by = "isocntry", all.x = TRUE)
names(daten_2019)[names(daten_2019) == "2019"] <- "gdp"

# for 2020 ####

# #### (1) trust_pol_parties qa6a_2
# subset_eb_20$qa6a_2 <- replace(subset_eb_20$qa6a_2, subset_eb_20$qa6a_2 == 3, NA)
# subset_eb_20$qa6a_2 <- replace(subset_eb_20$qa6a_2, subset_eb_20$qa6a_2 == 2, -1)
# # aggregate to country-level data
# eb_20_by_country <- aggregate(qa6a_2 ~ isocntry, data = subset_eb_20, FUN = mean)
# eb_20_by_country$year <- as.numeric(2020)
# #merge
# daten_2020 <- merge(daten_2020, eb_20_by_country, by = c("isocntry", "year"), all.x = TRUE)

# #### (2) trust_eu: qa6a_11
# subset_eb_20$qa6a_11 <- replace(subset_eb_20$qa6a_11, subset_eb_20$qa6a_11 == 3, NA)
# subset_eb_20$qa6a_11 <- replace(subset_eb_20$qa6a_11, subset_eb_20$qa6a_11 == 2, -1)
# # aggregate to country-level data
# eb_20_by_country <- aggregate(qa6a_11 ~ isocntry, data = subset_eb_20, FUN = mean)
# eb_20_by_country$year <- as.numeric(2020)
# daten_2020 <- merge(daten_2020, eb_20_by_country, by = c("isocntry", "year"), all.x = TRUE)

# ####(3) image_eu: d78
# subset_eb_20$d78 <- replace(subset_eb_20$d78, subset_eb_20$d78 == 5, -2)
# subset_eb_20$d78 <- replace(subset_eb_20$d78, subset_eb_20$d78 == 4, -1)
# subset_eb_20$d78 <- replace(subset_eb_20$d78, subset_eb_20$d78 == 3, 0)
# subset_eb_20$d78 <- replace(subset_eb_20$d78, subset_eb_20$d78 == 2, 11)
# subset_eb_20$d78 <- replace(subset_eb_20$d78, subset_eb_20$d78 == 1, 2)
# subset_eb_20$d78 <- replace(subset_eb_20$d78, subset_eb_20$d78 == 11, 1)
# subset_eb_20$d78 <- replace(subset_eb_20$d78, subset_eb_20$d78 == 6, NA)
# # aggregate to country-level data
# eb_20_by_country <- aggregate(d78 ~ isocntry, data = subset_eb_20, FUN = mean)
# eb_20_by_country$year <- as.numeric(2020)
# daten_2020 <- merge(daten_2020, eb_20_by_country, by = c("isocntry", "year"), all.x = TRUE)

#### (4) discuss_EU: d71a_2
subset_eb_20$d71a_2 <- replace(subset_eb_20$d71a_2, subset_eb_20$d71a_2 == 4, NA)
subset_eb_20$d71a_2 <- replace(subset_eb_20$d71a_2, subset_eb_20$d71a_2 == 2, 0)
subset_eb_20$d71a_2 <- replace(subset_eb_20$d71a_2, subset_eb_20$d71a_2 == 3, -1)
### aggregate to country-level data
eb_20_by_country <- aggregate(d71a_2 ~ isocntry, data = subset_eb_20, FUN = mean)
eb_20_by_country$year <- as.numeric(2020)
daten_2020 <- merge(daten_2020, eb_20_by_country, by = c("isocntry", "year"), all.x = TRUE)

# #### (5) attach_EU: qc1a_3
# subset_eb_20$qc1a_3 <- replace(subset_eb_20$qc1a_3, subset_eb_20$qc1a_3 == 5, NA)
# subset_eb_20$qc1a_3 <- replace(subset_eb_20$qc1a_3, subset_eb_20$qc1a_3 == 4, -3)
# subset_eb_20$qc1a_3 <- replace(subset_eb_20$qc1a_3, subset_eb_20$qc1a_3 == 3, -1)
# subset_eb_20$qc1a_3 <- replace(subset_eb_20$qc1a_3, subset_eb_20$qc1a_3 == 1, 3)
# subset_eb_20$qc1a_3 <- replace(subset_eb_20$qc1a_3, subset_eb_20$qc1a_3 == 2, 1)
# ### aggregate to country-level data
# eb_20_by_country <- aggregate(qc1a_3 ~ isocntry, data = subset_eb_20, FUN = mean)
# eb_20_by_country$year <- as.numeric(2020)
# daten_2020 <- merge(daten_2020, eb_20_by_country, by = c("isocntry", "year"), all.x = TRUE)

#### (6) citizen_EU: qc2_1
subset_eb_20$qc2_1 <- replace(subset_eb_20$qc2_1, subset_eb_20$qc2_1 == 5, NA)
subset_eb_20$qc2_1 <- replace(subset_eb_20$qc2_1, subset_eb_20$qc2_1 == 4, -3/3)
subset_eb_20$qc2_1 <- replace(subset_eb_20$qc2_1, subset_eb_20$qc2_1 == 3, -1/3)
subset_eb_20$qc2_1 <- replace(subset_eb_20$qc2_1, subset_eb_20$qc2_1 == 1, 3/3)
subset_eb_20$qc2_1 <- replace(subset_eb_20$qc2_1, subset_eb_20$qc2_1 == 2, 1/3)
### aggregate to country-level data
eb_20_by_country <- aggregate(qc2_1 ~ isocntry, data = subset_eb_20, FUN = mean)
eb_20_by_country$year <- as.numeric(2020)
daten_2020 <- merge(daten_2020, eb_20_by_country, by = c("isocntry", "year"), all.x = TRUE)
# 
# ### (7) es_future_outside: qa13_2
# subset_eb_20$qa13_2 <- replace(subset_eb_20$qa13_2, subset_eb_20$qa13_2 == 5, NA)
# subset_eb_20$qa13_2 <- replace(subset_eb_20$qa13_2, subset_eb_20$qa13_2 == 4, -3)
# subset_eb_20$qa13_2 <- replace(subset_eb_20$qa13_2, subset_eb_20$qa13_2 == 3, -1)
# subset_eb_20$qa13_2 <- replace(subset_eb_20$qa13_2, subset_eb_20$qa13_2 == 1, 3)
# subset_eb_20$qa13_2 <- replace(subset_eb_20$qa13_2, subset_eb_20$qa13_2 == 2, 1)
# ### aggregate to country-level data
# eb_20_by_country <- aggregate(qa13_2 ~ isocntry, data = subset_eb_20, FUN = mean)
# eb_20_by_country$year <- as.numeric(2020)
# daten_2020 <- merge(daten_2020, eb_20_by_country, by = c("isocntry", "year"), all.x = TRUE)

### (8)  trust_com: qa12_2
subset_eb_20$qa12_2 <- replace(subset_eb_20$qa12_2, subset_eb_20$qa12_2 == 3, NA)
subset_eb_20$qa12_2 <- replace(subset_eb_20$qa12_2, subset_eb_20$qa12_2 == 2, -1)
### aggregate to country-level data
eb_20_by_country <- aggregate(qa12_2 ~ isocntry, data = subset_eb_20, FUN = mean)
eb_20_by_country$year <- as.numeric(2020)
daten_2020 <- merge(daten_2020, eb_20_by_country, by = c("isocntry", "year"), all.x = TRUE)

### (9) edu_years: d8
subset_eb_20$d8 <- replace(subset_eb_20$d8, which(subset_eb_20$d8 >= 35 & subset_eb_20$d8 <= 99), NA)
subset_eb_20$d8 <- replace(subset_eb_20$d8, subset_eb_20$d8 == 98, NA)
subset_eb_20$d8 <- replace(subset_eb_20$d8, subset_eb_20$d8 == 97, NA)
subset_eb_20$d8 <- replace(subset_eb_20$d8, subset_eb_20$d8 == 0, NA)
subset_eb_20$d8 <- replace(subset_eb_20$d8, subset_eb_20$d8 == 1, 6) # 1 is coded as "no education"
subset_eb_20$d8 <- replace(subset_eb_20$d8, subset_eb_20$d8 == 2, NA)
subset_eb_20$d8 <- replace(subset_eb_20$d8, subset_eb_20$d8 == 3, NA)
subset_eb_20$d8 <- replace(subset_eb_20$d8, subset_eb_20$d8 == 4, NA)
subset_eb_20$d8 <- replace(subset_eb_20$d8, subset_eb_20$d8 == 5, NA)
# assuming that the full time education starts at age 6 in all countries
subset_eb_20$d8 <- subset_eb_20$d8-6 
### aggregate to country-level data
eb_20_by_country <- aggregate(d8 ~ isocntry, data = subset_eb_20, FUN = mean)
eb_20_by_country$year <- as.numeric(2020)
daten_2020 <- merge(daten_2020, eb_20_by_country, by = c("isocntry", "year"), all.x = TRUE)
# 
# ### (10) financial_situation: qa1a_5
# subset_eb_20$qa1a_5 <- replace(subset_eb_20$qa1a_5, subset_eb_20$qa1a_5 == 5, NA)
# subset_eb_20$qa1a_5 <- replace(subset_eb_20$qa1a_5, subset_eb_20$qa1a_5 == 4, -3)
# subset_eb_20$qa1a_5 <- replace(subset_eb_20$qa1a_5, subset_eb_20$qa1a_5 == 3, -1)
# subset_eb_20$qa1a_5 <- replace(subset_eb_20$qa1a_5, subset_eb_20$qa1a_5 == 2, 22)
# subset_eb_20$qa1a_5 <- replace(subset_eb_20$qa1a_5, subset_eb_20$qa1a_5 == 1, 3)
# subset_eb_20$qa1a_5 <- replace(subset_eb_20$qa1a_5, subset_eb_20$qa1a_5 == 22, 1)
# ### aggregate to country-level data
# eb_20_by_country <- aggregate(qa1a_5 ~ isocntry, data = subset_eb_20, FUN = mean)
# eb_20_by_country$year <- as.numeric(2020)
# daten_2020 <- merge(daten_2020, eb_20_by_country, by = c("isocntry", "year"), all.x = TRUE)

# add gdp variable
daten_2020 <- merge(daten_2020, gdp_per_capita_PPP[, c("isocntry", "2020")], by = "isocntry", all.x = TRUE)
names(daten_2020)[names(daten_2020) == "2020"] <- "gdp"


# for 2021 ####

# #### (1) trust_pol_parties qa6a_1
# subset_eb_21$qa6a_1 <- replace(subset_eb_21$qa6a_1, subset_eb_21$qa6a_1 == 3, NA)
# subset_eb_21$qa6a_1 <- replace(subset_eb_21$qa6a_1, subset_eb_21$qa6a_1 == 2, -1)
# # aggregate to country-level data
# eb_21_by_country <- aggregate(qa6a_1 ~ isocntry, data = subset_eb_21, FUN = mean)
# eb_21_by_country$year <- as.numeric(2021)
# #merge
# daten_2021 <- merge(daten_2021, eb_21_by_country, by = c("isocntry", "year"), all.x = TRUE)
# 
# #### (2) trust_eu: qa6a_10
# subset_eb_21$qa6a_10 <- replace(subset_eb_21$qa6a_10, subset_eb_21$qa6a_10 == 3, NA)
# subset_eb_21$qa6a_10 <- replace(subset_eb_21$qa6a_10, subset_eb_21$qa6a_10 == 2, -1)
# # aggregate to country-level data
# eb_21_by_country <- aggregate(qa6a_10 ~ isocntry, data = subset_eb_21, FUN = mean)
# eb_21_by_country$year <- as.numeric(2021)
# daten_2021 <- merge(daten_2021, eb_21_by_country, by = c("isocntry", "year"), all.x = TRUE)

# ####(3) image_eu: d78
# subset_eb_21$d78 <- replace(subset_eb_21$d78, subset_eb_21$d78 == 5, -2)
# subset_eb_21$d78 <- replace(subset_eb_21$d78, subset_eb_21$d78 == 4, -1)
# subset_eb_21$d78 <- replace(subset_eb_21$d78, subset_eb_21$d78 == 3, 0)
# subset_eb_21$d78 <- replace(subset_eb_21$d78, subset_eb_21$d78 == 2, 11)
# subset_eb_21$d78 <- replace(subset_eb_21$d78, subset_eb_21$d78 == 1, 2)
# subset_eb_21$d78 <- replace(subset_eb_21$d78, subset_eb_21$d78 == 11, 1)
# subset_eb_21$d78 <- replace(subset_eb_21$d78, subset_eb_21$d78 == 6, NA)
# # aggregate to country-level data
# eb_21_by_country <- aggregate(d78 ~ isocntry, data = subset_eb_21, FUN = mean)
# eb_21_by_country$year <- as.numeric(2021)
# daten_2021 <- merge(daten_2021, eb_21_by_country, by = c("isocntry", "year"), all.x = TRUE)

#### (4) discuss_EU: d71_2
subset_eb_21$d71_2 <- replace(subset_eb_21$d71_2, subset_eb_21$d71_2 == 4, NA)
subset_eb_21$d71_2 <- replace(subset_eb_21$d71_2, subset_eb_21$d71_2 == 2, 0)
subset_eb_21$d71_2 <- replace(subset_eb_21$d71_2, subset_eb_21$d71_2 == 3, -1)
### aggregate to country-level data
eb_21_by_country <- aggregate(d71_2 ~ isocntry, data = subset_eb_21, FUN = mean)
eb_21_by_country$year <- as.numeric(2021)
daten_2021 <- merge(daten_2021, eb_21_by_country, by = c("isocntry", "year"), all.x = TRUE)
# 
# #### (5) attach_EU: qc1a_3
# subset_eb_21$qc1a_3 <- replace(subset_eb_21$qc1a_3, subset_eb_21$qc1a_3 == 5, NA)
# subset_eb_21$qc1a_3 <- replace(subset_eb_21$qc1a_3, subset_eb_21$qc1a_3 == 4, -3)
# subset_eb_21$qc1a_3 <- replace(subset_eb_21$qc1a_3, subset_eb_21$qc1a_3 == 3, -1)
# subset_eb_21$qc1a_3 <- replace(subset_eb_21$qc1a_3, subset_eb_21$qc1a_3 == 1, 3)
# subset_eb_21$qc1a_3 <- replace(subset_eb_21$qc1a_3, subset_eb_21$qc1a_3 == 2, 1)
# ### aggregate to country-level data
# eb_21_by_country <- aggregate(qc1a_3 ~ isocntry, data = subset_eb_21, FUN = mean)
# eb_21_by_country$year <- as.numeric(2021)
# daten_2021 <- merge(daten_2021, eb_21_by_country, by = c("isocntry", "year"), all.x = TRUE)

#### (6) citizen_EU: qc2_1
subset_eb_21$qc2_1 <- replace(subset_eb_21$qc2_1, subset_eb_21$qc2_1 == 5, NA)
subset_eb_21$qc2_1 <- replace(subset_eb_21$qc2_1, subset_eb_21$qc2_1 == 4, -3/3)
subset_eb_21$qc2_1 <- replace(subset_eb_21$qc2_1, subset_eb_21$qc2_1 == 3, -1/3)
subset_eb_21$qc2_1 <- replace(subset_eb_21$qc2_1, subset_eb_21$qc2_1 == 1, 3/3)
subset_eb_21$qc2_1 <- replace(subset_eb_21$qc2_1, subset_eb_21$qc2_1 == 2, 1/3)
### aggregate to country-level data
eb_21_by_country <- aggregate(qc2_1 ~ isocntry, data = subset_eb_21, FUN = mean)
eb_21_by_country$year <- as.numeric(2021)
daten_2021 <- merge(daten_2021, eb_21_by_country, by = c("isocntry", "year"), all.x = TRUE)

# ### (7) es_future_outside: qa11_2
# subset_eb_21$qa11_2 <- replace(subset_eb_21$qa11_2, subset_eb_21$qa11_2 == 5, NA)
# subset_eb_21$qa11_2 <- replace(subset_eb_21$qa11_2, subset_eb_21$qa11_2 == 4, -3)
# subset_eb_21$qa11_2 <- replace(subset_eb_21$qa11_2, subset_eb_21$qa11_2 == 3, -1)
# subset_eb_21$qa11_2 <- replace(subset_eb_21$qa11_2, subset_eb_21$qa11_2 == 1, 3)
# subset_eb_21$qa11_2 <- replace(subset_eb_21$qa11_2, subset_eb_21$qa11_2 == 2, 1)
# ### aggregate to country-level data
# eb_21_by_country <- aggregate(qa11_2 ~ isocntry, data = subset_eb_21, FUN = mean)
# eb_21_by_country$year <- as.numeric(2021)
# daten_2021 <- merge(daten_2021, eb_21_by_country, by = c("isocntry", "year"), all.x = TRUE)

### (8)  trust_com: qa10_2
subset_eb_21$qa10_2 <- replace(subset_eb_21$qa10_2, subset_eb_21$qa10_2 == 3, NA)
subset_eb_21$qa10_2 <- replace(subset_eb_21$qa10_2, subset_eb_21$qa10_2 == 2, -1)
### aggregate to country-level data
eb_21_by_country <- aggregate(qa10_2 ~ isocntry, data = subset_eb_21, FUN = mean)
eb_21_by_country$year <- as.numeric(2021)
daten_2021 <- merge(daten_2021, eb_21_by_country, by = c("isocntry", "year"), all.x = TRUE)

### (9) edu_years: d8
subset_eb_21$d8 <- replace(subset_eb_21$d8, which(subset_eb_21$d8 >= 65 & subset_eb_21$d8 <= 99), NA)
subset_eb_21$d8 <- replace(subset_eb_21$d8, subset_eb_21$d8 == 98, NA)
subset_eb_21$d8 <- replace(subset_eb_21$d8, subset_eb_21$d8 == 97, NA)
subset_eb_21$d8 <- replace(subset_eb_21$d8, subset_eb_21$d8 == 0, NA)
subset_eb_21$d8 <- replace(subset_eb_21$d8, subset_eb_21$d8 == 1, 6) # 1 is coded as "no education"
subset_eb_21$d8 <- replace(subset_eb_21$d8, subset_eb_21$d8 == 2, NA)
subset_eb_21$d8 <- replace(subset_eb_21$d8, subset_eb_21$d8 == 3, NA)
subset_eb_21$d8 <- replace(subset_eb_21$d8, subset_eb_21$d8 == 4, NA)
subset_eb_21$d8 <- replace(subset_eb_21$d8, subset_eb_21$d8 == 5, NA)
# assuming that the full time education starts at age 6 in all countries
subset_eb_21$d8 <- subset_eb_21$d8-6 
### aggregate to country-level data
eb_21_by_country <- aggregate(d8 ~ isocntry, data = subset_eb_21, FUN = mean)
eb_21_by_country$year <- as.numeric(2021)
daten_2021 <- merge(daten_2021, eb_21_by_country, by = c("isocntry", "year"), all.x = TRUE)

# ### (10) financial_situation: qa1a_5
# subset_eb_21$qa1a_5 <- replace(subset_eb_21$qa1a_5, subset_eb_21$qa1a_5 == 5, NA)
# subset_eb_21$qa1a_5 <- replace(subset_eb_21$qa1a_5, subset_eb_21$qa1a_5 == 4, -3)
# subset_eb_21$qa1a_5 <- replace(subset_eb_21$qa1a_5, subset_eb_21$qa1a_5 == 3, -1)
# subset_eb_21$qa1a_5 <- replace(subset_eb_21$qa1a_5, subset_eb_21$qa1a_5 == 2, 22)
# subset_eb_21$qa1a_5 <- replace(subset_eb_21$qa1a_5, subset_eb_21$qa1a_5 == 1, 3)
# subset_eb_21$qa1a_5 <- replace(subset_eb_21$qa1a_5, subset_eb_21$qa1a_5 == 22, 1)
# ### aggregate to country-level data
# eb_21_by_country <- aggregate(qa1a_5 ~ isocntry, data = subset_eb_21, FUN = mean)
# eb_21_by_country$year <- as.numeric(2021)
# daten_2021 <- merge(daten_2021, eb_21_by_country, by = c("isocntry", "year"), all.x = TRUE)

# add gdp variable
daten_2021 <- merge(daten_2021, gdp_per_capita_PPP[, c("isocntry", "2021")], by = "isocntry", all.x = TRUE)
names(daten_2021)[names(daten_2021) == "2021"] <- "gdp"

# for 2022 ####
# 
# #### (1) trust_pol_parties qa6a_1
# subset_eb_22$qa6a_1 <- replace(subset_eb_22$qa6a_1, subset_eb_22$qa6a_1 == 3, NA)
# subset_eb_22$qa6a_1 <- replace(subset_eb_22$qa6a_1, subset_eb_22$qa6a_1 == 2, -1)
# # aggregate to country-level data
# eb_22_by_country <- aggregate(qa6a_1 ~ isocntry, data = subset_eb_22, FUN = mean)
# eb_22_by_country$year <- as.numeric(2022)
# #merge
# daten_2022 <- merge(daten_2022, eb_22_by_country, by = c("isocntry", "year"), all.x = TRUE)
# 
# 
# #### (2) trust_eu: qa6a_10
# subset_eb_22$qa6a_10 <- replace(subset_eb_22$qa6a_10, subset_eb_22$qa6a_10 == 3, NA)
# subset_eb_22$qa6a_10 <- replace(subset_eb_22$qa6a_10, subset_eb_22$qa6a_10 == 2, -1)
# # aggregate to country-level data
# eb_22_by_country <- aggregate(qa6a_10 ~ isocntry, data = subset_eb_22, FUN = mean)
# eb_22_by_country$year <- as.numeric(2022)
# daten_2022 <- merge(daten_2022, eb_22_by_country, by = c("isocntry", "year"), all.x = TRUE)
# 
# ####(3) image_eu: d78
# subset_eb_22$d78 <- replace(subset_eb_22$d78, subset_eb_22$d78 == 5, -2)
# subset_eb_22$d78 <- replace(subset_eb_22$d78, subset_eb_22$d78 == 4, -1)
# subset_eb_22$d78 <- replace(subset_eb_22$d78, subset_eb_22$d78 == 3, 0)
# subset_eb_22$d78 <- replace(subset_eb_22$d78, subset_eb_22$d78 == 2, 11)
# subset_eb_22$d78 <- replace(subset_eb_22$d78, subset_eb_22$d78 == 1, 2)
# subset_eb_22$d78 <- replace(subset_eb_22$d78, subset_eb_22$d78 == 11, 1)
# subset_eb_22$d78 <- replace(subset_eb_22$d78, subset_eb_22$d78 == 6, NA)
# # aggregate to country-level data
# eb_22_by_country <- aggregate(d78 ~ isocntry, data = subset_eb_22, FUN = mean)
# eb_22_by_country$year <- as.numeric(2022)
# daten_2022 <- merge(daten_2022, eb_22_by_country, by = c("isocntry", "year"), all.x = TRUE)

#### (4) discuss_EU: d71_2
subset_eb_22$d71_2 <- replace(subset_eb_22$d71_2, subset_eb_22$d71_2 == 4, NA)
subset_eb_22$d71_2 <- replace(subset_eb_22$d71_2, subset_eb_22$d71_2 == 2, 0)
subset_eb_22$d71_2 <- replace(subset_eb_22$d71_2, subset_eb_22$d71_2 == 3, -1)
### aggregate to country-level data
eb_22_by_country <- aggregate(d71_2 ~ isocntry, data = subset_eb_22, FUN = mean)
eb_22_by_country$year <- as.numeric(2022)
daten_2022 <- merge(daten_2022, eb_22_by_country, by = c("isocntry", "year"), all.x = TRUE)
# 
# #### (5) attach_EU: qd1a_3
# subset_eb_22$qd1a_3 <- replace(subset_eb_22$qd1a_3, subset_eb_22$qd1a_3 == 5, NA)
# subset_eb_22$qd1a_3 <- replace(subset_eb_22$qd1a_3, subset_eb_22$qd1a_3 == 4, -3)
# subset_eb_22$qd1a_3 <- replace(subset_eb_22$qd1a_3, subset_eb_22$qd1a_3 == 3, -1)
# subset_eb_22$qd1a_3 <- replace(subset_eb_22$qd1a_3, subset_eb_22$qd1a_3 == 1, 3)
# subset_eb_22$qd1a_3 <- replace(subset_eb_22$qd1a_3, subset_eb_22$qd1a_3 == 2, 1)
# ### aggregate to country-level data
# eb_22_by_country <- aggregate(qd1a_3 ~ isocntry, data = subset_eb_22, FUN = mean)
# eb_22_by_country$year <- as.numeric(2022)
# daten_2022 <- merge(daten_2022, eb_22_by_country, by = c("isocntry", "year"), all.x = TRUE)

#### (6) citizen_EU: qd2_1
subset_eb_22$qd2_1 <- replace(subset_eb_22$qd2_1, subset_eb_22$qd2_1 == 5, NA)
subset_eb_22$qd2_1 <- replace(subset_eb_22$qd2_1, subset_eb_22$qd2_1 == 4, -3/3)
subset_eb_22$qd2_1 <- replace(subset_eb_22$qd2_1, subset_eb_22$qd2_1 == 3, -1/3)
subset_eb_22$qd2_1 <- replace(subset_eb_22$qd2_1, subset_eb_22$qd2_1 == 1, 3/3)
subset_eb_22$qd2_1 <- replace(subset_eb_22$qd2_1, subset_eb_22$qd2_1 == 2, 1/3)
### aggregate to country-level data
eb_22_by_country <- aggregate(qd2_1 ~ isocntry, data = subset_eb_22, FUN = mean)
eb_22_by_country$year <- as.numeric(2022)
daten_2022 <- merge(daten_2022, eb_22_by_country, by = c("isocntry", "year"), all.x = TRUE)
# 
# ### (7) es_future_outside: qa11_2
# subset_eb_22$qa11_2 <- replace(subset_eb_22$qa11_2, subset_eb_22$qa11_2 == 5, NA)
# subset_eb_22$qa11_2 <- replace(subset_eb_22$qa11_2, subset_eb_22$qa11_2 == 4, -3)
# subset_eb_22$qa11_2 <- replace(subset_eb_22$qa11_2, subset_eb_22$qa11_2 == 3, -1)
# subset_eb_22$qa11_2 <- replace(subset_eb_22$qa11_2, subset_eb_22$qa11_2 == 1, 3)
# subset_eb_22$qa11_2 <- replace(subset_eb_22$qa11_2, subset_eb_22$qa11_2 == 2, 1)
# ### aggregate to country-level data
# eb_22_by_country <- aggregate(qa11_2 ~ isocntry, data = subset_eb_22, FUN = mean)
# eb_22_by_country$year <- as.numeric(2022)
# daten_2022 <- merge(daten_2022, eb_22_by_country, by = c("isocntry", "year"), all.x = TRUE)

### (8)  trust_com: qa10_2
subset_eb_22$qa10_2 <- replace(subset_eb_22$qa10_2, subset_eb_22$qa10_2 == 3, NA)
subset_eb_22$qa10_2 <- replace(subset_eb_22$qa10_2, subset_eb_22$qa10_2 == 2, -1)
### aggregate to country-level data
eb_22_by_country <- aggregate(qa10_2 ~ isocntry, data = subset_eb_22, FUN = mean)
eb_22_by_country$year <- as.numeric(2022)
daten_2022 <- merge(daten_2022, eb_22_by_country, by = c("isocntry", "year"), all.x = TRUE)

### (9) edu_years: d8
subset_eb_22$d8 <- replace(subset_eb_22$d8, which(subset_eb_22$d8 >= 65 & subset_eb_22$d8 <= 99), NA)
subset_eb_22$d8 <- replace(subset_eb_22$d8, subset_eb_22$d8 == 98, NA)
subset_eb_22$d8 <- replace(subset_eb_22$d8, subset_eb_22$d8 == 97, NA)
subset_eb_22$d8 <- replace(subset_eb_22$d8, subset_eb_22$d8 == 0, NA)
subset_eb_22$d8 <- replace(subset_eb_22$d8, subset_eb_22$d8 == 1, 6) # 1 is coded as "no education"
subset_eb_22$d8 <- replace(subset_eb_22$d8, subset_eb_22$d8 == 2, NA)
subset_eb_22$d8 <- replace(subset_eb_22$d8, subset_eb_22$d8 == 3, NA)
subset_eb_22$d8 <- replace(subset_eb_22$d8, subset_eb_22$d8 == 4, NA)
subset_eb_22$d8 <- replace(subset_eb_22$d8, subset_eb_22$d8 == 5, NA)
# assuming that the full time education starts at age 6 in all countries
subset_eb_22$d8 <- subset_eb_22$d8-6 
### aggregate to country-level data
eb_22_by_country <- aggregate(d8 ~ isocntry, data = subset_eb_22, FUN = mean)
eb_22_by_country$year <- as.numeric(2022)
daten_2022 <- merge(daten_2022, eb_22_by_country, by = c("isocntry", "year"), all.x = TRUE)
# 
# ### (10) financial_situation: qa1_5
# subset_eb_22$qa1_5 <- replace(subset_eb_22$qa1_5, subset_eb_22$qa1_5 == 5, NA)
# subset_eb_22$qa1_5 <- replace(subset_eb_22$qa1_5, subset_eb_22$qa1_5 == 4, -3)
# subset_eb_22$qa1_5 <- replace(subset_eb_22$qa1_5, subset_eb_22$qa1_5 == 3, -1)
# subset_eb_22$qa1_5 <- replace(subset_eb_22$qa1_5, subset_eb_22$qa1_5 == 2, 22)
# subset_eb_22$qa1_5 <- replace(subset_eb_22$qa1_5, subset_eb_22$qa1_5 == 1, 3)
# subset_eb_22$qa1_5 <- replace(subset_eb_22$qa1_5, subset_eb_22$qa1_5 == 22, 1)
# ### aggregate to country-level data
# eb_22_by_country <- aggregate(qa1_5 ~ isocntry, data = subset_eb_22, FUN = mean)
# eb_22_by_country$year <- as.numeric(2022)
# daten_2022 <- merge(daten_2022, eb_22_by_country, by = c("isocntry", "year"), all.x = TRUE)

# add gdp variable
daten_2022 <- merge(daten_2022, gdp_per_capita_PPP[, c("isocntry", "2022")], by = "isocntry", all.x = TRUE)
names(daten_2022)[names(daten_2022) == "2022"] <- "gdp"

# reassign column headers and merge eb daten dfs ####
daten_2019 <- daten_2019 %>% rename(
  # trust_pol_part= qa6a_2,
  # trust_EU = qa6a_10,
  # image_EU = qa7,
  discuss_EU = d71a_2,
  # attach_EU = qd1a_3,
  citizen_feel= qd2_1,
  # es_future_outside = qa18a_5,
  trust_com = qa14_2,
  edu_years = d8,
  # fin_situation = qa1a_5
)

daten_2020 <- daten_2020 %>% rename(
  # trust_pol_part= qa6a_2,
  # trust_EU = qa6a_11,
  # image_EU = d78,
  discuss_EU = d71a_2,
  # attach_EU = qc1a_3,
  citizen_feel= qc2_1,
  # es_future_outside = qa13_2,
  trust_com = qa12_2,
  edu_years = d8,
  # fin_situation = qa1a_5
)

daten_2021 <- daten_2021 %>% rename(
  # trust_pol_part= qa6a_1,
  # trust_EU = qa6a_10,
  # image_EU = d78,
  discuss_EU = d71_2,
  # attach_EU = qc1a_3,
  citizen_feel= qc2_1,
  # es_future_outside = qa11_2,
  trust_com = qa10_2,
  edu_years = d8,
  # fin_situation = qa1a_5
)

daten_2022 <- daten_2022%>% rename(
  # trust_pol_part= qa6a_1,
  # trust_EU = qa6a_10,
  # image_EU = d78,
  discuss_EU = d71_2,
  # attach_EU = qd1a_3,
  citizen_feel= qd2_1,
  # es_future_outside = qa11_2,
  trust_com = qa10_2,
  edu_years = d8,
  # fin_situation = qa1_5
)

daten <- rbind(daten_2019, daten_2020, daten_2021, daten_2022)
daten$year <- as.numeric(daten$year)
daten$edu <- as.numeric(daten$edu)
daten$category <- as.character(daten$category)
daten$edu_years <- as.numeric(daten$edu_years)
# daten$fin_situation <- as.numeric(daten$fin_situation)

# Remove commas from numbers formatted as "characters" of gdp data
daten$gdp <- gsub(",", "", daten$gdp)
daten$gdp <- as.numeric(daten$gdp)
daten$gdp <- daten$gdp/10000

daten <- daten%>% rename(
  participation = y_1,
  log_participation = y_log,
  participation_2 = y_2,
  log_participation_2 = y_log_2)

# add dummy variable for compulsory voting
daten$compulsory_voting <- ifelse(daten$isocntry %in% c("BE", "GR", "LU"), 1, 0)

# subsetting: deleting all observations with no reponse of a country for a legislation
daten_cleaned <- daten[daten$participation_2 != 0, ]

#subset with gdp data as full values
daten_plots <- daten
daten_plots$gdp <- daten_plots$gdp*10000


# ####

##########  Analysis ####


# descriptive statistics ####

#histograms of all dependent variables
par(mfrow = c(2, 3))

hist(unique(daten$trust_com), xlab = "Trust in the EU Commission", main = "I", xlim = c(-1, 1))
hist(unique(daten$discuss_EU), xlab = "Frequency of Discussing EU Politics", main = "II", xlim = c(-1, 1))
hist(unique(daten$citizen_feel), xlab = "Sense of Citizenship", main = "III", xlim = c(-1, 1))
hist(unique(daten$edu_years), xlab = "Years of Formal Education", main = "IV")
hist(unique(daten$gdp), xlab = "GDP per capita (*10.000, constant, PPP)", main = "V")


# bp discuss politics

daten %>%
  dplyr::group_by(isocntry) %>%
  mutate(Mean_label = sprintf("%.2f", round(mean(discuss_EU), 2)),
         SD_label = sprintf("%.2f", round(sd(discuss_EU), 2)),
         Mean = round(mean(discuss_EU), 2),
         SD = round(sd(discuss_EU), 2)) %>%
  ggplot(aes(reorder(isocntry, -Mean, FUN = mean), discuss_EU)) +
  geom_boxplot(coef = Inf) +
  geom_text(aes(label = paste("M = ", Mean_label, sep = ""), y = Mean), y = -2.1, hjust = 0.5, size = 4, angle = 90) +
  geom_text(aes(label = paste("SD = ", SD_label, sep = ""), y = Mean), y = -1.4, hjust = 0.5, size = 4, angle = 90) +
  theme_classic(base_size = 18) +
  theme(panel.grid.major.y = element_line(colour = "grey"),
        panel.grid.minor.y = element_blank()) +
  scale_y_continuous(breaks = c(-1, -0.5, 0, 0.5, 1)) +
  labs(x = "Country") +
  labs(y = "Frequency of Discussing Politics", cex = .5) +
  coord_cartesian(ylim = c(-2.4, 1)) + 
  guides(fill = "yes")

# bp trust commission
daten %>%
  dplyr::group_by(isocntry) %>%
  mutate(Mean_label = sprintf("%.2f", round(mean(trust_com), 2)),
         SD_label = sprintf("%.2f", round(sd(trust_com), 2)),
         Mean = round(mean(trust_com), 2),
         SD = round(sd(trust_com), 2)) %>%
  ggplot(aes(reorder(isocntry, -Mean, FUN = mean), trust_com)) +
  geom_boxplot(coef = Inf) +
  geom_text(aes(label = paste("M = ", Mean_label, sep = ""), y = Mean), y = -2.1, hjust = 0.5, size = 4, angle = 90) +
  geom_text(aes(label = paste("SD = ", SD_label, sep = ""), y = Mean), y = -1.4, hjust = 0.5, size = 4, angle = 90) +
  theme_classic(base_size = 18) +
  theme(panel.grid.major.y = element_line(colour = "grey"),
        panel.grid.minor.y = element_blank()) +
  scale_y_continuous(breaks = c(-1, -0.5, 0, 0.5, 1)) +
  labs(x = "Country") +
  labs(y = "Trust in the Commission", cex = .5) +
  coord_cartesian(ylim = c(-2.4, 1)) + 
  guides(fill = "yes")

# bp sense of citizenship
daten %>%
  dplyr::group_by(isocntry) %>%
  mutate(Mean_label = sprintf("%.2f", round(mean(citizen_feel), 2)),
         SD_label = sprintf("%.2f", round(sd(citizen_feel), 2)),
         Mean = round(mean(citizen_feel), 2),
         SD = round(sd(citizen_feel), 2)) %>%
  ggplot(aes(reorder(isocntry, -Mean, FUN = mean), citizen_feel)) +
  geom_boxplot(coef = Inf) +
  geom_text(aes(label = paste("M = ", Mean_label, sep = ""), y = Mean), y = -2.1, hjust = 0.5, size = 4, angle = 90) +
  geom_text(aes(label = paste("SD = ", SD_label, sep = ""), y = Mean), y = -1.4, hjust = 0.5, size = 4, angle = 90) +
  theme_classic(base_size = 18) +
  theme(panel.grid.major.y = element_line(colour = "grey"),
        panel.grid.minor.y = element_blank()) +
  scale_y_continuous(breaks = c(-1, -0.5, 0, 0.5, 1)) +
  labs(x = "Country") +
  labs(y = "Sense of Citizenship", cex = .5) +
  coord_cartesian(ylim = c(-2.4, 1)) + 
  guides(fill = "yes")

# education
daten %>%
  dplyr::group_by(isocntry) %>%
  mutate(Mean_label = sprintf("%.2f", round(mean(edu_years), 2)),
         SD_label = sprintf("%.2f", round(sd(edu_years), 2)),
         Mean = round(mean(edu_years), 2),
         SD = round(sd(edu_years), 2)) %>%
  ggplot(aes(reorder(isocntry, -Mean, FUN = mean), edu_years)) +
  geom_boxplot(coef = Inf) +
  geom_text(aes(label = paste("M = ", Mean_label, sep = ""), y = Mean), y = -12, hjust = 0.5, size = 4, angle = 90) +
  geom_text(aes(label = paste("SD = ", SD_label, sep = ""), y = Mean), y = -4, hjust = 0.5, size = 4, angle = 90) +
  theme_classic(base_size = 18) +
  theme(panel.grid.major.y = element_line(colour = "grey"),
        panel.grid.minor.y = element_blank()) +
  scale_y_continuous(breaks = c(0, 5, 10, 15, 20, 25)) +
  labs(x = "Country") +
  labs(y = "Years of Formal Education", cex = .5) +
  coord_cartesian(ylim = c(-15, 25)) + 
  guides(fill = "yes")

# bp gdp per capita
daten_plots %>%
  dplyr::group_by(isocntry) %>%
  mutate(Mean = round(mean(gdp), 0),
         SD = round(sd(gdp), 0)) %>%
  ggplot(aes(reorder(isocntry, -Mean, FUN = mean), gdp)) +
  geom_boxplot(coef = Inf) +
  geom_text(aes(label = paste("M = ", Mean, sep = ""), y = Mean), y = -85000, hjust = 0.5, size = 4, angle = 90) +
  geom_text(aes(label = paste("SD = ", SD, sep = ""), y = Mean), y = -32000, hjust = 0.5, size = 4, angle = 90) +
  theme_classic(base_size = 14) +
  theme(panel.grid.major.y = element_line(colour = "grey"),
        panel.grid.minor.y = element_blank()) +
  scale_y_continuous(breaks = c(0, 25000, 50000, 75000, 100000, 125000)) +
  labs(x = "Country") +
  labs(y = "GDP per Capita (2017 US$ constant prices, PPP)", cex = .1) +
  coord_cartesian(ylim = c(-100000, 125000)) + 
  guides(fill = "yes")

summary(daten$participation)

#bp of ratio variable
daten %>%
  dplyr::group_by(isocntry) %>%
  mutate(Mean_label = sprintf("%.2f", round(mean(participation), 2)),
         SD_label = sprintf("%.2f", round(sd(participation), 2)),
         Mean = round(mean(participation), 2),
         SD = round(sd(participation), 2)) %>%
  ggplot(aes(reorder(isocntry, -Mean, FUN = mean), participation)) +
  geom_boxplot(coef = Inf) +
  geom_text(aes(label = paste("M = ", Mean_label, sep = ""), y = Mean), y = -45, hjust = 0.5, size = 4, angle = 90) +
  geom_text(aes(label = paste("SD = ", SD_label, sep = ""), y = Mean), y = -15, hjust = 0.5, size = 4, angle = 90) +
  theme_classic(base_size = 18) +
  theme(panel.grid.major.y = element_line(colour = "grey"),
        panel.grid.minor.y = element_blank()) +
  scale_y_continuous(breaks = c(0, 10, 20, 30, 40, 50, 60, 70, 80)) +
  labs(x = "Country") +
  labs(y = "Ratio Observed/Expected Participation", cex = .5) +
  coord_cartesian(ylim = c(-50, 85)) + 
  guides(fill = "yes")
summary(daten_cleaned$log_participation)

#bp of log-ratio variable (no dropped observations, with add-on smoothing)
daten %>%
  dplyr::group_by(isocntry) %>%
  mutate(Mean_label = sprintf("%.2f", round(mean(log_participation), 2)),
         SD_label = sprintf("%.2f", round(sd(log_participation), 2)),
         Mean = round(mean(log_participation), 2),
         SD = round(sd(log_participation), 2)) %>%
  ggplot(aes(reorder(isocntry, -Mean, FUN = mean), log_participation)) +
  geom_boxplot(coef = Inf) +
  geom_text(aes(label = paste("M = ", Mean_label, sep = ""), y = Mean), y = -14.5, hjust = 0.5, size = 4, angle = 90) +
  geom_text(aes(label = paste("SD = ", SD_label, sep = ""), y = Mean), y = -11, hjust = 0.5, size = 4, angle = 90) +
  theme_classic(base_size = 18) +
  theme(panel.grid.major.y = element_line(colour = "grey"),
        panel.grid.minor.y = element_blank()) +
  scale_y_continuous(breaks = c(-7.5, -5, -2.5, 0, 2.5, 5)) +
  labs(x = "Country") +
  labs(y = "Log-Ratio Observed/Expected Participation", cex = .5) +
  coord_cartesian(ylim = c(-15, 5)) + 
  guides(fill = "yes")

hist(daten$log_participation, xlab = "Log-Ratio Observed/Expected Participation (no dropped observations, Laplace smoothing)", main = "Histogram of Log Participation")

#bp of log-ratio variable (dropped observations, no add-on smoothing)
daten_cleaned %>%
  dplyr::group_by(isocntry) %>%
  mutate(Mean_label = sprintf("%.2f", round(mean(log_participation_2), 2)),
         SD_label = sprintf("%.2f", round(sd(log_participation_2), 2)),
         Mean = round(mean(log_participation_2), 2),
         SD = round(sd(log_participation_2), 2)) %>%
  ggplot(aes(reorder(isocntry, -Mean, FUN = mean), log_participation_2)) +
  geom_boxplot(coef = Inf) +
  geom_text(aes(label = paste("M = ", Mean_label, sep = ""), y = Mean), y = -10.3, hjust = 0.5, size = 4, angle = 90) +
  geom_text(aes(label = paste("SD = ", SD_label, sep = ""), y = Mean), y = -7, hjust = 0.5, size = 4, angle = 90) +
  theme_classic(base_size = 18) +
  theme(panel.grid.major.y = element_line(colour = "grey"),
        panel.grid.minor.y = element_blank()) +
  scale_y_continuous(breaks = c(-5, -2.5, 0, 2.5, 5)) +
  labs(x = "Country") +
  labs(y = "Log-Ratio Observed/Expected Participation", cex = .5) +
  coord_cartesian(ylim = c(-11, 5)) + 
  guides(fill = "yes")

# histograms of DV
par(mfrow = c(1, 2))

number_of_observations <- length(daten$log_participation[!is.na(daten$log_participation)])

hist(daten$log_participation, 
     xlab = "log-ratio of participation share", 
     main = bquote("DV with Laplace smoothing," ~ "\n" ~ "no dropped observations ( N =" ~ .(number_of_observations) ~ ")"))

number_of_observations <- length(daten$log_participation_2[!is.na(daten$log_participation_2)])

hist(daten$log_participation_2, 
     xlab = "log-ratio of participation share", 
     main = bquote("DV with dropped observations ( N =" ~ .(number_of_observations) ~ ")"))



######   regression models  ######################


# check regression model assumptions

# Estimate the baseline plm model dropped observation DV

model <- plm(log_participation_2 ~ discuss_EU + trust_com + citizen_feel + edu_years + gdp,
             data = daten, index="year", model = "within")
summary(model)

# 1. Representativess (in text, no test)


# 2. Linearity: Scatter plot matrix

# Set the graphical parameters for a 3x2 plot matrix
par(mfrow = c(2, 3))


# First plot
plot(daten$discuss_EU, daten$log_participation_2,
     main = "I", xlab = "Frequency of Discussing EU Politics",
     ylab = "participation (log-ratio)", ylim = c(-7.5, 5), xlim = c(-1, 1),
     cex = 1,              # size of points
     cex.lab = 1.4,          # size of x and y labels
     cex.main = 1.4,         # size of title
     cex.axis = 1.4)         # size of axis tick labels
# Add the regression line to the plot
lm_model <- lm(log_participation_2 ~ discuss_EU, data = daten)
abline(lm_model, col = "red")

# Second plot
plot(daten$trust_com, daten$log_participation_2,
     main = "II", xlab = "Trust in the Commission",
     ylab = "participation (log-ratio)", ylim = c(-7.5, 5), xlim = c(-1,1),
     cex = 1,              # size of points
     cex.lab = 1.4,          # size of x and y labels
     cex.main = 1.4,         # size of title
     cex.axis = 1.4)         # size of axis tick labels
# Add the regression line to the plot
lm_model <- lm(log_participation_2 ~ trust_com, data = daten)
abline(lm_model, col = "red")

# Third plot
plot(daten$citizen_feel, daten$log_participation_2,
     main = "III", xlab = "Sense of Citizenship",
     ylab = "participation (log-ratio)", ylim = c(-7.5, 5), xlim = c(-1,1),
     cex = 1,              # size of points
     cex.lab = 1.4,          # size of x and y labels
     cex.main = 1.4,         # size of title
     cex.axis = 1.4)         # size of axis tick labels
# Add the regression line to the plot
lm_model <- lm(log_participation_2 ~ citizen_feel, data = daten)
abline(lm_model, col = "red")


# Fourth plot
plot(daten$edu_years, daten$log_participation_2,
     main = "IV", xlab = "Years of Formal Education",
     ylab = "participation (log-ratio)", ylim = c(-7.5, 5),
     cex = 1,              # size of points
     cex.lab = 1.4,          # size of x and y labels
     cex.main = 1.4,         # size of title
     cex.axis = 1.4)         # size of axis tick labels
# Add the regression line to the plot
lm_model <- lm(log_participation_2 ~ edu_years, data = daten)
abline(lm_model, col = "red")

# Fifth plot
plot(daten_plots$gdp, daten_plots$log_participation_2,
     main = "V", xlab = "GDP per Capita (constant, PPP)",
     ylab = "participation (log-ratio)", ylim = c(-7.5, 5),
     cex = 1,              # size of points
     cex.lab = 1.4,          # size of x and y labels
     cex.main = 1.4,         # size of title
     cex.axis = 1.4)         # size of axis tick labels
# Add the regression line to the plot
lm_model <- lm(log_participation_2 ~ gdp, data = daten_plots)
abline(lm_model, col = "red")



# 3. Multicollinearity: correlation matrix to find possible collinearity of IVs ####

IV <- data.frame(
  discuss_EU = daten$discuss_EU,
  citizen_feel = daten$citizen_feel,
  trust_com = daten$trust_com,
  edu_years = daten$edu_years,
  gdp = daten$gdp
)

# Assign the original column names to the new data frame
names(IV) <- c( "discuss_EU", "citizen_feel", "trust_com", "edu_years", "gdp_per_capita")
cor_matrix <- cor(IV)

cor_matrix

# Create correlation plot
corrplot(cor_matrix, method = "color", type = "upper", tl.col = "black")




# 4. Endogeneity and choice between Random Effects or Fixed Effects: Hausman test
model_fe <- plm(log_participation_2 ~ discuss_EU + trust_com + citizen_feel + edu_years + gdp,
                data = daten, model = "within")
model_re <- plm(log_participation_2 ~ discuss_EU + trust_com + citizen_feel + edu_years + gdp,
                data = daten, model = "random")
hausman_test <- phtest(model_fe, model_re)
print(hausman_test)



# 5. Homoscedasticity: Residuals vs Fitted values plot
par(mfrow = c(1, 2))


# standadrized residuals
residuals <- residuals(model)
fitted <- fitted.values(model)
standardized_residuals <- residuals / sd(residuals)
# Plot the standardized residuals against the fitted values
plot(as.character(fitted), as.character(standardized_residuals), 
     xlab = "Fitted Values", ylab = "Standardized Residuals", 
     cex = 0.3, ylim = c(-4, 4))
# Adding a smoothed line to the plot
smoothed_line <- lowess(fitted, standardized_residuals)
lines(smoothed_line, col = "red")


## backtransformation of log residuals
# Calculate residuals and fitted values on the log scale
residuals_log <- residuals(model)
fitted_log <- fitted.values(model)
# Duan's Smearing Factor
n <- length(residuals_log)
smearing_factor <- exp(sum(residuals_log^2)/(n-1))^0.5
# Back-transforming the fitted values
fitted_original_scale <- exp(fitted_log)
# Back-transforming the residuals
residuals_original_scale <- residuals_log * smearing_factor
# Plot the back-transformed residuals against the back-transformed fitted values
plot(as.character(fitted_original_scale), as.character(residuals_original_scale), xlab = "Fitted Values (Original Scale)", ylab = "Residuals (Original Scale)", cex=0.3)
# Adding a smoothed line to the plot
smoothed_line <- lowess(fitted_original_scale, residuals_original_scale)
lines(smoothed_line, col = "red")




# RESULTS ####

# Comparison of models
fixed <- plm(log_participation_2 ~ trust_com + discuss_EU + citizen_feel + edu_years + gdp, index=c("isocntry","year"), model="within", data = daten)
ols <-lm(log_participation_2 ~ trust_com + discuss_EU + citizen_feel + edu_years + gdp, data=daten)
pooled <-plm(log_participation_2 ~ trust_com + discuss_EU + citizen_feel + edu_years + gdp, index=c("isocntry","year"), model="pooling", data=daten)


# random vs. ols
plmtest(pooled, effect="individual", type="bp")
# fixed vs. ols
pFtest(fixed, ols) 



# model comparison fixed effects models
m1_fe <- plm(log_participation_2 ~ discuss_EU + trust_com + citizen_feel + edu_years + gdp + factor(isocntry), index="year", model ="within", data = daten)
m2_fe <- plm(log_participation ~ discuss_EU + trust_com + citizen_feel + edu_years + gdp + factor(isocntry), index="year", model ="within", data = daten)

summary(m1_fe)
summary(m2_fe)


# Output as HTML
stargazer(m1_fe, m2_fe,
          type = "html",
          covariate.labels = c("1 | discussing politics",
                               "2 | political trust",
                               "3 | sense of citizenship",
                               "4 | education years",
                               "5 | gdp per capita"),
          model.names = TRUE,
          single.row = TRUE,
          out = "fe_models.htm")




# model comparison random effects models
m1_re <- plm(log_participation_2 ~ discuss_EU + trust_com + citizen_feel + edu_years + gdp, model="random", data=daten)
m2_re <- plm(log_participation_2 ~ discuss_EU + trust_com + citizen_feel + edu_years + gdp + compulsory_voting, model="random", data=daten)

summary(m1_re)
summary(m2_re)


# Output as HTML
stargazer(m1_re, m2_re,
          type = "html",
          covariate.labels = c("1 | discussing politics",
                               "2 | political trust",
                               "3 | sense of citizenship",
                               "4 | education years",
                               "5 | gdp per capita",
                               "6 | compulsory voting"),
          model.names = TRUE,
          single.row = TRUE,
          out = "re_models.htm")


# check for bias related to population share that might have been introduced by
# the way of operationalizing the dependent variable

# Extract coefficients from the models

coefficients_m1_fe <- coef(m1_fe)[6:length(coef(m1_fe))] 
coefficients_m2_fe <- coef(m2_fe)[6:length(coef(m2_fe))] 

subset_country_info <- country_info[-1, ]

# Calculate correlation between population share and coefficients

correlation_m1_fe <- cor(subset_country_info$pop_share, coefficients_m1_fe)
correlation_m2_fe <- cor(subset_country_info$pop_share, coefficients_m2_fe)
correlation_m1_fe
correlation_m2_fe


# Output the correlations
cat("Correlation between population share and coefficients from fixed effects model (1):", correlation_m1_fe, "\n")
cat("Correlation between population share and coefficients from fixed effects model (2):", correlation_m2_fe, "\n")




# for appendix ####

# individual level descriptive statistics 

eb19 <- subset_eb_19[, c("isocntry", "d71a_2", "qa14_2", "qd2_1", "d8")]
eb20 <- subset_eb_20[, c("isocntry","d71a_2", "qa12_2", "qc2_1", "d8")]
eb21 <- subset_eb_21[, c("isocntry","d71_2", "qa10_2", "qc2_1", "d8")]
eb22 <- subset_eb_22[, c("isocntry","d71_2", "qa10_2", "qd2_1", "d8")]

eb19$year <- 2019
eb20$year <- 2020
eb21$year <- 2021
eb22$year <- 2022

eb19 <- eb19 %>% rename(
  discuss_EU = d71a_2,
  trust_com = qa14_2,
  citizen_feel = qd2_1,
  edu_years = d8)

eb20 <- eb20 %>% rename(
  discuss_EU = d71a_2,
  trust_com = qa12_2,
  citizen_feel = qc2_1,
  edu_years = d8)

eb21 <- eb21 %>% rename(
  discuss_EU = d71_2,
  trust_com = qa10_2,
  citizen_feel = qc2_1,
  edu_years = d8)

eb22 <- eb22 %>% rename(
  discuss_EU = d71_2,
  trust_com = qa10_2,
  citizen_feel = qd2_1,
  edu_years = d8)

eb19[] <- lapply(eb19, as.character)
eb20[] <- lapply(eb20, as.character)
eb21[] <- lapply(eb21, as.character)
eb22[] <- lapply(eb22, as.character)


all_eb <- bind_rows(eb19, eb20, eb21, eb22)

all_eb <- all_eb %>%
  mutate(across(-isocntry, as.numeric))



# Calculate descriptive statistics excluding min and max values and round them to two decimal places
descriptive_statistics <- all_eb %>%
  dplyr::select(-year) %>%
  group_by(isocntry) %>%
  summarise(
    `Discuss EU (Mean)` = round(mean(discuss_EU, na.rm = TRUE), 2),
    `Discuss EU (SD)` = round(sd(discuss_EU, na.rm = TRUE), 2),
    `Trust Com (Mean)` = round(mean(trust_com, na.rm = TRUE), 2),
    `Trust Com (SD)` = round(sd(trust_com, na.rm = TRUE), 2),
    `Citizen Feel (Mean)` = round(mean(citizen_feel, na.rm = TRUE), 2),
    `Citizen Feel (SD)` = round(sd(citizen_feel, na.rm = TRUE), 2),
    `Edu Years (Mean)` = round(mean(edu_years, na.rm = TRUE), 2),
    `Edu Years (SD)` = round(sd(edu_years, na.rm = TRUE), 2)
  )

# Create HTML table using stargazer
stargazer(descriptive_statistics, type = "html", summary = FALSE, out = "summary_table.html")



