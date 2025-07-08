# ------------------------------------------------------------------------------
# Script: 01_text_indicator_development.R
# Purpose: Generate country-year-level indicator of air pollution-related
#          climate policies using structural topic models (STM)
# Author: Dr. Lucas Geese
# Last updated: 2025-07-07
# ------------------------------------------------------------------------------
# Github
# Load required packages --------------------------------------------------

# Define required packages
required_packages <- c("here", "tidyverse", "sjmisc", "stringi", "stm", "quanteda", 
                       "rvest", "devtools", "officer")

# Install required packages only if not already installed (for reproducibility)
# Only install packages if the script is run interactively (e.g., in an R session)
# This avoids installation attempts during batch processing or when the script is executed
# in a non-interactive environment (e.g. Rscript calls).
if (interactive()) {
  for (pkg in required_packages) {
    if (!requireNamespace(pkg, quietly = TRUE)) {
      install.packages(pkg)
    }
  }
  # Install GitHub package if not already installed
  if (!requireNamespace("vdemdata", quietly = TRUE)) {
    remotes::install_github("vdeminstitute/vdemdata")
  }
}

# Load libraries
library(here)
library(tidyverse)
library(sjmisc)
library(stringi)
library(stm)
library(quanteda)
library(rvest)
library(vdemdata)
library(officer)

# Step 1: Load Climate Laws data ------------------------------------------

# Set root directory
here::i_am("scripts/01_text_indicator_development.R")

# Load raw data
cclw_raw <- read_csv(here("data", "raw", "cclw_raw.csv"))

# Inspect key columns and entries
glimpse(cclw_raw)
cclw_raw %>% frq(Category)

# Focus on of laws an executive policies
cclw_raw <- cclw_raw %>% filter(Category == "Executive" | Category == "Legislative")


# Step 2: Preprocess policy text summaries --------------------------------


# Inspect policy text summary variable
cclw_raw$`Family Summary`[c(sample(1:nrow(cclw_raw), 5))] 

# Dataset contains duplicate texts:
cclw_raw %>% group_by(`Document Title`) %>% summarise()
cclw_raw %>% group_by(`Family Summary`) %>% summarise()

# Drop excess variables and duplicate texts in Document Title and Text Summary
cclw_raw <- cclw_raw %>% 
  mutate(Year = year(`Last event in timeline`)) %>% 
  select(`Geography ISOs`, Category, Year, `Document Title`, `Family Summary`) %>% 
  rename(country_iso = `Geography ISOs`,
         doc_title = `Document Title`,
         text_raw = `Family Summary`)

# Clean text
text_clean <- c()
for(i in 1:nrow(cclw_raw)){
  # Some observations have missing values for the text summary of policies/laws, this keeps the missing values:
  if(is.na(cclw_raw$text_raw[i])==T) {
    x <- NA
  # Most text summaries carry html tags, this cleans them:
  } else if(str_detect(cclw_raw$text_raw[i], "<.*>")==T) {
    x <- cclw_raw$text_raw[i] %>% read_html() %>% html_text2()
  # Some text summaries do not carry html tags, this maintains them:
  } else {
    x <- cclw_raw$text_raw[i] 
  }
  text_clean <- c(text_clean, x)
}

# Strip excess white spaces and line breaks:
text_clean <- text_clean %>% str_squish()
text_clean <- text_clean %>% str_remove("&nbsp")

# Add cleaned text to dataframe
cclw_raw$text_clean <- text_clean

# Delete missing text observations
sum(is.na(cclw_raw$text_clean))
cclw_raw <- cclw_raw %>% filter(is.na(text_clean)==F)

# Delete very short texts
cclw_raw$n_words <- stri_count_words(cclw_raw$text_clean)
sum(cclw_raw$n_words<6)
cclw_raw$text_clean[cclw_raw$n_words<6]
cclw_raw <- cclw_raw %>% filter(n_words>5)


# Step 3: Extend data by country-level contextual data --------------------


# Impport V-Dem data: Indicators of Women's Political Empowerment and Political Corruption
vdemdata <- vdem %>% as_tibble()
vdemdata <- vdemdata %>% select(country_text_id, year, v2x_gender, v2x_corr) %>% 
  rename(country_iso = country_text_id,
         Year = year)

# Impport UN Human Development Index: country-year specific scores as well as 2023 group variable  
hdi <- read_csv("https://hdr.undp.org/sites/default/files/2025_HDR/HDR25_Composite_indices_complete_time_series.csv")
hdi <- hdi[,1:39]
hdi <- hdi %>% 
  select(-country, -region, -hdi_rank_2023) %>% 
  pivot_longer(cols = starts_with("hdi_"), names_to = "Year", values_to = "hdi") %>% 
  rename(country_iso = iso3,
         HDI_Group = hdicode) %>% 
  mutate(Year = as.integer(str_remove(Year, "hdi_")))

hdi_groups_2023 <- hdi %>% 
  filter(Year==2023) %>% 
  select(country_iso, HDI_Group) %>% 
  rename(HDI_Group_2023 = HDI_Group)

# Worldbank: GDP per capita, PPP (constant 2021 international $)
gdp_pc <- read_csv(here("data", "raw", "WB_GDP_PCA_PPP.csv"),skip = 3)
gdp_pc <- gdp_pc %>% 
  select(-`Country Name`, -`Indicator Name`, -`Indicator Code`, -`...70`)
colnames(gdp_pc) <- c("country_iso", str_c("gdp_", as.character(1960:2024)))
gdp_pc <- gdp_pc %>%
  pivot_longer(cols = starts_with("gdp_"), names_to = "Year", values_to = "gdp_pc") %>% 
  mutate(Year = as.integer(str_remove(Year, "gdp_")))

# Merge cclw with contextual data

cclw <- cclw_raw %>% 
  left_join(hdi) %>% left_join(hdi_groups_2023) %>% left_join(gdp_pc) %>% left_join(vdemdata) %>% 
  filter(Year<2025)

# Check variable codings and missing data
cclw %>% frq(HDI_Group, HDI_Group_2023)
sum(is.na(cclw$hdi))
sum(is.na(cclw$gdp_pc))
sum(is.na(cclw$v2x_gender))
sum(is.na(cclw$v2x_corr))

# Define missing data and listwise delete observations with missing values (required for Structural Topic Models)
cclw <- cclw %>% 
  mutate(HDI_Group_2023 = set_na(HDI_Group_2023, na = "Other Countries or Territories"),
         HDI_Group= set_na(HDI_Group, na = "Other Countries or Territories")) %>% 
  filter(is.na(HDI_Group_2023)==F) %>% 
  filter(is.na(gdp_pc)==F) %>%
  filter(is.na(v2x_gender)==F) %>%
  filter(is.na(v2x_corr)==F) 
cclw %>% frq(HDI_Group_2023, HDI_Group)


# Step 4: Estimate Structural Topic Models --------------------------------


# Generate quanteda corpus object
corpus <- corpus(cclw$text_clean,
                 docvars = bind_cols(HDI_Group_2023 = factor(cclw$HDI_Group_2023, levels = c("Low", "Medium", "High", "Very High")),
                                     Year = cclw$Year,
                                     gdp_pc = cclw$gdp_pc,
                                     v2x_gender = cclw$v2x_gender,
                                     v2x_corr = cclw$v2x_corr))

# Tokenise, remove punctuation and symbols, lowercase, remove stopwords, stem words and 
# create a document-term matrix
dtm_cclw <- corpus %>% 
  tokens(remove_punct = T, remove_symbols = T) %>% 
  tokens_tolower() %>% 
  dfm() %>% 
  dfm_remove(stopwords("english")) %>% 
  dfm_wordstem()
dim(dtm_cclw)

# To further reduce dimensionality, we delete very rare and very frequent terms, i.e
# terms that appear in at least 0.01% and at most 90% of all documents 
dtm_cclw_trimmed <- dfm_trim(dtm_cclw, min_docfreq = .001, max_docfreq = .9, docfreq_type = "prop")
dim(dtm_cclw_trimmed)

# Convert dtm to stm object
dtm_cclw_trimmed <- quanteda::convert(dtm_cclw_trimmed, to = c("stm"))
out <- dtm_cclw_trimmed

# Estimate STM model with the number of topics  being determined by the algorithm of Lee and Mimno (2014) 
set.seed(123)
stm.fit_cclw0 <- stm(dtm_cclw_trimmed$documents, dtm_cclw_trimmed$vocab, K=0, 
                     prevalence =~ HDI_Group_2023 + s(Year) + gdp_pc + v2x_gender + v2x_corr, 
                     data = dtm_cclw_trimmed$meta, 
                     init.type = "Spectral")

save(stm.fit_cclw0, file = here("models", "stm.fit_cclw.RData"))
# Optional: reload STM model without re-running
# load(here("models", "stm.fit_cclw.RData"))

# Check topic-defining words and proportions
labelTopics(stm.fit_cclw0)

# We find 5 Topics that have an air pollution dimension to it
labelTopics(stm.fit_cclw0, topics = c(18, 26, 38, 43, 47))

# Save topics, FREX terms, and topic labels in word table 
topic_table <- data.frame(
  Topic = c(18, 26, 38, 43, 47),
  `Top FREX Terms` = c(
    "air, pollut, limit, atmospher, coal-fir, dioxid, t",
    "liquid, fuel, automobil, truck, fossil, pact, biomethan",
    "physic, nitrogen, deposit, leefomgev, netherland, besluit, valu",
    "tax, car, km, usd, hybrid, vehicl, eur",
    "cycl, walk, bicycl, mobil, advertis, bike, center"
  ),
  Label = c(
    "Air Pollution Standards",
    "Fuel and Transport Emissions Regulation",
    "Nitrogen Pollution and Environmental Planning",
    "Carbon-Based Vehicle Taxation",
    "Active Travel Promotion"
  ),
  stringsAsFactors = FALSE
)

doc <- read_docx()
doc <- body_add_par(doc, "STM Topic Overview", style = "heading 2")
doc <- body_add_table(doc, topic_table, style = "table_template")
print(doc, target = here("output", "tables", "stm_topics_simple.docx"))

# Actual topic proportions are stored in a matrix inside the stm.fit object: theta
# This puts them into a new tibble object
topics_tibble <- as_tibble(stm.fit_cclw0$theta)

# Extract the highest topic proportion for each document across topics  
topics_tibble$max_prop <- apply(topics_tibble[,1:52], 1, function(x) rev(sort(x))[1])

# Identify documents maxing on the five air pollution topics; this variable therefore
# indicates policies with  one of the five air pollution topics scoring as the highest 
# of all topic proportions
topics_tibble$ap_topic_max <- ifelse((topics_tibble$V18 == topics_tibble$max_prop) | 
                                       (topics_tibble$V26 == topics_tibble$max_prop) |
                                       (topics_tibble$V38 == topics_tibble$max_prop) |
                                       (topics_tibble$V43 == topics_tibble$max_prop) |
                                       (topics_tibble$V47 == topics_tibble$max_prop), 1, 0)

# Identify documents maxing on another topic "Agricultural Sustainable Development" to be used for robustness checks
topics_tibble$asd_topic_max <- ifelse((topics_tibble$V7 == topics_tibble$max_prop) , 1, 0)
topics_tibble %>% frq(ap_topic_max, asd_topic_max)

# Assign topic proportions to cclw dataset:
cclw <- cclw %>% 
  add_column(ap_topic_max = topics_tibble$ap_topic_max,
             asd_topic_max = topics_tibble$asd_topic_max)


# Step 5: Aggregate STM output to country-year indicators -----------------


# Generate an empty country-year grid, so we can have "0" observations for country-years without policymaking
country_list <- cclw %>% group_by(country_iso) %>% summarise() %>% ungroup()
country_year_grid <- tibble(country_iso = as.character(c()), 
                            Year = as.numeric(c())) 
for(i in 1990:2024) {
  x <- country_list %>% mutate(Year = i)
  country_year_grid <- country_year_grid %>% add_row(x)
}

# Merge country-year with cclw data
country_year_grid <- country_year_grid %>% left_join(cclw)

# Aggregate cclw policies at country-year level such that each country-year indicates 
# a count of passed air-pollution policies (and agricultural sustainable development 
# policies) measured by the structural topic model
cclw_final <- country_year_grid %>% 
  select(country_iso, Year, ap_topic_max, asd_topic_max) %>% 
  group_by(country_iso, Year) %>% 
  summarise(ap_topic_max = sum(ap_topic_max, na.rm = T),
            asd_topic_max = sum(asd_topic_max, na.rm = T)) %>% 
  ungroup()


# # Step 6: Save final indicator dataset ----------------------------------


saveRDS(cclw_final, here("data", "processed", "cclw_final.rds"))
