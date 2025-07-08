# ------------------------------------------------------------------------------
# Script: 03_validation_and_robustness.R
# Purpose: Provide validation for computational text analysis and robustness
#          checks for statistical analysis
# Author: Dr. Lucas Geese
# Last updated: 2025-07-07
# ------------------------------------------------------------------------------


# Load required packages --------------------------------------------------

# Define required packages
required_packages <- c("here", "tidyverse", "sjmisc", "stringi", "stm", "quanteda", 
                       "rvest", "devtools", "officer", "plm", "readxl", "vdemdata",
                       "broom", "cowplot")

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
library(plm)
library(readxl)
library(vdemdata)
library(broom)
library(cowplot)

# Set root directory
here::i_am("scripts/03_validation_and_robustness.R")

# Step 1: Assess alternative STM specifications ---------------------------

load(here("models", "stm.fit_cclw.RData"))
set.seed(123)
stm.fit_cclw50 <- stm(dtm_cclw_trimmed$documents, dtm_cclw_trimmed$vocab, K=50, 
                     prevalence =~ HDI_Group_2023 + s(Year) + gdp_pc + v2x_gender + v2x_corr, 
                     data = dtm_cclw_trimmed$meta, 
                     init.type = "Spectral")
set.seed(123)
stm.fit_cclw60 <- stm(dtm_cclw_trimmed$documents, dtm_cclw_trimmed$vocab, K=60, 
                      prevalence =~ HDI_Group_2023 + s(Year) + gdp_pc + v2x_gender + v2x_corr, 
                      data = dtm_cclw_trimmed$meta, 
                      init.type = "Spectral")

save(stm.fit_cclw50, stm.fit_cclw60, file = here("models", "stm.fit_cclw_addon.RData"))
# Optional: reload STM model without re-running
# load(here("models", "stm.fit_cclw_addon.RData"))

# Check topic-defining words and proportions
labelTopics(stm.fit_cclw50)
labelTopics(stm.fit_cclw60)

# Topics that have an air pollution dimension to it
labelTopics(stm.fit_cclw0, topics = c(18, 26, 38, 43, 47))
labelTopics(stm.fit_cclw50, topics = c(5, 12, 25, 44, 47))
labelTopics(stm.fit_cclw60, topics = c(5, 12, 18, 25, 42, 48))

# Actual topic proportions are stored in a matrix inside the stm.fit object: theta
# This puts them into a new tibble object
topics_tibble <- as_tibble(stm.fit_cclw0$theta)
topics_tibble50 <- as_tibble(stm.fit_cclw50$theta)
topics_tibble60 <- as_tibble(stm.fit_cclw60$theta)

# Extract the highest topic proportion for each document across topics  
topics_tibble$max_prop <- apply(topics_tibble[,1:52], 1, function(x) rev(sort(x))[1])
topics_tibble50$max_prop <- apply(topics_tibble50[,1:50], 1, function(x) rev(sort(x))[1])
topics_tibble60$max_prop <- apply(topics_tibble60[,1:60], 1, function(x) rev(sort(x))[1])

# Identify documents maxing on the five air pollution topics; this variable therefore
# indicates policies with  one of the five air pollution topics scoring as the highest 
# of all topic proportions
topics_tibble$ap_topic_max <- ifelse((topics_tibble$V18 == topics_tibble$max_prop) | 
                                       (topics_tibble$V26 == topics_tibble$max_prop) |
                                       (topics_tibble$V38 == topics_tibble$max_prop) |
                                       (topics_tibble$V43 == topics_tibble$max_prop) |
                                       (topics_tibble$V47 == topics_tibble$max_prop), 1, 0)

topics_tibble50$ap_topic_max <- ifelse((topics_tibble50$V5 == topics_tibble50$max_prop) | 
                                       (topics_tibble50$V12 == topics_tibble50$max_prop) |
                                       (topics_tibble50$V25 == topics_tibble50$max_prop) |
                                       (topics_tibble50$V44 == topics_tibble50$max_prop) |
                                         (topics_tibble50$V47 == topics_tibble50$max_prop), 1, 0)
5, 12, 18, 25, 42, 48
topics_tibble60$ap_topic_max <- ifelse((topics_tibble60$V5 == topics_tibble60$max_prop) | 
                                         (topics_tibble60$V12 == topics_tibble60$max_prop) |
                                         (topics_tibble60$V18 == topics_tibble60$max_prop) |
                                         (topics_tibble60$V25 == topics_tibble60$max_prop) |
                                         (topics_tibble60$V42 == topics_tibble60$max_prop) |
                                         (topics_tibble60$V48 == topics_tibble60$max_prop), 1, 0)

table(topics_tibble$ap_topic_max, topics_tibble50$ap_topic_max)
(5008+266)/sum(table(topics_tibble$ap_topic_max, topics_tibble50$ap_topic_max))

table(topics_tibble$ap_topic_max, topics_tibble60$ap_topic_max)
(4944+196)/sum(table(topics_tibble$ap_topic_max, topics_tibble60$ap_topic_max))



# Step 3: Import and prepare health outcome data --------------------------



# Fetch the data: Lancet Countdown Indicator 2.3.1 Vulnerability to Severe Mosquito-Borne Disease
download.file("https://lancetcountdown.org/wp-content/uploads/2025/04/Indicator-2.3.1_Data-Download_2024-Lancet-Countdown-Report.xlsx", 
              destfile = "data/raw/Lancet_Indicator-2.3.1.xlsx",
              mode = "wb")
mosquito_disease <- read_excel("data/raw/Lancet_Indicator-2.3.1.xlsx", sheet = "2024 Report Data") %>% 
  rename(country_iso = `ISO3 Code`,
         mosquito_disease_vuln = `Vulnerability Index - Scaled`) %>% 
  select(country_iso, Year, mosquito_disease_vuln)


# Step 4: Merge health outcome data with CCLW and contextual data ---------

mosquito_disease <- mosquito_disease %>% 
  left_join(cclw_final) %>% 
  left_join(cclw_final_t1) %>% 
  left_join(cclw_final_t2) %>% 
  left_join(cclw_final_t3) %>% 
  left_join(cclw_final_t4) %>%  
  left_join(gdp_pc) %>% 
  left_join(hdi) %>% 
  left_join(vdemdata) %>% 
  mutate(mosquito_disease_vuln_z = scale(mosquito_disease_vuln),
         ap_topic_max_z = scale(ap_topic_max),
         ap_topic_max_z_lag1 = scale(ap_topic_max_lag1),
         ap_topic_max_z_lag2 = scale(ap_topic_max_lag2),
         ap_topic_max_z_lag3 = scale(ap_topic_max_lag3),
         ap_topic_max_z_lag4 = scale(ap_topic_max_lag4),
         asd_topic_max_z = scale(asd_topic_max),
         asd_topic_max_z_lag1 = scale(asd_topic_max_lag1),
         asd_topic_max_z_lag2 = scale(asd_topic_max_lag2),
         asd_topic_max_z_lag3 = scale(asd_topic_max_lag3),
         asd_topic_max_z_lag4 = scale(asd_topic_max_lag4),
         gdp_pc_z = scale(gdp_pc),
         v2x_gender_z = scale(v2x_gender),
         v2x_corr_z = scale(v2x_corr))


