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

# Step 1: Assess alternative STM specifications ---------------------------

load(here("models", "stm.fit_cclw.RData"))

stm.fit_cclw40 <- stm(dtm_cclw_trimmed$documents, dtm_cclw_trimmed$vocab, K=40, 
                     prevalence =~ HDI_Group_2023 + s(Year) + gdp_pc + v2x_gender + v2x_corr, 
                     data = dtm_cclw_trimmed$meta, 
                     init.type = "Spectral")

stm.fit_cclw60 <- stm(dtm_cclw_trimmed$documents, dtm_cclw_trimmed$vocab, K=40, 
                      prevalence =~ HDI_Group_2023 + s(Year) + gdp_pc + v2x_gender + v2x_corr, 
                      data = dtm_cclw_trimmed$meta, 
                      init.type = "Spectral")


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