# ------------------------------------------------------------------------------
# Script: 02_statistical_analysis.R
# Purpose: Undertake statistical analysis of how air pollution-related
#          climate policies impact average annual exposure to PM2.5 air pollution
# Author: Dr. Lucas Geese
# Last updated: 2025-07-08
# ------------------------------------------------------------------------------


# Load required packages --------------------------------------------------


# Define required packages
required_packages <- c("here", "tidyverse", "sjmisc", "plm", "readxl", "vdemdata",
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
library(plm)
library(readxl)
library(vdemdata)
library(broom)
library(cowplot)


# Step 1: Load and prepare processed CCLW  --------------------------------


# Set root directory
here::i_am("scripts/02_statistical_analysis.R")

# Import processed CCLW data
cclw_final <- readRDS(here("data", "processed", "cclw_final.rds"))

# Prepare time-lagged versions of CCLW dataset
# One-year lag:
cclw_final_t1 <- cclw_final %>%
  mutate(Year = Year + 1) %>% 
  rename(ap_topic_max_lag1 = ap_topic_max,
         asd_topic_max_lag1 = asd_topic_max) %>%  
  select(country_iso, Year, ap_topic_max_lag1, asd_topic_max_lag1)
# Two-year lag:
cclw_final_t2 <- cclw_final %>%
  mutate(Year = Year + 2) %>% 
  rename(ap_topic_max_lag2 = ap_topic_max,
         asd_topic_max_lag2 = asd_topic_max) %>%  
  select(country_iso, Year, ap_topic_max_lag2, asd_topic_max_lag2)
# Three-year lag:
cclw_final_t3 <- cclw_final %>%
  mutate(Year = Year + 3) %>% 
  rename(ap_topic_max_lag3 = ap_topic_max,
         asd_topic_max_lag3 = asd_topic_max) %>%  
  select(country_iso, Year, ap_topic_max_lag3, asd_topic_max_lag3)
# Four-year lag:
cclw_final_t4 <- cclw_final %>%
  mutate(Year = Year + 4) %>% 
  rename(ap_topic_max_lag4 = ap_topic_max,
         asd_topic_max_lag4 = asd_topic_max) %>%  
  select(country_iso, Year, ap_topic_max_lag4, asd_topic_max_lag4)


# Step 2: Import country-level contextual data ---------------------------


# Impport V-Dem data: includes political corruption and gender equality measures, used as control variables
vdemdata <- vdem %>% as_tibble()
vdemdata <- vdemdata %>% select(country_text_id, year, v2x_gender, v2x_corr) %>% 
  rename(country_iso = country_text_id,
         Year = year)

# Impport UN Human Development Index: country-year specific scores as well as categorises countries into 
# Low, Medium, High, Very High HDI  
hdi <- read_csv("https://hdr.undp.org/sites/default/files/2025_HDR/HDR25_Composite_indices_complete_time_series.csv")
hdi <- hdi[,1:39]
hdi <- hdi %>% 
  select(-country, -region, -hdi_rank_2023) %>% 
  pivot_longer(cols = starts_with("hdi_"), names_to = "Year", values_to = "hdi") %>% 
  rename(country_iso = iso3,
         HDI_Group = hdicode) %>% 
  mutate(Year = as.integer(str_remove(Year, "hdi_")))

# Worldbank: GDP per capita, PPP (constant 2021 international $)
gdp_pc <- read_csv(here("data", "raw", "WB_GDP_PCA_PPP.csv") ,skip = 3)
gdp_pc <- gdp_pc %>% 
  select(-`Country Name`, -`Indicator Name`, -`Indicator Code`, -`...70`)
colnames(gdp_pc) <- c("country_iso", str_c("gdp_", as.character(1960:2024)))
gdp_pc <- gdp_pc %>%
  pivot_longer(cols = starts_with("gdp_"), names_to = "Year", values_to = "gdp_pc") %>% 
  mutate(Year = as.integer(str_remove(Year, "gdp_")))


# Step 3: Import and prepare health outcome data --------------------------


# Fetch the data: Average annual exposure to PM2.5 air pollution from Our World in Data (OWID)
ap_exposure <- read.csv("https://ourworldindata.org/grapher/average-exposure-pm25-pollution.csv?v=1&csvType=full&useColumnShortNames=true")


# Step 4: Merge health outcome data with CCLW and contextual data ---------


ap_exposure <- ap_exposure %>% 
  rename(country_iso = Code,
         ap_exposure = en_atm_pm25_mc_m3) %>% 
  filter(country_iso!="") %>% 
  left_join(cclw_final) %>% 
  left_join(cclw_final_t1) %>% 
  left_join(cclw_final_t2) %>% 
  left_join(cclw_final_t3) %>% 
  left_join(cclw_final_t4) %>% 
  left_join(gdp_pc) %>% 
  left_join(hdi) %>% 
  left_join(vdemdata) %>% 
  # Standardize outcome and explanatory variables for comparability and interpretability in regression
  mutate(ap_exposure_z = scale(ap_exposure),
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


# Step 5: Run fixed effects panel models ---------------------------------


# Convert to panel data format

panel_data <- pdata.frame(ap_exposure, index = c("country_iso", "Year"))

# Fixed effects regression models
fe_model_t0 <- plm(ap_exposure_z ~ 
                  ap_topic_max_z + gdp_pc_z + v2x_gender_z + v2x_corr_z,
                  data = panel_data, 
                  model = "within")  # "within" specifies fixed effects

# Estimate time-lagged effects to account for policy implementation delays
fe_model_t1 <- plm(ap_exposure_z ~ 
                     ap_topic_max_z_lag1 + gdp_pc_z + v2x_gender_z + v2x_corr_z,
                   data = panel_data, 
                   model = "within")  # "within" specifies fixed effects

fe_model_t2 <- plm(ap_exposure_z ~ 
                     ap_topic_max_z_lag2 + gdp_pc_z + v2x_gender_z + v2x_corr_z,
                   data = panel_data, 
                   model = "within")  # "within" specifies fixed effects

fe_model_t3 <- plm(ap_exposure_z ~ 
                     ap_topic_max_z_lag3 + gdp_pc_z + v2x_gender_z + v2x_corr_z,
                   data = panel_data, 
                   model = "within")  # "within" specifies fixed effects

fe_model_t4 <- plm(ap_exposure_z ~ 
                     ap_topic_max_z_lag4 + gdp_pc_z + v2x_gender_z + v2x_corr_z,
                   data = panel_data, 
                   model = "within")  # "within" specifies fixed effects

# Interaction model to test whether policy effects differ by human development level
fe_model_hdi <- plm(ap_exposure_z ~ 
                     ap_topic_max_z*HDI_Group + gdp_pc_z + v2x_gender_z + v2x_corr_z,
                   data = panel_data, 
                   model = "within")  # "within" specifies fixed effects

# Summaries of the models
summary(fe_model_t0)
summary(fe_model_t1)
summary(fe_model_t2)
summary(fe_model_t3)
summary(fe_model_t4)
summary(fe_model_hdi)


# Step 6: Plot key findings from time-lagged models -----------------------


# Get tidy summaries for each model to extract coefficients and confidence intervals
tidy_t0 <- tidy(fe_model_t0, conf.int = TRUE)
tidy_t1 <- tidy(fe_model_t1, conf.int = TRUE)
tidy_t2 <- tidy(fe_model_t2, conf.int = TRUE)
tidy_t3 <- tidy(fe_model_t3, conf.int = TRUE)
tidy_t4 <- tidy(fe_model_t4, conf.int = TRUE)


# Extract estimates for the air pollution policy variable from each tidy summary
extract_coef <- function(tidy_model, varname) {
  tidy_model %>%
    filter(term == varname) %>%
    select(estimate, conf.low, conf.high)
}

effects_df <- bind_rows(
  extract_coef(tidy_t0, "ap_topic_max_z"),
  extract_coef(tidy_t1, "ap_topic_max_z_lag1"),
  extract_coef(tidy_t2, "ap_topic_max_z_lag2"),
  extract_coef(tidy_t3, "ap_topic_max_z_lag3"),
  extract_coef(tidy_t4, "ap_topic_max_z_lag4")) %>%
  mutate(lag = factor(c("t", "t-1", "t-2", "t-3", "t-4"),
                      levels = c("t", "t-1", "t-2", "t-3", "t-4")))

# Plot marginal effects
plot1 <- effects_df %>% 
  ggplot(aes(x = lag, y = estimate)) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.15) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey50") +
  labs(
    title = "Marginal Effects of Air Pollution Policy Indicator on PM2.5 Exposure\nAcross Time Lags",
    x = "Time Lag (Years)",
    y = "Estimated Effect (Standardised Outcome)") +
  theme_bw() + theme(axis.title.y = element_blank())


# Step 7: Plot key findings from HDI Group models -------------------------


# Get tidy summary from HDI interaction model
model_summary <- tidy(fe_model_hdi, conf.int = TRUE)

# Filter the relevant terms (main effect and interactions)
effects_summary <- model_summary %>%
  filter(term %in% c("ap_topic_max_z",
                     "ap_topic_max_z:HDI_GroupLow",
                     "ap_topic_max_z:HDI_GroupMedium",
                     "ap_topic_max_z:HDI_GroupVery High"))

# Compute marginal effects by HDI group using interaction terms
marginal_effects_summary <- tibble(
  hdi_group = c( "3 - High", "4 - Very high", "2 - Medium", "1 - Low"),
  estimate = c(
    model_summary$estimate[model_summary$term == "ap_topic_max_z"],
    model_summary$estimate[model_summary$term == "ap_topic_max_z"] +
      model_summary$estimate[model_summary$term == "ap_topic_max_z:HDI_GroupVery High"],
    model_summary$estimate[model_summary$term == "ap_topic_max_z"] +
      model_summary$estimate[model_summary$term == "ap_topic_max_z:HDI_GroupMedium"],
    model_summary$estimate[model_summary$term == "ap_topic_max_z"] +
      model_summary$estimate[model_summary$term == "ap_topic_max_z:HDI_GroupLow"]
  ),
  lower = c(
    model_summary$conf.low[model_summary$term == "ap_topic_max_z"],
    model_summary$conf.low[model_summary$term == "ap_topic_max_z"] +
      model_summary$conf.low[model_summary$term == "ap_topic_max_z:HDI_GroupVery High"],
    model_summary$conf.low[model_summary$term == "ap_topic_max_z"] +
      model_summary$conf.low[model_summary$term == "ap_topic_max_z:HDI_GroupMedium"],
    model_summary$conf.low[model_summary$term == "ap_topic_max_z"] +
      model_summary$conf.low[model_summary$term == "ap_topic_max_z:HDI_GroupLow"]
  ),
  upper = c(
    model_summary$conf.high[model_summary$term == "ap_topic_max_z"],
    model_summary$conf.high[model_summary$term == "ap_topic_max_z"] +
      model_summary$conf.high[model_summary$term == "ap_topic_max_z:HDI_GroupVery High"],
    model_summary$conf.high[model_summary$term == "ap_topic_max_z"] +
      model_summary$conf.high[model_summary$term == "ap_topic_max_z:HDI_GroupMedium"],
    model_summary$conf.high[model_summary$term == "ap_topic_max_z"] +
      model_summary$conf.high[model_summary$term == "ap_topic_max_z:HDI_GroupLow"]
  )
)


# Plot marginal effects
plot2 <- marginal_effects_summary %>% 
  ggplot(aes(x = hdi_group, y = estimate)) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.2) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey50") +
  labs(
    title = "Marginal Effects of Air Pollution Policy Indicator on PM2.5 Exposure by HDI Group",
    x = "HDI Group",
    y = "Estimated Effect (Standardised)"
  ) +
  theme_bw() + theme(axis.title.y = element_blank())


# Step 8: Combine in one common plot ---------------------------------------

# Combine plots vertically with cowplot
combined <- plot_grid(plot1, plot2,
  ncol = 1, align = "v", axis = "lr", rel_heights = c(1, 1))

# Add shared y-axis label
final_plot <- ggdraw() +
  draw_label("Estimated Effect on PM2.5 Exposure (Standardised)", 
             x = 0.02, y = 0.5, angle = 90, vjust = 0.5, size = 12) +
  draw_plot(combined, x = 0.05, y = 0, width = 0.95, height = 1)

# Open graphics device explicitly, then save
png(here("output", "figures", "combined_plot.png"), width = 8, height = 5, units = "in", res = 300)
print(final_plot)
dev.off()


# Step 9: Save recoded data frames for robustness checks ------------------

save(ap_exposure, cclw_final, cclw_final_t1, cclw_final_t2, cclw_final_t3, 
     cclw_final_t4, gdp_pc, hdi, vdemdata, 
     file = here("data", "processed", "stats_data.RData"))


