

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