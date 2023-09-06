
# This script is for transforming our (perfect!) full dataset to match NCEI specs a bit more
# This is designed for the DSP-paper version of our full dataset, not the one that 
# exists in my Summary Plots folder

# This is a mirror script of "transform_ncei_for_our_code.R"

library("tidyverse")

load("full_nd.Rdata")


glimpse(full)

# make the ncei df
ncei <- full

# Break the date and time into consituent columns
ncei <- ncei %>%
  mutate(YEAR_UTC = lubridate::year(time_utc)) %>% 
  mutate(MONTH_UTC = lubridate::month(time_utc)) %>% 
  mutate(DAY_UTC = lubridate::day(time_utc)) %>% 
  mutate(TIME_UTC = format(as.POSIXct(time_utc), format = "%H:%M:%S")) %>% 
  mutate(Yearday_UTC = lubridate::yday(time_utc) + (lubridate::second(time_utc)/60^2 + lubridate::minute(time_utc)/60 + 
                                                      lubridate::hour(time_utc))/24)


# Rename some columns
ncei <- ncei %>% 
  rename(Dataset_ID = dataset_id, Distance_From_Shore = distance_offshore,
         Collection_Scheme = sample_scheme, Environment = coastal_environment,
         Latitude = latitude, Longitude = longitude, Depth = depth_m,
         SST_ITS90 = t_C, SST_flag = t_flag, SAL_PSS78 = sal_pss, SAL_flag = sal_flag,
         Oxygen = do_umolkg, Oxygen_flag = do_flag, Percent_O2 = do_sat,
         DIC = tCO2_umolkg, DIC_flag = tCO2_flag, DIC_type = tCO2_type,
         TA = ta_umolkg, TA_flag = ta_flag, TA_type = ta_type,
         pH_T_insitu = pH_total, fCO2_measured = fCO2_uatm,
         pCO2_measured = pCO2_uatm, Silicate = si_umolkg, Phosphate = po4_umolkg,
         Nitrate = no3_umolkg, Nitrite = no2_umolkg, Nutrient_flag = nutr_flag, 
         Ammonium = nh4_umolkg, Nutrient_type = nutr_type,
         Chl_a = chl_ugL, Chl_a_flag = chl_flag, Chl_a_type = chl_type)

glimpse(ncei)

# Order things as expected
ncei <- ncei %>% 
  arrange(Dataset_ID, time_utc) %>% 
  select(-time_utc) %>% 
  select(Dataset_ID, Collection_Scheme, YEAR_UTC, MONTH_UTC, DAY_UTC, TIME_UTC, 
         Yearday_UTC, Latitude, Longitude, Depth, Distance_From_Shore, Environment, 
         SST_ITS90, SST_flag, SAL_PSS78, SAL_flag, Oxygen, Percent_O2, Oxygen_flag,
         DIC, DIC_flag, DIC_type, TA, TA_flag, TA_type, pH_T_insitu, pH_flag, pH_type,
         pCO2_measured, pCO2_flag, pCO2_type, fCO2_measured, fCO2_flag, fCO2_type,
         Silicate, Phosphate, Nitrate, Nitrite, Ammonium, Nutrient_flag, Nutrient_type,
         Chl_a, Chl_a_flag, Chl_a_type)

# Export
save(ncei, file = "full_ncei_format.Rdata")
write_csv(ncei, file = "full_ncei_format.csv")
