# This script is for transforming our (perfect!) full dataset to match NCEI specs a bit more
# This is designed for the DSP-paper version of our full dataset, not the one that 
# exists in my Summary Plots folder

# This is a mirror script of "transform_ncei_for_our_code.R"

library("tidyverse")

load("full_ncei_format.Rdata")


glimpse(ncei)

# make the ncei df
df <- ncei

# recombine the date and time into time_utc
df <- df %>%
  mutate(time_utc = lubridate::make_date(year = YEAR_UTC, month = MONTH_UTC, day = DAY_UTC)) %>% 
  mutate(time_utc = lubridate::ymd_hms(paste(time_utc, TIME_UTC)))



# Rename some columns
df <- df %>% 
  rename(dataset_id = Dataset_ID, distance_offshore = Distance_From_Shore,
         sample_scheme = Collection_Scheme, coastal_environment = Environment,
         latitude = Latitude, longitude = Longitude, depth_m = Depth,
         t_C = SST_ITS90, t_flag = SST_flag, sal_pss = SAL_PSS78, sal_flag = SAL_flag,
         do_umolkg = Oxygen, do_flag = Oxygen_flag, do_sat = Percent_O2,
         tCO2_umolkg = DIC, tCO2_flag = DIC_flag, tCO2_type = DIC_type,
         ta_umolkg = TA, ta_flag = TA_flag, ta_type = TA_type,
         pH_total = pH_T_insitu, fCO2_uatm = fCO2_measured,
         pCO2_uatm = pCO2_measured, si_umolkg = Silicate, po4_umolkg = Phosphate,
         no3_umolkg = Nitrate, no2_umolkg = Nitrite, nutr_flag = Nutrient_flag, 
         nh4_umolkg = Ammonium, nutr_type = Nutrient_type,
         chl_ugL = Chl_a, chl_flag = Chl_a_flag, chl_type = Chl_a_type)

df <- df %>% 
  select(-c(YEAR_UTC, MONTH_UTC, DAY_UTC, TIME_UTC, Yearday_UTC))

glimpse(df)



# Export if necessary
save(df, file = "df_from_ncei_reformatted.Rdata")
write_csv(df, file = "df_from_ncei_reformatted.csv")