# This code is for doing seacarb calculations on the dataset.
# It assumes data has been formatted according to the 
# "transform_ncei_for_our_code.R script.

# This calculated the full carbonate parameters for paired discrete samples ONLY,
# not paired sensor samples. 

library("sf")
library("sp")
library("raster")
library("tidyverse")
library("lubridate")
library("seacarb")



#  Make bottle-only dataframe to do seacarb calculations -------------------
load("df_from_ncei_reformatted.Rdata")

# Vector of eventual column names for ordering dataframes
nms <- c("dataset_id","latitude","longitude","depth_m","time_utc",
         "sample_scheme", "coastal_environment", "t_C","t_flag",
         "sal_pss","sal_flag","pH_total","pH_flag", "pH_type", "pCO2_uatm","pCO2_flag",
         "pCO2_type","fCO2_uatm","fCO2_flag","fCO2_type","tCO2_umolkg","tCO2_flag",
         "tCO2_type","ta_umolkg","ta_flag","ta_type","do_umolkg","do_sat","do_flag",
         "do_type","chl_ugL","chl_flag","chl_type","si_umolkg","nh4_umolkg","no3_umolkg",
         "no2_umolkg","po3_umolkg","nutr_flag","nutr_type", "c_fCO2_uatm",
         "c_pCO2_uatm","c_pH_total","c_CO2_umolkg","c_HCO3_umolkg","c_CO3_umolkg",
         "c_tCO2_umolkg","c_TA_umolkg","c_omega_ar","c_omega_ca","c_revelle", "distance_offshore")

#Make a full-length dataframe of "good" observations only to later match with the seacarb calcs
clean_df <- df %>% 
  mutate(t_C = ifelse(t_flag != 1, NA, t_C)) %>% 
  mutate(sal_pss = ifelse(sal_flag != 1, NA, sal_pss)) %>% 
  mutate(do_umolkg = ifelse(do_flag != 1, NA, do_umolkg)) %>% 
  mutate(do_sat = ifelse(do_flag != 1, NA, do_sat)) %>% 
  mutate(pH_total = ifelse(pH_flag != 1, NA, pH_total)) %>% 
  mutate(pCO2_uatm = ifelse(pCO2_flag != 1, NA, pCO2_uatm)) %>% 
  mutate(fCO2_uatm = ifelse(fCO2_flag != 1, NA, fCO2_uatm)) %>% 
  mutate(tCO2_umolkg = ifelse(tCO2_flag != 1, NA, tCO2_umolkg)) %>% 
  mutate(ta_umolkg = ifelse(ta_flag != 1, NA, ta_umolkg))

#Make a dataframe of paired discrete observations for calculations with only "good" measurements
bottle_seacarb <- df %>% 
  mutate(t_C = ifelse(t_flag != 1, NA, t_C)) %>% 
  mutate(sal_pss = ifelse(sal_flag != 1, NA, sal_pss)) %>% 
  mutate(do_umolkg = ifelse(do_flag != 1, NA, do_umolkg)) %>% 
  mutate(pH_total = ifelse(pH_flag != 1, NA, pH_total)) %>% 
  mutate(tCO2_umolkg = ifelse(tCO2_flag != 1, NA, tCO2_umolkg)) %>% 
  mutate(ta_umolkg = ifelse(ta_flag != 1, NA, ta_umolkg)) %>% 
  filter(!is.na(t_C) & !is.na(sal_pss)) %>% 
  filter((!is.na(pH_total) & !is.na(ta_umolkg) & pH_type == "discrete" & ta_type == "discrete") |
           (!is.na(pH_total) & !is.na(tCO2_umolkg) & pH_type == "discrete" & tCO2_type == "discrete") |
           (!is.na(tCO2_umolkg) & !is.na(ta_umolkg) & tCO2_type == "discrete" & ta_type == "discrete")) 


# Seacarb calcs with TA-DIC pairs -----------------------------------------
tadic_pairs <- bottle_seacarb %>% 
  filter(!is.na(ta_umolkg) & !is.na(tCO2_umolkg)) %>% 
  filter(ta_type == "discrete" & tCO2_type == "discrete")

#Need to change the DIC and TA columns to mol/kg from umol/kg and depth to bars (pressure)
seacarbunits <- tadic_pairs %>% 
  mutate(ta_umolkg = ta_umolkg/10^6) %>% 
  mutate(tCO2_umolkg = tCO2_umolkg/10^6) %>% 
  mutate(pressure = depth_m/10)

#Calculate the seacarb parameters. Note - this does not use nutrients
seacarb_data <- carb(flag = 15, var1 = seacarbunits$ta_umolkg, var2 = seacarbunits$tCO2_umolkg, 
                        S = seacarbunits$sal_pss, T = seacarbunits$t_C, Patm = 1, P = seacarbunits$pressure,
                        Sit = 0, k1k2 = "x", kf = "x", ks = "d", pHscale = "T")


#bind the two spreadsheets together, then clean up
tadic_pairs <- bind_cols(tadic_pairs, seacarb_data)

tadic_pairs <- tadic_pairs %>% 
  mutate(c_pH_total = pH) %>% 
  mutate(c_fCO2_uatm = fCO2) %>% 
  mutate(c_pCO2_uatm = pCO2) %>% 
  mutate(c_HCO3_umolkg = HCO3) %>% 
  mutate(c_CO3_umolkg = CO3) %>% 
  mutate(c_omega_ar = OmegaAragonite) %>% 
  mutate(c_omega_ca = OmegaCalcite) %>% 
  dplyr::select(nms)


# Calculations with pH-TA pairs -----------------------------------
phta_pairs <- bottle_seacarb %>% 
  filter(is.na(tCO2_umolkg)) %>% 
  filter(!is.na(t_C) & !is.na(sal_pss) & !is.na(ta_umolkg) & !is.na(pH_total)) %>% 
  filter(ta_type == "discrete" & pH_type == "discrete")

#Need to change the DIC and TA columns to mol/kg from umol/kg
seacarbunits <- phta_pairs %>% 
  mutate(ta_umolkg = ta_umolkg/10^6) %>% 
  mutate(tCO2_umolkg = tCO2_umolkg/10^6) %>% 
  mutate(pressure = depth_m/10)

#Calculate the seacarb parameters - flag 8 (pH and TA)
seacarb_data <- carb(flag = 8, var1 = seacarbunits$pH_total, var2 = seacarbunits$ta_umolkg, 
                     S = seacarbunits$sal_pss, T = seacarbunits$t_C, Patm = 1, P = seacarbunits$pressure,
                     Sit = 0, k1k2 = "x", kf = "x", ks = "d", pHscale = "T")

rm(seacarbunits)

phta_pairs <- bind_cols(phta_pairs, seacarb_data)

phta_pairs <- phta_pairs %>% 
  mutate(c_fCO2_uatm = fCO2) %>% 
  mutate(c_pCO2_uatm = pCO2) %>% 
  mutate(c_HCO3_umolkg = HCO3) %>% 
  mutate(c_CO3_umolkg = CO3) %>%
  mutate(c_tCO2_umolkg = DIC) %>% 
  mutate(c_omega_ar = OmegaAragonite) %>% 
  mutate(c_omega_ca = OmegaCalcite) %>% 
  dplyr::select(nms)

# Seacarb calcs with pH-DIC pairs -----------------------------------------
phdic_pairs <- bottle_seacarb %>% 
  filter(is.na(ta_umolkg)) %>% 
  filter(!is.na(t_C) & !is.na(sal_pss) & !is.na(tCO2_umolkg) & !is.na(pH_total)) %>%  
  filter(tCO2_type == "discrete" & pH_type == "discrete")

#Need to change the DIC and TA columns to mol/kg from umol/kg
seacarbunits <- phdic_pairs %>% 
  mutate(ta_umolkg = ta_umolkg/10^6) %>% 
  mutate(tCO2_umolkg = tCO2_umolkg/10^6) %>% 
  mutate(pressure = depth_m/10)

#Calculate the seacarb parameters
seacarb_data <- carb(flag = 9, var1 = seacarbunits$pH_total, var2 = seacarbunits$tCO2_umolkg, 
                     S = seacarbunits$sal_pss, T = seacarbunits$t_C, Patm = 1, P = seacarbunits$pressure,
                     Sit = 0, k1k2 = "x", kf = "x", ks = "d", pHscale = "T")

rm(seacarbunits)

phdic_pairs <- bind_cols(phdic_pairs, seacarb_data)

phdic_pairs <- phdic_pairs %>% 
  mutate(c_fCO2_uatm = fCO2) %>% 
  mutate(c_pCO2_uatm = pCO2) %>% 
  mutate(c_HCO3_umolkg = HCO3) %>% 
  mutate(c_CO3_umolkg = CO3) %>% 
  mutate(c_TA_umolkg = ALK) %>% 
  mutate(c_omega_ar = OmegaAragonite) %>% 
  mutate(c_omega_ca = OmegaCalcite) %>% 
  dplyr::select(nms)

rm(seacarb_phdic, seacarb_phta, seacarb_tadic)


# Bring the dataframes together, update units -------------------------------------------

bottle_seacarb <- rbind(phdic_pairs, phta_pairs, tadic_pairs)

#update units
bottle_seacarb <- bottle_seacarb %>% 
  mutate(c_HCO3_umolkg = c_HCO3_umolkg * 10^6) %>% 
  mutate(c_CO3_umolkg = c_CO3_umolkg * 10^6) %>% 
  mutate(c_tCO2_umolkg = c_tCO2_umolkg * 10^6) %>% 
  mutate(c_TA_umolkg = c_TA_umolkg * 10^6)

#Export just these discrete observations if intersted
write_csv(bottle_seacarb, file = "all_discrete_seacarbcalcs.csv")


#Combine everything into a giant dataframe
df_seacarb <- left_join(clean_df, bottle_seacarb)



#Order the full dataframe
df <- df_seacarb %>% 
  dplyr::select(nms)


summary(df)

#Export the full dataset with seacarb calcs
write_csv(df, file = "full_discrete_seacarb.csv")
save(df, file = "full_discrete_seacarb.Rdata")

