#I copied this from ch1_practiceplots on 1/29/21 to make it a stand-alone file.
# Run this AFTER seacarb_calc_full_dataset
#DATASET LAST RUN IN FULL WITH: full_btl_seacarb in full on 4/21/22. 
#Last made clean_all on 9/19/22 with updated full_btl_seacarb.

library("raster")
library("rgdal")
library("rgeos")
library("rworldmap")
library("sp")
library("sf")
library("tidyverse")

#wcoast <- subset(world2, world$NAME %in% c("Canada", "United States", "Mexico"))
usa <- st_as_sf(maps::map("state", fill=TRUE, plot =FALSE))
wcoast <- usa %>% 
  filter(ID %in% c("california", "oregon", "washington", "nevada", "idaho"))

#If not running from scratch:
load("clean_all_distance_offshore_seacarb.Rdata")


# Re-make clean_all -------------------------------------------------------

load("full_btl_seacarb.Rdata")

#make clean dataframes with seacarb calcs
clean_all <- full %>%
  filter(!is.na(longitude) & !is.na(latitude)) %>% 
  mutate(t_C = ifelse(t_flag == 1, t_C, NA)) %>%
  mutate(sal_pss = ifelse(sal_flag == 1, sal_pss, NA)) %>% 
  mutate(do_umolkg = ifelse(do_flag == 1, do_umolkg, NA)) %>%
  mutate(do_sat = ifelse(do_flag == 1, do_sat, NA)) %>% 
  mutate(pH_total = ifelse(pH_flag == 1, pH_total, NA)) %>% 
  mutate(pCO2_uatm = ifelse(pCO2_flag == 1, pCO2_uatm, NA)) %>% 
  mutate(fCO2_uatm = ifelse(fCO2_flag == 1, fCO2_uatm, NA)) %>% 
  mutate(tCO2_umolkg = ifelse(tCO2_flag == 1, tCO2_umolkg, NA)) %>% 
  mutate(ta_umolkg = ifelse(ta_flag == 1, ta_umolkg, NA)) %>% 
  mutate(chl_ugL = ifelse(chl_flag == 1, chl_ugL, NA)) %>% 
  mutate(across(c(si_umolkg, nh4_umolkg, no2_umolkg, po3_umolkg), ~ifelse(nutr_flag == 1, ., NA)))


save(clean_all, file = "clean_all_distance_offshore_seacarb.Rdata")


## Make a light dataframe with daily averages
#Now that our df has 14 million rows, it's becoming too large to make some of these
#plotting dfs. Especially the cross section ones.

# Drop the flag columns since we're already only using "good" data

# round the buoy depths to cut down on noise from micro depth changes
# datasets c(2, 3, 6:16, 18:20, 30:32, 34, 35, 43, 44, 50, 51, 53:55, 57, 58, 60:66, 70)

clean_all <- clean_all %>% 
  mutate(depth_m = ifelse(dataset_id %in% c(2, 3, 6:16, 18:20, 30:32, 34, 35, 43, 44, 50, 51, 53:55, 57, 58, 60:66, 70),
                          plyr::round_any(depth_m, 5), depth_m))

save(clean_all, file = "clean_rounded_depths.Rdata")

### Skip to this!
load("clean_rounded_depths.Rdata")


datasets <- unique(clean_all$dataset_id)

light <- clean_all[which(clean_all$dataset_id == 1), ]

light <- light %>% 
  select(-c(t_flag, sal_flag, pH_flag, pCO2_flag, fCO2_flag, do_flag, ta_flag, tCO2_flag,
           chl_flag, nutr_flag)) %>% 
  mutate(day = lubridate::date(time_utc)) %>% 
  group_by(day, latitude, longitude, depth_m) %>% 
  summarize(across(where(is.double), ~mean(.x, na.rm = TRUE)),
            across(where(is.character), ~first(.x)), 
            across(where(is.logical), ~first(.x))) %>% 
  dplyr::select(-time_utc) %>% 
  rename(time_utc = day)

nms <- colnames(light)

for(i in 2:length(datasets)){
  temp <- clean_all[which(clean_all$dataset_id == datasets[i]), ]
  
  temp <- temp %>% 
    select(-c(t_flag, sal_flag, pH_flag, pCO2_flag, fCO2_flag, do_flag, ta_flag, tCO2_flag,
              chl_flag, nutr_flag)) %>% 
    mutate(day = lubridate::date(time_utc)) %>% 
    group_by(day, latitude, longitude, depth_m) %>% 
    summarize(across(where(is.double), ~mean(.x, na.rm = TRUE)),
              across(where(is.character), ~first(.x)), 
              across(where(is.logical), ~first(.x))) %>% 
    ungroup() %>% 
    dplyr::select(-time_utc) %>% 
    rename(time_utc = day) %>% 
    dplyr::select(nms)
  
  light <- rbind(light, temp)
  
  light <- light %>% 
    ungroup()
}

light <- light %>% 
  ungroup()

save(light, file = "clean_all_light.Rdata")




# Make all coast seasonal 0.5 degree dataframes ---------------------------
clean_surface <- light %>% 
  filter(depth_m < 50)


seasonal05 <- clean_surface %>% 
  filter(!is.na(time_utc) & !is.na(depth_m)) %>% 
  mutate(season = case_when(lubridate::month(time_utc) %in% c(3:5) ~ "Spring",
                            lubridate::month(time_utc) %in% c(6:8) ~ "Summer",
                            lubridate::month(time_utc) %in% c(9:11) ~ "Fall",
                            lubridate::month(time_utc) %in% c(1, 2, 12) ~ "Winter")) %>% 
  mutate(season = factor(season, levels = c("Spring", "Summer", "Fall", "Winter"))) %>% 
  mutate(longitude = plyr::round_any(longitude, 0.5)) %>% 
  mutate(latitude = plyr::round_any(latitude, 0.5)) %>% 
  mutate(longitude = longitude - 0.1) %>% 
  mutate(pH_all = ifelse(is.na(pH_total), c_pH_total, pH_total)) %>%  #Combo pH, prioritize measured
  group_by(latitude, longitude, season) %>% 
  summarize(t_mean = mean(t_C, na.rm = TRUE), t_max = max(t_C, na.rm = TRUE), t_min = min(t_C, na.rm = TRUE), n_t = sum(!is.na(t_C)),
            sal_mean = mean(sal_pss, na.rm = TRUE), sal_max = max(sal_pss, na.rm = TRUE), sal_min = min(sal_pss, na.rm = TRUE), n_sal = sum(!is.na(sal_pss)),
            do_mean = mean(do_umolkg, na.rm = TRUE), do_max = max(do_umolkg, na.rm = TRUE), do_min = min(do_umolkg, na.rm = TRUE), n_do = sum(!is.na(do_umolkg)),
            pH_mean = mean(pH_total, na.rm = TRUE), pH_max = max(pH_total, na.rm = TRUE), pH_min = min(pH_total, na.rm = TRUE), n_pH = sum(!is.na(pH_total)),
            c_pH_mean = mean(c_pH_total, na.rm = TRUE), c_pH_max = max(c_pH_total, na.rm = TRUE), c_pH_min = min(c_pH_total, na.rm = TRUE), c_n_pH = sum(!is.na(c_pH_total)),
            all_pH_mean = mean(pH_all, na.rm = TRUE), all_pH_max = max(pH_all, na.rm = TRUE), all_pH_min = min(pH_all, na.rm = TRUE), n_all_pH = sum(!is.na(pH_all)),
            tCO2_mean = mean(tCO2_umolkg, na.rm = TRUE), tCO2_max = max(tCO2_umolkg, na.rm = TRUE), tCO2_min = min(tCO2_umolkg, na.rm = TRUE), n_tco2 = sum(!is.na(tCO2_umolkg)),
            ta_mean = mean(ta_umolkg, na.rm = TRUE), ta_max = max(ta_umolkg, na.rm = TRUE), ta_min = min(ta_umolkg, na.rm = TRUE), n_ta = sum(!is.na(ta_umolkg)),
            omega_ar_mean = mean(c_omega_ar, na.rm = TRUE), omega_ar_max = max(c_omega_ar, na.rm = TRUE), omega_ar_min = min(c_omega_ar, na.rm = TRUE), n_omAr = sum(!is.na(c_omega_ar)), 
            omega_ca_mean = mean(c_omega_ca, na.rm = TRUE), omega_ca_max = max(c_omega_ca, na.rm = TRUE), omega_ca_min = min(c_omega_ca, na.rm = TRUE), n_omCa = sum(!is.na(c_omega_ca)),
            distance_offshore = mean(distance_offshore)) %>% 
  mutate_at(vars(-c("latitude", "longitude", "season")), ~ifelse(. == "NaN" | . == -Inf | . == Inf, NA, .))

save(seasonal05, file = "seasonal05_allcoast.Rdata")

seasonal05_since2009 <- clean_surface %>% 
  filter(!is.na(time_utc) & !is.na(depth_m)) %>% 
  filter(lubridate::year(time_utc) >= 2009) %>% 
  mutate(season = case_when(lubridate::month(time_utc) %in% c(3:5) ~ "Spring",
                            lubridate::month(time_utc) %in% c(6:8) ~ "Summer",
                            lubridate::month(time_utc) %in% c(9:11) ~ "Fall",
                            lubridate::month(time_utc) %in% c(1, 2, 12) ~ "Winter")) %>% 
  mutate(season = factor(season, levels = c("Spring", "Summer", "Fall", "Winter"))) %>% 
  mutate(longitude = plyr::round_any(longitude, 0.5)) %>% #This may throw off the means
  mutate(latitude = plyr::round_any(latitude, 0.5)) %>% #This may throw off the means
  mutate(longitude = longitude - 0.1) %>% 
  mutate(pH_all = ifelse(is.na(pH_total), c_pH_total, pH_total)) %>% 
  group_by(latitude, longitude, season) %>% 
  summarize(t_mean = mean(t_C, na.rm = TRUE), t_max = max(t_C, na.rm = TRUE), t_min = min(t_C, na.rm = TRUE), n_t = sum(!is.na(t_C)),
            sal_mean = mean(sal_pss, na.rm = TRUE), sal_max = max(sal_pss, na.rm = TRUE), sal_min = min(sal_pss, na.rm = TRUE), n_sal = sum(!is.na(sal_pss)),
            do_mean = mean(do_umolkg, na.rm = TRUE), do_max = max(do_umolkg, na.rm = TRUE), do_min = min(do_umolkg, na.rm = TRUE), n_do = sum(!is.na(do_umolkg)),
            pH_mean = mean(pH_total, na.rm = TRUE), pH_max = max(pH_total, na.rm = TRUE), pH_min = min(pH_total, na.rm = TRUE), n_pH = sum(!is.na(pH_total)),
            c_pH_mean = mean(c_pH_total, na.rm = TRUE), c_pH_max = max(c_pH_total, na.rm = TRUE), c_pH_min = min(c_pH_total, na.rm = TRUE), c_n_pH = sum(!is.na(c_pH_total)),
            all_pH_mean = mean(pH_all, na.rm = TRUE), all_pH_max = max(pH_all, na.rm = TRUE), all_pH_min = min(pH_all, na.rm = TRUE), n_all_pH = sum(!is.na(pH_all)),
            tCO2_mean = mean(tCO2_umolkg, na.rm = TRUE), tCO2_max = max(tCO2_umolkg, na.rm = TRUE), tCO2_min = min(tCO2_umolkg, na.rm = TRUE), n_tco2 = sum(!is.na(tCO2_umolkg)),
            ta_mean = mean(ta_umolkg, na.rm = TRUE), ta_max = max(ta_umolkg, na.rm = TRUE), ta_min = min(ta_umolkg, na.rm = TRUE), n_ta = sum(!is.na(ta_umolkg)),
            omega_ar_mean = mean(c_omega_ar, na.rm = TRUE), omega_ar_max = max(c_omega_ar, na.rm = TRUE), omega_ar_min = min(c_omega_ar, na.rm = TRUE), n_omAr = sum(!is.na(c_omega_ar)), 
            omega_ca_mean = mean(c_omega_ca, na.rm = TRUE), omega_ca_max = max(c_omega_ca, na.rm = TRUE), omega_ca_min = min(c_omega_ca, na.rm = TRUE), n_omCa = sum(!is.na(c_omega_ca)),
            distance_offshore = mean(distance_offshore)) %>% 
  mutate_at(vars(-c("latitude", "longitude", "season")), ~ifelse(. == "NaN" | . == -Inf | . == Inf, NA, .))

save(seasonal05_since2009, file = "seasonal05_since2009.Rdata")

monthly05 <- clean_surface %>%
  filter(!is.na(time_utc) & !is.na(depth_m)) %>% 
  mutate(month = lubridate::month(time_utc)) %>% 
  mutate(year = lubridate::year(time_utc)) %>% 
  mutate(longitude = plyr::round_any(longitude, 0.5)) %>% 
  mutate(latitude = plyr::round_any(latitude, 0.5)) %>% 
  mutate(longitude = longitude - 0.1) %>% 
  mutate(pH_all = ifelse(is.na(pH_total), c_pH_total, pH_total)) %>% 
  group_by(latitude, longitude, month, year) %>% 
  summarize(t_mean = mean(t_C, na.rm = TRUE), t_max = max(t_C, na.rm = TRUE), t_min = min(t_C, na.rm = TRUE), n_t = sum(!is.na(t_C)),
            sal_mean = mean(sal_pss, na.rm = TRUE), sal_max = max(sal_pss, na.rm = TRUE), sal_min = min(sal_pss, na.rm = TRUE), n_sal = sum(!is.na(sal_pss)),
            do_mean = mean(do_umolkg, na.rm = TRUE), do_max = max(do_umolkg, na.rm = TRUE), do_min = min(do_umolkg, na.rm = TRUE), n_do = sum(!is.na(do_umolkg)),
            pH_mean = mean(pH_total, na.rm = TRUE), pH_max = max(pH_total, na.rm = TRUE), pH_min = min(pH_total, na.rm = TRUE), n_pH = sum(!is.na(pH_total)),
            c_pH_mean = mean(c_pH_total, na.rm = TRUE), c_pH_max = max(c_pH_total, na.rm = TRUE), c_pH_min = min(c_pH_total, na.rm = TRUE), c_n_pH = sum(!is.na(c_pH_total)),
            all_pH_mean = mean(pH_all, na.rm = TRUE), all_pH_max = max(pH_all, na.rm = TRUE), all_pH_min = min(pH_all, na.rm = TRUE), n_all_pH = sum(!is.na(pH_all)),
            tCO2_mean = mean(tCO2_umolkg, na.rm = TRUE), tCO2_max = max(tCO2_umolkg, na.rm = TRUE), tCO2_min = min(tCO2_umolkg, na.rm = TRUE), n_tco2 = sum(!is.na(tCO2_umolkg)),
            ta_mean = mean(ta_umolkg, na.rm = TRUE), ta_max = max(ta_umolkg, na.rm = TRUE), ta_min = min(ta_umolkg, na.rm = TRUE), n_ta = sum(!is.na(ta_umolkg)),
            omega_ar_mean = mean(c_omega_ar, na.rm = TRUE), omega_ar_max = max(c_omega_ar, na.rm = TRUE), omega_ar_min = min(c_omega_ar, na.rm = TRUE), n_omAr = sum(!is.na(c_omega_ar)), 
            omega_ca_mean = mean(c_omega_ca, na.rm = TRUE), omega_ca_max = max(c_omega_ca, na.rm = TRUE), omega_ca_min = min(c_omega_ca, na.rm = TRUE), n_omCa = sum(!is.na(c_omega_ca)),
            distance_offshore = mean(distance_offshore)) %>% 
  mutate_at(vars(-c("latitude", "longitude", "month", "year")), ~ifelse(. == "NaN" | . == -Inf | . == Inf, NA, .))

save(monthly05, file = "monthly05_allcoast.Rdata")

#Since 2009
monthly05_since2009 <- clean_surface %>%
  filter(!is.na(time_utc) & !is.na(depth_m)) %>% 
  filter(lubridate::year(time_utc) >= 2009) %>% 
  mutate(month = lubridate::month(time_utc)) %>% 
  mutate(year = lubridate::year(time_utc)) %>% 
  mutate(longitude = plyr::round_any(longitude, 0.5)) %>% 
  mutate(latitude = plyr::round_any(latitude, 0.5)) %>% 
  mutate(longitude = longitude - 0.1) %>% 
  mutate(pH_all = ifelse(is.na(pH_total), c_pH_total, pH_total)) %>% 
  group_by(latitude, longitude, month, year) %>% 
  summarize(t_mean = mean(t_C, na.rm = TRUE), t_max = max(t_C, na.rm = TRUE), t_min = min(t_C, na.rm = TRUE), n_t = sum(!is.na(t_C)),
            sal_mean = mean(sal_pss, na.rm = TRUE), sal_max = max(sal_pss, na.rm = TRUE), sal_min = min(sal_pss, na.rm = TRUE), n_sal = sum(!is.na(sal_pss)),
            do_mean = mean(do_umolkg, na.rm = TRUE), do_max = max(do_umolkg, na.rm = TRUE), do_min = min(do_umolkg, na.rm = TRUE), n_do = sum(!is.na(do_umolkg)),
            pH_mean = mean(pH_total, na.rm = TRUE), pH_max = max(pH_total, na.rm = TRUE), pH_min = min(pH_total, na.rm = TRUE), n_pH = sum(!is.na(pH_total)),
            c_pH_mean = mean(c_pH_total, na.rm = TRUE), c_pH_max = max(c_pH_total, na.rm = TRUE), c_pH_min = min(c_pH_total, na.rm = TRUE), c_n_pH = sum(!is.na(c_pH_total)),
            all_pH_mean = mean(pH_all, na.rm = TRUE), all_pH_max = max(pH_all, na.rm = TRUE), all_pH_min = min(pH_all, na.rm = TRUE), n_all_pH = sum(!is.na(pH_all)),
            tCO2_mean = mean(tCO2_umolkg, na.rm = TRUE), tCO2_max = max(tCO2_umolkg, na.rm = TRUE), tCO2_min = min(tCO2_umolkg, na.rm = TRUE), n_tco2 = sum(!is.na(tCO2_umolkg)),
            ta_mean = mean(ta_umolkg, na.rm = TRUE), ta_max = max(ta_umolkg, na.rm = TRUE), ta_min = min(ta_umolkg, na.rm = TRUE), n_ta = sum(!is.na(ta_umolkg)),
            omega_ar_mean = mean(c_omega_ar, na.rm = TRUE), omega_ar_max = max(c_omega_ar, na.rm = TRUE), omega_ar_min = min(c_omega_ar, na.rm = TRUE), n_omAr = sum(!is.na(c_omega_ar)), 
            omega_ca_mean = mean(c_omega_ca, na.rm = TRUE), omega_ca_max = max(c_omega_ca, na.rm = TRUE), omega_ca_min = min(c_omega_ca, na.rm = TRUE), n_omCa = sum(!is.na(c_omega_ca)),
            distance_offshore = mean(distance_offshore)) %>% 
  mutate_at(vars(-c("latitude", "longitude", "month", "year")), ~ifelse(. == "NaN" | . == -Inf | . == Inf, NA, .))

save(monthly05_since2009, file = "monthly05_since2009.Rdata")


#Make monthly and seasonal x-section dataframes
#Use the df with daily averages (light) because clean_all is too big
monthly_xs <- light %>%
  filter(!is.na(depth_m)) %>% 
  mutate(pH_all = ifelse(is.na(pH_total), c_pH_total, pH_total)) %>% 
  mutate(month = lubridate::month(time_utc)) %>% 
  mutate(year = lubridate::year(time_utc)) %>% 
  mutate(depth = plyr::round_any(depth_m, 10)) %>%
  group_by(latitude, longitude, month, year, depth) %>% 
  summarize(t_mean = mean(t_C, na.rm = TRUE), t_max = max(t_C, na.rm = TRUE), t_min = min(t_C, na.rm = TRUE), n_t = sum(!is.na(t_C)),
            sal_mean = mean(sal_pss, na.rm = TRUE), sal_max = max(sal_pss, na.rm = TRUE), sal_min = min(sal_pss, na.rm = TRUE), n_sal = sum(!is.na(sal_pss)),
            do_mean = mean(do_umolkg, na.rm = TRUE), do_max = max(do_umolkg, na.rm = TRUE), do_min = min(do_umolkg, na.rm = TRUE), n_do = sum(!is.na(do_umolkg)),
            pH_mean = mean(pH_total, na.rm = TRUE), pH_max = max(pH_total, na.rm = TRUE), pH_min = min(pH_total, na.rm = TRUE), n_pH = sum(!is.na(pH_total)),
            c_pH_mean = mean(c_pH_total, na.rm = TRUE), c_pH_max = max(c_pH_total, na.rm = TRUE), c_pH_min = min(c_pH_total, na.rm = TRUE), c_n_pH = sum(!is.na(c_pH_total)),
            all_pH_mean = mean(pH_all, na.rm = TRUE), all_pH_max = max(pH_all, na.rm = TRUE), all_pH_min = min(pH_all, na.rm = TRUE), n_all_pH = sum(!is.na(pH_all)),
            tCO2_mean = mean(tCO2_umolkg, na.rm = TRUE), tCO2_max = max(tCO2_umolkg, na.rm = TRUE), tCO2_min = min(tCO2_umolkg, na.rm = TRUE), n_tco2 = sum(!is.na(tCO2_umolkg)),
            ta_mean = mean(ta_umolkg, na.rm = TRUE), ta_max = max(ta_umolkg, na.rm = TRUE), ta_min = min(ta_umolkg, na.rm = TRUE), n_ta = sum(!is.na(ta_umolkg)),
            omega_ar_mean = mean(c_omega_ar, na.rm = TRUE), omega_ar_max = max(c_omega_ar, na.rm = TRUE), omega_ar_min = min(c_omega_ar, na.rm = TRUE), n_omAr = sum(!is.na(c_omega_ar)), 
            omega_ca_mean = mean(c_omega_ca, na.rm = TRUE), omega_ca_max = max(c_omega_ca, na.rm = TRUE), omega_ca_min = min(c_omega_ca, na.rm = TRUE), n_omCa = sum(!is.na(c_omega_ca)),
            distance_offshore = mean(distance_offshore)) %>% 
  mutate_at(vars(-c("latitude", "longitude", "month", "year", "depth")), ~ifelse(. == "NaN" | . == -Inf | . == Inf, NA, .))

save(monthly_xs, file = "monthly_xs_df.Rdata")

seasonal_xs <- light %>% 
  filter(!is.na(depth_m)) %>% 
  mutate(pH_all = ifelse(is.na(pH_total), c_pH_total, pH_total)) %>% 
  mutate(year = lubridate::year(time_utc)) %>% 
  mutate(depth = plyr::round_any(depth_m, 10)) %>%
  mutate(season = case_when(lubridate::month(time_utc) %in% c(3:5) ~ "Spring",
                            lubridate::month(time_utc) %in% c(6:8) ~ "Summer",
                            lubridate::month(time_utc) %in% c(9:11) ~ "Fall",
                            lubridate::month(time_utc) %in% c(1, 2, 12) ~ "Winter")) %>% 
  mutate(season = factor(season, levels = c("Spring", "Summer", "Fall", "Winter"))) %>% 
  group_by(latitude, longitude, season, depth) %>% 
  summarize(t_mean = mean(t_C, na.rm = TRUE), t_max = max(t_C, na.rm = TRUE), t_min = min(t_C, na.rm = TRUE), n_t = sum(!is.na(t_C)),
            sal_mean = mean(sal_pss, na.rm = TRUE), sal_max = max(sal_pss, na.rm = TRUE), sal_min = min(sal_pss, na.rm = TRUE), n_sal = sum(!is.na(sal_pss)),
            do_mean = mean(do_umolkg, na.rm = TRUE), do_max = max(do_umolkg, na.rm = TRUE), do_min = min(do_umolkg, na.rm = TRUE), n_do = sum(!is.na(do_umolkg)),
            pH_mean = mean(pH_total, na.rm = TRUE), pH_max = max(pH_total, na.rm = TRUE), pH_min = min(pH_total, na.rm = TRUE), n_pH = sum(!is.na(pH_total)),
            all_pH_mean = mean(pH_all, na.rm = TRUE), all_pH_max = max(pH_all, na.rm = TRUE), all_pH_min = min(pH_all, na.rm = TRUE), n_all_pH = sum(!is.na(pH_all)),
            c_pH_mean = mean(c_pH_total, na.rm = TRUE), c_pH_max = max(c_pH_total, na.rm = TRUE), c_pH_min = min(c_pH_total, na.rm = TRUE), c_n_pH = sum(!is.na(c_pH_total)),
            tCO2_mean = mean(tCO2_umolkg, na.rm = TRUE), tCO2_max = max(tCO2_umolkg, na.rm = TRUE), tCO2_min = min(tCO2_umolkg, na.rm = TRUE), n_tco2 = sum(!is.na(tCO2_umolkg)),
            ta_mean = mean(ta_umolkg, na.rm = TRUE), ta_max = max(ta_umolkg, na.rm = TRUE), ta_min = min(ta_umolkg, na.rm = TRUE), n_ta = sum(!is.na(ta_umolkg)),
            omega_ar_mean = mean(c_omega_ar, na.rm = TRUE), omega_ar_max = max(c_omega_ar, na.rm = TRUE), omega_ar_min = min(c_omega_ar, na.rm = TRUE), n_omAr = sum(!is.na(c_omega_ar)), 
            omega_ca_mean = mean(c_omega_ca, na.rm = TRUE), omega_ca_max = max(c_omega_ca, na.rm = TRUE), omega_ca_min = min(c_omega_ca, na.rm = TRUE), n_omCa = sum(!is.na(c_omega_ca)),
            distance_offshore = mean(distance_offshore)) %>% 
  mutate_at(vars(-c("latitude", "longitude", "season", "depth")), ~ifelse(. == "NaN" | . == -Inf | . == Inf, NA, .))

save(seasonal_xs, file = "seasonal_xs_df.Rdata")
