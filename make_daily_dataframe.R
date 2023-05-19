# This script generates a "clean" dataframe with only data flagged as "good",
# as well as a dataframe that's been summarized by day for each data source, location,
# and depth.

# The summarized dataframe is created with only good data - all questionable observations
# are eliminated. As such, the dataframe no longer includes quality flags.

# This script takes data after it has gone through calculate_carb_params.R

library("tidyverse")

# Make the "clean" dataframe with only good observations -------------------------------------------------------

load("full_discrete_seacarb.Rdata")

#make clean dataframes with seacarb calcs
clean_df <- df %>%
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


save(clean_df, file = "clean_df_distance_offshore_seacarb.Rdata")


## Make a light dataframe with daily averages

# Drop the flag columns since we're already only using "good" data

# round the buoy depths to cut down on noise from micro depth changes
# datasets c(2, 3, 6:16, 18:20, 30:32, 34, 35, 43, 44, 50, 51, 53:55, 57, 58, 60:66, 70)

clean_df <- clean_df %>% 
  mutate(depth_m = ifelse(dataset_id %in% c(2, 3, 6:16, 18:20, 30:32, 34, 35, 43, 44, 50, 51, 53:55, 57, 58, 60:66, 70),
                          plyr::round_any(depth_m, 5), depth_m))

save(clean_df, file = "clean_rounded_depths.Rdata")


# Make the "light" daily dataframe ----------------------------------------


### Skip to this if regenerating light!
load("clean_rounded_depths.Rdata")


datasets <- unique(clean_df$dataset_id)

light <- clean_all[which(clean_df$dataset_id == 1), ]

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
  temp <- clean_df[which(clean_df$dataset_id == datasets[i]), ]
  
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

