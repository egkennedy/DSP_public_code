#This code is for doing seacarb calculations on the full dataset.
#It was made on 12/22/20, pulling code from prospectus_figures.R
#This will now serve as my script for updating the seacarb_calcs dataset.

#LAST RUN for bottle seacarb: 4/21/23 through dataset 71
#LAST RUN bottom portion: 9/7/21

library("sf")
library("sp")
library("raster")
library("tidyverse")
library("lubridate")
library("seacarb")


usa <- st_as_sf(maps::map("state", fill=TRUE, plot =FALSE))
wcoast <- usa %>% 
  filter(ID %in% c("california", "oregon", "washington", "nevada", "idaho"))

source( "calculate_distance.R" )
land <- st_read( "shapefiles/americas" )
land <- st_transform(land, crs=crs(wcoast))


file_nms <- list.files("C:/Users/esthe/Documents/Data Synthesis Project/Final_datasets_nd")

nms <- c("dataset_id","latitude","longitude","depth_m","time_utc","t_C","t_flag",
         "sal_pss","sal_flag","pH_total","pH_flag", "pH_type", "pCO2_uatm","pCO2_flag",
         "pCO2_type","fCO2_uatm","fCO2_flag","fCO2_type","tCO2_umolkg","tCO2_flag",
         "tCO2_type","ta_umolkg","ta_flag","ta_type","do_umolkg","do_sat","do_flag",
         "do_type","chl_ugL","chl_flag","chl_type","si_umolkg","nh4_umolkg","no3_umolkg",
         "no2_umolkg","po3_umolkg","nutr_flag","nutr_type","hab","c_fCO2_uatm",
         "c_pCO2_uatm","c_pH_total","c_CO2_umolkg","c_HCO3_umolkg","c_CO3_umolkg",
         "c_tCO2_umolkg","c_TA_umolkg","c_omega_ar","c_omega_ca","c_revelle")

# 1. Rebuilding updated dataframe from scratch (Do not use unless necessary)---------------------------------------



#Make target dataframe, remove the top row.
full_i <- as.data.frame(as.list(nms), row.names = NULL, optional = FALSE)
full_i <- full_i[FALSE,]

full <- as.data.frame(as.list(nms), row.names = NULL, optional = FALSE)
full <- full %>% mutate(distance_offshore = NA)
full <- full_i[FALSE,]


#Full formatted dataset pulling from Final_datasets_nd folder.
# LAST UPDATED: 9/15/22
for(i in file_nms){
  full_i <- read_csv(paste("C:/Users/esthe/Documents/Data Synthesis Project/Final_datasets_nd/",i,sep=""),
                     col_types = cols(dataset_id = col_double(),
                                      latitude = col_double(),
                                      longitude = col_double(),
                                      depth_m = col_double(),
                                      time_utc = col_datetime(format = ""),
                                      t_C = col_double(),
                                      t_flag = col_double(),
                                      sal_pss = col_double(),
                                      sal_flag = col_double(),
                                      pH_total = col_double(),
                                      pH_flag = col_double(),
                                      pH_type = col_character(),
                                      pCO2_uatm = col_double(),
                                      pCO2_flag = col_double(),
                                      pCO2_type = col_character(),
                                      fCO2_uatm = col_double(),
                                      fCO2_flag = col_double(),
                                      fCO2_type = col_character(),
                                      tCO2_umolkg = col_double(),
                                      tCO2_flag = col_double(),
                                      tCO2_type = col_character(),
                                      ta_umolkg = col_double(),
                                      ta_flag = col_double(),
                                      ta_type = col_character(),
                                      do_umolkg = col_double(),
                                      do_sat = col_double(),
                                      do_flag = col_double(),
                                      do_type = col_character(),
                                      chl_ugL = col_double(),
                                      chl_flag = col_double(),
                                      chl_type = col_character(),
                                      si_umolkg = col_double(),
                                      nh4_umolkg = col_double(),
                                      no3_umolkg = col_double(),
                                      no2_umolkg = col_double(),
                                      po3_umolkg = col_double(),
                                      nutr_flag = col_double(),
                                      nutr_type = col_character(),
                                      hab = col_character(),
                                      c_fCO2_uatm = col_double(),
                                      c_pCO2_uatm = col_double(),
                                      c_pH_total = col_double(),
                                      c_CO2_umolkg = col_double(),
                                      c_HCO3_umolkg = col_double(),
                                      c_CO3_umolkg = col_double(),
                                      c_tCO2_umolkg = col_double(),
                                      c_TA_umolkg = col_double(),
                                      c_omega_ar = col_double(),
                                      c_omega_ca = col_double(),
                                      c_revelle = col_double()
                     ))
  
  full_i <- full_i %>% 
    filter(!is.na(latitude) & !is.na(longitude))
  
  data_sf <- st_as_sf(full_i, coords = c(x = "longitude", y = "latitude"), crs = crs(wcoast))
  
  data_sf[[ 'distance_offshore' ]] <- calculate_distance( data_sf, land, coords="geometry")
  
  full_i$distance_offshore <- data_sf$distance_offshore
  
  full <- rbind(full, data.frame(full_i))
}

rm(full_i, data_sf)

summary(full)
tail(full)


#Write a new full file. Also copy it to the general datasets folder
write_csv(full, "full_nd.csv")
save(full, file = "full_nd.Rdata")


# 2. Updating existing full dataset with new data ----------------------------
# Get existing full_nd dataset
load("full_nd.Rdata")

#Get the file names of the missing datasets
# Make sure these datasets are included in the Final_datasets_nd folder
datasets_included <- unique(full$dataset_id)

file_nms <- list.files("C:/Users/esthe/Documents/Data Synthesis Project/Final_datasets_nd")
total_datasets <- parse_number(file_nms)

missing_datasets <- which(!(total_datasets %in% datasets_included))
missing_datasets <- file_nms[missing_datasets]

#Make the blank template df to put new datasets into
nms <- c("dataset_id","latitude","longitude","depth_m","time_utc","t_C","t_flag",
         "sal_pss","sal_flag","pH_total","pH_flag", "pH_type", "pCO2_uatm","pCO2_flag",
         "pCO2_type","fCO2_uatm","fCO2_flag","fCO2_type","tCO2_umolkg","tCO2_flag",
         "tCO2_type","ta_umolkg","ta_flag","ta_type","do_umolkg","do_sat","do_flag",
         "do_type","chl_ugL","chl_flag","chl_type","si_umolkg","nh4_umolkg","no3_umolkg",
         "no2_umolkg","po3_umolkg","nutr_flag","nutr_type","hab","c_fCO2_uatm",
         "c_pCO2_uatm","c_pH_total","c_CO2_umolkg","c_HCO3_umolkg","c_CO3_umolkg",
         "c_tCO2_umolkg","c_TA_umolkg","c_omega_ar","c_omega_ca","c_revelle")

#Make target dataframe, remove the top row.
full_i <- as.data.frame(as.list(nms), row.names = NULL, optional = FALSE)
full_i <- full_i[FALSE,]

#Full formatted dataset pulling from Final_datasets_nd folder.
# LAST UPDATED: 4/10/23
for(i in missing_datasets){
  full_i <- read_csv(paste("C:/Users/esthe/Documents/Data Synthesis Project/Final_datasets_nd/",i,sep=""),
                     col_types = cols(dataset_id = col_double(),
                                      latitude = col_double(),
                                      longitude = col_double(),
                                      depth_m = col_double(),
                                      time_utc = col_datetime(format = ""),
                                      t_C = col_double(),
                                      t_flag = col_double(),
                                      sal_pss = col_double(),
                                      sal_flag = col_double(),
                                      pH_total = col_double(),
                                      pH_flag = col_double(),
                                      pH_type = col_character(),
                                      pCO2_uatm = col_double(),
                                      pCO2_flag = col_double(),
                                      pCO2_type = col_character(),
                                      fCO2_uatm = col_double(),
                                      fCO2_flag = col_double(),
                                      fCO2_type = col_character(),
                                      tCO2_umolkg = col_double(),
                                      tCO2_flag = col_double(),
                                      tCO2_type = col_character(),
                                      ta_umolkg = col_double(),
                                      ta_flag = col_double(),
                                      ta_type = col_character(),
                                      do_umolkg = col_double(),
                                      do_sat = col_double(),
                                      do_flag = col_double(),
                                      do_type = col_character(),
                                      chl_ugL = col_double(),
                                      chl_flag = col_double(),
                                      chl_type = col_character(),
                                      si_umolkg = col_double(),
                                      nh4_umolkg = col_double(),
                                      no3_umolkg = col_double(),
                                      no2_umolkg = col_double(),
                                      po3_umolkg = col_double(),
                                      nutr_flag = col_double(),
                                      nutr_type = col_character(),
                                      hab = col_character(),
                                      c_fCO2_uatm = col_double(),
                                      c_pCO2_uatm = col_double(),
                                      c_pH_total = col_double(),
                                      c_CO2_umolkg = col_double(),
                                      c_HCO3_umolkg = col_double(),
                                      c_CO3_umolkg = col_double(),
                                      c_tCO2_umolkg = col_double(),
                                      c_TA_umolkg = col_double(),
                                      c_omega_ar = col_double(),
                                      c_omega_ca = col_double(),
                                      c_revelle = col_double()
                     ))
 
  full_i <- full_i %>% 
    filter(!is.na(latitude) & !is.na(longitude))
  
  data_sf <- st_as_sf(full_i, coords = c(x = "longitude", y = "latitude"), crs = crs(wcoast))
  
  data_sf[[ 'distance_offshore' ]] <- calculate_distance( data_sf, land, coords="geometry")
  
  full_i$distance_offshore <- data_sf$distance_offshore
  
  full <- rbind(full, data.frame(full_i))

}

save(full, file = "full_nd.Rdata")



# 3. Make bottle-only dataframe to do seacarb calculations -------------------
load("full_nd.Rdata")

nms <- c("dataset_id","latitude","longitude","depth_m","time_utc","t_C","t_flag",
         "sal_pss","sal_flag","pH_total","pH_flag", "pH_type", "pCO2_uatm","pCO2_flag",
         "pCO2_type","fCO2_uatm","fCO2_flag","fCO2_type","tCO2_umolkg","tCO2_flag",
         "tCO2_type","ta_umolkg","ta_flag","ta_type","do_umolkg","do_sat","do_flag",
         "do_type","chl_ugL","chl_flag","chl_type","si_umolkg","nh4_umolkg","no3_umolkg",
         "no2_umolkg","po3_umolkg","nutr_flag","nutr_type","hab","c_fCO2_uatm",
         "c_pCO2_uatm","c_pH_total","c_CO2_umolkg","c_HCO3_umolkg","c_CO3_umolkg",
         "c_tCO2_umolkg","c_TA_umolkg","c_omega_ar","c_omega_ca","c_revelle", "distance_offshore")

#Make a full-length DF for later to match with the seacarb calcs
clean_full <- full %>% 
  mutate(t_C = ifelse(t_flag != 1, NA, t_C)) %>% 
  mutate(sal_pss = ifelse(sal_flag != 1, NA, sal_pss)) %>% 
  mutate(do_umolkg = ifelse(do_flag != 1, NA, do_umolkg)) %>% 
  mutate(do_sat = ifelse(do_flag != 1, NA, do_sat)) %>% 
  mutate(pH_total = ifelse(pH_flag != 1, NA, pH_total)) %>% 
  mutate(pCO2_uatm = ifelse(pCO2_flag != 1, NA, pCO2_uatm)) %>% 
  mutate(fCO2_uatm = ifelse(fCO2_flag != 1, NA, fCO2_uatm)) %>% 
  mutate(tCO2_umolkg = ifelse(tCO2_flag != 1, NA, tCO2_umolkg)) %>% 
  mutate(ta_umolkg = ifelse(ta_flag != 1, NA, ta_umolkg))

#Make bottle df for calculations and remove flagged measurements
bottle_seacarb <- full %>% 
  mutate(t_C = ifelse(t_flag != 1, NA, t_C)) %>% 
  mutate(sal_pss = ifelse(sal_flag != 1, NA, sal_pss)) %>% 
  mutate(do_umolkg = ifelse(do_flag != 1, NA, do_umolkg)) %>% 
  mutate(pH_total = ifelse(pH_flag != 1, NA, pH_total)) %>% 
  mutate(tCO2_umolkg = ifelse(tCO2_flag != 1, NA, tCO2_umolkg)) %>% 
  mutate(ta_umolkg = ifelse(ta_flag != 1, NA, ta_umolkg)) %>% 
  filter(!is.na(t_C) & !is.na(sal_pss)) %>% 
  filter((!is.na(pH_total) & !is.na(ta_umolkg) & pH_type == "bottle" & ta_type == "bottle") |
           (!is.na(pH_total) & !is.na(tCO2_umolkg) & pH_type == "bottle" & tCO2_type == "bottle") |
           (!is.na(tCO2_umolkg) & !is.na(ta_umolkg) & tCO2_type == "bottle" & ta_type == "bottle")) 


# 4. Seacarb calcs with TA-DIC pairs -----------------------------------------
tadic_pairs <- bottle_seacarb %>% 
  filter(!is.na(ta_umolkg) & !is.na(tCO2_umolkg)) %>% 
  filter(ta_type == "bottle" & tCO2_type == "bottle")

#Need to change the DIC and TA columns to mol/kg from umol/kg and depth to bars (pressure)
seacarbunits <- tadic_pairs %>% 
  mutate(ta_umolkg = ta_umolkg/10^6) %>% 
  mutate(tCO2_umolkg = tCO2_umolkg/10^6) %>% 
  mutate(pressure = depth_m/10)

#Calculate the seacarb parameters - note, not using nutrients this time
seacarb_tadic <- carb(flag = 15, var1 = seacarbunits$ta_umolkg, var2 = seacarbunits$tCO2_umolkg, 
                        S = seacarbunits$sal_pss, T = seacarbunits$t_C, Patm = 1, P = seacarbunits$pressure,
                        Sit = 0, k1k2 = "x", kf = "x", ks = "d", pHscale = "T")


#bind the two spreadsheets together, then clean up
tadic_pairs <- bind_cols(tadic_pairs, seacarb_tadic)

tadic_pairs <- tadic_pairs %>% 
  mutate(c_pH_total = pH) %>% 
  mutate(c_fCO2_uatm = fCO2) %>% 
  mutate(c_pCO2_uatm = pCO2) %>% 
  mutate(c_HCO3_umolkg = HCO3) %>% 
  mutate(c_CO3_umolkg = CO3) %>% 
  mutate(c_omega_ar = OmegaAragonite) %>% 
  mutate(c_omega_ca = OmegaCalcite) %>% 
  dplyr::select(nms)

rm(seacarbunits)


# 5. Seacarb calculations with pH-TA pairs -----------------------------------
#Note - there are still some pH-DIC pairs
phta_pairs <- bottle_seacarb %>% 
  filter(is.na(tCO2_umolkg)) %>% 
  filter(!is.na(t_C) & !is.na(sal_pss) & !is.na(ta_umolkg) & !is.na(pH_total)) %>% 
  filter(ta_type == "bottle" & pH_type == "bottle")

#Need to change the DIC and TA columns to mol/kg from umol/kg
seacarbunits <- phta_pairs %>% 
  mutate(ta_umolkg = ta_umolkg/10^6) %>% 
  mutate(tCO2_umolkg = tCO2_umolkg/10^6) %>% 
  mutate(pressure = depth_m/10)

#Calculate the seacarb parameters - flag 8 (pH and TA)
seacarb_phta <- carb(flag = 8, var1 = seacarbunits$pH_total, var2 = seacarbunits$ta_umolkg, 
                     S = seacarbunits$sal_pss, T = seacarbunits$t_C, Patm = 1, P = seacarbunits$pressure,
                     Sit = 0, k1k2 = "x", kf = "x", ks = "d", pHscale = "T")

rm(seacarbunits)

phta_pairs <- bind_cols(phta_pairs, seacarb_phta)

phta_pairs <- phta_pairs %>% 
  mutate(c_fCO2_uatm = fCO2) %>% 
  mutate(c_pCO2_uatm = pCO2) %>% 
  mutate(c_HCO3_umolkg = HCO3) %>% 
  mutate(c_CO3_umolkg = CO3) %>%
  mutate(c_tCO2_umolkg = DIC) %>% 
  mutate(c_omega_ar = OmegaAragonite) %>% 
  mutate(c_omega_ca = OmegaCalcite) %>% 
  dplyr::select(nms)

# 6. Seacarb calcs with pH-DIC pairs -----------------------------------------
phdic_pairs <- bottle_seacarb %>% 
  filter(is.na(ta_umolkg)) %>% 
  filter(!is.na(t_C) & !is.na(sal_pss) & !is.na(tCO2_umolkg) & !is.na(pH_total)) %>%  
  filter(tCO2_type == "bottle" & pH_type == "bottle")

#Need to change the DIC and TA columns to mol/kg from umol/kg
seacarbunits <- phdic_pairs %>% 
  mutate(ta_umolkg = ta_umolkg/10^6) %>% 
  mutate(tCO2_umolkg = tCO2_umolkg/10^6) %>% 
  mutate(pressure = depth_m/10)

#Calculate the seacarb parameters - flag 9 (pH and DIC)
seacarb_phdic <- carb(flag = 9, var1 = seacarbunits$pH_total, var2 = seacarbunits$tCO2_umolkg, 
                     S = seacarbunits$sal_pss, T = seacarbunits$t_C, Patm = 1, P = seacarbunits$pressure,
                     Sit = 0, k1k2 = "x", kf = "x", ks = "d", pHscale = "T")

rm(seacarbunits)

phdic_pairs <- bind_cols(phdic_pairs, seacarb_phdic)

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


# 7. Bring the dataframes together, update units -------------------------------------------

bottle_seacarb <- rbind(phdic_pairs, phta_pairs, tadic_pairs)

#update units
bottle_seacarb <- bottle_seacarb %>% 
  mutate(c_HCO3_umolkg = c_HCO3_umolkg * 10^6) %>% 
  mutate(c_CO3_umolkg = c_CO3_umolkg * 10^6) %>% 
  mutate(c_tCO2_umolkg = c_tCO2_umolkg * 10^6) %>% 
  mutate(c_TA_umolkg = c_TA_umolkg * 10^6)

#Export the bottle seacarb dataset
write_csv(bottle_seacarb, file = "all_bottle_seacarbcalcs.csv")


#Combine everything into a giant dataframe
calcs <- full %>% 
  dplyr::select(contains("c_")) %>% 
  colnames()

msmts <- full %>% 
  dplyr::select(-contains("c_")) %>% 
  colnames()

clean_full <- clean_full %>% 
  dplyr::select(msmts)


full_seacarb <- left_join(clean_full, bottle_seacarb)



#Prioritize these seacarb calculations, eliminate others
full <- full %>% 
  mutate(c_fCO2_uatm = full_seacarb$c_fCO2_uatm) %>% 
  mutate(c_pCO2_uatm = full_seacarb$c_pCO2_uatm) %>% 
  mutate(c_pH_total = full_seacarb$c_pH_total) %>% 
  mutate(c_CO2_umolkg = full_seacarb$c_CO2_umolkg) %>% 
  mutate(c_HCO3_umolkg = full_seacarb$c_HCO3_umolkg) %>% 
  mutate(c_CO3_umolkg = full_seacarb$c_CO3_umolkg) %>% 
  mutate(c_tCO2_umolkg = full_seacarb$c_tCO2_umolkg) %>% 
  mutate(c_TA_umolkg = full_seacarb$c_TA_umolkg) %>% 
  mutate(c_omega_ar = full_seacarb$c_omega_ar) %>% 
  mutate(c_omega_ca = full_seacarb$c_omega_ca) %>% 
  mutate(c_revelle = full_seacarb$c_revelle) %>% 
  dplyr::select(nms)


summary(full)

#Export the full dataset with seacarb calcs. Also copy to the dataset area
#write_csv(full, file = "full_btl_seacarb.csv")
save(full, file = "full_btl_seacarb.Rdata")



# 8. Doing seacarb on ALL the data possible -------------------------------------------
nms <- colnames(full)

clean_full <- full %>% 
  mutate(t_C = ifelse(t_flag == 1, t_C, NA)) %>% 
  mutate(sal_pss = ifelse(sal_flag == 1, sal_pss, NA)) %>% 
  mutate(do_umolkg = ifelse(do_flag == 1, do_umolkg, NA)) %>% 
  mutate(pH_total = ifelse(pH_flag == 1, pH_total, NA)) %>% 
  mutate(tCO2_umolkg = ifelse(tCO2_flag == 1, tCO2_umolkg, NA)) %>% 
  mutate(ta_umolkg = ifelse(ta_flag == 1, ta_umolkg, NA)) %>% 
  dplyr::select(nms[1:39])


#Reduce down to cases that could be calculated with seacarb:
clean_seacarb0 <- clean_full %>% 
  filter(!is.na(t_C) & !is.na(sal_pss) & !is.na(depth_m)) %>% 
  filter((!is.na(ta_umolkg) & !is.na(tCO2_umolkg)) | (!is.na(ta_umolkg) & !is.na(pH_total)) | (!is.na(tCO2_umolkg) & !is.na(pH_total)))


clean_seacarb <- clean_seacarb0 %>% 
  mutate(seacarb_flag = case_when(!is.na(ta_umolkg) & !is.na(tCO2_umolkg) ~ 15,
                                  !is.na(ta_umolkg) & !is.na(pH_total) ~ 8,
                                  !is.na(tCO2_umolkg) & !is.na(pH_total) ~ 9)) %>% 
  mutate(ta_seacarb = ta_umolkg/10^6) %>% 
  mutate(tCO2_seacarb = tCO2_umolkg/10^6) %>% 
  mutate(pressure = depth_m/10) %>% 
  mutate(po4 = ifelse(is.na(po3_umolkg), 0, po3_umolkg/10^6)) %>% 
  mutate(si = ifelse(is.na(si_umolkg), 0, si_umolkg/10^6)) %>% 
  mutate(var1 = case_when(seacarb_flag == 15 ~ ta_seacarb,
                          seacarb_flag == 8 ~ pH_total,
                          seacarb_flag == 9 ~ pH_total)) %>% 
  mutate(var2 = case_when(seacarb_flag == 15 ~ tCO2_seacarb,
                          seacarb_flag == 8 ~ ta_seacarb,
                          seacarb_flag == 9 ~ tCO2_seacarb))

summary(clean_seacarb) # It worked! no NAs!

allcalcs <- carb(flag = clean_seacarb$seacarb_flag, var1 = clean_seacarb$var1,
                 var2 = clean_seacarb$var2, S = clean_seacarb$sal_pss, T = clean_seacarb$t_C,
                 Patm = 1, P = clean_seacarb$pressure, Pt = clean_seacarb$po4, Sit = clean_seacarb$si,
                 k1k2 = "x", kf = "x", ks = "d", pHscale = "T", b = "u74")



#Fix the units and get the appropriate columns


clean_seacarb <- clean_seacarb %>% 
  mutate(c_fCO2_uatm = allcalcs$fCO2) %>% 
  mutate(c_pCO2_uatm = allcalcs$pCO2) %>% 
  mutate(c_pH_total = allcalcs$pH) %>% 
  mutate(c_CO2_umolkg = allcalcs$CO2 * 10^6) %>% 
  mutate(c_HCO3_umolkg = allcalcs$HCO3 * 10^6) %>% 
  mutate(c_CO3_umolkg = allcalcs$CO3 * 10^6) %>% 
  mutate(c_tCO2_umolkg = allcalcs$DIC * 10^6) %>% 
  mutate(c_TA_umolkg = allcalcs$ALK * 10^6) %>% 
  mutate(c_omega_ar = allcalcs$OmegaAragonite) %>% 
  mutate(c_omega_ca = allcalcs$OmegaCalcite) %>% 
  mutate(c_revelle = NA) %>% 
  dplyr::select(nms)

#Put stuff back into the big dataframe
clean_full2 <- left_join(clean_full, clean_seacarb)

full_seacarb <- full %>% 
  mutate(c_fCO2_uatm = clean_full2$c_fCO2_uatm) %>% 
  mutate(c_pCO2_uatm = clean_full2$c_pCO2_uatm) %>% 
  mutate(c_pH_total = clean_full2$c_pH_total) %>% 
  mutate(c_CO2_umolkg = clean_full2$c_CO2_umolkg) %>% 
  mutate(c_HCO3_umolkg = clean_full2$c_HCO3_umolkg) %>% 
  mutate(c_CO3_umolkg = clean_full2$c_CO3_umolkg) %>% 
  mutate(c_tCO2_umolkg = clean_full2$c_tCO2_umolkg) %>% 
  mutate(c_TA_umolkg = clean_full2$c_TA_umolkg) %>% 
  mutate(c_omega_ar = clean_full2$c_omega_ar) %>% 
  mutate(c_omega_ca = clean_full2$c_omega_ca) %>% 
  dplyr::select(nms)

summary(full_seacarb)

## Save the FULL FULL seacarb dataframe
save(full_seacarb, file = "full_seacarb_ALL.Rdata")

