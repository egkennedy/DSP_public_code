library("sp")
library("raster")
library("rgdal")
library("ggsn")
library("rworldmap")
library("rgeos")
library("sf")
library("tidyverse")
library("viridis")
library("RColorBrewer")
library("FSA")
library("car")
library("cowplot")
library("plotly")

#wcoast <- subset(world2, world$NAME %in% c("Canada", "United States", "Mexico"))
usa <- st_as_sf(maps::map("state", fill=TRUE, plot =FALSE))
wcoast <- usa %>% 
  filter(ID %in% c("california", "oregon", "washington", "nevada", "idaho"))
cal <- wcoast %>% filter(ID == "california")

countries <- st_as_sf(maps::map("world", fill = TRUE, plot = FALSE))
countries <- countries %>% filter(ID %in% c("Canada", "USA", "Mexico"))

#General style for all maps
plot_style <- list(geom_sf(data = wcoast, fill = "white", color = "gray"),
                   geom_sf(data = countries, fill = "transparent"),
                           theme_void(),  # Remove ugly grey background
                   theme(axis.text.x = element_blank(),
                         plot.title = element_text(hjust = 0.5)),
                   guides(alpha = "none"),
                   xlim(-153, -115), ylim(32, 50))

theme_set(theme_bw())

load("clean_all_light.Rdata") # For practice plots
load("full_nd.Rdata") # For percent "good"


# Really clean up the "light" dataset
light <- light %>% 
  mutate(do_sat = ifelse(do_sat < 0, 0, do_sat)) %>% 
  mutate(do_umolkg = ifelse(do_umolkg < 0, 0, do_umolkg)) %>% 
  mutate(no3_umolkg = ifelse(no3_umolkg < 0 , 0, no3_umolkg)) %>% 
  mutate(chl_ugL = ifelse(chl_ugL < 0 , 0, chl_ugL))




#Folder for figs
figpath <- "DSP_paper_figs/actual_paper_figs/"


## Make a function to print nice figs at high res! Then I can update the res whenever I need to
figurefun <- function(figure, filename, height, width){
  png(filename = paste0(figpath, filename), res = 300, height = height, width = width, units = "in")
  print(figure)
  dev.off()
}


# How much overall data? --------------------------------------------------

# Total number of observations
totalobs <- full %>%
  select(t_C, sal_pss, do_flag, pCO2_uatm, pH_total, tCO2_umolkg, ta_umolkg, no2_umolkg, no3_umolkg,
         po3_umolkg, si_umolkg, nh4_umolkg, chl_ugL)

sum(!is.na(totalobs))

totalobs <- totalobs %>% 
  summarize(t = sum(!is.na(t_C)), sal = sum(!is.na(sal_pss)), do = sum(!is.na(do_flag)),
            pCO2 = sum(!is.na(pCO2_uatm)), pH = sum(!is.na(pH_total)), DIC = sum(!is.na(tCO2_umolkg)),
            ta = sum(!is.na(ta_umolkg)), chl = sum(!is.na(chl_ugL)), 
            nutr = sum(!is.na(no2_umolkg)) + sum(!is.na(no3_umolkg)) + sum(!is.na(po3_umolkg))
            + sum(!is.na(si_umolkg)) + sum(!is.na(nh4_umolkg)))
totalobs

sum(totalobs)
rm(totalobs)


# Total number of DAILY observations
totalobs <- light %>%
  select(t_C, sal_pss, do_flag, pCO2_uatm, pH_total, tCO2_umolkg, ta_umolkg, no2_umolkg, no3_umolkg,
         po3_umolkg, si_umolkg, nh4_umolkg, chl_ugL, pH_seacarb)

sum(!is.na(totalobs))

totalobs <- totalobs %>% 
  summarize(t = sum(!is.na(t_C)), sal = sum(!is.na(sal_pss)), do = sum(!is.na(do_flag)),
            pCO2 = sum(!is.na(pCO2_uatm)), pH = sum(!is.na(pH_total)), DIC = sum(!is.na(tCO2_umolkg)),
            ta = sum(!is.na(ta_umolkg)), chl = sum(!is.na(chl_ugL)), 
            nutr = sum(!is.na(no2_umolkg)) + sum(!is.na(no3_umolkg)) + sum(!is.na(po3_umolkg))
            + sum(!is.na(si_umolkg)) + sum(!is.na(nh4_umolkg)))
totalobs

sum(totalobs)
rm(totalobs)

#seacarb calcs
light %>% 
  mutate(pHseacarb = ifelse(is.na(pH_total) & !is.na(c_pH_total), 1, NA)) %>% 
  mutate(dicseacarb = ifelse(is.na(tCO2_umolkg) & is.na(pHseacarb) & !is.na(c_tCO2_umolkg), 1, NA)) %>% 
  mutate(taseacarb = ifelse(is.na(ta_umolkg) & is.na(pHseacarb) & is.na(dicseacarb) & !is.na(c_TA_umolkg), 1, NA)) %>% 
  mutate(pco2seacarb = ifelse(is.na(pCO2_uatm) & is.na(taseacarb) & is.na(dicseacarb) & is.na(pHseacarb) & !is.na(c_pCO2_uatm), 1, NA)) %>% 
  dplyr::select(pHseacarb, dicseacarb, taseacarb, pco2seacarb) %>% 
  pivot_longer(cols = everything(), names_to = "vars", values_to = "present") %>% 
  filter(!is.na(present)) %>% nrow()

# Data locations figure ---------------------------------------------------
viridis(6)

# Raw data locations
locations <- light %>% 
  filter(depth_m < 25) %>% 
  mutate(pH = ifelse(!is.na(pH_total) | !is.na(c_pH_total), 1, NA)) %>%
  mutate(DO = ifelse(!is.na(do_umolkg) | !is.na(do_sat), 1, NA)) %>% 
  rename(Temperature = t_C) %>% 
  mutate(year = lubridate::year(time_utc)) %>% 
  dplyr::select(latitude, longitude, year, Temperature, DO, pH) %>% 
  group_by(latitude, longitude, year) %>% 
  summarize(Temperature = sum(!is.na(Temperature)), DO = sum(!is.na(DO)), pH = sum(!is.na(pH))) %>% 
  ungroup()



fig1 <- locations %>% 
 # dplyr::select(-c(DO, pH)) %>% 
  filter(Temperature > 0) %>% 
  #group_by(latitude, longitude) %>% 
  #summarize(Temperature = n_distinct(year)) %>% 
  ggplot()+
  geom_point(aes(x = longitude, y = latitude), color = "#440154FF", size = 0.4)+
  plot_style+
  labs(title = "Temperature")+
  scale_color_viridis()+
  theme(legend.position = "none")
fig1
figurefun(fig1, "temp_data_locations.png", height = 3, width = 4)


fig1 <- locations %>% 
  filter(DO > 0) %>% 
  ggplot()+
  geom_point(aes(x = longitude, y = latitude), color = "#2A788EFF", size = 0.4)+
  plot_style+
  labs(title = "Dissolved Oxygen")+
  theme(legend.position = "none")
fig1
figurefun(fig1, "oxy_data_locations.png", height = 3, width = 4)


fig1 <- locations %>% 
  filter(pH > 0) %>% 
  ggplot()+
  geom_point(aes(x = longitude, y = latitude), color = "#22A884FF", size = 0.4)+
  plot_style+
  labs(title = "pH")+
  theme(legend.position = "none")
fig1
figurefun(fig1, "pH_data_locations.png", height = 3, width = 4)


# QA/QC Figure ------------------------------------------------------------

fake_data <- full %>% 
  filter(latitude < 38.3 & latitude < 38) %>% 
  filter(depth_m < 25) %>% 
  filter(distance_offshore < 50)

fake_data <- fake_data %>% 
  filter(time_utc > "2010-01-01") %>% 
  mutate(depth = plyr::round_any(depth_m, 5)) %>% 
  mutate(latitude = plyr::round_any(latitude, 0.05)) %>% 
  mutate(date = lubridate::date(time_utc)) %>% 
  group_by(depth, latitude, date) %>% 
  summarize(t_C = mean(t_C, na.rm = TRUE), sal_pss = mean(sal_pss, na.rm = TRUE), 
            do_umolkg = mean(do_umolkg, na.rm = TRUE), pH_total = mean(pH_total, na.rm = TRUE),
            ta_umolkg = mean(ta_umolkg, na.rm = TRUE), t_flag = 2, sal_flag = 2, do_flag = 2,
            pH_flag = 2, ta_flag = 2)


fake <- fake_data %>% 
  filter(!(sal_pss < 20 & !is.na(do_umolkg))) %>% 
  filter(sal_pss > 5) %>% 
  filter(t_C < 30) %>% 
  filter(do_umolkg > 0) %>% 
  filter(!(t_C > 19 & sal_pss < 32)) %>% 
  filter(sal_pss < 34.3) %>% 
  filter(t_C < 30) %>% 
  filter(!(sal_pss > 34 & date < "2013-01-01")) %>% 
  filter(!(sal_pss > 34 & t_C > 20))

fake <- fake %>% 
  mutate(t_C = ifelse(t_C > 21.5 & date > "2019-01-01", t_C + 1.5, t_C))

# Property property plots?
fig <- fake %>% 
  mutate(year = lubridate::year(date)) %>% 
  filter(sal_pss > 28) %>% 
  ggplot()+
  geom_point(aes(x = sal_pss, y = t_C, color = year), alpha = 0.3)+
  labs(y = "Temperature", x = "Salinity", color = "Year")+
  scale_color_viridis(option = "plasma")
fig
figurefun(fig, "T_S_propprop_example.png", height = 3, width = 4)



#Possible flags
fake <- fake %>% 
  mutate(potential_flag = case_when(t_C > 23.4 ~ "yes",
                                    TRUE ~ "no"))


# Time series
fig <- fake %>%
  ggplot()+
  geom_point(aes(x = date, y = t_C, color = potential_flag), alpha = 0.5)+
  scale_color_manual(values = c("#440154FF", "red"))+
  labs(x = "Time", y = "Temperature", title = "Temperature Time Series")+
  theme(legend.position = "none")
fig
figurefun(fig, "T_TS_example.png", height = 3, width = 4)

  

fig <- fake %>% 
  mutate(potential_flag = factor(potential_flag)) %>% 
  filter(sal_pss > 32) %>% 
  filter(date > "2019-01-01" & date < "2020-01-01") %>% 
  ggplot()+
  geom_point(aes(x = date, y = sal_pss, color = potential_flag), alpha = 0.5)+
  scale_color_manual(values = c("#414487FF", "red"))+
  labs(x = "Time", y = "Salinity", title = "Salinity Time Series")+
  theme(legend.position = "none")
fig
figurefun(fig, "Sal_TS_example.png", height = 3, width = 4)



fake <- fake %>% 
  mutate(t_flag = ifelse(t_C > 23.4, 3, 1)) %>% 
  mutate(sal_flag = ifelse(sal_pss > 34, 3, 1))


fake %>% 
  filter(do_umolkg > 50 & do_umolkg < 450) %>% 
  ggplot()+
  geom_point(aes(x = t_C, y = do_umolkg, color = t_flag))

fake %>% 
  filter(do_umolkg > 50 & do_umolkg < 450) %>% 
  ggplot()+
  geom_point(aes(x = date, y = do_umolkg, color = t_flag))

fake %>% 
  filter(!is.na(pH_total)) %>% 
  filter(do_umolkg > 50 & do_umolkg < 450) %>% 
  ggplot()+
  geom_point(aes(x = t_C, y = do_umolkg, color = pH_total))

fake %>% 
  filter(do_umolkg > 50 & do_umolkg < 450) %>% 
  ggplot()+
  geom_point(aes(x = do_umolkg, y = pH_total, color = t_flag))



# NOT IN PAPER: Daily Data Type Distribution --------------------------------------------
#Check for the full datasets
full %>%
  rename(DO = do_type, pH = pH_type, DIC = tCO2_type, TA = ta_type) %>% 
  dplyr::select(DO, pH, DIC, TA) %>% 
  pivot_longer(cols = everything(), names_to = "Variable", values_to = "Type") %>% 
  filter(!is.na(Type)) %>% 
  mutate(Type = factor(Type, levels = c("bottle", "autonomous sensor", "handheld field sensor"))) %>% 
  group_by(Variable) %>% 
  mutate(totals = n()) %>% 
  ungroup() %>% 
  group_by(Variable, Type) %>% 
  summarize(Percent = n()/mean(totals), Totals = n()) %>% 
  ungroup() %>%
  group_by(Type) %>% 
  mutate(type_total = sum(Totals))

datatype <- light %>%
  rename(DO = do_type, pH = pH_type, DIC = tCO2_type, TA = ta_type) %>% 
  dplyr::select(DO, pH, DIC, TA) %>% 
  pivot_longer(cols = everything(), names_to = "Variable", values_to = "Type") %>% 
  filter(!is.na(Type)) %>% 
  mutate(Type = factor(Type, levels = c("discrete", "autonomous sensor", "CTD", "handheld sensor"))) %>% 
  group_by(Variable) %>% 
  mutate(totals = n()) %>% 
  ungroup() %>% 
  group_by(Variable, Type) %>% 
  summarize(Percent = n()/mean(totals), Totals = n()) %>% 
  ungroup() %>% 
  mutate(Variable = factor(Variable, levels = c("DO", "pH", "TA", "DIC"))) 

library("ggbreak")
library("patchwork")
# Also most of our "unknown" data types are DOs Probably sensors.
fig <- datatype %>% 
  ggplot()+
  geom_col(aes(x = Variable, y = Totals, fill = Type), position = "stack") +
  scale_fill_brewer(palette = "RdYlBu", direction = -1)+
  labs(title = "Number of Msmts By Type")
fig
figurefun(fig, "number_datatypes_all.png", height = 3, width = 4)

#Break the blot at 600,000 obs
fig <- datatype %>% 
  ggplot(aes(x = Variable, y = Totals, fill = Type)) + 
  geom_col(orientation="x")+
  scale_y_break(c(150000, 650000), ticklabels=c(50000, 100000, 700000, 800000))+
  scale_fill_brewer(palette = "RdYlBu", direction = -1)+
  labs(title = "Number of Msmts By Type")
fig
figurefun(fig, "number_datatypes_break.png", height = 3, width = 4)


datatype


extracarb <- light %>% 
  mutate(carb_extra = case_when(is.na(pH_total) & !is.na(c_pH_total) ~ 1,
                                is.na(ta_umolkg) & !is.na(c_TA_umolkg) ~ 1,
                                is.na(tCO2_umolkg) & !is.na(c_tCO2_umolkg) ~ 1,
                                is.na(pCO2_uatm) & !is.na(c_pCO2_uatm) ~ 1,
                                TRUE ~ 0))
extracarb <- sum(extracarb$carb_extra)
extracarb

obscarb <- sum(!is.na(light$pH_total))+
  sum(!is.na(light$ta_umolkg))+
  sum(!is.na(light$tCO2_umolkg))+
  sum(!is.na(light$pCO2_uatm))

extracarb/obscarb


# Flagging by dataset and parameter ---------------------------------------
#How much flagging overall?
flag_report <- full %>% 
  mutate(pCO2_flag = ifelse(is.na(pCO2_uatm), NA, pCO2_flag)) %>% 
  select(t_flag, sal_flag, pH_flag, ta_flag, do_flag, tCO2_flag, pCO2_flag) %>% 
  pivot_longer(cols = everything(), names_to = "vars", values_to = "flag") %>% 
  summarize(good = sum(flag == 1 & !is.na(flag)), bad = sum(flag != 1 & !is.na(flag)))

flag_report <- flag_report %>% 
  mutate(percent_good = good/(good+bad))
flag_report
write_csv(flag_report, file = "overall_flag_report.csv")

flagds <- full %>% 
  group_by(dataset_id) %>% 
  summarize(Temperature = sum(t_flag == 1, na.rm = TRUE)/sum(!is.na(t_flag)),
            temp_no = sum(!is.na(t_C)), 
            Salinity = sum(sal_flag == 1, na.rm = TRUE)/sum(!is.na(sal_flag)),
            sal_no = sum(!is.na(sal_pss)),
            DO = sum(do_flag == 1, na.rm = TRUE)/sum(!is.na(do_flag)),
            do_no = sum(!is.na(do_umolkg) | !is.na(do_sat)), 
            pH = sum(pH_flag == 1, na.rm = TRUE)/sum(!is.na(pH_flag)),
            pH_no = sum(!is.na(pH_total)),
            TA = sum(ta_flag == 1, na.rm = TRUE)/sum(!is.na(ta_flag)),
            ta_no = sum(!is.na(ta_umolkg)),
            DIC = sum(tCO2_flag == 1, na.rm = TRUE)/sum(!is.na(tCO2_flag)),
            dic_no = sum(!is.na(tCO2_umolkg))) %>% 
  pivot_longer(cols = -c(dataset_id, temp_no, sal_no, do_no, pH_no, ta_no, dic_no), names_to = "variable", values_to = "percent_good")

flagds <- flagds %>% 
  mutate(drop = case_when(variable == "Temperature" & temp_no == 0 ~ "drop",
                          variable == "Salinity" & sal_no == 0 ~ "drop",
                          variable == "DO" & do_no == 0 ~ "drop",
                          variable == "pH" & pH_no == 0 ~ "drop",
                          variable == "TA" & ta_no == 0 ~ "drop",
                          variable == "DIC" & dic_no == 0 ~ "drop",
                          TRUE ~ "keep")) %>% 
  filter(drop != "drop")

write_csv(flagds, "datasets_to_check.csv")

## NOT IN PAPER
fig <- flagds %>% 
  ggplot()+
  geom_jitter(aes(y = percent_good, x = dataset_id, color = variable))+
  scale_color_viridis_d(option = "plasma")+
  theme_minimal()+
  facet_wrap(~variable)
fig
figurefun(fig, "percent_good_by_DS.png", height = 3, width = 4)


## NOT IN PAPER
fig <- flagds %>%  
  filter(percent_good < 0.99) %>% 
  filter(percent_good > 0.5) %>% 
  mutate(dataset_id = factor(dataset_id)) %>% 
  mutate(variable = factor(variable, levels = c("Temperature", "Salinity", "DO", "pH", "TA", "DIC"))) %>% 
  mutate(percent_bad = (1-percent_good)*100) %>% 
  ggplot()+
  geom_jitter(aes(x = dataset_id, y = percent_bad, color = variable))+
  facet_grid(rows = vars(variable))+
  scale_color_viridis_d(option = "turbo")+
  theme_minimal()+
  labs(y = "Percent Unreliable", x = "Dataset ID", color = "Parameter")
fig
figurefun(fig, "percent_bad_by_DS_filtered1.png", height = 4, width = 4)


flagtable <- flagds %>% 
  filter(!is.na(percent_good)) %>% 
  mutate(percent_bad = (1-percent_good) * 100) %>% 
  mutate(flag_bin = case_when(percent_bad < 5 ~ "< 5%",
                              percent_bad <= 10 ~ "< 10%",
                              percent_bad <= 15 ~ "< 15%",
                              percent_bad <= 20 ~ "< 20%",
                              percent_bad <= 50 ~ "< 50%",
                              TRUE ~ "> 50%")) %>% 
  mutate(flag_bin = factor(flag_bin, levels = c("< 5%", "< 10%", "< 15%", "< 20%", "< 50%", "> 50%"))) %>% 
  group_by(variable, flag_bin) %>% 
  summarize(datasets = n())
#Can use flagds to make a table now

fig <- flagtable %>%
  ungroup() %>% 
  mutate(variable = factor(variable, levels = c("Temperature", "Salinity", "DO", "pH", "TA", "DIC"))) %>%   ggplot()+
  geom_col(aes(x = flag_bin, y = datasets, fill = variable), position = position_dodge2(width = 0.9, preserve = "single"))+
  scale_fill_viridis_d()+
  labs(x = "Maximum Flag Rate", y = "Number of Datasets", fill = "Parameter")
fig
figurefun(fig, "flag_rates_by_DS_summary.png", height = 4, width = 4)



fig <- flagtable %>% 
  group_by(variable) %>% 
  mutate(total_datasets = sum(datasets)) %>% 
  ungroup() %>% 
  mutate(percent = datasets/total_datasets) %>% 
  mutate(variable = factor(variable, levels = c("Temperature", "Salinity", "DO", "pH", "TA", "DIC"))) %>% 
  ggplot()+
  geom_col(aes(x = flag_bin, y = percent, fill = variable), position = position_dodge2(width = 0.9, preserve = "single"))+
  scale_fill_viridis_d()+
  labs(x = "Maximum Flag Rate", y = "Percentage of Datasets", fill = "Parameter")
fig
figurefun(fig, "flag_rates_by_DS_summary_percent.png", height = 4, width = 4)

  

# Make a table by dataset and type:
qa_table <- full %>% 
  group_by(dataset_id) %>% 
  summarize(temp = sum(t_flag == 1, na.rm = TRUE)/sum(!is.na(t_flag)),
            sal = sum(sal_flag == 1, na.rm = TRUE)/sum(!is.na(sal_flag)),
            do = sum(do_flag == 1, na.rm = TRUE)/sum(!is.na(do_flag)),
            pH = sum(pH_flag == 1, na.rm = TRUE)/sum(!is.na(pH_flag)),
            TA = sum(ta_flag == 1, na.rm = TRUE)/sum(!is.na(ta_flag)),
            DIC = sum(tCO2_flag == 1, na.rm = TRUE)/sum(!is.na(tCO2_flag)))
qa_table
write_csv(qa_table, file = paste0(figpath, "percent_good_by_ds.csv"))


# Flagging rates by method ------------------------------------------------

# Make DF
#Handle vars separately
pHtype <- full %>% 
  dplyr::select(pH_type, pH_flag) %>% 
  filter(!is.na(pH_type)) %>% 
  filter(pH_type != "seacarb") %>% 
  group_by(pH_type) %>% 
  summarize(percent_bad = 1-sum(pH_flag == 1, na.rm = TRUE)/sum(!is.na(pH_flag))) %>% 
  rename(Type = pH_type) %>% 
  mutate(Variable = "pH")

DOtype <- full %>% 
  dplyr::select(do_type, do_flag) %>% 
  filter(!is.na(do_type)) %>% 
  group_by(do_type) %>% 
  summarize(percent_bad = 1-sum(do_flag == 1, na.rm = TRUE)/sum(!is.na(do_flag))) %>% 
  rename(Type = do_type) %>% 
  mutate(Variable = "DO")

TAtype <- full %>% 
  filter(!is.na(ta_umolkg)) %>% 
  dplyr::select(ta_type, ta_flag) %>% 
  summarize(percent_bad = 1-sum(ta_flag == 1, na.rm = TRUE)/sum(!is.na(ta_flag))) %>% 
  mutate(Type = "discrete") %>% 
  mutate(Variable = "TA")

DICtype <- full %>% 
  filter(!is.na(tCO2_umolkg)) %>% 
  dplyr::select(tCO2_type, tCO2_flag) %>% 
  filter(tCO2_type != "seacarb" & tCO2_type != "calculated") %>% 
  group_by(tCO2_type) %>% 
  summarize(percent_bad = 1-sum(tCO2_flag == 1, na.rm = TRUE)/sum(!is.na(tCO2_flag))) %>% 
  rename(Type = tCO2_type) %>%
  mutate(Variable = "DIC")

#Put it all back together
typeflag <- rbind(DOtype, pHtype, TAtype, DICtype)

# Get the overall percentage too (raw measurements only that we can't replace)
overall <- full %>% 
  summarize(DO = 1-sum(do_flag == 1, na.rm = TRUE)/sum(!is.na(do_flag)),
            pH = 1-sum(pH_flag == 1, na.rm = TRUE)/sum(!is.na(pH_flag)),
            TA = 1-sum(ta_flag == 1, na.rm = TRUE)/sum(!is.na(ta_flag)),
            DIC = 1-sum(tCO2_flag == 1, na.rm = TRUE)/sum(!is.na(tCO2_flag))) %>% 
  pivot_longer(cols = everything(), names_to = "Variable", values_to = "overall") 

typeflag <- left_join(typeflag, overall)

fig <- typeflag %>% 
  filter(!is.na(Type)) %>% 
  mutate(Type = factor(Type, levels = c("discrete", "autonomous sensor", "CTD", "handheld sensor"))) %>% 
  mutate(percent_bad = percent_bad * 100) %>% 
  ggplot()+
  geom_point(aes(x = Variable, y = percent_bad, color = Type), size = 4)+
  labs(x = "Variable", title = "Percent of Raw Data Flagged `Bad`",
       y = "Percent")+
  scale_color_brewer(palette = "RdYlBu", direction = -1)+
  ylim(0, 12.5)
fig
figurefun(fig, "percent_bad_by_var_by_type.png", height = 3, width = 4)

# Better as a table?
typeflag

# Spatiotemporal data distribution ----------------------------------------

# Check this later...
#What percentage of obs are nearshore?
nearshore <- light %>% 
  mutate(ta_umolkg = ifelse(ta_type == "seacarb" | ta_type == "calculated", NA, ta_umolkg)) %>% 
  mutate(tCO2_umolkg = ifelse(tCO2_type == "seacarb" | tCO2_type == "calculated", NA, tCO2_umolkg)) %>% 
  mutate(pCO2_uatm = ifelse(pCO2_type == "seacarb" | pCO2_type == "calculated", NA, pCO2_uatm)) %>% 
  mutate(do_all = ifelse(is.na(do_umolkg), do_sat, do_umolkg)) %>% 
  select(distance_offshore, t_C, sal_pss, do_all, pH_total, ta_umolkg, tCO2_umolkg, pCO2_uatm) %>% 
  pivot_longer(-distance_offshore, names_to = "variable", values_to = "obs") %>% 
  filter(!is.na(obs))


nrow(subset(nearshore, distance_offshore < 100))/nrow(nearshore)
nrow(subset(nearshore, distance_offshore < 300))/nrow(nearshore)


depth <- light %>% 
  mutate(pCO2_uatm = ifelse(pCO2_type == "calculated", NA, pCO2_uatm)) %>% 
  mutate(do_all = ifelse(is.na(do_umolkg), do_sat, do_umolkg)) %>% 
  select(depth_m, t_C, sal_pss, do_all, pH_total, ta_umolkg, tCO2_umolkg, pCO2_uatm) %>% 
  pivot_longer(-depth_m, names_to = "variable", values_to = "obs") %>% 
  filter(!is.na(obs))

nrow(subset(depth, depth_m < 50))/nrow(depth)
nrow(subset(depth, depth_m < 100))/nrow(depth)


years <- light %>%
  mutate(year = lubridate::year(time_utc)) %>% 
  mutate(pCO2_uatm = ifelse(pCO2_type == "calculated", NA, pCO2_uatm)) %>% 
  mutate(do_all = ifelse(is.na(do_umolkg), do_sat, do_umolkg)) %>% 
  select(year, t_C, sal_pss, do_all, pH_total, ta_umolkg, tCO2_umolkg, pCO2_uatm) %>% 
  pivot_longer(-year, names_to = "variable", values_to = "obs") %>% 
  filter(!is.na(obs))

nrow(subset(years, year > 1990))/nrow(years)
nrow(subset(depth, depth_m < 100))/nrow(depth)



#Percent data by month
# How much data by month?
monthly <- light %>% 
  mutate(month = lubridate::month(time_utc)) %>% 
  mutate(ta_umolkg = ifelse(ta_type == "calculated", NA, ta_umolkg)) %>% 
  mutate(tCO2_umolkg = ifelse(tCO2_type == "calculated", NA, tCO2_umolkg)) %>% 
  mutate(pCO2_uatm = ifelse(pCO2_type == "calculated", NA, pCO2_uatm)) %>% 
  mutate(do_all = ifelse(is.na(do_umolkg), do_sat, do_umolkg)) %>% 
  group_by(month) %>% 
  summarize(Temperature = sum(!is.na(t_C)), Salinity = sum(!is.na(sal_pss)), DO = sum(!is.na(do_all)),
            pCO2 = sum(!is.na(pCO2_uatm)), pH = sum(!is.na(pH_total)), DIC = sum(!is.na(tCO2_umolkg)),
            TA = sum(!is.na(ta_umolkg)), chl = sum(!is.na(chl_ugL)), 
            nutr = sum(!is.na(no2_umolkg)) + sum(!is.na(no3_umolkg)) + sum(!is.na(po4_umolkg))
            + sum(!is.na(si_umolkg)) + sum(!is.na(nh4_umolkg))) %>% 
  ungroup() %>% 
  pivot_longer(cols = -month, names_to = "Variables", values_to = "Totals")

#Monthly bias as percentages
fig <- monthly %>%
  filter(!is.na(month)) %>% 
  mutate(Variables = factor(Variables, levels = c("Temperature", "Salinity", "DO", "pH", "TA", "DIC"))) %>% 
  ungroup() %>% 
  mutate(month = factor(month)) %>% 
  group_by(Variables) %>% 
  mutate(overalltotal = sum(Totals)) %>% 
  mutate(Percent = Totals/overalltotal*100) %>% 
  filter(Variables %in% c("Temperature", "Salinity", "DO")) %>% 
  ggplot()+
  geom_col(aes(x = month, y = Percent, fill = Variables), position = position_dodge(0.95), width = 0.8, alpha = 0.8)+
  geom_hline(yintercept = 8.33, linetype = "dotted")+
  scale_fill_manual(values = c("#440154FF", "#414487FF", "#2A788EFF"))+
  labs(y = "Percent Observations", x = "Month")
fig
figurefun(fig, "daily_data_percent_bymonth_TSO.png", height = 3, width = 4)


fig <- monthly %>%
  mutate(Variables = factor(Variables, levels = c("Temperature", "Salinity", "DO", "pH", "TA", "DIC"))) %>% 
  filter(!is.na(month)) %>% 
  ungroup() %>% 
  mutate(month = factor(month)) %>% 
  group_by(Variables) %>% 
  mutate(overalltotal = sum(Totals)) %>% 
  mutate(Percent = Totals/overalltotal*100) %>% 
  filter(Variables %in% c("pH", "DIC", "TA")) %>% 
  ggplot()+
  geom_col(aes(x = month, y = Percent, fill = Variables), position = position_dodge(0.95), width = 0.8)+
  geom_hline(yintercept = 8.33, linetype = "dotted")+
  scale_fill_manual(values = c("#22A884FF", "#7AD151FF", "#FDE725FF"))+
  labs(y = "Percent Observations", x = "Month")
fig
figurefun(fig, "daily_data_percent_bymonth_carb.png", height = 3, width = 4)


#ALL OF THEM
fig <- monthly %>%
  filter(!is.na(month)) %>% 
  mutate(Variables = factor(Variables, levels = c("Temperature", "Salinity", "DO", "pH", "TA", "DIC"))) %>% 
  filter(!is.na(Variables)) %>% 
  ungroup() %>% 
  mutate(month = factor(month)) %>% 
  group_by(Variables) %>% 
  mutate(overalltotal = sum(Totals)) %>% 
  mutate(Percent = Totals/overalltotal*100) %>% 
  ggplot()+
  geom_col(aes(x = month, y = Percent, fill = Variables), position = "dodge", width = 0.8)+
  geom_hline(yintercept = 8.33, linetype = "dotted")+
  scale_fill_viridis_d()+
  labs(y = "Percent Observations", x = "Month", title = "Data Distribution By Month")
fig
figurefun(fig, "daily_data_percent_bymonth_all.png", height = 3, width = 4)



# Spatiotemporal HOVs -----------------------------------------------------

# Try a tiled heatmap
latzones <- seq(from = 32, to = 45.5, by = 0.5)

heatmap <- light %>% 
  mutate(pH_all = ifelse(is.na(pH_total), c_pH_total, pH_total)) %>% 
  mutate(anycarb = ifelse(!is.na(pH_total) | !is.na(ta_umolkg) | !is.na(tCO2_umolkg) | !is.na(pCO2_uatm), 1, NA)) %>% 
  filter(latitude > 32 & latitude < 48.5) %>% 
  filter(time_utc > "1985-01-01") %>% 
  filter(depth_m < 25) %>% 
  filter(distance_offshore < 50) %>% 
  mutate(year = lubridate::year(time_utc)) %>% 
  mutate(month = lubridate::month(time_utc)) %>% 
  mutate(month = case_when(month %in% c(1, 2) ~ 1,
                           month %in% c(3, 4) ~ 3,
                           month %in% c(5, 6) ~ 5,
                           month %in% c(7, 8) ~ 7,
                           month %in% c(9, 10) ~ 9,
                           TRUE ~ 11)) %>% 
  mutate(zone = rowSums(outer(latitude, latzones, ">"))) %>% 
  mutate(latitude = latzones[zone]) %>% 
  mutate(time = year+month/12) %>% 
  group_by(time, latitude) %>% 
  mutate(do_all = ifelse(is.na(do_umolkg), do_sat, do_umolkg)) %>% 
  summarize(temp = sum(!is.na(t_C)), DO = sum(!is.na(do_all)), pH = sum(!is.na(pH_all)),
            anycarb = sum(!is.na(anycarb))) %>% 
  filter(time < 2020)

# Need to rasterize the data to make this look good?
fig <- heatmap %>% 
  filter(DO != 0) %>% 
  ggplot()+
  geom_tile(aes(x = time, y = latitude, fill = DO))+
  scale_fill_distiller(palette = "Blues", direction = 1, trans = "log10")+
  labs(title = "Nearshore DO Density", x = "Time", y = "Latitude",
       fill = "# Obs")
fig
figurefun(fig, "density_DO_tile.png", height = 4, width = 4)


fig <- heatmap %>% 
  filter(temp != 0) %>% 
  ggplot()+
  geom_tile(aes(x = time, y = latitude, fill = temp))+
  scale_fill_distiller(palette = "Purples", direction = 1, trans = "log10")+
  labs(title = "Nearshore Temperature Density", x = "Time", y = "Latitude",
       fill = "# Obs")
fig
figurefun(fig, "density_temp_tile.png", height = 4, width = 4)


fig <- heatmap %>% 
  filter(pH != 0) %>% 
  ggplot()+
  geom_tile(aes(x = time, y = latitude, fill = pH))+
  scale_fill_distiller(palette = "Greens", direction = 1, trans = "log10")+
  labs(title = "Nearshore pH Density", x = "Time", y = "Latitude",
       fill = "# Obs")+
  xlim(1985, 2020)
fig
figurefun(fig, "density_pH_tile.png", height = 4, width = 4)


fig <- heatmap %>% 
  filter(pH > 0) %>% 
  ggplot()+
  geom_tile(aes(x = time, y = latitude, fill = pH))+
  scale_fill_distiller(palette = "OrRd", direction = 1, trans = "log10")+
  labs(title = "Surface Carb Coverage Through Time", x = "Time", y = "Latitude",
       fill = "# Obs")+
  xlim(1985, 2020)
fig
figurefun(fig, "density_carb_tile.png", height = 4, width = 4)


# Seasonal boxplots -------------------------------------------------------

seasonal <- light %>% 
  mutate(pH_all = ifelse(is.na(pH_total), c_pH_total, pH_total)) %>% 
  filter(depth_m < 25) %>% 
  filter(distance_offshore <= 50) %>% 
  mutate(month = lubridate::month(time_utc)) %>% 
  mutate(season = case_when(month %in% c(1:3) ~ "Jan-Mar",
                            month %in% c(4:6) ~ "Apr-Jun",
                            month %in% c(7:9) ~ "Jul-Sep",
                            TRUE ~ "Oct-Dec")) %>% 
  mutate(region = case_when(latitude >= 32.5 & latitude < 34.5 ~ "So. CA",
                            latitude >= 34.5 & latitude < 42 ~ "NorCen CA",
                            latitude >= 42 & latitude < 46.25 ~ "Oregon",
                            latitude >= 46.25 & latitude < 48.5 ~ "Washington",
                            TRUE ~ "International")) %>% 
  filter(region != "International") %>% 
  rename(temp = t_C, DO = do_umolkg, pH = pH_all, arag = c_omega_ar) %>% 
  dplyr::select(season, region, temp, DO, pH, arag) %>% 
  mutate(season = factor(season, levels = c("Jan-Mar", "Apr-Jun", "Jul-Sep", "Oct-Dec"))) %>% 
  mutate(region = factor(region, levels = c("Washington", "Oregon", "NorCen CA", "So. CA")))

## Make seasonal boxplots
seasonalbp <- seasonal %>% 
  pivot_longer(cols = -c(season, region), names_to = "Variable", values_to = "Values") %>% 
  filter(!is.na(Values)) 

var <- c("temp", "DO", "pH", "arag")
names <- c("Temperature", "DO", "pH", "Aragonite Sat.")


for(i in 1:4){
  filename <- paste0("difbox_seasonal_", var[i], ".png")
  
  fig <- seasonalbp %>% 
    filter(Variable == var[i]) %>% 
    ggplot()+
    geom_jitter(aes(x = season, y = Values, color = season), alpha = 0.2, size = 0.5)+
    geom_boxplot(aes(x = season, y = Values), outlier.alpha = 0, fill = "white", alpha = 0.5)+
    scale_color_brewer(palette = "RdBu")+
    labs(title = paste0("Seasonal Differences in ", names[i]), y = names[i], x = "Season")+
    theme(legend.position = "none")+
    facet_wrap(~region)
  figurefun(fig, filename, height = 4, width = 4)
}

# Property property plots -------------------------------------------------

#TA-S
sf_region <- light %>% 
  filter(distance_offshore < 100 & depth_m < 50) %>% 
  filter(latitude > 37.25 & latitude < 38.45) %>% 
  filter(sal_pss > 30 & sal_pss < 36 & ta_umolkg > 1600) %>% 
  filter(ta_type == "discrete")

offshore <- light %>%
  filter(depth_m < 50) %>% 
  filter(distance_offshore > 5 & distance_offshore < 100) %>% 
  filter(sal_pss > 30 & sal_pss < 36 & ta_umolkg > 1600) %>% 
  filter(ta_type == "discrete")

offshore <- anti_join(offshore, sf_region)

nearshore <- light %>% 
  filter(distance_offshore <= 5 & depth_m < 50) %>% 
  filter(sal_pss > 30 & sal_pss < 36 & ta_umolkg > 1600) %>% 
  filter(ta_type == "discrete")

nearshore <- anti_join(nearshore, sf_region)

#lms
offshore_lm <- lm(ta_umolkg~sal_pss, data = offshore)
summary(offshore_lm)
nearshore_lm <- lm(ta_umolkg~sal_pss, data = nearshore)
summary(nearshore_lm)
sf_lm <- lm(ta_umolkg~sal_pss, data = sf_region)
summary(sf_lm)


# How dependent are relationships on latitude? 
library("broom")
offshore <- offshore %>% 
  mutate(latzone = case_when(latitude < 34.5 ~ "socal",
                             latitude < 42 ~ "norcen cal",
                             latitude < 46.25 ~ "oregon",
                             TRUE ~ "washington")) %>% 
  group_by(latzone)

do(offshore,
   tidy(
     lm(ta_umolkg~sal_pss, data = .)
   ))

nearshore <- nearshore %>% 
  mutate(latzone = case_when(latitude < 34.5 ~ "socal",
                             latitude < 42 ~ "norcen cal",
                             latitude < 46.25 ~ "oregon",
                             TRUE ~ "washington")) %>% 
  group_by(latzone)

do(nearshore,
   glance(
     lm(ta_umolkg~sal_pss, data = .)
   ))

summary(lm(ta_umolkg ~ sal_pss, data = subset(nearshore, latitude > 40)))

fig <- light %>% 
  filter(distance_offshore < 5 | dataset_id == 26) %>% 
  filter(sal_pss > 30 & sal_pss < 36 & ta_umolkg > 1600) %>% 
  filter(ta_type == "discrete") %>% 
  ggplot()+
  geom_point(data = offshore, aes(x = sal_pss, y = ta_umolkg), color = "gray", alpha = 0.5)+
  #geom_point(data = sf_region, aes(x = sal_pss, y = ta_umolkg), color = "blue", alpha = 0.5)+
  geom_point(aes(x = sal_pss, y = ta_umolkg, color = latitude), alpha = 0.5)+
  scale_color_viridis(direction = -1)+
  labs(x = "Salinity", y = "TA", color = "Nearshore\nLatitude")
fig
figurefun(fig, "TA_S_light_crop.png", height = 3, width = 4)

#Separate data into nearshore, offshore and SF region
fig1 <- offshore %>% 
  filter(sal_pss > 30) %>%
  ggplot()+
  geom_point(aes(x = sal_pss, y = ta_umolkg, color = distance_offshore), alpha = 0.5)+
  scale_color_viridis(direction = -1)+
  labs(x = "Salinity", y = "TA", color = "Distance\nOffshore", title = "5-100km Offshore")+
  xlim(30, 35.5)+
  ylim(1900, 2500)
fig1
figurefun(fig1, "semioffshore_TA-S_noSF.png", height = 3, width = 4)


fig2 <- nearshore %>% 
  ggplot()+
  geom_point(aes(x = sal_pss, y = ta_umolkg, color = distance_offshore), alpha = 0.5)+
  scale_color_viridis(direction = -1)+
  labs(x = "Salinity", y = "TA", color = "Distance\nOffshore", title = "Shore to 5 km")+
  xlim(30, 35.5)+
  ylim(1900, 2500)
fig2
figurefun(fig2, "nearshore_TA-S_noSF.png", height = 3, width = 4)


fig3 <- sf_region %>%
  ggplot()+
  geom_point(aes(x = sal_pss, y = ta_umolkg, color = distance_offshore), alpha = 0.5)+
  scale_color_viridis(direction = -1)+
  labs(x = "Salinity", y = "TA", color = "Distance\nOffshore", title = "SF Region to 100km Offshore")+
  xlim(30, 35.5)+
  ylim(1900, 2500)
fig3
figurefun(fig3, "SFregion_TA-S.png", height = 3, width = 4)



fig <- light %>% 
  filter(!is.na(do_umolkg)) %>% 
  filter(depth_m < 25 & distance_offshore < 50) %>% 
  filter(time_utc > "1985-01-01") %>% 
  mutate(pH_all = ifelse(!is.na(pH_total), pH_total, c_pH_total)) %>% 
  ggplot()+
  geom_point(aes(x = t_C, y = pH_all, color = do_umolkg))+
  scale_color_viridis(option = "plasma", direction = -1)+
  labs(x = "Temperature", y = "pH", color = "DO")
fig
figurefun(fig, "T_pH_light_crop.png", height = 3, width = 4)



# Shallow OAH events ------------------------------------------------------

### Nearshore shallow hypoxia and very low pH
#Shallow hypoxia
light %>% 
  filter(distance_offshore < 50 & depth_m < 100) %>% 
  #filter(dataset_id != 66) %>% 
  filter(do_umolkg < 63) %>% 
  nrow()
#4227 
# Note that the OR OOI events are matched by the WCOA data from nearby :)

light %>% 
  filter(distance_offshore < 50 & depth_m < 100) %>% 
  filter(do_umolkg < 63) %>% 
  group_by(latitude, longitude) %>% 
  summarize(dataset_id = first(dataset_id), numhypox = n()) %>% 
  ggplot()+
  geom_sf(data = wcoast, fill = "white")+
  geom_point(aes(x = longitude, y = latitude, color = numhypox))

light %>% 
  #filter(dataset_id != 66) %>% 
  filter(distance_offshore < 50 & depth_m < 100) %>% 
  filter(do_umolkg < 63) %>% 
  mutate(dataset_id = factor(dataset_id)) %>% 
  ggplot()+
  geom_point(aes(x = do_umolkg, y = pH_total, color = dataset_id))+
  scale_color_viridis_d(option = "plasma")

fig <- light %>% 
  filter(distance_offshore < 50 & depth_m < 100) %>%
  mutate(pH_all = ifelse(is.na(pH_total), c_pH_total, pH_total)) %>% 
  filter(do_umolkg < 63) %>% 
  ggplot()+
  geom_point(aes(x = do_umolkg, y = pH_all, color = t_C))+
  geom_hline(yintercept = 7.8, linetype = "dashed", color = "gray")+
  scale_color_distiller(palette = "RdYlBu", values = scales::rescale(c(8, 11, 15, 20)))+
  labs(x = "DO Concentration", y = "pH", title = "Nearshore, Shallow (<100m) Hypoxia",
       color = "Temp")
fig
figurefun(fig, "shallow_hypoxia_vs_pH.png", height = 3, width = 4)


fig <- light %>% 
  filter(distance_offshore < 50 & depth_m < 100) %>%
  filter(dataset_id != 66) %>% 
  mutate(pH_all = ifelse(is.na(pH_total), c_pH_total, pH_total)) %>% 
  filter(do_umolkg < 63) %>% 
  ggplot()+
  geom_point(aes(x = do_umolkg, y = pH_all, color = t_C))+
  geom_hline(yintercept = 7.8, linetype = "dashed", color = "gray")+
  scale_color_distiller(palette = "RdYlBu", values = scales::rescale(c(8, 11, 15, 20)))+
  labs(x = "DO Concentration", y = "pH", title = "Nearshore, Shallow (<100m) Hypoxia",
       color = "Temp", subtitle = "No Oregon OOI")
fig
figurefun(fig, "shallow_hypoxia_vs_pH_noOROOI.png", height = 3, width = 4)


q <- light %>% 
  filter(distance_offshore < 50 & depth_m < 100) %>%
  mutate(pH_all = ifelse(is.na(pH_total), c_pH_total, pH_total)) %>% 
  filter(do_umolkg < 63) %>% 
  filter(!is.na(pH_all))

fig <- light %>% 
  filter(distance_offshore < 50 & depth_m < 100 & do_umolkg < 63) %>%
  ggplot()+
  geom_point(aes(x = do_umolkg, y = t_C), color = "gray", alpha = 0.5)+
  geom_point(data = q, aes(x = do_umolkg, y = t_C, color = pH_all))+
  scale_color_distiller(palette = "Reds")+
  labs(x = "DO Concentration", y = "Temperature", title = "Nearshore, Shallow (<100m) Hypoxia",
       color = "pH")
fig
figurefun(fig, "shallow_hypoxia_vs_temp_by_pH.png", height = 3, width = 4)


#pH focus
light %>% 
  filter(distance_offshore < 50 & depth_m < 100) %>% 
  mutate(pH_all = ifelse(is.na(pH_total), c_pH_total, pH_total)) %>% 
  filter(pH_all <= 7.8) %>%
  nrow()
#12,804 of these observations!

# Percentages
# low DO with low pH
light %>% 
  filter(distance_offshore < 50 & depth_m < 100) %>% 
  mutate(pH_all = ifelse(is.na(pH_total), c_pH_total, pH_total)) %>% 
  filter(!is.na(pH_all) & !is.na(do_umolkg)) %>% 
  summarize(total_hypoxic = sum(do_umolkg < 63, na.rm = TRUE),
            total_pH = sum(pH_total < 7.8, na.rm = TRUE), 
            total_both = sum(do_umolkg < 63 & pH_total < 7.8, na.rm = TRUE))


fig <- light %>% 
  filter(distance_offshore < 50 & depth_m < 100) %>% 
  mutate(pH_all = ifelse(is.na(pH_total), c_pH_total, pH_total)) %>% 
  filter(pH_all <= 7.8) %>% 
  ggplot()+
  geom_point(aes(x = do_umolkg, y = pH_all, color = t_C), alpha = 0.5)+
  geom_vline(xintercept = 63, linetype = "dashed", color = "gray")+
  scale_color_distiller(palette = "RdYlBu", values = scales::rescale(c(8, 11, 15, 20)))+
  labs(x = "DO Concentration", y = "pH", title = "Nearshore, Shallow (<100m) Low pH",
       color = "Temp")
fig
figurefun(fig, "shallow_lowpH_vs_DO.png", height = 3, width = 4)



fig <- light %>% 
  filter(distance_offshore < 50 & depth_m < 100) %>% 
  mutate(pH_all = ifelse(is.na(pH_total), c_pH_total, pH_total)) %>% 
  filter(pH_all <= 7.8) %>% 
  mutate(dataset_id = factor(dataset_id)) %>% 
  ggplot()+
  geom_point(aes(x = do_umolkg, y = pH_all, color = dataset_id), alpha = 0.5)+
  geom_vline(xintercept = 63, linetype = "dashed", color = "gray")+
  scale_color_viridis_d(option = "plasma")+
  labs(x = "DO Concentration", y = "pH", title = "Nearshore, Shallow (<100m) Low pH",
       color = "Dataset")
fig





# Fraction multiparameter daily data -----------------------------------------
fig <- light %>% 
  mutate(pH_all = ifelse(is.na(pH_total), c_pH_total, pH_total)) %>%
  mutate(do_all = ifelse(is.na(do_umolkg), do_sat, do_umolkg)) %>% 
  mutate(stresses = case_when(!is.na(t_C) & !is.na(do_all) & !is.na(pH_all) ~ "pH-DO-Temp",
                              !is.na(t_C) & is.na(do_all) & !is.na(pH_all) ~ "pH-Temp",
                              !is.na(t_C) & !is.na(do_all) & is.na(pH_all) ~ "DO-Temp",
                              !is.na(t_C) ~ "Temp",
                              TRUE ~ "none")) %>% 
  filter(stresses != "none") %>% 
  group_by(stresses) %>% 
  summarize(total_obs = "Temperature Obs", subtotals = n()) %>% 
  mutate(stresses = factor(stresses, levels = c("pH-DO-Temp", "pH-Temp", "DO-Temp", "Temp"))) %>%
  #mutate(stresses = factor(stresses, levels = c("pH-Temp", "pH-DO-Temp","Temp", "DO-Temp"))) %>%
  ggplot()+
  geom_bar(aes(y = subtotals, x = total_obs, fill = stresses), position = "stack", stat = "identity")+
  scale_fill_brewer(palette = "Reds", direction = -1)+
  labs(y = "Thousands of Temperature Obs", x = "", fill = "Parameters",
       title = "Distribution of Daily\nMultiparameter Observations")+
  scale_y_continuous(breaks = c(2e5, 4e5, 6e5), labels = c("200", "400", "600"))+
  theme(axis.text.x = element_blank())
fig
figurefun(fig, "number_daily_multistressor_obs.png", height = 3, width = 4)


light %>% 
  mutate(do_all = ifelse(is.na(do_umolkg), do_sat, do_umolkg)) %>% 
  mutate(stresses = case_when(!is.na(t_C) & !is.na(do_all) & !is.na(pH_total) ~ "pH-DO-Temp",
                              !is.na(t_C) & is.na(do_all) & !is.na(pH_total) ~ "pH-Temp",
                              !is.na(t_C) & !is.na(do_all) & is.na(pH_total) ~ "DO-Temp",
                              !is.na(t_C) ~ "Temp",
                              TRUE ~ "none")) %>% 
  filter(stresses != "none") %>% 
  group_by(stresses) %>% 
  summarize(total_obs = "Temperature Obs", subtotals = n())
