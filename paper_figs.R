# Code to reproduce all figures included in the ESSD manuscript

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
library("ggpubr")

#wcoast <- subset(world2, world$NAME %in% c("Canada", "United States", "Mexico"))
usa <- st_as_sf(maps::map("state", fill=TRUE, plot =FALSE))
wcoast <- usa %>% 
  filter(ID %in% c("california", "oregon", "washington", "nevada", "idaho"))
cal <- wcoast %>% filter(ID == "california")

countries <- st_as_sf(maps::map("world", fill = TRUE, plot = FALSE))
countries <- countries %>% filter(ID %in% c("Canada", "USA", "Mexico"))

#General style for all maps
plot_style <- list(geom_sf(data = wcoast, fill = "white", color = "#3a3a3a"),
                   geom_sf(data = countries, fill = "transparent"),
                   theme_void(),  # Remove ugly grey background
                   theme(axis.text.x = element_blank(),
                         plot.title = element_text(hjust = 0.5)),
                   guides(alpha = "none"),
                   xlim(-153, -116), ylim(32, 49))

theme_set(theme_bw())

load("clean_all_light.Rdata") # For maps, data density
load("full_nd.Rdata") # For flagging and data totals


#Folder for figs
figpath <- "DSP_paper_figs/"


## Make a function to print nice figs at high res 
figurefun <- function(figure, filename, height, width){
  png(filename = paste0(figpath, filename), res = 300, height = height, width = width, units = "in")
  print(figure)
  dev.off()
}




# Fig 1: Data locations ---------------------------------------------------
# Raw data locations
locations <- light %>% 
  filter(depth_m < 25) %>% 
  mutate(pH = ifelse(!is.na(pH_total) | !is.na(c_pH_total), 1, NA)) %>%
  mutate(DO = ifelse(!is.na(do_umolkg) | !is.na(do_sat), 1, NA)) %>% 
  rename(Temperature = t_C) %>% 
  mutate(year = lubridate::year(time_utc)) %>% 
  dplyr::select(latitude, longitude, year, Temperature, DO, pH, sample_scheme) %>% 
  group_by(latitude, longitude, year, sample_scheme) %>% 
  summarize(Temperature = sum(!is.na(Temperature)), DO = sum(!is.na(DO)), pH = sum(!is.na(pH))) %>% 
  ungroup()

## Make three maps without titles (will be in the caption)
## Remake the three figs without titles
fig1 <- locations %>% 
  filter(Temperature > 0) %>% 
  ggplot()+
  geom_point(aes(x = longitude, y = latitude), color = "#440154FF", size = 0.4)+
  plot_style+
  theme_bw()+
  theme(legend.position = "none", axis.title = element_blank())+
  scale_x_continuous(breaks = c(-150, -140, -130, -120), limits = c(-150, -115))

fig2 <- locations %>% 
  filter(DO > 0) %>% 
  ggplot()+
  geom_point(aes(x = longitude, y = latitude), color = "#2A788EFF", size = 0.4)+
  plot_style+
  theme_bw()+
  theme(legend.position = "none", axis.title = element_blank())+
  scale_x_continuous(breaks = c(-150, -140, -130, -120), limits = c(-150, -115))

fig3 <- locations %>% 
  filter(longitude > -130) %>% 
  filter(pH > 0) %>% 
  ggplot()+
  geom_point(aes(x = longitude, y = latitude), color = "#22A884FF", size = 0.4)+
  plot_style+
  theme_bw()+
  theme(legend.position = "none", axis.title = element_blank())+
  scale_x_continuous(breaks = c(-150, -140, -130, -120), limits = c(-150, -115))


composite_dsp_col <- plot_grid(fig1, fig2, fig3, nrow = 1, labels = c("(a)", "(b)", "(c)"),
                               label_size = 10, vjust = 2)

figurefun(composite_dsp_col, filename = "data_locs_composite_bs_wide.png", height = 2, width = 7.5)



# Fig 2: Flagging by dataset and parameter ---------------------------------------
# Make flagging dataframe
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

# Figure 2 - flag percentage by dataset
flagtable <- flagds %>% 
  filter(!is.na(percent_good)) %>% 
  mutate(percent_bad = (1-percent_good) * 100) %>% 
  mutate(flag_bin = case_when(percent_bad < 5 ~ "<5%",
                              percent_bad <= 10 ~ "<10%",
                              percent_bad <= 20 ~ "<20%",
                              percent_bad <= 30 ~ "<30%",
                              TRUE ~ ">30%")) %>% 
  mutate(flag_bin = factor(flag_bin, levels = c("<5%", "<10%", "<20%", "<30%", ">30%"))) %>% 
  group_by(variable, flag_bin) %>% 
  summarize(datasets = n())


fig <- flagtable %>% 
  group_by(variable) %>% 
  mutate(total_datasets = sum(datasets)) %>% 
  ungroup() %>% 
  mutate(percent = datasets/total_datasets * 100) %>% 
  mutate(variable = case_when(variable == "Temperature" ~ "T",
                              variable == "Salinity" ~ "S",
                              TRUE ~ variable)) %>% 
  mutate(variable = factor(variable, levels = c("T", "S", "DO", "pH", "TA", "DIC"))) %>% 
  ggplot()+
  geom_col(aes(x = flag_bin, y = percent, fill = variable), color = "white", position = position_dodge2(width = 1, preserve = "single"))+
  scale_fill_viridis_d()+
  labs(x = "Maximum Flag Rate", y = "Percentage of Datasets", fill = "Parameter")

figurefun(fig, "flag_rates_by_DS_summary_percent.png", height = 4, width = 4)

# Fig 3: Spatiotemporal Hovmollers -----------------------------------------------------

# Try a tiled heatmap
latzones <- seq(from = 32, to = 48.5, by = 0.5)

heatmap <- light %>% 
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
  summarize(temp = sum(!is.na(t_C)), DO = sum(!is.na(do_all)), pH = sum(!is.na(pH_total)),
            anycarb = sum(!is.na(anycarb))) %>% 
  filter(time < 2020)

## Build the composite figure
# Vector of degree axis labels for plots
degreelabs <- paste0(c(32, 36, 40, 44, 48), "\u00B0", " N")

fig1 <- heatmap %>% 
  filter(temp != 0) %>%
  ggplot()+
  geom_tile(aes(x = time, y = latitude, fill = temp))+
  scale_fill_distiller(palette = "Purples", direction = 1, trans = "log10")+
  geom_text(aes(x = 2020.5, y = 34.5), label = "\U2605", size = 3)+
  geom_text(aes(x = 2020.5, y = 46.3), label = "\U2605", size = 3)+
  geom_text(aes(x = 2020.5, y = 42), label = "\U2605", size = 3)+
  labs(x = "Time", fill = "No.", title = "Temperature")+
  theme(axis.text.x = element_text(color = "white"),
        axis.title = element_blank(),
        legend.key.size = unit(0.5, "cm"),
        plot.margin = unit(c(0, 0, 0, 0.25), "cm"))+
  scale_y_continuous(breaks = c(32, 36, 40, 44, 48), labels = degreelabs)+
  scale_x_continuous(limits = c(1980, 2021), expand = c(0,0))

fig2 <- heatmap %>% 
  filter(DO != 0) %>% 
  ggplot()+
  geom_tile(aes(x = time, y = latitude, fill = DO))+
  scale_fill_distiller(palette = "Blues", direction = 1, trans = "log10")+
  geom_text(aes(x = 2020.5, y = 34.5), label = "\U2605", size = 4)+
  geom_text(aes(x = 2020.5, y = 46.3), label = "\U2605", size = 4)+
  geom_text(aes(x = 2020.5, y = 42), label = "\U2605", size = 4)+
  labs(x = "Time", fill = "No.", title = "Dissolved Oxygen")+
  theme(axis.text.x = element_text(color = "white"),
        axis.title = element_blank(),
        legend.key.size = unit(0.5, "cm"),
        plot.margin = unit(c(0, 0, 0, 0.25), "cm"))+
  scale_y_continuous(breaks = c(32, 36, 40, 44, 48), labels = degreelabs)+
  scale_x_continuous(limits = c(1980, 2021), expand = c(0,0))


fig3 <- heatmap %>% 
  filter(anycarb > 0) %>% 
  ggplot()+
  geom_tile(aes(x = time, y = latitude, fill = anycarb))+
  scale_fill_distiller(palette = "BuGn", direction = 1, trans = "log10")+
  geom_text(aes(x = 2020.5, y = 34.5), label = "\U2605", size = 4)+
  geom_text(aes(x = 2020.5, y = 46.3), label = "\U2605", size = 4)+
  geom_text(aes(x = 2020.5, y = 42), label = "\U2605", size = 4)+
  labs(x = "Time", fill = "No.", title = "Any Carbonate System")+
  theme(legend.key.size = unit(0.5, "cm"),
        axis.title.y = element_blank(),
        plot.margin = unit(c(0, 0, 0, 0.25), "cm"))+
  scale_y_continuous(breaks = c(32, 36, 40, 44, 48), labels = degreelabs)+
  scale_x_continuous(limits = c(1980, 2021), expand = c(0,0))

composite_hov <- plot_grid(fig1, fig2, fig3, ncol = 1, labels = c("(a)", "(b)", "(c)"), label_size = 10,
                           rel_heights = c(1, 1, 1), vjust = 1)

figurefun(composite_hov, filename = "density_composite_tile.png", height = 6, width = 4)

# Fig 4: Monthly data distribution ----------------------------------------

#Percent data by month
monthly <- light %>% 
  mutate(month = lubridate::month(time_utc)) %>% 
  mutate(ta_umolkg = ifelse(ta_type == "calculated", NA, ta_umolkg)) %>% 
  mutate(tCO2_umolkg = ifelse(tCO2_type == "calculated", NA, tCO2_umolkg)) %>% 
  mutate(pCO2_uatm = ifelse(pCO2_type == "calculated", NA, pCO2_uatm)) %>% 
  mutate(do_all = ifelse(is.na(do_umolkg), do_sat, do_umolkg)) %>% 
  group_by(month) %>% 
  summarize(`T` = sum(!is.na(t_C)), S = sum(!is.na(sal_pss)), DO = sum(!is.na(do_all)),
            pCO2 = sum(!is.na(pCO2_uatm)), pH = sum(!is.na(pH_total)), DIC = sum(!is.na(tCO2_umolkg)),
            TA = sum(!is.na(ta_umolkg)), chl = sum(!is.na(chl_ugL)), 
            nutr = sum(!is.na(no2_umolkg)) + sum(!is.na(no3_umolkg)) + sum(!is.na(po4_umolkg))
            + sum(!is.na(si_umolkg)) + sum(!is.na(nh4_umolkg))) %>% 
  ungroup() %>% 
  pivot_longer(cols = -month, names_to = "Variables", values_to = "Totals")


# Double figure
library("egg")
fig <- monthly %>%
  mutate(Variables = factor(Variables, levels = c("T", "S", "DO", "pH", "TA", "DIC"))) %>% 
  mutate(var_group = ifelse(Variables %in% c("T", "S", "DO"), 1, 2)) %>%
  mutate(var_group = factor(var_group)) %>% 
  filter(!is.na(month)) %>% 
  filter(!is.na(Variables)) %>% 
  ungroup() %>% 
  mutate(month = factor(month)) %>% 
  group_by(Variables) %>% 
  mutate(overalltotal = sum(Totals)) %>% 
  mutate(Percent = Totals/overalltotal*100) %>% 
  ggplot()+
  geom_col(aes(x = month, y = Percent, fill = Variables), color = "white", position = "dodge", width = 0.7)+ 
  geom_hline(yintercept = 8.33, linetype = "dotted")+
  scale_fill_manual(values = c("#440154FF", "#414487FF", "#2A788EFF", "#22A884FF", "#7AD151FF", "#FDE725FF"))+
  labs(y = "Percent of Observations", x = "Month", fill = "Parameters")+
  facet_grid(rows = vars(var_group))+
  theme(strip.text = element_blank())

figurefun(fig, filename = "data_percent_bymonth_all_vert.png", height = 4, width = 4)


# Fig 5: Seasonal boxplots w/in 100 m depth contour -------------------------------------------------------

library("marmap")
shelf <- getNOAA.bathy(lon1 = -150, lon2 = -115,
                       lat1 = 31, lat2 = 49, resolution = 4)

shelf <- marmap::as.raster(shelf)

crs(shelf) <- crs(wcoast)

## Extract the depths for all the locations in the light dataframe
lightlocs <- light %>% 
  distinct(latitude, longitude)

locs_sf <- st_as_sf(lightlocs, coords = c("longitude", "latitude"), crs = crs(wcoast))

depths <- raster::extract(shelf, locs_sf)

# Match the depths to the coordinates
locs_sf <- cbind(locs_sf, depths)
lightlocs$bathy <- locs_sf$depths


# Join the bathy stuff to light
lightdepths <- left_join(light, lightlocs)


## Make the regional, seasonal dataframe within the 100 m depth contour
seasonal <- lightdepths %>% 
  filter(bathy > -100) %>% 
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
  rename(temp = t_C, DO = do_umolkg, pH = pH_total, arag = c_omega_ar) %>% 
  dplyr::select(season, region, temp, DO, pH, arag) %>% 
  mutate(season = factor(season, levels = c("Jan-Mar", "Apr-Jun", "Jul-Sep", "Oct-Dec"))) %>% 
  mutate(region = factor(region, levels = c("Washington", "Oregon", "NorCen CA", "So. CA")))

## Set up for boxplots
seasonal <- seasonal %>% 
  pivot_longer(cols = -c(season, region), names_to = "Variable", values_to = "Values") %>% 
  filter(!is.na(Values)) %>% 
  mutate(month = case_when(season == "Jan-Mar" ~ "1-3",
                           season == "Apr-Jun" ~ "4-6",
                           season == "Jul-Sep" ~ "7-9",
                           TRUE ~ "10-12")) %>% 
  mutate(month = factor(month, levels = c("1-3", "4-6", "7-9", "10-12")))


## Construct the composite figure with standard colors
fig1 <- seasonal %>% 
  filter(Variable == "temp") %>% 
  ggplot()+
  geom_jitter(aes(x = month, y = Values), alpha = 0.2, size = 0.4, color = "#440154FF")+
  geom_boxplot(aes(x = month, y = Values), outlier.alpha = 0, fill = "white", alpha = 0.6)+
  labs(y = paste0("Temperature \u00B0C"), x = "Months")+
  theme(legend.position = "none",
        axis.title = element_text(size = 8))+
  facet_wrap(~region)

fig2 <- seasonal %>% 
  filter(Variable == "DO") %>% 
  ggplot()+
  geom_jitter(aes(x = month, y = Values), alpha = 0.2, size = 0.4, color = "#2A788EFF")+
  geom_boxplot(aes(x = month, y = Values), outlier.alpha = 0, fill = "white", alpha = 0.6)+
  labs(y = expression(paste(DO~concentration~mu*mol~kg^-1)), x = "Months")+
  theme(legend.position = "none",
        axis.title = element_text(size = 8))+
  facet_wrap(~region)

fig3 <- seasonal %>% 
  filter(Variable == "pH") %>% 
  ggplot()+
  geom_jitter(aes(x = month, y = Values), alpha = 0.2, size = 0.4, color = "#22A884FF")+
  geom_boxplot(aes(x = month, y = Values), outlier.alpha = 0, fill = "white", alpha = 0.6)+
  labs(y = paste0("pH"), x = "Months")+
  theme(legend.position = "none",
        axis.title = element_text(size = 8))+
  facet_wrap(~region)

composite <- plot_grid(fig1, fig2, fig3, nrow = 1, labels = c("(a)", "(b)", "(c)"), label_size = 12)

figurefun(composite, filename = "difbox_seasonal_composite_stdcolors_100bathy.png", height = 3, width = 9)

# Fig 6: OAH events -------------------------------------------------
### OAH events in state waters (< 5 km from shore)

# Targetting pH < 7.8 and DO < 107

fig <- light %>% 
  filter(distance_offshore < 5 & depth_m < 50) %>% 
  filter(pH_total <= 7.9) %>% 
  ggplot()+
  geom_jitter(aes(x = do_umolkg, y = pH_total, color = t_C), alpha = 0.75, size = 0.5)+
  geom_vline(xintercept = 63, linetype = "dashed", color = "red", alpha = 0.5)+
  geom_vline(xintercept = 107, linetype = "dashed", color = "gray")+
  geom_hline(yintercept = 7.8, linetype = "dashed", color = "gray")+
  scale_color_distiller(palette = "RdYlBu", values = scales::rescale(c(8, 11, 15, 20)))+
  labs(x = expression(paste(DO~content~mu*mol~kg^-1)), y = "pH", color = "\u00B0C")+
  xlim(0, 400)+
  scale_y_continuous(expand = c(0, 0), limits = c(7.3, 7.85))

figurefun(fig, filename = "state_water_OAH.png", height = 4, width = 4)


# Fig 7: TA-S relationships -----------------------------------------------
## Array of offshore vs nearshore scatter plots
## Filter data to near-oceanic salinities (> 28) and surface water
array <- light %>% 
  filter(sal_pss > 28 & sal_pss < 36) %>% 
  filter(depth_m < 25 & distance_offshore < 100) %>% 
  mutate(latzone = case_when(latitude < 34.5 ~ "So. CA",
                             latitude < 42 ~ "NorCen CA",
                             latitude < 46.25 ~ "OR",
                             TRUE ~ "WA")) %>% 
  mutate(nearshore = ifelse(distance_offshore < 2, "0-2 km", "2-100 km")) %>% 
  mutate(latzone = factor(latzone, levels = c("WA", "OR", "NorCen CA", "So. CA"))) %>%
  mutate(nearshore = factor(nearshore, levels = c("0-2 km", "2-100 km"))) %>% 
  group_by(latzone, nearshore) %>% 
  mutate(group = cur_group_id())


## Make the figure
library("ggpmisc")

fig <- array %>% 
  filter(sal_pss > 28 & sal_pss < 36) %>% 
  ggplot(aes(x = sal_pss, y = ta_umolkg))+
  geom_point(aes(x = sal_pss, y = ta_umolkg), alpha = 0.5)+
  stat_poly_line(method = "lm", aes(color = group), linewidth = 0.5)+
  stat_poly_eq(eq.with.lhs = "italic(TA)~`=`~",
               eq.x.rhs = "~italic(S)",
               use_label("eq"),
               size = 3,
               coef.digits = 4)+
  stat_poly_eq(use_label(c("p", "R2")),
               label.y = 0.85, size = 3)+
  facet_grid(rows = vars(latzone), cols = vars(nearshore))+
  theme(legend.position = "none")+
  scale_color_viridis()+
  labs(x = "Salinity", y = "Total Alkalinity")

figurefun(fig, filename = "TA-S_array.png", height = 7, width = 6.5)
