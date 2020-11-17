## Load FVS output data and cover-type map

# Data import
area <- read.csv("SCG_2018_Covertype_FVS_plot_Base.csv")
carbon <- read.csv("SCG_FVS_100Y_Carbon_Output_2018_Data_SD11162020.csv")
vol <- read.csv("SCG_FVS_Summary2_East.csv")

# Join data, select/rename variables, remove empty year
data <- vol %>% dplyr::select(StandID, Year, Tpa, BA, MCuFt) %>%
  left_join(carbon, by = c("StandID" = "Covertype", "Year" = "Year")) %>%
  left_join(area, by = c("StandID" = "Covertype")) %>%
  dplyr::select(covertype = StandID, year = Year, tpa = Tpa, ba = BA,
                merch_cuft = MCuFt,
                ag_ttl_l_t_ac_C = Aboveground_Total_Live,
                ag_merch_l_t_ac_C = Aboveground_Merch_Live,
                bg_l_t_ac_C = Belowground_Live,
                bg_d_t_ac_C = Belowground_Dead,
                ag_d_t_ac_C = Standing_Dead,
                cwd_t_ac_C = Forest_Down_Dead_Wood,
                floor_t_ac_C = Forest_Floor,
                shrub_herb_t_ac_C = Forest_Shrub_Herb,
                ttl_t_ac_C = Total_Stand_Carbon,
                area_ac = acres) %>%
  filter(year < 2118) %>%
  ungroup()

# What perc ttl area in each cover type?
temp <- data %>%
  group_by(covertype, area_ac) %>%
  summarise(ttl_area_ac = sum(area_ac))
    covertype_area_perc = area_ac/)
data$covertype_area_perc <- 

# Add cords & carbon conversion
# 1 cord = 128 cuft
# 1 cord tpyically ~ 2000-4000 dry lbs biomass (1-2 tons biomass); 1/2 that for C)
data$merch_cords <- data$merch_cuft / 128
data$t_C_per_cord <- data$ag_merch_l_t_ac_C / data$merch_cords 

# Some hw cover types >1 meaning each cord has > 1 t C.
# Most cover types < 1 meaning each cord has < 1 t C.
# Ratios highest for hardwoods, lowest for softwoods -- as expected (lower C density)

# 
