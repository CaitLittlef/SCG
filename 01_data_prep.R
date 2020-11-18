## Load FVS output data and cover-type map


# Data import
area <- read.csv("SCG_2018_Covertype_FVS_plot_Base.csv")
# carbon <- read.csv("SCG_FVS_100Y_Carbon_Output_2018_Data_SD11162020_v1.csv")
# carbon <- read.csv("SCG_FVS_100Y_Carbon_Output_2018_Data_SD11162020_v2.csv")
carbon <- read.csv("SCG_FVS_100Y_Carbon_Output_2018_Data_SD11162020_v3.csv")
# vol <- read.csv("SCG_FVS_Summary2_East_v1.csv")
vol <- read.csv("SCG_FVS_Summary2_East_v2.csv")


# Confirm no missing covertypes/stands
# (temp <- anti_join(area, carbon, by = "Covertype"))
# (temp <- anti_join(area, vol, by = c("Covertype" = "StandID")))
# # H3C is missing from carbon and volume. Remove for now
# area <- area %>% filter(!Covertype == "H3C")


# What's total area? and prop of each cover type
(scg_area_ac <- sum(area$acres))
area$prop_ttl_area_ac <- area$acres/scg_area_ac
sum(area$prop_ttl_area_ac)


# Convert all tons carbon to metric tons carbon
carbon[,3:11] <- carbon[,3:11]*0.907185


# Join data, select/rename variables, remove empty year
data <- vol %>% dplyr::select(StandID, Year, Tpa, BA, MCuFt) %>%
  left_join(carbon, by = c("StandID" = "Covertype", "Year" = "Year")) %>%
  left_join(area, by = c("StandID" = "Covertype")) %>%
  dplyr::select(covertype = StandID, year = Year, tpa = Tpa, ba = BA,
                merch_cuft = MCuFt,
                ag_ttl_l_mt_ac_C = Aboveground_Total_Live,
                ag_merch_l_mt_ac_C = Aboveground_Merch_Live,
                bg_l_mt_ac_C = Belowground_Live,
                bg_d_mt_ac_C = Belowground_Dead,
                ag_d_mt_ac_C = Standing_Dead,
                cwd_mt_ac_C = Forest_Down_Dead_Wood,
                floor_mt_ac_C = Forest_Floor,
                shrub_herb_mt_ac_C = Forest_Shrub_Herb,
                ttl_mt_ac_C = Total_Stand_Carbon,
                area_ac = acres, prop_ttl_area_ac) %>%
  filter(year < 2118) %>%
  ungroup()


# Add cords & carbon conversion
# 1 cord = 128 cuft
# 1 cord tpyically ~ 2000-4000 dry lbs biomass (1-2 tons biomass); 1/2 that for C)
data$merch_cords <- data$merch_cuft / 128
(data$mt_C_per_cord <- data$ag_merch_l_mt_ac_C / data$merch_cords) 
# Some hw cover types >1 meaning each cord has > 1 t C.
# Most cover types < 1 meaning each cord has < 1 t C.
# Ratios highest for hardwoods, lowest for softwoods -- as expected (lower C density)


# Do ratios persist over time?
temp <- data %>%
  group_by(covertype, year) %>%
  summarise(avg = mean(mt_C_per_cord)) %>%
  ungroup() #%>%
  # view()
# The ratios differ subtly over time, even within covertypes. NBD.

rm(temp)
