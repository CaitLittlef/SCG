
# Create area-weighted conversion factor for how many tons of carbon are in each cord.
mean(data$mt_C_per_cord)
sum(data[data$year == 2018,]$prop_ttl_area_ac)
  
data <- data %>%
  group_by(covertype) %>%
  mutate(ct_avg_mt_C_per_cord = mean(mt_C_per_cord)) %>% # take mean ratio for all yrs by covertype
  mutate(ct_avg_ar_mt_C_per_cord = ct_avg_mt_C_per_cord * prop_ttl_area_ac) %>% 
  ungroup()


# Just slect 1 year and sum area-weighted to get SCG-wide t_C_per_cord
scg_ar_mt_C_per_cord <- sum(data[data$year == 2018,]$ct_avg_ar_mt_C_per_cord) #0.8590868
# sum(data[data$year == 2018,]$prop_ttl_area_ac)


# Annually, carbon removals in merchantable volume amount to...
scg_ar_mt_C_per_cord * 3000 # =2577.261 metric tons


# Summarise stocks across cover types
ct_summ <- data %>%
  mutate(ct_ann_all_mtC = ttl_mt_ac_C * area_ac) %>%
  mutate(ct_ann_ag_bg_mtC = (ag_ttl_l_mt_ac_C*area_ac
                                 + bg_l_mt_ac_C*area_ac
                                 + ag_d_mt_ac_C*area_ac
                                 + bg_d_mt_ac_C*area_ac)) 
# Which has the highest? The lowest?
zoo <- ct_summ %>%
  filter(year == 2018) %>%
  dplyr::select(covertype, ttl_mt_ac_C)



# Summarise stocks for entire SCG, convert to CO2e, add annual removals (*10 b/c 10-yr timestep.
scg_summ <- ct_summ %>%
  group_by(year) %>%
  summarise(scg_ann_all_mtC = sum(ct_ann_all_mtC),
            scg_ann_ag_bg_mtC = sum(ct_ann_ag_bg_mtC),
            scg_ann_all_mtCO2e = scg_ann_all_mtC * 3.667,
            scg_ann_ag_bg_mtCO2e = scg_ann_ag_bg_mtC * 3.667,
            scg_10yr_rem_mtC = scg_ar_mt_C_per_cord * 3000*10,
            scg_10yr_rem_mtCO2e = scg_ar_mt_C_per_cord * 3000*10 * 3.667,
            scg_ann_all_mtCO2e_rem = scg_ann_all_mtCO2e - scg_10yr_rem_mtCO2e,
            scg_ann_ag_bg_mtCO2e_rem = scg_ann_ag_bg_mtCO2e - scg_10yr_rem_mtCO2e) %>%
  ungroup()
           


# What's annual increment of CO2 stored?
# Weird stuff happens to downed wood in first decade -- use long-term. 
yr <- max(scg_summ$year) - min(scg_summ$year)
(scg_summ[scg_summ$year == 2108,]$scg_ann_all_mtCO2e - scg_summ[scg_summ$year == 2018,]$scg_ann_all_mtCO2e)/yr
(scg_summ[scg_summ$year == 2108,]$scg_ann_ag_bg_mtCO2e - scg_summ[scg_summ$year == 2018,]$scg_ann_ag_bg_mtCO2e)/yr


# What's average per acre?
scg_summ[scg_summ$year == 2018,]$scg_ann_all_mtC / scg_area_ac # 39.51325
scg_summ[scg_summ$year == 2018,]$scg_ann_ag_bg_mtC / scg_area_ac # 28.58674
scg_summ[scg_summ$year == 2018,]$scg_ann_all_mtCO2e / scg_area_ac # 144.8951
scg_summ[scg_summ$year == 2018,]$scg_ann_ag_bg_mtCO2e / scg_area_ac # 104.8276
