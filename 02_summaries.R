# Create area-weighted conversion factor for how many tons of carbon are in each cord.
# Use that to approximate how much carbon is removed with 3000 cords annually (decades)
# Give area-weighted total SCG carbon and change over time (chart) -- with and without removals
# Give area-weighted by cover type (hw, mixed, sw) over time.

# Create area-weighted conversion factor for how many tons of carbon are in each cord.
mean(data$t_C_per_cord)
sum(data[data$year == 2018,]$prop_ttl_area_ac)
  
data <- data %>%
  group_by(covertype) %>%
  mutate(ct_avg_t_C_per_cord = mean(t_C_per_cord)) %>% # take mean ratio for all yrs by covertype
  mutate(ct_avg_ar_t_C_per_cord = ct_avg_t_C_per_cord * prop_ttl_area_ac) %>% 
  ungroup()


# Just slect 1 year and sum area-weighted to get SCG-wide t_C_per_cord
sum(data[data$year == 2018,]$ct_avg_ar_t_C_per_cord) #0.8590868
# sum(data[data$year == 2018,]$prop_ttl_area_ac)



