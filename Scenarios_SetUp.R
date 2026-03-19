##Scenario Set Up
### SCENARIOS-- 
##50% new demand is LFP --- ##30% of tesla is LFP -- canceled
## 15% reduction batt cap and continuation batt cap

##LDV and HDV extend to 2035 and then do 15% reduction of 2024 values until 2040

cathode_projections <- read_excel("/Users/elsawefes-potter/Documents/Critical_Minerals_Pablo/Cathode Projections.xlsx", sheet = "Sheet1")

### SET UP BATTERY CAP/CHEM SIMULATIONS
fixed_batt_cap_merged <- batt_cap_merged %>% select(-c(`Total Sales`, `Total Mwh`)) %>% filter(Propulsion != "FCEV")
batt_cap_merged$Sale_Year <- as.numeric(batt_cap_merged$Sale_Year)

# Filter for Sale Year 2024
batt_cap_2024 <- batt_cap_merged %>%
  filter(Sale_Year == 2024) %>%
  select(`Sale_Year`, Segment, Propulsion, Base_Capacity = `Avg Batt Cap (kwh/batt)`) 

years_batt_cap <- 2025:2035

trend_results <- batt_cap_merged %>%
  filter(!is.na(`Avg Batt Cap (kwh/batt)`)) %>%
  group_by(Segment, Propulsion) %>%
  filter(n() >= 3) %>%  # Ensure enough data points
  summarise(
    trend = coef(lm(`Avg Batt Cap (kwh/batt)` ~ Sale_Year))[2],
    .groups = "drop"
  ) %>%
  filter(Propulsion != "FCEV")

projection_base <- batt_cap_2024 %>%
  inner_join(trend_results, by = c("Segment", "Propulsion")) %>%
  crossing(years_batt_cap)

batt_cap_projection <- projection_base %>%
  mutate(`Projected Avg Batt Cap (kwh/batt)` = Base_Capacity + (years_batt_cap - 2024) * trend)

batt_cap_projection <- batt_cap_projection %>% select(-Sale_Year) %>% rename(Sale_Year = years_batt_cap)

## After 2035 its constant
batt_cap_projection <- batt_cap_projection %>%
  group_by(Segment, Propulsion) %>%
  complete(Sale_Year = 2025:2050) %>% 
  mutate(`Projected Avg Batt Cap (kwh/batt)`= ifelse(
    Sale_Year > 2035,
    `Projected Avg Batt Cap (kwh/batt)`[Sale_Year == 2035][1],
    `Projected Avg Batt Cap (kwh/batt)`
  )) %>%
  fill(`Projected Avg Batt Cap (kwh/batt)`, .direction = "down") %>%
  ungroup() %>%
  select(-c(Base_Capacity, trend))%>% 
  bind_rows(projection_HDV_full) 

#### BATT CAP Proj 2
batt_cap_2040 <- batt_cap_2024 %>%
  mutate(Base_Capacity = Base_Capacity * 0.85, Sale_Year = 2040)
batts <- bind_rows(batt_cap_2024,batt_cap_2040)

# Manual calculation using reframe
second_trend_results <- batts %>%
  group_by(Segment, Propulsion) %>%
  filter(n() == 2) %>%  # Keep only groups with both years
  reframe(
    cap_2024 = Base_Capacity[Sale_Year == 2024],
    cap_2040 = Base_Capacity[Sale_Year == 2040],
    slope    = (cap_2040 - cap_2024) / (2040 - 2024),
    intercept = cap_2024 - slope * 2024
  )

all_batt_cap_years <- 2025:2050

batt_cap_15 <- second_trend_results %>%
  crossing(Sale_Year = all_batt_cap_years) %>%
  mutate(
    `Projected Avg Batt Cap (kwh/batt)` = case_when(
      Sale_Year <= 2040 ~ intercept + slope * Sale_Year,
      TRUE ~ intercept + slope * 2040  # hold at 2040 value
    ) 
  ) %>%
  filter(Propulsion != "FCEV") %>% 
  bind_rows(HDV_batt_cap_15) %>%
  select(-c(cap_2024, cap_2040, intercept, slope))


### CLEAN BENCHMARK CHEMISTRY
# Slice and clean rows/columns
cp <- cathode_projections[12:21, ]  

cp <- cp %>%
  select(-`...2`, -`...3`) %>%
  rename(`Cathode Mix` = `...1`) %>%
  slice(-1)  

cp_melted <- cp %>%
  pivot_longer(-`Cathode Mix`, names_to = "Sale_Year", values_to = "Total Mwh") %>%
  mutate(`Sale_Year` = as.integer(`Sale_Year`)) %>%
  group_by(`Sale_Year`) %>%
  mutate(`Cathode Mix Share` = `Total Mwh` / sum(`Total Mwh`, na.rm = TRUE)) %>%
  ungroup()

replacement_future <- c(
  'NCM low nickel' = 'NMC 111',
  'NCM mid nickel' = 'NMC 622',
  'NCM high nickel' = 'NMC 811'
)

cp_melted$`Cathode Mix` <- recode(cp_melted$`Cathode Mix`, !!!replacement_future)
cp_melted <- cp_melted %>% filter(`Cathode Mix Share` != 0)

fixed_cp <- cp_melted

max_future <- fixed_cp %>%
  group_by(`Sale_Year`) %>%
  slice_max(`Cathode Mix Share`, n = 1, with_ties = FALSE) %>%
  ungroup()

# Merge back to fill in 'other' chemistries
future_match <- left_join(fixed_cp, max_future, by = "Sale_Year", suffix = c("_x", "_y"), relationship = "many-to-one")

# Replace unknown chemistries with most common
mask_mins_future <- future_match$`Cathode Mix_x` %in% c("4V Ni or Mn based", "5V Mn based", "LCO", "Other")
future_match$`Cathode Mix_x`[mask_mins_future] <- future_match$`Cathode Mix_y`[mask_mins_future]

# Clean columns and add in HDV
future_match <- future_match %>% 
  select(`Sale_Year`, `Cathode Mix` = `Cathode Mix_x`, 
         `Cathode Mix Share` = `Cathode Mix Share_x`, 
         `Total Mwh` = `Total Mwh_x`) %>%
  group_by(Sale_Year, `Cathode Mix`) %>%
  summarise(`Total Mwh` = sum(`Total Mwh`, na.rm = TRUE), 
            `Cathode Mix Share` = sum(`Cathode Mix Share`, na.rm = TRUE)) %>%
  select(-`Total Mwh`)

df_2040_proj <- future_match %>% filter(Sale_Year == 2040)
df_extend_proj <- df_2040_proj %>%
  mutate(Sale_Year = list(2041:2050)) %>%
  unnest(Sale_Year)

future_match <- bind_rows(
  future_match %>% filter(Sale_Year <= 2040),
  df_extend_proj
) %>%
  arrange(`Cathode Mix`, Sale_Year) 

future_match_HDV <- future_match %>%
  crossing(
    Propulsion = c("BEV", "PHEV"),
    Segment = c("Car", "SUV")
  ) %>%
  bind_rows(HDV_chem_project)

  

### HIGH LFP Scenario
total_mwh_per_year <- cp_melted %>%
  group_by(Sale_Year) %>%
  summarise(Total_Mwh = sum(`Total Mwh`, na.rm = TRUE), .groups = "drop")

lfp_targets <- tibble(
  Sale_Year = unique(cp_melted$Sale_Year),
  LFP_share_target = scales::rescale(Sale_Year, to = c(0.27, 0.5))  # From 27% in 2024 to 50% in 2040
)

lfp_mwh_per_year <- total_mwh_per_year %>%
  left_join(lfp_targets, by = "Sale_Year") %>%
  mutate(LFP_Mwh = Total_Mwh * LFP_share_target)

chem_with_targets <- future_match %>%
  left_join(lfp_mwh_per_year, by = "Sale_Year")

# Split into LFP and non-LFP
lfp_rows <- chem_with_targets %>%
  filter(`Cathode Mix` == "LFP") %>%
  mutate(Adjusted_Mwh = LFP_Mwh)

lfp_rows <- lfp_rows %>% mutate(New_Cathode_Share = LFP_share_target)

other_chems <- chem_with_targets %>%
  filter(`Cathode Mix` != "LFP")

adjusted_other_chems <- other_chems %>%
  group_by(Sale_Year) %>%
  mutate(
    total_other_share = sum(`Cathode Mix Share`, na.rm = TRUE),
    remaining_mwh = unique(Total_Mwh) - unique(LFP_Mwh),
    Adjusted_Mwh = (`Cathode Mix Share` / total_other_share) * remaining_mwh, # not lfp remaining mwh
    New_Cathode_Share = Adjusted_Mwh/Total_Mwh
  ) %>%
  ungroup()


final_adjusted_mix <- bind_rows(lfp_rows, adjusted_other_chems) %>% 
  mutate(`Cathode Mix Share` = New_Cathode_Share) %>% select(Sale_Year, `Cathode Mix`, `Cathode Mix Share`) 

df_2040_adjusted <- final_adjusted_mix %>% filter(Sale_Year == 2040)
df_extend_adjusted <- df_2040_adjusted %>%
  mutate(Sale_Year = list(2041:2050)) %>%    # add future years
  unnest(Sale_Year) 


final_adjusted_mix_extended <- bind_rows(
  final_adjusted_mix %>% filter(Sale_Year <= 2040),
  df_extend_adjusted
) %>%
  arrange(`Cathode Mix`, Sale_Year) %>%
  crossing(
    Propulsion = c("BEV", "PHEV"),
    Segment = c("Car", "SUV")
  ) %>% bind_rows(HDV_chem_project)

