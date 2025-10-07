library(dplyr)
library(purrr)
library(readxl)
library(dplyr)
library(tidyr)
library(stringr)
library(tidyverse)

### SCENARIOS-- 
##50% new demand is LFP --- ##30% of tesla is LFP -- canceled
## 15% reduction batt cap and continuation batt cap

## change 15% batt and 50% demand ends at 2040 and then continues

cathode_projections <- read_excel("/Users/elsawefes-potter/Documents/Critical_Minerals_Pablo/Cathode Projections (1).xlsx", sheet = "Sheet1")
EVLIB_Flows <- read_csv("/Users/elsawefes-potter/Documents/Critical_Minerals_Pablo/EVLIB_Flows_detail_ACCII (1) .csv")

### Label retirement vectors with the appropriate sale year
# Starting year
start_year <- 2020

name_vector_with_years <- function(vec_string, start_year) {
  # Make sure it's a string
  vec_string <- as.character(vec_string)
  
  # Split and convert to numeric
  vec <- as.numeric(strsplit(vec_string, "\\|")[[1]])
  
  # Assign year names (increasing years)
  names(vec) <- start_year - (seq_along(vec) - 1)
  
  return(vec)
}

# Apply to each row using Map
EVLIB_Flows$LIB_recycling_vector <- Map(
  name_vector_with_years,
  EVLIB_Flows$LIB_recycling_vector,
  EVLIB_Flows$Year
)


future_recycle_type <- EVLIB_Flows %>%
  mutate(
    recycle_df = map(LIB_recycling_vector, ~ {
      tibble(
        Sale_Year = as.integer(names(.x)),
        LIB_recycle_total = as.numeric(.x)
      )
    })
  ) %>%
  select(State, Segment, Propulsion, Year, recycle_df) %>%  # keep original Year here
  unnest(cols = recycle_df)

  
### RUN BATTERY CAPACITY SIMULATIONS
batt_cap_merged$Sale_Year <- as.numeric(batt_cap_merged$Sale_Year)

# Filter for Sale Year 2024
batt_cap_2024 <- batt_cap_merged %>%
  filter(Sale_Year == 2024) %>%
  select(`Sale_Year`, Segment, Propulsion, Base_Capacity = `Avg Batt Cap (kwh/batt)`) 

years_batt_cap <- 2025:2050

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

batt_cap_15 <- second_trend_results %>%
  crossing(Sale_Year = years_batt_cap) %>%
  mutate(
    `Projected Avg Batt Cap (kwh/batt)` = case_when(
      Sale_Year <= 2040 ~ intercept + slope * Sale_Year,
      TRUE ~ intercept + slope * 2040  # hold at 2040 value
    ) 
  ) %>%
  filter(Propulsion != "FCEV")



batt_scen <- list(batt_cap_projection, batt_cap_15)



### CLEAN BENCHMARK CHEMISTRY
# Slice and clean rows/columns
cp <- cathode_projections[12:21, ]  # Python iloc[11:21] is R 12:21

cp <- cp %>%
  select(-`...2`, -`...3`) %>%
  rename(`Cathode Mix` = `...1`) %>%
  slice(-1)  # Drop row 12 (Python index 11)

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
future_match <- left_join(fixed_cp, max_future, by = "Sale_Year", suffix = c("_x", "_y"), relationship = "many-to-many")

# Replace unknown chemistries with most common
mask_mins_future <- future_match$`Cathode Mix_x` %in% c("4V Ni or Mn based", "%V Mn based", "LCO", "Other")
future_match$`Cathode Mix_x`[mask_mins_future] <- future_match$`Cathode Mix_y`[mask_mins_future]

# Clean columns
future_match <- future_match %>%
  select(`Sale_Year`, `Cathode Mix` = `Cathode Mix_x`, 
         `Cathode Mix Share` = `Cathode Mix Share_x`, 
         `Total Mwh` = `Total Mwh_x`)


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
    Adjusted_Mwh = (`Cathode Mix Share` / total_other_share) * remaining_mwh,
    New_Cathode_Share = Adjusted_Mwh/Total_Mwh
  ) %>%
  ungroup()


final_adjusted_mix <- bind_rows(lfp_rows, adjusted_other_chems) %>% 
  mutate(`Cathode Mix Share` = New_Cathode_Share) %>% select(Sale_Year, `Cathode Mix`, `Cathode Mix Share`)

### Keep 2040 values till 2050
extend_flat_from_2040 <- function(df, id_cols, value_cols, cutoff_year = 2040, max_year = 2050) {
  # Extract rows for cutoff year
  df_2040 <- df %>% filter(Sale_Year == cutoff_year)
  
  # Generate new years
  years_post_2040 <- tibble(Sale_Year = (cutoff_year + 1):max_year)
  
  # Remove Sale_Year before crossing, to avoid duplication
  df_2040_noyear <- df_2040 %>%
    select(-Sale_Year)
  
  # Create extended rows with frozen values
  df_post <- crossing(df_2040_noyear, years_post_2040) %>%
    relocate(Sale_Year, .before = everything())
  
  # Keep data up to and including cutoff year
  df_pre <- df %>% filter(Sale_Year <= cutoff_year)
  
  # Combine
  bind_rows(df_pre, df_post)
}

final_adjusted_mix <- extend_flat_from_2040(
  df = final_adjusted_mix,
  id_cols = c("Sale_Year", "Cathode Mix"),
  value_cols = c("Cathode Mix Share")) %>% group_by(`Cathode Mix`) %>%
  arrange(`Cathode Mix`, Sale_Year, .by_group = TRUE)

future_match <- future_match %>% select (-`Total Mwh`)

future_match <- extend_flat_from_2040(
  df = future_match,
  id_cols = c("Sale_Year", "Cathode Mix"),
  value_cols = c("Cathode Mix Share")) %>% group_by(`Cathode Mix`) %>%
  arrange(`Cathode Mix`, Sale_Year, .by_group = TRUE)

chem_scens <- list(future_match, final_adjusted_mix)



### RUN CAPACITY SCENARIOS
capacity_chem_scenarios <- function(batt_cap_df,chem_df) {

  ### RECYCLE in Future- cut only those sales years with the projection
  future_recycle_cap <- merge(
    batt_cap_df, 
    future_recycle_type, 
    by = c("Sale_Year", "Segment", "Propulsion"), all.x = TRUE)
  
  # Apply avg battery size per powertrain and type
  future_recycle_cap$LIB_recycle_kwh <- future_recycle_cap$LIB_recycle_total * future_recycle_cap$`Projected Avg Batt Cap (kwh/batt)`
  
  # Keep only relevant columns
  future_recycle_cap <- future_recycle_cap %>%
    select(`Year`, `Sale_Year`, State, `Segment`,`Propulsion`, 
           `LIB_recycle_kwh`)
  
  ### APPLY BENCHMARK
  future_recycle_chem <- left_join(future_recycle_cap, chem_df, by = c("Sale_Year"), relationship = 'many-to-many') 
  future_recycle_chem$Cathode_kwh_state<- future_recycle_chem$LIB_recycle_kwh * future_recycle_chem$`Cathode Mix Share`
  
  future_minerals <- left_join(future_recycle_chem, mineral_intensity, by = "Cathode Mix", relationship = 'many-to-many') %>%
    mutate(`Available Recycled Minerals (kg)` = `kg_per_kwh` * `Cathode_kwh_state`) %>%
    select(`Year`, `Sale_Year`, State, Mineral, `Available Recycled Minerals (kg)`)
  
  future_final <- future_minerals %>%
    group_by(Year, State, Mineral) %>%
    summarise(`Available Recycled Minerals (kg)` = sum(`Available Recycled Minerals (kg)`, na.rm = TRUE), .groups = "drop") %>%
    filter(!is.na(`Mineral`))
  
  return(future_final)
  
  }
  

# Set names for scenarios
names(batt_scen) <- c("Baseline Battery", "15% Lower Battery")
names(chem_scens) <- c("Original Chemistry", "High LFP Chemistry")

# Use `crossing()` to create all 4 combinations
scenario_combos <- crossing(
  Batt = names(batt_scen),
  Chem = names(chem_scens)
)

# Safe version of your scenario function with fallback
safe_capacity_chem_scenarios <- function(batt_name, chem_name) {
  res <- tryCatch({
    df <- capacity_chem_scenarios(
      batt_cap_df = batt_scen[[batt_name]],
      chem_df = chem_scens[[chem_name]]
    )
    
    # If NULL or 0 rows, return empty tibble with correct columns
    if (is.null(df) || nrow(df) == 0) {
      tibble(
        Year = integer(),
        Sale_Year = integer(),
        State = character(),
        Mineral = character(),
        `Available Recycled Minerals (kg)` = numeric(),
        Battery_Scenario = character(),
        Chemistry_Scenario = character()
      )
    } else {
      df %>%
        mutate(
          Battery_Scenario = batt_name,
          Chemistry_Scenario = chem_name
        )
    }
  }, error = function(e) {
    warning(paste("Error in scenario:", batt_name, "/", chem_name, "->", e$message))
    tibble(
      Year = integer(),
      Sale_Year = integer(),
      State = character(),
      Mineral = character(),
      `Available Recycled Minerals (kg)` = numeric(),
      Battery_Scenario = character(),
      Chemistry_Scenario = character()
    )
  })
  
  return(res)
}


# Run all scenarios using pmap safely
all_scenarios <- scenario_combos %>%
  mutate(
    result = pmap(
      list(Batt, Chem),
      safe_capacity_chem_scenarios
    )
  )

# Combine all results
cap_chem_results <- bind_rows(all_scenarios$result)

cap_chem_results <- cap_chem_results %>%
  mutate(Scenario = paste(Battery_Scenario, Chemistry_Scenario, sep = " - "))



# Combine historical and future projections
final_future_hist <- bind_rows(hist_final_expanded, cap_chem_results)

summary_final_future_hist <- final_future_hist %>%
  group_by(Scenario, Year, State, Mineral) %>%
  summarise(
    `Available Recycled Minerals (kg)` = sum(`Available Recycled Minerals (kg)`, na.rm = TRUE),
    .groups = "drop"
  )

#### PLOTTING
# Get all unique states
states <- unique(summary_final_future_hist$State)
output_file <- "/Users/elsawefes-potter/Documents/Critical_Minerals_Pablo/recycled_minerals_by_state.pdf"


pdf(output_file, width = 12, height = 8)


# Loop over states and make one page per state
for (s in states) {
  state_data <- summary_final_future_hist %>%
    filter(State == s)
  
  if (nrow(state_data) == 0 || all(is.na(state_data$Mineral)) || length(unique(state_data$Mineral[!is.na(state_data$Mineral)])) == 0) {
    warning(paste("Skipping state due to no data or no valid Mineral:", s))
    next
  }
  
  p <- ggplot(state_data, aes(x = Year, y = `Available Recycled Minerals (kg)`,
                              color = Scenario, linetype = Scenario)) +
    geom_line() +
    facet_wrap(~ Mineral, scales = "free_y", ncol = 2) +  # adjust ncol/nrow as needed
    labs(
      title = paste("Minerals in Recycled Batteries –", s),
      x = "Year",
      y = "Minerals in Recycled Batteries (kg)",
      color = "Scenario",
      linetype = "Scenario"
    ) +
    theme_minimal() +
    theme(
      legend.position = "bottom",
      legend.text = element_text(size = 7),
      legend.key.width = unit(1.5, "cm"),
      plot.margin = margin(t = 10, r = 80, b = 50, l = 10),
      legend.box.margin = margin(t = 10)
    ) +
    coord_cartesian(clip = "off")
  
    print(p)
}


# Close the PDF device
dev.off()


