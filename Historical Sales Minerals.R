library(dplyr)
library(tidyverse)
library(data.table)
library(tidyr)
library(stringr)
library(purrr)
### Uses EV Volumes battery capacity and chemistry on

data_folder = "/Users/elsawefes-potter/Documents/Critical_Minerals_Pablo"
mineral_intensity <- read_excel(file.path(data_folder, "Mineral_Intensity(2).xlsx"), na = "") %>%
  filter(!Mineral %in% c("Phosphorus", "Stainless steel")) %>%
  rename(`Cathode Mix` = chemistry)

EVLIB_Flows_US_hist <- read_csv("/Users/elsawefes-potter/Documents/Critical_Minerals_Pablo/EVLIB_Flows_detail_ACCII.csv") %>%
  rename(State_Province = State)
EVLIB_Flows_CA_hist <- read_csv("/Users/elsawefes-potter/Documents/Critical_Minerals_Pablo/Canada-EVLIB_Flows_detail_ACCII.csv") %>%
  rename(State_Province = State)
BESSLIB_Flows_US_hist <- read_csv("/Users/elsawefes-potter/Documents/Critical_Minerals_Pablo/BESS_Retire_Vector_byStateSegProp_ACCII.csv") %>%
  rename(LIB_recycling_vector = BESS_retire_vector) %>%
  rename(`State_Province` = `State`)


EVLIB_Flows_hist <- bind_rows(EVLIB_Flows_US_hist, EVLIB_Flows_CA_hist)
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
EVLIB_Flows_hist$LIB_recycling_vector <- Map(
  name_vector_with_years,
  EVLIB_Flows_hist$LIB_recycling_vector,
  EVLIB_Flows_hist$Year
)

BESSLIB_Flows_US_hist$LIB_recycling_vector <- Map(
  name_vector_with_years,
  BESSLIB_Flows_US$LIB_recycling_vector,
  BESSLIB_Flows_US$Year
)



hist_recycle_type <- EVLIB_Flows_hist %>%
  mutate(
    recycle_df = map(LIB_recycling_vector, ~ {
      tibble(
        Sale_Year = as.integer(names(.x)),
        LIB_recycle_total = as.numeric(.x)
      )
    })
  ) %>%
  select(State_Province, Segment, Propulsion, Year, recycle_df)  %>% # keep original Year here
  unnest(cols = recycle_df) %>% 
  filter(Sale_Year <= 2025,
         Propulsion != "FCEV") 

BESS_hist_recycle_type <- BESSLIB_Flows_US_hist %>%
  mutate(
    recycle_df = map(LIB_recycling_vector, ~ {
      tibble(
        Sale_Year = as.integer(names(.x)),
        LIB_recycle_total = as.numeric(.x)
      )
    })
  ) %>%
  select(State_Province, Segment, Propulsion, Year, recycle_df) %>%  # keep original Year here
  unnest(cols = recycle_df) %>% 
  filter(Sale_Year <= 2025,
         Propulsion != "FCEV") %>%
  mutate(across(everything(), ~ replace_na(.x, 0)))

hist_recycle_type <- full_join(hist_recycle_type, BESS_hist_recycle_type, by = c("State_Province","Segment","Propulsion","Year","Sale_Year")) %>%
  mutate(across(everything(), ~ replace_na(.x, 0))) %>%
  mutate(LIB_recycle_total = LIB_recycle_total.x+LIB_recycle_total.y)


###CHEMISTRY
# Group and sum
chem_Mwh <- usa_sales_filtered %>%
  group_by(`Sale Year`, `Global Segment`, Propulsion, `Cathode Mix`) %>%
  summarise(`Total Mwh` = sum(`Total Mwh`, na.rm = TRUE), .groups = "drop") %>%
  filter(`Total Mwh` != 0)

# Compute Share of Avg Chem
chem_Mwh <- chem_Mwh %>%
  group_by(`Sale Year`, `Global Segment`, Propulsion) %>%
  mutate(`Share of Avg Chem` = `Total Mwh` / sum(`Total Mwh`, na.rm = TRUE)) %>%
  ungroup()

# Pivot
#chem_Mwh <- chem_Mwh %>%
#pivot_wider(names_from = Propulsion, values_from = `Share of Avg Chem`, values_fill = 0)

# Replace NA and -Inf with 0 (if any remain)
chem_Mwh[is.na(chem_Mwh)] <- 0
chem_Mwh <- chem_Mwh %>%
  mutate(across(everything(), ~ ifelse(is.infinite(.), 0, .)))

chem_Mwh <- chem_Mwh %>%
  rename(Segment = "Global Segment", Sale_Year = "Sale Year")

### BATTERY CAPACITY
# Group sales and MWh
batt_cap_sales <- usa_sales_filtered %>%
  group_by(`Sale Year`, `Global Segment`, Propulsion) %>%
  summarise(`Total Sales` = sum(`Total Sales`, na.rm = TRUE), .groups = "drop") %>%
  filter(`Total Sales` != 0)

batt_cap_Mwh <- usa_sales_filtered %>%
  group_by(`Sale Year`, `Global Segment`, Propulsion) %>%
  summarise(`Total Mwh` = sum(`Total Mwh`, na.rm = TRUE), .groups = "drop") %>%
  filter(`Total Mwh` != 0)

# Merge and compute Avg Battery Capacity
batt_cap_merged <- merge(batt_cap_sales, batt_cap_Mwh, 
                         by = c("Sale Year", "Global Segment", "Propulsion"))

batt_cap_merged <- batt_cap_merged %>%
  mutate(`Avg Batt Cap (kwh/batt)` = (`Total Mwh` / `Total Sales`) * 1000)

# Pivot
#batt_cap_merged <- batt_cap_merged %>%
  #pivot_wider(names_from = Propulsion, values_from = `Avg Batt Cap (kwh/batt)`, values_fill = 0)

# Replace NA and -Inf with 0 (just in case)
batt_cap_merged[is.na(batt_cap_merged)] <- 0
batt_cap_merged <- batt_cap_merged %>%
  mutate(across(everything(), ~ ifelse(is.infinite(.), 0, .)))

batt_cap_merged <- batt_cap_merged %>%
  rename(Segment = "Global Segment", Sale_Year = "Sale Year")


# chem_Mwh <- chem_Mwh %>% 
#   pivot_longer(
#     cols = c(BEV, PHEV, FCEV),
#     names_to = "Propulsion",
#     values_to = "kwh of Cathode"
#     )


### Apply disaggregations
###BATT CAP
hist_recycle_cap <- merge(batt_cap_merged, hist_recycle_type, by = c("Sale_Year", "Segment", "Propulsion"), all.x = TRUE)

# Apply avg battery size per powertrain and type
hist_recycle_cap$LIB_recycle_kwh <- hist_recycle_cap$LIB_recycle_total * hist_recycle_cap$`Avg Batt Cap (kwh/batt)`

# Keep only relevant columns
hist_recycle_cap <- hist_recycle_cap %>%
  select(`Year`, `Sale_Year`, State_Province, `Segment`,`Propulsion`, 
         `LIB_recycle_kwh`)


###CHEMISTRY

# Replace cathode mix values
replacement <- c(
  'NCA (unspecified)'='NCA',
  'LFP (unspecified)'='LFP',
  'LMO (unspecified)'='LMO',
  'LTO (unspecified)'='LMO-LTO',
  'NMC 111 + NCA'= 'NMCA 89:4:4:3',
  'NMC 811 + 111'= 'NMC 811',
  '70 % NMC 111 + 30 % NMC 622'= 'NMC 111',
  'NMC 422'='NMC 532',
  'NMC 111 + LMO'='NMC 111',
  'LMO+NMC+NCA'='NMCA 89:4:4:3'
)

chem_Mwh$`Cathode Mix` <- recode(chem_Mwh$`Cathode Mix`, !!!replacement)


##alotting 0.1% and 0.3% to BEV and PHEV respectively of the strange chemistries
##18% and 29% BEV and PHEV are NMC (unspecified)
chem_Mwh <- chem_Mwh %>% filter(Propulsion != "FCEV")

# cathode_mix_filter <- chem_Mwh$`Cathode Mix` %in% 
#   c("tba (unspecified)", "NiMH (unspecified)", "LMP (unspecified)")

# ## verify it is very small
# total_sums <- chem_Mwh %>%
#   filter(cathode_mix_filter) %>%
#   group_by(Propulsion) %>%
#   summarise(total_kwh = sum(`kwh of Cathode`, na.rm = TRUE), .groups = "drop")


  ##Apply mineral intensity
  ## remove FCEVs totally
  ## assign any chemistries not in the min intensity data set to the top in that year, pt and veh type

# Get max per group
max_values <- chem_Mwh %>%
  group_by(`Sale_Year`, `Segment`, `Propulsion`) %>%
  slice_max(order_by = `Share of Avg Chem`, n = 1, with_ties = FALSE) %>%
  ungroup()

fix_NMC <- chem_Mwh %>%
  filter(str_detect(`Cathode Mix`, "NMC"),
         !str_detect(`Cathode Mix`, "unspecified"))

max_NMC <- fix_NMC %>%
  group_by(`Sale_Year`, `Segment`, Propulsion) %>%
  slice_max(order_by = `Share of Avg Chem`, n = 1, with_ties = FALSE)

# Merge and fix NMC (unspecified)
NMC_match <- left_join(max_values, max_NMC,
                       by = c("Sale_Year", "Segment", "Propulsion"),
                       suffix = c("_x", "_y"))

NMC_match$`Cathode Mix_x`[NMC_match$`Cathode Mix_x` == "NMC (unspecified)"] <- 
NMC_match$`Cathode Mix_y`[NMC_match$`Cathode Mix_x` == "NMC (unspecified)"]

max_values <- NMC_match %>%
  select(`Sale_Year`, `Segment`, `Cathode Mix` = `Cathode Mix_x`, Propulsion)

# # Add manually fixed row for PHEV (2025, Car)
# max_values <- bind_rows(max_values, tibble(
#   `Sale Year` = 2025,
#   `Global Segment` = "Car",
#   `Cathode Mix` = "NMC 532",
#   Powertrain = "PHEV"
# ))

# melt_hist_recycle_chem <- hist_recycle_chem %>%
#   pivot_longer(cols = c(`Recycle BEV (kwh)`, `Recycle PHEV (kwh)`),
#                names_to = "Powertrain", values_to = "Total (kwh) by Cathode Mix") %>%
#   mutate(Powertrain = recode(Powertrain,
#                              "Recycle BEV (kwh)" = "BEV",
#                              "Recycle PHEV (kwh)" = "PHEV"))

chem_Mwh <- left_join(chem_Mwh, max_values, 
                      by = c("Sale_Year", "Segment","Propulsion"))

mask_mins <- chem_Mwh$`Cathode Mix.x` %in% 
  c("tba (unspecified)", "NiMH (unspecified)", "LMP (unspecified)", "NMC (unspecified)")

chem_Mwh$`Cathode Mix.x`[mask_mins] <- chem_Mwh$`Cathode Mix.y`[mask_mins]

chem_Mwh <- chem_Mwh %>%
  select(-`Cathode Mix.y`) %>%
  rename(`Cathode Mix` = `Cathode Mix.x`)



hist_recycle_chem <- merge(
  chem_Mwh, 
  hist_recycle_cap, 
  by = c("Sale_Year", "Propulsion", "Segment"),
  all.x = TRUE)


hist_recycle_chem$Cathode_kwh_state<- hist_recycle_chem$LIB_recycle_kwh * hist_recycle_chem$`Share of Avg Chem`

hist_recycle_chem <- hist_recycle_chem %>%
  mutate(Sale_Year = as.integer(Sale_Year)) %>%
  select(Year, Sale_Year, State_Province, `Cathode Mix`, Cathode_kwh_state, LIB_recycle_kwh) 


hist_final <- left_join(hist_recycle_chem, mineral_intensity, by = "Cathode Mix", relationship = "many-to-many") %>%
  mutate(`Available Recycled Minerals (kg)` = `kg_per_kwh` * `Cathode_kwh_state`) %>%
  select(`Sale_Year`, State_Province, Mineral, `Year`, `Available Recycled Minerals (kg)`) 

hist_final <- hist_final %>%
  group_by(Year, State_Province, Mineral) %>%
  summarise(`Available Recycled Minerals (kg)` = sum(`Available Recycled Minerals (kg)`, na.rm = TRUE), .groups = "drop") %>%
  filter(!is.na(`Mineral`))

# 
# ### EDIT HIST ONE TO TAKE SCENARIOS
# scenarios <- cap_chem_results %>%
#   distinct(Battery_Scenario, Chemistry_Scenario) %>%
#   mutate(Scenario = paste(Battery_Scenario, Chemistry_Scenario, sep = " - "))
# 
# hist_final_expanded <- hist_final %>%
#   crossing(scenarios %>% select(Scenario)) %>%
#   filter(Year >= 2025)
# 
# recycle_cols <- c(
#   "Available Recycled Minerals Current Cap (kg)",
#   "Available Recycled Minerals (w Scrap) (kg)",
#   "Available Recycled Minerals Increased R and Scrap (kg)",
#   "Available Recycled Minerals Increased R Same Scrap (kg)",
#   "Available Recycled Minerals Increased R No Scrap (kg)",
#   "Available Recycled Minerals No R Restraint (kg)"
# )
# 
# # Duplicate hist_final across all scenario columns
# hist_final_expanded <- hist_final_expanded %>%
#   mutate(
#     `Available Recycled Minerals Current Cap (kg)` = `Available Recycled Minerals (kg)`,
#     `Available Recycled Minerals (w Scrap) (kg)` = `Available Recycled Minerals (kg)`,
#     `Available Recycled Minerals Increased R and Scrap (kg)` = `Available Recycled Minerals (kg)`,
#     `Available Recycled Minerals Increased R Same Scrap (kg)` = `Available Recycled Minerals (kg)`,
#     `Available Recycled Minerals Increased R No Scrap (kg)` = `Available Recycled Minerals (kg)`,
#     `Available Recycled Minerals No R Restraint (kg)` = `Available Recycled Minerals (kg)`
#   )
