install.packages("writexl")
install.packages("colorspace")
library(dplyr)
library(purrr)
library(readxl)
library(dplyr)
library(tidyr)
library(stringr)
library(tidyverse)
library(writexl)
library(colorspace)

### Run Order:      ##EV_Volumes_Clean
                    ##Historical Sales Minerals
                    ##Manufacturing_Recycling_Demand
                    ##Future Recycling Minerals
                    ##Future Demand Minerals
                    ##Change to Delay at appropriate locations in following and run again
                          ## Manufacturing_Recycling_Demand
                          ## Future Recycling Minerals
                          ## Future Demand Minerals
                    ##Run all again w Repeal


### SCENARIOS-- 
##50% new demand is LFP --- ##30% of tesla is LFP -- canceled
## 15% reduction batt cap and continuation batt cap


## DATA INPUTS
### Start up phase of 4 years starts at about 20 on average and decreases to 4-12%
cathode_projections <- read_excel("/Users/elsawefes-potter/Documents/Critical_Minerals_Pablo/Cathode Projections (1).xlsx", sheet = "Sheet1")
EVLIB_Flows <- read_csv("/Users/elsawefes-potter/Documents/Critical_Minerals_Pablo/EVLIB_Flows_detail_ACCII.csv")
EV_Flows <- read_csv("/Users/elsawefes-potter/Documents/Critical_Minerals_Pablo/ClosedLoop_AddRetire_byStateSegment_ACCII.csv") %>%
  select(State, Segment, Year, add_BEV, add_PHEV) %>%
  group_by(State, Segment, Year) %>% summarise(add_BEV = sum(add_BEV, na.rm = TRUE), add_PHEV = sum(add_PHEV, na.rm = TRUE)) %>%
  rename(BEV = add_BEV, PHEV = add_PHEV) %>%
  pivot_longer(cols = c(BEV, PHEV),
               names_to = "Propulsion",
               values_to = "Add_EV")


EVLIB_Flows_CA <- read_csv("/Users/elsawefes-potter/Documents/Critical_Minerals_Pablo/Canada-EVLIB_Flows_detail_ACCII.csv") 
EV_Flows_CA <- read_csv("/Users/elsawefes-potter/Documents/Critical_Minerals_Pablo/Canada-ClosedLoop_AddRetire_byStateSegment_ACCII.csv") %>%
  select(State, Segment, Year, add_BEV, add_PHEV) %>%
  group_by(State, Segment, Year) %>% summarise(add_BEV = sum(add_BEV, na.rm = TRUE), add_PHEV = sum(add_PHEV, na.rm = TRUE)) %>%
  rename(BEV = add_BEV, PHEV = add_PHEV) %>%
  pivot_longer(cols = c(BEV, PHEV),
               names_to = "Propulsion",
               values_to = "Add_EV")

EV_Flows <- bind_rows(EV_Flows, EV_Flows_CA)
EVLIB_Flows <- bind_rows(EVLIB_Flows, EVLIB_Flows_CA)
data_folder = "/Users/elsawefes-potter/Documents/Critical_Minerals_Pablo"
mineral_intensity <- read_excel(file.path(data_folder, "Mineral_Intensity(2).xlsx"), na = "") %>%
  filter(!Mineral %in% c("Phosphorus", "Stainless steel"))%>%
  rename("Cathode Mix" = chemistry)


MX_dereg <- read_csv(file.path(data_folder, "Mexico_Dereg_EV_vectors_USsurvival.csv"), na = "") %>%
  select(Year, Total_vec) %>%
  rowwise() %>%
  mutate(
    Total_vec = list({
      v <- as.numeric(strsplit(Total_vec, "\\|")[[1]])
      names(v) <- Year - (seq_along(v) - 1)
      v
    })
  ) %>%
  ungroup()  %>%
  mutate(
    total_df = map(Total_vec, ~ {
      tibble(
        Sale_Year = as.integer(names(.x)),
        Total_sales = as.numeric(.x)
      )
    })
  ) %>%
  select(Year, total_df) %>%
  unnest(total_df)



## THIS IS ASSUMING US/CA distribution of segment and propulsion retiring each year --> using MX age distribution
## could use also US/CA age dist of seg/prop 
## so of the 2020 retirements of 2014 cars x% BEV y% PHEV instead of just of 2020 retirements x% BEV y% PHEV
## if 95% BEV in 2020 but mostly recent bevs and older year phevs rather than 95% of each sale_year

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
  unnest(cols = recycle_df) %>% rename(State_Province = State)



future_proportion_segment_prop <- future_recycle_type %>% 
  group_by(Year, Sale_Year, Segment, Propulsion) %>%
  summarise(
    LIB_recycle_total = sum(LIB_recycle_total, na.rm = TRUE),
    .groups = "drop"
  ) %>% group_by(Year, Sale_Year) %>%
  mutate(percent  = LIB_recycle_total/sum(LIB_recycle_total)) %>%
  select(Year, Sale_Year, Segment, Propulsion, percent)



hist_proportion_segment_prop <- hist_recycle_type %>% 
  group_by(Year, Sale_Year, Segment, Propulsion) %>%
  summarise(
    LIB_recycle_total = sum(LIB_recycle_total, na.rm = TRUE),
    .groups = "drop"
  ) %>% group_by(Year, Sale_Year) %>%
  mutate(percent  = LIB_recycle_total/sum(LIB_recycle_total)) %>%
  select(Year, Sale_Year, Segment, Propulsion, percent)

future_hist_proportions <- bind_rows(hist_proportion_segment_prop, future_proportion_segment_prop) %>%
  mutate(percent = replace(percent, is.nan(percent), 0))

MX_Flows <- MX_dereg %>% 
  left_join(future_hist_proportions, by =c("Sale_Year","Year")) %>% 
  mutate(LIB_recycle_total = Total_sales * percent,
         State_Province = "MX") %>% 
  filter(!is.na(LIB_recycle_total)) %>%
  select(Year, State_Province, Sale_Year, Segment,Propulsion, LIB_recycle_total)
  
future_recycle_type <- bind_rows(future_recycle_type, MX_Flows)

###LCO https://www.fluxpower.com/blog/what-is-the-energy-density-of-a-lithium-ion-battery?utm_source=chatgpt.com
specific_energy <- read_csv(file.path(data_folder, "Specific_Energy (-Energy BatPac).csv")) %>% rename (`Cathode Mix` = `Battery Chem`) 
specific_energy <- specific_energy %>%
  bind_rows(
    tibble(
      `Cathode Mix` = "NMCA",
      Pack_kg_kwh = (specific_energy$Pack_kg_kwh[specific_energy$`Cathode Mix` == "NCA"] +
                       specific_energy$Pack_kg_kwh[specific_energy$`Cathode Mix` == "NMC 811"])/2,
      Cell_kg_kwh = (specific_energy$Cell_kg_kwh[specific_energy$`Cathode Mix` == "NCA"] +
                       specific_energy$Cell_kg_kwh[specific_energy$`Cathode Mix` == "NMC 811"])/2
    ), 
    tibble (
      `Cathode Mix` = "High/Mid NMC",
      Pack_kg_kwh = (specific_energy$Pack_kg_kwh[specific_energy$`Cathode Mix` == "NMC 622"] +
                       specific_energy$Pack_kg_kwh[specific_energy$`Cathode Mix` == "NMC 811"])/2,
      Cell_kg_kwh = (specific_energy$Cell_kg_kwh[specific_energy$`Cathode Mix` == "NMC 622"] +
                       specific_energy$Cell_kg_kwh[specific_energy$`Cathode Mix` == "NMC 811"])/2
    ),
    tibble(
      `Cathode Mix` = "LCO",
      Pack_kg_kwh = 5.85,
      Cell_kg_kwh = 3.85
    )
  )

specific_energy <- specific_energy %>%
  mutate(`Cathode Mix` = if_else(`Cathode Mix` == "NMC 333", "NMC 111", `Cathode Mix`))


## Minerals
batpac_scrap_min <- read_csv("/Users/elsawefes-potter/Documents/Critical_Minerals_Pablo/Mins_in_Scrap (-Energy BatPac).csv") %>% select(where(~ !all(is.na(.)))) 
colnames(batpac_scrap_min) <- c("Product_Abbrev", "Mineral", "Value")

### all by yr
batpac_scrap_min <- batpac_scrap_min %>% 
  mutate(`kg/Gwh` = Value/50) #(kg/yr) *1/(50000000 kwh/yr) * 1000000 kwh/Gwh

mineral_map <- c(
  "Li, kg/yr" = "Lithium",
  "Ni, kg/yr" = "Nickel",
  "Co, kg/yr" = "Cobalt",
  "Mn, kg/yr" = "Manganese",
  "C, kg/yr"  = "Carbon",
  "Al, kg/yr" = "Aluminum",
  "Cu, kg/yr" = "Copper"
)
batpac_scrap_min <- batpac_scrap_min %>%
  mutate(Mineral = recode(Mineral, !!!mineral_map),
         Product_Abbrev = str_trim(Product_Abbrev), 
         `kg/Gwh` = as.numeric(`kg/Gwh`))


###SIDEBAR getting batpac for NMCA from ratios for NMC 622 and NMCA in min intensity
nmca_nmc <- mineral_intensity %>%
  filter(`Cathode Mix` %in% c("NMCA 89:4:4:3", "NMC 622"))

summary_chem <- nmca_nmc %>%
  group_by(`Cathode Mix`, Mineral) %>%
  summarise(
    total_kg_per_kwh = sum(kg_per_kwh, na.rm = TRUE),
    .groups = "drop"
  )

# Pivot wider and calculate ratio per mineral
ratio_nmca_nmc <- summary_chem %>%
  pivot_wider(
    names_from = `Cathode Mix`,
    values_from = total_kg_per_kwh
  ) %>%
  mutate(
    ratio = `NMCA 89:4:4:3` / `NMC 622`,
    ratio = as.numeric(ratio)
  )


nmca_rows <- bat_pac %>%
  filter(Product_Abbrev == "NMC 622") %>%       
  left_join(ratio_nmca_nmc, by = "Mineral") %>%
  mutate(
    Product_Abbrev = "NMCA",
    `kg/Gwh` = `kg/Gwh` * ratio
  ) %>%
  select(-ratio)

nmca_rows <- nmca_rows %>%
  select(-matches("^NMC 622$|^NMCA 89:4:4:3$"))

# Combine original bat_pac with NMCA rows
batpac_scrap_min_w_nmca <- bind_rows(batpac_scrap_min, nmca_rows)

mins_in_scrap <- bat_pac_w_nmca %>% rename(`Cathode Mix` = Product_Abbrev) %>% select(-Value) %>% filter(!is.na(`Cathode Mix`))
all_mins <- mineral_intensity %>% full_join(mins_in_scrap, by = c("Cathode Mix", "Mineral")) %>%
  mutate(
    kg_per_kwh = as.numeric(kg_per_kwh),
    `kg/Gwh`   = as.numeric(`kg/Gwh`)
  ) %>% mutate(
    kg_per_kwh = ifelse(is.na(kg_per_kwh), 0, kg_per_kwh),
    `kg/Gwh`   = ifelse(is.na(`kg/Gwh`), 0, `kg/Gwh`)
  ) 



## RECYCLING DATA CLEAN-- All refining is practically hydrometallurgical anyway
US_CA_Recycle <- recycling_tonnes_total %>% select(-c(Delay_Cumulative_black_mass_cap, Delay_Cumulative_refining_cap, Delay_Full_Recycle)) %>%
  rename(Black_Mass_MT = Cumulative_black_mass_cap, Refining_MT = Cumulative_refining_cap) %>%
  complete(Year = 2025:2050) %>%
  fill(Black_Mass_MT, Refining_MT, Full_Recycle, .direction = "down") %>%
  ungroup()


Delay_US_CA_Recycle <- recycling_tonnes_total %>% select(-c(Cumulative_black_mass_cap,Cumulative_refining_cap, Full_Recycle)) %>%
  rename(Black_Mass_MT = Delay_Cumulative_black_mass_cap, Refining_MT = Delay_Cumulative_refining_cap, Full_Recycle = Delay_Full_Recycle) %>% 
  complete(Year = 2025:2050) %>%
  fill(Black_Mass_MT, Refining_MT, Full_Recycle, .direction = "down") %>%
  ungroup()
  

## REESTABLISH MANUFACTURING SHARES BY STATE AND CHEMISTRY BUT FOR PRODUCTION 
## apply scrap through mineral intensity mins_in_scrap
p_all_manufacturing <- calendar %>%
  left_join(all_manufacturing, by = "State_Province", relationship = "many-to-many") %>%
  arrange(State_Province, Year) %>%
  group_by(State_Province, Year_Online) %>%
  mutate(
    Production_Adjusted_Down = ifelse(Year < Year_Online, 0, Downstream),
    Production_Adjusted_Mid  = ifelse(Year < Year_Online, 0, Midstream)
  ) %>%
  ungroup() %>%
  group_by(Year, State_Province) %>%
  summarise(
    Production_Adjusted_Down = sum(Production_Adjusted_Down, na.rm = TRUE),
    Production_Adjusted_Mid  = sum(Production_Adjusted_Mid, na.rm = TRUE),
    .groups = "drop_last"
  ) %>%
  group_by(Year) %>%
  mutate(
    Share_of_Year_Prod_Down = Production_Adjusted_Down / sum(Production_Adjusted_Down, na.rm = TRUE),
    Share_of_Year_Prod_Mid  = Production_Adjusted_Mid  / sum(Production_Adjusted_Mid,  na.rm = TRUE)
  ) %>%
  ungroup() %>%
  mutate(
    State_Province = if_else(State_Province == "SLP ", "MX", State_Province)
  )


p_delayed_manufacturing <- calendar_delayed %>%
  left_join(delayed_manufacturing, by = "State_Province", relationship = "many-to-many") %>%
  arrange(State_Province, Year) %>%
  group_by(State_Province, Year_Online) %>%
  mutate(
    Production_Adjusted_Down = ifelse(Year < Year_Online, 0, Downstream),
    Production_Adjusted_Mid  = ifelse(Year < Year_Online, 0, Midstream)
  ) %>%
  ungroup() %>%
  group_by(Year, State_Province) %>%
  summarise(
    Production_Adjusted_Down = sum(Production_Adjusted_Down, na.rm = TRUE),
    Production_Adjusted_Mid  = sum(Production_Adjusted_Mid, na.rm = TRUE),
    .groups = "drop_last"
  ) %>%
  group_by(Year) %>%
  mutate(
    Share_of_Year_Prod_Down = Production_Adjusted_Down / sum(Production_Adjusted_Down, na.rm = TRUE),
    Share_of_Year_Prod_Mid  = Production_Adjusted_Mid  / sum(Production_Adjusted_Mid,  na.rm = TRUE)
  ) %>%
  ungroup() %>%
  mutate(
    State_Province = if_else(State_Province == "SLP ", "MX", State_Province)
  )


p_nat_manu <- p_all_manufacturing %>% group_by(Year) %>%
  summarise(Production_Adjusted_Down = sum(Production_Adjusted_Down, na.rm = TRUE), 
            Production_Adjusted_Mid = sum(Production_Adjusted_Mid, na.rm= TRUE))

p_nat_manu_delayed <- p_delayed_manufacturing %>% group_by(Year) %>%
  summarise(Production_Adjusted_Down = sum(Production_Adjusted_Down, na.rm = TRUE), 
            Production_Adjusted_Mid = sum(Production_Adjusted_Mid, na.rm= TRUE))

p_cap_vs_manufac <- left_join(
  nat_cap_add,
  p_nat_manu,
  by = "Year"
) 

p_cap_vs_delayed_manu <- left_join(
  nat_cap_add, 
  p_nat_manu_delayed, 
  by = "Year")

## Project manufacturing based on the north american demand
p_manu_projected <- p_cap_vs_manufac %>%
  fill(Production_Adjusted_Down, Production_Adjusted_Mid,
       .direction = "down") %>%
  mutate(
    Production_Adjusted_Down_proj = case_when(
      Production_Adjusted_Down < Add_LIB_Gwh_proj ~ Add_LIB_Gwh_proj,
      TRUE ~ Production_Adjusted_Down
    ), Production_Adjusted_Mid_proj = case_when(
      Production_Adjusted_Mid < Add_LIB_Gwh_proj ~ Add_LIB_Gwh_proj,
      TRUE ~ Production_Adjusted_Mid
    ), 
    ## 15% reduction
    Production_Adjusted_Down_15 = case_when(
      Production_Adjusted_Down < Add_LIB_Gwh_15 ~ Add_LIB_Gwh_15,
      TRUE ~ Production_Adjusted_Down
    ), Production_Adjusted_Mid_15 = case_when(
      Production_Adjusted_Mid < Add_LIB_Gwh_15 ~ Add_LIB_Gwh_15,
      TRUE ~ Production_Adjusted_Mid
    )) %>% 
  select(-c(Production_Adjusted_Down, Production_Adjusted_Mid))


p_manu_delayed <- p_cap_vs_delayed_manu %>%  
  fill(Production_Adjusted_Down, Production_Adjusted_Mid,
       .direction = "down") %>%
  mutate(
    Production_Adjusted_Down_proj = case_when(
      Production_Adjusted_Down < Add_LIB_Gwh_proj ~ Add_LIB_Gwh_proj,
      TRUE ~ Production_Adjusted_Down
    ), Production_Adjusted_Mid_proj = case_when(
      Production_Adjusted_Mid < Add_LIB_Gwh_proj ~ Add_LIB_Gwh_proj,
      TRUE ~ Production_Adjusted_Mid
    ), 
    ## 15% reduction
    Production_Adjusted_Down_15 = case_when(
      Production_Adjusted_Down < Add_LIB_Gwh_15 ~ Add_LIB_Gwh_15,
      TRUE ~ Production_Adjusted_Down
    ), Production_Adjusted_Mid_15 = case_when(
      Production_Adjusted_Mid < Add_LIB_Gwh_15 ~ Add_LIB_Gwh_15,
      TRUE ~ Production_Adjusted_Mid
    )) %>% 
  select(-c(Production_Adjusted_Down, Production_Adjusted_Mid))


## assumption that continuing same amount of production and scrap and same split of where facilities 
# are and range at until around 2035 --> after that do ramp down of scrap 2028-2035 

### current and constructed-- assume it does go online- some go to BESS- scrap it's the same 
### planned either on or significantly delayed


### apply chemistries 
p_projected_manufac_by_chem <- tidyr::crossing(p_manu_projected, all_manu_chem) %>%
  mutate(Prod_proj_down = Production_Adjusted_Down_proj * Chem_Share,
         Prod_15_down = Production_Adjusted_Down_15 * Chem_Share, 
         Prod_proj_mid = Production_Adjusted_Mid_proj * Chem_Share,
         Prod_15_mid = Production_Adjusted_Mid_15 * Chem_Share, 
  ) %>%
  select(Year,`Cathode Mix`, Prod_proj_down, Prod_15_down, Prod_proj_mid, Prod_15_mid) 


p_delayed_manufac_by_chem <-  tidyr::crossing(p_manu_delayed, all_manu_chem) %>%
  mutate(Prod_proj_down = Production_Adjusted_Down_proj * Chem_Share,
         Prod_15_down = Production_Adjusted_Down_15 * Chem_Share, 
         Prod_proj_mid = Production_Adjusted_Mid_proj * Chem_Share,
         Prod_15_mid = Production_Adjusted_Mid_15 * Chem_Share, 
  ) %>%
  select(Year,`Cathode Mix`, Prod_proj_down, Prod_15_down, Prod_proj_mid, Prod_15_mid) 


### MANUFACTURING
all_manufacturing_expanded_complete_yrs <- p_all_manufacturing %>% filter(Year <= 2030) %>%
  group_by(State_Province) %>%
  # Ensure all years 2025–2050 exist for each state
  complete(Year = 2025:2050) %>%
  # Fill missing values downward (last observation carried forward)
  fill(Share_of_Year_Prod_Down, Share_of_Year_Prod_Mid, .direction = "down") %>%
  ungroup() %>% select(Year, State_Province, Share_of_Year_Prod_Down, Share_of_Year_Prod_Mid)

delayed_all_manufacturing_expanded_complete_yrs <- p_delayed_manufacturing %>% filter(Year <= 2032) %>%
  group_by(State_Province) %>%
  # Ensure all years 2025–2050 exist for each state
  complete(Year = 2025:2050) %>%
  # Fill missing values downward (last observation carried forward)
  fill(Share_of_Year_Prod_Down, Share_of_Year_Prod_Mid, .direction = "down") %>%
  ungroup() %>% select(Year, State_Province, Share_of_Year_Prod_Down, Share_of_Year_Prod_Mid)


### HERE DECIDE IF PACK HAS ANY SCRAP
## Clean Manu (projected for now will do delayed as well) --> for tonnes of material
p_clean_manu_projected_chem_state <- p_projected_manufac_by_chem %>% 
  left_join(all_manufacturing_expanded_complete_yrs, by="Year", relationship = "many-to-many") %>%
  mutate(Prod_proj_down = Prod_proj_down * Share_of_Year_Prod_Down,
         Prod_15_down = Prod_15_down * Share_of_Year_Prod_Down,
         Prod_proj_mid = Prod_proj_mid * Share_of_Year_Prod_Mid,
         Prod_15_mid = Prod_15_mid * Share_of_Year_Prod_Mid) %>%
  mutate(Prod_proj =  Prod_proj_mid,
         Prod_15 =  Prod_15_mid,
         State_Province = if_else(State_Province == "SLP ", "MX", State_Province)) %>%
  select(Year, State_Province, `Cathode Mix`, Prod_proj, Prod_15) %>%
  fill (Year, State_Province, Prod_proj, Prod_15, .direction = "down") 


p_clean_manu_delayed_chem_state <- p_delayed_manufac_by_chem %>% 
  left_join(delayed_all_manufacturing_expanded_complete_yrs, by="Year", relationship = "many-to-many") %>%
  mutate(Prod_proj_down = Prod_proj_down * Share_of_Year_Prod_Down,
         Prod_15_down = Prod_15_down * Share_of_Year_Prod_Down,
         Prod_proj_mid = Prod_proj_mid * Share_of_Year_Prod_Mid,
         Prod_15_mid = Prod_15_mid * Share_of_Year_Prod_Mid) %>%
  mutate(Prod_proj = Prod_proj_mid,
         Prod_15 = Prod_15_mid,
         State_Province = if_else(State_Province == "SLP ", "MX", State_Province)) %>%
  select(Year, State_Province, `Cathode Mix`, Prod_proj, Prod_15) %>%
  fill (Year, State_Province, Prod_proj, Prod_15, .direction = "down") 


## HERE FILL IN PACK SCRAP AGAIN?
## THIS IS FROM JESS PROJECTIONS OF SCRAPPAGE BY WEIGHT
clean_manu_projected_tonnes <- tonnes_manufac_projected %>%
  mutate(Scrap_proj_tonnes =  Tonnes_Scrap_proj_mid,
         Scrap_15_tonnes =  Tonnes_Scrap_15_mid) %>%
  select(Year, Scrap_proj_tonnes, Scrap_15_tonnes) 

scrap_proj_tonnes <- clean_manu_projected_tonnes %>% select(Year, Scrap_proj_tonnes) %>% 
  rename(Scrap_tonnes = Scrap_proj_tonnes) %>% mutate(Sale_Year = Year)
scrap_15_tonnes <- clean_manu_projected_tonnes %>% select(Year, Scrap_15_tonnes) %>% 
  rename(Scrap_tonnes = Scrap_15_tonnes) %>% mutate(Sale_Year = Year)

clean_manu_delayed_tonnes <- tonnes_manufac_delayed %>%
  mutate(Scrap_proj_tonnes =  Tonnes_Scrap_proj_mid,
         Scrap_15_tonnes =  Tonnes_Scrap_15_mid) %>%
  select(Year, Scrap_proj_tonnes, Scrap_15_tonnes) 

delay_scrap_proj_tonnes <- clean_manu_delayed_tonnes %>% select(Year, Scrap_proj_tonnes) %>% rename(Scrap_tonnes = Scrap_proj_tonnes) %>% mutate(Sale_Year = Year)
delay_scrap_15_tonnes <- clean_manu_delayed_tonnes %>% select(Year, Scrap_15_tonnes) %>% rename(Scrap_tonnes = Scrap_15_tonnes) %>% mutate(Sale_Year = Year)

### THIS IS CHEM (AND PRODUCTION TO BE TURNED INTO SCRAP AT MINERAL STAGE)
prod_proj_Gwh_state <- p_clean_manu_projected_chem_state %>% 
  select(Year, State_Province, `Cathode Mix`, Prod_proj) %>% 
  rename(Prod_Gwh_state = Prod_proj)

prod_15_Gwh_state <- p_clean_manu_projected_chem_state %>% 
  select(Year, State_Province, `Cathode Mix`, Prod_15) %>% 
  rename(Prod_Gwh_state = Prod_15)


delay_prod_proj_Gwh_state <- p_clean_manu_delayed_chem_state %>% 
  select(Year, State_Province, `Cathode Mix`, Prod_proj) %>% 
  rename(Prod_Gwh_state = Prod_proj)

delay_prod_15_Gwh_state <- p_clean_manu_delayed_chem_state %>% 
  select(Year, State_Province, `Cathode Mix`, Prod_15) %>% 
  rename(Prod_Gwh_state = Prod_15)



### SET UP BATTERY CAP/CHEM SIMULATIONS
fixed_batt_cap_merged <- batt_cap_merged %>% select(-c(`Total Sales`, `Total Mwh`)) %>% filter(Propulsion != "FCEV")
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

### RESTART HERE W DELAY
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

### INTRODUCE THE SCRAP and DELAY
batt_cap_projection <- batt_cap_projection %>%
  left_join(scrap_proj_tonnes,by = "Sale_Year") %>%
  left_join(prod_proj_Gwh_state,by = c("Year")) %>%
  select(
    Sale_Year,
    State_Province,
    Segment,
    Propulsion,
    `Projected Avg Batt Cap (kwh/batt)`,
    `Cathode Mix`,
    Scrap_tonnes,
    Prod_Gwh_state
  ) 

batt_cap_15 <- batt_cap_15 %>% 
  left_join(scrap_15_tonnes, by = "Sale_Year") %>% 
  left_join(prod_15_Gwh_state, by = c("Year")) %>% 
  select(
    Sale_Year, 
    State_Province, 
    Segment, 
    Propulsion, 
    `Projected Avg Batt Cap (kwh/batt)`, 
    `Cathode Mix`, 
    Scrap_tonnes, 
    Prod_Gwh_state
  )

all_states <- tibble(
  State_Province = unique(c(unname(state_map_rev), "MX"))
)

combo_cathodes <- batt_cap_projection %>%
  distinct(
    Sale_Year,
    Segment,
    Propulsion,
    `Cathode Mix`
  )

expanded_grid <- combo_cathodes %>%
  tidyr::crossing(all_states)

batt_cap_proj_ext <- expanded_grid %>%
  left_join(
    batt_cap_projection,
    by = c("Sale_Year", "Segment", "Propulsion", "State_Province", "Cathode Mix")
  ) 

batt_cap_15_ext <- expanded_grid %>%
  left_join(
    batt_cap_15, 
    by = c("Sale_Year", "Segment", "Propulsion", "State_Province", "Cathode Mix")
  )

combo_defaults_proj <- batt_cap_projection %>%
  group_by(
    Sale_Year,
    Segment,
    Propulsion,
    `Cathode Mix`
  ) %>%
  summarise(
    Scrap_tonnes = first(Scrap_tonnes),
    `Projected Avg Batt Cap (kwh/batt)` =
      first(`Projected Avg Batt Cap (kwh/batt)`),
    .groups = "drop"
  )

combo_defaults_15 <- batt_cap_15 %>%
  group_by(
    Sale_Year,
    Segment,
    Propulsion,
    `Cathode Mix`
  ) %>%
  summarise(
    Scrap_tonnes = first(Scrap_tonnes),
    `Projected Avg Batt Cap (kwh/batt)` =
      first(`Projected Avg Batt Cap (kwh/batt)`),
    .groups = "drop"
  )

batt_cap_proj_ext <- batt_cap_proj_ext %>%
  left_join(
    combo_defaults_proj,
    by = c("Sale_Year", "Segment", "Propulsion", "Cathode Mix")
  ) %>%
  mutate(
    Scrap_tonnes =
      coalesce(Scrap_tonnes.x, Scrap_tonnes.y),
    
    `Projected Avg Batt Cap (kwh/batt)` =
      coalesce(`Projected Avg Batt Cap (kwh/batt).x`,
               `Projected Avg Batt Cap (kwh/batt).y`),
    
    Prod_Gwh_state = replace_na(Prod_Gwh_state, 0)
  ) %>%
  select(
    Sale_Year,
    Segment,
    Propulsion,
    State_Province,
    `Cathode Mix`,
    Scrap_tonnes,
    Prod_Gwh_state,
    `Projected Avg Batt Cap (kwh/batt)`
  )  %>% mutate(Year = Sale_Year)


batt_cap_15_ext <- batt_cap_15_ext %>%
  left_join(
    combo_defaults_15,
    by = c("Sale_Year", "Segment", "Propulsion", "Cathode Mix")
  ) %>%
  mutate(
    Scrap_tonnes =
      coalesce(Scrap_tonnes.x, Scrap_tonnes.y),
    
    `Projected Avg Batt Cap (kwh/batt)` =
      coalesce(`Projected Avg Batt Cap (kwh/batt).x`,
               `Projected Avg Batt Cap (kwh/batt).y`),
    
    Prod_Gwh_state = replace_na(Prod_Gwh_state, 0)
  ) %>%
  select(
    Sale_Year,
    Segment,
    Propulsion,
    State_Province,
    `Cathode Mix`,
    Scrap_tonnes,
    Prod_Gwh_state,
    `Projected Avg Batt Cap (kwh/batt)`
  ) %>% mutate(Year = Sale_Year)


batt_scen <- list(batt_cap_proj_ext, batt_cap_15_ext) 



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
future_match <- left_join(fixed_cp, max_future, by = "Sale_Year", suffix = c("_x", "_y"), relationship = "many-to-one")

# Replace unknown chemistries with most common
mask_mins_future <- future_match$`Cathode Mix_x` %in% c("4V Ni or Mn based", "5V Mn based", "LCO", "Other")
future_match$`Cathode Mix_x`[mask_mins_future] <- future_match$`Cathode Mix_y`[mask_mins_future]

# Clean columns
future_match <- future_match %>% 
  select(`Sale_Year`, `Cathode Mix` = `Cathode Mix_x`, 
         `Cathode Mix Share` = `Cathode Mix Share_x`, 
         `Total Mwh` = `Total Mwh_x`) %>%
  group_by(Sale_Year, `Cathode Mix`) %>%
  summarise(`Total Mwh` = sum(`Total Mwh`, na.rm = TRUE), 
            `Cathode Mix Share` = sum(`Cathode Mix Share`, na.rm = TRUE))


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

df_2040_adjusted <- final_adjusted_mix %>% filter(Sale_Year == 2040)
df_extend_adjusted <- df_2040_adjusted %>%
  mutate(Sale_Year = list(2041:2050)) %>%    # add future years
  unnest(Sale_Year) 

final_adjusted_mix_extended <- bind_rows(
  final_adjusted_mix %>% filter(Sale_Year <= 2040),
  df_extend_adjusted
) %>%
  arrange(`Cathode Mix`, Sale_Year)


future_match <- future_match %>% select (-`Total Mwh`)

df_2040_proj <- future_match %>% filter(Sale_Year == 2040)
df_extend_proj <- df_2040_proj %>%
  mutate(Sale_Year = list(2041:2050)) %>%
  unnest(Sale_Year)

future_match <- bind_rows(
  future_match %>% filter(Sale_Year <= 2040),
  df_extend_proj
) %>%
  arrange(`Cathode Mix`, Sale_Year)

chem_scens <- list(future_match, final_adjusted_mix_extended)


### Assuming 100% collected but recycle what can?
###Put In Recycling Efficiency by 2035 for Seciton 177
sec177 <- c('California','Oregon','Washington','New York','Massachusetts',
            'Vermont','Colorado','Maryland','Delaware','New Mexico',
            'New Jersey','Rhode Island','District of Columbia')

future_recycle_type_collection <- future_recycle_type %>%  mutate(State_Province = case_when(
  State_Province %in% names(state_map_rev) ~ state_map_rev[State_Province],
  TRUE ~ State_Province))  %>% filter(Sale_Year > 2025)

# Convert all Sale_Year columns to integer
future_recycle_type_collection$Sale_Year <- as.integer(future_recycle_type_collection$Sale_Year) 



### Assumes manufacturing scrap is recycled anywhere and batteries are recycled anywhere
### this doesn't track where the recycling or manufacturing is happening

### RUN SCENARIOS
capacity_chem_scenarios <- function(batt_cap_df, chem_df, mineral_intensity, future_recycle_type_collection) {
  batt_df_collapsed <- batt_cap_df %>%
    group_by(State_Province, Segment, Propulsion, Sale_Year) %>%
    summarise(
      `Projected Avg Batt Cap (kwh/batt)` =
        first(`Projected Avg Batt Cap (kwh/batt)`),
      .groups = "drop"
    )
  
  ### RECYCLE in Future - cut only those sales years with the projection
  future_recycle_cap <- future_recycle_type_collection %>% left_join(
    batt_df_collapsed,
    by = c("State_Province","Sale_Year", "Segment", "Propulsion")
  ) 
  
  # Apply avg battery size per powertrain and type
  future_recycle_cap$LIB_recycle_kwh <- future_recycle_cap$LIB_recycle_total *
    future_recycle_cap$`Projected Avg Batt Cap (kwh/batt)`
  
  # Keep only useful column
  future_recycle_cap <- future_recycle_cap %>% group_by(Year, Sale_Year, State_Province) %>%
    summarise(LIB_recycle_kwh = sum(LIB_recycle_kwh))
  
  
  future_recycle_cap <- future_recycle_cap %>%
    arrange(State_Province, Year)
  
  ### APPLY BENCHMARK
  future_recycle_chem_fut <- future_recycle_cap %>%
    left_join(chem_df, by = "Sale_Year", relationship = "many-to-many") %>%
    mutate(Cathode_kwh_state = LIB_recycle_kwh * `Cathode Mix Share`) %>%
    select(-`Cathode Mix Share`) 
  
  
  future_recycle_chem <- bind_rows(hist_recycle_chem, future_recycle_chem_fut) %>%
    mutate(State_Province = case_when(
      State_Province %in% names(state_map_rev) ~ state_map_rev[State_Province],
      TRUE ~ State_Province)) %>% 
    group_by(Year, State_Province, `Cathode Mix`) %>%
    summarise(Cathode_kwh_state = sum(Cathode_kwh_state, na.rm = TRUE),
              LIB_recycle_kwh = sum(LIB_recycle_kwh, na.rm = TRUE)) %>%
    arrange(State_Province, Year)
  
  
  fut_c_nat <- future_recycle_chem %>% group_by(Year) %>%
    summarise(Cathode_kwh_nat = sum(Cathode_kwh_state))
  
  ## assuming no improvements in energy density 
  future_mass_recycle_chem <- future_recycle_chem%>% inner_join(specific_energy, by = "Cathode Mix") %>% 
    mutate(Batt_Mass_MT = LIB_recycle_kwh * Pack_kg_kwh/1000) ##check all chems have smth
  
  future_mass_recycle_total <- future_mass_recycle_chem %>%
    group_by(Year) %>%
    summarise(Batt_Mass_MT = sum(Batt_Mass_MT, na.rm = TRUE))
  ## Assumption-- all manufacturing gets recycled no matter where produced or where recycle is, and each 
  ## battery has an equal chance of getting recycled regardless of where it is
  ## all recycling will be used if available-- currently 9.2%
  
  ##currently use Jess's scrap and will add averaged available minerals using gwh with chem dist from NaatBatt
  ##potentially reconcile ghw from naatbatt and chemistries for whole database
  
  ### STILL WANT TO GET what isn't going there so if don't limit it at 0 for leftover how much is that and then 
  ### how much is the other half (100- percent) of what goes to post consumer--> run minerals leaving that way
  ### Quantities in tonnes and minerals--> print statements should cover it (in mass)
  ### need to go back to scrap mins and do percentage for first few years maybe
  ### then run 2 refining post consumer percents to get minerals (stay and go) --> stuff gets refined doesnt get refined 
  ## split not refined into what is turned to black mass and what is exported as batteries
  batt_df_nat_scrap <- batt_cap_df %>% group_by(Year) %>% 
    summarise(Scrap_tonnes = first(Scrap_tonnes), .groups = "drop")
  
  
  Available_Recycling_Capacity <- US_CA_Recycle %>% 
    inner_join(batt_df_nat_scrap, by = "Year") %>% 
    mutate(
      Leftover_blackmass_cap = pmax(Black_Mass_MT - Scrap_tonnes, 0),
      #Leftover_refining_cap  = pmax(Refining_MT - Scrap_proj_tonnes, 0),
      Leftover_Full_Recycle = pmax(Full_Recycle - Scrap_tonnes, 0), ## This has a variable constraint black mass when this is lowest and refining when that is lowest
      
      
      Scrap_full_recycle_percent = pmin(Full_Recycle/Scrap_tonnes,1),
      Unprocessed_Scrap = pmax(Scrap_tonnes-Black_Mass_MT,0),
      Unrefined_Scrap = pmax(Scrap_tonnes-Full_Recycle,0), ## all unrefined also from not processed
      
      Unprocessed_Scrap_percent = 1- pmin(Black_Mass_MT/Scrap_tonnes, 1),
      Exported_BM_Scrap_percent = pmax((Scrap_tonnes - Unprocessed_Scrap - Full_Recycle),0)/Scrap_tonnes,
      
      
    )  %>%
    inner_join(future_mass_recycle_total, by = "Year") %>%
    mutate(
      #Post_consumer_refine_percent   = pmin(Leftover_refining_cap / Batt_Mass_MT, 1),
      Post_consumer_blackmass_percent = pmin(Leftover_blackmass_cap / Batt_Mass_MT, 1),
      Post_consumer_full_recycle_percent = pmin(Leftover_Full_Recycle/Batt_Mass_MT,1), ### What matters
      
      ## Minerals
      Unprocessed_Batts = pmax(Batt_Mass_MT-Leftover_blackmass_cap,0), ## Exported EOY Batts
      Unrefined_Batts = pmax(Batt_Mass_MT-Leftover_Full_Recycle,0),
      Unprocessed_Batts_percent = 1-Post_consumer_blackmass_percent,
      Exported_BM_Batts_percent = pmax((Batt_Mass_MT - Unprocessed_Batts - Leftover_Full_Recycle),0)/Batt_Mass_MT,
      
      ## Recycling Needs
      Unused_Black_Mass = pmax(Leftover_blackmass_cap- Batt_Mass_MT,0),
      Unused_Refining = pmax(Refining_MT - pmin((Batt_Mass_MT + Scrap_tonnes)/Full_Recycle,1)*Full_Recycle,0),
      
      Needed_Black_Mass_change = (Unprocessed_Batts + Unprocessed_Scrap - Unused_Black_Mass), 
      Needed_Refining_change = (Unrefined_Batts + Unrefined_Scrap - Unused_Refining)
    ) %>%
    arrange(Year) %>%
    mutate(
      Needed_Black_Mass_level =
        accumulate(
          Needed_Black_Mass_change,
          ~ max(.x + .y, 0),
          .init = 0
        )[-1],
      
      Needed_Refining_level =
        accumulate(
          Needed_Refining_change,
          ~ max(.x + .y, 0),
          .init = 0
        )[-1]
    ) %>%
    
    select(
      Year,
      Post_consumer_full_recycle_percent,
      Scrap_full_recycle_percent, 
      
      Unprocessed_Batts_percent,
      Unprocessed_Scrap_percent,
      
      Exported_BM_Batts_percent,
      Exported_BM_Scrap_percent,
      
      Needed_Black_Mass_level,
      Needed_Refining_level,
      
      
    ) %>% mutate(across(where(is.numeric), ~ ifelse(abs(.) < 1e-12, 0, .)))
  
  
  batt_cap_state_scrap <- batt_cap_df %>% group_by(Year, `Cathode Mix`, State_Province) %>%
    summarise(Prod_Gwh_state = first(Prod_Gwh_state))
  
  
  #### National levels become state levels
  future_recycle_chem <- future_recycle_chem %>% select(-LIB_recycle_kwh) %>%
    inner_join(Available_Recycling_Capacity, by = "Year") %>% 
    mutate(Recycled_kwh_Batts = Cathode_kwh_state * Post_consumer_full_recycle_percent,
           Unprocessed_kwh_Batts = Cathode_kwh_state * Unprocessed_Batts_percent,
           Exported_BM_kwh_Batts = Cathode_kwh_state * Exported_BM_Batts_percent) %>% 
    left_join(batt_cap_state_scrap, by = c("Year", "Cathode Mix", "State_Province")) %>% 
    mutate(
      Recycled_Gwh_Prod = Prod_Gwh_state * Scrap_full_recycle_percent,
      Unprocessed_Gwh_Prod = Prod_Gwh_state * Unprocessed_Scrap_percent,
      Exported_BM_Gwh_Prod = Prod_Gwh_state * Exported_BM_Scrap_percent) %>%
    group_by(Year, State_Province, `Cathode Mix`) %>%
    summarise(
      Recycled_kwh_Batts    = sum(Recycled_kwh_Batts, na.rm = TRUE),
      Unprocessed_kwh_Batts = sum(Unprocessed_kwh_Batts, na.rm = TRUE),
      Exported_BM_kwh_Batts = sum(Exported_BM_kwh_Batts, na.rm = TRUE),
      
      Recycled_Gwh_Prod    = sum(Recycled_Gwh_Prod, na.rm = TRUE),
      Unprocessed_Gwh_Prod = sum(Unprocessed_Gwh_Prod, na.rm = TRUE),
      Exported_BM_Gwh_Prod = sum(Exported_BM_Gwh_Prod, na.rm = TRUE),
      
      Cathode_kwh_state = sum(Cathode_kwh_state, na.rm = TRUE),
      Prod_Gwh_state        = sum(Prod_Gwh_state, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    select(Year, State_Province, `Cathode Mix`, 
           Recycled_kwh_Batts, Unprocessed_kwh_Batts, Exported_BM_kwh_Batts, 
           Recycled_Gwh_Prod, Unprocessed_Gwh_Prod, Exported_BM_Gwh_Prod, 
           Cathode_kwh_state, Prod_Gwh_state) %>%
    mutate(across(where(is.numeric), ~ replace_na(.x, 0)))
  
  
  Nat_chem <- future_recycle_chem %>% group_by(Year) %>% 
    summarise(Recycled_kwh_Batts = sum(Recycled_kwh_Batts, na.rm = TRUE),
              Recycled_Gwh_Prod = sum(Recycled_Gwh_Prod, na.rm = TRUE),
              Prod_Gwh_state        = sum(Prod_Gwh_state, na.rm = TRUE),
              Cathode_kwh_state = sum(Cathode_kwh_state, na.rm = TRUE)) %>%
    mutate(total = Recycled_Gwh_Prod + Recycled_kwh_Batts/1e6)
  
  ## Scenario 1-2 current capacity w or without manufacturing
  ## Scenario 3-5 50% increase in capacity all hydro with more man, some man or no man
  ## Scenario 6 all has a place
  
  ### Define mineral groups
  everbatt_both  <- c("Nickel", "Cobalt")
  Copper         <- "Copper"
  Lithium        <- "Lithium"
  Graphite       <- "Graphite"
  Manganese      <- "Manganese"
  Not_recovered  <- c("Phosphorus", "Stainless steel", "Steel", "Aluminum","Carbon")
  
  future_minerals <- future_recycle_chem %>%
    full_join(all_mins,
              by = "Cathode Mix",
              relationship = "many-to-many") %>%
    filter(!Mineral %in% Not_recovered) %>%
    
    mutate(
      `Available Recycled Minerals (w Scrap) (kg)` =
        kg_per_kwh * Recycled_kwh_Batts + `kg/Gwh` * Recycled_Gwh_Prod,
      
      `Available Recycled Minerals No R Restraint (kg)` = 
        kg_per_kwh * Cathode_kwh_state + `kg/Gwh` * Prod_Gwh_state,
      
      `Minerals in Exported Scrap/Batts (kg)` = kg_per_kwh * Unprocessed_kwh_Batts + `kg/Gwh` * Unprocessed_Gwh_Prod,
      
      `Minerals in Exported BM (kg)` = kg_per_kwh * Exported_BM_kwh_Batts + `kg/Gwh` * Exported_BM_Gwh_Prod,
      
      Scrap_min = `kg/Gwh` * Recycled_Gwh_Prod,
      
      Batt_min = kg_per_kwh * Recycled_kwh_Batts
    ) %>% 
    group_by(Year, State_Province, Mineral) %>%
    summarise( `Available Recycled Minerals (w Scrap) (kg)` = sum(`Available Recycled Minerals (w Scrap) (kg)`, na.rm = TRUE),
               `Available Recycled Minerals No R Restraint (kg)` = sum(`Available Recycled Minerals No R Restraint (kg)`, na.rm = TRUE), 
               `Minerals in Exported Scrap/Batts (kg)` = sum(`Minerals in Exported Scrap/Batts (kg)`, na.rm = TRUE), 
               `Minerals in Exported BM (kg)` = sum(`Minerals in Exported BM (kg)`, na.rm = TRUE),
               Scrap_min = sum(Scrap_min, na.rm = TRUE),
               Batt_min = sum(Batt_min, na.rm = TRUE)) %>%
    select(Year, State_Province, Mineral,  
           `Available Recycled Minerals (w Scrap) (kg)`, `Available Recycled Minerals No R Restraint (kg)`, 
           `Minerals in Exported Scrap/Batts (kg)`, `Minerals in Exported BM (kg)`,
           Scrap_min, Batt_min) %>% filter(!is.na(Year))
  
  fut_min_nat <- future_minerals %>% group_by(Year, Mineral) %>% 
    summarise( `Available Recycled Minerals (w Scrap) (kg)` = sum(`Available Recycled Minerals (w Scrap) (kg)`, na.rm = TRUE),
               `Available Recycled Minerals No R Restraint (kg)` = sum(`Available Recycled Minerals No R Restraint (kg)`, na.rm = TRUE), 
               `Minerals in Exported Scrap/Batts (kg)` = sum(`Minerals in Exported Scrap/Batts (kg)`, na.rm = TRUE), 
               `Minerals in Exported BM (kg)` = sum(`Minerals in Exported BM (kg)`, na.rm = TRUE),
               Scrap_min = sum(Scrap_min, na.rm = TRUE),
               Batt_min = sum(Batt_min, na.rm = TRUE)) %>%
    filter(Mineral == "Nickel")
  
  EU_Lithium  <- 0.8
  EU_recovery <- 0.95
  recovery_90 <- 0.9

  target_year_recovery <- 2035
  start_year_recovery  <- min(future_minerals$Year, na.rm = TRUE)
  
  
  ### FINAL PROCESSING
  future_final <- future_minerals %>% ungroup() %>%
    ### Assumptions--> steel, nickel and cobalt get recovered at 90 and 95% already--> assuming regulation doesn't do much?
    ###            --> lithium, manganese, graphite and aluminum at 95% 
    ### is anything coming from LFP--> get info from someone??
    
    mutate(
      Multiplier = case_when(
        Mineral %in% everbatt_both   ~ EU_recovery,
        Mineral %in% Copper ~ ifelse(Year >= 2035, EU_recovery, recovery_90),
        Mineral %in% Lithium ~ ifelse(Year >= 2035, EU_Lithium, 0),
        Mineral %in% Manganese ~ ifelse(Year >= 2035, EU_recovery, 0),
        Mineral %in% Graphite  ~ ifelse(Year >= 2035, recovery_90, 0),
        Mineral %in% Not_recovered ~ 0,
        TRUE ~ 1
      )
    ) %>% mutate(
      Multiplier_no_limit = case_when(
        Mineral %in% everbatt_both ~ EU_recovery,
        Mineral %in% Copper       ~ EU_recovery,
        Mineral %in% Lithium      ~ EU_Lithium,
        Mineral %in% Manganese    ~ EU_recovery,
        Mineral %in% Graphite     ~ recovery_90,
        Mineral %in% Not_recovered ~ 0,
        TRUE ~ 1
      )
    )%>%
    mutate(
      `Available Recycled Minerals (w Scrap) (Tonne)` =
        `Available Recycled Minerals (w Scrap) (kg)` * Multiplier/1000,
      
      `Available Recycled Minerals No R Restraint (Tonne)` =
        `Available Recycled Minerals No R Restraint (kg)` * Multiplier_no_limit/1000, ### if all that was generated was recycled at hydrometallurgical now
      
      ### 3 sources of loss
      `Minerals Recoverable in Exported Scrap/Batts (Tonne)` = 
        `Minerals in Exported Scrap/Batts (kg)` * Multiplier_no_limit/1000,
      
      `Minerals Recoverable in Exported BM (Tonne)` = 
        `Minerals in Exported BM (kg)` * Multiplier_no_limit/1000,
      
      `Minerals Lost to Pyrometalurgy (Tonne)` = 
        `Available Recycled Minerals (w Scrap) (kg)`*Multiplier_no_limit/1000 - `Available Recycled Minerals (w Scrap) (Tonne)`,
      
      Scrap_min = Scrap_min * Multiplier_no_limit/1000,
      Batt_min = Batt_min * Multiplier_no_limit/1000
    ) %>%
    
    filter(!is.na(Mineral)) %>%
    select(Year, State_Province, Mineral, `Available Recycled Minerals (w Scrap) (Tonne)`, `Available Recycled Minerals No R Restraint (Tonne)`, `Minerals Recoverable in Exported Scrap/Batts (Tonne)`,
           `Minerals Recoverable in Exported BM (Tonne)`, `Minerals Lost to Pyrometalurgy (Tonne)`, Scrap_min, Batt_min)
  
  fin_min_nat <- future_final %>% group_by(Year, Mineral) %>% 
    summarise(Scrap_min = sum(Scrap_min, na.rm = TRUE),
              Batt_min = sum(Batt_min, na.rm = TRUE),
              `Available Recycled Minerals (w Scrap) (Tonne)` = sum(`Available Recycled Minerals (w Scrap) (Tonne)`, na.rm = TRUE),
              `Available Recycled Minerals No R Restraint (Tonne)` = sum(`Available Recycled Minerals No R Restraint (Tonne)`, na.rm = TRUE)) %>% 
    filter(Mineral == "Nickel")
  
  
  capacity_needs <- Available_Recycling_Capacity %>%
    select(
      Year,
      Needed_Black_Mass_level,
      Needed_Refining_level
    )
  return(
    list(
      future_final    = future_final,
      capacity_needs  = capacity_needs
    )
  )
}




# Set names for scenarios
names(batt_scen) <- c("Baseline Capacity", "15% Lower Capacity")
names(chem_scens) <- c("Original Chemistry", "High LFP Chemistry")

# Use `crossing()` to create all 4 combinations
scenario_combos <- crossing(
  Batt = names(batt_scen),
  Chem = names(chem_scens)
)

##TEST
safe_capacity_chem_scenarios <- function(batt_name, chem_name) {
  tryCatch({
    res <- capacity_chem_scenarios(
      batt_cap_df = batt_scen[[batt_name]],
      chem_df = chem_scens[[chem_name]],
      mineral_intensity = mineral_intensity,
      future_recycle_type_collection = future_recycle_type_collection
    )
    
    list(
      future_final = res$future_final %>%
        mutate(
          Battery_Scenario = batt_name,
          Chemistry_Scenario = chem_name
        ),
      
      capacity_needs = res$capacity_needs %>%
        mutate(
          Battery_Scenario = batt_name,
          Chemistry_Scenario = chem_name
        )
    )
    
  }, error = function(e) {
    message("⚠ Error in scenario: ", batt_name, " / ", chem_name)
    message("  -> ", conditionMessage(e))
    NULL
  })
}

### LOST 2025 scrap?
# Run all scenarios using pmap safely
all_scenarios <- scenario_combos %>%
  mutate(
    result = pmap(
      list(Batt, Chem),
      safe_capacity_chem_scenarios
    )
  )

cap_chem_results <- all_scenarios %>%
  pull(result) %>%
  compact() %>%
  map("future_final") %>%
  bind_rows()

capacity_needs_all <- all_scenarios %>%
  pull(result) %>%
  compact() %>%
  map("capacity_needs") %>%
  bind_rows()

# Combine all results

cap_chem_results <- cap_chem_results %>%
  mutate(Scenario = paste(Battery_Scenario, Chemistry_Scenario, sep = " - "))

needed_cap_results <- capacity_needs_all %>%
  mutate(Scenario = paste(Battery_Scenario, Chemistry_Scenario, sep = " - "))  %>% 
  select(-c(Battery_Scenario, Chemistry_Scenario)) %>%
  rename (`Black Mass` = Needed_Black_Mass_level, `Refining` = Needed_Refining_level) %>%
  pivot_longer(cols = c(`Black Mass`, `Refining`),
               names_to = "Recycling Step", 
               values_to = "Tonne") %>% mutate(
                 Scenario_Recycling = paste(Scenario, `Recycling Step`, sep = " - "),
                 Year = as.numeric(Year)  # important!
               ) 

## CHECKS
# fin_min_nat <- cap_chem_results %>% group_by(Year, Mineral, Scenario) %>% 
#   summarise(Scrap_min = sum(Scrap_min, na.rm = TRUE),
#             Batt_min = sum(Batt_min, na.rm = TRUE),
#             `Available Recycled Minerals (w Scrap) (Tonne)` = sum(`Available Recycled Minerals (w Scrap) (Tonne)`, na.rm = TRUE),
#             `Available Recycled Minerals No R Restraint (Tonne)` = sum(`Available Recycled Minerals No R Restraint (Tonne)`, na.rm = TRUE)) %>% 
#   filter(Mineral == "Nickel")

legend_order <- c(
  "Baseline Capacity - Original Chemistry",  # blue
  "Baseline Capacity - High LFP Chemistry",  # purple
  "15% Lower Capacity - Original Chemistry", # green
  "15% Lower Capacity - High LFP Chemistry"  # red
)
  
nat_cap_chem_rec <- cap_chem_results %>% group_by(Year, Scenario, Mineral) %>%
  summarise(`Current NA Recycling Capacity` =  sum(`Available Recycled Minerals (w Scrap) (Tonne)`, na.rm = TRUE),
            `All Material is Recycled in NA` = sum(`Available Recycled Minerals No R Restraint (Tonne)`, na.rm = TRUE)) %>%
            #`Recoverable Minerals Exported as Scrap/Batts` = sum(`Minerals Recoverable in Exported Scrap/Batts (Tonne)`, na.rm = TRUE),
            #`Recoverable Minerals Exported as BM` = sum(`Minerals Recoverable in Exported BM (Tonne)`, na.rm = TRUE),
            #`Minerals Lost in Non-Hydrometallurgical Facilities` = sum( `Minerals Lost to Pyrometalurgy (Tonne)`, na.rm = TRUE)) %>%
  #filter(Scenario == "Baseline Battery - Original Chemistry") %>%
  filter(Mineral == c("Cobalt", "Copper", "Nickel")) %>%
  pivot_longer(cols = c("Current NA Recycling Capacity", "All Material is Recycled in NA"),
                                                        # "Recoverable Minerals Exported as Scrap/Batts", "Recoverable Minerals Exported as BM",
                                                        # "Minerals Lost in Non-Hydrometallurgical Facilities"),
                        names_to = "Recycling Scenario", values_to = "Tonne") %>%
  mutate(Year = as.numeric(Year)) %>% filter(Year <= 2035) %>%
  mutate(Scenario = factor(Scenario, levels = legend_order)) %>%
  mutate(
    `Recycling Scenario` = forcats::fct_recode(
      `Recycling Scenario`,
      "Recycling Limited to NA 2025 Online or Planned Facilities" = "Current NA Recycling Capacity"
    )
  )


all_nat_cap_chem_rec <-cap_chem_results %>% group_by(Year, Scenario, Mineral) %>%
  summarise(`Current NA Recycling Capacity` =  sum(`Available Recycled Minerals (w Scrap) (Tonne)`, na.rm = TRUE),
            `All Material is Recycled in NA` = sum(`Available Recycled Minerals No R Restraint (Tonne)`, na.rm = TRUE)) %>%
  #`Recoverable Minerals Exported as Scrap/Batts` = sum(`Minerals Recoverable in Exported Scrap/Batts (Tonne)`, na.rm = TRUE),
  #`Recoverable Minerals Exported as BM` = sum(`Minerals Recoverable in Exported BM (Tonne)`, na.rm = TRUE),
  #`Minerals Lost in Non-Hydrometallurgical Facilities` = sum( `Minerals Lost to Pyrometalurgy (Tonne)`, na.rm = TRUE)) %>%
  #filter(Scenario == "Baseline Battery - Original Chemistry") %>%
  pivot_longer(cols = c("Current NA Recycling Capacity", "All Material is Recycled in NA"),
               # "Recoverable Minerals Exported as Scrap/Batts", "Recoverable Minerals Exported as BM",
               # "Minerals Lost in Non-Hydrometallurgical Facilities"),
               names_to = "Recycling Scenario", values_to = "Tonne") %>%
  mutate(Year = as.numeric(Year)) %>% 
  mutate(Scenario = factor(Scenario, levels = legend_order)) %>%
  mutate(
    `Recycling Scenario` = forcats::fct_recode(
      `Recycling Scenario`,
      "Recycling Limited to NA 2025 Online or Planned Facilities" = "Current NA Recycling Capacity"
    )
  )


non_recovery_lost <- cap_chem_results %>% group_by(Year, Scenario, Mineral) %>%
  summarise(`Minerals Lost From Non-Recovery` = sum( `Minerals Lost to Pyrometalurgy (Tonne)`, na.rm = TRUE)) %>%
  select(Year, Mineral, `Minerals Lost From Non-Recovery`, Scenario) %>%
  arrange(Mineral, Year, Scenario) %>%        # IMPORTANT
  group_by(Mineral) %>%              # group persists across years
  mutate(
    Cum_Tonne =
      cumsum(`Minerals Lost From Non-Recovery`)
  ) %>%
  ungroup() %>%
  filter(Year <= 2035,  Cum_Tonne > 0) %>%
  mutate(Year = as.numeric(Year)) %>%
  mutate(Scenario = factor(Scenario, levels = legend_order))



export_lost <- cap_chem_results %>% group_by(Year, Scenario, Mineral) %>%
  summarise(`Minerals Recoverable in Exported Scrap/Batts (Tonne)`  = sum( `Minerals Recoverable in Exported Scrap/Batts (Tonne)`, na.rm = TRUE),
            `Minerals Recoverable in Exported BM (Tonne)` = sum(`Minerals Recoverable in Exported BM (Tonne)`, na.rm = TRUE)) %>%
  mutate(Total_Minerals_Exported = `Minerals Recoverable in Exported BM (Tonne)` + `Minerals Recoverable in Exported Scrap/Batts (Tonne)`) %>%
  select(Year, Mineral, Total_Minerals_Exported, Scenario) %>%
  # arrange(Mineral, Year, Scenario) %>%        # IMPORTANT
  # group_by(Mineral) %>%              # group persists across years
  # mutate(
  #   Cum_Tonne =
  #     cumsum(Total_Minerals_Exported)
  # ) %>%
  # ungroup() %>%
  filter(Total_Minerals_Exported > 0) %>%
  mutate(Year = as.numeric(Year)) %>%
  mutate(Scenario = factor(Scenario, levels = legend_order))


## Recycling Plots
ggplot(
  nat_cap_chem_rec,
  aes(
    x = as.character(Year),
    y = Tonne,
    color = Scenario,                
    alpha = `Recycling Scenario`,    
    group = interaction(Scenario, `Recycling Scenario`)  
  )
) + 
  scale_y_sqrt() +
  geom_line(linewidth = 1.1) +
  geom_point(size = 2) +
  facet_wrap(~ Mineral, scales = "free_y") +
  labs(
    title = "North America Available Recycled Minerals Until 2035 by Mineral",
    x = "Year",
    y = "Recycled Minerals (Metric Tonnes)",
    color = "Battery Capacity - Chemistry Scenario",
    alpha = "Recycling Scenario"
  ) +
  scale_alpha_manual(values = c(
    "Recycling Limited to NA 2025 Online or Planned Facilities" = 1,  # darkest
    "All Material is Recycled in NA" = 0.4
    # add more if you have more recycling scenarios
  ),
  drop = FALSE) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(hjust = 0.5, size = 18, face = "bold"),
    axis.title = element_text(size = 14, face = "bold"),
    axis.text = element_text(size = 12),
    axis.text.x = element_text(angle = 30, hjust = 1),  # tilt x-axis labels
    strip.text = element_text(size = 14, face = "bold"),
    legend.position = "bottom",
    legend.title = element_text(size = 12, face = "bold"),
    legend.text = element_text(size = 11)
  ) +
  guides(
    color = guide_legend(
      title.position = "top",
      title.hjust = 0.5,
      nrow = 2,
      byrow = TRUE
    ),
    alpha = guide_legend(
      title.position = "top",
      title.hjust = 0.5,
      nrow = 2,
      byrow = TRUE,
      override.aes = list(
        color = "black",
        linewidth = 1.2
      )
    )
  )


## recovery plot
ggplot(
  non_recovery_lost,
  aes(
    x = as.character(Year),
    y = Cum_Tonne,
    group = interaction(Mineral, Scenario),
    color = Scenario,
  ) 
) + 
  geom_line(linewidth = 1.1) +
  geom_point(size = 2) +
  facet_wrap(~ Mineral, scales = "free_y") +
  labs(
    title = "Cumulative North America Minerals Lost to Lack of Recovery Standards",
    x = "Year",
    y = "Lost Minerals (Metric Tonnes)",
    color = "Battery Capacity - Chemistry Scenario"
  ) +
  theme_minimal(base_size = 14) +  # larger base font
  theme(
    plot.title = element_text(hjust = 0.5, size = 18, face = "bold"),
    axis.title = element_text(size = 14, face = "bold"),
    axis.text = element_text(size = 12),
    strip.text = element_text(size = 14, face = "bold"),
    axis.text.x = element_text(angle = 30, hjust = 1),  # tilt x-axis labels
    legend.position = "bottom",
    legend.title = element_text(size = 12, face = "bold"),
    legend.text = element_text(size = 11)
  ) +
  guides(
    color = guide_legend(nrow = 2, byrow = TRUE),  # first row for color
    alpha = guide_legend(nrow = 2, byrow = TRUE)   # second row for alpha
  )



# Define a base color for each Scenario


legend_order_recycle <- c(
  "Baseline Capacity - Original Chemistry - Black Mass",
  "Baseline Capacity - Original Chemistry - Refining",
  "Baseline Capacity - High LFP Chemistry - Black Mass",
  "Baseline Capacity - High LFP Chemistry - Refining",
  "15% Lower Capacity - Original Chemistry - Black Mass",
  "15% Lower Capacity - Original Chemistry - Refining",
  "15% Lower Capacity - High LFP Chemistry - Black Mass",
  "15% Lower Capacity - High LFP Chemistry - Refining"
)

needed_cap_long <- needed_cap_results %>%
  mutate(
    Scenario_Recycling = paste(Scenario, `Recycling Step`, sep = " - "),
    Scenario_Recycling = as.character(Scenario_Recycling),
    Scenario_Recycling = trimws(Scenario_Recycling),
    Year = as.numeric(Year),
    Tonne = Tonne/1e6
  ) %>%
  mutate(Scenario_Recycling = factor(Scenario_Recycling, levels = legend_order_recycle))

all_colors <- c(
  "Baseline Capacity - Original Chemistry - Black Mass" = "#F8766D",  # purple-blue
  "Baseline Capacity - Original Chemistry - Refining"    = "#F9B6AF",  # lighter purple
  
  "Baseline Capacity - High LFP Chemistry - Black Mass" = "#7CAE00",  # cyan
  "Baseline Capacity - High LFP Chemistry - Refining"    = "#C4E080",  # lighter cyan
  
  "15% Lower Capacity - Original Chemistry - Black Mass" = "#00BFC4", # green
  "15% Lower Capacity - Original Chemistry - Refining"    = "#80E6E9", # lighter green
  
  "15% Lower Capacity - High LFP Chemistry - Black Mass" = "#C77CFF", # red
  "15% Lower Capacity - High LFP Chemistry - Refining"    = "#D9B3FF"  # lighter red
)



# Plot
ggplot(needed_cap_long, aes(
  x = Year,
  y = Tonne,
  color = Scenario_Recycling,
  group = Scenario_Recycling
)) +
  geom_line(size = 1.2) +
  geom_point(size = 2) +
  scale_color_manual(values = all_colors) +  # now colors will show
  labs(
    title = "North America Black Mass and Refining Capacity Needed Yearly Until 2050",
    x = "Year",
    y = "Needed Recycling (Millions of MT)",
    color = "Battery Capacity - Chemistry Scenario - Recycling Step"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(hjust = 0.5, size = 18, face = "bold"),
    axis.title = element_text(size = 14, face = "bold"),
    axis.text = element_text(size = 12),
    axis.text.x = element_text(angle = 30, hjust = 1),
    strip.text = element_text(size = 14, face = "bold"),
    legend.position = "bottom",
    legend.title = element_text(size = 12, face = "bold"),
    legend.text = element_text(size = 11)
  ) + 
  guides(
    color = guide_legend(nrow = 4, byrow = TRUE)  # first row for color
  )


scenario_base_colors <- c(
  "Baseline Capacity - Original Chemistry" = "#F8766D",  # blue
  "Baseline Capacity - High LFP Chemistry" = "#7CAE00",  # purple
  "15% Lower Capacity - Original Chemistry" = "#00BFC4", # green
  "15% Lower Capacity - High LFP Chemistry" =  "#C77CFF" # red
)

ggplot(export_lost, aes(
  x = Year,
  y = Total_Minerals_Exported, 
  color = Scenario,
  group = Scenario
)) + geom_line(size = 1.2) +
  geom_point(size = 2) +
  scale_color_manual(values = scenario_base_colors) +  # now colors will show
  facet_wrap(~ Mineral, scales = "free_y") +
  labs(
    title = "Exported Mass of Battery Minerals Each Year",
    x = "Year",
    y = "Exported Minerals (Metric Tonnes)",
    color = "Battery Capacity - Chemistry Scenario") +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(hjust = 0.5, size = 18, face = "bold"),
    axis.title = element_text(size = 14, face = "bold"),
    axis.text = element_text(size = 12),
    axis.text.x = element_text(angle = 30, hjust = 1),
    strip.text = element_text(size = 14, face = "bold"),
    legend.position = "bottom",
    legend.title = element_text(size = 12, face = "bold"),
    legend.text = element_text(size = 11)
  ) + 
  guides(
    color = guide_legend(nrow = 4, byrow = TRUE)  # first row for color
  )




##IGNORE
# 
# summary_final_future_hist <- cap_chem_results %>%
#   group_by(Scenario, Year, State, Mineral) %>%
#   summarise(
#     `Available Recycled Minerals Current Cap (kg)` = sum(`Available Recycled Minerals Current Cap (kg)`, na.rm = TRUE),
#     across(
#       .cols = all_of(recycle_cols),
#       .fns = ~ sum(.x, na.rm = TRUE)
#     ),
#     .groups = "drop"
#   ) %>%
#   filter(!Mineral %in% c("Aluminum", "Steel"))
# 
# 
# 
# #### PLOTTING
# # Get all unique states
# states <- unique(summary_final_future_hist$State)
# output_file <- "/Users/elsawefes-potter/Documents/Critical_Minerals_Pablo/recycled_minerals_by_state_w_recovery_Repeal-TEST.pdf"
# 
# 
# pdf(output_file, width = 12, height = 8)
# 
# 
# # Loop over states and make one page per state
# # for (s in states) {
# #   state_data <- summary_final_future_hist %>%
# #     filter(State == s)
# #   
# #   if (nrow(state_data) == 0 || all(is.na(state_data$Mineral)) || length(unique(state_data$Mineral[!is.na(state_data$Mineral)])) == 0) {
# #     warning(paste("Skipping state due to no data or no valid Mineral:", s))
# #     next
# #   }
# #   
# #   p <- ggplot(state_data, aes(x = Year, y = `Available Recycled Minerals Current Cap (kg)`/1000000,
# #                               color = Scenario, linetype = Scenario)) +
# #     geom_line() +
# #     facet_wrap(~ Mineral, scales = "free_y", ncol = 2) +  # adjust ncol/nrow as needed
# #     labs(
# #       title = paste("Repeal- Minerals in Recycled Batteries –", s),
# #       x = "Year",
# #       y = "Minerals in Recycled Batteries (millions of kg)",
# #       color = "Scenario",
# #       linetype = "Scenario"
# #     ) +
# #     theme_minimal(base_size = 15) +
# #     theme(
# #       legend.position = "bottom",
# #       legend.text = element_text(size = 11),       # readable font
# #       legend.title = element_text(size = 12),      # optional, slightly bigger title
# #       legend.key.size = unit(0.3, "cm"),           # smaller legend boxes
# #       plot.margin = margin(t = 10, r = 80, b = 50, l = 10),
# #       legend.box.margin = margin(t = 10)
# #     ) +
# #     guides(
# #       color = guide_legend(nrow = 2, byrow = TRUE),  # multiple rows if needed
# #       fill  = guide_legend(nrow = 2, byrow = TRUE)
# #     ) +
# #     coord_cartesian(clip = "off")
# #   
# #   print(p)
# # }
# # 
# # 
# # # Close the PDF device
# # dev.off()
# 
# 
# ## National
#   {p <- ggplot(state_data, aes(x = Year, y = `Available Recycled Minerals Current Cap (kg)`/1000000,
#                               color = Scenario, linetype = Scenario)) +
#     geom_line() +
#     facet_wrap(~ Mineral, scales = "free_y", ncol = 2) +  # adjust ncol/nrow as needed
#     labs(
#       title = paste("Repeal- Minerals in Recycled Batteries –", s),
#       x = "Year",
#       y = "Minerals in Recycled Batteries (millions of kg)",
#       color = "Scenario",
#       linetype = "Scenario"
#     ) +
#     theme_minimal(base_size = 15) +
#     theme(
#       legend.position = "bottom",
#       legend.text = element_text(size = 11),       # readable font
#       legend.title = element_text(size = 12),      # optional, slightly bigger title
#       legend.key.size = unit(0.3, "cm"),           # smaller legend boxes
#       plot.margin = margin(t = 10, r = 80, b = 50, l = 10),
#       legend.box.margin = margin(t = 10)
#     ) +
#     guides(
#       color = guide_legend(nrow = 2, byrow = TRUE),  # multiple rows if needed
#       fill  = guide_legend(nrow = 2, byrow = TRUE)
#     ) +
#     coord_cartesian(clip = "off")
# 
#   print(p)
# }
# 
# 
# # Close the PDF device
# dev.off()
# 
# ##Manufacturing and Recycling capacity by state
# 
