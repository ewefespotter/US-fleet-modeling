install.packages("geofacet")
installed.packages()["geofacet", "Version"]
install.packages("devtools")
devtools::install_github("hafen/geofacet")
install.packages("openxlsx")
library(openxlsx)


library(readxl)
library(readr)
library(tidyverse)

library(ggplot2)
library(geofacet)


### open vs planned
state_map <- c(
  # US states
  AL = "Alabama", AK = "Alaska", AZ = "Arizona", AR = "Arkansas",
  CA = "California", CO = "Colorado", CT = "Connecticut", DE = "Delaware", DC = "District of Columbia",
  FL = "Florida", GA = "Georgia", HI = "Hawaii", ID = "Idaho",
  IL = "Illinois", IN = "Indiana", IA = "Iowa", KS = "Kansas",
  KY = "Kentucky", LA = "Louisiana", ME = "Maine", MD = "Maryland",
  MA = "Massachusetts", MI = "Michigan", MN = "Minnesota", MS = "Mississippi",
  MO = "Missouri", MT = "Montana", NE = "Nebraska", NV = "Nevada",
  NH = "New Hampshire", NJ = "New Jersey", NM = "New Mexico", NY = "New York",
  NC = "North Carolina", ND = "North Dakota", OH = "Ohio", OK = "Oklahoma",
  OR = "Oregon", PA = "Pennsylvania", RI = "Rhode Island", SC = "South Carolina",
  SD = "South Dakota", TN = "Tennessee", TX = "Texas", UT = "Utah",
  VT = "Vermont", VA = "Virginia", WA = "Washington", WV = "West Virginia",
  WI = "Wisconsin", WY = "Wyoming",
  
  # Canadian provinces and territories
  AB = "Alberta", BC = "British Columbia", MB = "Manitoba", NB = "New Brunswick",
  NL = "Newfoundland and Labrador", NS = "Nova Scotia", ON = "Ontario", PE = "Prince Edward Island",
  QC = "Quebec", SK = "Saskatchewan", NT = "Northwest Territories", NU = "Nunavut",
  YT = "Yukon",
  
  MX = "Mexico"
)

state_map_rev <- setNames(names(state_map), state_map)

##Manufacturing
EVLIB_Flows_US <- read_csv("/Users/elsawefes-potter/Documents/Critical_Minerals_Pablo/Data/EVLIB_Flows_detail_ACCII.csv") %>%
  rename(State_Province = State)
EV_Flows_US <- read_csv("/Users/elsawefes-potter/Documents/Critical_Minerals_Pablo/Data/ClosedLoop_AddRetire_byStateSegment_ACCII.csv") %>%
  select(State, Segment, Year, add_BEV, add_PHEV) %>%
  group_by(State, Segment, Year) %>% summarise(add_BEV = sum(add_BEV, na.rm = TRUE), add_PHEV = sum(add_PHEV, na.rm = TRUE)) %>%
  rename(BEV = add_BEV, PHEV = add_PHEV,
         State_Province = State) %>%
  pivot_longer(cols = c(BEV, PHEV),
               names_to = "Propulsion",
               values_to = "Add_EV")


EVLIB_Flows_CA <- read_csv("/Users/elsawefes-potter/Documents/Critical_Minerals_Pablo/Data/Canada-EVLIB_Flows_detail_ACCII.csv") %>%
  rename(State_Province = State)
EV_Flows_CA <- read_csv("/Users/elsawefes-potter/Documents/Critical_Minerals_Pablo/Data/Canada-ClosedLoop_AddRetire_byStateSegment_ACCII.csv") %>%
  select(State, Segment, Year, add_BEV, add_PHEV) %>%
  group_by(State, Segment, Year) %>% summarise(add_BEV = sum(add_BEV, na.rm = TRUE), add_PHEV = sum(add_PHEV, na.rm = TRUE)) %>%
  rename(BEV = add_BEV, PHEV = add_PHEV,
         State_Province = State) %>%
  pivot_longer(cols = c(BEV, PHEV),
               names_to = "Propulsion",
               values_to = "Add_EV")

EVLIB_Flows_MX <- read_csv("/Users/elsawefes-potter/Documents/Critical_Minerals_Pablo/Data/Mexico-EVLIB_Flows_detail_ACCII.csv") %>%
  rename(State_Province = State)
EV_Flows_MX <- read_csv("/Users/elsawefes-potter/Documents/Critical_Minerals_Pablo/Data/Mexico-ClosedLoop_StateTotals_ACCII.csv") %>%
  select(State, Year, add_BEV, add_PHEV) %>%
  group_by(State, Year) %>% summarise(add_BEV = sum(add_BEV, na.rm = TRUE), add_PHEV = sum(add_PHEV, na.rm = TRUE)) %>%
  rename(BEV = add_BEV, PHEV = add_PHEV,
         State_Province = State) %>%
  pivot_longer(cols = c(BEV, PHEV),
               names_to = "Propulsion",
               values_to = "Add_EV")



BESSLIB_Flows_US <- read_csv("/Users/elsawefes-potter/Documents/Critical_Minerals_Pablo/Data/BESS_Retire_Vector_byStateSegProp_ACCII.csv") %>%
  rename(LIB_recycling_vector = BESS_retire_vector) %>%
  rename(`State_Province` = `State`)

BESSLIB_Flows_CA <- read_csv("/Users/elsawefes-potter/Documents/Critical_Minerals_Pablo/Data/Canada-BESS_Retire_Vector_byStateSegProp_ACCII.csv") %>%
  rename(LIB_recycling_vector = BESS_retire_vector) %>%
  rename(`State_Province` = `State`)

BESSLIB_Flows_MX <- read_csv("/Users/elsawefes-potter/Documents/Critical_Minerals_Pablo/Data/Mexico-BESS_Retire_Vector_byStateSegProp_ACCII.csv") %>%
  rename(LIB_recycling_vector = BESS_retire_vector) %>%
  rename(`State_Province` = `State`)

EVLIB_Flows <- bind_rows(EVLIB_Flows_US, EVLIB_Flows_CA, EVLIB_Flows_MX)
EV_Flows <- bind_rows(EV_Flows_US, EV_Flows_CA, EV_Flows_MX)
BESSLIB_Flows <- bind_rows(BESSLIB_Flows_US, BESSLIB_Flows_CA, BESSLIB_Flows_MX)


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

BESSLIB_Flows$LIB_recycling_vector <- Map(
  name_vector_with_years,
  BESSLIB_Flows_US$LIB_recycling_vector,
  BESSLIB_Flows_US$Year
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
  select(State_Province, Segment, Propulsion, Year, recycle_df) %>%  # keep original Year here
  unnest(cols = recycle_df) 

BESS_future_recycle_type <- BESSLIB_Flows %>%
  mutate(
    recycle_df = map(LIB_recycling_vector, ~ {
      tibble(
        Sale_Year = as.integer(names(.x)),
        LIB_recycle_total = as.numeric(.x)
      )
    })
  ) %>%
  select(State_Province, Segment, Propulsion, Year, recycle_df) %>%  # keep original Year here
  unnest(cols = recycle_df) 

future_recycle_type <- full_join(future_recycle_type, BESS_future_recycle_type, by = c("State_Province","Segment","Propulsion","Year","Sale_Year")) %>%
  mutate(across(everything(), ~ replace_na(.x, 0))) %>%
  mutate(LIB_recycle_total = LIB_recycle_total.x+LIB_recycle_total.y) %>%
  select(-c(LIB_recycle_total.x, LIB_recycle_total.y))



scrap_by_mass = read_csv(file.path(data_folder, "Scrap_by_Mass (-Energy).csv"), na = "") %>%
  select(Chemistry, `Total Mass`, `Cell Mass`, `Pack Mass`) %>%   
  filter(!is.na(Chemistry), !is.na(`Total Mass`), !is.na(`Cell Mass`)) %>%
  mutate(Total_Cell_Mass_per_Year = `Cell Mass` * 211000000, ## cells per year from batpac
         Scrap_rate_kg_per_kg_cell = `Total Mass`/Total_Cell_Mass_per_Year,
         Cell_Pack = `Cell Mass`*400/`Pack Mass`) %>%
  #select(Chemistry, Scrap_rate_kg_per_kg_cell) %>%
  mutate(Avg = sum(Scrap_rate_kg_per_kg_cell)/9,
         Cell_Pack = sum(Cell_Pack)/9)

##RECYCLING
recycling_cap <- read_excel(file.path(data_folder, "NA recycling facilities.xlsx")) %>%
  select(
    "State/ Province",
    "Year online simplified",
    "Capacity simplified",
    "Feedstock simplified",
    "Final product recycling category"
  ) %>%
  rename(
    Year_online = `Year online simplified`,
    Capacity_Mt_yr = `Capacity simplified`,
    Feedstock = `Feedstock simplified`,
    Product_category = `Final product recycling category`
  ) %>%
  mutate(
    Year_online = ifelse(Year_online == "Online", 2025, Year_online),
    Year_online = as.integer(Year_online) 
  ) %>% filter (!is.na(Capacity_Mt_yr)) %>%
  mutate(
    Delay_online = case_when(
      Feedstock == "End-of-life battery" &
        Product_category == "Output" & Year_online > 2025 ~ Year_online + 2,
      TRUE ~ Year_online
    )
  ) 


black_mass <- recycling_cap %>%
  filter(
    (Product_category == "Output" & Feedstock == "End-of-life battery") |
      Product_category == "Intermediate"
  ) %>%
  group_by(`State/ Province`, Year_online) %>%
  summarise(
    Black_mass_cap = sum(Capacity_Mt_yr, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(`State/ Province`, Year_online) %>%
  group_by(`State/ Province`) %>%
  mutate(
    Cumulative_black_mass_cap = cumsum(Black_mass_cap)
  ) %>% rename(Year = Year_online) %>%  select(-Black_mass_cap)

# Step 2: Delay_Black_mass_cap (assuming it's grouped by Delay_online)
delay_black_mass <- recycling_cap %>%
  filter(
    (Product_category == "Output" & Feedstock == "End-of-life battery") |
      Product_category == "Intermediate"
  ) %>%
  group_by(`State/ Province`, Delay_online) %>%
  summarise(
    Delay_Black_mass_cap = sum(Capacity_Mt_yr, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(`State/ Province`, Delay_online) %>%
  group_by(`State/ Province`) %>%
  mutate(
    Delay_Cumulative_black_mass_cap = cumsum(Delay_Black_mass_cap)
  ) %>% rename(Year = Delay_online) %>% select(-Delay_Black_mass_cap)

# Step 3: Join the two datasets
black_mass_cap <- black_mass %>% full_join(
  delay_black_mass, by = c("Year", "State/ Province")) %>%
  mutate(`State/ Province` = as.character(`State/ Province`),
         Year = as.integer(Year))



refining <- recycling_cap %>%
  filter(Product_category == "Output") %>%
  group_by(`State/ Province`, Year_online) %>%
  summarise(
    Refining_cap = sum(Capacity_Mt_yr, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(`State/ Province`, Year_online) %>%
  group_by(`State/ Province`) %>%
  mutate(
    Cumulative_refining_cap = cumsum(Refining_cap) 
  ) %>% rename(Year = Year_online) %>% select(-Refining_cap)

delay_refining <- recycling_cap %>%
  filter(Product_category == "Output") %>%
  group_by(`State/ Province`, Delay_online) %>%
  summarise(
    Delay_refining_cap = sum(Capacity_Mt_yr, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(`State/ Province`, Delay_online) %>%
  group_by(`State/ Province`) %>%
  mutate(
    Delay_Cumulative_refining_cap = cumsum(Delay_refining_cap)
  ) %>% rename(Year = Delay_online) %>% select(-Delay_refining_cap)

refining_cap <- refining %>% full_join(
  delay_refining, by = c("Year", "State/ Province")) %>%
  mutate(`State/ Province` = as.character(`State/ Province`),
         Year = as.integer(Year))



recycling_tonnes_by_state <- full_join(
  black_mass_cap,
  refining_cap,
  by = c("State/ Province", "Year")
) %>%
  rename(State_Province = `State/ Province`) %>%
  select(
    Year, State_Province,
    Cumulative_black_mass_cap,
    Delay_Cumulative_black_mass_cap,
    Cumulative_refining_cap,
    Delay_Cumulative_refining_cap
  ) %>%
  mutate(
    State_Province = as.character(State_Province),
    Year = as.integer(Year)
  ) %>%
  ungroup() %>%
  
  
  # --- Fill missing years up to 2035 ---
  complete(State_Province, Year = seq(min(Year, na.rm = TRUE), 2035, 1)) %>%
  arrange(State_Province, Year) %>%
  
  # --- Forward-fill cumulative columns ---
  group_by(State_Province) %>%
  fill(
    Cumulative_black_mass_cap,
    Delay_Cumulative_black_mass_cap,
    Cumulative_refining_cap,
    Delay_Cumulative_refining_cap,
    .direction = "down"
  ) %>%
  
  # --- If first year was NA, set to 0 ---
  mutate(
    across(
      c(
        Cumulative_black_mass_cap,
        Delay_Cumulative_black_mass_cap,
        Cumulative_refining_cap,
        Delay_Cumulative_refining_cap
      ),
      ~ replace_na(.x, 0)
    )
  ) %>%
  ungroup()


recycling_tonnes_2030_projected <- recycling_tonnes_by_state %>% filter(Year == 2030) %>% select(-Delay_Cumulative_black_mass_cap, -Delay_Cumulative_refining_cap)
recycling_tonnes_2030_delayed <- recycling_tonnes_by_state %>% filter(Year == 2030) %>% select(-Cumulative_black_mass_cap, -Cumulative_refining_cap)


recycling_tonnes_total <- recycling_tonnes_by_state %>% group_by(Year) %>%
  summarise(Cumulative_black_mass_cap = sum(Cumulative_black_mass_cap, na.rm = TRUE),
            Cumulative_refining_cap = sum(Cumulative_refining_cap, na.rm = TRUE),
            Delay_Cumulative_black_mass_cap = sum(Delay_Cumulative_black_mass_cap, na.rm = TRUE),
            Delay_Cumulative_refining_cap = sum(Delay_Cumulative_refining_cap, na.rm = TRUE)) %>%
  mutate(Full_Recycle = case_when(Cumulative_refining_cap > Cumulative_black_mass_cap ~ Cumulative_black_mass_cap, TRUE ~ Cumulative_refining_cap)) %>%
  mutate(Delay_Full_Recycle = case_when(Delay_Cumulative_refining_cap > Delay_Cumulative_black_mass_cap ~ Delay_Cumulative_black_mass_cap, TRUE ~ Delay_Cumulative_refining_cap))

##MANUFACTURING
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
# Define years
scrap_rate <- seq(0.0767, 0.0434, length.out = 6)

## in Gwh
all_manufacturing <- read.xlsx("/Users/elsawefes-potter/Documents/Critical_Minerals_Pablo/Manu_Down_Mid.xlsx", sheet = "changed dates Narrowed Manu fac") %>%
  select(Year.online, Production.Capacity, Company, Facility.State.or.Province, Supply.Chain.Segment, Chemistry) %>%
  mutate(Gwh_yr = as.numeric(Production.Capacity)) %>%
  group_by(Supply.Chain.Segment, Facility.State.or.Province, Year.online) %>%
  summarise(Gwh_yr = sum(Gwh_yr, na.rm = TRUE), .groups = "drop") %>%
  rename(Year_Online = Year.online, State_Province = Facility.State.or.Province) %>%   
  filter(!is.na(Year_Online), Gwh_yr != 0) %>%
  mutate(Year_Online = as.numeric(Year_Online)) %>%
  pivot_wider(
    names_from = Supply.Chain.Segment,
    values_from = Gwh_yr,
    values_fn   = sum,
    values_fill = list(Gwh_yr = 0)
  ) %>%
  mutate(Downstream = Downstream *0.77,
         Midstream = Midstream * 0.77)
  

delayed_manufacturing <- all_manufacturing %>%
  mutate(
    Year_Online = case_when(
      Year_Online > 2026 ~ Year_Online + 5,
      TRUE ~ Year_Online
    )
  )
states <- all_manufacturing %>%
  distinct(State_Province, Year_Online)  # unique state + online year

calendar <- expand_grid(
  State_Province = unique(states$State_Province),
  Year = 2025:2035
)

calendar_delayed <- expand_grid(
  State_Province = unique(states$State_Province),
  Year = 2025:2040
)

decline_years <- 6
### keep at by year_online?--> how does this impact with the projection

## currently do scrap rate of kwh and then convert w specific energy (will just do mass scrapped per kwh --> no conversion)
all_manufacturing_expanded <- calendar %>%
  left_join(all_manufacturing, by = "State_Province", relationship = "many-to-many") %>%
  arrange(State_Province, Year) %>%
  group_by(State_Province, Year_Online) %>%
  mutate(
    Production_Adjusted_Down = ifelse(Year < Year_Online, 0, Downstream),
    Production_Adjusted_Mid = ifelse(Year < Year_Online, 0, Midstream),
    Scrap_Years = ifelse(Year >= Year_Online, Year - Year_Online + 1, NA),
    Scrap_Rate_Mid = if_else(
      !is.na(Scrap_Years),
      seq(0.1105567, 0.0772, length.out = decline_years)[
        pmin(Scrap_Years, decline_years)
      ],
      0
    ),
    # 
    # Scrap_Rate_Down = if_else(
    #   !is.na(Scrap_Years),
    #   seq(0.05, 0.0283, length.out = decline_years)[
    #     pmin(Scrap_Years, decline_years)
    #   ],
    #   0
    # ),
    Scrap_Rate_Down = 0.05,
    Gwh_Scrap_Down = Production_Adjusted_Down * Scrap_Rate_Down,
    Production_After_Scrap_Down = Production_Adjusted_Down * (1 - Scrap_Rate_Down),
    Gwh_Scrap_Mid = Production_Adjusted_Mid * Scrap_Rate_Mid,
    Production_After_Scrap_Mid = Production_Adjusted_Mid * (1 - Scrap_Rate_Mid)) %>% 
  ungroup() %>%
  group_by(Year, State_Province) %>%
  summarise(
    Gwh_Scrap_Down = sum(Gwh_Scrap_Down, na.rm = TRUE),
    Production_After_Scrap_Down = sum(Production_After_Scrap_Down, na.rm = TRUE),
    Gwh_Scrap_Mid = sum(Gwh_Scrap_Mid, na.rm = TRUE),
    Production_After_Scrap_Mid = sum(Production_After_Scrap_Mid, na.rm = TRUE),
    .groups = "drop_last"
  ) %>%
  group_by(Year) %>%
  mutate(
    Share_of_Year_Scrap_Down = Gwh_Scrap_Down / sum(Gwh_Scrap_Down, na.rm = TRUE),
    Share_of_Year_Prod_Down  = Production_After_Scrap_Down / sum(Production_After_Scrap_Down, na.rm = TRUE),
    Share_of_Year_Scrap_Mid = Gwh_Scrap_Mid / sum(Gwh_Scrap_Mid, na.rm = TRUE),
    Share_of_Year_Prod_Mid  = Production_After_Scrap_Mid / sum(Production_After_Scrap_Mid, na.rm = TRUE)
  ) %>%
  ungroup() 



delayed_manufacturing_expanded <- calendar_delayed %>%
  left_join(delayed_manufacturing, by = "State_Province", relationship = "many-to-many") %>%
  arrange(State_Province, Year) %>%
  group_by(State_Province, Year_Online) %>%
  mutate(
    Production_Adjusted_Down = ifelse(Year < Year_Online, 0, Downstream),
    Production_Adjusted_Mid = ifelse(Year < Year_Online, 0, Midstream),
    Scrap_Years = ifelse(Year >= Year_Online, Year - Year_Online + 1, NA),
    # Scrap_Rate_Mid = if_else(
    #   !is.na(Scrap_Years),
    #   seq(0.0767, 0.0434, length.out = decline_years)[
    #     pmin(Scrap_Years, decline_years)
    #   ],
    #   0
    # ),
    # 
    # Scrap_Rate_Down = if_else(
    #   !is.na(Scrap_Years),
    #   seq(0.05, 0.0283, length.out = decline_years)[
    #     pmin(Scrap_Years, decline_years)
    #   ],
    #   0
    # ),
    Scrap_Rate_Mid = 0.0767,
    Scrap_Rate_Down = 0.05,
    Gwh_Scrap_Down = Production_Adjusted_Down * Scrap_Rate_Down,
    Production_After_Scrap_Down = Production_Adjusted_Down * (1 - Scrap_Rate_Down),
    Gwh_Scrap_Mid = Production_Adjusted_Mid * Scrap_Rate_Mid,
    Production_After_Scrap_Mid = Production_Adjusted_Mid * (1 - Scrap_Rate_Mid)
  ) %>%
  ungroup() %>%
  group_by(Year, State_Province) %>%
  summarise(
    Gwh_Scrap_Down = sum(Gwh_Scrap_Down, na.rm = TRUE),
    Production_After_Scrap_Down = sum(Production_After_Scrap_Down, na.rm = TRUE),
    Gwh_Scrap_Mid = sum(Gwh_Scrap_Mid, na.rm = TRUE),
    Production_After_Scrap_Mid = sum(Production_After_Scrap_Mid, na.rm = TRUE),
    .groups = "drop_last"
  ) %>%
  group_by(Year) %>%
  mutate(
    Share_of_Year_Scrap_Down = Gwh_Scrap_Down / sum(Gwh_Scrap_Down, na.rm = TRUE),
    Share_of_Year_Prod_Down  = Production_After_Scrap_Down / sum(Production_After_Scrap_Down, na.rm = TRUE),
    Share_of_Year_Scrap_Mid = Gwh_Scrap_Mid / sum(Gwh_Scrap_Mid, na.rm = TRUE),
    Share_of_Year_Prod_Mid  = Production_After_Scrap_Mid / sum(Production_After_Scrap_Mid, na.rm = TRUE)
  ) %>%
  ungroup() 

## Still apply chemistry fractions
all_manu_chem <- read.csv("/Users/elsawefes-potter/Documents/Critical_Minerals_Pablo/total_manufacturing_edited.csv") %>%
  filter(Product_Abbrev != "-") %>%
  filter(Gwh.yr != "-") %>%
  mutate(Gwh.yr = as.numeric(Gwh.yr)) %>%
  select(Year, Product_Abbrev, Gwh.yr) %>%
  group_by(Product_Abbrev) %>%
  summarise(Gwh.yr = sum(Gwh.yr, na.rm = TRUE)) %>%
  mutate(
    Chem_Share = Gwh.yr/sum(Gwh.yr, na.rm = TRUE)) 

all_manu_chem <- all_manu_chem %>%
  mutate(Product_Abbrev = recode(Product_Abbrev,
                                 "NMC" = "High/Mid NMC")) %>%
  rename(`Cathode Mix` = Product_Abbrev)


## Still potentially reconsider if apply scrap to whole year (averaged) rather than by facility age
## Still need the year delay 
##FIND MANUFACTURING 20% adjustment based on 2028 fraction of consumption

### GET new Total LIB Flows ==> replacements only US and Canada
state_capacity_added <- EVLIB_Flows %>% group_by(State_Province, Year, Propulsion, Segment) %>% 
  summarise(LIB_new_add = sum(LIB_new_add, na.rm = TRUE)) %>%
  full_join (EV_Flows, by = c("State_Province", "Year", "Segment", "Propulsion")) %>%
  mutate(LIB_new_add = if_else(is.na(LIB_new_add), 0, LIB_new_add)) %>%
  mutate(Total_Add_LIB = LIB_new_add + Add_EV) %>%
  select(State_Province, Year, Segment, Propulsion, Total_Add_LIB)


###from FUTURE RECYCLING MINS
caps_projected <- batt_cap_projection %>% 
  select(Sale_Year, Segment, Propulsion, `Projected Avg Batt Cap (kwh/batt)`) %>% 
  rename(Year = Sale_Year) %>% 
  rename(Avg_Cap_Proj = `Projected Avg Batt Cap (kwh/batt)`) %>% 
  group_by(Year, Segment, Propulsion) %>%
  summarise(Avg_Cap_Proj = first(Avg_Cap_Proj))

caps_15_projected <- batt_cap_15 %>% 
  select(Sale_Year, Segment, Propulsion, `Projected Avg Batt Cap (kwh/batt)`) %>% 
  rename(Year = Sale_Year) %>% 
  rename(Avg_Cap_15 = `Projected Avg Batt Cap (kwh/batt)`) %>% 
  group_by(Year, Segment, Propulsion) %>%
  summarise(Avg_Cap_15 = first(Avg_Cap_15))

chem_proj <- future_match %>% rename (Year = Sale_Year) %>% rename(Mix_proj = `Cathode Mix Share`)
chem_15 <- final_adjusted_mix_extended %>% rename(Year = Sale_Year) %>% rename(Mix_15 = `Cathode Mix Share`)
chems_proj_15 <- chem_proj %>% left_join(chem_15, by=c("Year", "Cathode Mix"), relationship = "many-to-many")

##add into state libs to get gwh
state_cap_add <- state_capacity_added %>%
  left_join(caps_projected,     by = c("Year", "Segment", "Propulsion")) %>%
  left_join(caps_15_projected,  by = c("Year", "Segment", "Propulsion")) %>%
  mutate(Add_LIB_Gwh_proj = Avg_Cap_Proj * Total_Add_LIB/1e6) %>%
  mutate(Add_LIB_Gwh_15 = Avg_Cap_15 *Total_Add_LIB/1e6) %>% 
  filter(Year >= 2025) %>%
  select(State_Province, Year, Propulsion, Segment, Add_LIB_Gwh_15, Add_LIB_Gwh_proj) %>%
  group_by(State_Province, Year) %>%
  summarise(
    Add_LIB_Gwh_proj = sum(Add_LIB_Gwh_proj, na.rm = TRUE),
    Add_LIB_Gwh_15   = sum(Add_LIB_Gwh_15,   na.rm = TRUE)
  )%>%
  mutate(State_Province = case_when(
    State_Province %in% names(state_map_rev) ~ state_map_rev[State_Province],
    TRUE ~ State_Province   # leave unchanged if no match
  )) 

### DEMAND IN TONNES
state_cap_chem_tonne <- state_cap_add %>% 
  left_join(chems_proj_15, by= "Year", relationship = "many-to-many") %>%
  mutate(
    Add_LIB_Gwh_proj_chem = Add_LIB_Gwh_proj * Mix_proj,
    Add_LIB_Gwh_15_chem   = Add_LIB_Gwh_15 * Mix_proj,
    Add_LIB_Gwh_proj_LFP = Add_LIB_Gwh_proj *Mix_15,
    Add_LIB_Gwh_15_LFP = Add_LIB_Gwh_15*Mix_15
  ) %>% 
  left_join(specific_energy, by = "Cathode Mix") %>%
  mutate(Pack_kg_Gwh = Pack_kg_kwh*1000000) %>%
  mutate(
    Add_LIB_proj_tonnes = (Add_LIB_Gwh_proj_chem*Pack_kg_Gwh)/1000,
    Add_LIB_15_tonnes = (Add_LIB_Gwh_15_chem*Pack_kg_Gwh)/1000,
    Add_LIB_proj_LFP_tonnes = (Add_LIB_Gwh_proj_LFP*Pack_kg_Gwh)/1000,
    Add_LIB_15_LFP_tonnes = (Add_LIB_Gwh_15_LFP*Pack_kg_Gwh)/1000
  ) %>% group_by(Year, State_Province) %>%
  summarise(Add_LIB_proj_tonnes = sum(Add_LIB_proj_tonnes, na.rm = TRUE),
            Add_LIB_15_tonnes = sum(Add_LIB_15_tonnes, na.rm = TRUE),
            Add_LIB_proj_LFP_tonnes = sum(Add_LIB_proj_LFP_tonnes, na.rm = TRUE),
            Add_LIB_15_LFP_tonnes = sum(Add_LIB_15_LFP_tonnes, na.rm = TRUE)) %>% 
  select(Year, State_Province, Add_LIB_proj_tonnes, Add_LIB_15_tonnes, Add_LIB_proj_LFP_tonnes, Add_LIB_15_LFP_tonnes) 

state_demand_tonnes_2030 <- state_cap_chem_tonne %>% filter(Year == 2030) 

###USE National in Gwh to get manufacturing projection
nat_cap_add <- state_cap_add %>%group_by(Year) %>% 
  summarise(Add_LIB_Gwh_proj = sum(Add_LIB_Gwh_proj, na.rm = TRUE), 
            Add_LIB_Gwh_15 = sum(Add_LIB_Gwh_15, na.rm = TRUE))

nat_manu <- all_manufacturing_expanded %>% group_by(Year) %>%
  summarise(Gwh_Scrap_Down = sum(Gwh_Scrap_Down, na.rm = TRUE), 
            Production_After_Scrap_Down = sum(Production_After_Scrap_Down, na.rm= TRUE),
            Gwh_Scrap_Mid = sum(Gwh_Scrap_Mid, na.rm = TRUE), 
            Production_After_Scrap_Mid = sum(Production_After_Scrap_Mid, na.rm= TRUE))

nat_manu_delayed <- delayed_manufacturing_expanded %>% group_by(Year) %>%
  summarise(Gwh_Scrap_Down = sum(Gwh_Scrap_Down, na.rm = TRUE), 
            Production_After_Scrap_Down = sum(Production_After_Scrap_Down, na.rm= TRUE),
            Gwh_Scrap_Mid = sum(Gwh_Scrap_Mid, na.rm = TRUE), 
            Production_After_Scrap_Mid = sum(Production_After_Scrap_Mid, na.rm= TRUE))

cap_vs_manufac <- left_join(
  nat_cap_add,
  nat_manu,
  by = "Year"
) 

cap_vs_delayed_manu <- left_join(nat_cap_add, nat_manu_delayed, by = "Year")

manu_projected <- cap_vs_manufac %>%
  fill(Production_After_Scrap_Down, Gwh_Scrap_Down, Production_After_Scrap_Mid, Gwh_Scrap_Mid,
       .direction = "down") %>%
  mutate(
    Production_After_Scrap_Down_proj = case_when(
      Production_After_Scrap_Down < Add_LIB_Gwh_proj ~ Add_LIB_Gwh_proj,
      TRUE ~ Production_After_Scrap_Down
    ), Gwh_Scrap_Down_Proj = case_when(
      Production_After_Scrap_Down < Add_LIB_Gwh_proj ~
        Gwh_Scrap_Down +
        (1 - Production_After_Scrap_Down / Add_LIB_Gwh_proj) * Gwh_Scrap_Down,
      TRUE ~ Gwh_Scrap_Down
      ##MIDS
    ), Production_After_Scrap_Mid_proj = case_when(
      Production_After_Scrap_Mid < Add_LIB_Gwh_proj ~ Add_LIB_Gwh_proj,
      TRUE ~ Production_After_Scrap_Mid
    ), Gwh_Scrap_Mid_Proj = case_when(
      Production_After_Scrap_Mid < Add_LIB_Gwh_proj ~
        Gwh_Scrap_Mid +
        (1 - Production_After_Scrap_Mid / Add_LIB_Gwh_proj) * Gwh_Scrap_Mid,
      TRUE ~ Gwh_Scrap_Mid),
    ## 15% reduction
    Production_After_Scrap_Down_15 = case_when(
      Production_After_Scrap_Down < Add_LIB_Gwh_15 ~ Add_LIB_Gwh_15,
      TRUE ~ Production_After_Scrap_Down
    ), Gwh_Scrap_Down_15 = case_when(
      Production_After_Scrap_Down < Add_LIB_Gwh_15 ~
        Gwh_Scrap_Down +
        (1 - Production_After_Scrap_Down / Add_LIB_Gwh_15) * Gwh_Scrap_Down,
      TRUE ~ Gwh_Scrap_Down
      ##MIDS 15%
    ), Production_After_Scrap_Mid_15 = case_when(
      Production_After_Scrap_Mid < Add_LIB_Gwh_15 ~ Add_LIB_Gwh_15,
      TRUE ~ Production_After_Scrap_Mid
    ), Gwh_Scrap_Mid_15 = case_when(
      Production_After_Scrap_Mid < Add_LIB_Gwh_15 ~
        Gwh_Scrap_Mid +
        (1 - Production_After_Scrap_Mid / Add_LIB_Gwh_15) * Gwh_Scrap_Mid,
      TRUE ~ Gwh_Scrap_Mid)
  ) %>% select(-c(Production_After_Scrap_Down, Gwh_Scrap_Down, Production_After_Scrap_Mid, Gwh_Scrap_Mid))

### COME BACK
manu_delayed <- cap_vs_delayed_manu %>%  
  fill(
    Production_After_Scrap_Down, 
    Gwh_Scrap_Down, 
    Production_After_Scrap_Mid, 
    Gwh_Scrap_Mid,
    .direction = "down"
  ) %>%
  mutate(
    ## Downstream projections
    Production_After_Scrap_Down_proj = case_when(
      Year > 2030 ~ Add_LIB_Gwh_proj,
      TRUE ~ Production_After_Scrap_Down
    ),
    Gwh_Scrap_Down_Proj = case_when(
      Production_After_Scrap_Down < Add_LIB_Gwh_proj ~
        Gwh_Scrap_Down + (1 - Production_After_Scrap_Down / Add_LIB_Gwh_proj) * Gwh_Scrap_Down,
      TRUE ~ Gwh_Scrap_Down - (1 - Add_LIB_Gwh_proj / Production_After_Scrap_Down) * Gwh_Scrap_Down
    ),
    
    ## Midstream projections
    Production_After_Scrap_Mid_proj = case_when(
      Year > 2030 ~ Add_LIB_Gwh_proj,
      TRUE ~ Production_After_Scrap_Mid
    ),
    Gwh_Scrap_Mid_Proj = case_when(
      Production_After_Scrap_Mid < Add_LIB_Gwh_proj ~
        Gwh_Scrap_Mid + (1 - Production_After_Scrap_Mid / Add_LIB_Gwh_proj) * Gwh_Scrap_Mid,
      TRUE ~ Gwh_Scrap_Mid - (1 - Add_LIB_Gwh_proj / Production_After_Scrap_Mid) * Gwh_Scrap_Mid
    ),
    
    ## Downstream 15% reduction
    Production_After_Scrap_Down_15 = case_when(
      Year > 2030 ~ Add_LIB_Gwh_15,
      TRUE ~ Production_After_Scrap_Down
    ),
    Gwh_Scrap_Down_15 = case_when(
      Production_After_Scrap_Down < Add_LIB_Gwh_15 ~
        Gwh_Scrap_Down + (1 - Production_After_Scrap_Down / Add_LIB_Gwh_15) * Gwh_Scrap_Down,
      TRUE ~ Gwh_Scrap_Down - (1 - Add_LIB_Gwh_15 / Production_After_Scrap_Down) * Gwh_Scrap_Down
    ),
    
    ## Midstream 15% reduction
    Production_After_Scrap_Mid_15 = case_when(
      Year > 2030 ~ Add_LIB_Gwh_15,
      TRUE ~ Production_After_Scrap_Mid
    ),
    Gwh_Scrap_Mid_15 = case_when(
      Production_After_Scrap_Mid < Add_LIB_Gwh_15 ~
        Gwh_Scrap_Mid + (1 - Production_After_Scrap_Mid / Add_LIB_Gwh_15) * Gwh_Scrap_Mid,
      TRUE ~ Gwh_Scrap_Mid - (1 - Add_LIB_Gwh_15 / Production_After_Scrap_Mid) * Gwh_Scrap_Mid
    )
  ) %>%
  select(
    -c(Production_After_Scrap_Down, Gwh_Scrap_Down, Production_After_Scrap_Mid, Gwh_Scrap_Mid)
  )

                          
## assumption that continuing same amount of production and scrap and same split of where facilities 
# are and range at until around 2035 --> after that do ramp down of scrap 2028-2035 

### current and constructed-- assume it does go online- some go to BESS- scrap it's the same 
### planned either on or significantly delayed


### apply chemistries 
projected_manufac_by_chem <- tidyr::crossing(manu_projected, all_manu_chem) %>%
  mutate(Prod_proj_down = Production_After_Scrap_Down_proj * Chem_Share,
         Prod_15_down = Production_After_Scrap_Down_15 * Chem_Share, 
         Scrap_proj_down = Gwh_Scrap_Down_Proj*Chem_Share, 
         Scrap_15_down = Gwh_Scrap_Down_15*Chem_Share,
         Prod_proj_mid = Production_After_Scrap_Mid_proj * Chem_Share,
         Prod_15_mid = Production_After_Scrap_Mid_15 * Chem_Share, 
         Scrap_proj_mid = Gwh_Scrap_Mid_Proj*Chem_Share, 
         Scrap_15_mid = Gwh_Scrap_Mid_15*Chem_Share
         ) %>%
  select(Year,`Cathode Mix`, Prod_proj_down, Prod_15_down, Scrap_proj_down, Scrap_15_down, Prod_proj_mid, Prod_15_mid, Scrap_proj_mid, Scrap_15_mid) 


delayed_manufac_by_chem <-  tidyr::crossing(manu_delayed, all_manu_chem) %>%
  mutate(Prod_proj_down = Production_After_Scrap_Down_proj * Chem_Share,
         Prod_15_down = Production_After_Scrap_Down_15 * Chem_Share, 
         Scrap_proj_down = Gwh_Scrap_Down_Proj*Chem_Share, 
         Scrap_15_down = Gwh_Scrap_Down_15*Chem_Share,
         Prod_proj_mid = Production_After_Scrap_Mid_proj * Chem_Share,
         Prod_15_mid = Production_After_Scrap_Mid_15 * Chem_Share, 
         Scrap_proj_mid = Gwh_Scrap_Mid_Proj*Chem_Share, 
         Scrap_15_mid = Gwh_Scrap_Mid_15*Chem_Share
  ) %>%
  select(Year,`Cathode Mix`, Prod_proj_down, Prod_15_down, Scrap_proj_down, Scrap_15_down, Prod_proj_mid, Prod_15_mid, Scrap_proj_mid, Scrap_15_mid) 


## used the percents rather than the mass reported scrapped by batpac-- then specific energy
tonnes_manufac_projected<- projected_manufac_by_chem %>% 
  left_join(specific_energy, by = "Cathode Mix", relationship= "many-to-many") %>%
  drop_na()%>%
  mutate(Pack_kg_Gwh = Pack_kg_kwh*1000000,
         Cell_kg_Gwh = Cell_kg_kwh*1000000) %>%
  mutate(Tonnes_Scrap_proj_down = (Scrap_proj_down * Pack_kg_Gwh)/1000,
         Tonnes_Scrap_15_down = (Scrap_15_down *Pack_kg_Gwh)/1000, 
         Tonnes_Prod_proj_down = (Prod_proj_down *Pack_kg_Gwh)/1000,
         Tonnes_Prod_15_down = (Prod_15_down * Pack_kg_Gwh)/1000,
         Tonnes_Scrap_proj_mid = (Scrap_proj_mid * Cell_kg_Gwh)/1000,
         Tonnes_Scrap_15_mid = (Scrap_15_mid *Cell_kg_Gwh)/1000, 
         Tonnes_Prod_proj_mid = (Prod_proj_mid *Cell_kg_Gwh)/1000,
         Tonnes_Prod_15_mid = (Prod_15_mid * Cell_kg_Gwh)/1000) %>%
  group_by(Year) %>%
  summarise(Tonnes_Scrap_proj_down = sum(Tonnes_Scrap_proj_down, na.rm = TRUE),
            Tonnes_Scrap_15_down = sum(Tonnes_Scrap_15_down, na.rm = TRUE),
            Tonnes_Prod_proj_down = sum(Tonnes_Prod_proj_down, na.rm = TRUE),
            Tonnes_Prod_15_down = sum(Tonnes_Prod_15_down, na.rm = TRUE),
            Tonnes_Scrap_proj_mid = sum(Tonnes_Scrap_proj_mid, na.rm = TRUE),
            Tonnes_Scrap_15_mid = sum(Tonnes_Scrap_15_mid, na.rm = TRUE),
            Tonnes_Prod_proj_mid = sum(Tonnes_Prod_proj_mid, na.rm = TRUE),
            Tonnes_Prod_15_mid = sum(Tonnes_Prod_15_mid, na.rm = TRUE)) %>%
  select(Year, Tonnes_Scrap_proj_down, Tonnes_Scrap_15_down, Tonnes_Prod_proj_down, Tonnes_Prod_15_down, Tonnes_Scrap_proj_mid, Tonnes_Scrap_15_mid, Tonnes_Prod_proj_mid, Tonnes_Prod_15_mid)

tonnes_manufac_delayed<- delayed_manufac_by_chem %>% 
  left_join(specific_energy, by = "Cathode Mix", relationship= "many-to-many") %>%
  drop_na()%>%
  mutate(Pack_kg_Gwh = Pack_kg_kwh*1000000,
         Cell_kg_Gwh = Cell_kg_kwh*1000000) %>%
  mutate(Tonnes_Scrap_proj_down = (Scrap_proj_down * Pack_kg_Gwh)/1000,
         Tonnes_Scrap_15_down = (Scrap_15_down *Pack_kg_Gwh)/1000, 
         Tonnes_Prod_proj_down = (Prod_proj_down *Pack_kg_Gwh)/1000,
         Tonnes_Prod_15_down = (Prod_15_down * Pack_kg_Gwh)/1000,
         Tonnes_Scrap_proj_mid = (Scrap_proj_mid * Cell_kg_Gwh)/1000,
         Tonnes_Scrap_15_mid = (Scrap_15_mid *Cell_kg_Gwh)/1000, 
         Tonnes_Prod_proj_mid = (Prod_proj_mid *Cell_kg_Gwh)/1000,
         Tonnes_Prod_15_mid = (Prod_15_mid * Cell_kg_Gwh)/1000) %>%
  group_by(Year) %>%
  summarise(Tonnes_Scrap_proj_down = sum(Tonnes_Scrap_proj_down, na.rm = TRUE),
            Tonnes_Scrap_15_down = sum(Tonnes_Scrap_15_down, na.rm = TRUE),
            Tonnes_Prod_proj_down = sum(Tonnes_Prod_proj_down, na.rm = TRUE),
            Tonnes_Prod_15_down = sum(Tonnes_Prod_15_down, na.rm = TRUE),
            Tonnes_Scrap_proj_mid = sum(Tonnes_Scrap_proj_mid, na.rm = TRUE),
            Tonnes_Scrap_15_mid = sum(Tonnes_Scrap_15_mid, na.rm = TRUE),
            Tonnes_Prod_proj_mid = sum(Tonnes_Prod_proj_mid, na.rm = TRUE),
            Tonnes_Prod_15_mid = sum(Tonnes_Prod_15_mid, na.rm = TRUE)) %>%
  select(Year, Tonnes_Scrap_proj_down, Tonnes_Scrap_15_down, Tonnes_Prod_proj_down, Tonnes_Prod_15_down, Tonnes_Scrap_proj_mid, Tonnes_Scrap_15_mid, Tonnes_Prod_proj_mid, Tonnes_Prod_15_mid)

## Limits to 2035 bc state ratios --> only applicable to 2030 bc of shares
manufacturing_by_state_projected <- all_manufacturing_expanded  %>%                # only years <= 2035
  mutate(State_Province = if_else(State_Province == "SLP ", "MX", State_Province)) %>%
  left_join(tonnes_manufac_projected, by = "Year") %>%  # join totals by year
  mutate(
    Tonnes_Scrap_proj_down  = Tonnes_Scrap_proj_down  * coalesce(Share_of_Year_Scrap_Down, 0),
    Tonnes_Scrap_15_down    = Tonnes_Scrap_15_down    * coalesce(Share_of_Year_Scrap_Down, 0),
    Tonnes_Prod_proj_down   = Tonnes_Prod_proj_down   * coalesce(Share_of_Year_Prod_Down, 0),
    Tonnes_Prod_15_down     = Tonnes_Prod_15_down     * coalesce(Share_of_Year_Prod_Down, 0),
    Tonnes_Scrap_proj_mid   = Tonnes_Scrap_proj_mid   * coalesce(Share_of_Year_Scrap_Mid, 0),
    Tonnes_Scrap_15_mid     = Tonnes_Scrap_15_mid     * coalesce(Share_of_Year_Scrap_Mid, 0),
    Tonnes_Prod_proj_mid    = Tonnes_Prod_proj_mid    * coalesce(Share_of_Year_Prod_Mid, 0),
    Tonnes_Prod_15_mid      = Tonnes_Prod_15_mid      * coalesce(Share_of_Year_Prod_Mid, 0)
  ) %>%
  select(
    Year, State_Province, 
    Tonnes_Scrap_proj_down, Tonnes_Scrap_15_down,
    Tonnes_Prod_proj_down, Tonnes_Prod_15_down,
    Tonnes_Scrap_proj_mid, Tonnes_Scrap_15_mid,
    Tonnes_Prod_proj_mid, Tonnes_Prod_15_mid
  )

manufacturing_by_state_delayed <- all_manufacturing_expanded  %>%                # only years <= 2035
  mutate(State_Province = if_else(State_Province == "SLP ", "MX", State_Province)) %>%
  left_join(tonnes_manufac_delayed, by = "Year") %>%  # join totals by year
  mutate(
    Tonnes_Scrap_proj_down  = Tonnes_Scrap_proj_down  * coalesce(Share_of_Year_Scrap_Down, 0),
    Tonnes_Scrap_15_down    = Tonnes_Scrap_15_down    * coalesce(Share_of_Year_Scrap_Down, 0),
    Tonnes_Prod_proj_down   = Tonnes_Prod_proj_down   * coalesce(Share_of_Year_Prod_Down, 0),
    Tonnes_Prod_15_down     = Tonnes_Prod_15_down     * coalesce(Share_of_Year_Prod_Down, 0),
    Tonnes_Scrap_proj_mid   = Tonnes_Scrap_proj_mid   * coalesce(Share_of_Year_Scrap_Mid, 0),
    Tonnes_Scrap_15_mid     = Tonnes_Scrap_15_mid     * coalesce(Share_of_Year_Scrap_Mid, 0),
    Tonnes_Prod_proj_mid    = Tonnes_Prod_proj_mid    * coalesce(Share_of_Year_Prod_Mid, 0),
    Tonnes_Prod_15_mid      = Tonnes_Prod_15_mid      * coalesce(Share_of_Year_Prod_Mid, 0)
  ) %>%
  select(
    Year, State_Province, 
    Tonnes_Scrap_proj_down, Tonnes_Scrap_15_down,
    Tonnes_Prod_proj_down, Tonnes_Prod_15_down,
    Tonnes_Scrap_proj_mid, Tonnes_Scrap_15_mid,
    Tonnes_Prod_proj_mid, Tonnes_Prod_15_mid
  )

## all in projected scenario rn
manufacturing_tonnes_2030_projected = manufacturing_by_state_projected %>% filter(Year == 2030)
manufacturing_tonnes_2030_delayed = manufacturing_by_state_delayed %>% filter(Year == 2030)

## PREPPING TO PLOT-- put in delayed scenario
Mass_all_years <- full_join(state_cap_chem_tonne, 
                            manufacturing_by_state_projected, 
                            manufacturing_by_state_delayed,
                            by = c("Year","State_Province")) %>%
  full_join(recycling_tonnes_by_state,
            by = c("Year","State_Province")) %>%
  mutate(across(where(is.numeric), ~replace_na(.x, 0)))
                            

### ALL REGULAR MANUFACTURING 
Mass_2030_projected <- full_join(state_demand_tonnes_2030,
                       manufacturing_tonnes_2030_projected,
                       by = c("Year", "State_Province")) %>%
  full_join(recycling_tonnes_2030_projected,
            by = c("Year", "State_Province")) %>%
  mutate(across(where(is.numeric), ~replace_na(.x, 0)))


## State level one
Mass_2030_projected_ref <- Mass_2030_projected %>% 
  select(Year, State_Province, Add_LIB_proj_tonnes, Add_LIB_15_tonnes, 
         Tonnes_Prod_proj_down, Tonnes_Prod_15_down, Tonnes_Prod_proj_mid, Tonnes_Prod_15_mid,
         Cumulative_black_mass_cap, Cumulative_refining_cap)


### CHANGED TO 2030--include delay and 15% in one plot for national compare
Mass_2030_projected_ref <- Mass_2030_projected_ref %>%
  #mutate(State_Province = factor(State_Province, levels = west_to_east[west_to_east %in% State_Province])) %>%
  mutate(
    across(
      c(
        Add_LIB_proj_tonnes,
        Add_LIB_15_tonnes,
        Tonnes_Prod_proj_down,
        Tonnes_Prod_15_down,
        Tonnes_Prod_proj_mid,
        Tonnes_Prod_15_mid,
        Cumulative_black_mass_cap,
        Cumulative_refining_cap
      ),
      ~ .x / 1e6
    )
  ) %>%
  rename(`Demand` = Add_LIB_proj_tonnes, 
         `15% Reduced Batt Cap Demand` = Add_LIB_15_tonnes,
         `Pack Manufacturing` = Tonnes_Prod_proj_down, 
         `15% Reduced Batt Cap Pack Manufacturing` = Tonnes_Prod_15_down,
         `Cell Manufacturing` = Tonnes_Prod_proj_mid, 
         `15% Reduced Batt Cap Cell Manufacturing` = Tonnes_Prod_15_mid,
         `Black Mass` = Cumulative_black_mass_cap, 
         `Refining` = Cumulative_refining_cap) %>%
  select(-c(`15% Reduced Batt Cap Demand`,`15% Reduced Batt Cap Pack Manufacturing`,`15% Reduced Batt Cap Cell Manufacturing`)) %>%
  pivot_longer(cols = c(`Demand`,
                        `Pack Manufacturing`,
                        `Cell Manufacturing`,
                        `Black Mass`, `Refining`),
               names_to = "Origin",
               values_to = "Metric Tonnes (millions)") 

Mass_2030_projected_ref <- Mass_2030_projected_ref %>%
  mutate(
    Origin = as.character(Origin),  # ensures no leftover factor levels
    Origin = factor(
      Origin,
      levels = c(
        "Demand",
#        "15% Reduced Batt Cap Demand",
        "Pack Manufacturing",
#        "15% Reduced Batt Cap Pack Manufacturing",
        "Cell Manufacturing",
#        "15% Reduced Batt Cap Cell Manufacturing",
        "Black Mass",
        "Refining"
      )
    )
  )

## DELAYED DFs
Mass_2030_delayed <- full_join(state_demand_tonnes_2030,
                                 manufacturing_tonnes_2030_delayed,
                                 by = c("Year", "State_Province")) %>%
  full_join(recycling_tonnes_2030_delayed,
            by = c("Year", "State_Province")) %>%
  mutate(across(where(is.numeric), ~replace_na(.x, 0))) %>%
  rename(Cumulative_black_mass_cap = Delay_Cumulative_black_mass_cap, Cumulative_refining_cap = Delay_Cumulative_refining_cap)


## State level one
Mass_2030_delayed_ref <- Mass_2030_delayed %>% 
  select(Year, State_Province, Add_LIB_proj_tonnes, Add_LIB_15_tonnes, 
         Tonnes_Prod_proj_down, Tonnes_Prod_15_down, Tonnes_Prod_proj_mid, Tonnes_Prod_15_mid,
         Cumulative_black_mass_cap, Cumulative_refining_cap) 

### CHANGED TO 2030--include delay and 15% in one plot for national compare
Mass_2030_delayed_ref <- Mass_2030_delayed_ref %>%
  #mutate(State_Province = factor(State_Province, levels = west_to_east[west_to_east %in% State_Province])) %>%
  mutate(
    across(
      c(
        Add_LIB_proj_tonnes,
        Add_LIB_15_tonnes,
        Tonnes_Prod_proj_down,
        Tonnes_Prod_15_down,
        Tonnes_Prod_proj_mid,
        Tonnes_Prod_15_mid,
        Cumulative_black_mass_cap,
        Cumulative_refining_cap
      ),
      ~ .x / 1e6
    )
  ) %>%
  rename(`Demand` = Add_LIB_proj_tonnes, 
         `15% Reduced Batt Cap Demand` = Add_LIB_15_tonnes,
         `Pack Manufacturing` = Tonnes_Prod_proj_down, 
         `15% Reduced Batt Cap Pack Manufacturing` = Tonnes_Prod_15_down,
         `Cell Manufacturing` = Tonnes_Prod_proj_mid, 
         `15% Reduced Batt Cap Cell Manufacturing` = Tonnes_Prod_15_mid,
         `Black Mass` = Cumulative_black_mass_cap, 
         `Refining` = Cumulative_refining_cap) %>%
  select(-c(`15% Reduced Batt Cap Demand`,`15% Reduced Batt Cap Pack Manufacturing`,`15% Reduced Batt Cap Cell Manufacturing`)) %>%
  pivot_longer(cols = c(`Demand`,
                        `Pack Manufacturing`,
                        `Cell Manufacturing`,
                        `Black Mass`, `Refining`),
               names_to = "Origin",
               values_to = "Metric Tonnes (millions)") 

Mass_2030_delayed_ref <- Mass_2030_delayed_ref %>%
  mutate(
    Origin = as.character(Origin),  # ensures no leftover factor levels
    Origin = factor(
      Origin,
      levels = c(
        "Demand",
#        "15% Reduced Batt Cap Demand",
        "Pack Manufacturing",
#        "15% Reduced Batt Cap Pack Manufacturing",
        "Cell Manufacturing",
#        "15% Reduced Batt Cap Cell Manufacturing",
        "Black Mass",
        "Refining"
      )
    )
  )


origin_colors <- c(
  "Demand" = "#1b9e77",
 "15% Reduced Batt Cap Demand" = "#b2dfdb",
  "Pack Manufacturing" = "#D77FBF",
  "15% Reduced Batt Cap Pack Manufacturing" = "#EEC3DE",
  "Cell Manufacturing" = "#FC8D62",
  "15% Reduced Batt Cap Cell Manufacturing" = "#FDD0B5",
  "Black Mass" = "#000000",
  "Refining" = "#FFD700"
)

origin_colors <- origin_colors[names(origin_colors) %in% unique(Mass_2030_projected_ref$Origin)]

###PLOTTING JUST PLUG IN DELAYED OR NOT
grid_df <- as_tibble(ca_us_prov_state_grid1)

grid_df <- grid_df %>%
  filter(code != "PR") %>%          # remove PR if still present
  filter(code != "MX") %>%          # remove all duplicates first
  distinct(code, .keep_all = TRUE)  # keep only one MX row


# Add Mexico
grid_df <- grid_df %>%
  add_row(
    code = "MX",
    name = "Mexico",
    row = max(grid_df$row, na.rm = TRUE) + 1,  # position it below existing rows
    col = 4  # adjust column as you like
  )

# restore geofacet_grid class
class(grid_df) <- c("geofacet_grid", "data.frame")

# assign back
ca_us_prov_state_grid1 <- grid_df

library(scales)



ggplot(
  Mass_2030_projected_ref,
  aes(x = Origin, y = `Metric Tonnes (millions)`, fill = Origin)
) +
  geom_col() +
  facet_geo(~ State_Province, grid = ca_us_prov_state_grid1) +
  labs(
    title = "North American Battery Demand, Manufacturing and Recycling Tonnage (2030)",
    y = "Metric Tonnes (millions)",
    x = "Supply Chain Segment (Baseline Battery Capacity and Original Chemistry Projections)"
  ) +
  scale_y_continuous(
    trans = scales::pseudo_log_trans(base = 10, sigma = 0.1),
    breaks = function(x) {
      c(
        0.25, 0.75,
        10 ^ seq(
          floor(log10(max(1, min(x, na.rm = TRUE)))),
          ceiling(log10(max(x, na.rm = TRUE)))
        )
      )
    },
    labels = scales::comma
  ) +
  scale_fill_manual(
    values = origin_colors
  ) +
  theme_minimal(base_size = 14) +
  theme(
    axis.text.x = element_blank(),  # remove all x-axis text
    axis.ticks.x = element_blank(), # remove x-axis ticks
    legend.position = "bottom",
    plot.title = element_text(size = 18, face = "bold", hjust = 0.5),
    axis.title = element_text(size = 14, face = "bold"),
    axis.text = element_text(size = 12),
    strip.text = element_text(size = 14),
    legend.title = element_text(size = 12, face = "bold"),
    legend.text = element_text(size = 11),
    panel.grid.major = element_line(color = "grey70", linewidth = 0.6),
    panel.grid.minor = element_line(color = "grey80", linewidth = 0.4),
    # larger & centered# keep legend at bottom
  )

## JUST PLUG IN DELAYED OR NOT
## National compare scenarios 2035
Nat_Mass_2030 <- Mass_2030_projected %>%
  group_by(Year) %>%                        
  summarise(
    Add_LIB_proj_tonnes = sum(Add_LIB_proj_tonnes, na.rm = TRUE), 
    Add_LIB_15_tonnes = sum(Add_LIB_15_tonnes, na.rm = TRUE),
    Add_LIB_proj_LFP_tonnes = sum(Add_LIB_proj_LFP_tonnes, na.rm = TRUE),
    Add_LIB_15_LFP_tonnes = sum(Add_LIB_15_LFP_tonnes, na.rm = TRUE),
    Tonnes_Prod_proj_down = sum(Tonnes_Prod_proj_down, na.rm = TRUE),
    Tonnes_Prod_15_down = sum(Tonnes_Prod_15_down, na.rm = TRUE),
    Tonnes_Prod_proj_mid = sum(Tonnes_Prod_proj_mid, na.rm = TRUE),
    Tonnes_Prod_15_mid = sum(Tonnes_Prod_15_mid, na.rm = TRUE),
    Cumulative_black_mass_cap = sum(Cumulative_black_mass_cap, na.rm = TRUE),
    Cumulative_refining_cap = sum(Cumulative_refining_cap, na.rm = TRUE),
    .groups = "drop"  # <-- make sure this is after all commas
  ) %>% 
  rename("Demand (Baseline Capacity - Original Chemistry)" = Add_LIB_proj_tonnes,
         "Demand (15% Lower Batt Cap - Original Chemistry)" = Add_LIB_15_tonnes, 
         "Demand (Baseline Capacity - High LFP)" = Add_LIB_proj_LFP_tonnes,
         "Demand (15% Lower Batt Cap  - High LFP)" = Add_LIB_15_LFP_tonnes,
         "Pack Manufacturing" = Tonnes_Prod_proj_down,
         "15% Lower Batt Cap Pack Manufacturing" = Tonnes_Prod_15_down,
         "Cell Manufacturing" = Tonnes_Prod_proj_mid,
         "15% Lower Batt Cap Cell Manufacturing" = Tonnes_Prod_15_mid,
         "Black Mass" = Cumulative_black_mass_cap,
         "Refining" = Cumulative_refining_cap)


Nat_Mass_2030_long <- Nat_Mass_2030 %>% 
  pivot_longer(
    cols = -Year,           # keep Year as a separate column
    names_to = "Metric",    # column that stores the original column names
    values_to = "Tonnes"    # column that stores values
  ) %>% select(-Year) %>%
  mutate(Tonnes = Tonnes/1e6,
         Metric = factor(Metric, levels = c(
           "Demand (Baseline Capacity - Original Chemistry)",
           "Demand (15% Lower Batt Cap - Original Chemistry)",
           "Demand (Baseline Capacity - High LFP)",
           "Demand (15% Lower Batt Cap  - High LFP)",
           "Pack Manufacturing",
           "15% Lower Batt Cap Pack Manufacturing",
           "Cell Manufacturing",
           "15% Lower Batt Cap Cell Manufacturing",
           "Black Mass",
           "Refining"
         ))                                      # keep your desired order
  )


ggplot(Nat_Mass_2030_long, aes(x = Metric, y = Tonnes, fill = Metric)) +
  geom_col() +
  geom_text(aes(label = scales::comma(Tonnes)), 
            vjust = -0.3, size = 3) +  # label above each bar
  scale_y_continuous(labels = comma) +
  labs(
    x = "Supply Chain Segment (Battery Capacity - Chemistry Scenario)",
    y = "Metric Tonnes Batteries (millions)",
    fill = NULL,
    title = "North American Demand, Manufacturing and Recycling Tonnage (2030)"
  ) +
  scale_fill_manual(
    values = c(
      # Demand — Original Chemistry
      "Demand (Baseline Capacity - Original Chemistry)" = "#1b9e77",   # teal
      "Demand (15% Lower Batt Cap - Original Chemistry)" = "#b2dfdb", # light teal
      
      # Demand — High LFP
      "Demand (Baseline Capacity - High LFP)" = "#66A61E",             # olive
      "Demand (15% Lower Batt Cap  - High LFP)" = "#C7E9A8",           # light olive
      
      # Pack Manufacturing — slightly more purple
      "Pack Manufacturing" = "#D77FBF",                 # purple-pink
      "15% Lower Batt Cap Pack Manufacturing" = "#EEC3DE",  # light purple-pink
      
      # Cell Manufacturing — coral
      "Cell Manufacturing" = "#FC8D62",                                # coral
      "15% Lower Batt Cap Cell Manufacturing" = "#FDD0B5",          # light coral
      
      # Recycling / Refining
      "Black Mass" = "#000000",                                        # black
      "Refining" = "#E6AB02"                                           # golden amber
    )
  ) +
  theme_minimal(base_size = 14) +
  theme(
    axis.text.x = element_blank(),  # remove all x-axis text
    axis.ticks.x = element_blank(), # remove x-axis ticks
    legend.position = "bottom",
    plot.title = element_text(size = 18, face = "bold", hjust = 0.5),
    axis.title = element_text(size = 14, face = "bold"),
    axis.text = element_text(size = 12),
    strip.text = element_text(size = 14, face = "bold"),
    legend.title = element_text(size = 12, face = "bold"),
    legend.text = element_text(size = 10)
    # larger & centered# keep legend at bottom
  )










### JUST NAATBATT Midstream 
csv_list_manufac<- read.csv("/Users/elsawefes-potter/Documents/Critical_Minerals_Pablo/total_manufacturing_edited.csv") %>%
  rename("State/ Province" = State..Province)

cross_compare <- csv_list_manufac %>%
  semi_join(Naatbatt_Gwh, by = c("Company")) %>%
  mutate(Gwh.yr = as.numeric(Gwh.yr))

ontario_naat_batt <- read.csv("/Users/elsawefes-potter/Documents/Critical_Minerals_Pablo/Ontario_Naatbatt.csv") %>%
  rename("State/ Province" = State..Province, Info = X.1) 
cross_compare <- cross_compare[-c(31, 44, 41, 6, 5, 7, 26, 28, 50, 48, 47, 49), ] 
cross_compare <- cross_compare %>% bind_rows (ontario_naat_batt)

write.xlsx(cross_compare, "Naatbatt_Gwh_midstream.xlsx", rowNames = FALSE)

