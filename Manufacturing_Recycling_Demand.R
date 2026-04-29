install.packages("geofacet")
installed.packages()["geofacet", "Version"]
install.packages("devtools")
devtools::install_github("hafen/geofacet")
install.packages("ggpattern")
install.packages("openxlsx")
library(openxlsx)
library(ggpattern)
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

us_codes <- c(
  "AL","AK","AZ","AR","CA","CO","CT","DE","DC","FL","GA","HI","ID",
  "IL","IN","IA","KS","KY","LA","ME","MD","MA","MI","MN","MS",
  "MO","MT","NE","NV","NH","NJ","NM","NY","NC","ND","OH","OK",
  "OR","PA","RI","SC","SD","TN","TX","UT","VT","VA","WA","WV",
  "WI","WY"
)

ca_codes <- c(
  "AB","BC","MB","NB","NL","NS","ON","PE","QC","SK","NT","NU","YT"
)

region_mapping <- c(
  # US - WEST
  "WA" = "US-West", "OR" = "US-West", "CA" = "US-West", "NV" = "US-West", 
  "ID" = "US-West", "HI" = "US-West", "AK" = "US-West",
  
  # US - MOUNTAIN
  "MT" = "US-Mountain", "WY" = "US-Mountain", "UT" = "US-Mountain", 
  "CO" = "US-Mountain", "AZ" = "US-Mountain", "NM" = "US-Mountain",
  
  # US - MIDWEST
  "OH" = "US-Midwest", "IN" = "US-Midwest", "IL" = "US-Midwest", "MI" = "US-Midwest", 
  "WI" = "US-Midwest", "MN" = "US-Midwest", "IA" = "US-Midwest", "MO" = "US-Midwest", 
  "ND" = "US-Midwest", "SD" = "US-Midwest", "NE" = "US-Midwest", "KS" = "US-Midwest",
  
  # US - SOUTH
  "TX" = "US-South", "OK" = "US-South", "AR" = "US-South", "LA" = "US-South",
  "KY" = "US-South", "TN" = "US-South", "MS" = "US-South", "AL" = "US-South",
  
  # US - EAST
  "ME" = "US-East", "NH" = "US-East", "VT" = "US-East", "MA" = "US-East", 
  "RI" = "US-East", "CT" = "US-East", "NY" = "US-East", "NJ" = "US-East", 
  "PA" = "US-East", "DE" = "US-East", "MD" = "US-East", "DC" = "US-East", 
  "VA" = "US-East", "WV" = "US-East", "NC" = "US-East", "SC" = "US-East", 
  "GA" = "US-East", "FL" = "US-East",
  
  # CANADA - WEST
  "BC" = "Canada-West", "YT" = "Canada-West",
  
  # CANADA - MOUNTAIN
  "AB" = "Canada-Mountain",
  
  # CANADA - MIDWEST
  "MB" = "Canada-Midwest", "SK" = "Canada-Midwest",
  
  # CANADA - EAST
  "ON" = "Canada-East", "QC" = "Canada-East", "NB" = "Canada-East", "NS" = "Canada-East", 
  "PE" = "Canada-East", "NL" = "Canada-East", "NT" = "Canada-East", "NU" = "Canada-East",
  
  # MEXICO
  "MX" = "Mexico")

# Verify all states are included
all_states <- c(us_codes, ca_codes, "MX")
missing_states <- setdiff(all_states, names(region_mapping))
if(length(missing_states) > 0) {
  print(paste("Missing:", paste(missing_states, collapse = ", ")))
} else {
  print("All states, provinces, and Mexico mapped!")
}


state_map_rev <- setNames(names(state_map), state_map)

##Manufacturing
EVLIB_Flows_US <- read_csv("/Users/elsawefes-potter/Documents/Critical_Minerals_Pablo/Final_Data/US_EVLIB_Flows_detail_ACCII.csv") %>%
  rename(State_Province = State)
EV_Flows_US <- read_csv("/Users/elsawefes-potter/Documents/Critical_Minerals_Pablo/Final_Data/US_ClosedLoop_AddRetire_byStateSegment_ACCII.csv") %>%
  select(State, Segment, Year, add_BEV, add_PHEV) %>%
  group_by(State, Segment, Year) %>% summarise(add_BEV = sum(add_BEV, na.rm = TRUE), add_PHEV = sum(add_PHEV, na.rm = TRUE)) %>%
  rename(BEV = add_BEV, PHEV = add_PHEV,
         State_Province = State) %>%
  pivot_longer(cols = c(BEV, PHEV),
               names_to = "Propulsion",
               values_to = "Add_EV")


EVLIB_Flows_CA <- read_csv("/Users/elsawefes-potter/Documents/Critical_Minerals_Pablo/Final_Data/Canada_EVLIB_Flows_detail_ACCII.csv") %>%
  rename(State_Province = State)
EV_Flows_CA <- read_csv("/Users/elsawefes-potter/Documents/Critical_Minerals_Pablo/Final_Data/Canada_ClosedLoop_AddRetire_byStateSegment_ACCII.csv") %>%
  select(State, Segment, Year, add_BEV, add_PHEV) %>%
  group_by(State, Segment, Year) %>% summarise(add_BEV = sum(add_BEV, na.rm = TRUE), add_PHEV = sum(add_PHEV, na.rm = TRUE)) %>%
  rename(BEV = add_BEV, PHEV = add_PHEV,
         State_Province = State) %>%
  pivot_longer(cols = c(BEV, PHEV),
               names_to = "Propulsion",
               values_to = "Add_EV")

EVLIB_Flows_MX <- read_csv("/Users/elsawefes-potter/Documents/Critical_Minerals_Pablo/Final_Data/Mexico_EVLIB_Flows_detail_ACCII.csv") %>%
  rename(State_Province = State)
EV_Flows_MX <- read_csv("/Users/elsawefes-potter/Documents/Critical_Minerals_Pablo/Final_Data/Mexico_ClosedLoop_StateTotals_ACCII.csv") %>%
  select(State, Year, add_BEV, add_PHEV) %>%
  group_by(State, Year) %>% summarise(add_BEV = sum(add_BEV, na.rm = TRUE), add_PHEV = sum(add_PHEV, na.rm = TRUE)) %>%
  rename(BEV = add_BEV, PHEV = add_PHEV,
         State_Province = State) %>%
  pivot_longer(cols = c(BEV, PHEV),
               names_to = "Propulsion",
               values_to = "Add_EV")



BESSLIB_Flows_US <- read_csv("/Users/elsawefes-potter/Documents/Critical_Minerals_Pablo/Final_Data/US_BESS_Retire_Vector_byStateSegProp_ACCII.csv") %>%
  rename(LIB_recycling_vector = BESS_retire_vector) %>%
  rename(`State_Province` = `State`)

BESSLIB_Flows_CA <- read_csv("/Users/elsawefes-potter/Documents/Critical_Minerals_Pablo/Final_Data/Canada_BESS_Retire_Vector_byStateSegProp_ACCII.csv") %>%
  rename(LIB_recycling_vector = BESS_retire_vector) %>%
  rename(`State_Province` = `State`)

BESSLIB_Flows_MX <- read_csv("/Users/elsawefes-potter/Documents/Critical_Minerals_Pablo/Final_Data/Mexico_BESS_Retire_Vector_byStateSegProp_ACCII.csv") %>%
  rename(LIB_recycling_vector = BESS_retire_vector) %>%
  rename(`State_Province` = `State`)


HDV_LIBFlows <- read_csv("/Users/elsawefes-potter/Documents/Critical_Minerals_Pablo/Final_Data/HDV_EV_Turnover_ACCII.csv") %>%
  rename(`State_Province` = `State`) %>%
  mutate(Segment = Vehicle) 

HDV_BESSLIB_Flows <- read_csv("/Users/elsawefes-potter/Documents/Critical_Minerals_Pablo/Final_Data/HDV_BESS_Retire_ACCII.csv") %>%
  rename(LIB_recycling_vector = BESS_retire_vector) %>%
  rename(`State_Province` = `State`) %>%
  mutate(Segment = Vehicle)

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
  BESSLIB_Flows$LIB_recycling_vector,
  BESSLIB_Flows$Year
)

HDV_LIBFlows$LIB_recycling_vector <- Map(
  name_vector_with_years,
  HDV_LIBFlows$LIB_recycling_vector,
  HDV_LIBFlows$Year
) 

HDV_BESSLIB_Flows$LIB_recycling_vector <- Map(
  name_vector_with_years,
  HDV_BESSLIB_Flows$LIB_recycling_vector,
  HDV_BESSLIB_Flows$Year
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
  unnest(cols = recycle_df) %>%
  filter(Sale_Year >= 2025)%>%
  filter(Year >= 2025)

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
  unnest(cols = recycle_df) %>%
  filter(Sale_Year >= 2025)%>%
  filter(Year >= 2025)

HDV_future_recycle_type <- HDV_LIBFlows %>%
  mutate(
    recycle_df = map(LIB_recycling_vector, ~ {
      tibble(
        Sale_Year = as.integer(names(.x)),
        LIB_recycle_total = as.numeric(.x)
      )
    })
  ) %>%
  select(State_Province, Segment, Year, recycle_df) %>% # keep original Year here
  unnest(cols = recycle_df) %>%
  filter(Sale_Year >= 2025)%>%
  filter(Year >= 2025)%>%
  group_by(State_Province, Year, Sale_Year) %>%
  summarise(LIB_recycle_total = sum(LIB_recycle_total)) %>%
  mutate(Propulsion = "HDV") %>%
  mutate(Segment = "HDV")


HDV_BESS_future_recycle_type <- HDV_BESSLIB_Flows %>%
  mutate(
    recycle_df = map(LIB_recycling_vector, ~ {
      tibble(
        Sale_Year = as.integer(names(.x)),
        LIB_recycle_total = as.numeric(.x)
      )
    })
  ) %>%
  select(State_Province, Segment, Year, recycle_df) %>%  # keep original Year here
  unnest(cols = recycle_df) %>%
  filter(Sale_Year >= 2025)%>%
  filter(Year >= 2025)%>%
  group_by(State_Province, Year, Sale_Year) %>%
  summarise(LIB_recycle_total = sum(LIB_recycle_total)) %>%
  mutate(Propulsion = "HDV") %>%
  mutate(Segment = "HDV")


future_recycle_type <- full_join(future_recycle_type, BESS_future_recycle_type, by = c("State_Province","Segment","Propulsion","Year","Sale_Year")) %>%
  mutate(across(everything(), ~ replace_na(.x, 0))) %>%
  mutate(LIB_recycle_total = LIB_recycle_total.x+LIB_recycle_total.y) %>%
  select(-c(LIB_recycle_total.x, LIB_recycle_total.y))

future_recycle_HDV <- full_join(HDV_future_recycle_type, HDV_BESS_future_recycle_type, by = c("State_Province","Segment","Propulsion","Year","Sale_Year")) %>%
  mutate(across(everything(), ~ replace_na(.x, 0))) %>%
  mutate(LIB_recycle_total = LIB_recycle_total.x+LIB_recycle_total.y) %>%
  select(-c(LIB_recycle_total.x, LIB_recycle_total.y))

future_recycle_type <- bind_rows(future_recycle_type, future_recycle_HDV)



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
        Product_category == "Output" & Year_online > 2025 ~ Year_online + 5,
      TRUE ~ Year_online,
      Feedstock == "Black Mass" &
        Product_category == "Output" & Year_online > 2025 ~ Year_online + 5,
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
  filter(Year <= 2035) %>%
  ungroup() %>%
  group_by(State_Province) %>%
  # Ensure all years 2025–2050 exist for each state
  complete(Year = 2025:2050) %>%
  # Fill missing values downward (last observation carried forward)
  fill(Cumulative_black_mass_cap, Cumulative_refining_cap, Delay_Cumulative_black_mass_cap, Delay_Cumulative_refining_cap, .direction = "down") %>%
  ungroup() 


recycling_tonnes_2050_projected <- recycling_tonnes_by_state %>% filter(Year == 2050) %>% 
  select(-Delay_Cumulative_black_mass_cap, -Delay_Cumulative_refining_cap) 

recycling_tonnes_2050_delayed <- recycling_tonnes_by_state %>% filter(Year == 2050) %>% 
  select(-Cumulative_black_mass_cap, -Cumulative_refining_cap) 


recycling_tonnes_total <- recycling_tonnes_by_state %>% group_by(Year) %>%
  summarise(Cumulative_black_mass_cap = sum(Cumulative_black_mass_cap, na.rm = TRUE),
            Cumulative_refining_cap = sum(Cumulative_refining_cap, na.rm = TRUE),
            Delay_Cumulative_black_mass_cap = sum(Delay_Cumulative_black_mass_cap, na.rm = TRUE),
            Delay_Cumulative_refining_cap = sum(Delay_Cumulative_refining_cap, na.rm = TRUE)) %>%
  mutate(Full_Recycle = case_when(Cumulative_refining_cap > Cumulative_black_mass_cap ~ Cumulative_black_mass_cap, TRUE ~ Cumulative_refining_cap)) %>%
  mutate(Delay_Full_Recycle = case_when(Delay_Cumulative_refining_cap > Delay_Cumulative_black_mass_cap ~ Delay_Cumulative_black_mass_cap, TRUE ~ Delay_Cumulative_refining_cap))

NA_recycling_tonnes <- recycling_tonnes_total %>%
  select(-Delay_Cumulative_black_mass_cap, -Delay_Cumulative_refining_cap, -Full_Recycle, -Delay_Full_Recycle) 

NA_recycling_tonnes_delayed <- recycling_tonnes_total %>%
  select(Year, Delay_Cumulative_black_mass_cap, Delay_Cumulative_refining_cap, -Full_Recycle, -Delay_Full_Recycle) 
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
all_manufacturing <- read_xlsx("/Users/elsawefes-potter/Documents/Critical_Minerals_Pablo/Manu_Down_Mid.xlsx", sheet = "changed dates Narrowed Manu fac") %>%
  select(`Year online`, `Production Capacity`, Company, `Facility State or Province`, `Supply Chain Segment`, Chemistry) %>%
  mutate(Gwh_yr = as.numeric(`Production Capacity`)) %>%
  group_by(`Supply Chain Segment`, `Facility State or Province`, `Year online`) %>%
  summarise(Gwh_yr = sum(Gwh_yr, na.rm = TRUE), .groups = "drop") %>%
  rename(Year_Online = `Year online`, State_Province = `Facility State or Province`) %>%   
  filter(!is.na(Year_Online), Gwh_yr != 0) %>%
  mutate(Year_Online = as.numeric(Year_Online)) %>%
  pivot_wider(
    names_from = `Supply Chain Segment`,
    values_from = Gwh_yr,
    values_fill = list(Gwh_yr = 0)
  ) %>%
  mutate(Downstream = Downstream *0.77, ## assuming at 77% capacity (find reference)
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

HDV_add <- HDV_LIBFlows %>%group_by(State_Province, Year) %>%
  summarise(Total_Add_LIB = sum(New_Sales)) %>%
  mutate(Propulsion = "HDV",
         Segment = "HDV")

state_capacity_added <- state_capacity_added %>%
  bind_rows(HDV_add)


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

chem_proj <- future_match_HDV %>% rename (Year = Sale_Year) %>% rename(Mix_proj = `Cathode Mix Share`)
chem_LFP <- final_adjusted_mix_extended %>% rename(Year = Sale_Year) %>% rename(Mix_LFP = `Cathode Mix Share`)
chems_proj_LFP <- chem_proj %>% left_join(chem_LFP, by=c("Year", "Cathode Mix", "Segment","Propulsion"), relationship = "many-to-many")



##DEMAND IN TONNES - cap proj by state (add into state libs to get gwh)
state_cap_add <- state_capacity_added %>%
  left_join(caps_projected,     by = c("Year", "Segment", "Propulsion")) %>%
  left_join(caps_15_projected,  by = c("Year", "Segment", "Propulsion")) %>%
  mutate(Add_LIB_Gwh_proj = Avg_Cap_Proj * Total_Add_LIB/1e6) %>%
  mutate(Add_LIB_Gwh_15 = Avg_Cap_15 *Total_Add_LIB/1e6) %>% 
  filter(Year >= 2025) %>%
  select(State_Province, Year, Propulsion, Segment, Add_LIB_Gwh_15, Add_LIB_Gwh_proj) %>%
  group_by(State_Province, Year, Propulsion, Segment) %>%
  summarise(
    Add_LIB_Gwh_proj = sum(Add_LIB_Gwh_proj, na.rm = TRUE),
    Add_LIB_Gwh_15   = sum(Add_LIB_Gwh_15,   na.rm = TRUE)
  )%>%
  mutate(State_Province = case_when(
    State_Province %in% names(state_map_rev) ~ state_map_rev[State_Province],
    TRUE ~ State_Province   # leave unchanged if no match
  )) 


### DEMAND IN TONNES with projections of cap and chem
state_cap_chem_tonne <- state_cap_add %>% 
  left_join(chems_proj_LFP, by= c("Year", "Propulsion", "Segment"), relationship = "many-to-many") %>%
  mutate(
    Add_LIB_Gwh_proj_chem = Add_LIB_Gwh_proj * Mix_proj,
    Add_LIB_Gwh_15_chem   = Add_LIB_Gwh_15 * Mix_proj,
    Add_LIB_Gwh_proj_LFP = Add_LIB_Gwh_proj * Mix_LFP,
    Add_LIB_Gwh_15_LFP = Add_LIB_Gwh_15* Mix_LFP
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
  select(Year, State_Province, Add_LIB_proj_tonnes, Add_LIB_15_tonnes, Add_LIB_proj_LFP_tonnes, Add_LIB_15_LFP_tonnes)  %>%
  mutate(
    Country = case_when(
      State_Province %in% us_codes ~ "US",
      State_Province %in% ca_codes ~ "CA",
      State_Province == "MX" ~ "MX",
      TRUE ~ NA_character_
    )
  )

state_demand_tonnes_2050 <- state_cap_chem_tonne %>% filter(Year == 2050) 

nat_cap_add <- state_cap_add %>%group_by(Year) %>% 
  summarise(Add_LIB_Gwh_proj = sum(Add_LIB_Gwh_proj, na.rm = TRUE), 
            Add_LIB_Gwh_15 = sum(Add_LIB_Gwh_15, na.rm = TRUE))

NA_demand_tonnes <- state_cap_chem_tonne %>% group_by(Year) %>%
  summarise(Add_LIB_proj_tonnes = sum(Add_LIB_proj_tonnes, na.rm = TRUE),
            Add_LIB_15_tonnes = sum(Add_LIB_15_tonnes, na.rm = TRUE),
            Add_LIB_proj_LFP_tonnes = sum(Add_LIB_proj_LFP_tonnes, na.rm = TRUE),
            Add_LIB_15_LFP_tonnes = sum(Add_LIB_15_LFP_tonnes, na.rm = TRUE)) 
  
            


###USE National in Gwh to get manufacturing projection
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

all_manufacturing_expanded_complete_yrs <- all_manufacturing_expanded %>% filter(Year <= 2035) %>%
  group_by(State_Province) %>%
  # Ensure all years 2025–2050 exist for each state
  complete(Year = 2025:2050) %>%
  # Fill missing values downward (last observation carried forward)
  fill(Share_of_Year_Prod_Down, Share_of_Year_Prod_Mid,Share_of_Year_Scrap_Down, Share_of_Year_Scrap_Mid, .direction = "down") %>%
  ungroup() %>% select(Year, State_Province, Share_of_Year_Prod_Down, Share_of_Year_Prod_Mid, Share_of_Year_Scrap_Down, Share_of_Year_Scrap_Mid)

delayed_all_manufacturing_expanded_complete_yrs <- delayed_manufacturing_expanded %>% filter(Year <= 2040) %>%
  group_by(State_Province) %>%
  # Ensure all years 2025–2050 exist for each state
  complete(Year = 2025:2050) %>%
  # Fill missing values downward (last observation carried forward)
  fill(Share_of_Year_Prod_Down, Share_of_Year_Prod_Mid,Share_of_Year_Scrap_Down, Share_of_Year_Scrap_Mid, .direction = "down") %>%
  ungroup() %>% select(Year, State_Province, Share_of_Year_Prod_Down, Share_of_Year_Prod_Mid, Share_of_Year_Scrap_Down, Share_of_Year_Scrap_Mid)

manufacturing_by_state_projected <- all_manufacturing_expanded_complete_yrs  %>%               
  mutate(State_Province = if_else(State_Province == "SLP", "MX", State_Province)) %>%
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
  ) %>%
  ungroup() 

manufacturing_by_state_delayed <- delayed_all_manufacturing_expanded_complete_yrs  %>%                # only years <= 2035
  mutate(State_Province = if_else(State_Province == "SLP", "MX", State_Province)) %>%
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
  ) %>%
  ungroup() 

NA_manu <- manufacturing_by_state_projected %>% 
  group_by(Year) %>%
  summarise(Tonnes_Scrap_proj_down = sum(Tonnes_Scrap_proj_down), 
            Tonnes_Scrap_15_down = sum(Tonnes_Scrap_15_down),
            Tonnes_Prod_proj_down = sum(Tonnes_Prod_proj_down), 
            Tonnes_Prod_15_down = sum(Tonnes_Prod_15_down),
            Tonnes_Scrap_proj_mid = sum(Tonnes_Scrap_proj_mid),
            Tonnes_Scrap_15_mid = sum(Tonnes_Scrap_15_mid),
            Tonnes_Prod_proj_mid = sum(Tonnes_Prod_proj_mid),
            Tonnes_Prod_15_mid = sum(Tonnes_Prod_15_mid))

NA_manu_delayed <- manufacturing_by_state_delayed %>% 
  group_by(Year) %>%
  summarise(Tonnes_Scrap_proj_down = sum(Tonnes_Scrap_proj_down), 
            Tonnes_Scrap_15_down = sum(Tonnes_Scrap_15_down),
            Tonnes_Prod_proj_down = sum(Tonnes_Prod_proj_down), 
            Tonnes_Prod_15_down = sum(Tonnes_Prod_15_down),
            Tonnes_Scrap_proj_mid = sum(Tonnes_Scrap_proj_mid),
            Tonnes_Scrap_15_mid = sum(Tonnes_Scrap_15_mid),
            Tonnes_Prod_proj_mid = sum(Tonnes_Prod_proj_mid),
            Tonnes_Prod_15_mid = sum(Tonnes_Prod_15_mid))
  
## all in projected scenario rn
manufacturing_tonnes_2050_projected = manufacturing_by_state_projected %>% filter(Year == 2050) 
manufacturing_tonnes_2050_delayed = manufacturing_by_state_delayed %>% filter(Year == 2050) 

## PREPPING TO PLOT-- put in delayed scenario
Mass_all_years <- full_join(state_cap_chem_tonne, 
                            manufacturing_by_state_projected, 
                            manufacturing_by_state_delayed,
                            by = c("Year","State_Province")) %>%
  full_join(recycling_tonnes_by_state,
            by = c("Year","State_Province")) %>%
  mutate(across(where(is.numeric), ~replace_na(.x, 0)))

