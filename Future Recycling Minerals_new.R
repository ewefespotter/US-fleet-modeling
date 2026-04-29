install.packages("writexl")
install.packages("colorspace")
install.packages("patchwork")
library(dplyr)
library(purrr)
library(readxl)
library(dplyr)
library(tidyr)
library(stringr)
library(tidyverse)
library(writexl)
library(colorspace)
library(ggpattern)
library(patchwork)

### Run Order:      ##EV_Volumes_Clean
                    ##HMDV
                    ##Historical Sales Minerals
                    ##Scenarios SetUp

                    ##Manufacturing_Recycling_Demand
                    ##Future Recycling Minerals
                    ##Plotting_Demand_Recycle_Manu
                    ##Future Demand Minerals


                    ##Change to Delay at appropriate locations in following and run again
                          ## Manufacturing_Recycling_Demand
                          ## Future Recycling Minerals
                          ## Future Demand Minerals
                    ##Run all everything starting at Manufacturing_Recycling_Demand again w Repeal



## DATA INPUTS
### Start up phase of 4 years starts at about 20 on average and decreases to 4-12
mineral_intensity <- read_excel(file.path(data_folder, "Mineral_Intensity(2).xlsx"), na = "") %>%
  rename(`Cathode Mix` = chemistry)

batpac_scrap_min <- read_csv("/Users/elsawefes-potter/Documents/Critical_Minerals_Pablo/Mins_in_scrap (-Energy BatPac).csv") %>% 
  select(where(~ !all(is.na(.)))) 

scrap_mass <- read_csv("/Users/elsawefes-potter/Documents/Critical_Minerals_Pablo/Scrap_mass(-Energy BatPac).csv") %>% 
  select(where(~ !all(is.na(.)))) %>%
  rename(`Cathode Mix` = `Battery Chem`) %>% select(`Cathode Mix`, `Scrap kg/Gwh`)

colnames(batpac_scrap_min) <- c("Product_Abbrev", "Mineral", "Value")
batpac_scrap_min <- batpac_scrap_min %>%
  mutate(`kg/Gwh` = Value/50) 
#(kg/yr) *1/(50000000 kwh/yr) * 1000000 kwh/Gwh


## Minerals
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

nmca_rows <- batpac_scrap_min %>%
  filter(Product_Abbrev == "NMC 622") %>%       
  left_join(ratio_nmca_nmc, by = "Mineral")%>%
  mutate(
    Product_Abbrev = "NMCA",
    `kg/Gwh` = `kg/Gwh` * ratio
  ) %>%
  select(-ratio)

nmca_rows <- nmca_rows %>%
  select(-matches("^NMC 622$|^NMCA 89:4:4:3$"))

# Combine original bat_pac with NMCA rows
batpac_scrap_min_w_nmca <- bind_rows(batpac_scrap_min, nmca_rows)

mins_in_scrap <- batpac_scrap_min_w_nmca %>% 
  rename(`Cathode Mix` = Product_Abbrev) %>% 
  select(-Value) %>% filter(!is.na(`Cathode Mix`)) %>%
  full_join(scrap_mass, by = "Cathode Mix") %>% 
  mutate(`kg/Gwh` = as.numeric(`kg/Gwh`),
         `Scrap kg/Gwh` = as.numeric(`Scrap kg/Gwh`),
    Min_kg_Scrap_tonne = `kg/Gwh`/(`Scrap kg/Gwh`*1000))

all_mins <- mineral_intensity %>% full_join(mins_in_scrap, by = c("Cathode Mix", "Mineral")) %>%
  mutate(
    kg_per_kwh = ifelse(is.na(kg_per_kwh), 0, kg_per_kwh),
    Min_kg_Scrap_tonne  = ifelse(is.na(Min_kg_Scrap_tonne), 0, Min_kg_Scrap_tonne)
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
  

##Taken from Manufacturing_Recycling_Demand (Tonnes of scrap going to BM and Refining)
clean_manu_projected_tonnes <- manufacturing_by_state_projected %>%
  mutate(Scrap_proj_tonnes =  Tonnes_Scrap_proj_mid,
         Scrap_15_tonnes =  Tonnes_Scrap_15_mid) %>%
  select(Year, State_Province, Scrap_proj_tonnes, Scrap_15_tonnes) 

scrap_proj_tonnes <- clean_manu_projected_tonnes %>% select(Year, State_Province, Scrap_proj_tonnes) %>% 
  rename(Scrap_tonnes = Scrap_proj_tonnes) %>% mutate(Sale_Year = Year)
scrap_15_tonnes <- clean_manu_projected_tonnes %>% select(Year, State_Province, Scrap_15_tonnes) %>% 
  rename(Scrap_tonnes = Scrap_15_tonnes) %>% mutate(Sale_Year = Year)

clean_manu_delayed_tonnes <- manufacturing_by_state_delayed %>%
  mutate(Scrap_proj_tonnes =  Tonnes_Scrap_proj_mid,
         Scrap_15_tonnes =  Tonnes_Scrap_15_mid) %>%
  select(Year, State_Province, Scrap_proj_tonnes, Scrap_15_tonnes) 

delay_scrap_proj_tonnes <- clean_manu_delayed_tonnes %>% 
  select(Year, State_Province, Scrap_proj_tonnes) %>% 
  rename(Scrap_tonnes = Scrap_proj_tonnes) %>% mutate(Sale_Year = Year)

delay_scrap_15_tonnes <- clean_manu_delayed_tonnes %>% 
  select(Year, State_Province, Scrap_15_tonnes) %>% 
  rename(Scrap_tonnes = Scrap_15_tonnes) %>% 
  mutate(Sale_Year = Year)


### INTRODUCE THE SCRAP and DELAY into cap/chem scenarios
batt_cap_project <- batt_cap_projection %>%
  left_join(scrap_proj_tonnes,by = "Sale_Year") %>%
  select(
    Sale_Year,
    State_Province,
    Segment,
    Propulsion,
    `Projected Avg Batt Cap (kwh/batt)`,
    Scrap_tonnes,
  ) 

names(batt_cap_15) <- trimws(names(batt_cap_15))

batt_cap_15_new <- batt_cap_15 %>% 
  left_join(scrap_15_tonnes, by = "Sale_Year") %>% 
  select(
    Sale_Year, 
    State_Province, 
    Segment, 
    Propulsion, 
    `Projected Avg Batt Cap (kwh/batt)`, 
    Scrap_tonnes, 
  )

all_states <- tibble(
  State_Province = unique(c(unname(state_map_rev), "MX"))
)

combo_cathodes <- batt_cap_project %>%
  distinct(
    Sale_Year,
    Segment,
    Propulsion,
  )

expanded_grid <- combo_cathodes %>%
  tidyr::crossing(all_states)

# getting all states in there
batt_cap_proj_ext <- expanded_grid %>%
  left_join(
    batt_cap_project,
    by = c("Sale_Year", "Segment", "Propulsion", "State_Province")
  ) 

batt_cap_15_ext <- expanded_grid %>%
  left_join(
    batt_cap_15_new, 
    by = c("Sale_Year", "Segment", "Propulsion", "State_Province")
  )

combo_defaults_proj <- batt_cap_project %>%
  group_by(
    Sale_Year,
    Segment,
    Propulsion,
  ) %>%
  summarise(
    Scrap_tonnes = first(Scrap_tonnes),
    `Projected Avg Batt Cap (kwh/batt)` =
      first(`Projected Avg Batt Cap (kwh/batt)`),
    .groups = "drop"
  )

combo_defaults_15 <- batt_cap_15_new %>%
  group_by(
    Sale_Year,
    Segment,
    Propulsion,
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
    by = c("Sale_Year", "Segment", "Propulsion")
  ) %>%
  mutate(
    Scrap_tonnes =
      coalesce(Scrap_tonnes.x, Scrap_tonnes.y),
    
    `Projected Avg Batt Cap (kwh/batt)` =
      coalesce(`Projected Avg Batt Cap (kwh/batt).x`,
               `Projected Avg Batt Cap (kwh/batt).y`)  ) %>%
  select(
    Sale_Year,
    Segment,
    Propulsion,
    State_Province,
    Scrap_tonnes,
    `Projected Avg Batt Cap (kwh/batt)`
  )  %>% mutate(Year = Sale_Year)


batt_cap_15_ext <- batt_cap_15_ext %>%
  left_join(
    combo_defaults_15,
    by = c("Sale_Year", "Segment", "Propulsion")
  ) %>%
  mutate(
    Scrap_tonnes =
      coalesce(Scrap_tonnes.x, Scrap_tonnes.y),
    
    `Projected Avg Batt Cap (kwh/batt)` =
      coalesce(`Projected Avg Batt Cap (kwh/batt).x`,
               `Projected Avg Batt Cap (kwh/batt).y`)  ) %>%
  select(
    Sale_Year,
    Segment,
    Propulsion,
    State_Province,
    Scrap_tonnes,
    `Projected Avg Batt Cap (kwh/batt)`
  ) %>% mutate(Year = Sale_Year)

batt_scen <- list(batt_cap_proj_ext, batt_cap_15_ext) 
chem_scens <- list(future_match_HDV, final_adjusted_mix_extended)


future_recycle_type_collection <- future_recycle_type %>%  mutate(State_Province = case_when(
  State_Province %in% names(state_map_rev) ~ state_map_rev[State_Province],
  TRUE ~ State_Province))  %>% filter(Sale_Year >= 2025) %>%
  mutate(Sale_Year = as.integer(Sale_Year))

### Assumes manufacturing scrap is recycled anywhere and batteries are recycled anywhere
### this doesn't track where the recycling or manufacturing is happening
## Assume 100% collection

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
  future_recycle_cap <- future_recycle_cap %>% group_by(Year, Sale_Year, State_Province, Propulsion, Segment) %>%
    summarise(LIB_recycle_kwh = sum(LIB_recycle_kwh))
  
  
  future_recycle_cap <- future_recycle_cap %>%
    arrange(State_Province, Year)
  
  nat_recycle_cap <- future_recycle_cap %>% group_by(Year) %>%
    summarise(LIB_recycle_Gwh = sum(LIB_recycle_kwh)/1e6)

  
  ### APPLY BENCHMARK
  future_recycle_chem_fut <- future_recycle_cap %>%
    left_join(chem_df, by = c("Sale_Year", "Propulsion","Segment"), relationship = "many-to-many") %>%
    mutate(Cathode_kwh_state = LIB_recycle_kwh * `Cathode Mix Share`) %>%
    group_by(Sale_Year, State_Province, Year, `Cathode Mix`) %>%
    summarise(Cathode_kwh_state = sum(Cathode_kwh_state), .groups = "drop") 
  

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

  ## assuming no improvements in energy density (same every year)
  future_mass_recycle_chem <- future_recycle_chem %>% inner_join(specific_energy, by = ("Cathode Mix"), relationship = "many-to-many") %>% 
    mutate(Batt_Mass_MT = Cathode_kwh_state * Pack_kg_kwh/1000) ##check all chems have smth
  
  ## national scale
  future_mass_recycle_total <- future_mass_recycle_chem %>%
    group_by(Year) %>%
    summarise(Batt_Mass_MT = sum(Batt_Mass_MT, na.rm = TRUE))
  
  state_mass_recycle_batt <- future_mass_recycle_chem %>%
    group_by(Year, State_Province) %>%
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
  batt_df_nat_scrap <- batt_cap_df %>% group_by(Year, State_Province) %>% 
    summarise(Scrap_tonnes = first(Scrap_tonnes), .groups = "drop") %>%
    group_by(Year) %>%
    summarise(Scrap_tonnes = sum(Scrap_tonnes),.groups = "drop")

  ### the black mass facility takes off the pack materials and can handle 70% of caacity in cell materials
  ## Here introduce the pack equivalents for scrap (cell processing capacity smaller)
  Available_Recycling_Capacity <- US_CA_Recycle %>% 
    inner_join(batt_df_nat_scrap, by = "Year") %>% 
    mutate(
      Leftover_blackmass_cap = pmax(Black_Mass_MT - Scrap_tonnes/0.7078558, 0), ## 0.7078558 is ratio of cell to pack since it would only be cells going into BM facility but needs to be full batt equivalents
      #Leftover_refining_cap  = pmax(Refining_MT - Scrap_proj_tonnes, 0),
      Leftover_Full_Recycle = pmax(Full_Recycle - Scrap_tonnes/0.7078558, 0), ## This has a variable constraint black mass when this is lowest and refining when that is lowest
      
      Scrap_full_recycle_percent = pmin(Full_Recycle/Scrap_tonnes/0.7078558,1),
      Unprocessed_Scrap = pmax(Scrap_tonnes/0.7078558 - Black_Mass_MT,0),
      Unrefined_Scrap = pmax(Scrap_tonnes/0.7078558 - Full_Recycle,0), ## all unrefined also from not processed
      
      Unprocessed_Scrap_percent = 1- pmin(Black_Mass_MT/Scrap_tonnes/0.7078558, 1),
      Exported_BM_Scrap_percent = pmax((Scrap_tonnes/0.7078558 - Unprocessed_Scrap - Full_Recycle),0)/(Scrap_tonnes/0.7078558),
      
      
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
      Unused_Refining = pmax(Refining_MT - pmin((Batt_Mass_MT + Scrap_tonnes/0.7078558)/Full_Recycle,1)*Full_Recycle,0),
      
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
      Scrap_tonnes,
      
      
      Unprocessed_Batts_percent,
      Unprocessed_Scrap_percent,
      
      Exported_BM_Batts_percent,
      Exported_BM_Scrap_percent,
      
      Needed_Black_Mass_level,
      Needed_Refining_level,
      
      
    ) %>% mutate(across(where(is.numeric), ~ ifelse(abs(.) < 1e-12, 0, .)))
  
  View(Available_Recycling_Capacity)
  #batt_cap_state_scrap <- batt_cap_df %>% group_by(Year, `Cathode Mix`, State_Province) %>%
    #summarise(Prod_Gwh_state = first(Prod_Gwh_state))
  
  
  #### National levels become state levels
  future_recycle_chem <- future_recycle_chem %>% select(-LIB_recycle_kwh) %>%
    inner_join(Available_Recycling_Capacity, by = "Year") %>% 
    mutate(Recycled_kwh_Batts = Cathode_kwh_state * Post_consumer_full_recycle_percent,
           Unprocessed_kwh_Batts = Cathode_kwh_state * Unprocessed_Batts_percent,
           Exported_BM_kwh_Batts = Cathode_kwh_state * Exported_BM_Batts_percent) %>% 
    #left_join(batt_cap_state_scrap, by = c("Year", "Cathode Mix", "State_Province")) %>% 
    mutate( ## this is a proxy variable bc have to apply scrap percentages to full production to get scrap minerals later
      #Recycled_Gwh_Prod = Prod_Gwh_state * Scrap_full_recycle_percent,
      Recycled_Scrap = Scrap_tonnes * Scrap_full_recycle_percent,
      #Unprocessed_Gwh_Prod = Prod_Gwh_state * Unprocessed_Scrap_percent,
      Unprocessed_Scrap = Scrap_tonnes * Unprocessed_Scrap_percent,
      #Exported_BM_Gwh_Prod = Prod_Gwh_state * Exported_BM_Scrap_percent,
      Exported_BM_Scrap = Scrap_tonnes * Exported_BM_Scrap_percent) %>%
    group_by(Year, State_Province, `Cathode Mix`) %>%
    summarise(
      Recycled_kwh_Batts    = sum(Recycled_kwh_Batts, na.rm = TRUE),
      Unprocessed_kwh_Batts = sum(Unprocessed_kwh_Batts, na.rm = TRUE),
      Exported_BM_kwh_Batts = sum(Exported_BM_kwh_Batts, na.rm = TRUE),
      
      #Recycled_Gwh_Prod    = sum(Recycled_Gwh_Prod, na.rm = TRUE),
      Recycled_Scrap = sum(Recycled_Scrap, na.rm = TRUE),
      #Unprocessed_Gwh_Prod = sum(Unprocessed_Gwh_Prod, na.rm = TRUE),
      Unprocessed_Scrap = sum(Unprocessed_Scrap, na.rm = TRUE),
      #Exported_BM_Gwh_Prod = sum(Exported_BM_Gwh_Prod, na.rm = TRUE),
      Exported_BM_Scrap = sum(Exported_BM_Scrap, na.rm = TRUE),
      
      Cathode_kwh_state = sum(Cathode_kwh_state, na.rm = TRUE),
      Scrap_tonnes = sum(Scrap_tonnes, na.rm = TRUE),
      #Prod_Gwh_state        = sum(Prod_Gwh_state, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    select(Year, State_Province, `Cathode Mix`, 
           Recycled_kwh_Batts, Unprocessed_kwh_Batts, Exported_BM_kwh_Batts, 
           
           #Recycled_Gwh_Prod, Unprocessed_Gwh_Prod, Exported_BM_Gwh_Prod, Prod_Gwh_state,
           Cathode_kwh_state,
           Scrap_tonnes, Recycled_Scrap, Unprocessed_Scrap, Exported_BM_Scrap) %>%
    mutate(across(where(is.numeric), ~ replace_na(.x, 0)))
  

  #Nat_chem <- future_recycle_chem %>% group_by(Year) %>% 
    #summarise(Recycled_kwh_Batts = sum(Recycled_kwh_Batts, na.rm = TRUE),
              #Recycled_Gwh_Prod = sum(Recycled_Gwh_Prod, na.rm = TRUE),
              #Prod_Gwh_state        = sum(Prod_Gwh_state, na.rm = TRUE),
              #Cathode_kwh_state = sum(Cathode_kwh_state, na.rm = TRUE)) %>%
    #mutate(total = Recycled_Gwh_Prod + Recycled_kwh_Batts/1e6)
  
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
        kg_per_kwh * Recycled_kwh_Batts + Min_kg_Scrap_tonne * Recycled_Scrap,
      
      `Available Recycled Minerals No R Restraint (kg)` = 
        kg_per_kwh * Cathode_kwh_state + Min_kg_Scrap_tonne * Scrap_tonnes,
      
      `Minerals in Exported Scrap/Batts (kg)` = kg_per_kwh * Unprocessed_kwh_Batts + Min_kg_Scrap_tonne * Unprocessed_Scrap,
      
      `Minerals in Exported BM (kg)` = kg_per_kwh * Exported_BM_kwh_Batts + Min_kg_Scrap_tonne * Exported_BM_Scrap,
      
      Scrap_min = Min_kg_Scrap_tonne * Recycled_Scrap,
      
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
  View(future_minerals)
  
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
    ) %>% mutate( ## if were recycling everything
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
      capacity_needs  = capacity_needs,
      state_mass_recycle_batt = state_mass_recycle_batt
    )
  )
}




# Set names for scenarios
names(batt_scen) <- c("Baseline Capacity", "15% Lower Capacity")
names(chem_scens) <- c("Benchmark Chemistry", "High LFP Chemistry")

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
        ),
      
      state_mass_recycle_batt = res$state_mass_recycle_batt %>%
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

state_mass_recycle_batt <- all_scenarios %>%
  pull(result) %>%
  compact() %>%
  map("state_mass_recycle_batt") %>%
  bind_rows()

# Combine all results

cap_chem_results <- cap_chem_results %>%
  mutate(Scenario = paste(Battery_Scenario, Chemistry_Scenario, sep = " - "))

state_mass_recycle_batt <- state_mass_recycle_batt %>%
  mutate(Scenario = paste(Battery_Scenario, Chemistry_Scenario, sep = " - ")) %>%
  select(-c(Battery_Scenario, Chemistry_Scenario)) %>%
  mutate(
    Scenario = dplyr::recode(
      Scenario,
      "Baseline Capacity - Benchmark Chemistry" = "Increasing Batt Cap - Benchmark Chemistry",
      "Baseline Capacity - High LFP Chemistry" = "Increasing Batt Cap - High LFP Chemistry",
      "15% Lower Capacity - Benchmark Chemistry" = "Decreasing Batt Cap - Benchmark Chemistry",
      "15% Lower Capacity - High LFP Chemistry" = "Decreasing Batt Cap - High LFP Chemistry"
    )
  )

cap_chem_results <- cap_chem_results %>%
  mutate(
    Scenario = dplyr::recode(
      Scenario,
      "Baseline Capacity - Benchmark Chemistry" = "Increasing Batt Cap - Benchmark Chemistry",
      "Baseline Capacity - High LFP Chemistry" = "Increasing Batt Cap - High LFP Chemistry",
      "15% Lower Capacity - Benchmark Chemistry" = "Decreasing Batt Cap - Benchmark Chemistry",
      "15% Lower Capacity - High LFP Chemistry" = "Decreasing Batt Cap - High LFP Chemistry"
    )
  )
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
needed_cap_results <- needed_cap_results %>%
  mutate(
    Scenario = dplyr::recode(
      Scenario,
      "Baseline Capacity - Benchmark Chemistry" = "Increasing Batt Cap - Benchmark Chemistry",
      "Baseline Capacity - High LFP Chemistry" = "Increasing Batt Cap - High LFP Chemistry",
      "15% Lower Capacity - Benchmark Chemistry" = "Decreasing Batt Cap - Benchmark Chemistry",
      "15% Lower Capacity - High LFP Chemistry" = "Decreasing Batt Cap - High LFP Chemistry"
    )
  )
## CHECKS
# fin_min_nat <- cap_chem_results %>% group_by(Year, Mineral, Scenario) %>% 
#   summarise(Scrap_min = sum(Scrap_min, na.rm = TRUE),
#             Batt_min = sum(Batt_min, na.rm = TRUE),
#             `Available Recycled Minerals (w Scrap) (Tonne)` = sum(`Available Recycled Minerals (w Scrap) (Tonne)`, na.rm = TRUE),
#             `Available Recycled Minerals No R Restraint (Tonne)` = sum(`Available Recycled Minerals No R Restraint (Tonne)`, na.rm = TRUE)) %>% 
#   filter(Mineral == "Nickel")

legend_order <- c(
  "Increasing Batt Cap - Benchmark Chemistry",
  "Increasing Batt Cap - High LFP Chemistry",
  "Decreasing Batt Cap - Benchmark Chemistry",
  "Decreasing Batt Cap - High LFP Chemistry"
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

cap_chem_results <- cap_chem_results %>%
  mutate(
    Country = case_when(
      State_Province %in% us_codes ~ "US",
      State_Province %in% ca_codes ~ "CA",
      State_Province == "MX" ~ "MX",
      TRUE ~ NA_character_
    )
  ) 


NA_cap_chem_rec <- cap_chem_results %>% group_by(Year, Scenario, Mineral) %>%
  summarise(`Current NA Recycling Capacity` =  sum(`Available Recycled Minerals (w Scrap) (Tonne)`, na.rm = TRUE),
            `All Material is Recycled in NA` = sum(`Available Recycled Minerals No R Restraint (Tonne)`, na.rm = TRUE)) %>%
            #`Recoverable Minerals Exported as Scrap/Batts` = sum(`Minerals Recoverable in Exported Scrap/Batts (Tonne)`, na.rm = TRUE),
            #`Recoverable Minerals Exported as BM` = sum(`Minerals Recoverable in Exported BM (Tonne)`, na.rm = TRUE),
            #`Minerals Lost in Non-Hydrometallurgical Facilities` = sum( `Minerals Lost to Pyrometalurgy (Tonne)`, na.rm = TRUE)) %>%
  #filter(Scenario == "Baseline Battery - Benchmark Chemistry") %>%
  #filter(Mineral == c("Cobalt", "Copper", "Nickel")) %>%
  pivot_longer(cols = c("Current NA Recycling Capacity", "All Material is Recycled in NA"),
                                                        # "Recoverable Minerals Exported as Scrap/Batts", "Recoverable Minerals Exported as BM",
                                                        # "Minerals Lost in Non-Hydrometallurgical Facilities"),
                        names_to = "Recycling Scenario", values_to = "Tonne") %>%
  mutate(Year = as.numeric(Year)) %>% filter(Year <= 2050) %>%
  mutate(Scenario = factor(Scenario, levels = legend_order)) %>%
  mutate(
    `Recycling Scenario` = forcats::fct_recode(
      `Recycling Scenario`,
      "Recycling Limited to NA 2025 Online or Planned" = "Current NA Recycling Capacity"
    )
  )


all_NA_cap_chem_rec <-cap_chem_results %>% group_by(Year, Scenario, Mineral) %>%
  summarise(`Current NA Recycling Capacity` =  sum(`Available Recycled Minerals (w Scrap) (Tonne)`, na.rm = TRUE),
            `All Material is Recycled in NA` = sum(`Available Recycled Minerals No R Restraint (Tonne)`, na.rm = TRUE)) %>%
  #`Recoverable Minerals Exported as Scrap/Batts` = sum(`Minerals Recoverable in Exported Scrap/Batts (Tonne)`, na.rm = TRUE),
  #`Recoverable Minerals Exported as BM` = sum(`Minerals Recoverable in Exported BM (Tonne)`, na.rm = TRUE),
  #`Minerals Lost in Non-Hydrometallurgical Facilities` = sum( `Minerals Lost to Pyrometalurgy (Tonne)`, na.rm = TRUE)) %>%
  #filter(Scenario == "Baseline Battery - Benchmark Chemistry") %>%
  pivot_longer(cols = c("Current NA Recycling Capacity", "All Material is Recycled in NA"),
               # "Recoverable Minerals Exported as Scrap/Batts", "Recoverable Minerals Exported as BM",
               # "Minerals Lost in Non-Hydrometallurgical Facilities"),
               names_to = "Recycling Scenario", values_to = "Tonne") %>%
  mutate(Year = as.numeric(Year)) %>% 
  mutate(Scenario = factor(Scenario, levels = legend_order)) %>%
  mutate(
    `Recycling Scenario` = forcats::fct_recode(
      `Recycling Scenario`,
      "Recycling Limited to NA 2025 Online or Planned" = "Current NA Recycling Capacity"
    )
  )


non_recovery_lost <- cap_chem_results %>% 
  group_by(Year, Scenario, Mineral) %>%
  summarise(`Minerals Lost From Non-Recovery` = sum(`Minerals Lost to Pyrometalurgy (Tonne)`, na.rm = TRUE)) %>%
  select(Year, Mineral, `Minerals Lost From Non-Recovery`, Scenario) %>%
  arrange(Mineral, Year, Scenario) %>%
  group_by(Mineral, Scenario) %>%
  mutate(
    Cum_Tonne = cumsum(`Minerals Lost From Non-Recovery`)
  ) %>%
  ungroup() %>%
  filter(Year == 2035, Cum_Tonne > 0) %>%
  mutate(
    Year = as.numeric(Year),
    Scenario = factor(Scenario, levels = legend_order),
    Mineral = factor(Mineral, levels = c("Manganese", "Copper", "Lithium", "Graphite"))  # 👈 add this
  )


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
  filter(Total_Minerals_Exported >= 0, Year >=2035) %>%
  mutate(Year = as.numeric(Year)) %>%
  mutate(Scenario = factor(Scenario, levels = legend_order))

scenario_base_colors <- c(
  "Increasing Batt Cap - Benchmark Chemistry" = "#d7301f",
  "Increasing Batt Cap - High LFP Chemistry" = "#fdae85",  # warmer, more orange
  "Decreasing Batt Cap - Benchmark Chemistry" = "#2171b5",
  "Decreasing Batt Cap - High LFP Chemistry" = "#1b9e77"
)

## Recycling Plots
ggplot(
  NA_cap_chem_rec,
  aes(
    x = Year,
    y = Tonne/1000,
    color = Scenario,                
    linetype = `Recycling Scenario`,    
    group = interaction(Scenario, `Recycling Scenario`)  
  )
) + 
  scale_y_sqrt(
    breaks = scales::pretty_breaks(n = 6)
  ) +
  geom_line(linewidth = 1.1) +
  #geom_point(aes(shape = `Recycling Scenario`), size = 2) +
  facet_wrap(~ Mineral, scales = "free_y") +
  labs(
    title = "North America Yearly Recoverable Minerals Until 2050",
    x = "Year",
    y = "Recycled Minerals (thousands Metric Tonnes)",
    color = "Battery Capacity - Chemistry Scenario",
    linetype = "Recycling Scenario",
    shape = "Recycling Scenario"
  ) +
  scale_linetype_manual(values = c(
    "Recycling Limited to NA 2025 Online or Planned" = "solid",
    "All Material is Recycled in NA" = "dashed"
  ),
  drop = FALSE) +
  scale_shape_manual(values = c(
    "Recycling Limited to NA 2025 Online or Planned" = 16,
    "All Material is Recycled in NA" = NA
  ),
  drop = FALSE) +
  theme_minimal(base_size = 20) +
  scale_x_continuous(
    breaks = seq(2025, 2050, by = 5)
  ) +
  theme(
    plot.title = element_text(hjust = 0.5, size = 24, face = "bold"),
    axis.title = element_text(size = 20, face = "bold"),
    axis.text.y = element_text(size = 14),  # Change from 20 to smaller size
    axis.text.x = element_text(angle = 30, hjust = 1, size = 16),
    strip.text = element_text(size = 20, face = "bold"),
    legend.position = "bottom",
    legend.title = element_text(size = 20, face = "bold"),
    legend.text = element_text(size = 15),
    legend.box = "vertical",
    legend.box.just = "center"
  ) +
  scale_color_manual(values = scenario_base_colors) +
  guides(
    color = guide_legend(
      title.position = "top",
      title.hjust = 0.5,
      nrow = 2,
      byrow = TRUE,
      order = 1
    ),
    linetype = guide_legend(
      title.position = "top",
      title.hjust = 0.5,
      nrow = 2,
      byrow = TRUE,
      override.aes = list(
        color = "black"
      ),
      order = 2
    ),
    shape = "none"
  )




ggplot(
  non_recovery_lost,
  aes(
    x = Mineral,
    y = Cum_Tonne/1000,
    #fill = Scenario,
    pattern = Mineral
  )
) + 
  geom_col_pattern(
    position = position_dodge(width = 0.8),
    width = 0.7,
    color = "black",                 # outlines help readability
    pattern_fill = "black",
    pattern_density = 0.2,
    pattern_spacing = 0.03
  ) +
  labs(
    title = "Cumulative North America Minerals Lost to Lack of Recovery Standards (2035)",
    x = "Mineral",
    y = "Lost Minerals (thousands Metric Tonnes)",
    #fill = "Scenario",
    #pattern = "Mineral"
  ) +
  #scale_fill_manual(values = scenario_base_colors) +
  #scale_pattern_manual(values = c(
    #"Lithium" = "stripe",
   # "Copper" = "crosshatch",
   # "Manganese" = "circle",
   # "Graphite" = "wave"
  #)) + 
  #guides(
    #fill = guide_legend(
      #override.aes = list(pattern = "none"),
      #nrow = 2,
      #byrow = TRUE
    #),
    #pattern = guide_legend(
      #nrow = 1,
     # byrow = TRUE,
      #override.aes = list(
        #fill = "white",   # 👈 ensures white background
        #color = "black"
      #)
    #)
 # ) +
  scale_y_sqrt()+
  theme_minimal(base_size = 14) +
  theme(
    #legend.key = element_rect(fill = "white", color = NA),
    plot.title = element_text(hjust = 0.5, size = 24, face = "bold"),
    axis.title = element_text(size = 20, face = "bold"),
    axis.text = element_text(size = 20),
    axis.text.x = element_text(angle = 30, hjust = 1)
    #legend.position = "bottom",
    #legend.box = "horizontal",
    #legend.title = element_text(size = 20, face = "bold"),
    #legend.text = element_text(size = 20)
  ) 


# Define a base color for each Scenario

legend_order_recycle <- c(
  "Increasing Batt Cap - Benchmark Chemistry - Black Mass",
  "Increasing Batt Cap - Benchmark Chemistry - Refining",
  "Increasing Batt Cap - High LFP Chemistry - Black Mass",
  "Increasing Batt Cap - High LFP Chemistry - Refining",
  "Decreasing Batt Cap - Benchmark Chemistry - Black Mass",
  "Decreasing Batt Cap - Benchmark Chemistry - Refining",
  "Decreasing Batt Cap - High LFP Chemistry - Black Mass",
  "Decreasing Batt Cap - High LFP Chemistry - Refining"
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




# Plot
ggplot(needed_cap_long, aes(
  x = Year,
  y = Tonne,
  color = Scenario,
  linetype = `Recycling Step`,
  group = interaction(Scenario, `Recycling Step`)
)) +
  geom_line(linewidth = 1.2) +
  #geom_point(size = 2) +
  scale_x_continuous(
    breaks = seq(2025, 2050, by = 5),
    expand = c(0, 0)
  ) +
  scale_y_continuous(
    expand = c(0, 0)
  )+
  scale_color_manual(values = scenario_base_colors) +
  scale_linetype_manual(values = c(
    "Black Mass" = "solid",
    "Refining" = "dashed"
  )) +
  labs(
    title = "Yearly Deficit in Black Mass and Refining Capacity Until 2050",
    x = "Year",
    y = "Needed Recycling (Millions of MT)",
    color = "Scenario",
    linetype = "Recycling Step"
  ) +
  theme_minimal(base_size = 20) +
  guides(
    color = guide_legend(
      nrow = 2, 
      byrow = TRUE, 
      order = 1,
      title = "Scenario"
    ),
    linetype = guide_legend(
      nrow = 1, 
      order = 2,
      override.aes = list(
        color = "black",
        linewidth = 2,
        size = 3
      ),
      title = "Recycling Step"
    )
  ) +
  theme(
    plot.title = element_text(hjust = 0.5, size = 24, face = "bold"),
    axis.title = element_text(size = 20, face = "bold"),
    axis.text = element_text(size = 20),
    axis.text.x = element_text(angle = 30, hjust = 1),
    strip.text = element_text(size = 20, face = "bold"),
    legend.text = element_text(size = 16),
    legend.title = element_text(size = 20, face = "bold"),
    legend.position = "bottom",
    legend.box = "vertical",
    legend.box.just = "center",
    legend.key.width = unit(2.5, "cm"),
    legend.key.height = unit(0.8, "cm")
  )



ggplot(export_lost, aes(
  x = Year,
  y = Total_Minerals_Exported/1000, 
  color = Scenario,
  group = Scenario
)) + geom_line(size = 1.2) +
  geom_point(size = 2) +
  scale_color_manual(values = scenario_base_colors) +  # now colors will show
  facet_wrap(~ Mineral, scales = "free_y") +
  labs(
    title = "Exported Mass of Battery Minerals Each Year Under Current NA Recycling Plans",
    x = "Year",
    y = "Exported Minerals (thousands of Metric Tonnes)",
    color = "Battery Capacity - Chemistry Scenario") +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(hjust = 0.5, size = 18, face = "bold"),
    axis.title = element_text(size = 14, face = "bold"),
    axis.text = element_text(size = 12),
    axis.text.x = element_text(angle = 30, hjust = 1),
    strip.text = element_text(size = 14, face = "bold"),
    legend.box = "horizontal",
    legend.position = "bottom",
    legend.title = element_text(size = 12, face = "bold"),
    legend.text = element_text(size = 11),
  ) + 
  scale_y_sqrt(
    breaks = scales::pretty_breaks(n = 8)
  ) +
  guides(
    color = guide_legend(nrow = 2, byrow = TRUE)  # first row for color
  ) 







##IGNORE
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
# }s
# 
# 
# # Close the PDF device
# dev.off()
# 
# ##Manufacturing and Recycling capacity by state






Continent_LIB_Recycle <- future_recycle_type %>% group_by(Year) %>% summarise(all_recycle = sum(LIB_recycle_total))
Continent_Demand <- state_capacity_added %>% group_by(Year) %>% summarise(all_demand = sum(Total_Add_LIB))
ratio_in_batts <- Continent_LIB_Recycle %>% merge(Continent_Demand, on = "Year") %>% mutate(percent = Continent_LIB_Recycle/Continent_Demand)


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
