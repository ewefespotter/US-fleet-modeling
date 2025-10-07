library(dplyr)
library(purrr)
library(readxl)
library(dplyr)
library(tidyr)
library(stringr)
library(tidyverse)


EVLIB_Flows_demand <- read_csv("/Users/elsawefes-potter/Documents/Critical_Minerals_Pablo/EVLIB_Flows_detail_ACCII (1) .csv")

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
EVLIB_Flows_demand$EV_stock_vector <- Map(
  name_vector_with_years,
  EVLIB_Flows_demand$EV_stock_vector,
  EVLIB_Flows_demand$Year
)


future_demand_type <- EVLIB_Flows_demand %>%
  mutate(
    demand_df = map(EV_stock_vector, ~ {
      tibble(
        Sale_Year = as.integer(names(.x)),
        EV_stock_total = as.numeric(.x)
      )
    })
  ) %>%
  unnest(cols = demand_df)  %>%
  filter(Year == Sale_Year) %>%  # Only keep rows for the matching year
  select(State, Segment, Propulsion, Year, Sale_Year, EV_stock_total)




### RUN CAPACITY SCENARIOS
capacity_chem_scenarios <- function(batt_cap_df,chem_df) {
  
  ### RECYCLE in Future- cut only those sales years with the projection
  future_demand_cap <- merge(
    batt_cap_df, 
    future_demand_type, 
    by = c("Sale_Year", "Segment", "Propulsion"), all.x = TRUE)
  
  # Apply avg battery size per powertrain and type
  future_demand_cap$LIB_demand_kwh <- future_demand_cap$EV_stock_total * future_demand_cap$`Projected Avg Batt Cap (kwh/batt)`
  
  # Keep only relevant columns
  future_demand_cap <- future_demand_cap %>%
    select(`Year`, `Sale_Year`, State, `Segment`,`Propulsion`, 
           `LIB_demand_kwh`)
  
  
  ### APPLY BENCHMARK
  future_demand_chem <- left_join(future_demand_cap, chem_df, by = c("Sale_Year"), relationship = 'many-to-many') 
  future_demand_chem$Cathode_kwh_state<- future_demand_chem$LIB_demand_kwh * future_demand_chem$`Cathode Mix Share`
  
  
  future_demand_minerals <- left_join(future_demand_chem, mineral_intensity, by = "Cathode Mix", relationship = 'many-to-many') %>%
    mutate(`Demanded Minerals (kg)` = `kg_per_kwh` * `Cathode_kwh_state`) %>%
    select(`Year`, `Sale_Year`, State, Mineral, `Demanded Minerals (kg)`)
  
  future_demand_final <- future_demand_minerals %>%
    group_by(Year, State, Mineral) %>%
    summarise(`Demanded Minerals (kg)` = sum(`Demanded Minerals (kg)`, na.rm = TRUE), .groups = "drop") %>%
    filter(!is.na(`Mineral`))
  
  
  return(future_demand_final)
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
        `Demanded Minerals (kg)` = numeric(),
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
      `Demanded Minerals (kg)` = numeric(),
      Battery_Scenario = character(),
      Chemistry_Scenario = character()
    )
  })
  
  return(res)
}


# Run all scenarios using pmap safely
all_demand_scenarios <- scenario_combos %>%
  mutate(
    result = pmap(
      list(Batt, Chem),
      safe_capacity_chem_scenarios
    )
  )


cap_chem_demand_results <- bind_rows(all_demand_scenarios$result)

cap_chem_demand_results <- cap_chem_demand_results %>%
  mutate(Scenario = paste(Battery_Scenario, Chemistry_Scenario, sep = " - ")) %>%
  select(-Battery_Scenario,-Chemistry_Scenario)


ratio_results <- merge(cap_chem_demand_results, summary_final_future_hist, by = c("Year", "State", "Mineral", "Scenario"))

ratio_results <- ratio_results %>% mutate(Recycle_Demand = `Available Recycled Minerals (kg)`/`Demanded Minerals (kg)`)

# Combine historical and future projections
install.packages("ggforce")
library(ggplot2)
library(ggforce)
library(dplyr)

# Get all unique states
states <- unique(ratio_results$State)

output_file <- "/Users/elsawefes-potter/Documents/Critical_Minerals_Pablo/ratio_minerals_by_state.pdf"


pdf(output_file, width = 12, height = 8)

# Loop over states and make one page per state
for (s in states) {
  state_data <- ratio_results %>%
    filter(State == s)

  
  p <- ggplot(state_data, aes(x = Year, y = `Recycle_Demand`,
                              color = Scenario, linetype = Scenario)) +
    geom_line() +
    facet_wrap(~ Mineral, scales = "free_y", ncol = 2) +  # adjust ncol/nrow as needed
    labs(
      title = paste("Minerals in Recycled Batteries vs Minerals Demanded –", s),
      x = "Year",
      y = "Ratio (Recycled Material/Demanded Material)",
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

