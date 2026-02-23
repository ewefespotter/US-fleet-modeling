library(dplyr)
library(purrr)
library(readxl)
library(dplyr)
library(tidyr)
library(stringr)
library(tidyverse)
library(ggplot2)
library(ggforce)

## All replacement EVLIBs and new EV from EV_Flow
future_demand_type <-state_capacity_added

future_demand_type <- state_capacity_added %>%
  mutate(
    Year = as.integer(Year),
    State_Province = case_when(
      State_Province %in% names(state_map_rev) ~ state_map_rev[State_Province],
      TRUE ~ State_Province
    )
  ) %>%
  filter(Year > 2025) %>%
  rename(Sale_Year = Year) #for ease of conversions that are based on sale year---> here year = sale year


### RUN CAPACITY SCENARIOS
capacity_chem_scenarios <- function(batt_cap_df,chem_df, mineral_intensity, future_demand_type) {
  batt_df_collapsed <- batt_cap_df %>%
    group_by(State_Province, Segment, Propulsion, Sale_Year) %>%
    summarise(
      `Projected Avg Batt Cap (kwh/batt)` =
        first(`Projected Avg Batt Cap (kwh/batt)`),
      .groups = "drop"
    )
  ### RECYCLE in Future- cut only those sales years with the projection
  future_demand_cap <- future_demand_type %>% left_join(
    batt_df_collapsed, 
    by = c("State_Province", "Sale_Year", "Segment", "Propulsion"))

  # Apply avg battery size per powertrain and type
  future_demand_cap$LIB_demand_kwh <- future_demand_cap$Total_Add_LIB * future_demand_cap$`Projected Avg Batt Cap (kwh/batt)`

  future_demand_cap <- future_demand_cap %>% group_by(Sale_Year, State_Province) %>%
    summarise(LIB_demand_kwh = sum(LIB_demand_kwh, na.rm = TRUE),
              .groups = "drop")

  future_demand_cap <- future_demand_cap %>%
    arrange(State_Province, Sale_Year) 
  
  ### APPLY BENCHMARK
  future_demand_chem <- future_demand_cap %>% 
    left_join(chem_df, by = c("Sale_Year"), relationship = 'many-to-many') %>%
    mutate(Cathode_kwh_state = LIB_demand_kwh * `Cathode Mix Share`) %>%
    select(-`Cathode Mix Share`)
  
  nat_demand <- future_demand_chem %>% group_by(Sale_Year) %>%
    summarise(Cathode_kwh_state = sum(Cathode_kwh_state, na.rm = TRUE))

  future_demand_minerals <- 
    left_join(future_demand_chem, mineral_intensity, by = c("Cathode Mix"), relationship = 'many-to-many') %>%
    filter(!Mineral %in% Not_recovered) %>% ## don't care if demanded either then
    mutate(`Demanded Minerals (kg)` = `kg_per_kwh` * `Cathode_kwh_state`) %>%
    select(Sale_Year, State_Province, Mineral, `Demanded Minerals (kg)`) 

  nat_min_demand <- future_demand_minerals %>%group_by(Sale_Year, Mineral) %>%
    summarise(`Demanded Minerals (kg)` = sum(`Demanded Minerals (kg)`, na.rm = TRUE)) %>%
    filter(Mineral == "Nickel")

  future_demand_final <- future_demand_minerals %>%
    group_by(Sale_Year, State_Province, Mineral) %>%
    summarise(`Demanded Minerals (kg)` = sum(`Demanded Minerals (kg)`, na.rm = TRUE), .groups = "drop") %>%
    filter(!is.na(`Mineral`)) %>%
    mutate(`Demand Minerals (Tonne)` = `Demanded Minerals (kg)`/1000) %>%
    rename(Year = Sale_Year)
  
  
  fut_nat_fin <- future_demand_final %>% group_by(Year, Mineral) %>%
    summarise(`Demand Minerals (Tonne)` = sum(`Demand Minerals (Tonne)`, na.rm = TRUE)) 
  

  return(future_demand_final)
}


# Set names for scenarios
names(batt_scen) <- c("Baseline Capacity", "15% Lower Capacity")
names(chem_scens) <- c("Original Chemistry", "High LFP Chemistry")

# Use `crossing()` to create all 4 combinations
scenario_combos <- crossing(
  Batt = names(batt_scen),
  Chem = names(chem_scens)
)


safe_capacity_chem_scenarios <- function(batt_name, chem_name) {
  tryCatch({
    df <- capacity_chem_scenarios(
      batt_cap_df = batt_scen[[batt_name]],
      chem_df = chem_scens[[chem_name]],
      mineral_intensity = mineral_intensity,
      future_demand_type = future_demand_type
    )
    
    # Add scenario labels
    df %>%
      mutate(
        Battery_Scenario = batt_name,
        Chemistry_Scenario = chem_name
      )
    
  }, error = function(e) {
    message("⚠ Error in scenario: ", batt_name, " / ", chem_name)
    message("  -> ", e$message)
    NULL  # return NULL so you can filter out later
  })
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



nat_demand_cap_chem<- cap_chem_demand_results %>% group_by(Year, Scenario, Mineral) %>%
  summarise(`Demand Minerals (Tonne)` = sum(`Demand Minerals (Tonne)`, na.rm = TRUE)) %>%
  mutate(Year = as.numeric(Year))


recycle_shifted <- all_nat_cap_chem_rec %>%
  arrange(Mineral, Scenario, Year) %>%
  group_by(Mineral, Scenario) %>%
  mutate(Year = as.numeric(Year) + 1) %>%  # shift Tonne to the next year
  ungroup()

# Step 2: Merge with demand
ratio_results <- recycle_shifted %>%
  inner_join(nat_demand_cap_chem, by = c("Year", "Mineral", "Scenario")) %>%
  mutate(Recycle_v_Demand = Tonne / `Demand Minerals (Tonne)`) %>%
  select(-c(Tonne, `Demand Minerals (Tonne)`)) %>%
  mutate(Scenario = factor(Scenario, levels = legend_order))

#ratio_scrap <- merge(cap_chem_demand_results, recycle_and_scrap, by = c("Year", "State", "Mineral", "Scenario"))
#ratio_scrap <- ratio_scrap %>% mutate(Recycle_Demand = `Available Recycled Minerals (kg)`/`Demanded Minerals (kg)`) %>% filter (Mineral != "Aluminum") %>% filter (Mineral != "Steel")


## Recycling Plots
ggplot(
  ratio_results,
  aes(
    x = as.numeric(Year),
    y = Recycle_v_Demand*100,
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
    title = "Maximum Recycled Content in North America",
    x = "Year",
    y = "% Recycled Content",
    color = "Battery Capacity - Chemistry Scenario",
    alpha = "Recycling Scenario"
  ) +
  scale_alpha_manual(values = c(
    "Recycling Limited to NA 2025 Online or Planned Facilities" = 1,  # darkest
    "All Material is Recycled in NA" = 0.4
    # add more if you have more recycling scenarios
  )) +
  scale_x_continuous(breaks = seq(2025, 2050, by = 5)) + 
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





# # Combine historical and future projections
# install.packages("ggforce")
# ## change file to ACCII or Repeal- change title
# ## change state_data to scrap or results and chnage title and y axis for scrap or regular results
# # Get all unique states
# states <- unique(ratio_results$State)
# 
# 
# output_file <- "/Users/elsawefes-potter/Documents/Critical_Minerals_Pablo/ratio_minerals_by_state_w_recovery_scrap_Repeal.pdf"
# 
# pdf(output_file, width = 12, height = 8)
# 
# # Loop over states and make one page per state
# for (s in states) {
#   state_data <- ratio_scrap %>%
#     filter(State == s)
# 
#   
#   p <- ggplot(state_data, aes(x = Year, y = `Recycle_Demand`,
#                               color = Scenario, linetype = Scenario)) +
#     geom_line() +
#     facet_wrap(~ Mineral, scales = "free_y", ncol = 2) +  # adjust ncol/nrow as needed
#     labs(
#       title = paste("Repeal - Minerals in Recycled Batteries + Scrap vs Minerals Demanded –", s),
#       x = "Year",
#       y = "Ratio (Recycled Material + Scrap/Demanded Material)",
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
# # Close the PDF device
# dev.off()
# 
