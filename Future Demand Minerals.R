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

  future_demand_cap <- future_demand_cap %>% group_by(Sale_Year, State_Province, Segment, Propulsion) %>%
    summarise(LIB_demand_kwh = sum(LIB_demand_kwh, na.rm = TRUE),
              .groups = "drop")

  future_demand_cap <- future_demand_cap %>%
    arrange(State_Province, Sale_Year) 
  
  ### APPLY BENCHMARK
  future_demand_chem <- future_demand_cap %>% 
    left_join(chem_df, by = c("Sale_Year","Segment","Propulsion"), relationship = 'many-to-many') %>%
    mutate(Cathode_kwh_state = LIB_demand_kwh * `Cathode Mix Share`) %>%
    select(-`Cathode Mix Share`)
  
  nat_demand <- future_demand_chem %>% group_by(Sale_Year) %>%
    summarise(Cathode_kwh_state = sum(Cathode_kwh_state, na.rm = TRUE))
  
  Not_recovered  <- c("Phosphorus", "Stainless steel", "Steel", "Aluminum","Carbon")
  
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
names(batt_scen) <- c("Increasing Batt Cap", "Decreasing Batt Cap")
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
  select(-Battery_Scenario,-Chemistry_Scenario) %>%
  mutate(
    Country = case_when(
      State_Province %in% us_codes ~ "US",
      State_Province %in% ca_codes ~ "CA",
      State_Province == "MX" ~ "MX",
      TRUE ~ NA_character_
    )
  ) %>%
  mutate(
    Scenario = dplyr::recode(
      Scenario,
      "Increasing Batt Cap - Original Chemistry" = "Increasing Batt Cap - Benchmark Chemistry",
      "Increasing Batt Cap - High LFP Chemistry" = "Increasing Batt Cap - High LFP Chemistry",
      "Decreasing Batt Cap - Original Chemistry" = "Decreasing Batt Cap - Benchmark Chemistry",
      "Decreasing Batt Cap - High LFP Chemistry" = "Decreasing Batt Cap - High LFP Chemistry"
    ))

country_demand_cap_chem <- cap_chem_demand_results %>% group_by(Country, Year, Scenario, Mineral) %>%
  summarise(`Minerals Demand` = sum(`Demand Minerals (Tonne)`)) %>%
  pivot_longer(cols = `Minerals Demand`,
               names_to = "Recycling Scenario", values_to = "Tonnes") %>%
  mutate(Year = as.numeric(Year)) %>%
  filter(Year == 2050, 
         Scenario %in% c("Increasing Batt Cap - Benchmark Chemistry"))

country_cap_chem_rec <- cap_chem_results %>% group_by (Country, Year, Scenario, Mineral) %>%
  summarise(`Current NA Recycling Capacity` =  sum(`Available Recycled Minerals (w Scrap) (Tonne)`, na.rm = TRUE),
            `All Material is Recycled in NA` = sum(`Available Recycled Minerals No R Restraint (Tonne)`, na.rm = TRUE)) %>%
  pivot_longer(cols = c("Current NA Recycling Capacity", "All Material is Recycled in NA"),  
               names_to = "Recycling Scenario", values_to = "Tonne") %>%
  mutate(Year = as.numeric(Year)) %>% 
  filter(Year == 2050,
         Scenario %in% c("Increasing Batt Cap - Benchmark Chemistry"))


overall_circularity <- country_demand_cap_chem %>%
  rename(Demand_Tonne = Tonnes) %>%
  full_join(
    country_cap_chem_rec %>%
      rename(Recycling_Tonne = Tonne),
    by = c("Country", "Year", "Mineral", "Scenario", "Recycling Scenario")
  ) %>%
  mutate(
    Tonnes = coalesce(Demand_Tonne, Recycling_Tonne),
    Type = case_when(
      !is.na(Demand_Tonne) & is.na(Recycling_Tonne) ~ "Demand",
      is.na(Demand_Tonne) & !is.na(Recycling_Tonne) ~ "Recycling",
      TRUE ~ "Both"
    )
  ) %>%
  select(-Demand_Tonne, -Recycling_Tonne) %>%
  mutate(
    pattern_type = case_when(
      Country == "US" ~ "diagonal",
      Country == "CA" ~ "stripe",
      Country == "MX" ~ "circle"   # dotted look
    )
  ) %>%
  mutate(
    Country = factor(Country, levels = c("CA", "US", "MX")),
    Mineral = factor(Mineral, levels = c("Cobalt","Lithium","Manganese","Nickel","Copper","Graphite")),
    Tonnes = Tonnes/1e6
  ) 



##overall circularity
ggplot(overall_circularity, aes(x = `Recycling Scenario`, y = Tonnes, pattern = Country,
                              fill = Country)) +
  geom_col_pattern(
    position = "stack",
    color = "black",
    pattern_density = 0.2,
    pattern_spacing = 0.05,
    pattern_alpha = 0.3,
    pattern_size = 0.2,
    pattern_fill = "black"
  ) +
  scale_pattern_manual(
    values = c(
      "US" = "circle",
      "CA" = "stripe",
      "MX" = "crosshatch"
    )
  ) +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 5)) +
  scale_x_discrete(
    labels = function(x) str_wrap(x, width = 12)
  )+
  facet_wrap(~Mineral, scales = "free_y")+
  theme_minimal(base_size = 20) +
  labs(
    title = "Mineral Demand vs Mineral Availability (2050) ",
    x = "Recycling Scenario",
    y = "Tonnes (millions)",
    fill = "Country",
    pattern = "Country"
  ) +
  guides(
    color = guide_legend(
      nrow = 5,
      ncol = 2,
      byrow = TRUE,
      override.aes = list(
        fill = "white",
        color = "black"))
  ) +   
  theme_minimal(base_size = 20) +
  theme(
    legend.box = "vertical",
    legend.position = "bottom",
    plot.title = element_text(size = 24, hjust = 0.5, face = "bold"),
    axis.title = element_text(size = 20),
    axis.text = element_text(size = 14),
    strip.text = element_text(size = 20, face = "bold"),
    legend.title = element_text(size = 20),
    legend.text = element_text(size = 20),
    legend.key.width = unit(0.8, "cm")
  )




NA_demand_cap_chem<- cap_chem_demand_results %>% group_by(Year, Scenario, Mineral) %>%
  summarise(`Demand Minerals (Tonne)` = sum(`Demand Minerals (Tonne)`, na.rm = TRUE)) %>%
  mutate(Year = as.numeric(Year)) 


recycle_shifted <- all_NA_cap_chem_rec %>%
  arrange(Mineral, Scenario, Year) %>%
  group_by(Mineral, Scenario) %>%
  mutate(Year = as.numeric(Year) + 1) %>%  # shift Tonne to the next year
  ungroup()

# Step 2: Merge with demand
ratio_results <- recycle_shifted %>%
  inner_join(NA_demand_cap_chem, by = c("Year", "Mineral", "Scenario")) %>%
  mutate(Recycle_v_Demand = Tonne / `Demand Minerals (Tonne)`) %>%
  select(-c(Tonne, `Demand Minerals (Tonne)`)) %>%
  mutate(Scenario = factor(Scenario, levels = legend_order)) 
  

#ratio_scrap <- merge(cap_chem_demand_results, recycle_and_scrap, by = c("Year", "State", "Mineral", "Scenario"))
#ratio_scrap <- ratio_scrap %>% mutate(Recycle_Demand = `Available Recycled Minerals (kg)`/`Demanded Minerals (kg)`) %>% filter (Mineral != "Aluminum") %>% filter (Mineral != "Steel")

scenario_base_colors <- c(
  "Increasing Batt Cap - Benchmark Chemistry" = "#d7301f",
  "Increasing Batt Cap - High LFP Chemistry" = "#fdae85",  # warmer, more orange
  "Decreasing Batt Cap - Benchmark Chemistry" = "#2171b5",
  "Decreasing Batt Cap - High LFP Chemistry" = "#1b9e77"
)

## Recycling Plots
ggplot(
  ratio_results,
  aes(
    x = as.numeric(Year),
    y = Recycle_v_Demand * 100,
    color = Scenario,
    linetype = `Recycling Scenario`,
    group = interaction(Scenario, `Recycling Scenario`)
  )
) + 
  geom_line(linewidth = 1.2) +
  
  scale_color_manual(values = scenario_base_colors) +
  
  scale_linetype_manual(
    values = c(
      "Recycling Limited to NA 2025 Online or Planned" = "solid",
      "All Material is Recycled in NA" = "dashed"
    )
  ) +
  
  scale_y_continuous(
    limits = c(0, NA),
    breaks = scales::pretty_breaks(n = 8),
    expand = expansion(mult = c(0, 0.05))
  ) +
  
  facet_wrap(~ Mineral, scales = "free_y") +
  
  labs(
    title = "Maximum Recycled Content Standard in North America",
    x = "Year",
    y = "% Recycled Content",
    color = "Scenario",
    linetype = "Recycling Scenario"
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
      title = "Recycling Scenario"
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
