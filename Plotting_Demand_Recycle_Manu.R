library(openxlsx)
library(ggpattern)
library(readxl)
library(readr)
library(tidyverse)

library(ggplot2)
library(geofacet)

# from future_minerals_recycle script                
recycle_batts_by_state_2050 <- state_mass_recycle_batt %>%
  filter(Year == 2050) %>%
  pivot_wider(
    names_from = Scenario,
    values_from = Batt_Mass_MT
  ) %>%
  rename(
    Recycle_Batt_Proj           = `Increasing Batt Cap - Benchmark Chemistry`,
    Recycle_Batt_15             = `Decreasing Batt Cap - Benchmark Chemistry`,
    Recycle_Batt_Proj_LFP       = `Increasing Batt Cap - High LFP Chemistry`,
    Recycle_Batt_15_LFP         = `Decreasing Batt Cap - High LFP Chemistry`
  )



NA_batts <- state_mass_recycle_batt %>% 
  pivot_wider(
    names_from = Scenario,
    values_from = Batt_Mass_MT
  ) %>%
  rename(
    Recycle_Batt_Proj           = `Increasing Batt Cap - Benchmark Chemistry`,
    Recycle_Batt_15             = `Decreasing Batt Cap - Benchmark Chemistry`,
    Recycle_Batt_Proj_LFP       = `Increasing Batt Cap - High LFP Chemistry`,
    Recycle_Batt_15_LFP         = `Decreasing Batt Cap - High LFP Chemistry`
  )%>%
  group_by(Year) %>%
  summarise(Recycle_Batt_Proj = sum(Recycle_Batt_Proj),
            Recycle_Batt_15 = sum(Recycle_Batt_15),
            Recycle_Batt_Proj_LFP = sum(Recycle_Batt_Proj_LFP),
            Recycle_Batt_15_LFP = sum(Recycle_Batt_15_LFP))

### ALL REGULAR MANUFACTURING 
Mass_2050_projected <- full_join(state_demand_tonnes_2050,
                                 manufacturing_tonnes_2050_projected,
                                 by = c("Year", "State_Province")) %>%
  full_join(recycling_tonnes_2050_projected,
            by = c("Year", "State_Province")) %>%
  mutate(across(where(is.numeric), ~replace_na(.x, 0))) %>%
  full_join(recycle_batts_by_state_2050,
            by = c("Year","State_Province")) %>%
  mutate(
    Country = case_when(
      State_Province %in% us_codes ~ "US",
      State_Province %in% ca_codes ~ "CA",
      State_Province == "MX" ~ "MX",
      TRUE ~ NA_character_
    )
  )


## State level one
Mass_2050_projected_ref <- Mass_2050_projected %>% 
  select(Year, State_Province, Add_LIB_proj_tonnes, Add_LIB_15_tonnes, 
         Tonnes_Prod_proj_down, Tonnes_Prod_15_down, Tonnes_Prod_proj_mid, Tonnes_Prod_15_mid, 
         Recycle_Batt_Proj, Recycle_Batt_15,
         Cumulative_black_mass_cap, Cumulative_refining_cap) 


### CHANGED TO 2030--include delay and 15% in one plot for national compare
Mass_2050_projected_ref <- Mass_2050_projected_ref %>%
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
        Recycle_Batt_Proj, 
        Recycle_Batt_15,
        Cumulative_black_mass_cap,
        Cumulative_refining_cap
      ),
      ~ .x / 1e6
    )
  ) %>%
  rename(`LIB Demand` = Add_LIB_proj_tonnes, 
         `15% Reduced Batt Cap LIB Demand` = Add_LIB_15_tonnes,
         `Pack Manufacturing` = Tonnes_Prod_proj_down, 
         `15% Reduced Batt Cap Pack Manufacturing` = Tonnes_Prod_15_down,
         `Cell Manufacturing` = Tonnes_Prod_proj_mid, 
         `15% Reduced Batt Cap Cell Manufacturing` = Tonnes_Prod_15_mid,
         `End of Life Batteries` = Recycle_Batt_Proj,
         `15% Reduced Batt Cap End of Life Batteries` = Recycle_Batt_15,
         `Black Mass` = Cumulative_black_mass_cap, 
         `Refining` = Cumulative_refining_cap) %>%
  select(-c(`15% Reduced Batt Cap LIB Demand`,`15% Reduced Batt Cap Pack Manufacturing`,`15% Reduced Batt Cap Cell Manufacturing`, `15% Reduced Batt Cap End of Life Batteries`)) %>%
  pivot_longer(cols = c(`LIB Demand`,
                        `Pack Manufacturing`,
                        `Cell Manufacturing`,
                        `End of Life Batteries`,
                        `Black Mass`, `Refining`),
               names_to = "Origin",
               values_to = "Metric Tonnes (millions)") 




Mass_2050_projected_ref <- Mass_2050_projected_ref %>%
  mutate(
    Origin = as.character(Origin),  # ensures no leftover factor levels
    Origin = factor(
      Origin,
      levels = c(
        "LIB Demand",
        #        "15% Reduced Batt Cap LIB Demand",
        "Pack Manufacturing",
        #        "15% Reduced Batt Cap Pack Manufacturing",
        "Cell Manufacturing",
        #        "15% Reduced Batt Cap Cell Manufacturing",
        "End of Life Batteries",
        #      "15% Reduced Batt Cap End of Life Batteries",
        "Black Mass",
        "Refining"
      )
    )
  ) 

# Add Region column to your data
Mass_2050_projected_ref <- Mass_2050_projected_ref %>%
  mutate(Region = region_mapping[State_Province]) 

## DELAYED DFs
Mass_2050_delayed <- full_join(state_demand_tonnes_2050,
                               manufacturing_tonnes_2050_delayed,
                               by = c("Year", "State_Province")) %>%
  full_join(recycling_tonnes_2050_delayed,
            by = c("Year", "State_Province")) %>%
  full_join(recycle_batts_by_state_2050,
            by = c("Year","State_Province")) %>%
  mutate(across(where(is.numeric), ~replace_na(.x, 0))) %>%
  rename(Cumulative_black_mass_cap = Delay_Cumulative_black_mass_cap, Cumulative_refining_cap = Delay_Cumulative_refining_cap) %>%
  mutate(
    Country = case_when(
      State_Province %in% us_codes ~ "US",
      State_Province %in% ca_codes ~ "CA",
      State_Province == "MX" ~ "MX",
      TRUE ~ NA_character_
    )
  )



## State level one
Mass_2050_delayed_ref <- Mass_2050_delayed %>% 
  select(Year, State_Province, Add_LIB_proj_tonnes, Add_LIB_15_tonnes, 
         Tonnes_Prod_proj_down, Tonnes_Prod_15_down, Tonnes_Prod_proj_mid, Tonnes_Prod_15_mid, 
         Recycle_Batt_Proj, Recycle_Batt_15,
         Cumulative_black_mass_cap, Cumulative_refining_cap) 

### CHANGED TO 2030--include delay and 15% in one plot for national compare
Mass_2050_delayed_ref <- Mass_2050_delayed_ref %>%
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
        Recycle_Batt_Proj, 
        Recycle_Batt_15,
        Cumulative_black_mass_cap,
        Cumulative_refining_cap
      ),
      ~ .x / 1e6
    )
  ) %>%
  rename(`LIB Demand` = Add_LIB_proj_tonnes, 
         `15% Reduced Batt Cap LIB Demand` = Add_LIB_15_tonnes,
         `Pack Manufacturing` = Tonnes_Prod_proj_down, 
         `15% Reduced Batt Cap Pack Manufacturing` = Tonnes_Prod_15_down,
         `Cell Manufacturing` = Tonnes_Prod_proj_mid, 
         `15% Reduced Batt Cap Cell Manufacturing` = Tonnes_Prod_15_mid,
         `End of Life Batteries` = Recycle_Batt_Proj,
         `15% Reduced Batt Cap End of Life Batteries` = Recycle_Batt_15,
         `Black Mass` = Cumulative_black_mass_cap, 
         `Refining` = Cumulative_refining_cap) %>%
  select(-c(`15% Reduced Batt Cap LIB Demand`,`15% Reduced Batt Cap Pack Manufacturing`,`15% Reduced Batt Cap Cell Manufacturing`, `15% Reduced Batt Cap End of Life Batteries`)) %>%
  pivot_longer(cols = c(`LIB Demand`,
                        `Pack Manufacturing`,
                        `Cell Manufacturing`,
                        `End of Life Batteries`,
                        `Black Mass`, `Refining`),
               names_to = "Origin",
               values_to = "Metric Tonnes (millions)") 


Mass_2050_delayed_ref <- Mass_2050_delayed_ref %>%
  mutate(
    Origin = as.character(Origin),  # ensures no leftover factor levels
    Origin = factor(
      Origin,
      levels = c(
        "LIB Demand",
        #        "15% Reduced Batt Cap LIB Demand",
        "Pack Manufacturing",
        #        "15% Reduced Batt Cap Pack Manufacturing",
        "Cell Manufacturing",
        #        "15% Reduced Batt Cap Cell Manufacturing",
        "End of Life Batteries",
        #      "15% Reduced Batt Cap End of Life Batteries",
        "Black Mass",
        "Refining"
      )
    )
  )


origin_colors <- c(
  "LIB Demand" = "#1b7fb3",
  "Decreasing Batt Cap LIB Demand" = "#6ba8d4",
  "Pack Manufacturing" = "#D77FBF",
  "Decreasing Batt Cap Pack Manufacturing" = "#EEC3DE",
  "Cell Manufacturing" = "#FC8D62",
  "Decreasing Batt Cap Cell Manufacturing" = "#FDD0B5",
  "End of Life Batteries" = "#66A61E",
  "Decreasing Batt Cap End of Life Batteries" = "#C7E9A8",
  "Black Mass" = "#000000",
  "Refining" = "#FFD700"
)

origin_colors <- origin_colors[names(origin_colors) %in% unique(Mass_2050_projected_ref$Origin)]

# Set the factor order to match your color vector order
Mass_2050_projected_ref <- Mass_2050_projected_ref %>%
  mutate(Origin = factor(Origin, levels = names(origin_colors))) %>%
  complete(
    State_Province = ca_us_prov_state_grid1$code,
    Origin,
    fill = list(`Metric Tonnes (millions)` = 0)
  )


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
    row = 8 ,  # position it below existing rows
    col = 5  # adjust column as you like
  )

# restore geofacet_grid class
class(grid_df) <- c("geofacet_grid", "data.frame")

# assign back
ca_us_prov_state_grid1 <- grid_df
library(scales)

##state plot
ggplot(
  Mass_2050_projected_ref,
  aes(x = Origin, y = `Metric Tonnes (millions)`, fill = Origin)) +
  geom_col() +
  facet_geo(~ State_Province, grid = ca_us_prov_state_grid1) +
  scale_fill_manual(values = origin_colors) +
  labs(
    title = "North American Battery Demand, Manufacturing and Recycling Tonnage (2050)",
    y = "Metric Tonnes (millions)",
    x = "Supply Chain Segment (Increasing Battery Capacity and Benchmark Chemistry Projections)"
  ) +
  theme_minimal(base_size = 16) +
  theme(
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    legend.position = "bottom",
    plot.title = element_text(size = 20, face = "bold", hjust = 0.5),
    axis.title = element_text(size = 16, face = "bold"),
    axis.text = element_text(size = 14),
    strip.text = element_text(size = 16),
    legend.title = element_text(size = 14, face = "bold"),
    legend.text = element_text(size = 14),
    panel.grid.major = element_line(color = "grey70", linewidth = 0.6),
    panel.grid.minor = element_line(color = "grey80", linewidth = 0.4)
  )

##regions plot
region_grid <- data.frame(
  code = c("US-West", "US-Mountain", "US-Midwest", "US-South", "US-East", "Canada-West", "Canada-Mountain", "Canada-Midwest", "Canada-East", "Mexico"),
  name = c("US West", "US Mountain", "US Midwest", "US South", "US East", "Canada West", "Canada Mountain", "Canada Midwest", "Canada East", "Mexico"),
  col = c(1, 2, 3, 4, 5, 1, 2, 3, 4, 5),
  row = c(1, 1, 1, 1, 1, 2, 2, 2, 2, 2)
)

# Convert Region to a factor
Mass_2050_projected_ref <- Mass_2050_projected_ref %>%
  mutate(Region = factor(Region, levels = c("US-West", "US-Mountain", "US-Midwest", "US-South", "US-East", "Canada-West", "Canada-Mountain", "Canada-Midwest", "Canada-East", "Mexico")))

ggplot(
  Mass_2050_projected_ref,
  aes(x = Origin, y = `Metric Tonnes (millions)`, fill = Origin)) +
  geom_col() +
  facet_geo(~ Region, grid = region_grid) +
  scale_fill_manual(values = origin_colors) +
  labs(
    title = "North American Battery Demand, Manufacturing and Recycling Tonnage by Region (2050)",
    y = "Metric Tonnes (millions)",
    x = "Supply Chain Segment (Increasing Battery Capacity and Benchmark Chemistry Projections)"
  ) +
  theme_minimal(base_size = 20) +
  theme(
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    legend.position = "bottom",
    plot.title = element_text(size = 24, face = "bold", hjust = 0.5),
    axis.title = element_text(size = 20, face = "bold"),
    axis.text = element_text(size = 20),
    strip.text = element_text(size = 20),
    legend.title = element_text(size =20, face = "bold"),
    legend.text = element_text(size = 20),
    panel.grid.major = element_line(color = "grey70", linewidth = 0.6),
    panel.grid.minor = element_line(color = "grey80", linewidth = 0.4)
  )






## JUST PLUG IN DELAYED OR NOT
## National compare scenarios 2035
Nat_Mass_2050 <- Mass_2050_projected %>%
  group_by(Year, Country) %>%                        
  summarise(
    Add_LIB_proj_tonnes = sum(Add_LIB_proj_tonnes, na.rm = TRUE), 
    Add_LIB_15_tonnes = sum(Add_LIB_15_tonnes, na.rm = TRUE),
    #Add_LIB_proj_LFP_tonnes = sum(Add_LIB_proj_LFP_tonnes, na.rm = TRUE),
    #Add_LIB_15_LFP_tonnes = sum(Add_LIB_15_LFP_tonnes, na.rm = TRUE),
    Tonnes_Prod_proj_down = sum(Tonnes_Prod_proj_down, na.rm = TRUE),
    Tonnes_Prod_15_down = sum(Tonnes_Prod_15_down, na.rm = TRUE),
    Tonnes_Prod_proj_mid = sum(Tonnes_Prod_proj_mid, na.rm = TRUE),
    Tonnes_Prod_15_mid = sum(Tonnes_Prod_15_mid, na.rm = TRUE),
    Recycle_Batt_Proj = sum(Recycle_Batt_Proj, na.rm = TRUE),
    Recycle_Batt_15 = sum(Recycle_Batt_15, na.rm = TRUE),
    #Recycle_Batt_Proj_LFP = sum(Recycle_Batt_Proj_LFP, na.rm = TRUE),
    #Recycle_Batt_15_LFP = sum(Recycle_Batt_15_LFP, na.rm = TRUE),
    Cumulative_black_mass_cap = sum(Cumulative_black_mass_cap, na.rm = TRUE),
    Cumulative_refining_cap = sum(Cumulative_refining_cap, na.rm = TRUE),
    .groups = "drop"  # <-- make sure this is after all commas
  ) %>% 
  rename("LIB Demand (Increasing Batt Cap - Benchmark Chemistry)" = Add_LIB_proj_tonnes,
         "LIB Demand (Decreasing Batt Cap - Benchmark Chemistry)" = Add_LIB_15_tonnes, 
         #"LIB Demand (Increasing Batt Cap - High LFP)" = Add_LIB_proj_LFP_tonnes,
         #"LIB Demand (Decreasing Batt Cap  - High LFP)" = Add_LIB_15_LFP_tonnes,
         "Pack Manufacturing" = Tonnes_Prod_proj_down,
         "Decreasing Batt Cap Pack Manufacturing" = Tonnes_Prod_15_down,
         "Cell Manufacturing" = Tonnes_Prod_proj_mid,
         "Decreasing Batt Cap Cell Manufacturing" = Tonnes_Prod_15_mid,
         "EoL Batteries (Increasing Batt Cap - Benchmark Chemistry)" = Recycle_Batt_Proj,
         "EoL Batteries (Decreasing Batt Cap - Benchmark Chemistry)" = Recycle_Batt_15, 
         #"EoL Batteries (Increasing Batt Cap - High LFP)" = Recycle_Batt_Proj_LFP,
         #"EoL Batteries (Decreasing Batt Cap  - High LFP)" = Recycle_Batt_15_LFP,
         "Black Mass" = Cumulative_black_mass_cap,
         "Refining" = Cumulative_refining_cap)


Nat_Mass_2050_long <- Nat_Mass_2050 %>% 
  pivot_longer(
    cols = -c(Year, Country),           # keep Year as a separate column
    names_to = "Metric",    # column that stores the original column names
    values_to = "Tonnes"    # column that stores values
  ) %>% select(-Year) %>%
  mutate(Tonnes = Tonnes/1e6,
         Metric = factor(Metric, levels = c(
           "LIB Demand (Increasing Batt Cap - Benchmark Chemistry)",
           "LIB Demand (Decreasing Batt Cap - Benchmark Chemistry)",
           #"LIB Demand (Increasing Batt Cap - High LFP)",
           #"LIB Demand (Decreasing Batt Cap  - High LFP)",
           "Pack Manufacturing",
           "Decreasing Batt Cap Pack Manufacturing",
           "Cell Manufacturing",
           "Decreasing Batt Cap Cell Manufacturing",
           "EoL Batteries (Increasing Batt Cap - Benchmark Chemistry)",
           "EoL Batteries (Decreasing Batt Cap - Benchmark Chemistry)", 
           #"EoL Batteries (Increasing Batt Cap - High LFP)",
           #"EoL Batteries (Decreasing Batt Cap  - High LFP)",
           "Black Mass",
           "Refining"
         ))                                      # keep your desired order
  ) %>%
  mutate(
    pattern_type = case_when(
      Country == "US" ~ "diagonal",
      Country == "CA" ~ "stripe",
      Country == "MX" ~ "circle"   # dotted look
    )
  ) %>%
  mutate(
    Country = factor(Country, levels = c("CA", "US", "MX"))
  )

##country plot
ggplot(Nat_Mass_2050_long, aes(x = Metric, y = Tonnes, fill = Metric, pattern = Country)) +
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
  scale_fill_manual(
    values = c(
      "LIB Demand (Increasing Batt Cap - Benchmark Chemistry)" = "#1b7fb3",
      "LIB Demand (Decreasing Batt Cap - Benchmark Chemistry)" = "#6ba8d4",
      "EoL Batteries (Increasing Batt Cap - Benchmark Chemistry)" = "#66A61E",
      "EoL Batteries (Decreasing Batt Cap - Benchmark Chemistry)" = "#C7E9A8",
      "Pack Manufacturing" = "#D77FBF",
      "Decreasing Batt Cap Pack Manufacturing" = "#EEC3DE",
      "Cell Manufacturing" = "#FC8D62",
      "Decreasing Batt Cap Cell Manufacturing" = "#FDD0B5",
      "Black Mass" = "#808080",
      "Refining" = "#E6AB02"
    ),
    labels = function(x) stringr::str_wrap(x, width = 30)
  ) +
  guides(
    fill = guide_legend(
      override.aes = list(pattern = "none"),
      nrow = 2,
      byrow = FALSE
    ),
    pattern = guide_legend(
      title = "Country",
      nrow = 1,
      byrow = TRUE,
      override.aes = list(
        fill = "white",
        color = "black"
      )
    )
  ) +
  scale_y_continuous(labels = scales::comma) +
  labs(
    x = "Supply Chain Segment (Battery Capacity - Chemistry Scenario)",
    y = "Metric Tonnes Batteries (millions)",
    fill = NULL,
    title = "North American Demand, Manufacturing and Recycling Tonnage by Country (2050)"
  ) +
  theme_minimal(base_size = 20) +
  theme(
    legend.box = "vertical",
    legend.box.just = "left",
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    legend.position = "bottom",
    plot.title = element_text(size = 24, face = "bold", hjust = 0.5),
    axis.title = element_text(size = 20, face = "bold"),
    axis.text = element_text(size = 20),
    strip.text = element_text(size = 20, face = "bold"),
    legend.title = element_text(size = 20, face = "bold"),
    legend.text = element_text(size = 15)
  )

## all in millions
NA_plot_data <- NA_demand_tonnes %>% 
  full_join(NA_manu, by = "Year") %>%
  full_join(NA_batts, by = "Year") %>%
  full_join(NA_recycling_tonnes, by = "Year") %>%
  select(-Tonnes_Scrap_proj_down, -Tonnes_Scrap_15_down, -Tonnes_Scrap_proj_mid, 
         -Tonnes_Scrap_15_mid, -Recycle_Batt_15_LFP, -Recycle_Batt_Proj_LFP, -Add_LIB_proj_LFP_tonnes, -Add_LIB_15_LFP_tonnes) %>%
  rename("LIB Demand (Increasing Batt Cap - Benchmark Chemistry)" = Add_LIB_proj_tonnes,
         "LIB Demand (Decreasing Batt Cap - Benchmark Chemistry)" = Add_LIB_15_tonnes,
         "Pack Manufacturing" = Tonnes_Prod_proj_down,
         "Decreasing Batt Cap Pack Manufacturing" = Tonnes_Prod_15_down,
         "Cell Manufacturing" = Tonnes_Prod_proj_mid,
         "Decreasing Batt Cap Cell Manufacturing" = Tonnes_Prod_15_mid,
         "EoL Batteries (Increasing Batt Cap - Benchmark Chemistry)" = Recycle_Batt_Proj,
         "EoL Batteries (Decreasing Batt Cap - Benchmark Chemistry)" = Recycle_Batt_15, 
         "Black Mass" = Cumulative_black_mass_cap,
         "Refining" =Cumulative_refining_cap) %>%
  pivot_longer(
    cols = -c(Year),           
    names_to = "Metric",    
    values_to = "Tonnes"
  ) %>%
  mutate(
    Tonnes = Tonnes / 1e6,
    Metric = factor(Metric, levels = c(
      "LIB Demand (Increasing Batt Cap - Benchmark Chemistry)",
      "LIB Demand (Decreasing Batt Cap - Benchmark Chemistry)",
      "Pack Manufacturing",
      "Decreasing Batt Cap Pack Manufacturing",
      "Cell Manufacturing",
      "Decreasing Batt Cap Cell Manufacturing",
      "EoL Batteries (Increasing Batt Cap - Benchmark Chemistry)",
      "EoL Batteries (Decreasing Batt Cap - Benchmark Chemistry)", 
      "Black Mass",
      "Refining"
    ))) 



NA_plot_data$Year <- as.numeric(NA_plot_data$Year)
NA_plot_data <- NA_plot_data %>% filter(Year >= 2025)


##overtime data
ggplot(NA_plot_data, aes(x = Year, y = Tonnes, color = Metric, linetype = Metric)) +
  geom_line(linewidth = 2) +
  scale_color_manual(
    values = c(
      "LIB Demand (Increasing Batt Cap - Benchmark Chemistry)" = "#1b7fb3",
      "LIB Demand (Decreasing Batt Cap - Benchmark Chemistry)" = "#1b7fb3",
      "EoL Batteries (Increasing Batt Cap - Benchmark Chemistry)" = "#66A61E",
      "EoL Batteries (Decreasing Batt Cap - Benchmark Chemistry)" = "#66A61E",
      "Pack Manufacturing" = "#D77FBF",
      "Decreasing Batt Cap Pack Manufacturing" = "#D77FBF",
      "Cell Manufacturing" = "#FC8D62",
      "Decreasing Batt Cap Cell Manufacturing" = "#FC8D62",
      "Black Mass" = "#808080",
      "Refining" = "#E6AB02"
    ),
    labels = function(x) stringr::str_wrap(x, width = 30)
  ) +
  scale_linetype_manual(
    values = c(
      "LIB Demand (Increasing Batt Cap - Benchmark Chemistry)" = "solid",
      "LIB Demand (Decreasing Batt Cap - Benchmark Chemistry)" = "dashed",
      "EoL Batteries (Increasing Batt Cap - Benchmark Chemistry)" = "solid",
      "EoL Batteries (Decreasing Batt Cap - Benchmark Chemistry)" = "dashed",
      "Pack Manufacturing" = "solid",
      "Decreasing Batt Cap Pack Manufacturing" = "dashed",
      "Cell Manufacturing" = "solid",
      "Decreasing Batt Cap Cell Manufacturing" = "dashed",
      "Black Mass" = "solid",
      "Refining" = "solid"
    ),
    labels = function(x) stringr::str_wrap(x, width = 30)
  ) +
  guides(
    color = guide_legend(
      nrow = 5,
      ncol = 2,
      byrow = TRUE
    )
  ) +
  labs(
    x = "Year",
    y = "Metric Tonnes Batteries (millions)",
    title = "North American Demand, Manufacturing and Recycling Tonnage Over Time",
  ) +
  theme_minimal(base_size = 20) +
  theme(
    legend.box = "vertical",
    legend.position = "bottom",
    plot.title = element_text(size = 24, face = "bold", hjust = 0.5),
    axis.title = element_text(size = 20, face = "bold"),
    axis.text = element_text(size = 20),
    strip.text = element_text(size = 20, face = "bold"),
    legend.text = element_text(size = 14),
    legend.key.width = unit(2.5, "cm")
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

