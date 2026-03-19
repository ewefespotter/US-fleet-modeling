library(openxlsx)
library(ggpattern)
library(readxl)
library(readr)
library(tidyverse)

library(ggplot2)
library(geofacet)

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

Mass_2050_projected_ref <- Mass_2050_projected_ref %>%
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
    row = max(grid_df$row, na.rm = TRUE) + 1,  # position it below existing rows
    col = 4  # adjust column as you like
  )

# restore geofacet_grid class
class(grid_df) <- c("geofacet_grid", "data.frame")

# assign back
ca_us_prov_state_grid1 <- grid_df
library(scales)

ggplot(
  Mass_2050_projected_ref,
  aes(x = Origin, y = `Metric Tonnes (millions)`, fill = Origin)) +
  geom_col() +
  facet_geo(~ State_Province, grid = ca_us_prov_state_grid1) +
  scale_fill_manual(values = origin_colors) +
  labs(
    title = "North American Battery Demand, Manufacturing and Recycling Tonnage (2050)",
    y = "Metric Tonnes (millions)",
    x = "Supply Chain Segment (Baseline Battery Capacity and Original Chemistry Projections)"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    legend.position = "bottom",
    plot.title = element_text(size = 18, face = "bold", hjust = 0.5),
    axis.title = element_text(size = 14, face = "bold"),
    axis.text = element_text(size = 12),
    strip.text = element_text(size = 14),
    legend.title = element_text(size = 12, face = "bold"),
    legend.text = element_text(size = 11),
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
  rename("Demand (Baseline Capacity - Original Chemistry)" = Add_LIB_proj_tonnes,
         "Demand (15% Lower Batt Cap - Original Chemistry)" = Add_LIB_15_tonnes, 
         #"Demand (Baseline Capacity - High LFP)" = Add_LIB_proj_LFP_tonnes,
         #"Demand (15% Lower Batt Cap  - High LFP)" = Add_LIB_15_LFP_tonnes,
         "Pack Manufacturing" = Tonnes_Prod_proj_down,
         "15% Lower Batt Cap Pack Manufacturing" = Tonnes_Prod_15_down,
         "Cell Manufacturing" = Tonnes_Prod_proj_mid,
         "15% Lower Batt Cap Cell Manufacturing" = Tonnes_Prod_15_mid,
         "EOL Batteries (Baseline Capacity - Original Chemistry)" = Recycle_Batt_Proj,
         "EOL Batteries (15% Lower Batt Cap - Original Chemistry)" = Recycle_Batt_15, 
         #"EOL Batteries (Baseline Capacity - High LFP)" = Recycle_Batt_Proj_LFP,
         #"EOL Batteries (15% Lower Batt Cap  - High LFP)" = Recycle_Batt_15_LFP,
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
           "Demand (Baseline Capacity - Original Chemistry)",
           "Demand (15% Lower Batt Cap - Original Chemistry)",
           #"Demand (Baseline Capacity - High LFP)",
           #"Demand (15% Lower Batt Cap  - High LFP)",
           "Pack Manufacturing",
           "15% Lower Batt Cap Pack Manufacturing",
           "Cell Manufacturing",
           "15% Lower Batt Cap Cell Manufacturing",
           "EOL Batteries (Baseline Capacity - Original Chemistry)",
           "EOL Batteries (15% Lower Batt Cap - Original Chemistry)", 
           #"EOL Batteries (Baseline Capacity - High LFP)",
           #"EOL Batteries (15% Lower Batt Cap  - High LFP)",
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


ggplot(Nat_Mass_2050_long, aes(x = Metric, y = Tonnes, fill = Metric, pattern = Country)) +
  geom_col_pattern(
    position = "stack",
    color = "black",
    pattern_density = 0.1,
    pattern_spacing = 0.05,
    pattern_alpha = 0.3,
    pattern_size = 0.2,
    pattern_fill = "black"
  ) +
  scale_pattern_manual(
    values = c(
      "US" = "none",
      "CA" = "stripe",
      "MX" = "circle"
    )
  ) +
  scale_y_continuous(labels = comma) +
  labs(
    x = "Supply Chain Segment (Battery Capacity - Chemistry Scenario)",
    y = "Metric Tonnes Batteries (millions)",
    fill = NULL,
    title = "North American Demand, Manufacturing and Recycling Tonnage by Country (2050)"
  ) +
  scale_fill_manual(
    values = c(
      # Demand — Original Chemistry
      "Demand (Baseline Capacity - Original Chemistry)" = "#1b9e77",   # teal
      "Demand (15% Lower Batt Cap - Original Chemistry)" = "#b2dfdb", # light teal
      
      # EOL — High LFP
      "EOL Batteries (Baseline Capacity - Original Chemistry)" = "#66A61E",             # olive
      "EOL Batteries (15% Lower Batt Cap - Original Chemistry)" = "#C7E9A8",           # light olive
      
      # Pack Manufacturing — slightly more purple
      "Pack Manufacturing" = "#D77FBF",                 # purple-pink
      "15% Lower Batt Cap Pack Manufacturing" = "#EEC3DE",  # light purple-pink
      
      # Cell Manufacturing — coral
      "Cell Manufacturing" = "#FC8D62",                                # coral
      "15% Lower Batt Cap Cell Manufacturing" = "#FDD0B5",          # light coral
      
      # Recycling / Refining
      "Black Mass" = "#808080",                                        # black
      "Refining" = "#E6AB02"                                           # golden amber
    )
  ) +
  geom_col_pattern(
    position = "stack",
    color = "black",
    
    # 🔑 key fixes
    pattern_density = 0.5,     # ↓ much less dense
    pattern_spacing = 0.008,    # ↑ more space between stripes
    pattern_alpha = 0.3,       # ↓ more transparent pattern
    pattern_size = 0.2,        # ↓ thinner lines
    
    pattern_fill = "black"
  ) +
  theme_minimal(base_size = 16) +
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




library(ggplot2)
library(ggpattern)
library(dplyr)
library(scales)

Nat_Mass_2050_long <- Nat_Mass_2050 %>% 
  pivot_longer(
    cols = -c(Year, Country),           
    names_to = "Metric",    
    values_to = "Tonnes"
  ) %>%
  select(-Year) %>%
  mutate(
    Tonnes = Tonnes / 1e6,
    Metric = factor(Metric, levels = c(
      "Demand (Baseline Capacity - Original Chemistry)",
      "Demand (15% Lower Batt Cap - Original Chemistry)",
      #"Demand (Baseline Capacity - High LFP)",
      #"Demand (15% Lower Batt Cap  - High LFP)",
      "Pack Manufacturing",
      "15% Lower Batt Cap Pack Manufacturing",
      "Cell Manufacturing",
      "15% Lower Batt Cap Cell Manufacturing",
      "EOL Batteries (Baseline Capacity - Original Chemistry)",
      "EOL Batteries (15% Lower Batt Cap - Original Chemistry)", 
      #"EOL Batteries (Baseline Capacity - High LFP)",
      #"EOL Batteries (15% Lower Batt Cap  - High LFP)",
      "Black Mass",
      "Refining"
    )),
    Country = factor(Country, levels = c("CA", "US", "MX"))  # ensure stacking order
  )

ggplot(Nat_Mass_2050_long, aes(x = Metric, y = Tonnes, fill = Metric, pattern = Country)) +
  geom_col_pattern(
    position = "stack",
    color = "black",
    
    # pattern settings
    pattern_density = 0.2,
    pattern_spacing = 0.05,
    pattern_alpha = 0.3,
    pattern_size = 0.2,
    pattern_fill = "black"
  ) +
  scale_pattern_manual(
    values = c(
      "US" = "circle",     # US bars stay solid
      "CA" = "stripe",   # CA has stripes
      "MX" = "crosshatch"    # MX has dots
    )
  ) +
  scale_fill_manual(
    values = c(
      "Demand (Baseline Capacity - Original Chemistry)" = "#1b9e77",
      "Demand (15% Lower Batt Cap - Original Chemistry)" = "#b2dfdb",
      "EOL Batteries (Baseline Capacity - Original Chemistry)" = "#66A61E",
      "EOL Batteries (15% Lower Batt Cap - Original Chemistry)" = "#C7E9A8",
      "Pack Manufacturing" = "#D77FBF",
      "15% Lower Batt Cap Pack Manufacturing" = "#EEC3DE",
      "Cell Manufacturing" = "#FC8D62",
      "15% Lower Batt Cap Cell Manufacturing" = "#FDD0B5",
      "Black Mass" = "#808080",
      "Refining" = "#E6AB02"
    ),
    labels = function(x) stringr::str_wrap(x, width = 30)  # <- key line
  ) +
  guides(
    fill = guide_legend(
      override.aes = list(pattern = "none"),
      title = "Metric",
      nrow = 2,
      byrow = FALSE
    ),
    pattern = guide_legend(
      title = "Country",
      nrow = 1,
      byrow = TRUE
    )
  ) +
  theme(
    legend.box = "vertical"      # stack Metric above Country
  ) +
  scale_y_continuous(labels = comma) +
  labs(
    x = "Supply Chain Segment (Battery Capacity - Chemistry Scenario)",
    y = "Metric Tonnes Batteries (millions)",
    fill = NULL,
    title = "North American Demand, Manufacturing and Recycling Tonnage by Country (2050)"
  ) +
  theme_minimal(base_size = 16) +
  theme(
    legend.box = "vertical",   # Metric on top, Country below
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    legend.position = "bottom",
    plot.title = element_text(size = 18, face = "bold", hjust = 0.5),
    axis.title = element_text(size = 14, face = "bold"),
    axis.text = element_text(size = 12),
    strip.text = element_text(size = 14, face = "bold"),
    legend.title = element_text(size = 12, face = "bold"),
    legend.text = element_text(size = 10)
  )



NA_plot_data <- NA_demand_tonnes %>% 
  full_join(NA_manu, by = "Year") %>%
  full_join(NA_batts, by = "Year") %>%
  full_join(NA_recycling_tonnes, by = "Year") %>%
  select(-Tonnes_Scrap_proj_down, -Tonnes_Scrap_15_down, -Tonnes_Scrap_proj_mid, 
         -Tonnes_Scrap_15_mid, -Recycle_Batt_15_LFP, -Recycle_Batt_Proj_LFP) %>%
  rename("Demand (Baseline Capacity - Original Chemistry)" = Add_LIB_proj_tonnes,
         "Demand (15% Lower Batt Cap - Original Chemistry)" = Add_LIB_15_tonnes,
         "Pack Manufacturing" = Tonnes_Prod_proj_down,
         "15% Lower Batt Cap Pack Manufacturing" = Tonnes_Prod_15_down,
         "Cell Manufacturing" = Tonnes_Prod_proj_mid,
         "15% Lower Batt Cap Cell Manufacturing" = Tonnes_Prod_15_mid,
         "EOL Batteries (Baseline Capacity - Original Chemistry)" = Recycle_Batt_Proj,
         "EOL Batteries (15% Lower Batt Cap - Original Chemistry)" = Recycle_Batt_15, 
         "Black Mass" = Cumulative_black_mass_cap,
         "Refining" = Cumulative_refining_cap) %>%
  pivot_longer(
    cols = -c(Year),           
    names_to = "Metric",    
    values_to = "Tonnes"
  ) %>%
  mutate(
    Tonnes = Tonnes / 1e6,
    Metric = factor(Metric, levels = c(
      "Demand (Baseline Capacity - Original Chemistry)",
      "Demand (15% Lower Batt Cap - Original Chemistry)",
      "Pack Manufacturing",
      "15% Lower Batt Cap Pack Manufacturing",
      "Cell Manufacturing",
      "15% Lower Batt Cap Cell Manufacturing",
      "EOL Batteries (Baseline Capacity - Original Chemistry)",
      "EOL Batteries (15% Lower Batt Cap - Original Chemistry)", 
      "Black Mass",
      "Refining"
    ))) 
  
NA_plot_data$Year <- as.numeric(NA_plot_data$Year)

ggplot(NA_plot_data, aes(x = Year, y = Tonnes, color = Metric)) +
  geom_line(linewidth = 2) +
  scale_color_manual(
    values = c(
      "Demand (Baseline Capacity - Original Chemistry)" = "#1b9e77",
      "Demand (15% Lower Batt Cap - Original Chemistry)" = "#b2dfdb",
      "EOL Batteries (Baseline Capacity - Original Chemistry)" = "#66A61E",
      "EOL Batteries (15% Lower Batt Cap - Original Chemistry)" = "#C7E9A8",
      "Pack Manufacturing" = "#D77FBF",
      "15% Lower Batt Cap Pack Manufacturing" = "#EEC3DE",
      "Cell Manufacturing" = "#FC8D62",
      "15% Lower Batt Cap Cell Manufacturing" = "#FDD0B5",
      "Black Mass" = "#808080",
      "Refining" = "#E6AB02"
    ),
    labels = function(x) stringr::str_wrap(x, width = 30)
  ) +
  guides(
    color = guide_legend(
      title = "Metric",
      nrow = 2,
      byrow = FALSE
    )
  ) +
  labs(
    x = "Year",
    y = "Metric Tonnes Batteries (millions)",
    title = "North American Demand, Manufacturing and Recycling Tonnage Over Time"
  ) +
  theme_minimal(base_size = 18) +
  theme(
    legend.box = "vertical",   # Metric on top, Country below
    legend.position = "bottom",
    plot.title = element_text(size = 20, face = "bold", hjust = 0.5),
    axis.title = element_text(size = 16, face = "bold"),
    axis.text = element_text(size = 14),
    strip.text = element_text(size = 16, face = "bold"),
    legend.title = element_text(size = 14, face = "bold"),
    legend.text = element_text(size = 12)
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

