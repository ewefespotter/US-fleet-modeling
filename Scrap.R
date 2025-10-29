library(dplyr)
library(purrr)
library(readxl)
library(dplyr)
library(tidyr)
library(stringr)
library(tidyverse)
library(ggplot2)

## either direction-- need info about the chemistry - if %scrap of gwh then to kg/gwh
## or need 

##from bat_pac then distribute based on battery type 

##ignoring the scrap rate itself bc it varies by process and don't have process specific data for the plants
##get a % increase in production of each type from planned ones for each batt type
## x kg/yr scrap based on y gwh/year so can increase to z gwh/year 

## implicit kg/gwh
## increase kg/gwh for a few yrs for additional
## arrive at new kg/yr


scrap <- read_excel("/Users/elsawefes-potter/Documents/Critical_Minerals_Pablo/Online NAATBatt Lithium-Ion Battery Supply Chain Database  Transportation and Mobility Research  NREL (1).xlsx")
header.true <- function(df) {
  names(df) <- as.character(unlist(df[1,]))
  df[-1,]
}
scrap<-header.true(scrap)


open <- scrap %>% filter(Status != "Cancelled") %>% filter(Status == "Commercial")
plan <- scrap %>% filter(Status != "Cancelled") %>% filter(Status != "Commercial") %>% filter(Status != "Paused construction")

mineral_intensity <- read_excel(("/Users/elsawefes-potter/Documents/Critical_Minerals_Pablo/Mineral_Intensity(2).xlsx"), na = "")
bat_pac <- read_csv("/Users/elsawefes-potter/Documents/Critical_Minerals_Pablo/BatPac.csv")
bat_pac <- bat_pac %>% select(where(~ !all(is.na(.)))) 
colnames(bat_pac) <- c("Product_Abbrev", "Mineral", "Value")

bat_pac <- bat_pac %>% mutate(`kg/Gwh` = Value/50000000*1000000) #(kg/yr) *1/(50000000 kwh/yr) * 1000000 kwh/Gwh

mineral_map <- c(
  "Li, kg/yr" = "Lithium",
  "Ni, kg/yr" = "Nickel",
  "Co, kg/yr" = "Cobalt",
  "Mn, kg/yr" = "Manganese",
  "C, kg/yr"  = "Carbon",
  "Al, kg/yr" = "Aluminum",
  "Cu, kg/yr" = "Copper"
)
bat_pac <- bat_pac %>%
  mutate(Mineral = recode(Mineral, !!!mineral_map))

# # Now pivot wider to have minerals as columns
# bat_pac_wide <- bat_pac %>%
#   mutate(Mineral = recode(Mineral, !!!mineral_map)) %>%   # replace short names with full names
#   pivot_wider(names_from = Mineral, values_from = `kg/Gwh`) %>%
#   select(Product_Abbrev, all_of(unname(mineral_map)))   


###sidebar getting batpac for NMCA from ratios for NMC 622 and NMCA in min intensity

nmca_nmc <- mineral_intensity %>%
  filter(chemistry %in% c("NMCA 89:4:4:3", "NMC 622"))

summary_chem <- nmca_nmc %>%
  group_by(chemistry, Mineral) %>%
  summarise(
    total_kg_per_kwh = sum(kg_per_kwh, na.rm = TRUE),
    .groups = "drop"
  )

# Pivot wider and calculate ratio per mineral
ratio_nmca_nmc <- summary_chem %>%
  pivot_wider(
    names_from = chemistry,
    values_from = total_kg_per_kwh
  ) %>%
  mutate(
    ratio = `NMCA 89:4:4:3` / `NMC 622`
  )

bat_pac <- bat_pac %>%
  mutate(Product_Abbrev = str_trim(Product_Abbrev))  # remove extra spaces

# Make sure Value and ratio are numeric
bat_pac <- bat_pac %>%
  mutate(`kg/Gwh` = as.numeric(`kg/Gwh`))

ratio_nmca_nmc <- ratio_nmca_nmc %>%
  mutate(ratio = as.numeric(ratio))


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
bat_pac <- bind_rows(bat_pac, nmca_rows)

### open vs planned
state_map <- c(
  AL = "Alabama", AK = "Alaska", AZ = "Arizona", AR = "Arkansas",
  CA = "California", CO = "Colorado", CT = "Connecticut", DE = "Delaware",
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
  WI = "Wisconsin", WY = "Wyoming"
)

# Map abbreviations to full names
open$State_Province_Full <- state_map[as.character(open[["State/ Province"]])]

# Remove rows with no match
open <- open[!is.na(open$State_Province_Full), ]
open$`Production Capacity` <- gsub("–", NA, open$`Production Capacity`, fixed = TRUE)

open$product <- as.character(open$Product)
#open <- open[!is.na(open$`Production Capacity`),]
remove_products <- c(
  "Kavian™ 3D printing platform; Cypress® Li-Metal battery cells",
  "Specialized li-ion batteries for implantable medical devices",
  "Electrode",
  "Iron-air batteries",
  "Sealed Bipolar Lead Acid",
  "Silicon-based anode materials",
  "Solid-state battery",
  "Silicon nanowire anode",
  "Other/ Unknown",
  "Cell"
)

open <- open %>%
  mutate(
    # Replace “–” and "NA" strings with actual NA
    `Production Capacity` = na_if(`Production Capacity`, "–"),
    `Production Capacity` = na_if(`Production Capacity`, "NA"),
    
    # Extract numeric values and convert units
    cap_num = case_when(
      str_detect(`Production Capacity`, "GWh") ~ as.numeric(str_extract(`Production Capacity`, "[0-9.]+")),
      str_detect(`Production Capacity`, "MWh") ~ as.numeric(str_extract(`Production Capacity`, "[0-9.]+")) / 1000,
      TRUE ~ NA_real_  # ignore other entries
    )
  )

n_open_before <- nrow(open)
prod_open_before <- sum(open$cap_num, na.rm = TRUE)
na_open_before <- sum(is.na(open$cap_num))
print(na_open_before)
print(prod_open_before)
print(n_open_before)
# Filter out those products (case-insensitive)
open <- open %>%
  filter(!tolower(Product) %in% tolower(remove_products))
n_open_after <- nrow(open)
na_open_after <- sum(is.na(open$cap_num))
prod_open_after <- sum(open$cap_num, na.rm = TRUE)
print(na_open_after)
print(n_open_after)
print(prod_open_after)


###For those that have clear chemistries--> get total current capacity of that chemistry 
open <- open %>%
  mutate(Product_Abbrev = case_when(
    str_detect(Product, regex("NMCA", ignore_case = TRUE)) ~ "NMCA",
    str_detect(Product, regex("Nickel.*Manganese.*Cobalt|NMC", ignore_case = TRUE)) ~ "NMC",
    str_detect(Product, regex("Nickel.*Cobalt.*Manganese|Nickel cobalt manganese oxide", ignore_case = TRUE)) ~ "NMC",
    str_detect(Product, regex("Lithium.*Iron.*Phosphate|LFP", ignore_case = TRUE)) ~ "LFP",
    str_detect(Product, regex("Nickel.*Cobalt.*Aluminum|NCA", ignore_case = TRUE)) ~ "NCA",
    str_detect(Product, regex("Lithium titanium oxide", ignore_case = TRUE)) ~ "LTO",
    TRUE ~ NA_character_
  ))

open <- open %>%
  group_by(Product_Abbrev) %>%
  mutate(
    avg_cap = mean(cap_num, na.rm = TRUE),
    cap_num = if_else(is.na(cap_num), avg_cap, cap_num)
  ) %>%
  ungroup() %>%
  select(-avg_cap) %>% 
  mutate(
    Product_Abbrev = if_else(Product_Abbrev == "NMC", "NMC 622", Product_Abbrev),
    Product_Abbrev = if_else(Product_Abbrev == "LTO", "LMO-LTO", Product_Abbrev)
  )

years = 2025:2050

open_scrap_output <- open %>%
  inner_join(bat_pac, by = "Product_Abbrev", relationship = "many-to-many") %>%
  mutate(`kg/yr` = `kg/Gwh`*cap_num) %>%
  select(Mineral, State_Province_Full, `kg/yr`) %>%
  crossing(Year = years) %>%  
  group_by(Year, Mineral, State_Province_Full) %>%
  summarise(kg_open = sum(`kg/yr`, na.rm = TRUE), .groups ="drop") 

open_scrap_output <- as.data.frame(open_scrap_output)
num_cols <- sapply(open_scrap_output, is.numeric)

open_scrap_output[num_cols] <- lapply(open_scrap_output[num_cols], function(x) {
  x[is.nan(x) | is.na(x)] <- 0
  return(x)
})


##NOW PLANNED
# Map abbreviations to full names
plan$State_Province_Full <- state_map[as.character(plan[["State/ Province"]])]

# Remove rows with no match
plan <- plan[!is.na(plan$State_Province_Full), ]
plan$`Production Capacity` <- gsub("–", NA, plan$`Production Capacity`, fixed = TRUE)


plan$product <- as.character(plan$Product)
#open <- open[!is.na(open$`Production Capacity`),]
remove_products <- c(
  "Kavian™ 3D printing platform; Cypress® Li-Metal battery cells",
  "Specialized li-ion batteries for implantable medical devices",
  "Electrode",
  "Iron-air batteries",
  "Sealed Bipolar Lead Acid",
  "Silicon-based anode materials",
  "Solid-state battery",
  "Silicon nanowire anode",
  "Lithium-sulfur battery cells",
  "Rechargeable aqueous hybrid battery",
  "Flooded and AGM Batteries",
  "Nickel-zinc (NiZn) batteries",
  "Metal-free cathodes",
  "Silicon-based solid-state batteries",
  "Prismatic (pouch) cells",
  "Lithium-metal batteries",
  "Other/ Unknown",
  "Cell"
)

plan <- plan %>%
  mutate(
    # Replace “–” and "NA" strings with actual NA
    `Production Capacity` = na_if(`Production Capacity`, "–"),
    `Production Capacity` = na_if(`Production Capacity`, "NA"),
    
    # Extract numeric values and convert units
    cap_num = case_when(
      str_detect(`Production Capacity`, "GWh") ~ as.numeric(str_extract(`Production Capacity`, "[0-9.]+")),
      str_detect(`Production Capacity`, "MWh") ~ as.numeric(str_extract(`Production Capacity`, "[0-9.]+")) / 1000,
      TRUE ~ NA_real_  # ignore other entries
    )
  )


n_plan_before <- nrow(plan)
prod_plan_before <- sum(plan$cap_num, na.rm = TRUE)
na_plan_before <- sum(is.na(plan$cap_num))
print(na_plan_before)
print(prod_plan_before)
print(n_plan_before)



# Filter out those products (case-insensitive)
plan <- plan %>%
  filter(!tolower(Product) %in% tolower(remove_products))

n_plan_after <- nrow(plan)
na_plan_after <- sum(is.na(plan$cap_num))
prod_plan_after <- sum(plan$cap_num, na.rm = TRUE)
print(na_plan_after)
print(n_plan_after)
print(prod_plan_after)



###For those that have clear chemistries--> get total current capacity of that chemistry 
plan <- plan %>%
  mutate(Product_Abbrev = case_when(
    str_detect(Product, regex("NMCA", ignore_case = TRUE)) ~ "NMCA",
    str_detect(Product, regex("Nickel.*Manganese.*Cobalt|NMC", ignore_case = TRUE)) ~ "NMC",
    str_detect(Product, regex("Nickel.*Cobalt.*Manganese|Nickel cobalt manganese oxide", ignore_case = TRUE)) ~ "NMC",
    str_detect(Product, regex("Lithium.*Iron.*Phosphate|LFP", ignore_case = TRUE)) ~ "LFP",
    str_detect(Product, regex("Nickel.*Cobalt.*Aluminum|NCA", ignore_case = TRUE)) ~ "NCA",
    str_detect(Product, regex("Lithium titanium oxide", ignore_case = TRUE)) ~ "LTO",
    str_detect(Product, regex("Lithium cobalt oxide", ignore_case = TRUE)) ~ "LCO",
    TRUE ~ NA_character_
  ))


# Split rows where Product contains multiple products

# Split multi-product rows
multi_rows <- plan %>%
  filter(
    str_detect(Product, regex("Lithium cobalt oxide.*,.*Nickel.*Manganese.*Cobalt", ignore_case = TRUE)) |
    str_detect(Product, regex("Nickel manganese cobalt.*,.*Lithium.*Iron.*Phosphate", ignore_case = TRUE))
  ) %>%
  # Split Product into separate rows
  separate_rows(Product, sep = ",\\s*") %>%
  # Assign Product_Abbrev
  mutate(Product_Abbrev = case_when(
    str_detect(Product, regex("Lithium cobalt oxide", ignore_case = TRUE)) ~ "LCO",
    str_detect(Product, regex("Nickel.*Manganese.*Cobalt", ignore_case = TRUE)) ~ "NMC",
    str_detect(Product, regex("Lithium.*Iron.*Phosphate", ignore_case = TRUE)) ~ "LFP",
    TRUE ~ NA_character_
  )) %>%
  # Divide cap_num evenly based on how many rows came from the original
  group_by(row_id = row_number()) %>%
  mutate(cap_num = cap_num / 2) %>%
  ungroup() %>%
  select(-row_id)

# Keep all other rows
plan_keep <- plan %>%
  filter(
    !str_detect(Product, regex("Lithium cobalt oxide.*,.*Nickel.*Manganese.*Cobalt", ignore_case = TRUE)) &
    !str_detect(Product, regex("Nickel manganese cobalt.*,.*Lithium.*Iron.*Phosphate", ignore_case = TRUE))
  )

# Combine back
plan <- bind_rows(plan_keep, multi_rows) %>%
  arrange(Product_Abbrev)


plan <- plan %>%
  group_by(Product_Abbrev) %>%
  mutate(
    avg_cap = mean(cap_num, na.rm = TRUE),
    cap_num = if_else(is.na(cap_num), avg_cap, cap_num)
  ) %>%
  ungroup() %>%
  select(-avg_cap) %>% 
  mutate(
    Product_Abbrev = if_else(Product_Abbrev == "NMC", "NMC 622", Product_Abbrev),
    Product_Abbrev = if_else(Product_Abbrev == "LTO", "LMO-LTO", Product_Abbrev))


plan_scrap_output <- plan %>%
  inner_join(bat_pac, by = "Product_Abbrev", relationship = "many-to-many") %>%
  mutate(`kg/yr` = `kg/Gwh`*cap_num) %>%
  select(Mineral, State_Province_Full, Product_Abbrev, `kg/yr`, `Status`)


### NOW DO MULTIPLIERS
# Step 1: Create year range
years <- 2025:2050

make_multiplier <- function(start_year, years, duration = 4, peak = 3, base = 1) {
  multiplier <- rep(0, length(years))           # 0 before start
  start_index <- which(years == start_year)
  
  if (length(start_index) > 0) {
    end_index <- min(start_index + duration - 1, length(years))
    
    # ramp from peak to base over duration
    ramp <- seq(peak, base, length.out = end_index - start_index + 1)
    multiplier[start_index:end_index] <- ramp
    
    # years after ramp stay at base
    if (end_index < length(years)) {
      multiplier[(end_index + 1):length(years)] <- base
    }
  }
  
  return(multiplier)
}

# Step 3: Build dataset
multiplier_df <- data.frame(
  Year = years,
  under_construction = make_multiplier(start_year = 2027, years = years),
  planned = make_multiplier(start_year = 2030, years = years),
  start_up = make_multiplier(start_year = 2025, years = years)
)

plan_under_const <- plan_scrap_output %>%
  filter(Status == "Under construction")

plan_plan <-  plan_scrap_output %>%
  filter(Status == "Planned")

plan_start_up <- plan_scrap_output %>%
  filter(Status == "Pre-commercial/ startup") 


plan_under_const <- plan_under_const %>%
  rowwise() %>%
  mutate(Year = list(years[years >= 2027])) %>%  
  unnest(Year) %>%
  left_join(
    multiplier_df %>% select(Year, Multiplier = under_construction),
    by = "Year"
  ) %>%
  mutate(`kg/yr` = `kg/yr` * Multiplier) %>%  
  group_by(Year, Mineral, State_Province_Full) %>%
  summarise(kg_under_const = sum(`kg/yr`, na.rm = TRUE), .groups ="drop") 

# Planned
plan_plan <- plan_plan %>%
  rowwise() %>%
  mutate(Year = list(years[years >= 2030])) %>%  
  unnest(Year) %>%
  left_join(
    multiplier_df %>% select(Year, Multiplier = planned),
    by = "Year"
  ) %>%
  mutate(`kg/yr` = `kg/yr` * Multiplier) %>%  # apply multiplier
  group_by(Year, Mineral, State_Province_Full) %>%
  summarise(kg_plan = sum(`kg/yr`, na.rm = TRUE), .groups ="drop") 

# Start-up projects
plan_start_up <- plan_start_up %>%
  rowwise() %>%
  mutate(Year = list(years[years >= 2025])) %>%  
  unnest(Year) %>%
  left_join(
    multiplier_df %>% select(Year, Multiplier = start_up),
    by = "Year"
  ) %>%
  mutate(`kg/yr` = `kg/yr` * Multiplier) %>%  # apply multiplier
  group_by(Year, Mineral, State_Province_Full) %>%
  summarise(kg_start_up = sum(`kg/yr`, na.rm = TRUE), .groups ="drop") 

# Rename kg columns in each data frame before merge
names(open_scrap_output)[names(open_scrap_output) == "kg/yr"] <- "kg_open"
names(plan_under_const)[names(plan_under_const) == "kg/yr"] <- "kg_under_const"
names(plan_plan)[names(plan_plan) == "kg/yr"] <- "kg_plan"
names(plan_start_up)[names(plan_start_up) == "kg/yr"] <- "kg_start_up"

open_scrap_output$Source   <- "Open Scrap Output"
plan_under_const$Source    <- "Plan Under Construction"
plan_plan$Source           <- "Plan Planned"
plan_start_up$Source       <- "Plan Start Up"


# Merge
all_scrap <- open_scrap_output %>%
  full_join(plan_under_const, by = c("Year", "Mineral", "State_Province_Full")) %>%
  full_join(plan_plan, by = c("Year", "Mineral", "State_Province_Full")) %>%
  full_join(plan_start_up, by = c("Year", "Mineral", "State_Province_Full"))

# Replace NA with 0 only in numeric columns
all_scrap <- all_scrap %>%
  mutate(across(where(is.numeric), ~replace_na(.x, 0)))

all_scrap <- all_scrap %>%
  select(-starts_with("Source"))


# Sum the kg columns into one
all_scrap$kg_total <- rowSums(
  all_scrap[, c("kg_open", "kg_under_const", "kg_plan", "kg_start_up")],
  na.rm = TRUE
)


test <- all_scrap %>%
  select(Year, Mineral, State_Province_Full, kg_open) %>% filter(State_Province_Full == "California")

# Plot kg_open over time for each Mineral
ggplot(test, aes(x = Year, y = kg_open/1e6)) +
  geom_line(size = 1) +
  geom_point(size = 2) +
  facet_wrap(~ Mineral, scales = "free_y") +
  theme_minimal(base_size = 14) +
  labs(
    x = "Year",
    y = "kg Open (millions)",
    color = "Mineral",
    title = "Open Scrap (kg) over Time by State"
  ) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


sum_scrap <- all_scrap %>%
  group_by(Year, Mineral, State_Province_Full) %>%
  summarise(kg_total = sum(kg_total, na.rm = TRUE), .groups = "drop")



scenarios <- cap_chem_results %>%
  distinct(Battery_Scenario, Chemistry_Scenario) %>%
  mutate(Scenario = paste(Battery_Scenario, Chemistry_Scenario, sep = " - "))

sum_scrap_expanded <- sum_scrap %>%
  crossing(scenarios %>% select(Scenario)) %>%
  filter("Year" >= 2025) %>% rename(State = State_Province_Full, `Available Recycled Minerals (kg)` = kg_total)

recycle_and_scrap <- merge(sum_scrap_expanded, summary_final_future_hist, by = c("Year", "State", "Mineral", "Scenario"), all = TRUE)

recycle_and_scrap <- recycle_and_scrap %>%
  mutate(`Available Recycled Minerals (kg)` =
           rowSums(select(., matches("^Available Recycled Minerals \\(kg\\)")),
                   na.rm = TRUE)) %>%
  # Optionally, remove the original split columns if they exist
  select(-matches("^Available Recycled Minerals \\(kg\\)\\.[xy]$"))


##PLOT
# df_summary <- all_scrap %>%
#   group_by(Year, Mineral, State_Province_Full) %>%
#   summarise(across(starts_with("kg_"), ~sum(.x, na.rm = TRUE)), .groups = "drop") %>% select(-kg_total)

kg_cols <- setdiff(grep("^kg_", names(all_scrap), value = TRUE), "kg_total")

df_long <- all_scrap %>%
  pivot_longer(
    cols = all_of(kg_cols),   # safe selection of columns
    names_to = "kg_type",
    values_to = "kg_value"
  ) %>%
  filter(Mineral != "Carbon")

### national totals

# Aggregate national totals
df_national <- df_long %>%
  group_by(Year, Mineral, kg_type) %>%
  summarize(kg_value = sum(kg_value, na.rm = TRUE), .groups = "drop")

# Plot
ggplot(df_national, aes(x = Year, y = kg_value/1e6, fill = kg_type)) +
  geom_col(width = 0.8, alpha = 0.8) +
  facet_wrap(~ Mineral, scales = "free_y") +
  scale_fill_discrete(labels = kg_labels) +
  theme_minimal(base_size = 15) +
  theme(
    legend.position = "bottom",
    legend.text = element_text(size = 11),
    legend.key.size = unit(0.3, "cm"),
    axis.text.x = element_text(angle = 45, hjust = 1),
    strip.text = element_text(face = "bold", size = 12)
  ) +
  labs(
    x = "Year",
    y = "Millions of kg",
    fill = "Facility Type",
    title = "National Manufacturing Scrap Generated"
  )


kg_labels <- c(kg_open = "Open Commercial", 
               kg_under_const = "Under Construction",
               kg_plan = "Planned",
               kg_start_up = "Start-Up/Pre-Commercial")

state_name = "California"
# Plot
ggplot(df_long %>% filter(State_Province_Full == state_name), aes(x = Year, y = kg_value/1e6, fill = kg_type)) +
  geom_col(width = 0.8, alpha = 0.8) +
  facet_wrap(~ Mineral, scales = "free_y") +
  scale_fill_discrete(labels = kg_labels) +
  theme_minimal(base_size = 15) +
  theme(
    legend.position = "bottom",
    legend.text = element_text(size = 11),
    legend.key.size = unit(0.3, "cm"),
    axis.text.x = element_text(angle = 45, hjust = 1),
    strip.text = element_text(face = "bold", size = 12)
  ) +
  labs(
    x = "Year",
    y = "Millions of kg",
    fill = "Facility Type",
    title = paste("Manufacturing Scrap Generated in", state_name)
  )




    