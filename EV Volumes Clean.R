# Prepare EV Sales Data
# Source: EV Volumes 
# EWP July 2025

# Load libraries and path -----
source("/Users/elsawefes-potter/Documents/Critical_Minerals_Pablo/00-Libraries.R", encoding = "UTF-8")
install.packages("tidyverse")
library(tidyverse)
library(readxl)
library(stringr)         # Load the stringr package
library(dplyr)
library(tidyr)
library(purrr)




data_folder = "/Users/elsawefes-potter/Documents/Critical_Minerals_Pablo"
# Read raw data -----
EV_historical <- read_excel(file.path(data_folder, "Cathode Mix Update.xlsx"))
regs <- read_excel(file.path(data_folder, "LDV registration and sales.xlsx"), sheet = "registrations ")
usa_sales <- EV_historical

mineral_intensity <- read_excel(file.path(data_folder, "Mineral_Intensity(2).xlsx"), na = "")

#Aggregate monthly Reg to annual -----
reg_cols <- grep("^Reg_\\d{4}[-_]\\d{2}[-_]\\d{2}", names(usa_sales), value = TRUE)
reg_year_groups <- split(reg_cols, str_extract(reg_cols, "\\d{4}"))
for (yr in names(reg_year_groups)) {
   cols_this_year <- reg_year_groups[[yr]]
   numeric_data <- usa_sales[, cols_this_year] %>%
     mutate(across(everything(), ~as.numeric(.)))
   usa_sales[[paste0("Total_Reg_", yr)]] <- rowSums(numeric_data, na.rm = TRUE)
 }

# Aggregate monthly MWh to annual -----
mwh_cols <- grep("^Mwh_\\d{4}[-_]\\d{2}[-_]\\d{2}", names(usa_sales), value = TRUE)
mwh_year_groups <- split(mwh_cols, str_extract(mwh_cols, "\\d{4}"))
 
for (yr in names(mwh_year_groups)) {
   cols_this_year <- mwh_year_groups[[yr]]
   numeric_data <- usa_sales[, cols_this_year] %>%
  mutate(across(everything(), ~as.numeric(.)))
   usa_sales[[paste0("Total_Mwh_", yr)]] <- rowSums(numeric_data, na.rm = TRUE)
 }
 
# Drop raw Reg and Mwh monthly columns -----
usa_sales <- usa_sales %>% select(-all_of(c(reg_cols, mwh_cols)))
 

# Replace "NA" string in Cathode Mix -----
usa_sales <- usa_sales %>%
   mutate(`Cathode Mix` = ifelse(`Cathode Mix` == "NA",
                                 paste0(`Cathode Chemistry`, " (unspecified)"),
                                 `Cathode Mix`))
 
# Drop 2013 data -----
usa_sales <- usa_sales %>% select(-matches("2013$"))
 
# Reshape to long format -----
usa_sales$id <- 1:nrow(usa_sales)
usa_sales_long <- usa_sales %>%
   pivot_longer(cols = starts_with("Total_"),
                names_to = c(".value", "Sale Year"),
                names_pattern = "Total_(.*)_(\\d{4})")
 

# Select and rename final columns -----
usa_sales_filtered <- usa_sales_long %>%
   select(`Sale Year`, `Battery kWh`, `Cathode Mix`, Propulsion,
          `Global Segment`, Reg, Mwh) %>%
   rename(`Total Sales` = Reg,
          `Total Mwh` = Mwh)


 
# # Filter out non-LDV segments (e.g., PUP) -----
#usa_sales_filtered <- usa_sales_filtered %>%
   #stats::filter(!str_starts(`Global Segment`, "PUP"))
 
 # Normalize segment names to Car/SUV -----
usa_sales_filtered <- usa_sales_filtered %>%
   mutate(`Global Segment` = case_when(
     str_starts(`Global Segment`, "Car") ~ "Car",
     str_starts(`Global Segment`, "SUV") ~ "SUV",
     str_starts(`Global Segment`, "MPV") ~ "SUV",
     str_starts(`Global Segment`, "SS") ~ "SUV",
     str_starts(`Global Segment`, "LCV") ~ "SUV",
     str_starts(`Global Segment`, "PUP") ~ "SUV",
     TRUE ~ `Global Segment`
   ))

powertrain <- c("BEV", "PHEV")

## renaming reg year to sale year for merger later (assumption that regs distribution = sales distribution)
regs_zev <- regs %>%
  select(State, `Electric (EV)`, `Plug-In Hybrid Electric (PHEV)`, Hydrogen, Year) %>%
  rename(BEV = `Electric (EV)`,
         PHEV = `Plug-In Hybrid Electric (PHEV)`,
         FCEV = Hydrogen,
         `Sale Year` = Year) %>%
  filter(State != "United States")

regs_zev <- regs_zev %>%
  group_by(`Sale Year`) %>%
  mutate(across(all_of(powertrain), ~ . / sum(., na.rm = TRUE), .names = "Fraction_{.col}")) %>%
  ungroup() %>%
  select(State, `Sale Year`, starts_with("Fraction_"))

# Expand missing years (2014–2015 and 2024) -----
keep_new <- regs_zev %>% filter(`Sale Year` == 2023) %>% mutate(`Sale Year` = 2024)
keep_old <- regs_zev %>% filter(`Sale Year` == 2016)
old_years <- 2014:2015

extended <- bind_rows(
  map_dfr(old_years, ~ keep_old %>% mutate(`Sale Year` = .x)),
  regs_zev,
  keep_new
) %>%
  arrange(`Sale Year`, State)

pt_veh_sales <- usa_sales_filtered %>%
  group_by(`Sale Year`, Propulsion, `Global Segment`) %>%
  summarise(`Total Sales` = sum(`Total Sales`, na.rm = TRUE), .groups = "drop") 
  

pt_veh_sales$`Sale Year` <- as.character(pt_veh_sales$`Sale Year`)
extended$`Sale Year` <- as.character(extended$`Sale Year`)

historical_state_pt_veh_df <- left_join(extended, pt_veh_sales, by = "Sale Year")
historical_state_pt_veh_df <- historical_state_pt_veh_df[historical_state_pt_veh_df$Propulsion != "FCEV", ]
print(historical_state_pt_veh_df)

historical_state_pt_veh_df <- historical_state_pt_veh_df %>%
  rowwise() %>%
  mutate(`Sales` = get(paste0("Fraction_", Propulsion)) * `Total Sales`) %>%
  ungroup()
print(historical_state_pt_veh_df)

historical_state_pt_veh_df <- historical_state_pt_veh_df %>% select(`State`, `Sale Year`, `Propulsion`, `Global Segment`, `Sales`)

print(historical_state_pt_veh_df)


write_csv(historical_state_pt_veh_df, "/Users/elsawefes-potter/Documents/Critical_Minerals_Pablo/historical_state_pt_veh_df.csv")



