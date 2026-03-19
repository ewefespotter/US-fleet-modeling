## HDV scenarios
library(readxl)
library(tidyverse)
library(stringr)         
library(dplyr)
library(tidyr)
library(purrr)
library(writexl)
library(colorspace)

HDV_chem <- read_xlsx("/Users/elsawefes-potter/Documents/Critical_Minerals_Pablo/HMDV_EV_Volumes.xlsx", sheet = "Batteries - MWh", skip = 10)%>%
  rename(
    `2020` = "MWh 2020",
    `2021` = "MWh 2021",
    `2022` = "MWh 2022",
    `2023` = "MWh 2023",
    `2024` = "MWh 2024"
  ) %>%select(-"MWh 2025 CY") %>%
  pivot_longer(
    cols = `2020`:`2024`,
    names_to = "Year",
    values_to = "MWh"
  ) %>%
  mutate(`Cathode Chemistry` = str_replace(`Cathode Chemistry`, "LF`P", "LFP")) %>%
  group_by(Year,`Cathode Chemistry`) %>%
  summarise(MWh = sum(MWh))

years <- as.character(2010:2024)

HDV_cap <- read_xlsx("/Users/elsawefes-potter/Documents/Critical_Minerals_Pablo/HMDV_EV_Volumes.xlsx", sheet = "Batteries - Units", skip = 11) %>%
  select(`OEM Group`, `Cathode Chemistry`, all_of(years)) %>%
  mutate(`Cathode Chemistry` = replace(`Cathode Chemistry`, 564, "Grand Total")) %>%
  filter(!str_ends(`OEM Group`, "Total") | `OEM Group` == "Grand Total") %>%
  group_by(`Cathode Chemistry`) %>%
  summarise(across(`2010`:`2024`, ~sum(.x, na.rm = TRUE))) %>%
  pivot_longer(
    cols = `2010`:`2024`,
    names_to = "Year",
    values_to = "Units"
  ) %>%
  filter(Year >= 2020) 

totals_cap <- HDV_cap %>% 
  filter(`Cathode Chemistry` == "Grand Total") %>%
  rename(Total_Units = Units) %>%
  select(-`Cathode Chemistry`)

HDV_cap <- HDV_cap %>%
  filter(`Cathode Chemistry` != "Grand Total")

HDV_cap_percent <- HDV_cap %>%
  merge(totals_cap, by = "Year") %>%
  group_by(Year, `Cathode Chemistry`, Total_Units) %>%
  mutate(
    Units = as.numeric(Units),   # make sure numeric
    Total_Units = as.numeric(Total_Units),   # make sure numeric
    Percent = Units / Total_Units * 100
  ) %>%
  ungroup()


totals_chem <- HDV_chem %>% 
  filter(`Cathode Chemistry` == "Grand Total") %>%
  rename(Total_MWh = MWh) %>%
  select(-`Cathode Chemistry`)

HDV_chem <- HDV_chem %>% 
  filter(`Cathode Chemistry` != "Grand Total")

HDV_chem_percent <- HDV_chem %>%
  merge(totals_chem, by = "Year") %>%
  group_by(Year, `Cathode Chemistry`, Total_MWh) %>%
  mutate(
    MWh = as.numeric(MWh),   # make sure numeric
    Total_MWh = as.numeric(Total_MWh),   # make sure numeric
    Percent = MWh / Total_MWh * 100
  ) %>%
  ungroup() %>%
  filter(Percent >= 2)

comparisons_percents <- HDV_chem_percent %>% 
  right_join(HDV_cap_percent, by = c("Year", "Cathode Chemistry")) 

### ok not doing avg cap per chemistry--> just avg cap per year and chem dist per year

HDV_avg_cap <- totals_cap %>% merge(totals_chem, on = (Year)) %>%
  mutate(Avg_kwh_unit = Total_MWh/Total_Units*1000
         )

sum = sum(HDV_chem_percent$Percent)
print(sum)

## HDV cap proj
HDV_cap_trend <- HDV_avg_cap %>%
  summarise(
    trend = coef(lm(`Avg_kwh_unit` ~ Year))[2]
  )

years_batt_cap <- 2024:2035


projection_HDV <- HDV_avg_cap %>% filter(Year == 2024) %>%
  merge(HDV_cap_trend) %>%
  crossing(years_batt_cap)


projection_HDV_full <- projection_HDV %>%
  mutate(`HDV_kwh_unit` = Avg_kwh_unit + (years_batt_cap - 2024) * trend) %>% 
  select(-c(Year, Total_Units, Total_MWh, trend, Avg_kwh_unit)) %>% 
  rename(Sale_Year = years_batt_cap,`Projected Avg Batt Cap (kwh/batt)` = HDV_kwh_unit) 

projection_HDV_full <- bind_rows(
  projection_HDV_full,
  projection_HDV_full %>%
    filter(Sale_Year == 2035) %>%
    slice(rep(1:n(), 15)) %>%        # repeat rows
    mutate(Sale_Year = 2036:2050)
) %>%
  mutate(Segment = "HDV",
         Propulsion = "HDV")


#### HDV BATT CAP Proj 2
HDV_cap_2040 <- HDV_avg_cap %>% filter(Year ==2024) %>% rename(Sale_Year = Year) %>%
  mutate(Avg_kwh_unit = Avg_kwh_unit * 0.85, Sale_Year = 2040)

HDV_batts <- bind_rows(HDV_avg_cap %>% 
                         filter(Year == 2024) %>% 
                         rename(Sale_Year = Year) %>%
                         mutate(Sale_Year = as.numeric(Sale_Year)),HDV_cap_2040)

all_batt_cap_years <- 2024:2050

# Manual calculation using reframe
HDV_second_trend <- HDV_batts %>%
  reframe(
    cap_2024 = Avg_kwh_unit[Sale_Year == 2024],
    cap_2040 = Avg_kwh_unit[Sale_Year == 2040],
    slope    = (cap_2040 - cap_2024) / (2040 - 2024),
    intercept = cap_2024 - slope * 2024
  )

HDV_batt_cap_15 <- HDV_second_trend %>%
  crossing(Sale_Year = all_batt_cap_years) %>%
  mutate(
    `Avg_kwh_unit` = case_when(
      Sale_Year <= 2040 ~ intercept + slope * Sale_Year,
      TRUE ~ intercept + slope * 2040  # hold at 2040 value
    ) 
  ) %>% select(Sale_Year, Avg_kwh_unit) %>%
  rename(`Projected Avg Batt Cap (kwh/batt)` = Avg_kwh_unit) %>%
  mutate(Segment = "HDV",
         Propulsion = "HDV")


## Independently made
## Use same chem projection for both scenarios
HDV_chem_project <- crossing(
  Sale_Year = 2024:2050,
  `Cathode Mix` = c("LFP", "NMC 811")
) %>%
  mutate(
    `Cathode Mix Share` = ifelse(`Cathode Mix` == "LFP", 0.85, 0.15)
  ) %>%
  mutate(Segment = "HDV",
        Propulsion = "HDV")

HDV_chem_hist <- crossing(
  Sale_Year = 2022:2024,
  `Cathode Mix` = c("LFP", "NMC 811")
) %>%
  mutate(
    `Share of Avg Chem` = ifelse(`Cathode Mix` == "LFP", 0.85, 0.15)
  ) %>%
  mutate(Segment = "HDV",
        Propulsion = "HDV")
  
