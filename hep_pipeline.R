library(billionaiRe)
library(tidyverse)
library(whoville)

df <- load_billion_data("hep") 

trans_df <- df %>%
  transform_hep_data() %>%
  calculate_hep_components()

write_csv(trans_df, "output/hep/hep_components.csv",
          na = "")

bill_df <- calculate_hep_billion(trans_df)

write_csv(bill_df, "output/hep/hep_billions.csv",
          na = "")

## Wrangling for dashboard

# Fake DNR data for dashboard for missing countries

dnr_df <- expand_grid(iso3 = who_member_states(),
                      Year_FK = 2018:2023) %>%
  mutate(GeoArea_FK = iso3_to_names(iso3),
         Low_Level_Category = "Detect and Respond",
         High_Level_Category = "Detect and Respond",
         Indicator_FK = "HE_events")

# output for dashboard

bill_df %>%
  filter(ind == "hep_idx") %>%
  transmute(iso3,
            year,
            ind = "hep_num",
            transform_value = contribution) %>%
  bind_rows(calculate_hep_billion(trans_df, end_year = 2019) %>%
              filter(ind == "hep_idx") %>%
              transmute(iso3, year, ind = "hep_num", transform_value = contribution)) %>%
  bind_rows(trans_df) %>%
  transmute(Indicator_FK = convert_ind_codes(ind, "analysis_code", "dashboard_id"),
            ind,
            Year_FK = year,
            iso3,
            GeoArea_FK = iso3_to_names(iso3),
            Actual_Value = round(transform_value, 1),
            Country_Share = NA,
            Gap_Contr_Country_Share = NA,
            Contr_Perc_Country_Share = NA,
            High_Level_Category = case_when(
              Indicator_FK %in% c("HE_chemical",
                                  "HE_coordination",
                                  "HE_food",
                                  "HE_hr",
                                  "HE_laboratory",
                                  "HE_legislation",
                                  "HE_points",
                                  "HE_preparedness",
                                  "HE_radionuclear",
                                  "HE_response",
                                  "HE_risk",
                                  "HE_surveillance",
                                  "HE_zoonosis",
                                  "SDG3d1_ihr") ~ "Prepare",
              Indicator_FK %in% c("HE_detect",
                                  "HE_notify",
                                  "HE_respond",
                                  "HE_events") ~ "Detect and Respond",
              Indicator_FK == "HE_hepi" ~ "HEPI",
              Indicator_FK == "HE_numberofpeople" ~ "Number of people",
              Indicator_FK %in% c("HE_measles",
                                  "HE_polio",
                                  "HE_vaccinecov",
                                  "HE_cholera",
                                  "HE_meningitis",
                                  "HE_yellowfever") ~ "Prevent"
            ),
            Low_Level_Category = case_when(
              Indicator_FK == "HE_chemical" ~ "Chemical",
              Indicator_FK == "HE_coordination" ~ "Coordination",
              Indicator_FK == "HE_food" ~ "Food safety",
              Indicator_FK == "HE_hr" ~ "Human resources",
              Indicator_FK == "HE_laboratory" ~ "Laboratory",
              Indicator_FK == "HE_legislation" ~ "Legislation",
              Indicator_FK == "HE_points" ~ "Points of entry",
              Indicator_FK == "HE_preparedness" ~ "Preparedness",
              Indicator_FK == "HE_radionuclear" ~ "Radionuclear",
              Indicator_FK == "HE_response" ~ "Response",
              Indicator_FK == "HE_risk" ~ "Risk communication",
              Indicator_FK == "HE_surveillance" ~ "Surveillance",
              Indicator_FK == "HE_zoonosis" ~ "Zoonosis",
              Indicator_FK == "SDG3d1_ihr" ~ "Prepare",
              Indicator_FK == "HE_detect" ~ "Detect",
              Indicator_FK == "HE_notify" ~ "Notify",
              Indicator_FK == "HE_respond" ~ "Respond",
              Indicator_FK == "HE_events" ~ "Detect and Respond",
              Indicator_FK == "HE_hepi" ~ "HEPI",
              Indicator_FK == "HE_numberofpeople" ~ "Number of people",
              Indicator_FK == "HE_measles" ~ "Measles",
              Indicator_FK == "HE_polio" ~ "Polio",
              Indicator_FK == "HE_cholera" ~ "Cholera",
              Indicator_FK == "HE_yellowfever" ~ "Yellow Fever",
              Indicator_FK == "HE_meningitis" ~ "Meningitis",
              Indicator_FK == "HE_vaccinecov" ~ "Prevent"
            ),
            Actual_Level = level,
            Value_Type = case_when(
              type == "projected" ~ "Projection",
              type == "estimated" ~ "Estimate",
              type == "reported" ~ "Actual",
              TRUE ~ type
            )) %>%
  filter(!is.na(Indicator_FK),
         !(Indicator_FK) %in% c("HE_detect", "HE_notify", "HE_respond"),
         Year_FK <= 2023) %>%
  full_join(dnr_df) %>%
  write_csv("output/hep/hep_dashboard_upload.csv",
            na = "")

# Scenario tools data

dnr_st <- trans_df %>%
  filter(year %in% c(2018, 2023),
         ind == "detect_respond")

prep_st <- trans_df %>%
  filter(year %in% c(2018, 2023),
         ind == "espar")

prep_comp_base_st <- trans_df %>%
  filter(str_detect(ind, "espar[0-9]{2}"),
         year %in% 2018:2019) %>%
  group_by(iso3, ind) %>%
  filter(!(n() > 1 & year == 2019)) %>%
  ungroup() %>%
  mutate(prep_comp_year = year,
         year = 2018) 

prep_comp_st <- trans_df %>%
  filter(str_detect(ind, "espar[0-9]{2}")) %>%
  group_by(iso3, ind) %>%
  filter(year == max(year)) %>%
  ungroup() %>%
  mutate(prep_comp_year = year,
         year = 2023) %>%
  bind_rows(prep_comp_base_st)

prev_cmpgn_st <- df %>%
  transform_hep_data(cholera_latest_year = 2023,
                     meningitis_latest_year = 2023,
                     yellow_fever_latest_year = 2023) %>%
  filter(str_detect(ind, "campaign"),
         year %in% c(2018, 2019, 2023))
  
prev_rtne_st <- trans_df %>%
  filter(year %in% c(2018, 2023),
         str_detect(ind, "routine|infants") | ind == "prevent") %>%
  group_by(iso3, year) %>%
  mutate(transform_value = ifelse(str_detect(ind, "routine"),
                                  100 * transform_value / transform_value[ind == "surviving_infants"],
                                  transform_value)) %>%
  ungroup()

hep_idx_st <- trans_df %>%
  filter(ind == "hep_idx",
         year %in% c(2018, 2023))

st_df <- bind_rows(dnr_st, prep_st, prep_comp_st, prev_cmpgn_st, prev_rtne_st, hep_idx_st) %>%
  pivot_wider(c(iso3, year, ind),
              names_from = year,
              values_from = c(transform_value, level, prep_comp_year)) %>%
  left_join(bill_df, by = c("iso3", "ind")) %>%
  transmute(Country_ISO3Code = iso3,
            Indicator_CODE_Technical_ID = convert_ind_codes(ind, from = "analysis_code", to = "dashboard_id"),
            HE_Raw_Value_2018 = transform_value_2018,
            HE_Raw_Value_2019 = transform_value_2019,
            HE_Raw_Value_2020 = NA,
            HE_Raw_Value_2021 = NA,
            HE_Raw_Value_2023 = NA,
            HE_Raw_Value_2023 = transform_value_2023,
            Contributions_2023 = contribution,
            ACTUAL_LEVEL_2018 = level_2018,
            ACTUAL_LEVEL_2023 = level_2023,
            Prepare_Component_Year_2018 = prep_comp_year_2018,
            Prepare_Component_Year_2023 = prep_comp_year_2023,
            Population_2023 = wppdistro::get_population(iso3, 2023)) 

cmpgn_st <- st_df %>%
  filter(str_detect(Indicator_CODE_Technical_ID, "Camp")) %>%
  mutate(across(contains("Raw_Value"),
                ~NA))

st_df %>%
  mutate(Indicator_CODE_Technical_ID = ifelse(str_detect(Indicator_CODE_Technical_ID, "Camp"),
                                              paste0(Indicator_CODE_Technical_ID, "_Internal"),
                                              Indicator_CODE_Technical_ID)) %>%
  bind_rows(cmpgn_st) %>%
  pivot_longer(matches("_[0-9]{4}"),
               names_pattern = "(.*)_([0-9]{4})",
               names_to = c(".value", "Year_FK")) %>%
  filter(Year_FK %in% c(2018, 2019, 2023),
         !(Year_FK == 2019 & !str_detect(Indicator_CODE_Technical_ID, "cholera"))) %>%
  write_csv("output/hep/scenario_tools_load.csv",
            na = "")
