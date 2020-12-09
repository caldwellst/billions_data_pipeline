library(billionaiRe)
library(tidyverse)
library(whoville)

df <- load_billion_data("hep") %>%
  transform_hep_data() %>%
  calculate_hep_components()

write_csv(df, "output/hep/hep_components.csv")

bill_df <- calculate_hep_billion(df)

write_csv(bill_df, "output/hep/hep_billions.csv")

## Wrangling for dashboard

bill_df %>%
  filter(ind == "hep_idx") %>%
  transmute(iso3,
            year,
            ind = "hep_num",
            transform_value = contribution) %>%
  bind_rows(calculate_hep_billion(df, end_year = 2019) %>%
              filter(ind == "hep_idx") %>%
              transmute(iso3, year, ind = "hep_num", transform_value = contribution)) %>%
  bind_rows(df) %>%
  transmute(Indicator_FK = convert_ind_codes(ind, "analysis_code", "dashboard_id"),
            ind,
            Year_FK = year,
            iso3,
            GeoArea_FK = iso3_to_names(iso3),
            Actual_Value = transform_value,
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
  filter(!is.na(Indicator_FK)) %>%
  write_csv("output/hep/hep_dashboard_upload.csv",
            na = "")

