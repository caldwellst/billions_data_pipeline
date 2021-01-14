library(tidyverse)
library(billionaiRe)
library(whoville)

df <- load_billion_data("hpop")

trans_df <- transform_hpop_data(df)

# hpop_indicator2 file
trans_df %>%
  transmute(iso3 = iso3,
            GeoArea_FK = iso3_to_names(iso3),
            gho_id = convert_ind_codes(ind, "analysis_code", "gho_code"),
            Indicator_FK = convert_ind_codes(ind, "analysis_code", "dashboard_id"),
            year = year,
            value = value,
            trans = transform_value,
            type = type,
            source = source) %>%
  filter(!is.na(value),
         year <= 2023) %>%
  write_csv("output/hpop/hpop_indicator2.csv", na = "")

# Indicator contributions

hpop_contr_df <- trans_df %>%
  add_hpop_populations() %>%
  calculate_hpop_contributions()

hpop_contr_df %>%
  transmute(gho_id = convert_ind_codes(ind, "analysis_code", "gho_code"),
            Indicator_FK = convert_ind_codes(ind, "analysis_code", "dashboard_id"),
            iso3 = iso3,
            GeoArea_FK = iso3_to_names(iso3),
            region = NA,
            sdg = NA,
            indicator = NA,
            topic = NA,
            name_short = NA,
            units = NA,
            end_value = round(value_2023, 1),
            end_scaled = round(transform_value_2023, 1),
            end_year = 2023,
            rmedbase = NA,
            reg10 = NA,
            reg25 = NA,
            reg75 = NA,
            reg90 = NA,
            qq = NA,
            level = NA,
            diff = end_scaled - round(transform_value_2018, 1),
            healthierk = contribution,
            start_value = round(value_2018, 1),
            start_scaled = round(transform_value_2018, 1),
            start_year = 2018,
            end_relpop = population,
            policy = NA) %>%
  write_csv("output/hpop/hp9chosen.csv", na = "")  

# Billions

hpop_2023 <- hpop_contr_df %>%
  calculate_hpop_billion() %>%
  transmute(GeoArea_FK = iso3_to_names(iso3),
            iso3 = iso3,
            plus = healthier,
            minus = unhealthier,
            net_healthier = net_healthier,
            popall = wppdistro::get_population(iso3, 2023),
            perc_healthier = 100 * net_healthier / popall,
            HP_Target = get_country_shares(iso3, "hpop"),
            HP_Target_Population_Perc = get_country_shares(iso3, "hpop", "perc"),
            `Projection 2023` = net_healthier,
            perc_healthier_2023 = perc_healthier)

hpop_2019 <- trans_df %>%
  add_hpop_populations() %>%
  calculate_hpop_contributions(start_year = 2018,
                               end_year = 2019) %>%
  calculate_hpop_billion() %>%
  transmute(iso3 = iso3,
            `Year now` = 2019,
            Projection_now = net_healthier,
            perc_healthier_now = perc_healthier)

left_join(hpop_2023, hpop_2019, by = "iso3") %>%
  write_csv("output/hpop/b3_bycountry.csv", na = "")

## Scenario tools

ind_scen_df <- hpop_contr_df %>%
  transmute(Country_ISO3Code = iso3,
            Indicator_CODE_Technical_ID = convert_ind_codes(ind, "analysis_code", "dashboard_id"),
            HP_Raw_Value_2018 = value_2018,
            HP_Raw_Value_2023 = value_2023,
            HP_Normalized_Value_2018 = transform_value_2018,
            HP_Normalized_Value_2023 = transform_value_2023,
            Population_Data_2023 = population,
            Contribution = contribution)

bill_scen_df <- hpop_contr_df %>%
  calculate_hpop_billion() %>%
  transmute(Country_ISO3Code = iso3,
            HP = net_healthier,
            HP_Per = perc_healthier,
            Guidelines = get_country_shares(iso3,),
            Guidelines_Per = HP * 100 / Guidelines,
            NUHL = unhealthier,
            NHL = healthier) %>%
  pivot_longer(-Country_ISO3Code,
               names_to = "Indicator_CODE_Technical_ID",
               values_to = "Contribution")

bind_rows(ind_scen_df, bill_scen_df) %>%
  write_csv("output/hpop/hpop_scenario_tool_data.csv", na = "")
s