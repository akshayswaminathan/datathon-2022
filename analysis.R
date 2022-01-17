source("~/datathon-2022/helper_functions.R")
library(gtsummary)
library(MatchIt)

data_path <- "~/Downloads/"

data_files <- list.files(data_path) %>% 
  tibble(file = .) %>% 
  filter(grepl("^NIS", file)) %>% 
  pull(file) %>% 
  setNames(c("core19", "hospital19", "severity19"))

all_data <- map(data_files,
                function(x) {
                  print(x)
                  fread(glue("{data_path}/{x}"))
                } )


age_range1 <- 60:64
age_range2 <- 65:66
age_range <- sort(unique(c(age_range1, age_range2)))

ie_list <- list("Age between 60 and 70" = quo(age %in% age_range),
                "On Medicare, Medicaid, or self-pay" = quo(pay1 %in% c(1, 2, 4)))

ie_applied <- make_attrition_table(all_data$core19 %>% 
                                     as_tibble(), ie_list)

ie_applied$cohort_selection

final_df <- ie_applied$filtered_datasets %>% 
  tail(1) %>% 
  pluck(1) %>%
  select(key_nis,
         hosp_nis,
         payer = pay1,
         age,
         died,
         drg,
         elective_admission = elective,
         female,
         ed_entry = hcup_ed,
         hosp_division,
         starts_with("i10"),
         los,
         or_indicator = pclass_orproc,
         urban_rural = pl_nchs,
         race,
         major_dx_cat = mdc,
         total_charges = totchg,
         transfer_in = tran_in,
         transfer_out = tran_out,
         year,
         income_quartile = zipinc_qrtl) %>% 
  left_join(all_data$severity19 %>% 
              select(key_nis,
                     aprdrg,
                     drg_severity = aprdrg_severity,
                     mortality_risk = aprdrg_risk_mortality)) %>% 
  left_join(all_data$hospital19 %>% 
              select(hosp_nis,
                     hosp_size = hosp_bedsize,
                     hosp_region,
                     hosp_location_teaching = hosp_locteach)) %>% 
  mutate(payer = case_when(payer == 1 ~ "Medicare",
                           payer == 2 ~ "Medicaid",
                           payer == 4 ~ "Self-pay"),
         group = case_when(age %in% age_range1 & payer == "Self-pay" ~ "Young, Self-pay",
                           age %in% age_range1 & payer == "Medicaid" ~ "Young, Medicaid",
                           age %in% age_range2 & payer == "Medicare" ~ "Old, Medicare"),
         ed_entry = case_when(ed_entry == 0 ~ "No ED",
                              T ~ "Suspected ED"),
         hosp_region = case_when(hosp_region == 1 ~ "Northeast",
                                 hosp_region == 2 ~ "Midwest",
                                 hosp_region == 3 ~ "South",
                                 hosp_region == 4 ~ "West"),
         race = case_when(race == 1 ~ "White",
                          race == 2 ~ "Black",
                          race == 3 ~ "Hispanic",
                          race == 4 ~ "API",
                          race == 5 ~ "Native American",
                          race == 6 ~ "Other"),
         service_line = case_when(i10_serviceline == 1 ~ "Maternal/neonatal",
                                  i10_serviceline == 2 ~ "Mental health/substance",
                                  i10_serviceline == 3 ~ "Injury",
                                  i10_serviceline == 4 ~ "Surgical",
                                  i10_serviceline == 5 ~ "Medical"),
         urban_rural = case_when(urban_rural %in% 1:4 ~ "Urban",
                                 T ~ "Rural"),
         hosp_size = case_when(hosp_size == 1 ~ "Small",
                               hosp_size == 2 ~ "Medium",
                               hosp_size == 3 ~ "Large"),
         hosp_location_teaching = case_when(hosp_location_teaching %in% 1:2 ~ "Non-teaching",
                                            hosp_location_teaching == 3 ~ "Teaching")) %>% 
  filter(group %in% c("Old, Medicare", "Young, Self-pay"),
         !is.na(elective_admission),
         !is.na(race),
         !is.na(female))


table1_vars <- c("% Female" = "female",
                 "Race" = "race",
                 "Urban/Rural" = "urban_rural",
                 "Income quartile" = "income_quartile",
                 "Elective admission" = "elective_admission",
                 "Hospital teaching" = "hosp_location_teaching",
                 "ED entry" = "ed_entry",
                 "Hospital Region" = "hosp_region",
                 "Service line" = "service_line",
                 "Total charges" = "total_charges",
                 "OR indicator" = "or_indicator",
                 "Hospital size" = "hosp_size",
                 "Group" = "group",
                 "Final grouping" = "final_grouping")

grouping_var <- "Final grouping"

final_df %>% 
  filter(!is.na(group)) %>% 
  select(all_of(table1_vars)) %>% 
  tbl_summary(by = "Group")

matching_fit <- matchit(group ~ service_line + elective_admission + ed_entry + female + race, 
                        data = final_df, 
                     method = 'exact')

matched_df <- final_df %>% 
  mutate(discard = matching_fit$discarded,
         subclass = matching_fit$subclass,
         matched = T) %>% 
  filter(!discard) %>% 
  group_by(subclass) %>% 
  nest() %>% 
  mutate(selected_sample = map2(subclass, data,
                               function(subclass_, in_df) {
                                 
                                 to_match <- in_df %>% 
                                   filter(group == "Young, Self-pay")
                                 
                                 eligible <- in_df %>% 
                                   filter(group == "Old, Medicare")
                                 
                                 n_to_match <- pmin(2 * nrow(to_match), nrow(eligible))
                                 
                                 matched <- eligible %>% 
                                   sample_n(n_to_match) %>% 
                                   bind_rows(to_match)
                                 
                                 return(matched)
                                 
                               })) %>% 
  select(-data) %>% 
  unnest(selected_sample) %>% 
  select(-subclass, -discard) %>% 
  ungroup()

# final_df %>% 
#   mutate(discard = matching_fit$discarded,
#          subclass = matching_fit$subclass,
#          matched = T) %>% 
#   count(subclass, group) %>% 
#   arrange(n) %>% 
#   View()

full_df <- final_df %>% 
  mutate(matched = F) %>% 
  bind_rows(matched_df) %>% 
  mutate(final_grouping = case_when(group == "Old, Medicare" & matched ~ "Old medicare, matched",
                                    group == "Old, Medicare" & !matched ~ "Old medicare, unmatched",
                                    group == "Young, Self-pay" & matched ~ "Young, Self-pay")) %>% 
  filter(!is.na(final_grouping))

table_1 <- full_df %>% 
  select(all_of(table1_vars)) %>% 
  tbl_summary(by = grouping_var)





