source("~/datathon-2022/helper_functions.R")
library(gtsummary)
library(MatchIt)

DATA_PATH <- "~/Documents/Schoolwork/Stanford/MS1 Winter/Data Competition/Data/"
AGE_RANGE_1 <- 60:64
AGE_RANGE_2 <- 65:69

load_data <- function(data_path = DATA_PATH, data_year) {
  data_files <- list.files(data_path) %>% 
    tibble(file = .) %>% 
    filter(grepl(paste0("^NIS", "_20", data_year), file)) %>% 
    pull(file) %>% 
    setNames(c(paste0("core", data_year), 
               paste0("hospital", data_year), 
               paste0("severity", data_year)))
  
  all_data <- map(data_files,
                  function(x) {
                    print(x)
                    fread(glue("{data_path}/{x}"))
                  } )
  
  return(all_data)
}



clean_data <- function(all_data) {
  
  age_range1 <- AGE_RANGE_1
  age_range2 <- AGE_RANGE_2
  age_range <- sort(unique(c(age_range1, age_range2)))
  
  ie_list <- list("Age between 60 and 70" = quo(age %in% age_range),
                  "On Medicare, Medicaid, or self-pay" = quo(pay1 %in% c(1, 2, 4)))
  
  
  ie_applied <- make_attrition_table(all_data[[1]] %>% 
                                       as_tibble(), ie_list)
  
  initial_attrition_tbl <- ie_applied$cohort_selection
  
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
           income_quartile = zipinc_qrtl,
           weights = discwt) %>% 
    left_join(all_data[[3]] %>% 
                select(key_nis,
                       aprdrg,
                       drg_severity = aprdrg_severity,
                       mortality_risk = aprdrg_risk_mortality)) %>% 
    left_join(all_data[[2]] %>% 
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
                                              hosp_location_teaching == 3 ~ "Teaching")) 
  
  second_ie_list <- list("Either old medicare or young self-pay" = quo(group %in% c("Old, Medicare", "Young, Self-pay")),
                         "No missing data on matching variables" = quo(!is.na(elective_admission) & !is.na(race) & !is.na(female) & !is.na(income_quartile)))
  
  second_applied_ie <- make_attrition_table(final_df, second_ie_list)
  
  second_attrition_tbl <- second_applied_ie$cohort_selection
  
  final_cleaned_df <- second_applied_ie$filtered_datasets %>% 
    tail(1) %>% 
    pluck(1)
  
  out <- list(attrition_table = bind_rows(initial_attrition_tbl,
                                          second_attrition_tbl),
              final_df = final_cleaned_df)
  
  return(out)
  
}

do_matching <- function(starting_df) {
  
  matching_fit <- matchit(group ~ service_line + female + race + hosp_region + urban_rural + income_quartile, 
                          data = starting_df, 
                          method = 'exact')
  
  matched_df <- starting_df %>% 
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
  
  full_df <- starting_df %>% 
    mutate(matched = F) %>% 
    bind_rows(matched_df) %>% 
    mutate(final_grouping = case_when(group == "Old, Medicare" & matched ~ "Old medicare, matched",
                                      group == "Old, Medicare" & !matched ~ "Old medicare, unmatched",
                                      group == "Young, Self-pay" & matched ~ "Young, Self-pay")) %>% 
    filter(!is.na(final_grouping)) %>% 
    filter(matched)
  
  return(full_df)
  
}

make_table1 <- function(full_df) {
  
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
  
  table_1 <- full_df %>% 
    select(all_of(table1_vars)) %>% 
    tbl_summary(by = grouping_var)
  
  return(table_1)
  
  
}

make_money_plot <- function(full_df) {
  full_matched_df$edBin <- full_matched_df$ed_entry == "Suspected ED"
  plot_df <- full_matched_df %>% 
    group_by(age) %>% 
    summarise(mortality = mean(died, na.rm = T),
              #total_charge = mean(total_charges, na.rm = T),
              n_dx = mean(i10_ndx, na.rm = T),
              # n_procedures = mean(i10_npr, na.rm = T),
              #los = mean(los, na.rm = T),
              ed = mean(edBin,  na.rm = T),
              elective = mean(elective_admission, na.rm = T),
              transfer_in = mean(transfer_in != 0, na.rm = T),
              transfer_out = mean(transfer_out != 0, na.rm = T)) %>% 
    pivot_longer(cols = 2:ncol(.),
                 names_to = "outcome",
                 values_to = "value")
  
  plot_df$merger <- paste0(plot_df$outcome, plot_df$age %in% AGE_RANGE_1)
  
  plot_df_2 <- full_matched_df %>% 
    group_by(age %in% AGE_RANGE_1) %>% 
    summarise(mortality = mean(died, na.rm = T),
              #total_charge = mean(total_charges, na.rm = T),
              n_dx = mean(i10_ndx, na.rm = T),
             # n_procedures = mean(i10_npr, na.rm = T),
             #los = mean(los, na.rm = T),
             ed = mean(edBin,  na.rm = T),
             elective = mean(elective_admission, na.rm = T),
              transfer_in = mean(transfer_in != 0, na.rm = T),
              transfer_out = mean(transfer_out != 0, na.rm = T)) %>% 
    pivot_longer(cols = 2:ncol(.),
                 names_to = "outcome",
                 values_to = "value")
  
  colnames(plot_df_2)[1] <- "age"
  plot_df_2$merger <- paste0(plot_df_2$outcome, plot_df_2$age)
  
  plot_df <- merge(plot_df, plot_df_2, by = "merger")
  
  ggplot(plot_df, aes(x = age.x, y = value.x)) +
    geom_point() +
    geom_line() +
    geom_line(mapping = aes(y = value.y, group = age.y), color = "blue", linetype = "dashed", size = 1) +
    facet_wrap(~outcome.x, scales = "free") +
    geom_vline(xintercept = 64.5, linetype = "dashed")
  
}

if (F) {
  
  all_data <- load_data(DATA_PATH,19)
  
  cleaned_data <- clean_data(all_data)
  
  final_df <- cleaned_data$final_df
  
  full_matched_df <- do_matching(final_df)
  
  table1 <- make_table1(full_matched_df)
  
  figure1 <- make_money_plot(full_matched_df)
  
  model_df <- full_matched_df %>% 
    select(final_grouping, died, drg, elective_admission, female, ed_entry,
           hosp_region, urban_rural, race, major_dx_cat, income_quartile,
           hosp_size, service_line) %>% 
    mutate(age_var = case_when(grepl("young", tolower(final_grouping)) ~ "young",
                               T ~ "old")) %>% 
    drop_na()
  
  skimr::skim(model_df)
  
  outcomes <- c("Mortality" = "died",
                "Transfer out" = "transfer_out",
                "Total charges" = "total_charge",
                "") 
  
  model_fit <- glm(died ~ final_grouping, family = binomial, data = model_df)
  model_fit %>% 
    broom::tidy(exp = T)
  
}





