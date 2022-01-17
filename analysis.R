source("~/datathon-2022/helper_functions.R")

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

age_range <- 60:70

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
                     hosp_location_teaching = hosp_locteach)) %>% 
  mutate(group = case_when())


table1_vars <- c()


  names() %>% 
  sort()







