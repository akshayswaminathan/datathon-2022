get_percent_change <- function(matched_df, var_name, drg) {

  old_young <- full_matched_df %>% 
    mutate(dx = case_when(drg %in% 871:872 ~ "Sepsis",
                          drg == 291 ~ "Heart failure",
                          drg == 470 ~ "Joint replacement",
                          drg == 885 ~ "Psychosis",
                          drg == 392 ~ ""))
    group_by(final_grouping) %>% 
    summarise(mean = mean(!!sym(var_name), na.rm = T)) %>% 
    pull(mean)
  
  scales::percent((old_young[1] - old_young[2]) / old_young[1])
  
}

drg_list <- list("Nervous system" = 20:103,
     "Eye disorder" = 113:125,
     "ENT disorder" = 135:159,
     "Respiratory disorder" = 163:208,
     "Cardiovascular disorder" = 215:320,
     "GI disorder" = 326:446,
     "Musculoskeletal disorder" = 453:556,
     "Skin disorder" = 570:607,
     "Metabolic disorder" = 614:645,
     "Genitourinary/Reproductive disorder" = 650:795,
     "Blood/cancer disorder" = 799:872,
     "Mental disorder" = 876:887
     )

get_percent_change(full_matched_df, "transfer_out")

full_matched_df %>% 
  select(starts_with("i10_dx")) %>% 
  mutate(all_icds = )

load("~/Documents/full_matched.RData")

var_name <- "died"

young_group <- full_matched_df %>% 
  filter(final_grouping == "Young, Self-pay") %>% 
  summarise(mean = mean(ed_entry == "Suspected ED")) %>% 
  pull(mean)

full_matched_df %>% 
  group_by(final_grouping) %>% 
  summarise(mean = mean(ed_entry == "Suspected ED"), na.rm = T) %>% 
  pull(mean)

young_group <- full_matched_df %>% 
  filter(final_grouping == "Young, Self-pay") %>% 
  summarise(mean = mean(!!sym(var_name))) %>% 
  pull(mean)


full_matched_df %>% 
  select(starts_with("i10_dx")) %>% 
  map(ascharact)


