detach(package:plyr)
library(dplyr)

# can we look at hospital stay ends?

hospitalisations <- read_csv('data/hospitalisations_hb_long_unfiltered.csv')
admissions <- read_csv('data/admissions_hb_long_unfiltered.csv')

head(hospitalisations)

hospitalisations %>% 
  filter(admission_type != 'All' & admission_type != 'NonC19' & covid_status != 'All') %>% 
  select(-admission_type) %>% 
  pivot_wider(id_cols = c(date, health_board), names_from = covid_status, values_from = count) %>% 
  mutate(total_hospitalisations = Confirmed + Suspected + ifelse(is.na(Recovering), 0, Recovering)) -> hospitalisations_wide

admissions %>% 
  filter(admission_type == 'C19') %>% 
  select(date, health_board, new_admissions = count) %>% 
  right_join(hospitalisations_wide, by = c("date", "health_board")) %>% 
  arrange(health_board, date) %>% 
  mutate(prev_day_beds_occupied = lag(total_hospitalisations)) %>% 
  select(date, health_board, Suspected, Confirmed, Recovering, total_hospitalisations, new_admissions, prev_day_beds_occupied) %>% 
  mutate(difference = total_hospitalisations - prev_day_beds_occupied - new_admissions) -> admissions_hospitalisations
  
admissions_hospitalisations

write.csv(admissions_hospitalisations, 'data/admissions_hospitalisations_change.csv', row.names = F)

admissions_hospitalisations %>% 
  filter(!is.na(Recovering))
