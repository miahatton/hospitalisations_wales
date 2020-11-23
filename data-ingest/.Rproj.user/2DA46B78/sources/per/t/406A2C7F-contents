library(tidyverse)

beds_percent_covid <- read_csv('data/beds_percent_covid.csv')

beds <- beds_percent_covid %>% 
  select(-percent_beds_in_use_covid)%>% 
  mutate(date = as.Date(date)) %>% 
  arrange(health_board, date)
head(beds)
##### LOCKDOWN INFO

lockdown_info <- read_csv('data/lockdowns_health_boards.csv')

lockdown_info$date <- mapvalues(
  lockdown_info$date, 
  "01/07/2020",
  "01/04/2020"
)

head(lockdown_info)

lockdown_info %>% 
  mutate(date = as.Date(date, "%d/%m/%Y")) -> lockdown_info

beds_lockdowns <- beds %>% 
  left_join(., lockdown_info, by = c(
    "health_board" = "health_board",
    "date" = "date"
  ))
head(beds_lockdowns)

# propagate values
for (i in 1:nrow(beds_lockdowns)) {
  if (is.na(beds_lockdowns$prop_lockdown[i])) beds_lockdowns$prop_lockdown[i] <- beds_lockdowns$prop_lockdown[i-1]
}

beds_lockdowns %>% 
  select(-num_lockdown) %>% 
  select(-hb_pop) -> beds_lockdowns

beds_lockdowns %>% 
  mutate(
    lockdown_status = case_when(
      prop_lockdown == 0 ~ "No local lockdowns",
      prop_lockdown > 0 & prop_lockdown < 2 ~ "Local lockdowns",
      prop_lockdown == 2 & date < as.Date("2020-11-09", "%Y-%m-%d") ~ "Firebreak lockdown",
      date >= as.Date("2020-11-09", "%Y-%m-%d") ~ "Post-firebreak"
    )) %>% 
  mutate(
    proportion_lockdown = ifelse(
      lockdown_status == 'Post-firebreak',
      0,
      prop_lockdown
    )
  ) -> beds_lockdowns

write.csv(beds_lockdowns, 'data/lockdown_status_hb_date.csv', row.names = FALSE)

head(beds_lockdowns)
tail(beds_lockdowns)

lockdown_info %>% 
  filter(str_detect(health_board, 'Aneurin') & prop_lockdown > 0) %>% 
  write.csv('data/lockdown_status_ab.csv', row.names = F)


lockdown_info %>% 
  filter(str_detect(health_board, 'Betsi') & prop_lockdown > 0) %>% 
  write.csv('data/lockdown_status_bc.csv', row.names = F)


lockdown_info %>% 
  filter(str_detect(health_board, 'Cwm') & prop_lockdown > 0) %>% 
  write.csv('data/lockdown_status_ct.csv', row.names = F)

lockdown_info %>% 
  filter(str_detect(health_board, 'Powys') & prop_lockdown > 0) %>% 
  write.csv('data/lockdown_status_p.csv', row.names = F)
