## HOSPITAL ADMISSIONS

library(statswalesr)
library(plyr)
library(tidyverse)

df <- statswalesr::statswales_get_dataset("hlth0091")

all_hosp_data <- df %>% 
  filter(Hospitaltype_Code == "NHS") %>% 
  select(
    Date_Code, LocalHealthBoard_ItemName_ENG, Indicator_Code, Indicator_ItemName_ENG, Data
  ) %>% 
  mutate(
    date = as.Date(Date_Code, "%Y%m%d"),
    count = as.integer(Data)
  ) %>% 
  select(date, health_board = LocalHealthBoard_ItemName_ENG, 
         indicator_code = Indicator_Code, 
         indicator_name = Indicator_ItemName_ENG,
         count)

unique(all_hosp_data$health_board)


all_hosp_data$health_board <- mapvalues(
  all_hosp_data$health_board, 
  unique(all_hosp_data$health_board),
  c(
    "Betsi Cadwaladr University Health Board",
    "Hywel Dda University Health Board",
    "Abertawe Bro Morgannwg University Health Board",
    "Cardiff and Vale University Health Board",
    "Cwm Taf University Health Board",
    "Aneurin Bevan University Health Board",
    "Velindre University NHS Trust",
    "Wales",
    "Powys Teaching Health Board"
  )
)


head(all_hosp_data)

# separate admissions data

unique(all_hosp_data$indicator_name)

admissions <- all_hosp_data %>% 
  filter(str_detect(indicator_name, ".*admissions.*")) %>% 
  separate(indicator_code, into = c(NA, NA, 'admission_type'),sep = '_', remove = TRUE) %>% 
  select(-indicator_name)

head(admissions)

write.csv(admissions, 'data/admissions_hb_long_unfiltered.csv', row.names = FALSE)



# separate hospitalisation data

hosps <- all_hosp_data %>% 
  filter(str_detect(indicator_name, ".*ospitalisations.*"))  %>% 
  separate(indicator_code, into = c(NA, 'admission_type'),sep = 'Hosps_', remove = TRUE) %>% 
  select(-indicator_name) %>% 
  mutate(covid_status = ifelse(
    admission_type == "All" | admission_type == "NonC19", NA,
      ifelse(admission_type == "C19", "All", 
             ifelse( admission_type == "C19_Con", "Confirmed", 
                     ifelse(admission_type == "C19_Sus", "Suspected",
                            ifelse(admission_type == "C19_Rec", "Recovering", NA))))
  ))

head(hosps)

write.csv(hosps, 'data/hospitalisations_hb_long_unfiltered.csv', row.names = FALSE)


## ADD LOCKDOWN DATA

lockdowns <- read_csv('data/lockdown_status_hb_date.csv')
head(lockdowns)

lockdowns %>% 
  mutate(date = as.Date(date, format = "%d/%m/%Y")) -> lockdowns

# function for propagating values

propagate <- function(df) {
  for (i in 1:nrow(df)) {
    if (is.na(df$lockdown_status[i])) {df$lockdown_status[i] <- df$lockdown_status[i-1]}
  }  
  return(df)
}

## admissions

admissions_ld <- left_join(admissions, lockdowns, by= c("date", "health_board")) %>% 
  arrange(health_board, date) %>% 
  select(date, health_board, lockdown_status, admission_type, count)

admissions_ld <- propagate(admissions_ld)

head(admissions_ld)
tail(admissions_ld)

write.csv(admissions_ld, 'data/admissions_hb_lockdowns_long_unfiltered.csv', row.names = FALSE)


## hospitalisations

hosps_ld <- left_join(hosps, lockdowns, by = c("date", "health_board")) %>% 
  arrange(health_board, date) %>% 
  select(date, health_board, lockdown_status, hosp_type = admission_type, covid_status, count)

head(hosps_ld)
tail(hosps_ld)

hosps_ld <- propagate(hosps_ld)

write.csv(hosps_ld, 'data/hospitalisations_hb_lockdowns_long_unfiltered.csv', row.names = FALSE)


