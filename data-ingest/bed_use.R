library(statswalesr)
library(plyr)
library(tidyverse)

df <- statswalesr::statswales_get_dataset("hlth0092")

# for future reference - recovering code is "CO_GABed_C19_Rec" and "CO_IVBed_C19_Rec"

df %>% 
  filter(HospitalType_Code == "NHS") %>% 
  filter(Indicator_Code %in% c(
    "CO_GABed_C19", "CO_GABed_NonC19", "CO_GABed_Spare", "CO_IVBed_C19", "CO_IVBed_NonC19", "CO_IVBed_Spare"
  )) %>% 
  select(
    Date_Code, LocalHealthBoard_ItemName_ENG, Indicator_Code, Data
  ) -> beds

beds %>% 
  mutate(
    Date = as.Date(Date_Code, "%Y%m%d"),
    Count = as.integer(Data)
  ) %>% 
  select(Date, LocalHealthBoard_ItemName_ENG, Indicator_Code, Count) -> beds

# hb names from geojson file
# "Betsi Cadwaladr University Health Board",
# "Powys Teaching Health Board",
# "Hywel Dda University Health Board",
# "Abertawe Bro Morgannwg University Health Board",
# "Cwm Taf University Health Board",
# "Aneurin Bevan University Health Board",
# "Cardiff and Vale University Health Board"

beds %>% distinct(LocalHealthBoard_ItemName_ENG)

beds$LocalHealthBoard_ItemName_ENG <- mapvalues(
      beds$LocalHealthBoard_ItemName_ENG, 
      unique(beds$LocalHealthBoard_ItemName_ENG),
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

beds %>% 
  dplyr::rename(health_board = LocalHealthBoard_ItemName_ENG) -> beds

head(beds)
  
write.csv(beds, 'data/beds_hb_long_unfiltered.csv', row.names = FALSE)

beds %>% 
  pivot_wider(id_cols = c(Date, health_board), names_from = Indicator_Code, values_from = Count) -> beds_pivot

head(beds_pivot)

beds_pivot %>% 
  mutate(
    CO_GABed_Tot = CO_GABed_C19 + CO_GABed_NonC19 + CO_GABed_Spare,
    CO_IVBed_Tot = CO_IVBed_C19 + CO_IVBed_NonC19 + CO_IVBed_Spare
  ) -> beds_pivot

head(beds_pivot)

beds_pivot %>% 
  select(Date, health_board, sort(names(beds_pivot)[3:10])) -> beds_pivot

write.csv(beds_pivot, 'data/beds_hb_wide_unfiltered.csv', row.names = F)

beds_pivot %>% 
  mutate(beds_total = CO_IVBed_Tot + CO_GABed_Tot) %>% 
  mutate(percent_beds_in_use_covid = ((CO_GABed_C19 + CO_IVBed_C19)) / beds_total) %>% 
  select(date = Date, health_board, percent_beds_in_use_covid) -> beds_percent_covid

write.csv(beds_percent_covid, 'data/beds_percent_covid.csv', row.names = FALSE)

head(beds_percent_covid)
tail(beds_percent_covid)

###### ANEURIN BEVAN DRILLDOWN

# for future reference - recovering code is "CO_GABed_C19_Rec" and "CO_IVBed_C19_Rec"

df %>% 
  filter(HospitalType_Code == "NHS") %>% 
  filter(LocalHealthBoard_ItemName_ENG == "Aneurin Bevan University Local Health Board") %>% 
  filter(Indicator_Code %in% c(
    "CO_GABed_C19_Con", "CO_GABed_C19_Sus", "CO_GABed_C19_Rec", "CO_GABed_NonC19", "CO_GABed_Spare", "CO_IVBed_C19_Sus", "CO_IVBed_C19_Con", "CO_IVBed_C19_Rec", "CO_IVBed_NonC19", "CO_IVBed_Spare"
  )) %>% 
  mutate(date = as.Date(Date_Code, '%Y%m%d'), count = as.integer(Data)) %>% 
  select(date,indicator = Indicator_Code, count) -> beds_ab

head(beds_ab)

write.csv(beds_ab, 'data/bed_use_aneurin_bevan.csv', row.names = F)


## BETSI CADWALADR

df %>% 
  filter(HospitalType_Code == "NHS") %>% 
  filter(LocalHealthBoard_ItemName_ENG == "Betsi Cadwaladr University Local Health Board") %>% 
  filter(Indicator_Code %in% c(
    "CO_GABed_C19_Con", "CO_GABed_C19_Sus", "CO_GABed_C19_Rec", "CO_GABed_NonC19", "CO_GABed_Spare", "CO_IVBed_C19_Sus", "CO_IVBed_C19_Con", "CO_IVBed_C19_Rec", "CO_IVBed_NonC19", "CO_IVBed_Spare"
  )) %>% 
  mutate(date = as.Date(Date_Code, '%Y%m%d'), count = as.integer(Data)) %>% 
  select(date,indicator = Indicator_Code, count) -> beds_bc

head(beds_bc)

write.csv(beds_bc, 'data/bed_use_betsi_cadwaladr.csv', row.names = F)

## CWM TAF

df %>% 
  filter(HospitalType_Code == "NHS") %>% 
  filter(LocalHealthBoard_ItemName_ENG == "Cwm Taf Morgannwg University Local Health Board") %>% 
  filter(Indicator_Code %in% c(
    "CO_GABed_C19_Con", "CO_GABed_C19_Sus", "CO_GABed_C19_Rec", "CO_GABed_NonC19", "CO_GABed_Spare", "CO_IVBed_C19_Sus", "CO_IVBed_C19_Con", "CO_IVBed_C19_Rec", "CO_IVBed_NonC19", "CO_IVBed_Spare"
  )) %>% 
  mutate(date = as.Date(Date_Code, '%Y%m%d'), count = as.integer(Data)) %>% 
  select(date,indicator = Indicator_Code, count) -> beds_ct

head(beds_ct)

write.csv(beds_ct, 'data/bed_use_cwm_taf.csv', row.names = F)

## Powys Teaching Local Health Board
df %>% 
  filter(HospitalType_Code == "NHS") %>% 
  filter(LocalHealthBoard_ItemName_ENG == "Cwm Taf Morgannwg University Local Health Board") %>% 
  filter(Indicator_Code %in% c(
    "CO_GABed_C19_Con", "CO_GABed_C19_Sus", "CO_GABed_C19_Rec", "CO_GABed_NonC19", "CO_GABed_Spare", "CO_IVBed_C19_Sus", "CO_IVBed_C19_Con", "CO_IVBed_C19_Rec", "CO_IVBed_NonC19", "CO_IVBed_Spare"
  )) %>% 
  mutate(date = as.Date(Date_Code, '%Y%m%d'), count = as.integer(Data)) %>% 
  select(date,indicator = Indicator_Code, count) -> beds_p

write.csv(beds_p, 'data/bed_use_powys.csv', row.names = F)

unique(df$LocalHealthBoard_ItemName_ENG)
