

library(readxl)
library(dplyr)
library(tidyr)

fname1 <- c("input_data/Pipeline_Existing_UnderRenovation_Database_US_March_2016.xlsx")

facility_info <- read_excel(fname1, sheet = 2, col_names = TRUE, col_types = NULL)

colnames(facility_info) <- colnames(facility_info) %>%
  tolower() %>%
  gsub("\\.", "\\_", .) %>%
  gsub(" ", "_", .) %>%
  # replaces _(y/n) with ""
  gsub("_\\(y\\/n\\)", "", .)

colnames(facility_info)

facility_info <- facility_info %>%
  filter(!is.na(str_number)) %>%
  data.frame() %>%
  mutate(latitude = as.numeric(latitude)) %>%
  mutate(longitude = as.numeric(longitude)) %>% 
  mutate(postal_code5 = postal_code) %>%
  # this separates on non alphanumeric characters as default
  # the extra="merge" part keeps an extra matches by merging them together in the rightmost column
  # the fill="right" effectively means that if there isn't at least one match, then fill the right hand column
  # with an NA
  separate(postal_code5, c("postal_code5", "postal_codep4"), extra="merge", fill = "right")

save(facility_info, file="output_data/facility_info.Rdata")




