# data comes from
# https://data.sunshinecoast.qld.gov.au/Administration/Registered-Animals/7f87-i6kx/data
# http://rstudio-pubs-static.s3.amazonaws.com/434674_24af082031324d9c8e8739d1ca7227f9.html

library(tidyverse)

raw_data <-
  read.csv("../data_prep/registered_animals.csv", na.strings = "") %>%
  as_tibble() %>%
  rename_all(~str_replace_all(.x, "(.)([A-Z])", "\\1_\\2")) %>%
  rename_all(tolower)


clean_data <-
  raw_data %>%
  rename(
    type = animal_type,
    color = primary_colour,
    sterile = de.sexed,
    breed = primary_breed,
    breed_specific = specific_breed
  ) %>%
  mutate_all(trimws) %>%
  mutate(
    gender = recode(gender, F = "Female", M = "Male"),
    name =
      str_replace(tolower(name), "rocky.*", "rocky") %>%
      str_remove_all("\\(formerly.*") %>%
      str_replace_all('["\\(\\)\\{]', "'"),
    name =
      ifelse(
        str_detect(name, "'.*'"),
        str_extract(name, "(?<=')[^']+"),
        name
      ) %>%
      trimws() %>%
      str_to_title(),
    type = recode(type, D = "Dog"),
    age = as.integer(age),
    sterile = (sterile == "Y")
  ) %>%
  mutate_if(is.character, as.factor)


final_data <-
  clean_data %>%
  filter(
    age < 25,
    !str_detect(tolower(name), "unknown"),
    gender != "U"
  ) %>%
  drop_na() %>%
  group_by(type) %>%
  mutate(
    #breed = fct_lump_n(breed, n = 30, other_level = "(other)"),
    color = fct_lump_n(color, n = 15, other_level = "(other)"),
    locality = fct_lump_min(locality, min = 500, other_level = "(other)")
  ) %>%
  ungroup() %>%
  mutate(
    breed = fct_infreq(breed) %>% fct_rev(),
    color = fct_infreq(color) %>% fct_rev(),
    born_year = 2020 - age
  ) %>%
  ungroup()


write_rds(final_data, "registered_animals.Rds")
